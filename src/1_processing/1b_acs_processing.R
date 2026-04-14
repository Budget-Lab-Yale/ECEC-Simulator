#------------------------------------------------------------------------------
# 1b_acs_processing.R
#
# ACS Processing: demand-side imputations including earnings, ECEC enrollment,
# employment probabilities, and SPM unit processing.
#
# Primary function: run_acs_processing()
# Called from: main.R
#------------------------------------------------------------------------------



run_acs_processing <- function() {

  #----------------------------------------------------------------------------
  # Process 2019 ACS data for demand-side calibration. Produces hierarchical
  # household tables, earnings imputations (Heckman), ECEC enrollment
  # imputations (random forest), employment probability estimates, and SPM
  # unit processing. Requires NSECE data from run_nsece_processing() or
  # copy_nsece_interface().
  #
  # Params: none
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  # Get estimation data info
  estimation_info <- get_estimation_info()


  #---------------------------------------------------------------------------
  # Local helpers (used multiple times below)
  #---------------------------------------------------------------------------

  get_primary_caregiver <- function(hhm_id2, male1, male2) {

    #--------------------------------------------------------------------------
    # Identify primary caregiver: mother in two-parent families, parent 1
    # otherwise. Returns 1L or 2L indicating which parent is primary.
    #
    # Params:
    #   - hhm_id2 (int): Household member ID for parent 2 (NA if single-parent)
    #   - male1 (int): Sex indicator for parent 1 (0 = female, 1 = male)
    #   - male2 (int): Sex indicator for parent 2 (0 = female, 1 = male)
    #
    # Returns: (int) 1L or 2L indicating the primary caregiver parent index
    #--------------------------------------------------------------------------

    case_when(
      !is.na(hhm_id2) & male1 == 0 ~ 1L,
      !is.na(hhm_id2) & male2 == 0 ~ 2L,
      !is.na(hhm_id2)              ~ 1L,
      TRUE                         ~ 1L
    )
  }

  derive_employment_choice <- function(hours) {

    #--------------------------------------------------------------------------
    # Map weekly hours worked to an employment choice factor with levels
    # 'none', 'pt' (part-time), and 'ft' (full-time). Threshold at 35 hours.
    #
    # Params:
    #   - hours (num vec): Weekly hours worked (NA or 0 treated as 'none')
    #
    # Returns: (factor) Employment choice with levels c('none', 'pt', 'ft')
    #--------------------------------------------------------------------------

    factor(case_when(
      is.na(hours) | hours == 0 ~ 'none',
      hours < 35                ~ 'pt',
      TRUE                      ~ 'ft'
    ), levels = c('none', 'pt', 'ft'))
  }

  compute_income_pctiles <- function(children, households) {

    #--------------------------------------------------------------------------
    # Compute child-weighted income percentile cutpoints for children under 5.
    # Joins children to households, then calculates weighted quantiles (1-99)
    # of household income using child weights.
    #
    # Params:
    #   - children (df): Children table with hh_id, age, child_weight columns
    #   - households (df): Households table with hh_id, income columns
    #
    # Returns: (num vec) 99-element named vector of income cutpoints
    #--------------------------------------------------------------------------

    children %>%
      left_join(households, by = 'hh_id') %>%
      filter(age < 5, income > 0) %>%
      reframe(
        income_pctile = wtd.quantile(
          x      = income + runif(nrow(.)),
          weight = child_weight,
          probs  = seq(0.01, 0.99, 0.01)
        )
      ) %>%
      deframe()
  }

  extract_pc_features <- function(df, primary_caregiver_col = 'primary_caregiver') {

    #--------------------------------------------------------------------------
    # Extract primary-caregiver RF features from expanded parent_units. Selects
    # the correct parent's attributes (age, educ, race, etc.) based on which
    # parent is the primary caregiver, and computes partner earnings/hours.
    #
    # Params:
    #   - df (df): Expanded parent_units dataframe with parent 1/2 columns
    #   - primary_caregiver_col (chr): Column name indicating primary caregiver
    #
    # Returns: (df) Input df with added pc_* and partner_* feature columns
    #--------------------------------------------------------------------------

    df %>%
      mutate(
        pc_age             = if_else(.data[[primary_caregiver_col]] == 1, age1, age2),
        pc_educ            = if_else(.data[[primary_caregiver_col]] == 1, educ1, educ2),
        pc_married         = if_else(.data[[primary_caregiver_col]] == 1, married1, married2),
        pc_race            = if_else(.data[[primary_caregiver_col]] == 1, race1, race2),
        pc_hispanic        = if_else(.data[[primary_caregiver_col]] == 1, hispanic1, hispanic2),
        pc_hourly_earnings = if_else(.data[[primary_caregiver_col]] == 1, hourly_earnings1, hourly_earnings2),
        pc_male            = if_else(.data[[primary_caregiver_col]] == 1, male1, male2),
        partner_earnings = case_when(
          n_parents == 1                           ~ 0,
          .data[[primary_caregiver_col]] == 1 ~ replace_na(baseline_earnings2, 0),
          .data[[primary_caregiver_col]] == 2 ~ replace_na(baseline_earnings1, 0)
        ),
        partner_hours = case_when(
          n_parents == 1                           ~ 0,
          .data[[primary_caregiver_col]] == 1 ~ replace_na(hours2, 0),
          .data[[primary_caregiver_col]] == 2 ~ replace_na(hours1, 0)
        ),
        n_children  = n_children_original,
        has_partner = as.integer(n_parents == 2)
      )
  }

  fit_heckman <- function(data, suffix, include_married) {

    #--------------------------------------------------------------------------
    # Fit a Heckman two-step selection model for hourly earnings imputation.
    # Estimates selection into employment and outcome (log hourly earnings),
    # then returns exponentiated unconditional predictions with variance.
    #
    # Params:
    #   - data (df): Parent units dataframe with suffixed columns (e.g. age1)
    #   - suffix (chr): Parent column suffix ('1' or '2')
    #   - include_married (logical): Whether to include marital status in
    #     the selection equation
    #
    # Returns: (list) With elements 'model' (sampleSelection object) and
    #   'fitted_values' (num vec of predicted hourly earnings in levels)
    #--------------------------------------------------------------------------

    model_data <- data %>%
      mutate(
        works               = as.integer(!is.na(.data[[paste0('hourly_earnings', suffix)]]) & .data[[paste0('hourly_earnings', suffix)]] > 0),
        log_hourly_earnings = log(if_else(works == 1, .data[[paste0('hourly_earnings', suffix)]], NA_real_))
      ) %>%
      select(
        per_weight = !!sym(paste0('per_weight', suffix)),
        works, log_hourly_earnings,
        age      = !!sym(paste0('age', suffix)),
        male     = !!sym(paste0('male', suffix)),
        educ     = !!sym(paste0('educ', suffix)),
        race     = !!sym(paste0('race', suffix)),
        hispanic = !!sym(paste0('hispanic', suffix)),
        married  = !!sym(paste0('married', suffix)),
        n_kids_0to2,
        n_kids_3to5,
        n_kids_6to12
      )

    selection_formula <- if (include_married) {
      works ~ poly(age, 2) + male + hispanic + as.factor(race) + as.factor(educ) + married + n_kids_0to2 + n_kids_3to5 + n_kids_6to12
    } else {
      works ~ poly(age, 2) + male + hispanic + as.factor(race) + as.factor(educ) + n_kids_0to2 + n_kids_3to5 + n_kids_6to12
    }

    model <- selection(
      selection = selection_formula,
      outcome   = log_hourly_earnings ~ poly(age, 2) + male + hispanic + as.factor(race) + as.factor(educ),
      data      = model_data,
      weights   = model_data$per_weight,
      method    = 'ml'
    )

    predictions <- predict(model, newdata = model_data, type = 'unconditional')
    sigma <- model$estimate['sigma']
    predictions_with_variance <- predictions + rnorm(length(predictions), mean = 0, sd = sigma)

    list(
      model = model,
      fitted_values = exp(predictions_with_variance)
    )
  }

  compute_distribution_by_parents <- function(data, group_var, weight_expr, prefix = '') {

    #--------------------------------------------------------------------------
    # Compute weighted percentage distribution of a variable by parent count.
    # Groups by n_parents and the specified variable, calculates weighted
    # shares, then pivots wide with one column per parent count.
    #
    # Params:
    #   - data (df): Dataframe with n_parents column and the group variable
    #   - group_var (chr): Name of the variable to compute distribution over
    #   - weight_expr (expr): Unquoted expression for the weight column
    #   - prefix (chr): Prefix for pivoted column names (default '')
    #
    # Returns: (df) Wide-format table with columns: subcategory,
    #   {prefix}n_parents_1, {prefix}n_parents_2
    #--------------------------------------------------------------------------

    data %>%
      group_by(n_parents, !!sym(group_var)) %>%
      summarise(weighted_children = sum(!!weight_expr), .groups = 'drop') %>%
      group_by(n_parents) %>%
      mutate(pct = 100 * weighted_children / sum(weighted_children)) %>%
      ungroup() %>%
      select(n_parents, subcategory = !!sym(group_var), pct) %>%
      pivot_wider(
        names_from = n_parents, values_from = pct,
        names_prefix = paste0(prefix, 'n_parents_')
      )
  }

  prepare_data_for_rf <- function(children, parent_units, households,
                                  household_members, enrollment,
                                  income_pctiles, source_label,
                                  is_nsece = FALSE) {

    #--------------------------------------------------------------------------
    # Prepare child-level data for random forest ECEC enrollment models.
    # Shared logic for both NSECE (training) and ACS (prediction). Joins
    # children to parent/household data, derives features, censors 3+ child
    # families to 2 youngest, and splits into one-child and two-child groups
    # by parent count (0, 1, 2).
    #
    # Params:
    #   - children (df): Children table with hh_id, child_id, age, etc.
    #   - parent_units (df): Parent units table with parent attributes
    #   - households (df): Households table with income, region, n_members
    #   - household_members (df): Household members table with demographics
    #   - enrollment (df): ECEC enrollment table (NSECE only; NULL for ACS)
    #   - income_pctiles (num vec): Income percentile cutpoints
    #   - source_label (chr): Label for error messages ('NSECE' or 'ACS')
    #   - is_nsece (logical): Whether the data source is NSECE (default FALSE)
    #
    # Returns: (list) With elements 'one_child' and 'two_child', each a named
    #   list by parent count ('0', '1', '2') of prepared dataframes
    #--------------------------------------------------------------------------

    # Start with children
    rf_tmp <- children %>%
      select(hh_id, child_id, parent_unit_id, child_weight, age, male) %>%
      filter(age < 5)

    # Add enrollment info (NSECE only - ACS children don't have observed enrollment)
    if (is_nsece && !is.null(enrollment)) {
      rf_tmp <- rf_tmp %>%
        left_join(
          enrollment %>% select(hh_id, child_id, ecec_choice, ecec_hours_choice),
          by = c('hh_id', 'child_id')
        ) %>%
        filter(!is.na(ecec_choice), !is.na(ecec_hours_choice)) %>%
        mutate(ecec_class = paste0(ecec_choice, '_', ecec_hours_choice), .after = child_weight) %>%
        select(-ecec_choice, -ecec_hours_choice)
    }

    # Add parent unit information
    if (is_nsece) {
      # NSECE: respondent (parent 1) is primary caregiver by survey design
      rf_tmp <- rf_tmp %>%
        left_join(
          parent_units %>%
            select(hh_id, parent_unit_id, p.hhm_id1 = hhm_id1, p.hhm_id2 = hhm_id2,
                   p.male1 = male1, p.hours1 = hours1, p.hours2 = hours2),
          by = c('hh_id', 'parent_unit_id')
        ) %>%
        left_join(
          household_members %>%
            filter(!is.na(hhm_id)) %>%
            select(hh_id, p.hhm_id1 = hhm_id, p.age1 = age),
          by = c('hh_id', 'p.hhm_id1')
        ) %>%
        left_join(
          household_members %>%
            filter(!is.na(hhm_id)) %>%
            select(hh_id, p.hhm_id2 = hhm_id, p.age2 = age),
          by = c('hh_id', 'p.hhm_id2')
        )
    } else {
      # ACS: need both parents' sex for primary caregiver determination
      rf_tmp <- rf_tmp %>%
        left_join(
          parent_units %>%
            select(hh_id, parent_unit_id, p.hhm_id1 = hhm_id1, p.hhm_id2 = hhm_id2,
                   p.male1 = male1, p.male2 = male2, p.age1 = age1, p.age2 = age2,
                   p.hours1 = hours1, p.hours2 = hours2),
          by = c('hh_id', 'parent_unit_id')
        )
    }

    # Add household characteristics
    rf_tmp <- rf_tmp %>%
      left_join(
        households %>% select(hh_id, income, n_members, region),
        by = 'hh_id'
      ) %>%
      mutate(
        income_pctile = cut(income, c(-Inf, income_pctiles, Inf), labels = 1:100) %>%
          as.character() %>% as.integer(),
        region = factor(region)
      )

    # Derive primary caregiver employment choice
    if (is_nsece) {
      # NSECE: respondent (parent 1) is primary caregiver
      rf_tmp <- rf_tmp %>%
        mutate(
          hours_pc = p.hours1,
          employment_choice_pc = derive_employment_choice(hours_pc)
        ) %>%
        select(-hours_pc, -income)
    } else {
      # ACS: identify primary caregiver then derive employment choice
      rf_tmp <- rf_tmp %>%
        mutate(
          primary_caregiver = get_primary_caregiver(p.hhm_id2, p.male1, p.male2),
          hours_pc = if_else(primary_caregiver == 1L, p.hours1, p.hours2),
          employment_choice_pc = derive_employment_choice(hours_pc)
        ) %>%
        select(-income, -hours_pc)
    }

    # Build family_id and count children
    rf_tmp <- rf_tmp %>%
      mutate(
        family_id = if_else(
          is.na(parent_unit_id),
          paste0('np_', hh_id),
          paste0('pu_', hh_id, '_', parent_unit_id)
        )
      ) %>%
      add_count(family_id, name = 'n_children') %>%
      mutate(
        n_children_original = n_children,
        child_category = case_when(
          n_children == 1 ~ '1',
          n_children >= 2 ~ '2+'
        )
      )

    # Censor 3+ child families to 2 youngest, reweight
    if (any(!is.finite(rf_tmp$child_weight))) {
      stop('ecec_imputation: ', source_label, ' child_weight contains NA/Inf before censoring.')
    }
    pre_censor_weight <- sum(rf_tmp$child_weight)

    rf_tmp <- rf_tmp %>%
      arrange(family_id, age, child_id) %>%
      group_by(family_id) %>%
      mutate(child_index_temp = row_number()) %>%
      ungroup() %>%
      filter(child_index_temp <= 2) %>%
      select(-child_index_temp)

    if (any(!is.finite(rf_tmp$child_weight))) {
      stop('ecec_imputation: ', source_label, ' child_weight contains NA/Inf after censoring.')
    }
    post_censor_weight <- sum(rf_tmp$child_weight)

    if (!is.finite(pre_censor_weight) || !is.finite(post_censor_weight) || post_censor_weight <= 0) {
      stop('ecec_imputation: invalid child weights during censoring reweight (', source_label, '). ',
           'pre_censor_weight=', pre_censor_weight, ', post_censor_weight=', post_censor_weight)
    }
    reweight_factor <- pre_censor_weight / post_censor_weight
    if (!is.finite(reweight_factor) || reweight_factor <= 0) {
      stop('ecec_imputation: invalid censoring reweight_factor (', source_label, '): ', reweight_factor)
    }
    rf_tmp <- rf_tmp %>%
      mutate(child_weight = child_weight * reweight_factor)

    # Update n_children to reflect effective count after censoring
    rf_tmp <- rf_tmp %>%
      group_by(family_id) %>%
      mutate(n_children = n()) %>%
      ungroup()

    # Split one-child families
    one_child <- rf_tmp %>% filter(n_children == 1)
    if (is_nsece) {
      one_child <- one_child %>%
        filter(!is.na(ecec_class), !is.na(age), !is.na(male)) %>%
        mutate(model_weight = child_weight)
    }

    # Two-child families: reshape wide
    two_child_base <- rf_tmp %>% filter(n_children == 2)
    if (is_nsece) {
      two_child_base <- two_child_base %>%
        filter(!is.na(ecec_class), !is.na(age), !is.na(male))
    }

    pivot_cols <- if (is_nsece) {
      c('child_id', 'child_weight', 'age', 'male', 'ecec_class')
    } else {
      c('child_id', 'child_weight', 'age', 'male')
    }
    two_child_wide <- two_child_base %>%
      arrange(family_id, age, child_id) %>%
      group_by(family_id) %>%
      mutate(child_index = row_number()) %>%
      ungroup() %>%
      pivot_wider(
        id_cols = c(
          family_id, hh_id, parent_unit_id, n_children_original, child_category,
          income_pctile, n_members, region, p.hhm_id2, p.age1, p.age2, p.male1, employment_choice_pc
        ),
        names_from  = child_index,
        values_from = all_of(pivot_cols),
        names_glue  = '{.value}_{child_index}'
      )

    if (is_nsece) {
      two_child_wide <- two_child_wide %>%
        mutate(
          model_weight = child_weight_1 + child_weight_2,
          ecec_class   = paste(ecec_class_1, ecec_class_2, sep = '__'),
          is_3plus     = as.integer(n_children_original >= 3)
        )
    } else {
      two_child_wide <- two_child_wide %>%
        mutate(is_3plus = as.integer(n_children_original >= 3))
    }

    # Filter to complete cases with valid region
    one_child_split <- list(
      `0` = one_child %>% filter(is.na(parent_unit_id), !is.na(region)),
      `1` = one_child %>% filter(!is.na(parent_unit_id), is.na(p.hhm_id2), !is.na(p.male1), !is.na(p.age1), !is.na(region)),
      `2` = one_child %>% filter(!is.na(parent_unit_id), !is.na(p.hhm_id2), !is.na(p.male1), !is.na(p.age1), !is.na(p.age2), !is.na(region))
    )
    two_child_split <- list(
      `0` = two_child_wide %>% filter(is.na(parent_unit_id), !is.na(region)),
      `1` = two_child_wide %>% filter(!is.na(parent_unit_id), is.na(p.hhm_id2), !is.na(p.male1), !is.na(p.age1), !is.na(region)),
      `2` = two_child_wide %>% filter(!is.na(parent_unit_id), !is.na(p.hhm_id2), !is.na(p.male1), !is.na(p.age1), !is.na(p.age2), !is.na(region))
    )

    list(one_child = one_child_split, two_child = two_child_split)
  }


  #-------------------------------------------
  # Load NSECE household data (required for imputations)
  #-------------------------------------------

  nsece_parent_units_path <- file.path(estimation_info$paths$data, 'nsece_parent_units_2019.csv')
  if (!file.exists(nsece_parent_units_path)) {
    stop('NSECE data not found. Run run_nsece_processing() first or specify --nsece-interface.')
  }

  processed_nsece_hh <- list()
  nsece_tables <- c('households', 'household_members', 'children', 'parent_units', 'providers', 'enrollment')
  for (table_name in nsece_tables) {
    table_path <- file.path(estimation_info$paths$data, paste0('nsece_', table_name, '_2019.csv'))
    if (file.exists(table_path)) {
      processed_nsece_hh[[table_name]] <- fread(table_path)
    }
  }


  #------------------------------------------------
  # Process 2019 ACS (for demand model estimation)
  #------------------------------------------------

  cat('Processing 2019 ACS data...\n')

  acs_raw_path <- file.path(estimation_info$paths$ACS, 'ipums_usa.csv')
  if (!file.exists(acs_raw_path)) {
    stop('ACS data file not found: ', acs_raw_path, '\n',
         'Please ensure raw ACS data exists.')
  }

  acs_raw <- acs_raw_path %>%
    fread() %>%
    tibble()

  # Filter to 2019 and build hierarchical tables
  processed_acs_2019 <- {

    acs <- acs_raw %>%
      filter(YEAR == 2019) %>%
      filter(GQ == 1 | GQ == 2) %>%
      select(
        hh_id = SERIAL,
        HHWT,
        HHINCOME,
        REGION,
        PERNUM, PERWT,
        AGE, SEX, MARST, RACE, HISPAN, EDUC,
        RELATE,
        SPLOC, MOMLOC, POPLOC, MOMLOC2, POPLOC2,
        UHRSWORK, WKSWORK1, INCWAGE, INCBUS00,
        tax_unit_id = TAXID, agi = ADJGINC,
        any_of(c('SPMFAMUNIT', 'SPMTOTRES', 'SPMTHRESH',
                 'SPMCHXPNS', 'SPMWKXPNS', 'SPMCAPXPNS'))
      )

    # Subset to specified fraction of households if calib_sample < 100
    if (calib_sample < 100) {
      cat('Using', calib_sample, '% of ACS sample for calibration\n')
      set.seed(random_seed_base)

      random_hh <- acs %>%
        distinct(hh_id, HHWT) %>%
        slice_sample(prop = calib_sample / 100) %>%
        pull(hh_id)

      acs <- acs %>%
        filter(hh_id %in% random_hh) %>%
        mutate(
          HHWT  = HHWT  / (calib_sample / 100),
          PERWT = PERWT / (calib_sample / 100)
        )
    }


    #------------
    # Households
    #------------

    households <- acs %>%
      group_by(hh_id) %>%
      summarise(
        hh_weight = first(HHWT),
        income    = first(HHINCOME),
        # IPUMS REGION is a 2-digit division code (11-42). First digit is region.
        region    = first(REGION) %/% 10L,
        n_members = n(),
        .groups   = 'drop'
      )


    #--------------------------
    # Children (age <= 12 only)
    #--------------------------

    children <- acs %>%
      filter(AGE <= 12) %>%
      mutate(
        child_id     = PERNUM,
        child_weight = PERWT,
        age          = AGE,
        male         = as.integer(SEX == 1)
      ) %>%
      select(hh_id, child_id, tax_unit_id, child_weight, age, male) %>%
      arrange(hh_id, child_id)


    #---------------------------------------
    # Household members (all persons in HH)
    #---------------------------------------

    household_members <- acs %>%
      mutate(
        child_id    = if_else(AGE <= 12, PERNUM, NA),
        hhm_id      = if_else(AGE > 12, PERNUM, NA),
        male        = as.integer(SEX == 1),
        married     = as.integer(MARST %in% 1:2),
        hispanic    = as.integer(HISPAN != 0),
        hours       = UHRSWORK,
        weeks       = WKSWORK1,
        wages       = if_else(INCWAGE  != 999999, INCWAGE,  0),
        se_earnings = if_else(INCBUS00 != 999999, INCBUS00, 0),
        earnings    = wages + se_earnings,
        hourly_earnings = if_else(
          weeks > 0 & hours > 0 & earnings > 0,
          earnings / (weeks * hours),
          NA
        )
      ) %>%
      select(
        hh_id, hhm_id, child_id, tax_unit_id, per_weight = PERWT,
        age = AGE, male, married,
        race = RACE, hispanic,
        educ = EDUC,
        hours, weeks, earnings, hourly_earnings
      ) %>%
      arrange(hh_id, hhm_id)


    #-------------------
    # Parent unit table
    #-------------------

    # Build links child (<=12) -> parents using MOMLOC/POPLOC and *_LOC2 (same-sex)
    child_parents_long <- acs %>%
      filter(AGE <= 12) %>%
      select(hh_id, child_id = PERNUM, MOMLOC, POPLOC, MOMLOC2, POPLOC2) %>%
      pivot_longer(
        cols      = c(MOMLOC, POPLOC, MOMLOC2, POPLOC2),
        names_to  = 'which_parent',
        values_to = 'parent_hhm_id'
      ) %>%
      filter(!is.na(parent_hhm_id) & parent_hhm_id > 0) %>%
      arrange(hh_id, child_id, which_parent)

    # For each child, collapse to at most 2 unique parents and make a unit id
    parent_units_core <- child_parents_long %>%
      group_by(hh_id, child_id) %>%
      summarise(
        hhm_id1 = sort(unique(parent_hhm_id))[1],
        hhm_id2 = sort(unique(parent_hhm_id))[2],
        .groups = 'drop'
      ) %>%
      mutate(
        parent_unit_id = if_else(
          is.na(hhm_id2),
          as.character(hhm_id1),
          paste0(hhm_id1, '_', hhm_id2)
        ),
        n_parents = if_else(is.na(hhm_id2), 1L, 2L),
        .after = child_id
      )

    # Add parent_unit_id onto the children table
    children <- children %>%
      left_join(
        parent_units_core %>% select(hh_id, child_id, parent_unit_id),
        by = c('hh_id', 'child_id')
      ) %>%
      relocate(parent_unit_id, .after = child_id) %>%
      relocate(tax_unit_id, .after = parent_unit_id) %>%
      arrange(hh_id, child_id)

    # Count children under 5 per parent unit and create child_category
    child_counts <- children %>%
      filter(age < 5) %>%
      group_by(hh_id, parent_unit_id) %>%
      summarise(n_children_original = n(), .groups = 'drop') %>%
      mutate(
        child_category = case_when(
          n_children_original == 1 ~ '1',
          n_children_original >= 2 ~ '2+'
        )
      )

    # Deduplicate parent_units to one row per unique parent unit per hh
    parent_units_core <- parent_units_core %>%
      distinct(hh_id, parent_unit_id, .keep_all = TRUE) %>%
      select(-child_id) %>%
      left_join(child_counts, by = c('hh_id', 'parent_unit_id')) %>%
      arrange(hh_id, parent_unit_id)

    # Attach parent 1 and parent 2 attributes to the parent unit row
    parent_units <- parent_units_core %>%
      left_join(
        household_members %>%
          filter(is.na(child_id)) %>%
          select(-child_id) %>%
          rename_with(~ paste0(.x, '1'), -c(hh_id, hhm_id)) %>%
          rename(hhm_id1 = hhm_id),
        by = c('hh_id', 'hhm_id1')
      ) %>%
      left_join(
        household_members %>%
          filter(is.na(child_id)) %>%
          select(-child_id) %>%
          rename_with(~ paste0(.x, '2'), -c(hh_id, hhm_id)) %>%
          rename(hhm_id2 = hhm_id),
        by = c('hh_id', 'hhm_id2')
      ) %>%
      select(
        hh_id, parent_unit_id,
        n_parents, n_children_original, child_category,
        hhm_id1, hhm_id2,
        tax_unit_id1, tax_unit_id2,
        per_weight1, per_weight2,
        age1, male1, married1, race1, hispanic1, educ1,
        age2, male2, married2, race2, hispanic2, educ2,
        hours1, weeks1, earnings1, hourly_earnings1,
        hours2, weeks2, earnings2, hourly_earnings2
      ) %>%
      # Filter out parent units where any parent is under 18
      filter(age1 >= 18 & (is.na(age2) | age2 >= 18)) %>%
      arrange(hh_id)


    #----------------
    # Tax unit table
    #----------------

    tax_units <- acs %>%
      group_by(hh_id, tax_unit_id) %>%
      summarise(
        tax_unit_weight = mean(PERWT),
        filing_status = case_when(
          any(MARST == 1)   ~ 2,
          sum(AGE < 18) > 0 ~ 4,
          T                 ~ 1
        ),
        n_dep_u13 = pmin(3, sum(AGE <= 12)),
        agi       = first(agi),
        .groups = 'drop'
      ) %>%
      arrange(hh_id, tax_unit_id)

    # Remove earnings info from household members table -- we only need it for parents
    household_members <- household_members %>%
      select(-hours, -weeks, -earnings, -hourly_earnings)

    list(
      households        = households,
      household_members = household_members,
      children          = children,
      parent_units      = parent_units,
      tax_units         = tax_units
    )
  }

  #------------------------------------------------
  # Earnings imputation (Heckman selection model)
  #------------------------------------------------

  processed_acs_2019$parent_units <- {

    set.seed(random_seed_base + 4)

    # Add children counts for earnings predictors
    parent_units_enhanced <- processed_acs_2019$parent_units %>%
      left_join(
        processed_acs_2019$children %>%
          group_by(hh_id, parent_unit_id) %>%
          summarise(
            n_kids_0to2  = sum(age <= 2),
            n_kids_3to5  = sum(age >= 3 & age <= 5),
            n_kids_6to12 = sum(age >= 6 & age <= 12),
            .groups = 'drop'
          ),
        by = c('hh_id', 'parent_unit_id')
      )

    # Fit Heckman models for parent 1 (all) and parent 2 (couples only)
    heckman_model1 <- fit_heckman(parent_units_enhanced, '1', include_married = TRUE)
    heckman_model2 <- fit_heckman(
      parent_units_enhanced %>% filter(!is.na(hhm_id2)),
      '2', include_married = FALSE
    )

    # Save model summaries
    writeLines(capture.output(summary(heckman_model1$model)), con = file.path(estimation_info$paths$output, 'models/heckman1.txt'))
    writeLines(capture.output(summary(heckman_model2$model)), con = file.path(estimation_info$paths$output, 'models/heckman2.txt'))

    # Assign fitted values
    parent_units <- processed_acs_2019$parent_units
    parent_units$hourly_earnings1_hat <- heckman_model1$fitted_values
    parent_units[!is.na(parent_units$hhm_id2), 'hourly_earnings2_hat'] <- heckman_model2$fitted_values

    # Replace values for nonworkers
    parent_units %>%
      mutate(
        imputed_earnings1 = as.integer(is.na(hourly_earnings1) | hourly_earnings1 <= 0),
        hourly_earnings1 = if_else(
          imputed_earnings1 == 1,
          hourly_earnings1_hat,
          hourly_earnings1
        ),
        imputed_earnings2 = as.integer(!is.na(hhm_id2) & (is.na(hourly_earnings2) | hourly_earnings2 <= 0)),
        hourly_earnings2 = if_else(
          imputed_earnings2 == 1,
          hourly_earnings2_hat,
          hourly_earnings2
        )
      ) %>%
      select(-hourly_earnings1_hat, -hourly_earnings2_hat)
  }


  #----------------------------------------------------------------------------
  # ECEC enrollment imputation
  #----------------------------------------------------------------------------

  predict_enrollment_by_employment <- function(rf_models, acs_rf) {

    #--------------------------------------------------------------------------
    # Predict ECEC enrollment probabilities for each employment counterfactual
    # (none, pt, ft). Runs trained RF models on ACS data, generating predicted
    # enrollment distributions for one-child and two-child families under each
    # primary caregiver employment state.
    #
    # Params:
    #   - rf_models (list): Trained ranger RF models, nested by family type
    #     and parent count (e.g. rf_models$one_child[['1']])
    #   - acs_rf (list): ACS prediction data prepared by prepare_data_for_rf(),
    #     nested by family type and parent count
    #
    # Returns: (list) With elements 'one_child' (df with p_enrollment.none,
    #   p_enrollment.pt, p_enrollment.ft) and 'two_child' (df with p_joint.none,
    #   p_joint.pt, p_joint.ft) in wide format
    #--------------------------------------------------------------------------

    employment_states <- c('none', 'pt', 'ft')
    parent_keys <- c('0', '1', '2')

    # One-child families: predict for each employment state
    one_child_by_emp <- map(employment_states, function(emp_state) {

      map_dfr(parent_keys, function(parent_count) {
        model <- rf_models$one_child[[parent_count]]
        data  <- acs_rf$one_child[[parent_count]]

        if (is.null(model) || is.null(data) || nrow(data) == 0) {
          return(tibble())
        }

        if (parent_count == '0') {
          predictions <- predict(model, data = data)$predictions
        } else {
          data_cf <- data %>%
            mutate(employment_choice_pc = factor(emp_state, levels = c('none', 'pt', 'ft')))
          predictions <- predict(model, data = data_cf)$predictions
        }

        as.data.frame(predictions) %>%
          tibble() %>%
          bind_cols(data %>% select(hh_id, parent_unit_id, child_id)) %>%
          pivot_longer(
            cols      = -c(hh_id, parent_unit_id, child_id),
            names_to  = 'ecec_class',
            values_to = 'p_enrollment'
          ) %>%
          separate(
            ecec_class,
            into  = c('ecec_choice', 'ecec_hours_choice'),
            sep   = '_(?=[^_]+$)',
            extra = 'merge',
            fill  = 'right'
          )
      })
    }) %>%
      set_names(employment_states)

    # Combine into wide format: p_enrollment.none, p_enrollment.pt, p_enrollment.ft
    one_child_join_keys <- c('hh_id', 'parent_unit_id', 'child_id', 'ecec_choice', 'ecec_hours_choice')
    one_child_wide <- map(employment_states, function(s) {
      one_child_by_emp[[s]] %>% rename(!!paste0('p_enrollment.', s) := p_enrollment)
    }) %>% reduce(left_join, by = one_child_join_keys)

    # -- Assertion: reduce/join didn't duplicate rows ----
    # Each (hh_id, parent_unit_id, child_id, ecec_choice, ecec_hours_choice)
    # should appear exactly once. If the join keys don't uniquely identify
    # rows in each employment state's predictions, reduce(left_join) will
    # silently multiply rows.
    n_expected <- nrow(one_child_by_emp[['none']])
    stopifnot(nrow(one_child_wide) == n_expected)

    # Two-child families: predict joint for each employment state
    two_child_by_emp <- map(employment_states, function(emp_state) {

      map_dfr(parent_keys, function(parent_count) {
        model <- rf_models$two_child[[parent_count]]
        data  <- acs_rf$two_child[[parent_count]]

        if (is.null(model) || is.null(data) || nrow(data) == 0) {
          return(tibble())
        }

        if (parent_count == '0') {
          predictions <- predict(model, data = data)$predictions
        } else {
          data_cf <- data %>%
            mutate(employment_choice_pc = factor(emp_state, levels = c('none', 'pt', 'ft')))
          predictions <- predict(model, data = data_cf)$predictions
        }

        as.data.frame(predictions) %>%
          tibble() %>%
          bind_cols(data %>% select(hh_id, parent_unit_id, child_id_1, child_id_2)) %>%
          pivot_longer(
            cols     = -c(hh_id, parent_unit_id, child_id_1, child_id_2),
            names_to = 'joint_class',
            values_to = 'p_joint'
          ) %>%
          separate(joint_class,  into = c('ecec_class_1', 'ecec_class_2'),         sep = '__', extra = 'merge', fill = 'right') %>%
          separate(ecec_class_1, into = c('ecec_choice_1', 'ecec_hours_choice_1'), sep = '_(?=[^_]+$)', extra = 'merge', fill = 'right') %>%
          separate(ecec_class_2, into = c('ecec_choice_2', 'ecec_hours_choice_2'), sep = '_(?=[^_]+$)', extra = 'merge', fill = 'right')
      })
    }) %>%
      set_names(employment_states)

    # Combine into wide format: p_joint.none, p_joint.pt, p_joint.ft
    two_child_join_keys <- c('hh_id', 'parent_unit_id', 'child_id_1', 'child_id_2',
                             'ecec_choice_1', 'ecec_hours_choice_1', 'ecec_choice_2', 'ecec_hours_choice_2')
    two_child_wide <- map(employment_states, function(s) {
      two_child_by_emp[[s]] %>% rename(!!paste0('p_joint.', s) := p_joint)
    }) %>% reduce(left_join, by = two_child_join_keys)

    # -- Assertion: two-child reduce/join didn't duplicate rows ----
    n_expected_2c <- nrow(two_child_by_emp[['none']])
    stopifnot(nrow(two_child_wide) == n_expected_2c)

    list(
      one_child = one_child_wide,
      two_child = two_child_wide
    )
  }


  ecec_result <- {

    set.seed(random_seed_base + 1)

    # Prepare NSECE data for RF training
    nsece_income_pctiles <- compute_income_pctiles(
      processed_nsece_hh$children, processed_nsece_hh$households
    )
    nsece_hh_rf <- prepare_data_for_rf(
      children         = processed_nsece_hh$children,
      parent_units     = processed_nsece_hh$parent_units,
      households       = processed_nsece_hh$households,
      household_members = processed_nsece_hh$household_members,
      enrollment       = processed_nsece_hh$enrollment,
      income_pctiles   = nsece_income_pctiles,
      source_label     = 'NSECE',
      is_nsece         = TRUE
    )

    # Prepare ACS data for RF prediction
    acs_income_pctiles <- compute_income_pctiles(
      processed_acs_2019$children, processed_acs_2019$households
    )
    acs_rf <- prepare_data_for_rf(
      children         = processed_acs_2019$children,
      parent_units     = processed_acs_2019$parent_units,
      households       = processed_acs_2019$households,
      household_members = processed_acs_2019$household_members,
      enrollment       = NULL,
      income_pctiles   = acs_income_pctiles,
      source_label     = 'ACS',
      is_nsece         = FALSE
    )

    # Train ECEC RF models
    rf_models <- {

      # Feature sets include 'region' (Census region: 1=NE, 2=MW, 3=S, 4=W)
      # employment_choice_pc captures primary caregiver employment status (none/pt/ft)
      feature_sets <- list(
        one_child = list(
          `0` = c('age', 'male', 'income_pctile', 'n_members', 'region'),
          `1` = c('age', 'male', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.male1', 'employment_choice_pc'),
          `2` = c('age', 'male', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.age2', 'employment_choice_pc')
        ),
        two_child = list(
          `0` = c('age_1', 'age_2', 'male_1', 'male_2', 'income_pctile', 'n_members', 'region', 'is_3plus'),
          `1` = c('age_1', 'age_2', 'male_1', 'male_2', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.male1', 'employment_choice_pc', 'is_3plus'),
          `2` = c('age_1', 'age_2', 'male_1', 'male_2', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.age2', 'employment_choice_pc', 'is_3plus')
        )
      )

      imap(nsece_hh_rf, function(family_data, family_name) {
        imap(family_data, function(df, parent_count) {
          if (is.null(df) || nrow(df) == 0) {
            return(NULL)
          }

          features <- feature_sets[[family_name]][[parent_count]]
          train_raw <- df %>%
            select(model_weight, ecec_class, all_of(features))

          if (nrow(train_raw) == 0) {
            return(NULL)
          }

          sample_size  <- 50000
          replace_flag <- nrow(train_raw) < sample_size

          train_df <- train_raw %>%
            sample_n(
              size    = sample_size,
              weight  = model_weight,
              replace = replace_flag
            ) %>%
            mutate(ecec_class = factor(ecec_class))

          rf <- ranger(
            formula        = ecec_class ~ .,
            data           = train_df[, c('ecec_class', features)],
            num.trees      = 500,
            min.node.size  = 10,
            mtry           = length(features),
            importance      = 'impurity',
            probability    = TRUE,
            verbose        = FALSE
          )

          # Save RF report
          {
            rf_report_path <- file.path(
              estimation_info$paths$output,
              'models',
              sprintf('ecec_rf_%s_p%s.txt', family_name, parent_count)
            )
            out <- capture.output({
              cat('RANGER MODEL (printed):\n')
              print(rf)
              cat('\nTOP VARIABLE IMPORTANCE:\n')
              if (!is.null(rf$variable.importance)) {
                imp <- sort(rf$variable.importance, decreasing = TRUE)
                print(head(imp, 15))
              } else {
                cat('(not available)\n')
              }
            })
            writeLines(out, con = rf_report_path)
          }

          rf
        })
      })
    }

    # Fit ECEC probabilities from RF predictions
    {

      parent_keys <- c('0', '1', '2')
      employment_states <- c('none', 'pt', 'ft')

      # Get counterfactual predictions for all employment states
      cf_predictions <- predict_enrollment_by_employment(rf_models, acs_rf)

      one_child_wide <- cf_predictions$one_child
      two_child_wide <- cf_predictions$two_child

      # Get marginal probabilities from joint for two-child families
      two_child_marginals <- map(1:2, function(k) {
        two_child_wide %>%
          group_by(
            hh_id, parent_unit_id,
            child_id          = .data[[paste0('child_id_', k)]],
            ecec_choice       = .data[[paste0('ecec_choice_', k)]],
            ecec_hours_choice = .data[[paste0('ecec_hours_choice_', k)]]
          ) %>%
          summarise(
            p_enrollment.none = sum(p_joint.none),
            p_enrollment.pt   = sum(p_joint.pt),
            p_enrollment.ft   = sum(p_joint.ft),
            .groups = 'drop'
          )
      })

      imputations_wide <- bind_rows(one_child_wide, two_child_marginals[[1]], two_child_marginals[[2]])

      #------------------------------------------------------------------------
      # Generate marginal probabilities for extra children in 3+ child families
      #------------------------------------------------------------------------

      children_in_3plus <- processed_acs_2019$children %>%
        filter(age < 5) %>%
        group_by(hh_id, parent_unit_id) %>%
        filter(n() >= 3) %>%
        ungroup()

      if (nrow(children_in_3plus) > 0) {
        children_with_imputations <- imputations_wide %>%
          distinct(hh_id, parent_unit_id, child_id)

        extra_children <- children_in_3plus %>%
          anti_join(children_with_imputations, by = c('hh_id', 'parent_unit_id', 'child_id'))

        if (nrow(extra_children) > 0) {
          # Reuse ACS income percentiles computed above
          extra_children_features <- extra_children %>%
            select(hh_id, child_id, parent_unit_id, child_weight, age, male) %>%
            left_join(
              processed_acs_2019$parent_units %>%
                select(
                  hh_id, parent_unit_id, p.hhm_id1 = hhm_id1, p.hhm_id2 = hhm_id2,
                  p.male1 = male1, p.male2 = male2, p.age1 = age1, p.age2 = age2,
                  p.hours1 = hours1, p.hours2 = hours2
                ),
              by = c('hh_id', 'parent_unit_id')
            ) %>%
            left_join(
              processed_acs_2019$households %>%
                select(hh_id, income, n_members, region),
              by = 'hh_id'
            ) %>%
            mutate(
              income_pctile = cut(income, c(-Inf, acs_income_pctiles, Inf), labels = 1:100) %>%
                as.character() %>% as.integer(),
              primary_caregiver = get_primary_caregiver(p.hhm_id2, p.male1, p.male2),
              hours_pc = if_else(primary_caregiver == 1L, p.hours1, p.hours2),
              employment_choice_pc = derive_employment_choice(hours_pc),
              region = factor(region)
            ) %>%
            select(-income, -hours_pc, -primary_caregiver)

          # Run one-child RF model for each employment state
          extra_child_by_emp <- map(employment_states, function(emp_state) {
            map_dfr(parent_keys, function(parent_count) {
              model <- rf_models$one_child[[parent_count]]

              if (parent_count == '0') {
                data <- extra_children_features %>%
                  filter(is.na(parent_unit_id))
              } else if (parent_count == '1') {
                data <- extra_children_features %>%
                  filter(!is.na(parent_unit_id), is.na(p.hhm_id2),
                         !is.na(p.male1), !is.na(p.age1), !is.na(region))
              } else {
                data <- extra_children_features %>%
                  filter(!is.na(parent_unit_id), !is.na(p.hhm_id2),
                         !is.na(p.male1), !is.na(p.age1), !is.na(p.age2), !is.na(region))
              }

              if (is.null(model) || nrow(data) == 0) {
                return(tibble())
              }

              if (parent_count == '0') {
                predictions <- predict(model, data = data)$predictions
              } else {
                data_cf <- data %>%
                  mutate(employment_choice_pc = factor(emp_state, levels = c('none', 'pt', 'ft')))
                predictions <- predict(model, data = data_cf)$predictions
              }

              as.data.frame(predictions) %>%
                tibble() %>%
                bind_cols(data %>% select(hh_id, parent_unit_id, child_id)) %>%
                pivot_longer(
                  cols      = -c(hh_id, parent_unit_id, child_id),
                  names_to  = 'ecec_class',
                  values_to = 'p_enrollment'
                ) %>%
                separate(
                  ecec_class,
                  into  = c('ecec_choice', 'ecec_hours_choice'),
                  sep   = '_(?=[^_]+$)',
                  extra = 'merge',
                  fill  = 'right'
                )
            })
          }) %>%
            set_names(employment_states)

          # Combine into wide format
          extra_child_wide <- extra_child_by_emp$none %>%
            rename(p_enrollment.none = p_enrollment) %>%
            left_join(
              extra_child_by_emp$pt %>% rename(p_enrollment.pt = p_enrollment),
              by = c('hh_id', 'parent_unit_id', 'child_id', 'ecec_choice', 'ecec_hours_choice')
            ) %>%
            left_join(
              extra_child_by_emp$ft %>% rename(p_enrollment.ft = p_enrollment),
              by = c('hh_id', 'parent_unit_id', 'child_id', 'ecec_choice', 'ecec_hours_choice')
            )

          imputations_wide <- bind_rows(imputations_wide, extra_child_wide)
        }
      }

      # Determine who is in the universe
      valid_children <- imputations_wide %>%
        distinct(hh_id, parent_unit_id, child_id)

      # Fit values for universe (marginal probabilities per child)
      enrollment <- processed_acs_2019$children %>%
        filter(age < 5) %>%
        semi_join(valid_children, by = c('hh_id', 'parent_unit_id', 'child_id')) %>%
        expand_grid(
          imputations_wide %>%
            distinct(ecec_choice, ecec_hours_choice)
        ) %>%
        left_join(imputations_wide, by = c('hh_id', 'parent_unit_id', 'child_id', 'ecec_choice', 'ecec_hours_choice')) %>%
        mutate(
          p_enrollment.none = replace_na(p_enrollment.none, 0),
          p_enrollment.pt = replace_na(p_enrollment.pt, 0),
          p_enrollment.ft = replace_na(p_enrollment.ft, 0)
        ) %>%
        select(-tax_unit_id, -age, -male)

      # -- Assertions: RF imputation integrity ----

      # Enrollment probabilities across care types must sum to ~1 for each
      # (child x employment state). If the RF is miscalibrated or the pivot
      # lost categories, they won't sum correctly.
      prob_sums <- enrollment %>%
        group_by(hh_id, parent_unit_id, child_id) %>%
        summarise(
          sum_none = sum(p_enrollment.none),
          sum_pt = sum(p_enrollment.pt),
          sum_ft = sum(p_enrollment.ft),
          .groups = 'drop'
        )
      stopifnot(all(abs(prob_sums$sum_none - 1.0) < 0.01))
      stopifnot(all(abs(prob_sums$sum_pt - 1.0) < 0.01))
      stopifnot(all(abs(prob_sums$sum_ft - 1.0) < 0.01))

      # Enrollment probabilities should vary across children (if all children
      # get the same distribution, the RF features aren't differentiating)
      if (nrow(prob_sums) > 1) {
        spot_check <- enrollment %>%
          filter(ecec_choice == 'Parent Only', ecec_hours_choice == 'none')
        if (nrow(spot_check) > 1) {
          stopifnot(sd(spot_check$p_enrollment.none) > 0)
        }
      }

      list(
        enrollment = enrollment,
        enrollment_joint = two_child_wide
      )
    }
  }

  #-------------------------------------------
  # ECEC imputation report
  #-------------------------------------------
  {

    # NSECE distribution
    nsece_children <- processed_nsece_hh$children %>%
      filter(age < 5) %>%
      left_join(
        processed_nsece_hh$enrollment %>%
          select(hh_id, child_id, ecec_choice, ecec_hours_choice),
        by = c('hh_id', 'child_id')
      ) %>%
      filter(!is.na(ecec_choice), !is.na(ecec_hours_choice))

    total_nsece_weight <- sum(nsece_children$child_weight)

    nsece_distribution <- nsece_children %>%
      count(ecec_choice, ecec_hours_choice, wt = child_weight, name = 'weighted_children') %>%
      mutate(mean_y = weighted_children / total_nsece_weight) %>%
      select(ecec_choice, ecec_hours_choice, mean_y)

    # ACS distribution (using observed employment state probabilities)
    acs_child_weights <- ecec_result$enrollment %>%
      distinct(hh_id, parent_unit_id, child_id, child_weight)

    total_acs_weight <- sum(acs_child_weights$child_weight)

    acs_emp_state <- processed_acs_2019$parent_units %>%
      distinct(hh_id, parent_unit_id, hhm_id2, male1, male2, hours1, hours2) %>%
      mutate(
        primary_caregiver = get_primary_caregiver(hhm_id2, male1, male2),
        hours_pc = if_else(primary_caregiver == 1L, hours1, hours2),
        emp_state = case_when(
          is.na(hours_pc) | hours_pc == 0 ~ 'none',
          hours_pc < 35                   ~ 'pt',
          TRUE                            ~ 'ft'
        )
      ) %>%
      select(hh_id, parent_unit_id, emp_state)

    acs_distribution <- ecec_result$enrollment %>%
      left_join(acs_emp_state, by = c('hh_id', 'parent_unit_id')) %>%
      mutate(
        emp_state = replace_na(emp_state, 'none'),
        p_enrollment_obs = case_when(
          emp_state == 'none' ~ p_enrollment.none,
          emp_state == 'pt'   ~ p_enrollment.pt,
          emp_state == 'ft'   ~ p_enrollment.ft,
          TRUE                ~ p_enrollment.none
        )
      ) %>%
      group_by(ecec_choice, ecec_hours_choice) %>%
      summarise(predicted_weight = sum(child_weight * p_enrollment_obs), .groups = 'drop') %>%
      mutate(mean_yhat = predicted_weight / total_acs_weight) %>%
      select(ecec_choice, ecec_hours_choice, mean_yhat)

    # Comparison
    unconditional_comparison <- full_join(
      nsece_distribution,
      acs_distribution,
      by = c('ecec_choice', 'ecec_hours_choice')
    ) %>%
      mutate(
        mean_y    = replace_na(mean_y, 0),
        mean_yhat = replace_na(mean_yhat, 0),
        diff      = mean_yhat - mean_y
      ) %>%
      arrange(-mean_y)

    estimation_info$paths$output %>%
      file.path('models', 'nsece_acs_comp.csv') %>%
      write_csv(unconditional_comparison, .)


    # Distribution by parent count
    nsece_by_parent_count <- nsece_children %>%
      left_join(
        processed_nsece_hh$parent_units %>%
          select(hh_id, parent_unit_id, n_parents = hhm_id2) %>%
          mutate(n_parents = if_else(is.na(n_parents), 1, 2)),
        by = c('hh_id', 'parent_unit_id')
      ) %>%
      filter(!is.na(n_parents), n_parents %in% c(1, 2))

    # NSECE distributions by parent count
    choice_by_parents <- compute_distribution_by_parents(
      nsece_by_parent_count, 'ecec_choice', expr(child_weight)
    ) %>% mutate(category = 'Child care type', .before = 1)

    hours_by_parents <- compute_distribution_by_parents(
      nsece_by_parent_count, 'ecec_hours_choice', expr(child_weight)
    ) %>% mutate(category = 'Child care hours', .before = 1)

    # ACS distributions by parent count
    acs_by_parent_count <- ecec_result$enrollment %>%
      left_join(acs_emp_state, by = c('hh_id', 'parent_unit_id')) %>%
      mutate(
        emp_state = replace_na(emp_state, 'none'),
        p_enrollment_obs = case_when(
          emp_state == 'none' ~ p_enrollment.none,
          emp_state == 'pt'   ~ p_enrollment.pt,
          emp_state == 'ft'   ~ p_enrollment.ft,
          TRUE                ~ p_enrollment.none
        )
      ) %>%
      left_join(
        processed_acs_2019$parent_units %>%
          select(hh_id, parent_unit_id, n_parents = hhm_id2) %>%
          mutate(n_parents = if_else(is.na(n_parents), 1, 2)),
        by = c('hh_id', 'parent_unit_id')
      ) %>%
      filter(!is.na(n_parents), n_parents %in% c(1, 2))

    acs_choice_by_parents <- compute_distribution_by_parents(
      acs_by_parent_count, 'ecec_choice', expr(child_weight * p_enrollment_obs), prefix = 'acs_'
    ) %>% mutate(category = 'Child care type', .before = 1)

    acs_hours_by_parents <- compute_distribution_by_parents(
      acs_by_parent_count, 'ecec_hours_choice', expr(child_weight * p_enrollment_obs), prefix = 'acs_'
    ) %>% mutate(category = 'Child care hours', .before = 1)

    # Combine NSECE and ACS data
    parent_count_table <- bind_rows(
      choice_by_parents %>%
        left_join(acs_choice_by_parents, by = c('category', 'subcategory')),
      tibble(
        category = 'Child care type',
        subcategory = 'Total',
        n_parents_1 = sum(choice_by_parents$n_parents_1, na.rm = TRUE),
        n_parents_2 = sum(choice_by_parents$n_parents_2, na.rm = TRUE),
        acs_n_parents_1 = sum(acs_choice_by_parents$acs_n_parents_1, na.rm = TRUE),
        acs_n_parents_2 = sum(acs_choice_by_parents$acs_n_parents_2, na.rm = TRUE)
      ),
      hours_by_parents %>%
        left_join(acs_hours_by_parents, by = c('category', 'subcategory')),
      tibble(
        category = 'Child care hours',
        subcategory = 'Total',
        n_parents_1 = sum(hours_by_parents$n_parents_1, na.rm = TRUE),
        n_parents_2 = sum(hours_by_parents$n_parents_2, na.rm = TRUE),
        acs_n_parents_1 = sum(acs_hours_by_parents$acs_n_parents_1, na.rm = TRUE),
        acs_n_parents_2 = sum(acs_hours_by_parents$acs_n_parents_2, na.rm = TRUE)
      )
    ) %>%
      rename(
        Category = category,
        Subcategory = subcategory,
        'NSECE: One' = n_parents_1,
        'NSECE: Two' = n_parents_2,
        'ACS: One' = acs_n_parents_1,
        'ACS: Two' = acs_n_parents_2
      )

    estimation_info$paths$output %>%
      file.path('models', 'nsece_parent_count_comp.csv') %>%
      write_csv(parent_count_table, .)
  }

  processed_acs_2019$enrollment <- ecec_result$enrollment
  processed_acs_2019$enrollment_joint <- ecec_result$enrollment_joint

  #-------------------------------------------
  # Expand parent labor supply
  #-------------------------------------------

  processed_acs_2019$parent_units <- {

    parent_units <- processed_acs_2019$parent_units %>%
      rename(
        baseline_earnings1 = earnings1,
        baseline_earnings2 = earnings2
      )

    # Extract primary caregiver attributes
    caregiver_base <- parent_units %>%
      mutate(
        primary_caregiver = get_primary_caregiver(hhm_id2, male1, male2)
      ) %>%
      mutate(
        hhm_id_pc          = if_else(primary_caregiver == 1, hhm_id1, hhm_id2),
        per_weight_pc      = if_else(primary_caregiver == 1, per_weight1, per_weight2),
        hours_pc           = if_else(primary_caregiver == 1, hours1, hours2),
        weeks_pc           = if_else(primary_caregiver == 1, weeks1, weeks2),
        earnings_pc        = if_else(primary_caregiver == 1, baseline_earnings1, baseline_earnings2),
        hourly_earnings_pc = if_else(primary_caregiver == 1, hourly_earnings1, hourly_earnings2)
      ) %>%
      mutate(
        employment_choice_observed = case_when(
          is.na(hours_pc) | hours_pc == 0 ~ 'none',
          hours_pc < 35 ~ 'pt',
          hours_pc >= 35 ~ 'ft',
          TRUE ~ 'none'
        )
      )

    # Calculate mean hours and weeks by employment status for counterfactuals
    mean_hours_weeks <- caregiver_base %>%
      group_by(employment_choice = employment_choice_observed) %>%
      summarise(
        mean_hours = weighted.mean(hours_pc, per_weight_pc, na.rm = TRUE),
        mean_weeks = weighted.mean(weeks_pc, per_weight_pc, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      complete(employment_choice = c('none', 'pt', 'ft'), fill = list(mean_hours = 0, mean_weeks = 0))

    # Create three records per parent unit (one observed + two counterfactuals)
    expanded_parent_units <- caregiver_base %>%
      expand_grid(employment_choice = c('none', 'pt', 'ft')) %>%
      left_join(mean_hours_weeks, by = 'employment_choice') %>%
      mutate(
        p_employment = as.integer(employment_choice == employment_choice_observed),

        hours_choice    = if_else(p_employment == 1, hours_pc, mean_hours),
        weeks_choice    = if_else(p_employment == 1, weeks_pc, mean_weeks),
        earnings_choice = if_else(p_employment == 1, earnings_pc, hourly_earnings_pc * hours_choice * weeks_choice),
        earnings_delta = earnings_choice - earnings_pc,

        # Update hours/weeks/earnings for primary caregiver parent
        hours1 = case_when(
          is.na(hhm_id2)        ~ hours_choice,
          primary_caregiver == 1 ~ hours_choice,
          primary_caregiver == 2 ~ hours1
        ),
        hours2 = case_when(
          is.na(hhm_id2)        ~ NA,
          primary_caregiver == 1 ~ hours2,
          primary_caregiver == 2 ~ hours_choice
        ),
        weeks1 = case_when(
          is.na(hhm_id2)        ~ weeks_choice,
          primary_caregiver == 1 ~ weeks_choice,
          primary_caregiver == 2 ~ weeks1
        ),
        weeks2 = case_when(
          is.na(hhm_id2)        ~ NA,
          primary_caregiver == 1 ~ weeks2,
          primary_caregiver == 2 ~ weeks_choice
        ),
        earnings1 = case_when(
          is.na(baseline_earnings2) ~ earnings_choice,
          primary_caregiver == 1    ~ earnings_choice,
          primary_caregiver == 2    ~ baseline_earnings1
        ),
        earnings2 = case_when(
          is.na(baseline_earnings2) ~ NA,
          primary_caregiver == 1    ~ baseline_earnings2,
          primary_caregiver == 2    ~ earnings_choice
        )
      ) %>%
      select(
        -hhm_id_pc, -per_weight_pc, -hours_pc, -weeks_pc, -earnings_pc,
        -hourly_earnings_pc, -employment_choice_observed, -mean_hours, -mean_weeks,
        -hours_choice, -weeks_choice, -earnings_choice
      ) %>%
      relocate(employment_choice, .after = tax_unit_id2)

    # Generate AGI data
    agi_data <- {

      processed_acs_2019_temp_parent_units <- expanded_parent_units

      processed_acs_2019_temp_parent_units %>%
        left_join(
          processed_acs_2019$tax_units %>%
            select(hh_id, tax_unit_id1 = tax_unit_id, filing_status1 = filing_status, n_dep_u13_1 = n_dep_u13, agi1 = agi),
          by = c('hh_id', 'tax_unit_id1')
        ) %>%
        left_join(
          processed_acs_2019$tax_units %>%
            select(hh_id, tax_unit_id2 = tax_unit_id, filing_status2 = filing_status, n_dep_u13_2 = n_dep_u13, agi2 = agi),
          by = c('hh_id', 'tax_unit_id2')
        ) %>%
        mutate(
          across(
            .cols = c(filing_status2, n_dep_u13_2, agi2),
            .fns  = ~ if_else(is.na(tax_unit_id2) | (tax_unit_id1 == tax_unit_id2), NA, .)
          )
        ) %>%
        select(hh_id, parent_unit_id, employment_choice, filing_status1, filing_status2, n_dep_u13_1, n_dep_u13_2, agi1, agi2)
    }

    # Join AGI to expanded parent_units and adjust based on earnings_delta
    expanded_parent_units %>%
      left_join(agi_data, by = c('hh_id', 'parent_unit_id', 'employment_choice')) %>%
      mutate(
        baseline_agi1 = agi1,
        baseline_agi2 = agi2,

        earnings_delta1 = case_when(
          is.na(agi2)            ~ earnings_delta,
          primary_caregiver == 1 ~ earnings_delta,
          primary_caregiver == 2 ~ 0
        ),
        earnings_delta2 = case_when(
          is.na(agi2)            ~ NA_real_,
          primary_caregiver == 1 ~ 0,
          primary_caregiver == 2 ~ earnings_delta
        ),

        agi1 = case_when(
          is.na(agi2)            ~ agi1 + earnings_delta,
          primary_caregiver == 1 ~ agi1 + earnings_delta,
          primary_caregiver == 2 ~ agi1
        ),
        agi2 = case_when(
          is.na(agi2)            ~ NA,
          primary_caregiver == 1 ~ agi2,
          primary_caregiver == 2 ~ agi2 + earnings_delta
        )
      ) %>%
      select(-earnings_delta)
  }

  # Preserve observed employment choice before RF replacement
  observed_emp_lookup <- processed_acs_2019$parent_units %>%
    filter(p_employment == 1) %>%
    select(hh_id, parent_unit_id, employment_observed = employment_choice)

  processed_acs_2019$parent_units <- processed_acs_2019$parent_units %>%
    left_join(observed_emp_lookup, by = c('hh_id', 'parent_unit_id'))

  #-------------------------------------------
  # Employment probability RF
  #-------------------------------------------

  employment_rf <- {

    train_data <- processed_acs_2019$parent_units %>%
      filter(p_employment == 1) %>%
      mutate(
        emp_observed = factor(employment_choice, levels = c('none', 'pt', 'ft')),
        per_weight_pc = if_else(primary_caregiver == 1, per_weight1, per_weight2)
      ) %>%
      extract_pc_features() %>%
      select(
        emp_observed, per_weight_pc,
        pc_age, pc_educ, pc_married, pc_race, pc_hispanic, pc_hourly_earnings,
        partner_earnings, partner_hours, n_children, has_partner, pc_male
      ) %>%
      filter(complete.cases(.))

    required_vars <- c(
      'pc_age', 'pc_educ', 'pc_married', 'pc_race', 'pc_hispanic',
      'pc_hourly_earnings', 'partner_earnings', 'partner_hours',
      'n_children', 'has_partner', 'pc_male'
    )
    missing_vars <- setdiff(required_vars, names(train_data))
    if (length(missing_vars) > 0) {
      stop('train_employment_rf: missing required predictors: ',
           paste(missing_vars, collapse = ', '))
    }

    ranger(
      emp_observed ~ pc_age + pc_educ + pc_married + pc_race + pc_hispanic +
        pc_hourly_earnings + partner_earnings + partner_hours + n_children + has_partner + pc_male,
      data = train_data,
      num.trees = 500,
      probability = TRUE,
      case.weights = train_data$per_weight_pc,
      importance = 'impurity',
      seed = 42
    )
  }

  # Predict employment probabilities
  emp_probs <- {

    pred_data <- processed_acs_2019$parent_units %>%
      group_by(hh_id, parent_unit_id) %>%
      slice_max(order_by = coalesce(p_employment, -Inf), n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      extract_pc_features()

    # Handle missing values - use median imputation for prediction
    pred_data <- pred_data %>%
      mutate(
        pc_age = replace_na(pc_age, median(pc_age, na.rm = TRUE)),
        pc_educ = replace_na(pc_educ, median(pc_educ, na.rm = TRUE)),
        pc_married = replace_na(pc_married, 0),
        pc_race = replace_na(pc_race, 1),
        pc_hispanic = replace_na(pc_hispanic, 0),
        pc_hourly_earnings = replace_na(pc_hourly_earnings, median(pc_hourly_earnings, na.rm = TRUE)),
        partner_earnings = replace_na(partner_earnings, 0),
        partner_hours = replace_na(partner_hours, 0),
        n_children = replace_na(n_children, 1),
        has_partner = replace_na(has_partner, 0),
        pc_male = replace_na(pc_male, 0)
      )

    required_vars <- employment_rf$forest$independent.variable.names
    missing_vars <- setdiff(required_vars, names(pred_data))
    if (length(missing_vars) > 0) {
      stop('predict_employment_probs: missing required predictors: ',
           paste(missing_vars, collapse = ', '))
    }

    preds <- predict(employment_rf, data = pred_data, type = 'response')
    prob_matrix <- preds$predictions

    pred_data %>%
      select(hh_id, parent_unit_id) %>%
      mutate(
        p_emp.none = prob_matrix[, 'none'],
        p_emp.pt = prob_matrix[, 'pt'],
        p_emp.ft = prob_matrix[, 'ft']
      )
  }

  # Replace p_employment with RF probabilities
  processed_acs_2019$parent_units <- processed_acs_2019$parent_units %>%
    left_join(emp_probs, by = c('hh_id', 'parent_unit_id')) %>%
    mutate(
      p_employment = case_when(
        employment_choice == 'none' ~ p_emp.none,
        employment_choice == 'pt' ~ p_emp.pt,
        employment_choice == 'ft' ~ p_emp.ft
      )
    ) %>%
    select(-p_emp.none, -p_emp.pt, -p_emp.ft)

  processed_acs_2019$employment_rf <- employment_rf


  #-------------------------------------------
  # Process SPM units (for poverty analysis)
  #-------------------------------------------

  # Reuse acs_raw already in memory (avoid redundant ~2-4GB reload)
  acs_raw_for_spm <- acs_raw

  sampled_hh_ids <- unique(processed_acs_2019$households$hh_id)
  acs_sampled <- acs_raw_for_spm %>% filter(YEAR == 2019, SERIAL %in% sampled_hh_ids)

  spm_result <- {

    required_vars <- c('SPMFAMUNIT', 'SPMTOTRES', 'SPMTHRESH')
    has_spm_data <- all(required_vars %in% names(acs_sampled))

    if (!has_spm_data) {
      cat('  SPM variables not found in ACS data - poverty analysis will be disabled\n')
      list(
        spm_units = NULL,
        hh_spm_xwalk = NULL,
        has_spm_data = FALSE
      )
    } else {

      cat('  Processing SPM units for poverty analysis...\n')

      acs_spm <- acs_sampled %>%
        filter(
          !is.na(SPMFAMUNIT),
          SPMFAMUNIT != 9999999999,
          SPMTOTRES != 99999999,
          SPMTHRESH != 99999999
        )

      if (nrow(acs_spm) == 0) {
        cat('  No valid SPM records found - poverty analysis will be disabled\n')
        list(
          spm_units = NULL,
          hh_spm_xwalk = NULL,
          has_spm_data = FALSE
        )
      } else {

        wage_col <- 'INCWAGE'
        age_col <- 'AGE'
        weight_col <- 'PERWT'

        if (!'INCBUS00' %in% names(acs_spm)) acs_spm$INCBUS00 <- 0
        if (!'SPMCHXPNS' %in% names(acs_spm)) acs_spm$SPMCHXPNS <- 0
        if (!'SPMWKXPNS' %in% names(acs_spm)) acs_spm$SPMWKXPNS <- 0
        if (!'SPMCAPXPNS' %in% names(acs_spm)) acs_spm$SPMCAPXPNS <- 0

        acs_spm <- acs_spm %>%
          mutate(
            SPMCHXPNS = if_else(SPMCHXPNS == 99999, 0, as.numeric(SPMCHXPNS)),
            SPMWKXPNS = if_else(SPMWKXPNS == 99999, 0, as.numeric(SPMWKXPNS)),
            SPMCAPXPNS = if_else(SPMCAPXPNS == 99999, 0, as.numeric(SPMCAPXPNS))
          )

        spm_units <- acs_spm %>%
          group_by(hh_id = SERIAL, spm_unit_id = SPMFAMUNIT) %>%
          summarise(
            spm_resources = first(SPMTOTRES),
            spm_threshold = first(SPMTHRESH),
            spm_childcare_exp = first(SPMCHXPNS),
            spm_work_exp = first(SPMWKXPNS),
            spm_capped_exp = first(SPMCAPXPNS),
            spm_weight = sum(PERWT, na.rm = TRUE),
            n_members = n(),
            n_children = sum(AGE < 18),
            n_adults = sum(AGE >= 18),
            n_members_weighted = sum(PERWT, na.rm = TRUE),
            n_children_weighted = sum(PERWT * (AGE < 18), na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            is_poor_baseline = (spm_resources < spm_threshold)
          )

        # Compute expense cap: min earnings among parents/spouses in the SPM unit
        has_relate <- 'RELATE' %in% names(acs_spm)
        if (!has_relate) {
          warning('  RELATE not found in ACS SPM data; falling back to all-adult earnings cap.')
        }

        spm_parent_earnings <- acs_spm %>%
          mutate(
            wages = if_else(!is.na(INCWAGE) & INCWAGE < 999999, as.numeric(INCWAGE), 0),
            se_earnings = if_else(!is.na(INCBUS00) & INCBUS00 < 999999, as.numeric(INCBUS00), 0),
            total_earnings = pmax(0, wages) + pmax(0, se_earnings),
            is_parent_proxy = if (has_relate) (!is.na(RELATE) & RELATE %in% c(1, 2, 11)) else TRUE
          ) %>%
          filter(!is.na(AGE) & AGE >= 18 & is_parent_proxy) %>%
          select(
            hh_id = SERIAL,
            spm_unit_id = SPMFAMUNIT,
            per_id = PERNUM,
            baseline_earnings = total_earnings
          )

        spm_caps <- spm_parent_earnings %>%
          filter(baseline_earnings > 0) %>%
          group_by(hh_id, spm_unit_id) %>%
          summarise(
            min_adult_earnings = min(baseline_earnings, na.rm = TRUE),
            .groups = 'drop'
          )

        spm_units <- spm_units %>%
          left_join(spm_caps, by = c('hh_id', 'spm_unit_id')) %>%
          mutate(min_adult_earnings = coalesce(min_adult_earnings, 0))

        # Adjust weights if ACS sample was subsetted
        if (calib_sample < 100) {
          weight_factor <- 100 / calib_sample
          spm_units <- spm_units %>%
            mutate(
              spm_weight = spm_weight * weight_factor,
              n_members_weighted = n_members_weighted * weight_factor,
              n_children_weighted = n_children_weighted * weight_factor
            )
        }

        cat('    SPM units processed:', nrow(spm_units), '\n')
        cat('    SPM parent records:', nrow(spm_parent_earnings), '\n')
        cat('    Baseline poverty rate:',
            round(100 * sum(spm_units$n_members_weighted * spm_units$is_poor_baseline) /
                  sum(spm_units$n_members_weighted), 1), '%\n')

        list(
          spm_units = spm_units,
          spm_parent_earnings = spm_parent_earnings,
          has_spm_data = TRUE
        )
      }
    }
  }

  if (spm_result$has_spm_data) {
    processed_acs_2019$spm_units <- spm_result$spm_units
    processed_acs_2019$spm_parent_earnings <- spm_result$spm_parent_earnings

    # Create parent_unit -> spm_unit crosswalk
    # spm_unit_id is only unique within household, so crosswalk includes hh_id
    if ('SPMFAMUNIT' %in% names(acs_sampled)) {
      pu_spm_xwalk <- processed_acs_2019$children %>%
        select(hh_id, child_id, parent_unit_id) %>%
        left_join(
          acs_sampled %>% select(hh_id = SERIAL, child_id = PERNUM, spm_unit_id = SPMFAMUNIT),
          by = c('hh_id', 'child_id')
        ) %>%
        filter(!is.na(spm_unit_id), spm_unit_id != 9999999999) %>%
        group_by(hh_id, parent_unit_id) %>%
        summarise(spm_unit_id = first(spm_unit_id), .groups = 'drop') %>%
        left_join(
          processed_acs_2019$parent_units %>%
            select(hh_id, parent_unit_id, hhm_id1, hhm_id2) %>%
            distinct(hh_id, parent_unit_id, .keep_all = TRUE),
          by = c('hh_id', 'parent_unit_id')
        )

      processed_acs_2019$pu_spm_xwalk <- pu_spm_xwalk
    }
  }


  # Free raw ACS data no longer needed (~2-4GB)
  rm(acs_raw, acs_raw_for_spm, acs_sampled)

  #-------------------------------------------
  # Write 2019 ACS data to disk
  #-------------------------------------------

  c('households', 'household_members', 'parent_units', 'children', 'tax_units',
    'enrollment', 'enrollment_joint') %>%
    walk(
      .f = function(table_name) {
        estimation_info$paths$data %>%
          file.path(paste0('acs_', table_name, '_2019.csv')) %>%
          fwrite(processed_acs_2019[[table_name]], file = ., na = 'NA')
      }
    )

  if (spm_result$has_spm_data && !is.null(processed_acs_2019$pu_spm_xwalk)) {
    fwrite(
      processed_acs_2019$spm_units,
      file.path(estimation_info$paths$data, 'acs_spm_units_2019.csv'),
      na = 'NA'
    )
    fwrite(
      processed_acs_2019$pu_spm_xwalk,
      file.path(estimation_info$paths$data, 'acs_pu_spm_xwalk_2019.csv'),
      na = 'NA'
    )
    fwrite(
      processed_acs_2019$spm_parent_earnings,
      file.path(estimation_info$paths$data, 'acs_spm_parent_earnings_2019.csv'),
      na = 'NA'
    )
  }

  cat('ACS data processing complete!\n')
}
