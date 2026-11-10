# 1a_nsece_processing.R
#
# NSECE processing: household demand data, supply parameters,
# and price heterogeneity model training. Called from main.R.


run_nsece_processing <- function(nsece_year = 2019, nsece_2024_options = character(0)) {

  #----------------------------------------------------------------------------
  # Processes NSECE data: household demand data, supply parameters,
  # and price heterogeneity QRF models. Writes to estimation output folder.
  # The 2019 survey is the default; the 2024 survey is processed the same way
  # (see get_nsece_year_settings()). Household tables carry the survey year
  # (nsece_*_<survey year>.csv). Supply parameters are written in dollars of
  # the model base year, the ACS year paired with the survey
  # (NSECE_ACS_YEAR_PAIRS), as supply_<base year>.yaml.
  #
  # Params:
  #   - nsece_year (int): Survey year to process, 2019 (default) or 2024
  #   - nsece_2024_options (chr): optional 2024 data treatments, any of
  #       NSECE_2024_OPTIONS ('weights', 'supply'; see constants.R).
  #       Ignored for 2019. Default none.
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  # Optional treatments, 2024 survey only (missing-hours fill always on, both
  # years -- see parent units below)
  use_2024_weights <- nsece_year == 2024 && 'weights' %in% nsece_2024_options
  use_2024_supply  <- nsece_year == 2024 && 'supply'  %in% nsece_2024_options
  if (nsece_year == 2024) {
    cat('2024 NSECE options:',
        if (length(nsece_2024_options) == 0) 'none' else paste(nsece_2024_options, collapse = ', '), '\n')
  }

  # Get estimation data info
  estimation_info <- get_estimation_info()

  # File names, the low/high center price cut, and code harmonization for
  # the chosen survey year
  nsece_settings <- get_nsece_year_settings(nsece_year)

  # Model base year: the ACS year paired with this survey (2019 -> 2019,
  # 2024 -> 2024). Supply-side dollar values are written in base-year dollars;
  # with equal years the conversion factors are 1.
  base_year <- unname(NSECE_ACS_YEAR_PAIRS[as.character(nsece_year)])
  dollar_conversion <- get_nsece_dollar_conversion(
    nsece_year, base_year, estimation_info$paths$`Macro-Projections`
  )
  if (nsece_year != base_year) {
    cat('NSECE', nsece_year, 'dollars will be converted to', base_year, 'dollars:',
        'wages divided by', round(dollar_conversion$wage_factor, 4), '(hourly wage index),',
        'non-labor price residuals and the Other Paid price by', round(dollar_conversion$cpi_factor, 4), '(CPI-U)\n')
  }


  # Helper functions
  nsece_admin_classify <- function(value, miss_code = -1, admin_code = -8) {

    #--------------------------------------------------------------------------
    # Classify NSECE values as OK/MISS/ADMIN based on sign codes.
    #
    # Params:
    #   - value (dbl): NSECE variable value to classify
    #   - miss_code (int): Code indicating missing data (default -1)
    #   - admin_code (int): Code indicating administrative suppression (default -8)
    #
    # Returns: (chr) Status string ('OK', 'MISS', or 'ADMIN')
    #--------------------------------------------------------------------------

    case_when(
      value == miss_code  ~ 'MISS',
      value == admin_code ~ 'ADMIN',
      value >= 0          ~ 'OK'
    )
  }

  recode_missing_admin_to_na <- function(value, status) {

    #--------------------------------------------------------------------------
    # Recode MISS/ADMIN status values to NA.
    #
    # Params:
    #   - value (dbl): Original NSECE variable value
    #   - status (chr): Classification status from nsece_admin_classify
    #
    # Returns: (dbl) Value with MISS/ADMIN recoded to NA
    #--------------------------------------------------------------------------

    case_when(
      status %in% c('MISS', 'ADMIN') ~ NA,
      TRUE                            ~ value
    )
  }

  recode_admin_na <- function(value, miss_code = -1, admin_code = -8) {

    #--------------------------------------------------------------------------
    # Combined: classify then recode MISS/ADMIN to NA in one step.
    #
    # Params:
    #   - value (dbl): NSECE variable value to classify and recode
    #   - miss_code (int): Code indicating missing data (default -1)
    #   - admin_code (int): Code indicating administrative suppression (default -8)
    #
    # Returns: (dbl) Value with missing/admin recoded to NA
    #--------------------------------------------------------------------------

    status <- nsece_admin_classify(value, miss_code, admin_code)
    recode_missing_admin_to_na(value, status)
  }


  train_single_qrf <- function(df, target_col, name) {

    #--------------------------------------------------------------------------
    # Trains a single quantile regression forest model.
    #
    # Params:
    #   - df (df): Input dataframe with predictor and target columns
    #   - target_col (chr): Name of the target column to predict
    #   - name (chr): Display name for logging progress
    #
    # Returns: (ranger) Trained QRF model with region_levels attached
    #--------------------------------------------------------------------------

    # Filter out NAs
    df <- df %>%
      filter(
        !is.na(income_pctile),
        !is.na(region),
        !is.na(n_parents),
        !is.na(child_age),
        !is.na(.data[[target_col]])
      )

    sample_size  <- 50000
    replace_flag <- nrow(df) < sample_size

    train_df <- df %>%
      sample_n(size = sample_size, weight = weight, replace = replace_flag) %>%
      rename(target = !!sym(target_col)) %>%
      select(target, income_pctile, region, n_parents, child_age)

    # Train ranger with quantreg = TRUE for quantile regression forest
    qrf <- ranger(
      formula       = target ~ income_pctile + region + n_parents + child_age,
      data          = train_df,
      num.trees     = 500,
      mtry          = 4,
      min.node.size = 10,
      quantreg      = TRUE,
      keep.inbag    = TRUE
    )

    # Store factor levels for prediction alignment
    qrf$region_levels <- levels(train_df$region)

    qrf
  }


  # --- SECTION 1: Process household (demand-side) NSECE data ---
  cat('Processing', nsece_year, 'NSECE household data...\n')

  nsece_hh <- read_nsece_file(estimation_info$paths$NSECE, nsece_settings, 'household')

  processed_nsece_hh <- {

    nsece_hh <- nsece_hh %>%

      # Normalize the hours naming convention: they come as PROVY_X (provider, child).
      # Rename them to PROV_X_Y (child, provider) so they match the rest.
      rename_with(
        .cols = matches('^HH9_HRS_WEEK_PROV[0-9]+_[0-9]+$'),
        .fn   = ~ str_replace(.x, '^HH9_HRS_WEEK_PROV([0-9]+)_([0-9]+)$', 'HH9_HRS_WEEK_PROV_\\2_\\1')
      ) %>%
      rename_with(
        .cols = matches('^HH9_HOURS_WEEK_PROV[0-9]+_[0-9]+$'),
        .fn   = ~ str_replace(.x, '^HH9_HOURS_WEEK_PROV([0-9]+)_([0-9]+)$','HH9_HRS_WEEK_PROV_\\2_\\1')
      )

    households <- {
      nsece_hh %>%
        mutate(
          resp_male = if_else(HH9_RGENDER != 3, as.integer(HH9_RGENDER == 1), NA),
          resp_has_partner = case_when(
            HH9_RSPOUSEPARTNER_LOOP > 0   ~ 1,
            HH9_RSPOUSEPARTNER_LOOP == -1 ~ 0,
            T ~ NA
          ),
          resp_partner = if_else(HH9_RSPOUSEPARTNER_LOOP < 0, NA, HH9_RSPOUSEPARTNER_LOOP),
          # Census region: 1=Northeast, 2=Midwest, 3=South, 4=West
          region = HH9_REGION
        ) %>%
        select(
          hh_id        = HH9_METH_CASEID,
          hh_weight    = HH9_METH_WEIGHT,
          n_members    = HH9_HHCOMP_MEMBERS,
          income       = HH9_ECON_INCOME_ANNUAL,
          region,
          resp_male,
          resp_has_partner,
          resp_partner
        )
    }

    children <- {
      child_age_cols      <- grep('^HHC9_AGE_[1-9]$',                  names(nsece_hh), value = T)
      # Age in months as of September 1 (variable name includes the relevant year, e.g. "..._18_")
      child_age_sep1_cols <- grep('^HHC9_AGE_AT_SEP1_[0-9]+_[1-9]$',  names(nsece_hh), value = TRUE)
      child_gender_cols   <- grep('^HHC9_GENDER_[1-9]$',               names(nsece_hh), value = T)
      child_wt_cols       <- grep('^HHC9_METH_WEIGHT_[1-9]$',          names(nsece_hh), value = T)

      nsece_hh %>%

        # Reshape long in child, wide in variable
        select(
          hh_id = HH9_METH_CASEID,
          all_of(child_age_cols),
          all_of(child_age_sep1_cols),
          all_of(child_gender_cols),
          all_of(child_wt_cols),
        ) %>%
        pivot_longer(
          cols      = -hh_id,
          names_to  = 'variable',
          values_to = 'value'
        ) %>%
        mutate(
          name = case_when(
            str_detect(variable, 'AGE_AT_SEP1_') ~ 'age_months_sep1',
            str_detect(variable, 'AGE_')         ~ 'age_months',
            str_detect(variable, 'GENDER') ~ 'sex',
            str_detect(variable, 'WEIGHT') ~ 'child_weight'
          ),
          child_id = as.integer(str_sub(variable, start = -1))
        ) %>%
        select(-variable) %>%
        pivot_wider() %>%
        mutate(
          male = case_when(
            sex == 1 ~ 1,
            sex == 2 ~ 0,
            T ~ NA
          )
        ) %>%
        filter(!is.na(age_months) & age_months >= 0) %>%

        # Clean up
        mutate(
          age = floor(age_months / 12),
          age_months_sep1 = if_else(!is.na(age_months_sep1) & age_months_sep1 >= 0, age_months_sep1, NA_real_),
          age_sep1 = if_else(!is.na(age_months_sep1), floor(age_months_sep1 / 12), NA_real_)
        ) %>%
        select(hh_id, child_id, child_weight, age, age_months, age_sep1, age_months_sep1, male) %>%
        arrange(hh_id, child_id)
    }

    # Optional 2024 treatment 'weights': the 2024 NSECE overstates mothers'
    # employment (64.3% of children under 5 have a working mother or one
    # parent, against 59.6% in the March 2024 CPS). Multiply the child
    # weights of children under 5 by factors that rake them to the March 2024
    # CPS shares of children under 5 in five groups: two parents with the
    # mother at work / not at work, one parent at work / not at work, no
    # parent. Children whose caregiver's work status is unknown keep their
    # share; children 5 and older keep their weights. The factors were built
    # by analysis/nsece2024_options/variants2024/scripts/build_v2_weights.R
    # (option B of the employment benchmark report).
    if (use_2024_weights) {
      weight_factors <- read_csv(
        './config/nsece_reweight/nsece2024_child_weight_factors_cps2024.csv',
        show_col_types = FALSE
      )
      children <- children %>%
        left_join(weight_factors, by = c('hh_id', 'child_id')) %>%
        mutate(
          weight_factor = if_else(age_months < 60 & !is.na(weight_factor), weight_factor, 1),
          child_weight  = child_weight * weight_factor
        )
      cat('  Option weights: child weights of', sum(children$weight_factor != 1),
          'children under 5 raked to the March 2024 CPS (factor range',
          round(min(children$weight_factor), 3), 'to', round(max(children$weight_factor), 3), ')\n')
      children <- children %>% select(-weight_factor)
    }

    household_members <- {
      # Member numbers run 1-9 in 2019 and 1-13 in 2024
      hhm_age_cols <- grep('^HH9_HHM_AGE_[0-9]+$', names(nsece_hh), value = TRUE)

      nsece_hh %>%

        # Get adults
        select(hh_id = HH9_METH_CASEID, all_of(hhm_age_cols)) %>%
        pivot_longer(
          cols      = starts_with('HH9_HHM_AGE_'),
          names_to  = 'hhm_id',
          values_to = 'age'
        ) %>%

        # Filter to household member indexes
        filter(age > -9) %>%
        mutate(
          hhm_id   = as.integer(str_extract(hhm_id, '[0-9]+$')),
          age      = if_else(!is.na(age) & age >= 0, age, NA),
          child_id = NA
        ) %>%
        select(hh_id, hhm_id, child_id, age) %>%

        # Add children
        bind_rows(
          children %>%
            mutate(hhm_id = NA) %>%
            select(hh_id, hhm_id, child_id, age)
        ) %>%
        arrange(hh_id, hhm_id, child_id)
    }

    parent_units <- {
      parch_cols <- grep('^HH9_PARCH_[1-9]_[0-9]+$', names(nsece_hh), value = T)

      # Start by constructing household - parent - child links
      pu <- nsece_hh %>%
        select(hh_id = HH9_METH_CASEID, all_of(parch_cols)) %>%

        # Reshape long in parent and child
        pivot_longer(
          cols            = starts_with('HH9_PARCH_'),
          names_to        = c('child_id', 'hhm_id'),
          names_pattern   = 'HH9_PARCH_([1-9])_([0-9]+)',
          names_transform = as.integer,
          values_to       = 'is_parent'
        ) %>%
        filter(is_parent == 1) %>%
        arrange(hh_id, child_id, hhm_id) %>%
        select(hh_id, child_id, hhm_id) %>%

        # Create parent IDs
        group_by(hh_id, child_id) %>%
        mutate(
          parent_index = dense_rank(hhm_id)
        ) %>%
        pivot_wider(
          names_from   = parent_index,
          names_prefix = 'hhm_id',
          values_from  = hhm_id
        ) %>%
        ungroup() %>%

        # ASSUMPTION: Drop nonsense parent units with more than 2 people (0.3% of cases)
        # (any_of: a survey year need not contain a child with 3 or 4 flagged parents)
        select(-any_of(paste0('hhm_id', 3:13))) %>%
        mutate(
          parent_unit_id = if_else(
            is.na(hhm_id2),
            as.character(hhm_id1),
            paste0(hhm_id1, '_', hhm_id2)
          ),
          n_parents = if_else(is.na(hhm_id2), 1L, 2L),
          .before = hhm_id1
        ) %>%

        # Add sex
        left_join(
          nsece_hh %>%
            mutate(
              hhm_id1 = 1,
              male1 = if_else(HH9_RGENDER != 3, as.integer(HH9_RGENDER == 1), NA)
            ) %>%
            select(hh_id = HH9_METH_CASEID, hhm_id1, male1),
          by = c('hh_id', 'hhm_id1')
        ) %>%

        # Parent 2's sex. The public file has the respondent's sex only, so
        # infer the other parent's from household counts of mothers and
        # fathers (HH9_HHCOMP_NUMMOTHER / NUMFATHER) when parent 1 is the
        # respondent: respondent father + one father in the household +
        # 1+ mothers -> parent 2 is a mother, and the reverse. NA otherwise.
        # Needed so 1b can pick the mother as primary caregiver, as for ACS.
        left_join(
          nsece_hh %>%
            select(hh_id     = HH9_METH_CASEID,
                   n_mothers = HH9_HHCOMP_NUMMOTHER,
                   n_fathers = HH9_HHCOMP_NUMFATHER),
          by = 'hh_id'
        ) %>%
        mutate(
          male2 = case_when(
            is.na(hhm_id2) | n_mothers < 0 | n_fathers < 0  ~ NA_integer_,
            male1 %in% 1 & n_fathers == 1 & n_mothers >= 1 ~ 0L,
            male1 %in% 0 & n_mothers == 1 & n_fathers >= 1 ~ 1L,
            TRUE                                           ~ NA_integer_
          )
        ) %>%
        select(-n_mothers, -n_fathers)

      pu <- {
        labor_hi_cols     <- grep('^HH9_WST_PARCH_HI_[1-9]$',    names(nsece_hh), value = T)
        labor_lo_cols     <- grep('^HH9_WST_PARCH_LO_[1-9]$',    names(nsece_hh), value = T)
        labor_hrs_hi_cols <- grep('^HH9_WST_HRS_TOT_PHI_[1-9]$', names(nsece_hh), value = T)
        labor_hrs_lo_cols <- grep('^HH9_WST_HRS_TOT_PLO_[1-9]$', names(nsece_hh), value = T)

        parent_labor_supply <- nsece_hh %>%
          select(
            hh_id = HH9_METH_CASEID,
            all_of(labor_hi_cols),
            all_of(labor_lo_cols),
            all_of(labor_hrs_hi_cols),
            all_of(labor_hrs_lo_cols),
          ) %>%

          # Reshape long in parent, wide in variable
          pivot_longer(-hh_id) %>%
          mutate(
            variable = if_else(grepl('WST_HRS_TOT_P', name), 'hours', 'hhm_id'),
            earner   = if_else(grepl('HI', name), 'primary', 'secondary'),
            child_id = as.integer(str_sub(name, start = -1))
          ) %>%
          select(-name) %>%
          pivot_wider(
            names_from = variable,
          ) %>%

          # ASSUMPTION: remove records w/ nonresponse -- code -10 (missing
          # calendar block) in 11 / 1 / 5 child loops (HRS_TOT_PHI / PLO,
          # PARCH_HI) in 2019, 412 / 417 / 363 in 2024; missing-hours fill
          # below gives these parents hours
          filter(hhm_id > 0, hours >= 0)

        # Align who counts as working with the ACS, where a parent works if
        # at work in the reference week. The calendar hours above count work,
        # school, training and commuting last week, so they are not a pure
        # work measure. Use survey item D1a ("Last week, did you do any work
        # for pay?": 1 = yes, 2 = no, 3 = DK/refused) instead:
        #   - did not work for pay (school or training only): zero hours
        #   - worked but no calendar hours (about 6% of second earners with
        #     a child under 5): donor hours (missing-hours fill below)
        #   - D1a missing or DK/refused: calendar hours as before
        work_cols <- grep('^HH9_D1A_WORK_[0-9]+$', names(nsece_hh), value = T)

        worked_last_week <- nsece_hh %>%
          select(hh_id = HH9_METH_CASEID, all_of(work_cols)) %>%
          mutate(across(all_of(work_cols), as.integer)) %>%
          pivot_longer(
            cols         = -hh_id,
            names_to     = 'hhm_id',
            names_prefix = 'HH9_D1A_WORK_',
            values_to    = 'worked'
          ) %>%
          mutate(hhm_id = as.integer(hhm_id)) %>%
          filter(worked %in% c(1L, 2L))

        # Parent wages (also a donor-cell trait below)
        wage_cols  <- grep('^HH9_D3D_WAGE_TC_[0-9]+$', names(nsece_hh), value = T)
        unit_cols  <- grep('^HH9_D3D_UNIT_[0-9]+$',    names(nsece_hh), value = T)

        wages <- nsece_hh %>%
          select(
            hh_id = HH9_METH_CASEID,
            all_of(wage_cols),
            all_of(unit_cols)
          ) %>%
          pivot_longer(
            cols          = -hh_id,
            names_to      = c('name', 'hhm_id'),
            names_pattern = 'HH9_D3D_(WAGE_TC|UNIT)_([0-9]+)$'
          ) %>%
          mutate(hhm_id = as.integer(hhm_id)) %>%
          pivot_wider() %>%
          select(hh_id, hhm_id, pay = WAGE_TC, pay_unit = UNIT) %>%
          mutate(
            pay      = if_else(is.na(pay)      | pay < 0,           NA, pay),
            pay_unit = if_else(is.na(pay_unit) | pay_unit %in% c(7, 8), NA, pay_unit)  # 7=Other, 8=DK/Ref
          ) %>%
          mutate(
            pay = case_when(
              pay == 0 ~ NA,                     # treat $0 wage -- either a response issue or indication of nonwork -- as missing
              pay_unit == 1 ~ pay,               # per hour
              pay_unit == 2 ~ pay / 8,           # per day -> 8 hrs
              pay_unit == 3 ~ pay / 40,          # per week -> 40 hrs
              pay_unit == 4 ~ pay / 80,          # bi-weekly -> 80 hrs
              pay_unit == 5 ~ pay / (2000 / 12), # per month -> 2080/12
              pay_unit == 6 ~ pay / 2080,        # per year -> 2080 hrs
              TRUE ~ NA
            )
          ) %>%
          select(hh_id, hhm_id, pay, pay_unit)

        # One record per parent (a parent appears once per child)
        parent_hours <- parent_labor_supply %>%
          group_by(hh_id, hhm_id) %>%
          summarise(hours = first(hours), earner = first(earner), .groups = 'drop') %>%
          left_join(worked_last_week, by = c('hh_id', 'hhm_id'))

        # Missing work hours, both years (Anna, 2026-11-06; crosswalk mismatch
        # item 3). Parents who said yes to D1a but have zero calendar hours, or
        # no hours record (2024 blanks both parents' hours, code -10, when
        # either has a missing calendar block; some typical-week parents never
        # answer), get hours; parents who said no get zero.
        #   1. 2024 calendar households, no record: own adult calendar (blocks
        #      coded work / school / training x 15 min; a missing block takes
        #      own work share at same time of day on observed days). Reproduces
        #      NORC's hours for all 3,857 complete calendars.
        #   2. Everyone else: donor hours from parents who worked last week w/
        #      positive hours, same cell of earner rank, child's weekly
        #      non-parental care hours (0 / 1-19 / 20-34 / 35+), household
        #      income quintile, hourly wage band, region, urban density; keys
        #      dropped from the end until >= 5 donors (draw_donor_hours()).
        # Why these cells: filled parents' children mostly have no non-parental
        # care (54-81% vs 33% of donors); earner-rank or rank x wage draws
        # reversed the full-time gap between children in 35+ hours of care and
        # none (true +7.5 / +10.8 points 2019 / 2024, drawn -5.9 / -2.6) and
        # halved the income gap. These cells keep both; care type, parent
        # count, child age, questionnaire version add nothing (masked test,
        # scratch 2026-11-06).
        hh_traits <- nsece_hh %>%
          transmute(hh_id = HH9_METH_CASEID, hh_weight = HH9_METH_WEIGHT, income = HH9_ECON_INCOME_ANNUAL,
                    region = if_else(HH9_REGION > 0, as.integer(HH9_REGION), 0L),
                    urban  = if_else(HH9_COMM_URBAN_DENSITY > 0, as.integer(HH9_COMM_URBAN_DENSITY), 0L))
        care_cols <- grep('^HHC9_NPC_HRSWEEK_TOC[123458]_[1-9]$', names(nsece_hh), value = T)   # regular ECE types
        child_care <- nsece_hh %>%
          select(hh_id = HH9_METH_CASEID, all_of(care_cols)) %>%
          pivot_longer(-hh_id, names_to = 'child_id', names_pattern = '_([1-9])$',
                       names_transform = as.integer, values_to = 'h') %>%
          group_by(hh_id, child_id) %>%
          summarise(care_hours = sum(pmax(h, 0, na.rm = TRUE)), .groups = 'drop')

        parent_traits <- bind_rows(
          pu %>% transmute(hh_id, child_id, hhm_id = hhm_id1,
                           earner_guess = if_else(n_parents == 2 & male1 %in% 0, 'secondary', 'primary')),
          pu %>% filter(!is.na(hhm_id2)) %>%
            transmute(hh_id, child_id, hhm_id = hhm_id2, earner_guess = if_else(male2 %in% 0, 'secondary', 'primary'))
        ) %>%
          left_join(children %>% select(hh_id, child_id, age_months), by = c('hh_id', 'child_id')) %>%
          left_join(child_care, by = c('hh_id', 'child_id')) %>%
          # Focal child: parent's youngest
          group_by(hh_id, hhm_id) %>%
          slice_min(age_months, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          left_join(hh_traits, by = 'hh_id') %>%
          left_join(wages %>% select(hh_id, hhm_id, pay), by = c('hh_id', 'hhm_id')) %>%
          left_join(parent_hours %>% select(hh_id, hhm_id, earner), by = c('hh_id', 'hhm_id')) %>%
          mutate(
            earner = coalesce(earner, earner_guess),
            care   = cut(coalesce(care_hours, 0), c(-Inf, 0, 19.999, 34.999, Inf), labels = c('0', '1-19', '20-34', '35+')),
            band   = case_when(is.na(pay) ~ 'none', pay < 15 ~ '<15', pay < 25 ~ '15-25', pay < 40 ~ '25-40', TRUE ~ '40+')
          )
        pos  <- parent_traits %>% filter(!is.na(income), income > 0)
        cuts <- wtd.quantile(pos$income, pos$hh_weight, c(0.2, 0.4, 0.6, 0.8))
        parent_traits <- parent_traits %>%
          mutate(incq = if_else(is.na(income) | income <= 0, 'q0',
                                paste0('q', findInterval(income, cuts, left.open = TRUE) + 1))) %>%
          select(hh_id, hhm_id, earner, care, incq, band, region, urban)
        cell_keys <- c('earner', 'care', 'incq', 'band', 'region', 'urban')

        donors <- parent_hours %>%
          filter(worked %in% 1L, hours > 0) %>%
          select(hh_id, hhm_id, hours) %>%
          inner_join(parent_traits, by = c('hh_id', 'hhm_id'))

        # Recipients: zero calendar hours (record kept) and no record at all
        no_record <- parent_traits %>%
          anti_join(parent_hours, by = c('hh_id', 'hhm_id')) %>%
          inner_join(worked_last_week, by = c('hh_id', 'hhm_id'))
        if (nsece_year == 2024) {
          cal <- parent_calendar_hours(estimation_info$paths$NSECE, nsece_settings)
          no_record <- no_record %>% left_join(cal, by = c('hh_id', 'hhm_id'))
        } else {
          no_record <- no_record %>% mutate(cal_hours = NA_real_)
        }
        use_cal <- no_record$worked %in% 1L & !is.na(no_record$cal_hours) & no_record$cal_hours > 0
        need <- bind_rows(
          parent_hours %>% filter(worked %in% 1L, hours == 0) %>% select(hh_id, hhm_id) %>%
            inner_join(parent_traits, by = c('hh_id', 'hhm_id')),
          no_record %>% filter(worked %in% 1L, !use_cal) %>% select(hh_id, hhm_id, all_of(cell_keys))
        )
        drawn <- withr::with_seed(random_seed_base + 7, draw_donor_hours(need, donors, cell_keys))
        need$donor_hours <- as.numeric(drawn)

        parent_hours <- parent_hours %>%
          left_join(need %>% select(hh_id, hhm_id, donor_hours), by = c('hh_id', 'hhm_id')) %>%
          mutate(hours_adj = case_when(
            worked %in% 2L              ~ 0,
            worked %in% 1L & hours == 0 ~ donor_hours,
            TRUE                        ~ hours
          ))
        missing_hours <- no_record %>%
          left_join(need %>% select(hh_id, hhm_id, donor_hours), by = c('hh_id', 'hhm_id')) %>%
          mutate(hours_fill = case_when(
            worked %in% 2L                    ~ 0,
            worked %in% 1L & cal_hours %in% 0 ~ donor_hours,
            worked %in% 1L & !is.na(cal_hours) ~ cal_hours,
            worked %in% 1L                    ~ donor_hours
          ))

        cat('NSECE parents: worked last week but zero calendar hours (donor hours):',
            sum(parent_hours$worked %in% 1L & parent_hours$hours == 0),
            '| did not work but positive calendar hours (set to zero):',
            sum(parent_hours$worked %in% 2L & parent_hours$hours > 0), '\n')
        cat('  No hours record: worked last week', sum(no_record$worked %in% 1L), '(own calendar:', sum(use_cal),
            '| donor:', sum(no_record$worked %in% 1L & !use_cal), ') | did not work, zero hours:',
            sum(no_record$worked %in% 2L), '\n')
        cat('  Donor cells (', paste(cell_keys, collapse = ' x '), '), recipients by keys dropped: ',
            paste(names(table(attr(drawn, 'dropped'))), table(attr(drawn, 'dropped')), sep = ':', collapse = ' '),
            '\n', sep = '')

        parent_labor_supply <- parent_labor_supply %>%
          left_join(
            parent_hours %>% select(hh_id, hhm_id, hours_adj),
            by = c('hh_id', 'hhm_id')
          ) %>%
          mutate(hours = hours_adj) %>%
          select(-hours_adj)

        # Add labor supply information to parent units table
        pu <- pu %>%
          left_join(
            parent_labor_supply %>%
              select(hh_id, child_id, hhm_id1 = hhm_id, hours1 = hours),
            by = c('hh_id', 'child_id', 'hhm_id1')
          ) %>%
          left_join(
            parent_labor_supply %>%
              select(hh_id, child_id, hhm_id2 = hhm_id, hours2 = hours),
            by = c('hh_id', 'child_id','hhm_id2')
          )

        # Child row w/out hours: parent's own hours from another child's record
        # (records are per child), else no-record fill -- own calendar, donor,
        # or zero (above)
        parent_fill <- bind_rows(
          parent_hours %>% select(hh_id, hhm_id, h = hours_adj),
          missing_hours %>% select(hh_id, hhm_id, h = hours_fill)
        ) %>%
          filter(!is.na(h))
        pu <- pu %>%
          left_join(parent_fill %>% rename(hhm_id1 = hhm_id, fill1 = h), by = c('hh_id', 'hhm_id1')) %>%
          left_join(parent_fill %>% rename(hhm_id2 = hhm_id, fill2 = h), by = c('hh_id', 'hhm_id2')) %>%
          mutate(
            hours1 = coalesce(hours1, fill1),
            hours2 = if_else(is.na(hhm_id2), hours2, coalesce(hours2, fill2))
          ) %>%
          select(-fill1, -fill2)

        # Add wage info
        pu %>%
          left_join(
            wages %>% rename(pay1 = pay, pay_unit1 = pay_unit, hhm_id1 = hhm_id),
            by = c('hh_id', 'hhm_id1')
          ) %>%
          left_join(
            wages %>% rename(pay2 = pay, pay_unit2 = pay_unit, hhm_id2 = hhm_id),
            by = c('hh_id', 'hhm_id2')
          )
      }

      # keep_child_id = T, so do not remove child_id
      pu
    }

    children <- {
      children %>%
        left_join(
          parent_units %>%
            select(hh_id, child_id, parent_unit_id),
          by = c('hh_id', 'child_id')
        ) %>%
        relocate(parent_unit_id, .after = child_id)
    }

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

    # Deduplicate parent units and add child category info
    parent_units <- parent_units %>%
      distinct(hh_id, parent_unit_id, .keep_all = T) %>%
      select(-child_id) %>%
      left_join(child_counts, by = c('hh_id', 'parent_unit_id'))

    enrollment_data <- {

      # ---- Column groups (child X provider) ----------------------------------
      children_xy_cols  <- grep('^HH9_CHILDREN_[1-9]_[0-9]+$',          names(nsece_hh), value = T)
      regular_xy_cols   <- grep('^HH9_REGCARE_[1-9]_[0-9]+$',           names(nsece_hh), value = T)
      typeof_xy_cols    <- grep('^HH9_TYPEOFCARE_AGG_[1-9]_[0-9]+$',    names(nsece_hh), value = T)
      headstart_xy_cols <- grep('^HH9_HEADSTART_[1-9]_[0-9]+$',         names(nsece_hh), value = T)
      pubprek_xy_cols   <- grep('^HH9_PUBPREK_[1-9]_[0-9]+$',           names(nsece_hh), value = T)
      hours_xy_cols     <- grep('^HH9_HRS_WEEK_PROV_[1-9]_[0-9]+$',     names(nsece_hh), value = T)
      payment_xy_cols   <- grep('^HH9_WEEKLY_PAYMENT_TC_[1-9]_[0-9]+$', names(nsece_hh), value = T)
      subsidy_xy_cols   <- grep('^HH9_SUBSIDY_[1-9]_[0-9]+$',           names(nsece_hh), value = T)
      careloc_xy_cols   <- grep('^HH9_CARELOC_[1-9]_[0-9]+$',           names(nsece_hh), value = T)

      # Provider-only prior relationship (Y loop) -> prep for join (hh_id, provider_id)
      priorrel_y_cols <- grep('^HH9_PRIORREL_[0-9]+$', names(nsece_hh), value = T)
      priorrel_long <- nsece_hh %>%
        select(hh_id = HH9_METH_CASEID, all_of(priorrel_y_cols)) %>%
        pivot_longer(
          cols          = -hh_id,
          names_to      = 'provider_id',
          names_pattern = 'HH9_PRIORREL_([0-9]+)$',
          values_to     = 'priorrel'
        ) %>%
        mutate(provider_id = as.integer(provider_id))

      # ---- Provider-level table ----------------------------------------------
      providers <- nsece_hh %>%

        # Reshape long in child X provider, wide in variable
        select(
          hh_id = HH9_METH_CASEID,
          all_of(children_xy_cols),
          all_of(regular_xy_cols),
          all_of(typeof_xy_cols),
          all_of(headstart_xy_cols),
          all_of(pubprek_xy_cols),
          all_of(hours_xy_cols),
          all_of(payment_xy_cols),
          all_of(subsidy_xy_cols),
          all_of(careloc_xy_cols)
        ) %>%
        pivot_longer(
          cols          = -hh_id,
          names_to      = c('name', 'child_id', 'provider_id'),
          names_pattern = '(HH9_[A-Z0-9_]+)_([1-9])_([0-9]+)$',
          values_to     = 'value'
        ) %>%
        mutate(
          child_id    = as.integer(child_id),
          provider_id = as.integer(provider_id)
        ) %>%
        pivot_wider() %>%
        filter(HH9_CHILDREN == 1) %>%

        # Bring in prior relationship (provider-level)
        left_join(priorrel_long, by = c('hh_id', 'provider_id')) %>%

        select(
          hh_id, child_id, provider_id,
          regular           = HH9_REGCARE,
          type_raw          = HH9_TYPEOFCARE_AGG,
          headstart         = HH9_HEADSTART,
          pubprek           = HH9_PUBPREK,
          careloc           = HH9_CARELOC,
          priorrel,
          hours             = HH9_HRS_WEEK_PROV,
          net_weekly_cost = HH9_WEEKLY_PAYMENT_TC,
          subsidy_weekly    = HH9_SUBSIDY
        ) %>%

        # Status handling; add CARELOC and PRIORREL
        {
          status_cols <- list(
            regular         = list(miss_threshold = -2),
            headstart       = list(),
            pubprek         = list(),
            net_weekly_cost = list(),
            subsidy_weekly  = list(),
            careloc         = list(),
            priorrel        = list()
          )
          df <- .
          for (col in names(status_cols)) {
            args <- status_cols[[col]]
            status_col <- paste0(col, '_status')

            miss_threshold <- if (!is.null(args$miss_threshold)) args$miss_threshold else -1
            df[[status_col]] <- case_when(
              df[[col]] >= 0               ~ 'OK',
              df[[col]] >= miss_threshold   ~ 'MISS',
              TRUE                          ~ 'NIU'
            )

            df[[col]] <- case_when(
              df[[status_col]] == 'MISS' ~ NA,
              df[[status_col]] == 'NIU'  ~ 0,
              TRUE                       ~ df[[col]]
            )
          }
          df
        } %>%

        # Derive per-hour costs and categorize providers
        mutate(

          # Paid/free and hourly price
          paid_calc                = as.integer(net_weekly_cost > 0),
          net_hourly_cost        = if_else(hours > 0, net_weekly_cost / hours, NA),
          net_hourly_cost_status = if_else(hours == 0, 'MISS', net_weekly_cost_status),

          # Base provider groups from TOC (paper treats 4=center, 6=K-8)
          is_k8     = type_raw == 6,
          is_center = type_raw == 4,
          is_home   = type_raw %in% c(1, 2, 3),

          # is_other will be derived below using location/relationship.
          # Low/high center split on the household's own out-of-pocket price
          # per hour: the within-year median ($4.75 in 2019, $6.86 in 2024;
          # see get_nsece_year_settings())
          # A payment of don't know / refused (-1 -> NA) is missing, not
          # unpaid: is_paid stays NA, so the arrangement gets no category and
          # the child is classified by its other arrangements, or is NA
          is_paid      = paid_calc == 1,
          is_lowprice  = is_center & is_paid & net_hourly_cost_status == 'OK' & net_hourly_cost <  nsece_settings$center_price_cut,
          is_highprice = is_center & is_paid & net_hourly_cost_status == 'OK' & net_hourly_cost >= nsece_settings$center_price_cut,

          # - "Home-Based": individual care (TOC 1-3), outside child's home, no prior relationship
          # - "Other":     individual care with in-home/prior-rel, OR org/irregular/unknown (TOC 5,7,8)
          in_child_home = careloc == 1,
          has_prior_rel = priorrel == 1,

          is_home_based = is_home & !in_child_home & !has_prior_rel,
          is_other_kind = (!is_center & !is_k8) & (!is_home | in_child_home | has_prior_rel),

          ecec_choice = case_when(
            is_k8                                               ~ 'K-8',
            is_center & !is_paid                                ~ 'Unpaid Center-Based',
            is_center & is_paid & is_lowprice                   ~ 'Low-Priced Center-Based',
            is_center & is_paid & is_highprice                  ~ 'High-Priced Center-Based',
            is_center & is_paid & !(is_lowprice | is_highprice) ~ NA,
            is_home_based & !is_paid                            ~ 'Unpaid Home-Based',
            is_home_based &  is_paid                            ~ 'Paid Home-Based',
            is_other_kind & !is_paid                            ~ 'Other Unpaid',
            is_other_kind &  is_paid                            ~ 'Other Paid',
            TRUE                                                ~ NA
          )
        )

      # ---- Child-level overlay ('Parent only' + hours buckets) ---------------
      # Priority ranking for primary care assignment when child has multiple arrangements.
      # Center-based care (including unpaid programs like Head Start/pre-K) ranks above
      # home-based and other care because we focus on policies affecting formal ECEC markets.
      category_priority <- c(
        'K-8',
        'High-Priced Center-Based',
        'Low-Priced Center-Based',
        'Unpaid Center-Based',
        'Paid Home-Based',
        'Other Paid',
        'Unpaid Home-Based',
        'Other Unpaid'
      )

      enrollment <- children %>%

        # Bring in provider rows (if any) for each child
        left_join(
          providers %>% mutate(has_provider_info = 1L),
          by = c('hh_id', 'child_id')
        ) %>%
        group_by(hh_id, child_id) %>%
        group_modify(~{
          df   <- .x
          prov <- df %>% filter(has_provider_info == 1L)

          # Hours by category from usable rows only
          category_hours <- prov %>%
            filter(
              regular == 1,
              !is.na(ecec_choice),
              hours > 0
            ) %>%
            group_by(ecec_choice) %>%
            summarise(hours = sum(hours), .groups = 'drop')

          if (nrow(category_hours) > 0) {

            # Borowsky method: privilege paid over unpaid (priority first), then hours as tie-break
            return(
              category_hours %>%
                mutate(priority = match(ecec_choice, category_priority)) %>%
                arrange(priority, desc(hours)) %>%
                slice(1) %>%
                select(ecec_choice, hours)
            )
          } else {

            # No usable rows; decide NA vs Parent Only
            if (nrow(prov) == 0) {
              return(tibble(ecec_choice = 'Parent Only', hours = 0))
            }

            # Provider rows exist, but are all NA in key fields -> NA bucket
            all_na_keys <- all(is.na(prov$regular) | is.na(prov$ecec_choice))
            if (all_na_keys) {
              return(tibble(ecec_choice = NA, hours = NA))
            }

            # Otherwise: there were defined rows, just not regular>0/hours>0 -> Parent Only
            return(tibble(ecec_choice = 'Parent Only', hours = 0))
          }
        }) %>%
        ungroup() %>%
        mutate(
          ecec_hours_choice = case_when(
            is.na(ecec_choice) ~ NA,
            hours == 0         ~ 'none',
            hours < 20         ~ 'pt',
            TRUE               ~ 'ft'
          )
        ) %>%
        select(hh_id, child_id, ecec_choice, hours, ecec_hours_choice) %>%
        arrange(hh_id, child_id)

      list(
        providers = providers,
        enrollment = enrollment
      )
    }

    # -- Assertions: NSECE data integrity after processing ----

    # Every child must appear in exactly one enrollment record (the primary
    # care arrangement). If the enrollment join + group_modify produced
    # duplicates, child_weight is silently double-counted in calibration.
    enrollment <- enrollment_data$enrollment
    n_enrollment_keys <- enrollment %>%
      distinct(hh_id, child_id) %>%
      nrow()
    stopifnot(n_enrollment_keys == nrow(enrollment))

    # Enrollment ecec_choice must only contain known categories or NA.
    # If the recode logic in the provider classification has a gap, an
    # unexpected value could slip through and break downstream RF training.
    # 'K-8' is a legitimate enrollment category here (school-age children
    # whose primary arrangement is school; produced by the provider
    # classification and ranked first in category_priority) even though it
    # is not one of the 8 model care choices.
    valid_ecec_choices <- c(
      'K-8',
      'High-Priced Center-Based', 'Low-Priced Center-Based',
      'Unpaid Center-Based', 'Paid Home-Based', 'Unpaid Home-Based',
      'Other Paid', 'Other Unpaid', 'Parent Only', NA_character_
    )
    stopifnot(all(enrollment$ecec_choice %in% valid_ecec_choices))

    # ecec_hours_choice must be {none, pt, ft, NA} only. Unexpected values
    # would create wrong columns in downstream pivots.
    stopifnot(all(enrollment$ecec_hours_choice %in% c('none', 'pt', 'ft', NA_character_)))

    # Parent unit (hh_id, parent_unit_id) must be unique after dedup.
    # If the distinct() call on line 432 didn't work as expected (e.g.,
    # .keep_all = T kept the wrong row), keys would be duplicated.
    n_pu_keys <- parent_units %>%
      distinct(hh_id, parent_unit_id) %>%
      nrow()
    stopifnot(n_pu_keys == nrow(parent_units))

    # Most children should have a parent_unit_id after the join. A modest NA
    # share is expected: some children have no assignable parent unit in the
    # household roster (e.g. grandparent-headed households) and drop from
    # parent-unit-based analysis by design (~4% in the reference 2019 data).
    # A large NA share indicates the parent_unit assignment logic broke.
    n_na_pu <- sum(is.na(children$parent_unit_id))
    na_pu_share <- n_na_pu / nrow(children)
    cat('  Children without an assignable parent unit: ', n_na_pu, ' of ',
        nrow(children), ' (', round(100 * na_pu_share, 1), '%)\n', sep = '')
    if (na_pu_share > 0.10) {
      stop('run_nsece_processing: ', round(100 * na_pu_share, 1), '% of children ',
           'have NA parent_unit_id (expected ~4%). The parent_unit assignment ',
           'join has likely broken; inspect the household roster processing.')
    }

    # Return as list
    list(
      households         = households,
      household_members  = household_members,
      children           = children,
      parent_units       = parent_units,
      providers          = enrollment_data$providers,
      enrollment         = enrollment_data$enrollment
    )
  }

  # Write processed NSECE household data
  processed_nsece_hh %>%
    names() %>%
    walk(
      .f = function(table_name) {
        estimation_info$paths$data %>%
          file.path(paste0('nsece_', table_name, '_', nsece_year, '.csv')) %>%
          fwrite(processed_nsece_hh[[table_name]], file = ., na = 'NA')
      }
    )


  # --- SECTION 2: Process provider (supply-side) NSECE data ---
  # Must run BEFORE price model training to get supply-side prices
  cat('Processing', nsece_year, 'NSECE supply data...\n')

  # Read center-based provider, home-based provider, and ECEC workforce surveys
  nsece_cb <- read_nsece_file(estimation_info$paths$NSECE, nsece_settings, 'center')
  nsece_hb <- read_nsece_file(estimation_info$paths$NSECE, nsece_settings, 'home')
  nsece_wf <- read_nsece_file(estimation_info$paths$NSECE, nsece_settings, 'workforce')

  # Administrative-data centers (CB9_METH_DATA_SOURCE = 2: public-school
  # programs built from administrative records, 483 in 2019, 1,141 in 2024)
  # have no prices, classroom data, or workforce data -- their workforce
  # records are empty placeholders. Dropped from every sector, both years
  # (Anna, 2026-11-06): their only staff item is a total count NORC imputed
  # for 623 of 1,141 in 2024, so supply uses no admin staff. Unpaid ratios
  # come from interviewed centers; L_0 excludes admin children.
  # See docs/nsece_2019_to_2024_crosswalk.md, Administrative-data centers.
  n_admin  <- sum(nsece_cb$CB9_METH_DATA_SOURCE %in% 2)
  nsece_cb <- nsece_cb %>% filter(!(CB9_METH_DATA_SOURCE %in% 2))
  cat('  Administrative-data centers dropped from supply:', n_admin, '\n')

  # 2019 centers answering "don't know" to whether families pay (FAMPAY = 3):
  # NORC flags every age -8 ("no families pay"), which 1a reads as free. They
  # look like paying centers (77% report tuition revenue, 26% for profit) and
  # have no posted rate, so missing price, as 2024 already does (flag 3, as
  # harmonize_nsece_2024_centers() sets). Crosswalk mismatch item 1.
  if (nsece_year == 2019) {
    for (rate_age in c('INF', '2YR', '3YR', '4YR')) {
      flag_col <- paste0('CB9_B1_3A_AGERATE_', rate_age, '_R')
      dk_free  <- nsece_cb$CB9_B1_3_FAMPAY %in% 3 & nsece_cb[[flag_col]] %in% -8
      nsece_cb[[flag_col]] <- if_else(dk_free, 3, nsece_cb[[flag_col]])
    }
    cat('  Centers w/ don\'t-know family pay, free flag -> missing price:',
        sum(nsece_cb$CB9_B1_3_FAMPAY %in% 3), '\n')
  }

  # Weekly posted rates above $2,000 ($50/hr at 40 hours): unit errors --
  # monthly or yearly amounts entered as weekly (38 cells w/ enrollment in
  # 2019, up to $146,000; 8 in 2024). Sector prices are means, so they lift
  # the 2019 High-priced residual $1.40. Set to -20 (own missing code -- 2019
  # -2 stays NORC's censoring code, filled below), both years. Crosswalk
  # mismatch item 2.
  for (rate_age in c('INF', '2YR', '3YR', '4YR')) {
    rate_col <- paste0('CB9_STDRATE_WKLY_', rate_age)
    too_high <- !is.na(nsece_cb[[rate_col]]) & nsece_cb[[rate_col]] > 2000
    cat('  Weekly rates above $2,000 set to missing,', rate_age, ':', sum(too_high), '\n')
    nsece_cb[[rate_col]] <- if_else(too_high, -20, nsece_cb[[rate_col]])
  }

  # NORC censors posted rates at 5th, 95th percentile of each pay unit, per
  # age: 2019 deletes them (-2), 2024 sets them to the cap value. Both years:
  # each censored rate gets lognormal mean beyond its cut, so cell keeps its
  # sector (low tail Low-priced, high tail High-priced) instead of dropping
  # out (2019) or pricing at the cap (2024). Replaces option 'supply' part 1
  # (2024 caps deleted). Anna, 2026-11-06; crosswalk mismatch item 4.
  nsece_cb <- fill_censored_rates(nsece_cb, nsece_year, estimation_info$paths$NSECE)

  supply_params <- {

    processed_centers <- {
      enroll_cols   <- grep('^CB9_ENRL_(INF|1YR|2YR|3YR|4YR|5YR)$',    names(nsece_cb), value = T)
      ft_share_cols <- grep('^CB9_ENRL_FT_(INF|1YR|2YR|3YR|4YR|5YR)$', names(nsece_cb), value = T)
      price_cols    <- grep('^CB9_STDRATE_WKLY_(INF|2YR|3YR|4YR)$',    names(nsece_cb), value = T)
      flag_cols     <- grep('^CB9_B1_3A_AGERATE_(INF|2YR|3YR|4YR)_R$', names(nsece_cb), value = T)

      processed_nsece_cb <- nsece_cb %>%

        # Reshape long in age group, wide in variable
        select(
          id               = CB9_METH_CASEID,
          center_weight    = CB9_METH_WEIGHT,
          all_of(enroll_cols),
          all_of(ft_share_cols),
          all_of(price_cols),
          all_of(flag_cols)
        ) %>%
        pivot_longer(
          cols          = all_of(c(enroll_cols, ft_share_cols, price_cols, flag_cols)),
          names_to      = c('.value', 'age_bin'),
          names_pattern = '^CB9_(.+?)_(INF|1YR|2YR|3YR|4YR|5YR)(?:_R)?$'
        ) %>%

        # Encode types of missingness for price
        mutate(
          price_status = case_when(
            is.na(STDRATE_WKLY) ~ 'NIU',
            ENRL ==  0          ~ 'NIU',
            B1_3A_AGERATE == -8 ~ 'FREE',
            B1_3A_AGERATE ==  2 ~ 'NORATE',
            STDRATE_WKLY  == -1 ~ 'MISS',
            STDRATE_WKLY  == -2 ~ 'MISS',
            # Any other reserve code (e.g. 2024 -9, no rate / no family pays)
            # is not a price. Only -20 (over $2,000, above) reaches this line
            # in the 2019 or 2024 public files (src/misc/nsece_2024_checks/
            # 06_supply_checks.py); a negative value here would be read as a price.
            STDRATE_WKLY  <   0 ~ 'MISS',
            T                   ~ 'OK'
          ),

          # Scale to hourly
          price = case_when(
            price_status == 'OK'   ~ STDRATE_WKLY / 40,
            price_status == 'FREE' ~ 0,
            T                      ~ NA
          )
        ) %>%

        # Aggregate to three age groups
        group_by(
          id,
          age_bin = case_when(
            age_bin %in% c('INF', '1YR') ~ '0-1',
            age_bin %in% c('2YR', '3YR') ~ '2-3',
            age_bin %in% c('4YR', '5YR') ~ '4-5',
          )
        ) %>%

        # Aggregation rules
        summarise(
          center_weight = mean(center_weight),

          # Enrollment is a simple sum
          enrollment = sum(ENRL),

          # For FT share, use nonmissing values where possible
          share_ft = case_when(
            all(ENRL_FT < 0) ~ 0,
            any(ENRL_FT < 0) ~ mean(ENRL_FT[ENRL_FT >= 0]) / 100,
            T                ~ mean(ENRL_FT) / 100
          ),

          # For price, NAs are for 1 and 5 only, so use other value. For 2-3, use simple mean of
          # nonmissing (-1, -2). This is the first layer of aggregation.
          price = case_when(
            all(is.na(price)) ~ NA,
            T                 ~ mean(price, na.rm = T)
          ),
          .groups = 'drop'
        ) %>%

        # Express enrollment in FTE terms
        mutate(
          enrollment_fte = 1 * (enrollment * share_ft) + 0.5 * (enrollment * (1 - share_ft))
        ) %>%
        select(id, center_weight, age_bin, enrollment, enrollment_fte, price)


      # Classify production into one of three types. First, calculate median price
      medians <- processed_nsece_cb %>%
        filter(
          !is.na(price),
          price > 0
        ) %>%
        group_by(age_bin) %>%
        summarise(
          median = wtd.quantile(
            x      = price,
            weight = center_weight * enrollment_fte,
            probs  = 0.5
          )
        )

      # Then do classification and return
      processed_nsece_cb %>%
        left_join(medians, by = 'age_bin') %>%
        mutate(
          ecec_choice = case_when(
            is.na(price)    ~ NA,
            price == 0      ~ 'Unpaid Center-Based',
            price < median  ~ 'Low-Priced Center-Based',
            price >= median ~ 'High-Priced Center-Based'
          ),
          .after = age_bin
        )
    }

    processed_classrooms <- {
      nsece_cb %>%
        select(
          id               = CB9_METH_CASEID,
          classroom_weight = CB9_WEIGHT_CLSM,
          enrollment       = CB9_F3_ENROLL_TC,
          n_teacher        = CB9_F3_TEACH_TC,
          n_assistant      = CB9_F3_ASSIST_TC,
          age_youngest     = CB9_F3_YOUNGMONTHS_R,
          head_start_flag  = CB9_F15_CHILD_FUND_HS_IND,
          public_pk_flag   = CB9_F16_CHILD_FUND_PK_IND
        ) %>%

        # Handle missing values (-1 = DK/Ref, -8 = ADMIN -> NA, 0+ = keep)
        mutate(across(
          c(enrollment, n_teacher, n_assistant, age_youngest, head_start_flag, public_pk_flag),
          recode_admin_na
        )) %>%

        # Derive variables
        mutate(

          # Staffing ratio
          child_per_staff = enrollment / (n_teacher + n_assistant),

          # Map average age to age bin
          age_bin = case_when(
            floor(age_youngest / 12) %in% 0:1 ~ '0-1',
            floor(age_youngest / 12) %in% 2:3 ~ '2-3',
            floor(age_youngest / 12) %in% 4:5 ~ '4-5',
            T                                 ~ NA
          )
        )
    }

    processed_wf <- {
      nsece_wf %>%

        # Recode missings
        mutate(

          # Wage rate
          wage_status = case_when(
            WF9_WORK_WAGE >  0  ~ 'OK',
            WF9_WORK_WAGE == -1 ~ 'MISS',
            TRUE                ~ 'ADMIN'
          ),
          wage = recode_missing_admin_to_na(WF9_WORK_WAGE, wage_status),

          # Wage imputation flag
          wage_imputed = pmin(WF9_WORK_WAGE_IMP, 1),
          wage_imputed_status = case_when(
            wage_imputed >= 0  ~ 'OK',
            wage_imputed == -8 ~ 'ADMIN'
          ),
          wage_imputed = recode_missing_admin_to_na(wage_imputed, wage_imputed_status),

          # Role
          role_status = case_when(
            WF9_WORK_ROLE %in% c(1, 2, 3) ~ 'OK',
            WF9_WORK_ROLE == -1           ~ 'MISS',
            TRUE                          ~ 'ADMIN'
          ),
          role = recode_missing_admin_to_na(WF9_WORK_ROLE, role_status),


          # Highest education
          college_status = case_when(
            WF9_CHAR_EDUC > 0   ~ 'OK',
            WF9_CHAR_EDUC == -1 ~ 'MISS',
            TRUE                ~ 'ADMIN'
          ),
          college = case_when(
            WF9_CHAR_EDUC %in% c(7, 8)          ~ 1,   # BA or Grad/Prof
            WF9_CHAR_EDUC %in% c(1, 3, 4, 5, 6) ~ 0,   # < BA (per your mapping)
            TRUE                                ~ NA
          ),
          college = recode_missing_admin_to_na(college, college_status),

          # FT/PT indicator
          ft_status = case_when(
            WF9_WORK_FT %in% c(1, 2) ~ 'OK',
            WF9_WORK_FT == -1        ~ 'MISS',
            TRUE                     ~ 'ADMIN'
          ),
          ft = case_when(
            WF9_WORK_FT == 1 ~ 1,
            WF9_WORK_FT == 2 ~ 0.5,
            TRUE             ~ NA
          ),
          ft = recode_missing_admin_to_na(ft, ft_status)
        ) %>%

        # Return only variables needed for later averages
        select(
          id            = CB9_METH_CASEID,
          worker_id     = WF9_METH_CASEID,
          worker_weight = WF9_METH_WEIGHT,
          role, role_status,
          college, college_status,
          wage, wage_status,
          wage_imputed, wage_imputed_status,
          ft, ft_status
        )
    }

    wage_rates <- {
      processed_wf %>%
        filter(
          !is.na(college),
          !is.na(wage),
          role %in% 1:2
        ) %>%
        group_by(college) %>%
        summarise(wage = weighted.mean(wage, worker_weight)) %>%
        arrange(college) %>%
        pull(wage) %>%
        set_names(c('no_ba', 'ba'))
    }

    center_inputs <- {
      # Calculate worker attributes
      worker_totals <- processed_wf %>%
        left_join(
          processed_classrooms %>%
            select(id, classroom_weight, age_bin),
          by = 'id'
        ) %>%
        left_join(
          processed_centers %>%
            select(id, age_bin, ecec_choice),
          by = c('id', 'age_bin')
        ) %>%
        filter(
          role %in% 1:2,
          !is.na(age_bin),
          !is.na(ecec_choice),
          !is.na(college),
          !is.na(ft)
        ) %>%
        group_by(ecec_choice, age_bin) %>%
        summarise(
          ba       = sum(college        * ft * worker_weight),
          no_ba    = sum((college == 0) * ft * worker_weight),
          .groups = 'drop'
        )

      # Calculate total enrollment by age and price
      center_totals <- processed_centers %>%
        filter(
          !is.na(ecec_choice)
        ) %>%
        group_by(ecec_choice, age_bin) %>%
        summarise(
          price          = weighted.mean(price, center_weight * enrollment_fte, na.rm = T),
          enrollment_fte = sum(enrollment_fte * center_weight),
          .groups = 'drop'
        )

      # Join labor input and output to calculate input ratios
      center_totals %>%
        left_join(
          worker_totals %>%
            select(-ends_with('share')),
          by = c('ecec_choice', 'age_bin')
        ) %>%
        group_by(ecec_choice) %>%
        summarise(
          price    = weighted.mean(price, enrollment_fte),
          across(
            .cols = c(enrollment_fte, ba, no_ba),
            .fns  = sum
          )
        ) %>%
        mutate(
          ba_ratio    = ba    / enrollment_fte,
          no_ba_ratio = no_ba / enrollment_fte,
          labor_cost  = wage_rates['ba'] * ba_ratio + wage_rates['no_ba'] * no_ba_ratio,
          residual    = if_else(price > 0, price - labor_cost, NA)
        )
    }

    # Optional 2024 treatment 'supply', part 2: measure center staffing from
    # the director's count of teachers and aides working with children 0-5
    # (full-time counts 1, part-time 0.5), spread over the center's age
    # groups in proportion to FTE enrollment, divided by FTE enrollment of
    # the same center-age cells. Numerator and denominator then come from the
    # same centers, unlike the workforce-file ratio above. The BA / no-BA
    # split is the FTE-weighted BA share of the workforce records counted
    # above in each sector. Prices and enrollment are unchanged.
    if (use_2024_supply) {
      to_count <- function(x) if_else(!is.na(x) & x >= 0, as.numeric(x), NA_real_)

      director_staff <- nsece_cb %>%
        transmute(
          id        = CB9_METH_CASEID,
          n_teach   = to_count(CB9_STAFF_TYPE_TCHR_NUM_0TO5),
          n_aide    = to_count(CB9_STAFF_TYPE_AIDEASST_NUM0TO5),
          pft_teach = to_count(CB9_STAFF_TYPE_TCHR_PRCNTFT_0TO5),
          pft_aide  = to_count(CB9_STAFF_TYPE_AIDEASST_PFT0TO5),
          staff_fte = n_teach * (pft_teach / 100 + 0.5 * (1 - pft_teach / 100)) +
                      n_aide  * (pft_aide  / 100 + 0.5 * (1 - pft_aide  / 100))
        ) %>%
        select(id, staff_fte)

      director_ratios <- processed_centers %>%
        group_by(id) %>%
        mutate(fte_center = sum(enrollment_fte)) %>%
        ungroup() %>%
        left_join(director_staff, by = 'id') %>%
        filter(!is.na(ecec_choice), !is.na(staff_fte), fte_center > 0) %>%
        group_by(ecec_choice) %>%
        summarise(
          staff_ratio = sum(staff_fte * enrollment_fte / fte_center * center_weight) /
                        sum(enrollment_fte * center_weight),
          .groups = 'drop'
        )

      ba_shares <- processed_wf %>%
        left_join(processed_classrooms %>% select(id, age_bin), by = 'id') %>%
        left_join(processed_centers %>% select(id, age_bin, ecec_choice), by = c('id', 'age_bin')) %>%
        filter(role %in% 1:2, !is.na(age_bin), !is.na(ecec_choice), !is.na(college), !is.na(ft)) %>%
        group_by(ecec_choice) %>%
        summarise(ba_share = weighted.mean(college, ft * worker_weight), .groups = 'drop')

      center_inputs <- center_inputs %>%
        left_join(director_ratios, by = 'ecec_choice') %>%
        left_join(ba_shares, by = 'ecec_choice') %>%
        mutate(
          ba_ratio    = staff_ratio * ba_share,
          no_ba_ratio = staff_ratio * (1 - ba_share),
          labor_cost  = wage_rates['ba'] * ba_ratio + wage_rates['no_ba'] * no_ba_ratio,
          residual    = if_else(price > 0, price - labor_cost, NA)
        ) %>%
        select(-staff_ratio, -ba_share)

      cat('  Option supply: director-reported staff per FTE child (no BA / BA):\n')
      for (k in seq_len(nrow(center_inputs))) {
        cat('    ', center_inputs$ecec_choice[k], ': ', round(center_inputs$no_ba_ratio[k], 4), ' / ',
            round(center_inputs$ba_ratio[k], 4), ', residual ', round(center_inputs$residual[k], 2), '\n', sep = '')
      }
    }

    home_inputs <- {
      # Get price from household side (we lack sufficient info to do an enrollment-
      # weighted average on this side)
      price_home <- processed_nsece_hh$children %>%
        left_join(
          processed_nsece_hh$providers,
          by = c('hh_id', 'child_id')
        ) %>%
        filter(
          ecec_choice == 'Paid Home-Based',
          age < 5,
          net_hourly_cost_status == 'OK'
        ) %>%
        summarise(price = weighted.mean(net_hourly_cost, child_weight)) %>%
        pull()

      nsece_hb %>%
        select(
          hb_id               = HB9_METH_CASEID,
          hb_weight           = HB9_METH_WEIGHT,
          listed_source       = HB9_METH_SAMPLE_SOURCE,
          paid                = HB9_PRGM_PAID,
          enrollment          = HB9_ENRL_TOTAL,
          resp_college        = HB9_CAREER_EDUC,
          n_helpers           = HB9_H2_NUMHELP,
          helpers_pct_college = HB9_STAFF_EDUC_PRCNT_4YR
        ) %>%

        # Drop all the true missings
        filter(
          enrollment >= 0,
          resp_college > 0,
          n_helpers > -4,
          helpers_pct_college != -1, helpers_pct_college != -3
        ) %>%

        # Restrict to listed (licensed/regulated) providers only
        # These are the providers eligible for policy subsidies
        # HB9_METH_SAMPLE_SOURCE: 1 = Unlisted, 2 = Listed
        # Note: Listed implies paid (licensed providers are businesses by definition).
        # Testing confirmed adding explicit paid==1 filter changes ratios by <1%.
        filter(listed_source == 2) %>%

        # Derive variables
        mutate(
          n_helpers = if_else(n_helpers == -2, 0, n_helpers),
          n_workers = 1 + n_helpers,
          n_ba      = as.integer(resp_college >= 6) + n_helpers * helpers_pct_college / 100,
          n_no_ba   = n_workers - n_ba
        ) %>%

        # Get weighted totals. Provider weights make the home-based labor
        # stock and hours national totals, like the center side; unweighted
        # sums made home-based care under 1% of L_0 instead of 7-16%.
        summarise(
          enrollment = sum(enrollment * hb_weight),
          ba         = sum(n_ba       * hb_weight),
          no_ba      = sum(n_no_ba    * hb_weight),
        ) %>%
        mutate(

          # Rename to enrollment_fte for consistency with center-based
          # (home-based enrollment is treated as FTE)
          enrollment_fte = enrollment,

          # Calculate labor input ratios
          ba_ratio    = ba    / enrollment_fte,
          no_ba_ratio = no_ba / enrollment_fte,

          # Calculate labor cost
          labor_cost = wage_rates['ba'] * ba_ratio + wage_rates['no_ba'] * no_ba_ratio,

          # Calculate residual revenue
          price    = price_home,
          residual = price - labor_cost,

          ecec_choice = 'Paid Home-Based'

        )
    }


    #-----------------------------------
    # Construct supply parameter output
    #-----------------------------------

    sector_order <- c('Unpaid Center-Based', 'Low-Priced Center-Based', 'High-Priced Center-Based', 'Paid Home-Based')

    # Get inputs in pre-specified order
    inputs <- center_inputs %>%
      bind_rows(home_inputs) %>%
      arrange(match(ecec_choice, sector_order))

    # Get labor input matrix
    L_req <- inputs %>%
      select(no_ba = no_ba_ratio, ba = ba_ratio) %>%
      as.matrix()
    rownames(L_req) <- sector_order

    # Extract per-unit residual (delta_0 = price - unit_labor_cost)
    delta_0 <- inputs %>%
      pull(residual) %>%
      set_names(sector_order)

    # Compute total hours of care by sector in calibration year (for L_0 calculation)
    hours_by_sector <- inputs %>%
      pull(enrollment_fte) %>%
      set_names(sector_order)
    hours_by_sector <- hours_by_sector * FT_CARE_HOURS * WEEKS_PER_YEAR

    # Compute L_0: total labor by type in calibration year
    # L_0[type] = sum across sectors of L_req[sector, type] * hours[sector]
    L_0 <- colSums(L_req * hours_by_sector)
    names(L_0) <- c('no_ba', 'ba')

    # Set labor supply elasticities
    e <- c('no_ba' = 4, 'ba' = 2)

    # Extract prices by sector (for price_wedge calculation in price heterogeneity model)
    # These are supply-side gross prices: what providers charge per hour
    prices <- inputs %>%
      pull(price) %>%
      set_names(sector_order)

    # Return as list
    list(
      year    = nsece_year,
      L_req   = L_req,
      w       = wage_rates,
      L       = L_0,
      e       = e,
      delta_0 = delta_0,
      prices  = prices
    )
  }

  # Keep survey-year dollar values for the price wedges (household price over
  # provider price, both in survey-year dollars), then convert the supply
  # parameters to base-year dollars. No change when the two years are equal.
  supply_params_nsece_dollars <- supply_params
  if (nsece_year != base_year) {
    supply_params$year    <- base_year
    supply_params$w       <- supply_params$w / dollar_conversion$wage_factor
    supply_params$delta_0 <- supply_params$delta_0 / dollar_conversion$cpi_factor
    labor_cost_base       <- as.vector(supply_params$L_req %*% supply_params$w)
    supply_params$prices  <- if_else(
      is.na(supply_params$delta_0),
      supply_params$prices / dollar_conversion$cpi_factor,
      labor_cost_base + supply_params$delta_0
    ) %>%
      set_names(names(supply_params$prices))
  }

  {
    # Convert data to YAML-friendly structure with named elements (flattened)
    yaml_data <- list(
      year = base_year,

      labor_requirements = setNames(lapply(1:4, function(i) {
        list(no_ba = supply_params$L_req[i, 1], ba = supply_params$L_req[i, 2])
      }), c('unpaid_center_based', 'low_price_center_based',
           'high_price_center_based', 'paid_home_based')),

      wages = list(
        no_ba = supply_params$w[1],
        ba    = supply_params$w[2]
      ),

      labor_supply = list(
        no_ba = supply_params$L[1],
        ba    = supply_params$L[2]
      ),

      elasticities = list(
        no_ba = supply_params$e[1],
        ba    = supply_params$e[2]
      ),

      per_unit_residual = list(
        unpaid_center_based     = supply_params$delta_0[1],
        low_price_center_based  = supply_params$delta_0[2],
        high_price_center_based = supply_params$delta_0[3],
        paid_home_based         = supply_params$delta_0[4]
      )
    )

    # Record the survey year and the survey-year dollar values when they
    # were converted to base-year dollars
    if (nsece_year != base_year) {
      yaml_data$nsece_year <- nsece_year
      yaml_data$dollar_conversion <- list(
        note        = paste0('Wages and per-unit residuals are in ', base_year, ' dollars. ',
                             'Survey-year values divided by these factors.'),
        wage_factor = dollar_conversion$wage_factor,
        cpi_factor  = dollar_conversion$cpi_factor,
        wages_survey_dollars = list(
          no_ba = supply_params_nsece_dollars$w[1],
          ba    = supply_params_nsece_dollars$w[2]
        ),
        per_unit_residual_survey_dollars = list(
          unpaid_center_based     = supply_params_nsece_dollars$delta_0[1],
          low_price_center_based  = supply_params_nsece_dollars$delta_0[2],
          high_price_center_based = supply_params_nsece_dollars$delta_0[3],
          paid_home_based         = supply_params_nsece_dollars$delta_0[4]
        )
      )
    }

    # Write to estimation output folder (run-specific record)
    estimation_dir <- file.path(output_root, 'estimation', 'supply')
    dir.create(estimation_dir, recursive = TRUE, showWarnings = FALSE)
    write_yaml(yaml_data, file.path(estimation_dir, paste0('supply_', base_year, '.yaml')))

    cat('Supply parameters written to:\n')
    cat('  - ', file.path(estimation_dir, paste0('supply_', base_year, '.yaml')), '\n')
  }


  # --- SECTION 3: Train price heterogeneity models ---
  {
    # Survey-year dollars: price wedges are ratios of household prices to
    # provider prices from the same survey
    supply_prices <- supply_params_nsece_dollars$prices
    tc_output_root <- estimation_info$paths$output

    qrf_data <- {

      # Extract supply-side prices by sector
      supply_price_center_low  <- supply_prices['Low-Priced Center-Based']
      supply_price_center_high <- supply_prices['High-Priced Center-Based']
      supply_price_home        <- supply_prices['Paid Home-Based']


      # Get income percentiles (child-weighted) for the NSECE sample
      income_pctiles <- processed_nsece_hh$children %>%
        left_join(processed_nsece_hh$households, by = 'hh_id') %>%
        filter(age < 5, income > 0) %>%
        reframe(
          income_pctile = wtd.quantile(
            x      = income + runif(n()),
            weight = child_weight,
            probs  = seq(0.01, 0.99, 0.01)
          )
        ) %>%
        deframe()

      # Build base provider data with required joins
      provider_base <- processed_nsece_hh$providers %>%
        filter(regular == 1) %>%
        left_join(
          processed_nsece_hh$children %>%
            select(hh_id, child_id, age, child_weight, parent_unit_id),
          by = c('hh_id', 'child_id')
        ) %>%
        filter(age < 5) %>%
        left_join(
          processed_nsece_hh$households %>%
            select(hh_id, income, region),
          by = 'hh_id'
        ) %>%
        left_join(
          processed_nsece_hh$parent_units %>%
            select(hh_id, parent_unit_id, n_parents),
          by = c('hh_id', 'parent_unit_id')
        ) %>%
        mutate(
          income_pctile = cut(income, c(-Inf, income_pctiles, Inf), labels = 1:100) %>%
            as.character() %>% as.integer(),
          region = factor(region),
          child_age = age
        )

      make_price_wedge_df <- function(choice, base_price) {

        #--------------------------------------------------------------------------
        # Creates price wedge dataset for a care type.
        #
        # Params:
        #   - choice (chr): ECEC care type choice label
        #   - base_price (dbl): Base supply price for the care type
        #
        # Returns: (df) Price wedge data with income_pctile, region, n_parents,
        #   child_age, price_wedge, weight
        #--------------------------------------------------------------------------

        provider_base %>%
          filter(
            ecec_choice == choice,
            net_hourly_cost_status == 'OK',
            net_hourly_cost > 0
          ) %>%
          mutate(price_wedge = net_hourly_cost / base_price) %>%
          select(income_pctile, region, n_parents, child_age, price_wedge, weight = child_weight)
      }

      # Other Paid has no supply sector; use weighted mean OOP as base price
      other_paid_base_price <- provider_base %>%
        filter(ecec_choice == 'Other Paid', net_hourly_cost_status == 'OK', net_hourly_cost > 0) %>%
        summarise(base_price = weighted.mean(net_hourly_cost, child_weight)) %>%
        pull(base_price)

      center_low_price  <- make_price_wedge_df('Low-Priced Center-Based', supply_price_center_low)
      center_high_price <- make_price_wedge_df('High-Priced Center-Based', supply_price_center_high)
      home_price        <- make_price_wedge_df('Paid Home-Based', supply_price_home)
      other_paid_price  <- make_price_wedge_df('Other Paid', other_paid_base_price)

      list(
        center_low_price         = center_low_price,
        center_high_price        = center_high_price,
        home_price               = home_price,
        other_paid_price         = other_paid_price,
        supply_price_center_low  = supply_price_center_low,
        supply_price_center_high = supply_price_center_high,
        supply_price_home        = supply_price_home,
        other_paid_base_price    = other_paid_base_price
      )
    }

    price_qrf_models <- {

      qrf_center_low_price  <- train_single_qrf(qrf_data$center_low_price, 'price_wedge', 'center_low_price')
      qrf_center_high_price <- train_single_qrf(qrf_data$center_high_price, 'price_wedge', 'center_high_price')
      qrf_home_price        <- train_single_qrf(qrf_data$home_price, 'price_wedge', 'home_price')
      qrf_other_paid_price  <- train_single_qrf(qrf_data$other_paid_price, 'price_wedge', 'other_paid_price')

      model_keys <- c('center_low_price', 'center_high_price', 'home_price', 'other_paid_price')
      summary_stats <- map(model_keys, function(key) {
        d <- qrf_data[[key]]
        list(
          n    = nrow(d),
          mean = weighted.mean(d$price_wedge, d$weight),
          sd   = sqrt(wtd.var(d$price_wedge, d$weight)),
          q25  = wtd.quantile(d$price_wedge, d$weight, 0.25),
          q50  = wtd.quantile(d$price_wedge, d$weight, 0.50),
          q75  = wtd.quantile(d$price_wedge, d$weight, 0.75)
        )
      }) %>% set_names(model_keys)
      summary_stats$supply_price_center_low  <- qrf_data$supply_price_center_low
      summary_stats$supply_price_center_high <- qrf_data$supply_price_center_high
      summary_stats$supply_price_home        <- qrf_data$supply_price_home
      summary_stats$other_paid_base_price    <- qrf_data$other_paid_base_price

      # Base prices in base-year dollars (calibration and simulation multiply
      # the wedges by these)
      if (nsece_year != base_year) {
        summary_stats$supply_price_center_low  <- supply_params$prices['Low-Priced Center-Based']
        summary_stats$supply_price_center_high <- supply_params$prices['High-Priced Center-Based']
        summary_stats$supply_price_home        <- supply_params$prices['Paid Home-Based']
        summary_stats$other_paid_base_price    <- qrf_data$other_paid_base_price / dollar_conversion$cpi_factor
        summary_stats$other_paid_base_price_survey_dollars <- qrf_data$other_paid_base_price
        summary_stats$nsece_year <- nsece_year
        summary_stats$base_year  <- base_year
      }

      list(
        center_low_price  = qrf_center_low_price,
        center_high_price = qrf_center_high_price,
        home_price        = qrf_home_price,
        other_paid_price  = qrf_other_paid_price,
        summary_stats     = summary_stats
      )
    }

    {
      models_dir <- file.path(tc_output_root, 'models')
      dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

      #----------------------------------
      # 1. Summary statistics CSV
      #----------------------------------

      model_keys <- c('center_low_price', 'center_high_price', 'home_price', 'other_paid_price')
      base_price_keys <- c('supply_price_center_low', 'supply_price_center_high',
                           'supply_price_home', 'other_paid_base_price')
      summary_df <- map_dfr(seq_along(model_keys), function(i) {
        s <- price_qrf_models$summary_stats[[model_keys[i]]]
        tibble(
          model = model_keys[i], n_obs = s$n, mean = s$mean, sd = s$sd,
          q25 = s$q25, q50 = s$q50, q75 = s$q75,
          base_price = price_qrf_models$summary_stats[[base_price_keys[i]]]
        ) %>%
          rename_with(~ paste0('base_price_', base_year), .cols = base_price)
      })

      write_csv(summary_df, file.path(models_dir, 'price_summary.csv'))

      #----------------------------------
      # 2. Distribution by region CSV
      #----------------------------------

      care_type_map <- c(center_low_price = 'center_low', center_high_price = 'center_high',
                         home_price = 'home', other_paid_price = 'other_paid')
      price_by_region <- map_dfr(names(care_type_map), function(key) {
        qrf_data[[key]] %>%
          group_by(region) %>%
          summarise(
            n                = n(),
            mean_price_wedge = weighted.mean(price_wedge, weight),
            sd_price_wedge   = sqrt(wtd.var(price_wedge, weight)),
            q25_price_wedge  = wtd.quantile(price_wedge, weight, 0.25),
            q50_price_wedge  = wtd.quantile(price_wedge, weight, 0.50),
            q75_price_wedge  = wtd.quantile(price_wedge, weight, 0.75),
            .groups          = 'drop'
          ) %>%
          mutate(care_type = care_type_map[[key]])
      })

      write_csv(price_by_region, file.path(models_dir, 'price_by_region.csv'))

      #----------------------------------
      # 3. Distribution by income quintile
      #----------------------------------

      price_by_income <- map_dfr(names(care_type_map), function(key) {
        qrf_data[[key]] %>%
          mutate(income_quintile = cut(income_pctile, c(0, 20, 40, 60, 80, 100), labels = 1:5)) %>%
          group_by(income_quintile) %>%
          summarise(
            n                = n(),
            mean_price_wedge = weighted.mean(price_wedge, weight),
            sd_price_wedge   = sqrt(wtd.var(price_wedge, weight)),
            .groups          = 'drop'
          ) %>%
          mutate(care_type = care_type_map[[key]])
      })

      write_csv(price_by_income, file.path(models_dir, 'price_by_income_quintile.csv'))

      #----------------------------------
      # 4. Supply prices and price_wedge info text file
      #----------------------------------

      ss <- price_qrf_models$summary_stats
      base_prices <- list(
        'Low-priced center-based (supply)'  = ss$supply_price_center_low,
        'High-priced center-based (supply)' = ss$supply_price_center_high,
        'Paid home-based (supply)'          = ss$supply_price_home,
        'Other paid (weighted mean OOP)'    = ss$other_paid_base_price
      )
      wedge_stats <- list(
        'Low-priced center'  = ss$center_low_price,
        'High-priced center' = ss$center_high_price,
        'Paid home-based'    = ss$home_price,
        'Other paid'         = ss$other_paid_price
      )
      price_info <- capture.output({
        cat('=== BASE PRICES (', base_year, ' $/hour) ===\n', sep = '')
        for (nm in names(base_prices)) cat(nm, ':', round(base_prices[[nm]], 2), '\n')
        cat('\n=== PRICE HETEROGENEITY (price_wedge = household_OOP / base_price) ===\n')
        for (nm in names(wedge_stats)) {
          cat(nm, ': mean =', round(wedge_stats[[nm]]$mean, 3),
              ', sd =', round(wedge_stats[[nm]]$sd, 3), '\n')
        }
      })

      writeLines(price_info, file.path(models_dir, 'price_info.txt'))
    }

    # Cache to disk (file name depends on the survey year)
    cache_path <- get_price_model_cache_path(nsece_year)
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

    saveRDS(price_qrf_models, cache_path)
    cat('Price models cached to', cache_path, '\n')
  }

  cat('NSECE estimation data preparation complete!\n')
}



get_nsece_year_settings <- function(nsece_year) {

  #----------------------------------------------------------------------------
  # Returns the file names, variable-name year digit, low/high center price
  # cut, and code-harmonization functions for one NSECE survey year.
  #
  # The 2024 public-use files (ICPSR 39466) use the 2019 variable names with
  # the year digit 4 in place of 9 (HH4_ for HH9_, CB4_ for CB9_, and so on).
  # read_nsece_file() renames 2024 columns to the 2019 spelling, and the
  # harmonize functions recode the few 2024 reserve codes that differ from
  # 2019, so the processing code serves both years unchanged.
  #
  # The household-side cut between low- and high-priced center care is the
  # within-year weighted median hourly out-of-pocket price of paid center care
  # for the youngest child under 5 (Borowsky et al. 2022): child weights,
  # positive fees only. That recipe gives $4.73 on 2019 (the model keeps the
  # published $4.75) and $6.86 on 2024. See
  # src/misc/nsece_2024_checks/05_price_cutoff.py and
  # docs/nsece_2019_to_2024_crosswalk.md. Growing $4.75 by the CPI for day
  # care ($5.76) is not used: center prices rose faster than that index.
  #
  # Params:
  #   - nsece_year (int): Survey year, 2019 or 2024
  #
  # Returns: (list) folder, prefix_digit, files, center_price_cut, harmonize
  #----------------------------------------------------------------------------

  settings <- list(
    '2019' = list(
      folder           = '2019',
      prefix_digit     = '9',
      files            = list(
        household = '37941-0009-Data.tsv',
        center    = '37941-0006-Data.tsv',
        home      = '37941-0007-Data.tsv',
        workforce = '37941-0005-Data.tsv'
      ),
      center_price_cut = 4.75,
      harmonize        = list()
    ),
    '2024' = list(
      folder           = '2024',
      prefix_digit     = '4',
      files            = list(
        household = '39466-0005-Data.tsv',
        center    = '39466-0002-Data.tsv',
        home      = '39466-0003-Data.tsv',
        workforce = '39466-0001-Data.tsv',
        calendar  = '39466-0004-Data.tsv'   # weekly child + adult calendars, 15-min blocks
      ),
      center_price_cut = 6.86,
      harmonize        = list(
        household = harmonize_nsece_2024_household,
        center    = harmonize_nsece_2024_centers,
        home      = harmonize_nsece_2024_home
      )
    )
  )

  key <- as.character(nsece_year)
  if (!key %in% names(settings)) {
    stop('get_nsece_year_settings: no settings for NSECE year ', nsece_year,
         '. Supported years: ', paste(names(settings), collapse = ', '), '.')
  }
  settings[[key]]
}



read_nsece_file <- function(nsece_root, nsece_settings, file_type) {

  #----------------------------------------------------------------------------
  # Reads one NSECE public-use data file, renames its variables to the 2019
  # spelling (year digit 9), and applies the year's code harmonization.
  #
  # Params:
  #   - nsece_root (chr): NSECE raw data folder (contains 2019/ and 2024/)
  #   - nsece_settings (list): Output of get_nsece_year_settings()
  #   - file_type (chr): 'household', 'center', 'home', or 'workforce'
  #
  # Returns: (tibble) The data file with 2019-style variable names and codes
  #----------------------------------------------------------------------------

  df <- nsece_root %>%
    file.path(nsece_settings$folder, nsece_settings$files[[file_type]]) %>%
    fread() %>%
    tibble()

  # HH4_, HHC4_, CB4_, HB4_, WF4_ -> HH9_, HHC9_, CB9_, HB9_, WF9_
  if (nsece_settings$prefix_digit != '9') {
    prefix_pattern <- paste0('^(HHC|HH|CB|HB|WF)', nsece_settings$prefix_digit, '_')
    df <- df %>%
      rename_with(~ str_replace(.x, prefix_pattern, '\\19_'))
  }

  harmonize <- nsece_settings$harmonize[[file_type]]
  if (!is.null(harmonize)) {
    df <- harmonize(df)
  }

  df
}



harmonize_nsece_2024_household <- function(nsece_hh) {

  #----------------------------------------------------------------------------
  # Recodes 2024 household-file items whose form changed from 2019.
  #
  # Respondent's sex: 2019 has one item (HH9_RGENDER: 1 = male, 2 = female,
  # 3 = don't know/refused). 2024 asks "select all that apply" and the public
  # file has two yes/no flags (HH4_RGENDER_1 male, HH4_RGENDER_2 female).
  # Build the 2019 item from them: male only -> 1, female only -> 2,
  # neither or both -> 3 (treated as unknown downstream).
  #
  # All other household variables the model uses keep their 2019 codes. The
  # 2024 file has up to 13 household members and 15 providers (2019: 9 and
  # 14); the processing code reads any number of each.
  #
  # Params:
  #   - nsece_hh (df): 2024 household file with 2019-style names
  #
  # Returns: (df) Same data with HH9_RGENDER added
  #----------------------------------------------------------------------------

  nsece_hh %>%
    mutate(
      HH9_RGENDER = case_when(
        HH9_RGENDER_1 == 1 & HH9_RGENDER_2 != 1 ~ 1L,
        HH9_RGENDER_2 == 1 & HH9_RGENDER_1 != 1 ~ 2L,
        TRUE                                    ~ 3L
      )
    )
}



harmonize_nsece_2024_centers <- function(nsece_cb) {

  #----------------------------------------------------------------------------
  # Recodes the 2024 center-based provider file to the 2019 conventions the
  # supply-side code expects.
  #
  # 1. Free programs. In 2019 the rate flag CB9_B1_3A_AGERATE_*_R = -8 meant
  #    "no families pay" (the same centers as CB9_B1_3_FAMPAY = 2 or 3). In
  #    2024, -8 instead marks administrative-data cases, and programs where
  #    no family pays carry -3 (valid skip). Free status is therefore taken
  #    from CB4_B1_3_FAMPAY = 2 and written back as -8. Other non-answers
  #    become 3 (don't know), which the code treats as a missing price.
  # 2. Posted rates. 2024 uses -8 for administrative cases; recode to -1
  #    (missing). The 2024 public file censors rates at the 5th and 95th
  #    percentiles within each time unit instead of coding them -2 as in
  #    2019; capped values left as is here (they can be detected as
  #    pile-ups, see crosswalk mismatch item 4).
  # 3. Administrative-data cases (CB4_METH_DATA_SOURCE = 2) are left as they
  #    are here (enrollment by age -8 -> 0 below); run_nsece_processing()
  #    sets them aside and adds free ones to Unpaid Center-Based from their
  #    total 0-5 enrollment and total staff count.
  # 4. Enrollment by age unknown (-1, 1-2% of centers per age): set to 0, so
  #    the center does not count at that age. Full-time share unknown (-4):
  #    set to the enrollment-weighted mean share for that age.
  #
  # Params:
  #   - nsece_cb (df): 2024 center file with 2019-style names
  #
  # Returns: (df) Same data with 2019-style codes
  #----------------------------------------------------------------------------

  ages      <- c('INF', '1YR', '2YR', '3YR', '4YR', '5YR')
  rate_ages <- c('INF', '2YR', '3YR', '4YR')
  is_admin  <- nsece_cb$CB9_METH_DATA_SOURCE %in% 2

  # Enrollment and full-time share by age
  for (age in ages) {
    enrl_col <- paste0('CB9_ENRL_', age)
    ft_col   <- paste0('CB9_ENRL_FT_', age)

    enrl <- as.numeric(nsece_cb[[enrl_col]])
    ft   <- as.numeric(nsece_cb[[ft_col]])

    enrl <- if_else(enrl < 0, 0, enrl)

    reported <- !is_admin & enrl > 0 & ft >= 0
    mean_ft  <- weighted.mean(ft[reported], (nsece_cb$CB9_METH_WEIGHT * enrl)[reported])
    ft <- case_when(
      ft == -4 & enrl > 0 ~ mean_ft,
      ft == -4            ~ -2,
      TRUE                ~ ft
    )

    nsece_cb[[enrl_col]] <- enrl
    nsece_cb[[ft_col]]   <- ft
  }

  # Free status and posted rates
  for (age in rate_ages) {
    flag_col <- paste0('CB9_B1_3A_AGERATE_', age, '_R')
    rate_col <- paste0('CB9_STDRATE_WKLY_', age)

    nsece_cb[[flag_col]] <- case_when(
      nsece_cb$CB9_B1_3_FAMPAY == 2      ~ -8,
      nsece_cb[[flag_col]] %in% c(1, 2)  ~ as.numeric(nsece_cb[[flag_col]]),
      TRUE                               ~ 3
    )
    nsece_cb[[rate_col]] <- if_else(nsece_cb[[rate_col]] == -8, -1, as.numeric(nsece_cb[[rate_col]]))
  }

  nsece_cb
}



harmonize_nsece_2024_home <- function(nsece_hb) {

  #----------------------------------------------------------------------------
  # Recodes 2024 home-based provider reserve codes to their 2019 values.
  #
  # Number of helpers (HB9_H2_NUMHELP): 2024 uses -3 for "no outside help"
  # (2019: -2) and -1 for don't know/refused (2019: -4).
  # Percent of helpers with a BA (HB9_STAFF_EDUC_PRCNT_4YR): 2024 uses -3 for
  # "no other people help" (2019: -2), -1 for "number of helpers unknown"
  # (2019: -1 don't know), and a new -5 for "not enough information", which
  # is mapped to 2019's -3 so that, like -1, it is dropped.
  #
  # Params:
  #   - nsece_hb (df): 2024 home-based file with 2019-style names
  #
  # Returns: (df) Same data with 2019-style codes
  #----------------------------------------------------------------------------

  nsece_hb %>%
    mutate(
      HB9_H2_NUMHELP = case_when(
        HB9_H2_NUMHELP == -3 ~ -2,
        HB9_H2_NUMHELP == -1 ~ -4,
        TRUE                 ~ as.numeric(HB9_H2_NUMHELP)
      ),
      HB9_STAFF_EDUC_PRCNT_4YR = case_when(
        HB9_STAFF_EDUC_PRCNT_4YR == -3 ~ -2,
        HB9_STAFF_EDUC_PRCNT_4YR == -5 ~ -3,
        TRUE                           ~ as.numeric(HB9_STAFF_EDUC_PRCNT_4YR)
      )
    )
}



RATE_AGES <- c('INF', '2YR', '3YR', '4YR')



find_censored_rates <- function(nsece_cb, nsece_year) {

  #----------------------------------------------------------------------------
  # Censored posted weekly rates per center x rate age. 2019: NORC code -2.
  # 2024: rate equal to a cap. Caps not flagged, but show as pile-ups of one
  # exact value -- per age, most frequent value below 15th percentile, above
  # 85th percentile, separately for monthly amounts (rate x 4.34524 whole
  # dollar) and others -> four caps (low, high, low monthly, high monthly).
  # Cap value = the cell's cut, side known from which cap.
  #
  # Params:
  #   - nsece_cb (df): center file, 2019-style names, admin centers dropped
  #   - nsece_year (int): 2019 or 2024
  #
  # Returns: (tibble) row, age, side ('low' / 'high', NA in 2019), cut ($/wk, NA in 2019)
  #----------------------------------------------------------------------------

  bind_rows(lapply(RATE_AGES, function(a) {
    rate <- nsece_cb[[paste0('CB9_STDRATE_WKLY_', a)]]
    if (nsece_year == 2019) {
      i <- which(rate %in% -2)
      return(tibble(row = i, age = a, side = NA_character_, cut = NA_real_))
    }
    positive <- !is.na(rate) & rate > 0
    v     <- round(rate[positive], 2)
    q     <- quantile(v, c(0.15, 0.85))
    tab   <- table(v)
    vals  <- as.numeric(names(tab))
    month <- abs(vals * 4.34524 - round(vals * 4.34524)) < 0.05
    pick  <- function(sel) if (any(sel)) vals[sel][which.max(tab[sel])] else NA_real_
    caps  <- c(pick(vals < q[1] & !month), pick(vals > q[2] & !month),
               pick(vals < q[1] &  month), pick(vals > q[2] &  month))
    k <- match(round(rate, 2), caps)
    i <- which(positive & !is.na(k))
    tibble(row = i, age = a, side = if_else(k[i] %in% c(1, 3), 'low', 'high'), cut = rate[i])
  }))
}



censored_cell_features <- function(nsece_cb, cens) {

  #----------------------------------------------------------------------------
  # Center traits + own other-age rate position for each censored cell --
  # inputs to the tail-side model (which tail a censored rate is in).
  # Other-age position: mean over the center's uncensored rates at other ages
  # (positive, <= $2,000) of their rank among all uncensored rates at that age.
  #
  # Params:
  #   - nsece_cb (df): center file
  #   - cens (tibble): find_censored_rates() output
  #
  # Returns: (tibble) cens + profit, pubsch, hs, pk, subs, log_fte, region,
  #   density, other_pct (0.5 when none), no_other, w
  #----------------------------------------------------------------------------

  flag <- function(x) as.numeric(!is.na(x) & x == 1)
  ages <- c('INF', '1YR', '2YR', '3YR', '4YR', '5YR')
  enrl <- sapply(ages, function(a) pmax(nsece_cb[[paste0('CB9_ENRL_', a)]], 0, na.rm = TRUE))
  ft   <- sapply(ages, function(a) {
    f <- nsece_cb[[paste0('CB9_ENRL_FT_', a)]]
    if_else(!is.na(f) & f >= 0, f / 100, 0)
  })
  fte  <- rowSums(enrl * ft + 0.5 * enrl * (1 - ft))

  body <- lapply(setNames(RATE_AGES, RATE_AGES), function(a) {
    r <- nsece_cb[[paste0('CB9_STDRATE_WKLY_', a)]]
    ok <- !is.na(r) & r > 0 & r <= 2000
    ok[cens$row[cens$age == a]] <- FALSE
    list(ok = ok, sorted = sort(r[ok]), r = r)
  })
  other_pct <- mapply(function(i, a) {
    p <- sapply(setdiff(RATE_AGES, a), function(b) {
      x <- body[[b]]
      if (x$ok[i]) findInterval(x$r[i], x$sorted, left.open = TRUE) / length(x$sorted) else NA_real_
    })
    if (all(is.na(p))) NA_real_ else mean(p, na.rm = TRUE)
  }, cens$row, cens$age)

  d <- nsece_cb$CB9_COMM_URBAN_DENSITY
  cens %>%
    mutate(
      profit    = flag(nsece_cb$CB9_A8_PROFIT_R2[row]),
      pubsch    = flag(nsece_cb$CB9_METH_PUBSCHFLAG[row]),
      hs        = flag(nsece_cb$CB9_F15_CHILD_FUND_HS_IND[row]),
      pk        = flag(nsece_cb$CB9_F16_CHILD_FUND_PK_IND[row]),
      subs      = flag(nsece_cb$CB9_RVNU_SUBS_PAST_YR[row]),
      log_fte   = log(fte[row] + 1),
      region    = factor(nsece_cb$CB9_REGION[row], levels = 1:4),
      density   = factor(if_else(d[row] %in% 1:3, as.integer(d[row]), 0L), levels = 0:3),
      no_other  = as.numeric(is.na(other_pct)),
      other_pct = coalesce(other_pct, 0.5),
      w         = nsece_cb$CB9_METH_WEIGHT[row]
    )
}



fill_censored_rates <- function(nsece_cb, nsece_year, nsece_root) {

  #----------------------------------------------------------------------------
  # Replaces each NORC-censored posted weekly rate w/ the lognormal mean
  # beyond its cut, per rate age. Body = uncensored positive rates <= $2,000,
  # center weights (lognormal QQ R2 0.983-0.992 by age, both years; one mode).
  #
  # 2024: censored (Tobit) lognormal MLE w/ known caps; side + cut from cap.
  # 2019: censored share known (-2 count), side not. Lognormal from QQ fit of
  #   body w/ half of censored share in each tail; cuts = fitted quantiles at
  #   those shares. Side from logit fit on 2024 censored cells (traits,
  #   region, urban density, other-age position): w/out density, 88% of 2024
  #   sides right in 5-fold CV vs 70% always-low (scratch test 2026-11-06;
  #   crosswalk item 4); in-sample 87.9% w/ density.
  # Low-tail mean < every age-bin median, high-tail mean > it in both years,
  # so cell lands in Low- / High-priced by the usual median rule.
  #
  # Params:
  #   - nsece_cb (df): center file, 2019-style names, admin centers dropped
  #   - nsece_year (int): 2019 or 2024
  #   - nsece_root (chr): NSECE raw folder (2019 run reads 2024 center file)
  #
  # Returns: (df) nsece_cb w/ censored rates filled
  #----------------------------------------------------------------------------

  tail_mean <- function(mu, s, cut, side) {
    z <- (log(cut) - mu) / s
    m <- exp(mu + s^2 / 2)
    if_else(side == 'low', m * pnorm(z - s) / pnorm(z),
            m * pnorm(z - s, lower.tail = FALSE) / pnorm(z, lower.tail = FALSE))
  }
  wq <- function(x, w, p) {
    o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
    sapply(p, function(pp) x[which(cw >= pp)[1]])
  }

  cens <- find_censored_rates(nsece_cb, nsece_year)

  # 2019 side: logit on 2024 censored cells, sides known
  if (nsece_year == 2019) {
    cb24 <- read_nsece_file(nsece_root, get_nsece_year_settings(2024), 'center') %>%
      filter(!(CB9_METH_DATA_SOURCE %in% 2))
    tr <- censored_cell_features(cb24, find_censored_rates(cb24, 2024)) %>%
      mutate(high = as.numeric(side == 'high'))
    fit <- glm(high ~ profit + pubsch + hs + pk + subs + log_fte + region + density + other_pct + no_other,
               family = quasibinomial(), data = tr, weights = w / mean(w))
    ft <- censored_cell_features(nsece_cb, cens)
    cens$side <- if_else(predict(fit, newdata = ft, type = 'response') >= 0.5, 'high', 'low')
    cat('  Censored rates, side model on 2024 (', nrow(tr), ' cells): weighted in-sample accuracy ',
        round(100 * weighted.mean((fitted(fit) >= 0.5) == (tr$high == 1), tr$w), 1), '%\n', sep = '')
  }

  for (a in RATE_AGES) {
    col  <- paste0('CB9_STDRATE_WKLY_', a)
    r    <- nsece_cb[[col]]
    w    <- nsece_cb$CB9_METH_WEIGHT
    ca   <- cens %>% filter(age == a)
    ok   <- !is.na(r) & r > 0 & r <= 2000
    ok[ca$row] <- FALSE
    x    <- log(r[ok]); wx <- w[ok]

    if (nsece_year == 2024) {
      lo <- ca %>% filter(side == 'low'); hi <- ca %>% filter(side == 'high')
      nll <- function(p) {
        s <- exp(p[2])
        -(sum(wx * dnorm(x, p[1], s, log = TRUE)) +
            sum(w[lo$row] * pnorm(log(lo$cut), p[1], s, log.p = TRUE)) +
            sum(w[hi$row] * pnorm(log(hi$cut), p[1], s, lower.tail = FALSE, log.p = TRUE)))
      }
      est <- optim(c(weighted.mean(x, wx), log(sd(x))), nll)$par
      mu <- est[1]; s <- exp(est[2])
      cut <- ca$cut
    } else {
      t  <- nrow(ca) / 2 / (sum(ok) + nrow(ca))
      p  <- seq(0.08, 0.92, by = 0.02)
      pb <- (p - t) / (1 - 2 * t)
      keep <- pb > 0.005 & pb < 0.995
      qq <- lm(wq(x, wx, pb[keep]) ~ qnorm(p[keep]))
      mu <- unname(coef(qq)[1]); s <- unname(coef(qq)[2])
      cut <- if_else(ca$side == 'low', exp(mu + s * qnorm(t)), exp(mu + s * qnorm(1 - t)))
    }
    fill <- tail_mean(mu, s, cut, ca$side)
    nsece_cb[[col]][ca$row] <- fill
    cat('  Censored rates filled, ', a, ': ', sum(ca$side == 'low'), ' low (mean $',
        round(mean(fill[ca$side == 'low']) / 40, 2), '/hr), ', sum(ca$side == 'high'), ' high (mean $',
        round(mean(fill[ca$side == 'high']) / 40, 2), '/hr); lognormal mu ', round(mu, 3),
        ' sigma ', round(s, 3), '\n', sep = '')
  }
  nsece_cb
}



draw_donor_hours <- function(recipients, donors, keys, min_donors = 5) {

  #----------------------------------------------------------------------------
  # One donor draw per recipient from donors in the same cell of keys; keys
  # dropped from the end until the cell has >= min_donors. Uses the global
  # RNG stream -- caller sets the seed.
  #
  # Params:
  #   - recipients (df): one row per parent to fill, w/ keys
  #   - donors (df): parents w/ hours and keys
  #   - keys (chr): cell variables, most important first
  #   - min_donors (int): smallest usable cell
  #
  # Returns: (num) donor hours per recipient; attr 'dropped' = keys dropped
  #----------------------------------------------------------------------------

  out     <- rep(NA_real_, nrow(recipients))
  dropped <- rep(NA_integer_, nrow(recipients))
  todo    <- seq_len(nrow(recipients))
  for (L in length(keys):1) {
    rk    <- do.call(paste, c(lapply(keys[1:L], function(k) recipients[[k]]), sep = '|'))
    dk    <- do.call(paste, c(lapply(keys[1:L], function(k) donors[[k]]), sep = '|'))
    pools <- split(donors$hours, dk)
    size  <- unname(lengths(pools)[rk[todo]])
    ok    <- todo[!is.na(size) & size >= min_donors]
    for (g in unique(rk[ok])) {
      i <- ok[rk[ok] == g]
      p <- pools[[g]]
      out[i] <- p[sample.int(length(p), length(i), replace = TRUE)]   # index: sample() on length-1 draws 1:x
    }
    dropped[ok] <- length(keys) - L
    todo <- setdiff(todo, ok)
    if (!length(todo)) break
  }
  if (length(todo)) {
    out[todo]     <- donors$hours[sample.int(nrow(donors), length(todo), replace = TRUE)]
    dropped[todo] <- length(keys)
  }
  attr(out, 'dropped') <- dropped
  out
}



parent_calendar_hours <- function(nsece_root, nsece_settings) {

  #----------------------------------------------------------------------------
  # 2024 weekly work-related hours per parent from the adult calendar
  # (HH4_ADCAL_Y_Z: member Y, 15-min block Z = 1..672, Monday 12:00am first).
  # Codes 1 work, 2 school, 3 training count (as NORC's WST hours do); 0
  # none; 4 missing; -2 no calendar (typical-week version), 8 skipped
  # section D, 9 no member -> no calendar for that member. Missing block
  # takes member's own share of work-related blocks at same time of day on
  # observed days (masked test: abs error ~10 h vs ~15 h for any donor
  # rule). All 672 missing -> NA.
  #
  # Params:
  #   - nsece_root (chr): NSECE raw folder
  #   - nsece_settings (list): get_nsece_year_settings(2024)
  #
  # Returns: (tibble) hh_id, hhm_id, cal_hours, cal_missing_blocks
  #----------------------------------------------------------------------------

  path <- file.path(nsece_root, nsece_settings$folder, nsece_settings$files$calendar)
  hdr  <- names(fread(path, nrows = 0))
  ad   <- grep('^HH4_ADCAL_[0-9]+_[0-9]+$', hdr, value = TRUE)
  cal  <- fread(path, select = c('HH4_METH_CASEID', ad))
  t_of <- ((1:672) - 1) %% 96 + 1

  bind_rows(lapply(sort(unique(as.integer(sub('^HH4_ADCAL_([0-9]+)_.*', '\\1', ad)))), function(y) {
    cols <- paste0('HH4_ADCAL_', y, '_', 1:672)
    if (!all(cols %in% ad)) return(NULL)
    m     <- as.matrix(cal[, ..cols])
    valid <- rowSums(m >= 0 & m <= 4, na.rm = TRUE) == 672
    m     <- m[valid, , drop = FALSE]
    if (!nrow(m)) return(NULL)
    work  <- m >= 1 & m <= 3
    miss  <- m == 4
    fill  <- rep(0, nrow(m))
    for (t in 1:96) {
      idx   <- which(t_of == t)
      n_obs <- rowSums(!miss[, idx, drop = FALSE])
      share <- if_else(n_obs > 0, rowSums(work[, idx, drop = FALSE]) / n_obs, 0)
      fill  <- fill + rowSums(miss[, idx, drop = FALSE]) * share
    }
    n_miss <- rowSums(miss)
    tibble(hh_id = cal$HH4_METH_CASEID[valid], hhm_id = y,
           cal_hours = if_else(n_miss == 672, NA_real_, (rowSums(work) + fill) / 4),
           cal_missing_blocks = n_miss)
  }))
}
