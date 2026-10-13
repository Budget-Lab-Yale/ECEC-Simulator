# 1a_nsece_processing.R
#
# NSECE processing: household demand data, supply parameters,
# and price heterogeneity quantile grids. Called from main.R.
#
# Label draws (nsece_mi_draws.R): nsece_enrollment_2019.csv one row per
# child x draw (NSECE_MI_DRAWS draws; columns draw, draw_weight = 1 / M) --
# weight readers multiply child_weight by draw_weight. nsece_providers_2019.csv
# holds draw 0 (= single draw before MI).


run_nsece_processing <- function() {

  #----------------------------------------------------------------------------
  # Processes 2019 NSECE data: household demand data, supply parameters,
  # and price heterogeneity quantile grids. Writes to estimation output folder.
  #
  # Params: none
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  # Get estimation data info
  estimation_info <- get_estimation_info()


  # Helper functions
  centre_age_bin <- function(age) {

    #--------------------------------------------------------------------------
    # Child age (years) -> centre price age bin, same bins as provider file
    # rates (0-1 infant, 2-3, 4-5; older pooled w/ 4-5).
    #
    # Params:
    #   - age (num vec): child age in years
    #
    # Returns: (chr vec) '0-1', '2-3', '4-5'; NA where age NA
    #--------------------------------------------------------------------------

    case_when(
      age <= 1 ~ '0-1',
      age <= 3 ~ '2-3',
      age >= 4 ~ '4-5'
    )
  }

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


  # --- Centre posted prices and tier boundary (sections 1 and 2) ---
  cat('Processing 2019 NSECE centre prices...\n')

  # Read center-based provider survey
  nsece_cb <- estimation_info$paths$NSECE %>%
    file.path('2019/37941-0006-Data.tsv') %>%
    fread() %>%
    tibble()

  enroll_cols   <- grep('^CB9_ENRL_(INF|1YR|2YR|3YR|4YR|5YR)$',    names(nsece_cb), value = T)
  ft_share_cols <- grep('^CB9_ENRL_FT_(INF|1YR|2YR|3YR|4YR|5YR)$', names(nsece_cb), value = T)
  price_cols    <- grep('^CB9_STDRATE_WKLY_(INF|2YR|3YR|4YR)$',    names(nsece_cb), value = T)
  flag_cols     <- grep('^CB9_B1_3A_AGERATE_(INF|2YR|3YR|4YR)_R$', names(nsece_cb), value = T)

  processed_nsece_cb <- nsece_cb %>%

    # Reshape long in age group, wide in variable
    select(
      id               = CB9_METH_CASEID,
      center_weight    = CB9_METH_WEIGHT,
      region           = CB9_REGION,
      # Community urban density 1 high / 2 moderate / 3 rural (urban_density.R)
      density          = CB9_COMM_URBAN_DENSITY,
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
      region,
      density,
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
    select(id, center_weight, region, density, age_bin, enrollment, enrollment_fte, price)


  # Tier boundary: density-adjusted line per Census region x community urban
  # density x age bin (2026-10-12; region x age median before, 2026-10-09) --
  # regional high-density median x national density price ratio
  # (compute_tier_thresholds(), urban_density.R), FTE x survey weighted,
  # positive prices. One line both sides -- centres (section 2, posted price,
  # own CB9 density) and families (section 1, hourly payment, own HH9
  # density) fall below it (low) or at/above it (high). Same $10 reads
  # higher tier in rural market than in dense city
  tier_thresholds <- compute_tier_thresholds(processed_nsece_cb)
  medians <- tier_thresholds %>%
    select(region, density, age_bin, median = threshold)
  cat('  Paid-centre tier line ($/hr), region x density (1 high, 2 moderate, 3 rural) x age:\n')
  print(as.data.frame(tier_thresholds %>%
                        mutate(across(c(median_high, national_ratio, threshold), ~ round(.x, 3)))))
  fwrite(tier_thresholds, file.path(estimation_info$paths$data, 'nsece_tier_thresholds_2019.csv'))


  # --- SECTION 1: Process household (demand-side) NSECE data ---
  cat('Processing 2019 NSECE household data...\n')

  nsece_hh <- estimation_info$paths$NSECE %>%
    file.path('2019/37941-0009-Data.tsv') %>%
    fread() %>%
    tibble()

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
          region = HH9_REGION,
          # Community urban density 1 high / 2 moderate / 3 rural -- tier
          # line, care-forest feature (urban_density.R)
          density = HH9_COMM_URBAN_DENSITY
        ) %>%
        select(
          hh_id        = HH9_METH_CASEID,
          hh_weight    = HH9_METH_WEIGHT,
          n_members    = HH9_HHCOMP_MEMBERS,
          income       = HH9_ECON_INCOME_ANNUAL,
          region,
          density,
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

    household_members <- {
      hhm_age_cols <- grep('^HH9_HHM_AGE_[1-9]$', names(nsece_hh), value = TRUE)

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
          hhm_id   = as.integer(str_sub(hhm_id, -1)),
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
      parch_cols <- grep('^HH9_PARCH_[1-9]_[1-9]$', names(nsece_hh), value = T)

      # Start by constructing household - parent - child links
      links <- nsece_hh %>%
        select(hh_id = HH9_METH_CASEID, all_of(parch_cols)) %>%

        # Reshape long in parent and child
        pivot_longer(
          cols            = starts_with('HH9_PARCH_'),
          names_to        = c('child_id', 'hhm_id'),
          names_pattern   = 'HH9_PARCH_([1-9])_([1-9])',
          names_transform = as.integer,
          values_to       = 'is_parent'
        ) %>%
        filter(is_parent == 1) %>%
        arrange(hh_id, child_id, hhm_id) %>%
        select(hh_id, child_id, hhm_id)

      # Parent sex (infer_parent_sex()): respondent from HH9_RGENDER, other
      # parents from household mother / father counts -- roster sex not on
      # public file. Before: male1 only when parent 1 = respondent, no
      # male2; RGENDER -4 coded female
      parent_sex <- infer_parent_sex(
        links,
        nsece_hh %>%
          transmute(
            hh_id     = HH9_METH_CASEID,
            resp_male = if_else(HH9_RGENDER %in% c(1, 2), as.numeric(HH9_RGENDER == 1), NA_real_),
            n_mothers = if_else(HH9_HHCOMP_NUMMOTHER >= 0, as.numeric(HH9_HHCOMP_NUMMOTHER), NA_real_),
            n_fathers = if_else(HH9_HHCOMP_NUMFATHER >= 0, as.numeric(HH9_HHCOMP_NUMFATHER), NA_real_)
          )
      )
      cat('  Parent sex source (parents, raw): ',
          paste(names(table(parent_sex$sex_source)), table(parent_sex$sex_source),
                sep = ' ', collapse = ', '), '\n', sep = '')

      pu <- links %>%

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
        select(-hhm_id3, -hhm_id4) %>%
        mutate(
          parent_unit_id = if_else(
            is.na(hhm_id2),
            as.character(hhm_id1),
            paste0(hhm_id1, '_', hhm_id2)
          ),
          n_parents = if_else(is.na(hhm_id2), 1L, 2L),
          .before = hhm_id1
        ) %>%

        # Add sex, both parents
        left_join(
          parent_sex %>% select(hh_id, hhm_id1 = hhm_id, male1 = male),
          by = c('hh_id', 'hhm_id1')
        ) %>%
        left_join(
          parent_sex %>% select(hh_id, hhm_id2 = hhm_id, male2 = male),
          by = c('hh_id', 'hhm_id2')
        )

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

          # ASSUMPTION: remove records (12 of them) with nonresponse
          filter(hhm_id > 0, hours >= 0)

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

        # Employment band per parent, ACS concept (usual hours, past 12
        # months) as near as public file allows (nsece_person_employment()):
        # worked for pay last week -> reference-week WST hours (hours1 /
        # hours2); not working, still holds job or last job within 12 months
        # of interview -> usual hours at that job; else none. Hours /
        # recency unknown -> NA (family out of forest frame, 1b). Before:
        # WST hours of parent 1, NA -> none (2026-10-12)
        person_cols <- grep('^HH9_(D1A_WORK|D5B_LASTWRKMOYR_R|D5C_HOUR)_[1-9]$', names(nsece_hh), value = T)
        interview <- children %>%
          mutate(t = nsece_interview_time(age_months, age_months_sep1)) %>%
          filter(!is.na(t)) %>%
          group_by(hh_id) %>%
          summarise(t = median(t), .groups = 'drop')
        # Household w/out child born before 2018-09-01 -> median interview time
        t_fill <- median(interview$t)
        person_work <- nsece_hh %>%
          select(hh_id = HH9_METH_CASEID, all_of(person_cols)) %>%
          pivot_longer(
            cols          = -hh_id,
            names_to      = c('name', 'hhm_id'),
            names_pattern = 'HH9_(D1A_WORK|D5B_LASTWRKMOYR_R|D5C_HOUR)_([1-9])$'
          ) %>%
          mutate(hhm_id = as.integer(hhm_id)) %>%
          pivot_wider() %>%
          left_join(interview, by = 'hh_id') %>%
          transmute(hh_id, hhm_id, worked = D1A_WORK, last_work = D5B_LASTWRKMOYR_R,
                    last_hours = D5C_HOUR, cutoff = coalesce(t, t_fill) - 1)
        pu <- pu %>%
          left_join(person_work %>% rename_with(~ paste0(.x, '_p1'), -hh_id) %>% rename(hhm_id1 = hhm_id_p1),
                    by = c('hh_id', 'hhm_id1')) %>%
          left_join(person_work %>% rename_with(~ paste0(.x, '_p2'), -hh_id) %>% rename(hhm_id2 = hhm_id_p2),
                    by = c('hh_id', 'hhm_id2')) %>%
          mutate(
            emp1 = nsece_person_employment(worked_p1, hours1, last_work_p1, last_hours_p1, cutoff_p1),
            emp2 = if_else(is.na(hhm_id2), NA_character_,
                           nsece_person_employment(worked_p2, hours2, last_work_p2, last_hours_p2, cutoff_p2))
          ) %>%
          select(-matches('_p[12]$'))

        # Add parent wages
        wage_cols  <- grep('^HH9_D3D_WAGE_TC_[1-9]$', names(nsece_hh), value = T)
        unit_cols  <- grep('^HH9_D3D_UNIT_[1-9]$',    names(nsece_hh), value = T)

        wages <- nsece_hh %>%
          select(
            hh_id = HH9_METH_CASEID,
            all_of(wage_cols),
            all_of(unit_cols)
          ) %>%
          pivot_longer(
            cols          = -hh_id,
            names_to      = c('name', 'hhm_id'),
            names_pattern = 'HH9_D3D_(WAGE_TC|UNIT)_([1-9])$'
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
      children_xy_cols  <- grep('^HH9_CHILDREN_[1-9]_(?:[1-9]|1[0-4])$',          names(nsece_hh), value = T)
      regular_xy_cols   <- grep('^HH9_REGCARE_[1-9]_(?:[1-9]|1[0-4])$',           names(nsece_hh), value = T)
      typeof_xy_cols    <- grep('^HH9_TYPEOFCARE_AGG_[1-9]_(?:[1-9]|1[0-4])$',    names(nsece_hh), value = T)
      headstart_xy_cols <- grep('^HH9_HEADSTART_[1-9]_(?:[1-9]|1[0-4])$',         names(nsece_hh), value = T)
      pubprek_xy_cols   <- grep('^HH9_PUBPREK_[1-9]_(?:[1-9]|1[0-4])$',           names(nsece_hh), value = T)
      hours_xy_cols     <- grep('^HH9_HRS_WEEK_PROV_[1-9]_(?:[1-9]|1[0-4])$',     names(nsece_hh), value = T)
      payment_xy_cols   <- grep('^HH9_WEEKLY_PAYMENT_TC_[1-9]_(?:[1-9]|1[0-4])$', names(nsece_hh), value = T)
      paidhh_xy_cols    <- grep('^HH9_PAID_[1-9]_(?:[1-9]|1[0-4])$',              names(nsece_hh), value = T)
      subsidy_xy_cols   <- grep('^HH9_SUBSIDY_[1-9]_(?:[1-9]|1[0-4])$',           names(nsece_hh), value = T)
      careloc_xy_cols   <- grep('^HH9_CARELOC_[1-9]_(?:[1-9]|1[0-4])$',           names(nsece_hh), value = T)

      # Provider-only prior relationship (Y loop) -> prep for join (hh_id, provider_id)
      priorrel_y_cols <- grep('^HH9_PRIORREL_(?:[1-9]|1[0-4])$', names(nsece_hh), value = T)
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
          all_of(paidhh_xy_cols),
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
          # Raw code kept -- resolve_regular_care() reads -1 / -2
          regcare           = HH9_REGCARE,
          type_raw          = HH9_TYPEOFCARE_AGG,
          headstart         = HH9_HEADSTART,
          pubprek           = HH9_PUBPREK,
          careloc           = HH9_CARELOC,
          priorrel,
          hours             = HH9_HRS_WEEK_PROV,
          # Topcoded direct charges household pays provider (2019 NSECE User's Guide --
          # Household, OPRE 2023-170, p. 271). Excludes amounts others pay provider;
          # does NOT subtract subsidy paid to household (that is HH9_WEEKLY_COST_CARE_TC).
          # Name kept for callers -- "net" of third-party payments only.
          net_weekly_cost = HH9_WEEKLY_PAYMENT_TC,
          # Questionnaire paid status (J1 "do you pay provider anything
          # directly", C5E) -- 1 paid, 0 not paid by household, -1 DK, -2 no
          # Section J. Raw code kept; resolves paid status of unknown-amount
          # rows (impute_unknown_payment(), paid_q)
          paid_hh           = HH9_PAID,
          subsidy_weekly    = HH9_SUBSIDY
        ) %>%

        # Status handling; add CARELOC and PRIORREL
        {
          status_cols <- list(
            regular         = list(miss_threshold = -2),
            headstart       = list(),
            pubprek         = list(),
            # -2 = ineligible for Section J (not asked) -> amount unknown,
            # as -1 DK / refused (2026-10-12; NIU, unpaid before)
            net_weekly_cost = list(miss_threshold = -2),
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

          # Base provider groups from TOC (paper treats 4=center, 6=K-8). TOC 5
          # ("Other organizational ECE") stays Other: NSECE User's Guide --
          # Household (2019) reserves TOC 4 for every center program under 72
          # months; TOC 5 = drop-in, lessons, church care during services,
          # recreation, after-school
          is_k8     = type_raw == 6,
          is_center = type_raw == 4,
          is_home   = type_raw %in% c(1, 2, 3),

          # is_other will be derived below using location/relationship
          is_paid      = !is.na(paid_calc)   & paid_calc == 1,
          # Payment amount unknown (HH9_WEEKLY_PAYMENT_TC -1 DK / refused,
          # -2 no Section J) -> neither paid nor unpaid label (2026-10-12;
          # was unpaid). Child-level overlay uses paid status from HH9_PAID
          # when 0 / 1, else drawn (impute_unknown_payment())
          pay_unknown  = is.na(paid_calc),

          # - "Home-Based": individual care (TOC 1-3), outside child's home, no prior relationship
          # - "Other":     individual care with in-home/prior-rel, OR org (TOC 5).
          #   TOC 7 / 8 rows also land here but never regular
          #   (resolve_regular_care(), TOC 1-5 only) -> never count, never
          #   payment donors / draw targets (2026-10-12; TOC 8 counted Other before)
          in_child_home = careloc == 1,
          has_prior_rel = priorrel == 1,

          is_home_based = is_home & !in_child_home & !has_prior_rel,
          is_other_kind = (!is_center & !is_k8) & (!is_home | in_child_home | has_prior_rel),

          # Kind of unknown-payment row -- picks its candidate labels
          # (PAY_UNKNOWN_OPTIONS) in child-level overlay
          unknown_kind = case_when(
            is_k8 | !pay_unknown ~ NA_character_,
            is_center            ~ 'center',
            is_home_based        ~ 'home',
            is_other_kind        ~ 'other'
          ),

          ecec_choice = case_when(
            is_k8                                               ~ 'K-8',
            pay_unknown                                         ~ NA,
            is_center & !is_paid                                ~ 'Unpaid Center-Based',
            # Paid centre -- tier set below against own market's median
            is_center & is_paid                                 ~ 'Paid Center-Based',
            is_home_based & !is_paid                            ~ 'Unpaid Home-Based',
            is_home_based &  is_paid                            ~ 'Paid Home-Based',
            is_other_kind & !is_paid                            ~ 'Other Unpaid',
            is_other_kind &  is_paid                            ~ 'Other Paid',
            TRUE                                                ~ NA
          )
        )

      # Draw stage, NSECE_MI_DRAWS times (multiple imputation,
      # nsece_mi_draws.R): draw m seeds = base + m x NSECE_MI_SEED_STEP, m = 0
      # = single draw before. Which rows draw (hours_imputed, tier_unknown,
      # unknown payment) same every draw -- only drawn values change
      providers_base <- providers
      label_draw <- function(m) {

        #------------------------------------------------------------------------
        # One draw: regular care + hours, paid-centre tier, unknown payment.
        #
        # Params:
        #   - m (int): draw index, 0 .. NSECE_MI_DRAWS - 1
        #
        # Returns: (tibble) provider rows w/ draw m's hours, labels
        #------------------------------------------------------------------------

        show <- m == 0

        # Regular care + weekly hours (resolve_regular_care(), shared w/
        # state_data 02): REGCARE -1 / -2 -> regular when TOC 1-5; regular row
        # w/ zero / missing reference-week hours -> imputed hours (keyed draw
        # from weighted donor hours, TOC group x age cell). Overwrites
        # `regular`, `hours`;
        # net_hourly_cost above stays on reported hours (price models,
        # home price use reported hours only)
        rc <- providers_base %>%
          left_join(children %>% select(hh_id, child_id, age, child_weight),
                    by = c('hh_id', 'child_id')) %>%
          resolve_regular_care(seed = nsece_mi_seed(REGULAR_HOURS_SEED, m))
        providers <- rc$arr %>% select(-child_weight)
        if (show) {
          cat('  Regular care: REGCARE -1/-2 rows regular by TOC ', sum(providers$regular_by_type),
              ', regular rows w/ imputed hours ', sum(providers$hours_imputed), '\n', sep = '')
        }

        # Paid-centre tier: family's hourly payment vs centre tier boundary
        # (`medians`, provider posted price) for household's region x community
        # density x child's age bin -- same dollar line as centres. Payment net of third-party
        # payments, so subsidised families fall low more often (limitation).
        # Hourly payment on reported hours only. Paid centre w/ imputed hours
        # (no reference-week care): weekly payment over donor-drawn hours
        # isn't family's own hourly price -> tier drawn, as unknown payment
        # (impute_unknown_payment(), tier_unknown) (2026-10-12; tiered on
        # imputed hours before, 42 rows). No hours -> no tier, label NA (row
        # can't count)
        providers <- providers %>%
          left_join(households %>% select(hh_id, region, density), by = 'hh_id') %>%
          mutate(age_bin = centre_age_bin(age),
                 tier_unknown = ecec_choice %in% 'Paid Center-Based' & hours_imputed,
                 tier_price = if_else(!hours_imputed & !is.na(hours) & hours > 0,
                                      net_weekly_cost / hours, NA_real_)) %>%
          left_join(medians, by = c('region', 'density', 'age_bin')) %>%
          mutate(
            ecec_choice = case_when(
              is.na(ecec_choice) | ecec_choice != 'Paid Center-Based' ~ ecec_choice,
              is.na(tier_price)                                       ~ NA_character_,
              is.na(median)                                           ~ ecec_choice,
              tier_price >= median                                    ~ 'High-Priced Center-Based',
              TRUE                                                    ~ 'Low-Priced Center-Based'
            )
          ) %>%
          select(-age, -region, -density, -age_bin, -median, -tier_price)
        if (any(providers$ecec_choice %in% 'Paid Center-Based')) {
          stop('NSECE paid-centre arrangements w/out a tier boundary (region x density x age bin). ',
               'Check HH9_REGION / HH9_COMM_URBAN_DENSITY codes and child ages.')
        }

        # Unknown payment: paid status (+ centre tier) drawn per arrangement,
        # impute_unknown_payment() (shared w/ state_data 02 -- same draws).
        # assign_primary_care() uses draw on every counting unknown row.
        # Paid centre w/ imputed hours (tier_unknown) -> tier alone drawn
        imp_in <- providers %>%
          left_join(children %>% select(hh_id, child_id, age, child_weight),
                    by = c('hh_id', 'child_id')) %>%
          left_join(households %>% select(hh_id, region, density), by = 'hh_id') %>%
          transmute(
            hh_id, child_id, provider_id, age, region, density, child_weight,
            kind   = pay_kind(is_k8, is_center, is_home_based, is_other_kind),
            paid   = if_else(pay_unknown, NA, is_paid),
            paid_q = case_when(paid_hh %in% 1 ~ TRUE, paid_hh %in% 0 ~ FALSE),
            tier   = case_when(ecec_choice %in% 'High-Priced Center-Based' ~ 'High',
                               ecec_choice %in% 'Low-Priced Center-Based'  ~ 'Low'),
            counts = !is.na(regular) & regular == 1 & !is.na(hours) & hours > 0,
            tier_unknown
          )
        pay_imp <- impute_unknown_payment(imp_in, seed = nsece_mi_seed(IMPUTE_PAY_SEED, m))
        if (show) {
          cat('  Unknown-payment arrangements w/ draws: ', sum(!pay_imp$draws$tier_only),
              ' (paid drawn ', sum(pay_imp$draws$imp_paid[!pay_imp$draws$tier_only]),
              '); paid centres w/ imputed hours, tier drawn: ', sum(pay_imp$draws$tier_only),
              '\n', sep = '')
        }
        providers %>%
          left_join(pay_imp$draws %>% select(hh_id, child_id, provider_id, imp_paid, imp_tier),
                    by = c('hh_id', 'child_id', 'provider_id')) %>%
          mutate(imputed_choice = imputed_pay_label(unknown_kind, imp_paid, imp_tier),
                 ecec_choice = if_else(tier_unknown,
                                       imputed_pay_label(rep('center', n()), imp_paid, imp_tier),
                                       ecec_choice))
      }
      draws <- lapply(seq_len(NSECE_MI_DRAWS) - 1L, label_draw)

      # providers table = draw 0 (single draw before). Price inputs (home
      # price, price wedges) read observed payment over reported hours only
      # -- no drawn row enters them
      providers <- draws[[1]]

      # ---- Child-level overlay ('Parent only' + hours buckets) ---------------
      # Primary arrangement per child: assign_primary_care() (shared_functions)
      # -- rank paid over unpaid, centre first (formal ECEC markets focus),
      # paid-centre tiers share one rank, unknown payment from imputed draws
      primary_care <- function(prov, kids) {

        #------------------------------------------------------------------------
        # Primary arrangement, hours band per child, one draw.
        #
        # Params:
        #   - prov (tibble): one draw's provider rows
        #   - kids (tibble): children to label
        #
        # Returns: (tibble) hh_id, child_id, ecec_choice, hours,
        #   ecec_hours_choice -- one row per child
        #------------------------------------------------------------------------

        kids %>%

          # Bring in provider rows (if any) for each child
          left_join(
            prov %>% mutate(has_provider_info = 1L),
            by = c('hh_id', 'child_id')
          ) %>%
          group_by(hh_id, child_id) %>%
          group_modify(~ assign_primary_care(.x %>% filter(has_provider_info == 1L))) %>%
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
      }

      # Child w/ any drawn row (imputed hours, tier-only draw, drawn paid
      # status) relabelled every draw; other children same label every draw
      # -- draw 0 label reused. Every child NSECE_MI_DRAWS rows w/ draw,
      # draw_weight 1 / M (stack_mi_draws()); child_weight x draw_weight
      # summed over child's rows = child_weight
      varies <- providers %>%
        filter(hours_imputed | tier_unknown | !is.na(imp_paid)) %>%
        distinct(hh_id, child_id)
      enr0 <- primary_care(providers, children)
      fixed <- anti_join(enr0, varies, by = c('hh_id', 'child_id'))
      kids_varies <- semi_join(children, varies, by = c('hh_id', 'child_id'))
      enrollment <- stack_mi_draws(c(list(enr0), lapply(draws[-1], function(p) {
        bind_rows(fixed, primary_care(p, kids_varies)) %>% arrange(hh_id, child_id)
      })))
      cat('  Label draws: ', NSECE_MI_DRAWS, ' per child; children w/ drawn rows ',
          nrow(varies), ' of ', nrow(children), '\n', sep = '')

      list(
        providers = providers,
        enrollment = enrollment
      )
    }

    # -- Assertions: NSECE data integrity after processing ----

    # Every child must appear in exactly one enrollment record per label
    # draw (the primary care arrangement), NSECE_MI_DRAWS draws. If the
    # enrollment join + group_modify produced duplicates, child_weight is
    # silently double-counted in calibration. Draw weights sum to 1 per child.
    enrollment <- enrollment_data$enrollment
    n_enrollment_keys <- enrollment %>%
      distinct(hh_id, child_id, draw) %>%
      nrow()
    stopifnot(n_enrollment_keys == nrow(enrollment),
              nrow(enrollment) == NSECE_MI_DRAWS * nrow(children),
              abs(sum(enrollment$draw_weight) - nrow(children)) < 1e-6)

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
          file.path(paste0('nsece_', table_name, '_2019.csv')) %>%
          fwrite(processed_nsece_hh[[table_name]], file = ., na = 'NA')
      }
    )


  # --- SECTION 2: Process provider (supply-side) NSECE data ---
  # Must run BEFORE price model training to get supply-side prices
  cat('Processing 2019 NSECE supply data...\n')

  # Read home-based provider survey
  nsece_hb <- estimation_info$paths$NSECE %>%
    file.path('2019/37941-0007-Data.tsv') %>%
    fread() %>%
    tibble()

  # Read ECEC workforce survey
  nsece_wf <- estimation_info$paths$NSECE %>%
    file.path('2019/37941-0005-Data.tsv') %>%
    fread() %>%
    tibble()

  supply_params <- {

    processed_centers <- {
      # Then do classification and return
      processed_nsece_cb %>%
        left_join(medians, by = c('region', 'density', 'age_bin')) %>%
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

        # Get totals -- provider-weighted (HB9_METH_WEIGHT), as centre totals
        # (2026-10-12; unweighted sums before)
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
      year    = 2019,
      L_req   = L_req,
      w       = wage_rates,
      L       = L_0,
      e       = e,
      delta_0 = delta_0,
      prices  = prices
    )
  }

  {
    # Convert data to YAML-friendly structure with named elements (flattened)
    yaml_data <- list(
      year = 2019,

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

    # Write to estimation output folder (run-specific record)
    estimation_dir <- file.path(output_root, 'estimation', 'supply')
    dir.create(estimation_dir, recursive = TRUE, showWarnings = FALSE)
    write_yaml(yaml_data, file.path(estimation_dir, paste0('supply_', 2019, '.yaml')))

    cat('Supply parameters written to:\n')
    cat('  - ', file.path(estimation_dir, paste0('supply_', 2019, '.yaml')), '\n')
  }


  # --- SECTION 3: Price heterogeneity quantile grids ---
  # Per care type: weighted empirical distribution of price_wedge (household
  # OOP / base price), 99 quantiles. Replaces price QRF (2026-10-12) -- 5-fold
  # household-held-out CV: negative pseudo-R2, negative pinball skill vs
  # constant quantile for every care type, income-rank scheme. Low-price
  # center, Other Paid add one grid per income quintile (same CV: pinball
  # loss 8-13% lower every fold); cell < PRICE_WEDGE_MIN_CELL_HH households
  # -> unconditional grid
  {
    supply_prices <- supply_params$prices
    tc_output_root <- estimation_info$paths$output

    price_data <- {

      # Extract supply-side prices by sector
      supply_price_center_low  <- supply_prices['Low-Priced Center-Based']
      supply_price_center_high <- supply_prices['High-Priced Center-Based']
      supply_price_home        <- supply_prices['Paid Home-Based']


      # Income percentiles (child-weighted), ranked nationally per scheme
      # (income_rank.R): national_rpp (default) deflates income by region RPP,
      # national keeps nominal income. Quintile grids (center_low, other_paid),
      # price_by_income_quintile.csv; ACS side ranks same scheme
      # (price_wedge_income_pctiles())
      rank_hh <- processed_nsece_hh$households %>%
        filter(!is.na(region))
      ri <- income_rank_inputs(rank_hh$income, rank_hh$region, NULL, 'nsece')
      rank_hh <- rank_hh %>%
        transmute(hh_id, income_rank = ri$income, rank_group = ri$group)
      cat('  Price diagnostic income ranks: scheme', get_income_rank_scheme(), '\n')
      pctile_base <- processed_nsece_hh$children %>%
        left_join(processed_nsece_hh$households, by = 'hh_id') %>%
        left_join(rank_hh, by = 'hh_id') %>%
        filter(age < 5, income > 0, !is.na(region)) %>%
        mutate(income = income_rank)
      income_pctiles <- lapply(split(pctile_base, pctile_base$rank_group), function(d) {
        wtd.quantile(
          x      = d$income + runif(nrow(d)),
          weight = d$child_weight,
          probs  = seq(0.01, 0.99, 0.01)
        )
      })

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
        left_join(rank_hh, by = 'hh_id') %>%
        left_join(
          processed_nsece_hh$parent_units %>%
            select(hh_id, parent_unit_id, n_parents),
          by = c('hh_id', 'parent_unit_id')
        ) %>%
        mutate(
          # Bin = 1 + group's breaks strictly below income (= cut(right = TRUE))
          income_pctile = {
            p <- rep(NA_integer_, n())
            for (r in names(income_pctiles)) {
              i <- which(as.character(rank_group) == r)
              p[i] <- findInterval(income_rank[i], income_pctiles[[r]], left.open = TRUE) + 1L
            }
            p
          },
          # Quintile for grids; NA w/out positive income (ACS side same:
          # unconditional grid)
          income_quintile = if_else(!is.na(income) & income > 0,
                                    as.integer(ceiling(income_pctile / 20)), NA_integer_),
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
        # Returns: (df) Price wedge data with hh_id, income_pctile,
        #   income_quintile, region, n_parents, child_age, price_wedge, weight
        #--------------------------------------------------------------------------

        provider_base %>%
          filter(
            ecec_choice == choice,
            net_hourly_cost_status == 'OK',
            net_hourly_cost > 0
          ) %>%
          mutate(price_wedge = net_hourly_cost / base_price) %>%
          select(hh_id, income_pctile, income_quintile, region, n_parents, child_age,
                 price_wedge, weight = child_weight)
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

    price_wedge_quantiles <- {

      model_keys <- c('center_low_price', 'center_high_price', 'home_price', 'other_paid_price')

      # Wedge grids at percentiles 1..99, child-weighted, all care type's
      # provider rows (compute_price_wedge_cache.R reads grid[[type]][idx])
      quantile_grid <- map(model_keys, function(key) {
        d <- price_data[[key]]
        price_wedge_quantile_grid(d$price_wedge, d$weight)
      }) %>% set_names(PRICE_WEDGE_TYPES)

      # Income-quintile grids (PRICE_WEDGE_QUINTILE_TYPES): one per quintile,
      # thin cell -> unconditional grid (compute_price_wedge_cache.R reads
      # grid_by_quintile[[type]][[q]][idx])
      quintile_fits <- map(PRICE_WEDGE_QUINTILE_TYPES, function(k) {
        d <- price_data[[model_keys[match(k, PRICE_WEDGE_TYPES)]]]
        fit <- price_wedge_quintile_grids(d$price_wedge, d$weight, d$hh_id, d$income_quintile)
        cat('  Price wedge quintile grids ', k, ': households per quintile ',
            paste(fit$n_hh, collapse = ', '),
            '; effective sample size ', paste(round(fit$ess), collapse = ', '),
            '; largest weight share ', paste(round(fit$max_w_share, 3), collapse = ', '),
            if (length(fit$fallback) > 0) {
              paste0(' -- quintile(s) ', paste(fit$fallback, collapse = ', '), ' < ',
                     PRICE_WEDGE_MIN_CELL_HH, ' households, use unconditional grid')
            } else {
              ' -- no fallback'
            }, '\n', sep = '')
        fit
      }) %>% set_names(PRICE_WEDGE_QUINTILE_TYPES)
      quantile_grid_by_quintile <- map(quintile_fits, 'grids')

      summary_stats <- map(model_keys, function(key) {
        d <- price_data[[key]]
        list(
          n    = nrow(d),
          mean = weighted.mean(d$price_wedge, d$weight),
          sd   = sqrt(wtd.var(d$price_wedge, d$weight)),
          q25  = wtd.quantile(d$price_wedge, d$weight, 0.25),
          q50  = wtd.quantile(d$price_wedge, d$weight, 0.50),
          q75  = wtd.quantile(d$price_wedge, d$weight, 0.75)
        )
      }) %>% set_names(model_keys)
      summary_stats$supply_price_center_low  <- price_data$supply_price_center_low
      summary_stats$supply_price_center_high <- price_data$supply_price_center_high
      summary_stats$supply_price_home        <- price_data$supply_price_home
      summary_stats$other_paid_base_price    <- price_data$other_paid_base_price

      list(
        quantile_grid             = quantile_grid,
        quantile_grid_by_quintile = quantile_grid_by_quintile,
        quintile_n_hh             = map(quintile_fits, 'n_hh'),
        quintile_ess              = map(quintile_fits, 'ess'),
        quintile_max_w_share      = map(quintile_fits, 'max_w_share'),
        quintile_fallback         = map(quintile_fits, 'fallback'),
        income_rank_scheme        = get_income_rank_scheme(),
        summary_stats             = summary_stats
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
        s <- price_wedge_quantiles$summary_stats[[model_keys[i]]]
        tibble(
          model = model_keys[i], n_obs = s$n, mean = s$mean, sd = s$sd,
          q25 = s$q25, q50 = s$q50, q75 = s$q75,
          base_price_2019 = price_wedge_quantiles$summary_stats[[base_price_keys[i]]]
        )
      })

      write_csv(summary_df, file.path(models_dir, 'price_summary.csv'))

      # Quantile grids, one column per care type (as cached)
      write_csv(bind_cols(tibble(pctile = 1:99), as_tibble(price_wedge_quantiles$quantile_grid)),
                file.path(models_dir, 'price_wedge_quantiles.csv'))

      # Quintile grids, long: care type x quintile x pctile (as cached;
      # fallback = cell on unconditional grid)
      write_csv(
        map_dfr(PRICE_WEDGE_QUINTILE_TYPES, function(k) {
          map_dfr(1:5, function(q) {
            tibble(care_type = k, income_quintile = q, pctile = 1:99,
                   price_wedge = price_wedge_quantiles$quantile_grid_by_quintile[[k]][[q]],
                   n_hh = price_wedge_quantiles$quintile_n_hh[[k]][q],
                   fallback = q %in% price_wedge_quantiles$quintile_fallback[[k]])
          })
        }),
        file.path(models_dir, 'price_wedge_quantiles_by_quintile.csv')
      )

      #----------------------------------
      # 2. Distribution by region CSV
      #----------------------------------

      care_type_map <- c(center_low_price = 'center_low', center_high_price = 'center_high',
                         home_price = 'home', other_paid_price = 'other_paid')
      price_by_region <- map_dfr(names(care_type_map), function(key) {
        price_data[[key]] %>%
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
        price_data[[key]] %>%
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

      ss <- price_wedge_quantiles$summary_stats
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
        cat('=== BASE PRICES (2019 $/hour) ===\n')
        for (nm in names(base_prices)) cat(nm, ':', round(base_prices[[nm]], 2), '\n')
        cat('\n=== PRICE HETEROGENEITY (price_wedge = household_OOP / base_price) ===\n')
        for (nm in names(wedge_stats)) {
          cat(nm, ': mean =', round(wedge_stats[[nm]]$mean, 3),
              ', sd =', round(wedge_stats[[nm]]$sd, 3), '\n')
        }
      })

      writeLines(price_info, file.path(models_dir, 'price_info.txt'))
    }

    # Cache to disk
    cache_dir <- './cache/price_models'
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    saveRDS(price_wedge_quantiles, file.path(cache_dir, 'price_wedge_quantiles.rds'))
  }

  cat('NSECE estimation data preparation complete!\n')
}
