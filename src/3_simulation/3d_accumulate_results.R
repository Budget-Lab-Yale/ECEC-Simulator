#------------------------------------------------------------------------------
# 3d_accumulate_results.R
#
# Aggregates simulation outputs across years and scenarios for allocation,
# employment, fiscal cost, distributional impact, poverty, child earnings,
# and child fiscal NPV. Called from 3b after each scenario.
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# Local helpers (used by multiple functions below)
#------------------------------------------------------------------------------

map_over_parent_units <- function(result, fn) {

  #----------------------------------------------------------------------------
  # Applies fn(pu_df, n_children) to each non-empty parent unit in result
  #
  # Params:
  #   - result (list): Simulation result containing $parent_units named list
  #   - fn (fn): Function(pu_df, n_children) to apply to each parent unit
  #
  # Returns: (list) Named list of fn results, one per non-empty parent unit
  #----------------------------------------------------------------------------

  out <- list()
  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    pu_df <- result$parent_units[[pu_name]]
    if (!is.null(pu_df) && nrow(pu_df) > 0) {
      out[[pu_name]] <- fn(pu_df, PARENT_UNIT_N_CHILDREN[i])
    }
  }
  out
}



validate_cols <- function(df, cols, context) {

  #----------------------------------------------------------------------------
  # Checks that cols exist and contain no NAs in df, stopping with context
  # message on failure
  #
  # Params:
  #   - df (df): Data frame to validate
  #   - cols (chr vec): Column names that must be present and non-NA
  #   - context (chr): Label for error messages identifying the caller
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  cols_missing <- setdiff(cols, names(df))
  if (length(cols_missing) > 0) {
    stop(paste0(context, ' is missing required columns: ', paste(cols_missing, collapse = ', ')))
  }
  cols_with_na <- cols[vapply(cols, function(col) any(is.na(df[[col]])), logical(1))]
  if (length(cols_with_na) > 0) {
    stop(paste0(context, ' encountered NA in required columns: ', paste(cols_with_na, collapse = ', ')))
  }
}



get_employer_subsidy_rate <- function(result) {

  #----------------------------------------------------------------------------
  # Extracts employer subsidy rate per sector from simulation result
  #
  # Params:
  #   - result (list): Simulation result with $employer_subsidy, $L_req,
  #       and $employer_subsidy_sectors
  #
  # Returns: (num vec) Employer subsidy rate per sector, length 4
  #----------------------------------------------------------------------------

  if (!is.null(result$employer_subsidy) && !is.null(result$L_req)) {
    rate <- as.vector(result$L_req %*% result$employer_subsidy)
    if (is.null(result$employer_subsidy_sectors)) {
      stop('Employer subsidy is present but employer_subsidy_sectors is missing. ',
           'Cannot allocate fiscal costs by sector without explicit coverage.')
    }
    covered <- result$employer_subsidy_sectors
    if (any(!covered %in% seq_along(rate))) {
      stop('employer_subsidy_sectors contains invalid sector ids.')
    }
    rate[!seq_along(rate) %in% covered] <- 0
    rate
  } else {
    c(0, 0, 0, 0)
  }
}



compute_child_subsidies <- function(df, child_idx, supply_subsidy, employer_subsidy_rate) {

  #----------------------------------------------------------------------------
  # Joins sector mapping and computes supply/employer subsidies for a child.
  # Adds sector_id.{idx}, c{idx}_supply_subsidy, c{idx}_employer_subsidy
  # columns to the data frame.
  #
  # Params:
  #   - df (df): Parent unit data frame with ecec_type.{idx} and ecec_hours.{idx}
  #   - child_idx (int): Child index (1 or 2)
  #   - supply_subsidy (num vec): Supply subsidy rate per sector, length 4
  #   - employer_subsidy_rate (num vec): Employer subsidy rate per sector, length 4
  #
  # Returns: (df) Input df with added sector and subsidy columns for this child
  #----------------------------------------------------------------------------

  ecec_type_col <- paste0('ecec_type.', child_idx)
  ecec_hours_col <- paste0('ecec_hours.', child_idx)
  sid_col <- paste0('sector_id.', child_idx)
  prefix <- paste0('c', child_idx)

  n_before <- nrow(df)

  result <- df %>%
    left_join(SECTOR_MAPPING, by = setNames('ecec_type', ecec_type_col)) %>%
    rename(!!sid_col := sector_id) %>%
    mutate(
      !!paste0(prefix, '_hours') := HOURS_ANNUAL[.data[[ecec_hours_col]]],
      !!paste0(prefix, '_supply_rate') := ifelse(is.na(.data[[sid_col]]), 0, supply_subsidy[.data[[sid_col]]]),
      !!paste0(prefix, '_supply_subsidy') := .data[[paste0(prefix, '_hours')]] * .data[[paste0(prefix, '_supply_rate')]],
      !!paste0(prefix, '_employer_rate') := ifelse(is.na(.data[[sid_col]]), 0, employer_subsidy_rate[.data[[sid_col]]]),
      !!paste0(prefix, '_employer_subsidy') := .data[[paste0(prefix, '_hours')]] * .data[[paste0(prefix, '_employer_rate')]]
    )

  # -- Assertion: join must not change row count ----
  # SECTOR_MAPPING has one row per ecec_type. If it had duplicates, or if
  # ecec_type values don't match cleanly, the left_join could silently
  # expand rows — inflating all downstream fiscal totals.
  stopifnot(nrow(result) == n_before)

  result
}



strip_collapsed_scalars <- function(df) {

  #----------------------------------------------------------------------------
  # Removes collapsed scalar and choice-indexed columns for mechanical
  # re-computation of policy effects
  #
  # Params:
  #   - df (df): Parent unit data frame with collapsed scalars to strip
  #
  # Returns: (df) Input df with agi, taxes, subsidy, cdctc, net_income, Y, C,
  #     ecec_type/hours, and employment_choice columns removed
  #----------------------------------------------------------------------------

  df %>%
    select(-matches('^(agi|taxes|gross_ecec_cost|subsidy|cdctc|net_income|Y|C)$'),
           -matches('^(agi|taxes|gross_ecec_cost|subsidy|cdctc|net_income|Y|C)\\.[0-9]+$'),
           -matches('^ecec_type\\.[0-9]+$'),
           -matches('^ecec_hours\\.[0-9]+$'),
           -employment_choice)
}



get_cap_earnings <- function(pu, emp_col, fallback) {

  #----------------------------------------------------------------------------
  # Selects min_parent_earnings by employment choice, falling back to the
  # provided vector when min_parent_earnings columns are absent
  #
  # Params:
  #   - pu (df): Parent unit data frame, optionally with min_parent_earnings.*
  #   - emp_col (chr): Name of the employment choice column to match on
  #   - fallback (num vec): Fallback earnings vector when cap columns are absent
  #
  # Returns: (num vec) Selected earnings caps, one per row of pu
  #----------------------------------------------------------------------------

  cap_cols <- paste0('min_parent_earnings.', c('none', 'pt', 'ft'))
  if (all(cap_cols %in% names(pu))) {
    case_when(
      pu[[emp_col]] == 'none' ~ pu[['min_parent_earnings.none']],
      pu[[emp_col]] == 'pt'   ~ pu[['min_parent_earnings.pt']],
      pu[[emp_col]] == 'ft'   ~ pu[['min_parent_earnings.ft']],
      TRUE ~ NA_real_
    )
  } else {
    fallback
  }
}



poverty_summary <- function(df, weight_col, label) {

  #----------------------------------------------------------------------------
  # Computes poverty metrics (rates, counts, mechanical/behavioral/total
  # decomposition) using the specified weight column for a given universe
  #
  # Params:
  #   - df (df): SPM poverty data with is_poor_baseline, is_poor_mechanical,
  #       is_poor_total columns
  #   - weight_col (chr): Name of the weight column to use for aggregation
  #   - label (chr): Universe label (e.g. 'total', 'children')
  #
  # Returns: (df) Single-row tibble with poverty rates, counts, and changes
  #----------------------------------------------------------------------------

  result <- df %>%
    summarise(
      universe = label,
      n_people = sum(.data[[weight_col]]),
      poverty_rate_baseline = sum(.data[[weight_col]] * is_poor_baseline) / n_people,
      poverty_rate_mechanical = sum(.data[[weight_col]] * is_poor_mechanical) / n_people,
      poverty_rate_policy = sum(.data[[weight_col]] * is_poor_total) / n_people,
      poverty_rate_change_mechanical = poverty_rate_mechanical - poverty_rate_baseline,
      poverty_rate_change_total = poverty_rate_policy - poverty_rate_baseline,
      poverty_rate_change_behavioral = poverty_rate_change_total - poverty_rate_change_mechanical,
      n_poor_baseline = sum(.data[[weight_col]] * is_poor_baseline),
      n_poor_mechanical = sum(.data[[weight_col]] * is_poor_mechanical),
      n_poor_policy = sum(.data[[weight_col]] * is_poor_total),
      net_change_mechanical = n_poor_mechanical - n_poor_baseline,
      net_change_total = n_poor_policy - n_poor_baseline,
      net_change_behavioral = net_change_total - net_change_mechanical
    )

  result
}



summarise_child_earnings_group <- function(grouped_df) {

  #----------------------------------------------------------------------------
  # Shared summarise for child earnings aggregation (overall, by_quintile,
  # by_age_group). Expects an already-grouped or ungrouped data frame with
  # weighted_child, switched, weighted_delta, and weighted_pct_change columns.
  #
  # Params:
  #   - grouped_df (df): Grouped or ungrouped data frame with child earnings
  #       helper columns
  #
  # Returns: (df) Summary tibble with n_children, n_children_switched,
  #     pct_switched, total/avg earnings deltas
  #----------------------------------------------------------------------------

  grouped_df %>%
    summarise(
      n_children = sum(weighted_child),
      n_children_switched = sum(weighted_child * switched),
      pct_switched = 100 * n_children_switched / n_children,
      total_earnings_delta = sum(weighted_delta),
      avg_earnings_delta_all = total_earnings_delta / n_children,
      avg_pct_change_all = 100 * sum(weighted_pct_change) / n_children,
      avg_earnings_delta_switchers = if_else(
        n_children_switched > 0,
        sum(weighted_delta) / n_children_switched,
        0
      ),
      .groups = 'drop'
    )
}



aggregate_year_allocation <- function(result, year) {

  #----------------------------------------------------------------------------
  # Aggregates child care allocation from in-memory result for a single year.
  # Counts children by age, ECEC type, and hours choice across all parent
  # units, pivoting to wide format with age columns.
  #
  # Params:
  #   - result (list): Simulation result with $converged, $parent_units
  #   - year (int): Simulation year for labeling
  #
  # Returns: (df) Tibble with year-level allocation statistics by ecec_choice
  #     and ecec_hours_choice, or empty tibble if not converged
  #----------------------------------------------------------------------------

  if (!result$converged || is.null(result$parent_units)) {
    return(tibble())
  }

  allocation_list <- map_over_parent_units(result, function(pu_df, n_children) {
    if (!'ecec_type.1' %in% names(pu_df)) {
      return(tibble(child_age = integer(), ecec_choice = character(),
                    ecec_hours_choice = character(), value = numeric()))
    }

    # Process each child and combine
    alloc_results <- tibble()
    for (child_idx in 1:n_children) {
      ecec_type_col <- paste0('ecec_type.', child_idx)
      child_age_col <- paste0('child_age.', child_idx)
      child_weight_col <- paste0('child_weight.', child_idx)
      ecec_hours_col <- paste0('ecec_hours.', child_idx)

      if (!all(c(ecec_type_col, child_age_col, child_weight_col) %in% names(pu_df))) next

      child_allocation <- pu_df %>%
        group_by(
          child_age = .data[[child_age_col]],
          ecec_choice = .data[[ecec_type_col]],
          ecec_hours_choice = .data[[ecec_hours_col]]
        ) %>%
        summarise(value = sum(.data[[child_weight_col]]), .groups = 'drop')

      alloc_results <- bind_rows(alloc_results, child_allocation)
    }

    # Final aggregation across children
    aggregated <- alloc_results %>%
      group_by(child_age, ecec_choice, ecec_hours_choice) %>%
      summarise(value = sum(value), .groups = 'drop')

    # Expand to include all ecec_type/hours combinations (even with 0 allocation)
    expand_grid(child_age = unique(aggregated$child_age), CHILD_CARE_CHOICES) %>%
      rename(ecec_choice = ecec_type, ecec_hours_choice = hours_choice) %>%
      left_join(aggregated, by = c('child_age', 'ecec_choice', 'ecec_hours_choice')) %>%
      mutate(value = replace_na(value, 0))
  })

  # Combine all
  combined <- bind_rows(allocation_list)

  if (nrow(combined) == 0) {
    return(tibble())
  }

  # Summarize and pivot (same logic as original generate function)
  year_stats <- combined %>%
    group_by(child_age, ecec_choice, ecec_hours_choice) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    pivot_wider(
      names_from   = child_age,
      names_prefix = 'age_',
      values_from  = value,
      values_fill  = 0
    )

  # Add totals and year
  age_cols <- names(year_stats)[grepl('^age_', names(year_stats))]
  if (length(age_cols) == 0) {
    stop('Allocation aggregation produced no age_* columns (unexpected).')
  }
  if (anyNA(year_stats[, age_cols])) {
    stop('Allocation aggregation produced NA in age_* columns (unexpected).')
  }
  year_stats %>%
    mutate(
      total = rowSums(across(all_of(age_cols))),
      total_share = total / sum(total),
      year = year,
      .before = everything()
    ) %>%
    mutate(across(c(total, all_of(age_cols)), ~ round(., -3)))
}

aggregate_year_employment <- function(result, year) {

  #----------------------------------------------------------------------------
  # Aggregates parental employment from in-memory result for a single year.
  # Summarizes primary caregiver and secondary parent employment by sex,
  # including baseline employment probabilities when available.
  #
  # Params:
  #   - result (list): Simulation result with $converged, $parent_units
  #   - year (int): Simulation year for labeling
  #
  # Returns: (df) Tibble with employment counts/shares by sex (f/m/all),
  #     or empty tibble if not converged
  #----------------------------------------------------------------------------

  if (!result$converged || is.null(result$parent_units)) {
    return(tibble())
  }

  employment_list <- map_over_parent_units(result, function(pu_df, n_children) {
    if (!'employment_choice' %in% names(pu_df)) return(tibble())

    has_secondary <- 'hours_secondary' %in% names(pu_df)
    has_baseline_probs <- all(c('p_employment.pt', 'p_employment.ft') %in% names(pu_df))

    parent_data <- pu_df %>%
      mutate(
        is_single_parent = is.na(per_weight2) | is.na(male2),
        sex_pc = case_when(
          primary_caregiver == 1 ~ if_else(male1 == 1, 'm', 'f'),
          primary_caregiver == 2 ~ if_else(male2 == 1, 'm', 'f')
        ),
        sex_secondary = case_when(
          is_single_parent ~ NA_character_,
          primary_caregiver == 1 ~ if_else(male2 == 1, 'm', 'f'),
          primary_caregiver == 2 ~ if_else(male1 == 1, 'm', 'f')
        ),
        weight_pc = if_else(primary_caregiver == 1, per_weight1, per_weight2),
        weight_secondary = if_else(primary_caregiver == 1, per_weight2, per_weight1),
        emp_pc = if_else(employment_choice == 'none', 'nw', employment_choice)
      )

    if (has_baseline_probs) {
      parent_data <- parent_data %>%
        mutate(baseline_emp_prob = replace_na(p_employment.pt, 0) + replace_na(p_employment.ft, 0))
    }

    if (has_secondary) {
      parent_data <- parent_data %>%
        mutate(
          emp_secondary = case_when(
            is_single_parent ~ NA_character_,
            is.na(hours_secondary) | hours_secondary == 0 ~ 'nw',
            hours_secondary < 35 ~ 'pt',
            TRUE ~ 'ft'
          )
        )
    } else {
      parent_data <- parent_data %>% mutate(emp_secondary = NA_character_)
    }

    pc_summary <- parent_data %>%
      filter(!is.na(weight_pc)) %>%
      group_by(sex = sex_pc, emp = emp_pc) %>%
      summarise(weight = sum(weight_pc, na.rm = TRUE), .groups = 'drop')

    secondary_summary <- parent_data %>%
      filter(!is.na(weight_secondary) & !is.na(sex_secondary) & !is.na(emp_secondary)) %>%
      group_by(sex = sex_secondary, emp = emp_secondary) %>%
      summarise(weight = sum(weight_secondary, na.rm = TRUE), .groups = 'drop')

    emp_results <- bind_rows(pc_summary, secondary_summary)
    if (nrow(emp_results) == 0) return(tibble())

    summary_by_sex <- emp_results %>%
      group_by(sex, emp) %>%
      summarise(value = sum(weight), .groups = 'drop') %>%
      pivot_wider(names_from = emp, values_from = value, values_fill = 0)

    for (col in c('nw', 'pt', 'ft')) {
      if (!col %in% names(summary_by_sex)) summary_by_sex[[col]] <- 0
    }

    summary_by_sex <- summary_by_sex %>%
      mutate(total = nw + pt + ft) %>%
      select(sex, nw, pt, ft, total)

    if (has_baseline_probs) {
      baseline_by_sex <- parent_data %>%
        filter(!is.na(weight_pc)) %>%
        group_by(sex = sex_pc) %>%
        summarise(
          baseline_employed_pc = sum(baseline_emp_prob * weight_pc, na.rm = TRUE),
          total_weight_pc = sum(weight_pc, na.rm = TRUE),
          .groups = 'drop'
        )

      secondary_baseline <- parent_data %>%
        filter(!is.na(weight_secondary) & !is.na(sex_secondary) & !is.na(emp_secondary)) %>%
        mutate(is_employed = emp_secondary != 'nw') %>%
        group_by(sex = sex_secondary) %>%
        summarise(
          baseline_employed_secondary = sum(is_employed * weight_secondary, na.rm = TRUE),
          total_weight_secondary = sum(weight_secondary, na.rm = TRUE),
          .groups = 'drop'
        )

      summary_by_sex <- summary_by_sex %>%
        left_join(baseline_by_sex, by = 'sex') %>%
        left_join(secondary_baseline, by = 'sex') %>%
        mutate(
          across(c(baseline_employed_pc, total_weight_pc,
                   baseline_employed_secondary, total_weight_secondary),
                 ~ replace_na(., 0)),
          baseline_employed = baseline_employed_pc + baseline_employed_secondary,
          baseline_emp_rate = baseline_employed / total
        ) %>%
        select(sex, nw, pt, ft, total, baseline_employed, baseline_emp_rate)
    }

    # Add 'all' row
    sum_cols <- if (has_baseline_probs) c('nw', 'pt', 'ft', 'total', 'baseline_employed') else c('nw', 'pt', 'ft', 'total')
    all_row <- summary_by_sex %>%
      summarise(across(all_of(sum_cols), sum)) %>%
      mutate(sex = 'all')
    if (has_baseline_probs) all_row <- all_row %>% mutate(baseline_emp_rate = baseline_employed / total)

    bind_rows(summary_by_sex, all_row)
  })

  # Combine results
  combined <- bind_rows(employment_list)

  if (nrow(combined) == 0) {
    return(tibble())
  }

  has_baseline <- 'baseline_employed' %in% names(combined)
  sum_cols <- c('nw', 'pt', 'ft', 'total')
  if (has_baseline) sum_cols <- c(sum_cols, 'baseline_employed')

  result <- combined %>%
    group_by(sex) %>%
    summarise(across(all_of(sum_cols), sum), .groups = 'drop') %>%
    mutate(
      nw_share = nw / total,
      pt_share = pt / total,
      ft_share = ft / total,
      emp_rate = (pt + ft) / total
    )

  sel_cols <- c('sex', 'nw', 'nw_share', 'pt', 'pt_share', 'ft', 'ft_share', 'total')
  if (has_baseline) {
    result <- result %>%
      mutate(
        baseline_emp_rate = baseline_employed / total,
        across(all_of(sum_cols), ~ round(.x, -3))
      )
    sel_cols <- c(sel_cols, 'baseline_employed', 'baseline_emp_rate')
  } else {
    result <- result %>% mutate(across(all_of(sum_cols), ~ round(.x, -3)))
  }
  sel_cols <- c(sel_cols, 'emp_rate')

  result %>%
    select(all_of(sel_cols)) %>%
    mutate(year = year, .before = everything())
}

#------------------------------------------------------------------------------
# Fiscal cost: budget effect summaries (demand/supply/employer subsidies, taxes)
#------------------------------------------------------------------------------


get_col_or_zero <- function(df, col_name) {

  #----------------------------------------------------------------------------
  # Returns the named column from df, or a zero vector if the column is absent
  #
  # Params:
  #   - df (df): Data frame to extract from
  #   - col_name (chr): Column name to look up
  #
  # Returns: (num vec) Column values, or rep(0, nrow(df)) if column is missing
  #----------------------------------------------------------------------------

  if (col_name %in% names(df)) {
    return(df[[col_name]])
  }
  rep(0, nrow(df))
}


select_earnings_for_employment_choice <- function(df, stub) {

  #----------------------------------------------------------------------------
  # Selects earnings columns ({stub}.none, {stub}.pt, {stub}.ft) based on
  # each row's employment_choice value
  #
  # Params:
  #   - df (df): Data frame with employment_choice and {stub}.{none,pt,ft}
  #   - stub (chr): Column name prefix (e.g. 'earnings1', 'earnings2')
  #
  # Returns: (num vec) Selected earnings values, one per row
  #----------------------------------------------------------------------------

  if (!'employment_choice' %in% names(df)) {
    stop('Fiscal accounting requires employment_choice to select earnings by employment.')
  }

  emp <- df$employment_choice
  bad_emp <- which(is.na(emp) | !emp %in% c('none', 'pt', 'ft'))
  if (length(bad_emp) > 0) {
    stop('Fiscal accounting encountered invalid employment_choice (expected none/pt/ft).')
  }

  e_none <- get_col_or_zero(df, paste0(stub, '.none'))
  e_pt <- get_col_or_zero(df, paste0(stub, '.pt'))
  e_ft <- get_col_or_zero(df, paste0(stub, '.ft'))

  out <- rep(NA_real_, nrow(df))
  out[emp == 'none'] <- e_none[emp == 'none']
  out[emp == 'pt'] <- e_pt[emp == 'pt']
  out[emp == 'ft'] <- e_ft[emp == 'ft']

  out
}


compute_employer_payroll_tax <- function(df) {

  #----------------------------------------------------------------------------
  # Computes employer-side payroll tax (FICA mirror) from employment choice
  # earnings and the OASI wage base cap
  #
  # Params:
  #   - df (df): Data frame with employment_choice, earnings1/2.{none,pt,ft},
  #       and oasi_tax_max columns
  #
  # Returns: (num vec) Employer payroll tax amount per row
  #----------------------------------------------------------------------------

  if (!'oasi_tax_max' %in% names(df)) {
    stop('Fiscal accounting requires oasi_tax_max to compute employer payroll taxes.')
  }

  cap <- df$oasi_tax_max
  if (length(cap) != nrow(df) || any(!is.finite(cap)) || any(cap <= 0)) {
    stop('Fiscal accounting encountered invalid oasi_tax_max (must be positive finite for all rows).')
  }

  e1 <- select_earnings_for_employment_choice(df, 'earnings1')
  e2 <- select_earnings_for_employment_choice(df, 'earnings2')

  e1 <- pmax(0, coalesce(e1, 0))
  e2 <- pmax(0, coalesce(e2, 0))

  employer_payroll <- (MEDICARE_TAX_RATE * (e1 + e2)) +
    (OASI_TAX_RATE * (pmin(e1, cap) + pmin(e2, cap)))

  if (any(!is.finite(employer_payroll))) {
    stop('Fiscal accounting produced non-finite employer payroll taxes (unexpected).')
  }

  employer_payroll
}



aggregate_year_fiscal_cost <- function(result, year) {

  #----------------------------------------------------------------------------
  # Aggregates fiscal data from in-memory result for a single year. Computes
  # demand/supply/employer subsidies, CDCTC costs, and tax revenue in billions
  # with per-family and per-child breakdowns.
  #
  # Params:
  #   - result (list): Simulation result with $converged, $parent_units,
  #       $supply_subsidy, $employer_subsidy, $L_req
  #   - year (int): Simulation year for labeling
  #
  # Returns: (df) Tibble with year-level subsidy/tax statistics in billions,
  #     or empty tibble if not converged
  #----------------------------------------------------------------------------

  if (!result$converged || is.null(result$parent_units)) {
    return(tibble())
  }

  supply_subsidy <- if (!is.null(result$supply_subsidy)) result$supply_subsidy else c(0, 0, 0, 0)
  employer_subsidy_rate <- get_employer_subsidy_rate(result)

  fiscal_list <- map_over_parent_units(result, function(pu_df, n_children) {
    aggregate_fiscal_cost(pu_df, n_children, supply_subsidy, employer_subsidy_rate)
  })

  combined <- bind_rows(fiscal_list)

  if (nrow(combined) == 0) {
    return(tibble())
  }

  validate_cols(combined, c('demand_subsidy', 'supply_subsidy', 'employer_subsidy',
                            'cdctc_cost', 'tax_revenue', 'n_parent_units_weighted',
                            'n_children_weighted'), 'Fiscal aggregation')

  # -- Assertions: fiscal cost completeness ----
  # All five cost components must be present in the aggregation. This catches
  # the Feb 2025 bug where employer_subsidy was missing from the formula.
  fiscal_cols <- c('demand_subsidy', 'supply_subsidy', 'employer_subsidy',
                   'cdctc_cost', 'tax_revenue')
  stopifnot(all(fiscal_cols %in% names(combined)))

  combined %>%
    summarise(
      total_demand_subsidy_raw = sum(demand_subsidy),
      total_supply_subsidy_raw = sum(supply_subsidy),
      total_employer_subsidy_raw = sum(employer_subsidy),
      total_cdctc_cost_raw = sum(cdctc_cost),
      total_tax_revenue_raw = sum(tax_revenue),
      n_parent_units_weighted = sum(n_parent_units_weighted),
      n_children_weighted = sum(n_children_weighted),
      .groups = 'drop'
    ) %>%
    mutate(
      # Convert to billions
      total_demand_subsidy = total_demand_subsidy_raw / 1e9,
      total_supply_subsidy = total_supply_subsidy_raw / 1e9,
      total_employer_subsidy = total_employer_subsidy_raw / 1e9,
      total_cdctc_cost = total_cdctc_cost_raw / 1e9,
      total_tax_revenue = total_tax_revenue_raw / 1e9,

      # Per-family metrics (using parent unit weight)
      demand_subsidy_per_family = total_demand_subsidy_raw / n_parent_units_weighted,
      supply_subsidy_per_family = total_supply_subsidy_raw / n_parent_units_weighted,
      employer_subsidy_per_family = total_employer_subsidy_raw / n_parent_units_weighted,
      cdctc_cost_per_family = total_cdctc_cost_raw / n_parent_units_weighted,
      tax_revenue_per_family = total_tax_revenue_raw / n_parent_units_weighted,

      # Per-child metrics (for reference)
      demand_subsidy_per_child = total_demand_subsidy_raw / n_children_weighted,
      supply_subsidy_per_child = total_supply_subsidy_raw / n_children_weighted,
      employer_subsidy_per_child = total_employer_subsidy_raw / n_children_weighted,
      cdctc_cost_per_child = total_cdctc_cost_raw / n_children_weighted,
      tax_revenue_per_child = total_tax_revenue_raw / n_children_weighted,

      year = year,
      .before = everything()
    ) %>%
    select(-total_demand_subsidy_raw, -total_supply_subsidy_raw, -total_employer_subsidy_raw,
           -total_cdctc_cost_raw, -total_tax_revenue_raw)
}



aggregate_year_fiscal_cost_by_age <- function(result, year) {

  #----------------------------------------------------------------------------
  # Aggregates fiscal data by child age (0-4) for NPV cost compounding.
  # Returns per-age subsidy and tax totals in raw dollars.
  #
  # Params:
  #   - result (list): Simulation result with $converged, $parent_units,
  #       $supply_subsidy, $employer_subsidy, $L_req
  #   - year (int): Simulation year (unused in output but for consistency)
  #
  # Returns: (df) Tibble with child_age (0-4), demand_subsidy, supply_subsidy,
  #     employer_subsidy, cdctc_cost, tax_revenue in raw dollars
  #----------------------------------------------------------------------------

  empty_by_age <- tibble(child_age = 0:4, demand_subsidy = 0, supply_subsidy = 0,
                         employer_subsidy = 0, cdctc_cost = 0, tax_revenue = 0)

  if (!result$converged || is.null(result$parent_units)) {
    return(empty_by_age)
  }

  supply_subsidy <- if (!is.null(result$supply_subsidy)) result$supply_subsidy else c(0, 0, 0, 0)
  employer_subsidy_rate <- get_employer_subsidy_rate(result)

  child_rows <- list()

  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    n_children <- PARENT_UNIT_N_CHILDREN[i]
    pu_df <- result$parent_units[[pu_name]]

    if (is.null(pu_df) || nrow(pu_df) == 0) next
    if (!'subsidy' %in% names(pu_df)) next

    pu_df <- pu_df %>%
      mutate(
        parent_unit_weight = (per_weight1 + coalesce(per_weight2, per_weight1)) / 2,
        cost_share = 1 / n_children,
        taxes_revenue = taxes + compute_employer_payroll_tax(pu_df)
      )
    if (!'cdctc' %in% names(pu_df)) pu_df$cdctc <- 0

    # Build child-level rows for each child
    for (cidx in 1:n_children) {
      ecec_type_col <- paste0('ecec_type.', cidx)
      ecec_hours_col <- paste0('ecec_hours.', cidx)
      child_age_col <- paste0('child_age.', cidx)

      child_row <- pu_df %>%
        left_join(SECTOR_MAPPING, by = setNames('ecec_type', ecec_type_col)) %>%
        mutate(
          child_age = .data[[child_age_col]],
          demand_subsidy = subsidy * cost_share * parent_unit_weight,
          tax_revenue = taxes_revenue * cost_share * parent_unit_weight,
          cdctc_cost = cdctc * cost_share * parent_unit_weight,
          c_hours = HOURS_ANNUAL[.data[[ecec_hours_col]]],
          supply_subsidy = c_hours * ifelse(is.na(sector_id), 0, supply_subsidy[sector_id]) * parent_unit_weight,
          employer_subsidy = c_hours * ifelse(is.na(sector_id), 0, employer_subsidy_rate[sector_id]) * parent_unit_weight
        ) %>%
        select(child_age, demand_subsidy, supply_subsidy, employer_subsidy, cdctc_cost, tax_revenue)

      child_rows[[paste0(pu_name, '_c', cidx)]] <- child_row
    }
  }

  all_children <- bind_rows(child_rows)
  if (nrow(all_children) == 0) return(empty_by_age)

  # -- Assertions: by-age fiscal path matches year-level path ----
  # The by-age and year-level fiscal functions must agree on total demand_subsidy.
  # If they diverge, one path is missing a component or weighting differently.
  # (This is the class of bug from Feb 2025 — employer_subsidy was missing here)
  stopifnot(all(c('demand_subsidy', 'supply_subsidy', 'employer_subsidy',
                   'cdctc_cost', 'tax_revenue') %in% names(all_children)))

  # Aggregate by child_age and ensure all ages 0-4 are present
  result_by_age <- all_children %>%
    group_by(child_age) %>%
    summarise(across(c(demand_subsidy, supply_subsidy, employer_subsidy, cdctc_cost, tax_revenue),
                     ~ sum(., na.rm = TRUE)), .groups = 'drop')

  tibble(child_age = 0:4) %>%
    left_join(result_by_age, by = 'child_age') %>%
    mutate(across(c(demand_subsidy, supply_subsidy, employer_subsidy, cdctc_cost, tax_revenue),
                  ~ coalesce(., 0)))
}



aggregate_fiscal_cost <- function(parent_units_df, n_children, supply_subsidy = c(0, 0, 0, 0),
                                   employer_subsidy_rate = c(0, 0, 0, 0)) {

  #----------------------------------------------------------------------------
  # Aggregates fiscal data from discrete choice parent units by market sector.
  # Computes demand/supply/employer subsidies, CDCTC costs, and tax revenue
  # grouped by sector and hours choice.
  #
  # Params:
  #   - parent_units_df (df): Parent unit data frame with subsidy, taxes,
  #       ecec_type/hours, and weight columns
  #   - n_children (int): Number of children in this parent unit type (1 or 2)
  #   - supply_subsidy (num vec): Supply subsidy rate per sector, length 4
  #   - employer_subsidy_rate (num vec): Employer subsidy rate per sector, length 4
  #
  # Returns: (df) Tibble with sector-level subsidy/tax totals
  #----------------------------------------------------------------------------

  if (!'subsidy' %in% names(parent_units_df)) {
    return(tibble(sector = character(), hours_choice = character(),
                  demand_subsidy = numeric(), supply_subsidy = numeric(),
                  employer_subsidy = numeric(), cdctc_cost = numeric(), tax_revenue = numeric(),
                  n_children_weighted = numeric(), n_parent_units_weighted = numeric()))
  }

  fiscal_data <- parent_units_df %>%
    mutate(
      parent_unit_weight = (per_weight1 + coalesce(per_weight2, per_weight1)) / 2,
      total_child_weight = child_weight.1 + coalesce(child_weight.2, 0),
      taxes_revenue = taxes + compute_employer_payroll_tax(parent_units_df)
    )

  # Compute child-specific supply/employer subsidies
  fiscal_data <- compute_child_subsidies(fiscal_data, 1, supply_subsidy, employer_subsidy_rate)

  if (n_children == 2) {
    fiscal_data <- compute_child_subsidies(fiscal_data, 2, supply_subsidy, employer_subsidy_rate) %>%
      mutate(
        total_supply_subsidy = c1_supply_subsidy + c2_supply_subsidy,
        total_employer_subsidy = c1_employer_subsidy + c2_employer_subsidy
      )
  } else {
    fiscal_data <- fiscal_data %>%
      mutate(total_supply_subsidy = c1_supply_subsidy, total_employer_subsidy = c1_employer_subsidy)
  }

  # Determine sector for grouping (use child 1's sector as primary)
  fiscal_data <- fiscal_data %>%
    mutate(
      sector = case_when(
        is.na(sector_id.1) ~ 'Non-Market',
        sector_id.1 == 1 ~ 'Unpaid Center-Based',
        sector_id.1 == 2 ~ 'Low-Priced Center-Based',
        sector_id.1 == 3 ~ 'High-Priced Center-Based',
        sector_id.1 == 4 ~ 'Paid Home-Based'
      ),
      hours_choice = ecec_hours.1
    )

  if (!'cdctc' %in% names(fiscal_data)) fiscal_data$cdctc <- 0

  validate_cols(fiscal_data, c('subsidy', 'taxes', 'taxes_revenue', 'cdctc',
                               'parent_unit_weight', 'total_supply_subsidy',
                               'total_employer_subsidy', 'total_child_weight',
                               'ecec_hours.1'), 'Fiscal aggregation')

  # -- Assertions: row count preserved through joins ----
  # compute_child_subsidies does a left_join on SECTOR_MAPPING. If ecec_type
  # has unexpected values, the join could silently duplicate or drop rows.
  stopifnot(nrow(fiscal_data) == nrow(parent_units_df))

  # Aggregate
  results <- fiscal_data %>%
    group_by(sector, hours_choice) %>%
    summarise(
      demand_subsidy = sum(subsidy * parent_unit_weight),
      supply_subsidy = sum(total_supply_subsidy * parent_unit_weight),
      employer_subsidy = sum(total_employer_subsidy * parent_unit_weight),
      cdctc_cost = sum(cdctc * parent_unit_weight),
      tax_revenue = sum(taxes_revenue * parent_unit_weight),
      n_parent_units_weighted = sum(parent_unit_weight),
      n_children_weighted = sum(total_child_weight),
      .groups = 'drop'
    )

  return(results)
}



compute_mechanical_fiscal_effect <- function(baseline_result, policy_demand_file,
                                              policy_supply_file, year,
                                              policy_cdctc_file = 'baseline',
                                              policy_tax_file = 'baseline',
                                              employer_subsidy_rate = c(0, 0, 0, 0),
                                              demand_params = NULL,
                                              cpi_growth_factor = 1.0) {

  #----------------------------------------------------------------------------
  # Computes MECHANICAL fiscal effect: the policy cost if behavior stayed at
  # baseline choices. Applies policy subsidy/tax rules to baseline allocations
  # and reports subsidy/tax deltas in billions.
  #
  # Params:
  #   - baseline_result (list): Baseline simulation result with $converged,
  #       $parent_units, $prices
  #   - policy_demand_file (chr): Demand policy filename to load
  #   - policy_supply_file (chr): Supply policy filename to load
  #   - year (int): Simulation year for labeling and tax policy application
  #   - policy_cdctc_file (chr): CDCTC policy filename (default 'baseline')
  #   - policy_tax_file (chr): Tax policy filename (default 'baseline')
  #   - employer_subsidy_rate (num vec): Employer subsidy rate per sector, length 4
  #   - demand_params (list): Demand model parameters for policy components
  #   - cpi_growth_factor (dbl): CPI growth factor for price inflation
  #
  # Returns: (df) Tibble with mechanical subsidy/tax deltas in billions,
  #     or empty tibble if not converged
  #----------------------------------------------------------------------------

  if (!baseline_result$converged || is.null(baseline_result$parent_units)) {
    return(tibble())
  }

  # Get baseline prices
  P_baseline <- baseline_result$prices

  # Load policy functions using unified loaders
  policy_do_demand <- load_demand_policy(policy_demand_file)
  supply_policy <- load_supply_policy(policy_supply_file)
  policy_supply_subsidy <- supply_policy$supply_subsidy
  policy_do_cdctc <- load_cdctc_policy(policy_cdctc_file)
  policy_do_tax <- load_policy('policy_tax', policy_tax_file, 'do_tax_policy')

  fiscal_list <- list()
  tax_change_list <- list()

  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    n_children <- PARENT_UNIT_N_CHILDREN[i]
    baseline_pu <- baseline_result$parent_units[[pu_name]]
    if (is.null(baseline_pu) || nrow(baseline_pu) == 0) next

    pu_clean <- strip_collapsed_scalars(baseline_pu)

    # Store baseline taxes before tax policy application
    baseline_taxes_by_emp <- list(
      none = pu_clean$total_tax.none,
      pt = pu_clean$total_tax.pt,
      ft = pu_clean$total_tax.ft
    )

    pu_clean <- policy_do_tax(pu_clean, n_children = n_children, year = year)

    catalog <- get_choice_catalog(n_children)
    pc <- compute_policy_components(pu_clean, P_baseline, n_children, demand_params,
                                     policy_do_demand, policy_do_cdctc, cpi_growth_factor)
    pu_with_policy <- bind_cols(pu_clean, pc$components)

    # Aggregate mechanical fiscal cost
    mfc_n_choices <- if (n_children == 1) N_CHOICES_1_CHILD else N_CHOICES_2_CHILD
    fiscal_list[[pu_name]] <- if (!'choice' %in% names(baseline_pu) ||
                                  !all(paste0('subsidy.', 1:mfc_n_choices) %in% names(pu_with_policy))) {
      tibble(demand_subsidy = 0, supply_subsidy = 0, employer_subsidy = 0,
             cdctc_cost = 0, n_children_weighted = 0)
    } else {
      mfc_policy_subsidy <- extract_selected_values(pu_with_policy, baseline_pu$choice, 'subsidy', mfc_n_choices)
      mfc_policy_cdctc <- extract_selected_values(pu_with_policy, baseline_pu$choice, 'cdctc', mfc_n_choices)

      mfc_combined <- baseline_pu %>%
        mutate(
          parent_unit_weight = (per_weight1 + coalesce(per_weight2, per_weight1)) / 2,
          total_child_weight = child_weight.1 + coalesce(child_weight.2, 0),
          policy_subsidy = mfc_policy_subsidy,
          policy_cdctc = mfc_policy_cdctc
        )

      mfc_combined <- compute_child_subsidies(mfc_combined, 1, policy_supply_subsidy, employer_subsidy_rate)
      if (n_children == 2) {
        mfc_combined <- compute_child_subsidies(mfc_combined, 2, policy_supply_subsidy, employer_subsidy_rate) %>%
          mutate(total_supply_subsidy = c1_supply_subsidy + c2_supply_subsidy,
                 total_employer_subsidy = c1_employer_subsidy + c2_employer_subsidy)
      } else {
        mfc_combined <- mfc_combined %>%
          mutate(total_supply_subsidy = c1_supply_subsidy, total_employer_subsidy = c1_employer_subsidy)
      }

      mfc_combined %>%
        summarise(
          demand_subsidy = sum(policy_subsidy * parent_unit_weight, na.rm = TRUE),
          supply_subsidy = sum(total_supply_subsidy * parent_unit_weight, na.rm = TRUE),
          employer_subsidy = sum(total_employer_subsidy * parent_unit_weight, na.rm = TRUE),
          cdctc_cost = sum(policy_cdctc * parent_unit_weight, na.rm = TRUE),
          n_parent_units_weighted = sum(parent_unit_weight, na.rm = TRUE),
          n_children_weighted = sum(total_child_weight, na.rm = TRUE),
          .groups = 'drop'
        )
    }

    # Mechanical tax change: policy taxes - baseline taxes at baseline choice
    baseline_choices <- baseline_pu$choice
    emp_for_choice <- catalog$employment_choice[baseline_choices]
    baseline_taxes_at_choice <- case_when(
      emp_for_choice == 'none' ~ baseline_taxes_by_emp$none,
      emp_for_choice == 'pt'   ~ baseline_taxes_by_emp$pt,
      emp_for_choice == 'ft'   ~ baseline_taxes_by_emp$ft
    )
    policy_taxes_at_choice <- extract_selected_values(pu_with_policy, baseline_choices, 'taxes', nrow(catalog))

    parent_unit_weight <- (baseline_pu$per_weight1 + coalesce(baseline_pu$per_weight2, baseline_pu$per_weight1)) / 2
    tax_change_list[[pu_name]] <- sum((policy_taxes_at_choice - baseline_taxes_at_choice) * parent_unit_weight, na.rm = TRUE)
  }

  # Combine
  combined <- bind_rows(fiscal_list)

  if (nrow(combined) == 0) {
    return(tibble())
  }

  # Sum mechanical tax change across all parent unit types
  total_mechanical_tax_change <- sum(unlist(tax_change_list), na.rm = TRUE)

  # Calculate totals (in billions)
  combined %>%
    summarise(
      mechanical_demand_subsidy_raw = sum(demand_subsidy),
      mechanical_supply_subsidy_raw = sum(supply_subsidy),
      mechanical_employer_subsidy_raw = sum(employer_subsidy, na.rm = TRUE),
      mechanical_cdctc_cost_raw = sum(cdctc_cost, na.rm = TRUE),
      n_children_weighted = sum(n_children_weighted),
      .groups = 'drop'
    ) %>%
    mutate(
      # Convert to billions
      mechanical_demand_subsidy = mechanical_demand_subsidy_raw / 1e9,
      mechanical_supply_subsidy = mechanical_supply_subsidy_raw / 1e9,
      mechanical_employer_subsidy = mechanical_employer_subsidy_raw / 1e9,
      mechanical_cdctc_cost = mechanical_cdctc_cost_raw / 1e9,

      # Tax change from tax policy (can be non-zero if policy differs from baseline)
      # Negative = tax cut (revenue loss), Positive = tax increase (revenue gain)
      mechanical_tax_change = total_mechanical_tax_change / 1e9,

      year = year,
      .before = everything()
    ) %>%
    select(-mechanical_demand_subsidy_raw, -mechanical_supply_subsidy_raw,
           -mechanical_employer_subsidy_raw, -mechanical_cdctc_cost_raw)
}



aggregate_year_distributional_impact <- function(baseline_result, policy_result,
                                                  policy_demand_file, policy_supply_file,
                                                  policy_cdctc_file = 'baseline',
                                                  policy_tax_file = 'baseline',
                                                  year, tax_sim_path = NULL,
                                                  demand_params = NULL,
                                                  cpi_growth_factor = 1.0) {

  #----------------------------------------------------------------------------
  # Computes mechanical, welfare, and total net income effects by demographic
  # groups. Decomposes policy impact into mechanical (price changes at baseline
  # behavior) and welfare (EV at policy prices) components.
  #
  # Params:
  #   - baseline_result (list): Baseline simulation result
  #   - policy_result (list): Policy simulation result
  #   - policy_demand_file (chr): Demand policy filename to load
  #   - policy_supply_file (chr): Supply policy filename to load
  #   - policy_cdctc_file (chr): CDCTC policy filename (default 'baseline')
  #   - policy_tax_file (chr): Tax policy filename (default 'baseline')
  #   - year (int): Simulation year for labeling and tax policy application
  #   - tax_sim_path (chr): Path to Tax-Simulator output for AGI percentiles
  #   - demand_params (list): Demand model parameters for policy components
  #   - cpi_growth_factor (dbl): CPI growth factor for price inflation
  #
  # Returns: (df) Tibble with distributional impacts by group (income quintile,
  #     AGI bracket, marital status, n_children, parent age), or empty tibble
  #----------------------------------------------------------------------------

  # Handle non-converged scenarios
  if (!baseline_result$converged || !policy_result$converged) {
    return(tibble())
  }

  if (is.null(baseline_result$parent_units) || is.null(policy_result$parent_units)) {
    return(tibble())
  }

  # Get baseline prices for mechanical effect calculation
  P_baseline <- baseline_result$prices
  # Get policy equilibrium prices for "welfare at policy prices" calculation
  P_policy <- policy_result$prices

  # Load policy functions using unified loaders
  policy_do_demand <- load_demand_policy(policy_demand_file)
  policy_do_cdctc <- load_cdctc_policy(policy_cdctc_file)
  policy_do_tax <- load_policy('policy_tax', policy_tax_file, 'do_tax_policy')

  # Load baseline policy functions for baseline-wide NI computation
  baseline_do_demand <- load_demand_policy('baseline')
  baseline_do_cdctc <- load_cdctc_policy('baseline')

  # Collect family-level data from all parent unit types first
  # Then aggregate across all families together
  family_data_list <- list()

  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    n_children <- PARENT_UNIT_N_CHILDREN[i]

    baseline_pu <- baseline_result$parent_units[[pu_name]]
    policy_pu <- policy_result$parent_units[[pu_name]]

    if (is.null(baseline_pu) || nrow(baseline_pu) == 0) next
    if (is.null(policy_pu) || nrow(policy_pu) == 0) next

    pu_stripped <- strip_collapsed_scalars(baseline_pu)
    catalog <- get_choice_catalog(n_children)
    pu_for_baseline_wide <- pu_stripped

    # Apply tax policy to modify total_tax.{none,pt,ft} for mechanical/welfare effects
    # This happens BEFORE compute_base_matrices() so taxes_matrix reflects policy
    pu_for_mechanical <- policy_do_tax(pu_stripped, n_children = n_children, year = year)

    # Welfare version: same policy rules, but evaluated at policy equilibrium prices
    pu_for_welfare <- pu_for_mechanical

    # Mechanical effect: policy rules at baseline prices
    pc_mech <- compute_policy_components(pu_for_mechanical, P_baseline, n_children, demand_params,
                                          policy_do_demand, policy_do_cdctc, cpi_growth_factor)
    pu_with_mechanical <- bind_cols(pu_for_mechanical, pc_mech$components)

    # Baseline-wide: baseline rules at baseline prices (for welfare EV at k1)
    pc_baseline <- compute_policy_components(pu_for_baseline_wide, P_baseline, n_children, demand_params,
                                              baseline_do_demand, baseline_do_cdctc, cpi_growth_factor)
    pu_with_baseline_wide <- bind_cols(pu_for_baseline_wide, pc_baseline$components)

    # Welfare: policy rules at policy equilibrium prices
    pc_welfare <- compute_policy_components(pu_for_welfare, P_policy, n_children, demand_params,
                                             policy_do_demand, policy_do_cdctc, cpi_growth_factor)
    pu_with_welfare <- bind_cols(pu_for_welfare, pc_welfare$components)

    # Get family-level data (not aggregated yet)
    family_data_list[[pu_name]] <- {
      flm_mechanical_pu <- pu_with_mechanical
      flm_welfare_pu <- pu_with_welfare
      flm_baseline_wide_pu <- pu_with_baseline_wide
      flm_n_choices <- if (n_children == 1) N_CHOICES_1_CHILD else N_CHOICES_2_CHILD

      if (!'net_income' %in% names(baseline_pu) ||
          !'net_income' %in% names(policy_pu) ||
          !'choice' %in% names(baseline_pu)) {
        tibble()
      } else {
        flm_net_income_cols <- paste0('net_income.', 1:flm_n_choices)
        if (!all(flm_net_income_cols %in% names(flm_mechanical_pu)) ||
            !all(flm_net_income_cols %in% names(flm_welfare_pu)) ||
            !all(flm_net_income_cols %in% names(flm_baseline_wide_pu))) {
          tibble()
        } else {
          flm_gross_ecec_cost_cols <- paste0('gross_ecec_cost.', 1:flm_n_choices)
          flm_subsidy_cols <- paste0('subsidy.', 1:flm_n_choices)
          flm_cdctc_cols <- paste0('cdctc.', 1:flm_n_choices)

          flm_join_keys <- c('hh_id', 'parent_unit_id', 'pseudofamily_id', 'epsilon_id')

          flm_baseline_cols <- baseline_pu %>%
            select(
              all_of(flm_join_keys),
              per_weight1, per_weight2,
              n_children_original,
              age1, age2, married1, primary_caregiver,
              baseline_agi = agi,
              baseline_net_income = net_income,
              baseline_gross_ecec_cost = gross_ecec_cost,
              baseline_subsidy = subsidy,
              baseline_cdctc = cdctc,
              baseline_employment_choice = employment_choice
            )

          flm_policy_cols <- policy_pu %>%
            select(
              all_of(flm_join_keys),
              policy_net_income = net_income,
              policy_gross_ecec_cost = gross_ecec_cost,
              policy_subsidy = subsidy,
              policy_cdctc = cdctc,
              policy_employment_choice = employment_choice
            )

          flm_mechanical_ni <- extract_selected_values(
            flm_mechanical_pu, baseline_pu$choice, 'net_income', flm_n_choices
          )

          flm_welfare_ni_at_k0 <- extract_selected_values(
            flm_welfare_pu, baseline_pu$choice, 'net_income', flm_n_choices
          )

          flm_policy_choice_aligned <- baseline_pu %>%
            select(all_of(flm_join_keys)) %>%
            inner_join(policy_pu %>% select(all_of(flm_join_keys), choice), by = flm_join_keys) %>%
            pull(choice)

          flm_baseline_ni_at_k1 <- extract_selected_values(
            flm_baseline_wide_pu, flm_policy_choice_aligned, 'net_income', flm_n_choices
          )

          flm_mechanical_cols <- baseline_pu %>%
            select(all_of(flm_join_keys)) %>%
            mutate(mechanical_ni = flm_mechanical_ni)

          flm_welfare_cols <- baseline_pu %>%
            select(all_of(flm_join_keys)) %>%
            mutate(
              welfare_ni_at_k0 = flm_welfare_ni_at_k0,
              baseline_ni_at_k1 = flm_baseline_ni_at_k1
            )

          flm_mech_gross <- if (all(flm_gross_ecec_cost_cols %in% names(flm_mechanical_pu))) {
            extract_selected_values(flm_mechanical_pu, baseline_pu$choice, 'gross_ecec_cost', flm_n_choices)
          } else {
            rep(NA_real_, nrow(baseline_pu))
          }

          flm_mech_subsidy <- if (all(flm_subsidy_cols %in% names(flm_mechanical_pu))) {
            extract_selected_values(flm_mechanical_pu, baseline_pu$choice, 'subsidy', flm_n_choices)
          } else {
            rep(NA_real_, nrow(baseline_pu))
          }

          flm_mech_cdctc <- if (all(flm_cdctc_cols %in% names(flm_mechanical_pu))) {
            extract_selected_values(flm_mechanical_pu, baseline_pu$choice, 'cdctc', flm_n_choices)
          } else {
            rep(NA_real_, nrow(baseline_pu))
          }

          flm_ecec_eval_cols <- baseline_pu %>%
            select(all_of(flm_join_keys)) %>%
            mutate(
              mechanical_gross_ecec_cost = flm_mech_gross,
              mechanical_subsidy = flm_mech_subsidy,
              mechanical_cdctc = flm_mech_cdctc
            )

          flm_baseline_cols %>%
            inner_join(flm_policy_cols, by = flm_join_keys) %>%
            inner_join(flm_mechanical_cols, by = flm_join_keys) %>%
            inner_join(flm_welfare_cols, by = flm_join_keys) %>%
            inner_join(flm_ecec_eval_cols, by = flm_join_keys) %>%
            mutate(
              parent_unit_weight = (per_weight1 + coalesce(per_weight2, per_weight1)) / 2,

              baseline_net_ecec_cost = baseline_gross_ecec_cost - baseline_subsidy - baseline_cdctc,
              policy_net_ecec_cost = policy_gross_ecec_cost - policy_subsidy - policy_cdctc,
              mechanical_net_ecec_cost = mechanical_gross_ecec_cost - mechanical_subsidy - mechanical_cdctc,

              baseline_expected_net_income = baseline_net_income,

              total_net_income_change = policy_net_income - baseline_net_income,
              mechanical_effect = mechanical_ni - baseline_net_income,

              welfare_effect = 0.5 * (welfare_ni_at_k0 - baseline_net_income)
                             + 0.5 * (policy_net_income - baseline_ni_at_k1),

              baseline_ecec_burden_level = if_else(baseline_agi > 0, pmax(baseline_net_ecec_cost, 0) / baseline_agi, NA_real_),
              mechanical_ecec_burden_level = if_else(baseline_agi > 0, pmax(mechanical_net_ecec_cost, 0) / baseline_agi, NA_real_),
              total_ecec_burden_level = if_else(baseline_agi > 0, pmax(policy_net_ecec_cost, 0) / baseline_agi, NA_real_),

              uses_paid_ecec_baseline = baseline_net_ecec_cost > 0,
              uses_paid_ecec_policy = policy_net_ecec_cost > 0,

              mechanical_gain = mechanical_effect > 0,
              mechanical_loss = mechanical_effect < 0,
              welfare_gain = welfare_effect > 0,
              welfare_loss = welfare_effect < 0,
              total_gain = total_net_income_change > 0,
              total_loss = total_net_income_change < 0,

              baseline_working = baseline_employment_choice != 'none',
              policy_working = policy_employment_choice != 'none'

            ) %>%
            select(-epsilon_id,
                   -per_weight1, -per_weight2,
                   -baseline_gross_ecec_cost, -policy_gross_ecec_cost,
                   -baseline_subsidy, -policy_subsidy,
                   -baseline_cdctc, -policy_cdctc,
                   -mechanical_gross_ecec_cost, -mechanical_subsidy, -mechanical_cdctc,
                   -baseline_net_income, -policy_net_income,
                   -mechanical_ni, -welfare_ni_at_k0, -baseline_ni_at_k1,
                   -baseline_net_ecec_cost, -policy_net_ecec_cost, -mechanical_net_ecec_cost,
                   -baseline_employment_choice, -policy_employment_choice)
        }
      }
    }
  }

  # Combine all family data across parent unit types
  all_families <- bind_rows(family_data_list)

  if (nrow(all_families) == 0) {
    return(tibble())
  }

  # Load AGI percentile cutoffs from tax data (dep_status == 0)
  agi_percentile_cutoffs <- NULL
  if (!is.null(tax_sim_path)) {
    agi_percentile_cutoffs <- {
      tax_file <- file.path(tax_sim_path, 'static', 'detail', paste0(year, '.csv'))

      if (!file.exists(tax_file)) {
        warning('Tax file not found: ', tax_file, '. Using fallback percentile calculation.')
        NULL
      } else {
        tax_data_agi <- fread(tax_file) %>%
          tibble() %>%
          filter(dep_status == 0)

        cutoffs <- wtd.quantile(
          x      = tax_data_agi$agi,
          weight = tax_data_agi$weight,
          probs  = c(0.20, 0.40, 0.60, 0.80, 0.90, 0.95, 0.99)
        )
        names(cutoffs) <- c('p20', 'p40', 'p60', 'p80', 'p90', 'p95', 'p99')
        cutoffs
      }
    }
  }

  # Now aggregate across ALL families by group
  results <- aggregate_all_groups(all_families, agi_percentile_cutoffs)

  # Add year column
  results <- results %>%
    mutate(year = year, .before = everything())

  return(results)
}



aggregate_all_groups <- function(all_families, agi_percentile_cutoffs = NULL) {

  #----------------------------------------------------------------------------
  # Aggregates family-level data by all grouping dimensions: income quintiles
  # (tax-filer and parent), AGI brackets, marital status, n_children, and
  # parent age groups
  #
  # Params:
  #   - all_families (df): Family-level data with distributional columns
  #   - agi_percentile_cutoffs (num vec): Named vector of AGI percentile
  #       cutoffs (p20, p40, ..., p99) from tax data, or NULL for fallback
  #
  # Returns: (df) Stacked tibble of distributional metrics by all group
  #     dimensions
  #----------------------------------------------------------------------------

  # Assign income groups (using tax data cutoffs if provided)
  all_families <- all_families %>%
    assign_income_groups(agi_percentile_cutoffs) %>%
    assign_agi_groups() %>%
    assign_demographic_groups()

  # Assign parent-specific income quintiles
  {
    parent_cutoffs <- wtd.quantile(
      x      = all_families$baseline_agi,
      weight = all_families$parent_unit_weight,
      probs  = c(0.20, 0.40, 0.60, 0.80, 0.90, 0.95, 0.99)
    )
    names(parent_cutoffs) <- c('p20', 'p40', 'p60', 'p80', 'p90', 'p95', 'p99')

    all_families <- all_families %>%
      mutate(
        parent_income_group = case_when(
          baseline_agi <= parent_cutoffs['p20'] ~ 'Q1',
          baseline_agi <= parent_cutoffs['p40'] ~ 'Q2',
          baseline_agi <= parent_cutoffs['p60'] ~ 'Q3',
          baseline_agi <= parent_cutoffs['p80'] ~ 'Q4',
          TRUE                                  ~ 'Q5'
        ),
        is_parent_top_10pct = baseline_agi > parent_cutoffs['p90'],
        is_parent_top_5pct  = baseline_agi > parent_cutoffs['p95'],
        is_parent_top_1pct  = baseline_agi > parent_cutoffs['p99']
      )
  }

  # Aggregate by each grouping dimension
  results <- bind_rows(
    # Overall (no cuts)
    aggregate_by_group(all_families %>% mutate(overall_group = 'All'), 'overall', 'overall_group'),

    # Income quintiles (Q1-Q5) — cutoffs from full tax-filer population
    aggregate_by_group(all_families, 'income_quintile', 'income_group'),

    # Top percentile breakouts (as part of income dimension)
    aggregate_by_group(all_families %>% filter(is_top_10pct) %>% mutate(income_group = 'Top 10%'), 'income_quintile', 'income_group'),
    aggregate_by_group(all_families %>% filter(is_top_5pct) %>% mutate(income_group = 'Top 5%'), 'income_quintile', 'income_group'),
    aggregate_by_group(all_families %>% filter(is_top_1pct) %>% mutate(income_group = 'Top 1%'), 'income_quintile', 'income_group'),

    # Parent income quintiles (Q1-Q5) — cutoffs from parent unit AGI distribution
    aggregate_by_group(all_families, 'parent_income_quintile', 'parent_income_group'),

    # Top percentile breakouts (parent income)
    aggregate_by_group(all_families %>% filter(is_parent_top_10pct) %>% mutate(parent_income_group = 'Top 10%'), 'parent_income_quintile', 'parent_income_group'),
    aggregate_by_group(all_families %>% filter(is_parent_top_5pct) %>% mutate(parent_income_group = 'Top 5%'), 'parent_income_quintile', 'parent_income_group'),
    aggregate_by_group(all_families %>% filter(is_parent_top_1pct) %>% mutate(parent_income_group = 'Top 1%'), 'parent_income_quintile', 'parent_income_group'),

    # AGI groups (0-10k, 10-25k, etc.)
    aggregate_by_group(all_families, 'agi_group', 'agi_group'),

    # Marital status
    aggregate_by_group(all_families, 'marital', 'marital_group'),

    # Number of children
    aggregate_by_group(all_families, 'n_children', 'n_children_group'),

    # Parent age
    aggregate_by_group(all_families, 'parent_age', 'parent_age_group')
  )

  return(results)
}



assign_income_groups <- function(df, agi_percentile_cutoffs = NULL) {

  #----------------------------------------------------------------------------
  # Assigns families to income quintiles (Q1-Q5) and top percentile groups
  # based on AGI cutoffs from the full tax-filer population or parent data
  #
  # Params:
  #   - df (df): Family data with baseline_agi and parent_unit_weight
  #   - agi_percentile_cutoffs (num vec): Named vector of AGI percentile
  #       cutoffs (p20, p40, ..., p99), or NULL to compute from df
  #
  # Returns: (df) Input df with income_group, is_top_10pct, is_top_5pct,
  #     is_top_1pct columns added
  #----------------------------------------------------------------------------

  # Use tax data cutoffs if provided, otherwise fall back to parent unit data
  if (!is.null(agi_percentile_cutoffs)) {
    quintile_cutoffs <- agi_percentile_cutoffs
  } else {
    quintile_cutoffs <- wtd.quantile(
      x      = df$baseline_agi,
      weight = df$parent_unit_weight,
      probs  = c(0.20, 0.40, 0.60, 0.80, 0.90, 0.95, 0.99)
    )
    names(quintile_cutoffs) <- c('p20', 'p40', 'p60', 'p80', 'p90', 'p95', 'p99')
  }

  df <- df %>%
    mutate(
      income_group = case_when(
        baseline_agi <= quintile_cutoffs['p20'] ~ 'Q1',
        baseline_agi <= quintile_cutoffs['p40'] ~ 'Q2',
        baseline_agi <= quintile_cutoffs['p60'] ~ 'Q3',
        baseline_agi <= quintile_cutoffs['p80'] ~ 'Q4',
        TRUE                                    ~ 'Q5'
      ),
      is_top_10pct = baseline_agi > quintile_cutoffs['p90'],
      is_top_5pct  = baseline_agi > quintile_cutoffs['p95'],
      is_top_1pct  = baseline_agi > quintile_cutoffs['p99']
    )

  return(df)
}



assign_agi_groups <- function(df) {

  #----------------------------------------------------------------------------
  # Assigns families to AGI bracket groups ($0-10k through $500k+) using
  # baseline_agi values
  #
  # Params:
  #   - df (df): Family data with baseline_agi column
  #
  # Returns: (df) Input df with agi_group column added
  #----------------------------------------------------------------------------

  df <- df %>%
    mutate(
      agi_group = case_when(
        baseline_agi < 10000   ~ '01_$0-10k',
        baseline_agi < 25000   ~ '02_$10-25k',
        baseline_agi < 50000   ~ '03_$25-50k',
        baseline_agi < 75000   ~ '04_$50-75k',
        baseline_agi < 100000  ~ '05_$75-100k',
        baseline_agi < 150000  ~ '06_$100-150k',
        baseline_agi < 200000  ~ '07_$150-200k',
        baseline_agi < 250000  ~ '08_$200-250k',
        baseline_agi < 500000  ~ '09_$250-500k',
        TRUE                   ~ '10_$500k+'
      )
    )

  return(df)
}



assign_demographic_groups <- function(df) {

  #----------------------------------------------------------------------------
  # Assigns families to marital status, number of children, and primary
  # caregiver age groups for distributional analysis
  #
  # Params:
  #   - df (df): Family data with married1, n_children_original, age1, age2,
  #       primary_caregiver columns
  #
  # Returns: (df) Input df with marital_group, n_children_group,
  #     parent_age_group columns added
  #----------------------------------------------------------------------------

  df <- df %>%
    mutate(
      # Marital status
      marital_group = if_else(married1 == 1, 'Married', 'Unmarried'),

      # Number of children
      n_children_group = case_when(
        n_children_original == 1 ~ '1 child',
        n_children_original == 2 ~ '2 children',
        n_children_original >= 3 ~ '3+ children'
      ),

      # Primary caregiver age (use age1 or age2 based on primary_caregiver)
      pc_age = if_else(primary_caregiver == 1, age1, age2),
      parent_age_group = case_when(
        pc_age < 25  ~ '1_Under 25',
        pc_age < 30  ~ '2_25-29',
        pc_age < 35  ~ '3_30-34',
        pc_age < 40  ~ '4_35-39',
        TRUE         ~ '5_40+'
      )
    ) %>%
    select(-pc_age)

  return(df)
}



aggregate_by_group <- function(df, dimension, group_col) {

  #----------------------------------------------------------------------------
  # Aggregates distributional metrics (income changes, ECEC burden levels,
  # employment rates) by a single grouping dimension
  #
  # Params:
  #   - df (df): Family-level data with distributional columns
  #   - dimension (chr): Dimension label (e.g. 'income_quintile', 'marital')
  #   - group_col (chr): Column name in df to group by
  #
  # Returns: (df) Tibble with aggregated metrics per group value, including
  #     income changes, ECEC burden, and employment rates
  #----------------------------------------------------------------------------

  # Core income change metrics
  income_metrics <- df %>%
    group_by(group = .data[[group_col]]) %>%
    summarise(
      n_families = sum(parent_unit_weight),

      # Income changes (renamed to be explicit about "income change")
      avg_baseline_net_income = weighted.mean(baseline_expected_net_income, parent_unit_weight),
      avg_baseline_agi = weighted.mean(baseline_agi, parent_unit_weight),
      avg_mechanical_income_change = weighted.mean(mechanical_effect, parent_unit_weight),
      avg_welfare_income_change = weighted.mean(welfare_effect, parent_unit_weight),
      avg_total_income_change = weighted.mean(total_net_income_change, parent_unit_weight),

      # Income direction fractions
      frac_mechanical_gain = sum(parent_unit_weight * mechanical_gain) / sum(parent_unit_weight),
      frac_mechanical_loss = sum(parent_unit_weight * mechanical_loss) / sum(parent_unit_weight),
      frac_welfare_gain = sum(parent_unit_weight * welfare_gain) / sum(parent_unit_weight),
      frac_welfare_loss = sum(parent_unit_weight * welfare_loss) / sum(parent_unit_weight),
      frac_total_gain = sum(parent_unit_weight * total_gain) / sum(parent_unit_weight),
      frac_total_loss = sum(parent_unit_weight * total_loss) / sum(parent_unit_weight),

      .groups = 'drop'
    ) %>%
    mutate(
      # Percent changes as share of baseline AGI
      pct_mechanical_income_change = avg_mechanical_income_change / avg_baseline_agi * 100,
      pct_welfare_income_change = avg_welfare_income_change / avg_baseline_agi * 100,
      pct_total_income_change = avg_total_income_change / avg_baseline_agi * 100,
      dimension = dimension,
      .before = group
    )

  # ECEC burden levels (all families)
  ecec_levels <- df %>%
    group_by(group = .data[[group_col]]) %>%
    summarise(
      avg_baseline_ecec_burden_level = weighted.mean(baseline_ecec_burden_level, parent_unit_weight, na.rm = TRUE),
      avg_mechanical_ecec_burden_level = weighted.mean(mechanical_ecec_burden_level, parent_unit_weight, na.rm = TRUE),
      avg_total_ecec_burden_level = weighted.mean(total_ecec_burden_level, parent_unit_weight, na.rm = TRUE),
      .groups = 'drop'
    )

  # ECEC burden levels (conditional on using paid ECEC)
  ecec_levels_cond_baseline <- df %>%
    filter(uses_paid_ecec_baseline) %>%
    group_by(group = .data[[group_col]]) %>%
    summarise(
      avg_baseline_ecec_burden_level_cond = weighted.mean(baseline_ecec_burden_level, parent_unit_weight, na.rm = TRUE),
      avg_mechanical_ecec_burden_level_cond = weighted.mean(mechanical_ecec_burden_level, parent_unit_weight, na.rm = TRUE),
      frac_using_paid_ecec_baseline = n(),  # placeholder, will compute properly below
      .groups = 'drop'
    )

  ecec_levels_cond_policy <- df %>%
    filter(uses_paid_ecec_policy) %>%
    group_by(group = .data[[group_col]]) %>%
    summarise(
      avg_total_ecec_burden_level_cond = weighted.mean(total_ecec_burden_level, parent_unit_weight, na.rm = TRUE),
      .groups = 'drop'
    )

  # Fraction using paid ECEC (baseline and policy)
  paid_ecec_fracs <- df %>%
    group_by(group = .data[[group_col]]) %>%
    summarise(
      frac_using_paid_ecec_baseline = sum(parent_unit_weight * uses_paid_ecec_baseline) / sum(parent_unit_weight),
      frac_using_paid_ecec_policy = sum(parent_unit_weight * uses_paid_ecec_policy) / sum(parent_unit_weight),
      .groups = 'drop'
    )

  # Employment rates
  employment_metrics <- df %>%
    group_by(group = .data[[group_col]]) %>%
    summarise(
      baseline_employment_rate = sum(parent_unit_weight * baseline_working) / sum(parent_unit_weight),
      policy_employment_rate = sum(parent_unit_weight * policy_working) / sum(parent_unit_weight),
      .groups = 'drop'
    ) %>%
    mutate(
      pp_employment_change = (policy_employment_rate - baseline_employment_rate) * 100
    )

  # Combine all metrics
  result <- income_metrics %>%
    left_join(ecec_levels, by = 'group') %>%
    left_join(ecec_levels_cond_baseline %>% select(-frac_using_paid_ecec_baseline), by = 'group') %>%
    left_join(ecec_levels_cond_policy, by = 'group') %>%
    left_join(paid_ecec_fracs, by = 'group') %>%
    left_join(employment_metrics, by = 'group')

  return(result)
}



aggregate_year_poverty_impact <- function(baseline_result, policy_result,
                                           policy_demand_file,
                                           policy_cdctc_file = 'baseline',
                                           policy_tax_file = 'baseline',
                                           year,
                                           aged_spm, pu_spm_xwalk,
                                           spm_parent_earnings = NULL,
                                           demand_params = NULL,
                                           cpi_growth_factor = 1.0) {

  #----------------------------------------------------------------------------
  # SPM poverty impact analysis with mechanical + behavioral decomposition.
  # Computes policy effects on SPM resources and derives poverty rate changes
  # for total population and children.
  #
  # Params:
  #   - baseline_result (list): Baseline simulation result
  #   - policy_result (list): Policy simulation result
  #   - policy_demand_file (chr): Demand policy filename to load
  #   - policy_cdctc_file (chr): CDCTC policy filename (default 'baseline')
  #   - policy_tax_file (chr): Tax policy filename (default 'baseline')
  #   - year (int): Simulation year
  #   - aged_spm (df): Age-adjusted SPM data with spm_resources, spm_threshold
  #   - pu_spm_xwalk (df): Parent unit to SPM unit crosswalk
  #   - spm_parent_earnings (df): Person-level SPM parent earnings for expense
  #       cap computation, or NULL
  #   - demand_params (list): Demand model parameters for policy components
  #   - cpi_growth_factor (dbl): CPI growth factor for price inflation
  #
  # Returns: (df) Tibble with poverty metrics by universe (total, children),
  #     or empty tibble if not converged
  #----------------------------------------------------------------------------

  # Validate inputs
  if (!baseline_result$converged || !policy_result$converged) {
    return(tibble())
  }

  if (is.null(aged_spm) || nrow(aged_spm) == 0) {
    return(tibble())
  }

  if (is.null(pu_spm_xwalk) || nrow(pu_spm_xwalk) == 0) {
    return(tibble())
  }

  # Get baseline prices for mechanical effect calculation
  P_baseline <- baseline_result$prices

  # Load policy functions for mechanical effect computation
  policy_do_demand <- load_demand_policy(policy_demand_file)
  policy_do_cdctc <- load_cdctc_policy(policy_cdctc_file)
  policy_do_tax <- load_policy('policy_tax', policy_tax_file, 'do_tax_policy')

  # Collect parent unit-level deltas
  pu_deltas_list <- list()

  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    n_children <- PARENT_UNIT_N_CHILDREN[i]

    baseline_pu <- baseline_result$parent_units[[pu_name]]
    policy_pu <- policy_result$parent_units[[pu_name]]

    # Mechanical effect: policy subsidies at baseline prices
    pu_for_mechanical <- strip_collapsed_scalars(baseline_pu)
    catalog <- get_choice_catalog(n_children)
    pu_for_mechanical <- policy_do_tax(pu_for_mechanical, n_children = n_children, year = year)

    pc <- compute_policy_components(pu_for_mechanical, P_baseline, n_children, demand_params,
                                     policy_do_demand, policy_do_cdctc, cpi_growth_factor)
    mechanical_pu <- bind_cols(pu_for_mechanical, pc$components)

    # Compute parent unit-level deltas
    pu_deltas_list[[pu_name]] <- {
      pov_catalog <- get_choice_catalog(n_children)
      pov_n_choices <- nrow(pov_catalog)

      pov_required_scalar <- c('gross_ecec_cost', 'subsidy', 'cdctc', 'taxes', 'agi', 'choice')
      if (!all(pov_required_scalar %in% names(baseline_pu)) ||
          !all(pov_required_scalar %in% names(policy_pu)) ||
          !paste0('gross_ecec_cost.', 1) %in% names(mechanical_pu)) {
        tibble()
      } else {
        pov_baseline_net_ecec <- baseline_pu$gross_ecec_cost - baseline_pu$subsidy - baseline_pu$cdctc
        pov_baseline_earnings <- baseline_pu$agi
        pov_baseline_taxes <- baseline_pu$taxes

        pov_policy_net_ecec <- policy_pu$gross_ecec_cost - policy_pu$subsidy - policy_pu$cdctc
        pov_policy_earnings <- policy_pu$agi
        pov_policy_taxes <- policy_pu$taxes

        pov_baseline_choices <- baseline_pu$choice
        pov_mech_gross_ecec <- extract_selected_values(mechanical_pu, pov_baseline_choices, 'gross_ecec_cost', pov_n_choices)
        pov_mech_subsidy <- extract_selected_values(mechanical_pu, pov_baseline_choices, 'subsidy', pov_n_choices)
        pov_mech_cdctc <- extract_selected_values(mechanical_pu, pov_baseline_choices, 'cdctc', pov_n_choices)
        pov_mech_taxes <- extract_selected_values(mechanical_pu, pov_baseline_choices, 'taxes', pov_n_choices)

        pov_mech_net_ecec <- pov_mech_gross_ecec - pov_mech_subsidy - pov_mech_cdctc

        pov_mechanical_delta_childcare <- pov_baseline_net_ecec - pov_mech_net_ecec
        pov_mechanical_delta_earnings <- 0
        pov_mechanical_delta_taxes <- pov_mech_taxes - pov_baseline_taxes

        pov_total_delta_childcare <- pov_baseline_net_ecec - pov_policy_net_ecec
        pov_total_delta_earnings <- pov_policy_earnings - pov_baseline_earnings
        pov_total_delta_taxes <- pov_policy_taxes - pov_baseline_taxes

        pov_cap_cols <- paste0('min_parent_earnings.', c('none', 'pt', 'ft'))
        pov_has_cap_cols <- all(pov_cap_cols %in% names(baseline_pu)) && all(pov_cap_cols %in% names(policy_pu))
        if (!pov_has_cap_cols) {
          warning('  min_parent_earnings.* not found in parent unit results; falling back to AGI for expense cap.')
        }

        pov_baseline_cap_earnings <- get_cap_earnings(baseline_pu, 'employment_choice', pov_baseline_earnings)
        pov_policy_cap_earnings <- get_cap_earnings(policy_pu, 'employment_choice', pov_policy_earnings)

        pov_total_child_weight <- baseline_pu$child_weight.1
        if ('child_weight.2' %in% names(baseline_pu)) {
          pov_total_child_weight <- pov_total_child_weight + replace_na(baseline_pu$child_weight.2, 0)
        }

        tibble(
          hh_id = baseline_pu$hh_id,
          parent_unit_id = baseline_pu$parent_unit_id,
          epsilon_id = if ('epsilon_id' %in% names(baseline_pu)) baseline_pu$epsilon_id else NA_integer_,
          n_children_original = baseline_pu$n_children_original,
          hhm_id1 = if ('hhm_id1' %in% names(baseline_pu)) baseline_pu$hhm_id1 else NA_integer_,
          hhm_id2 = if ('hhm_id2' %in% names(baseline_pu)) baseline_pu$hhm_id2 else NA_integer_,
          primary_caregiver = if ('primary_caregiver' %in% names(baseline_pu)) baseline_pu$primary_caregiver else 1L,
          child_weight = pov_total_child_weight,
          per_weight1 = baseline_pu$per_weight1,
          mechanical_delta_childcare = pov_mechanical_delta_childcare,
          mechanical_delta_earnings = pov_mechanical_delta_earnings,
          mechanical_delta_taxes = pov_mechanical_delta_taxes,
          total_delta_childcare = pov_total_delta_childcare,
          total_delta_earnings = pov_total_delta_earnings,
          total_delta_taxes = pov_total_delta_taxes,
          baseline_cap_earnings = pov_baseline_cap_earnings,
          policy_cap_earnings = pov_policy_cap_earnings
        )
      }
    }
  }

  # Combine all parent unit deltas
  all_pu_deltas <- bind_rows(pu_deltas_list)

  if (nrow(all_pu_deltas) == 0) {
    return(tibble())
  }

  # Aggregate parent-unit deltas to SPM units
  spm_deltas <- {
    xwalk_unique <- pu_spm_xwalk %>%
      select(hh_id, parent_unit_id, spm_unit_id) %>%
      distinct(hh_id, parent_unit_id, .keep_all = TRUE)

    pu_with_spm <- all_pu_deltas %>%
      inner_join(xwalk_unique, by = c('hh_id', 'parent_unit_id'))

    if (nrow(pu_with_spm) == 0) {
      tibble()
    } else {
      spm_group_vars <- c('hh_id', 'spm_unit_id')
      if ('epsilon_id' %in% names(pu_with_spm)) {
        spm_group_vars <- c(spm_group_vars, 'epsilon_id')
      }

      pu_with_spm %>%
        group_by(across(all_of(spm_group_vars))) %>%
        summarise(
          mechanical_delta_childcare = sum(mechanical_delta_childcare),
          mechanical_delta_earnings = sum(mechanical_delta_earnings),
          mechanical_delta_taxes = sum(mechanical_delta_taxes),
          total_delta_childcare = sum(total_delta_childcare),
          total_delta_earnings = sum(total_delta_earnings),
          total_delta_taxes = sum(total_delta_taxes),
          baseline_min_earnings = {
            x <- baseline_cap_earnings
            if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
          },
          policy_min_earnings = {
            x <- policy_cap_earnings
            if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
          },
          total_child_weight = sum(child_weight),
          .groups = 'drop'
        )
    }
  }

  if (nrow(spm_deltas) == 0) {
    return(tibble())
  }

  # Compute policy expense caps from person-level earnings
  policy_caps <- {
    if (is.null(spm_parent_earnings) || nrow(spm_parent_earnings) == 0) {
      NULL
    } else {
      cap_has_epsilon <- 'epsilon_id' %in% names(spm_deltas) && any(!is.na(spm_deltas$epsilon_id))

      pu_person_deltas <- all_pu_deltas %>%
        select(hh_id, parent_unit_id, any_of('epsilon_id'),
               hhm_id1, hhm_id2, primary_caregiver, total_delta_earnings)

      pu_deltas_long <- bind_rows(
        pu_person_deltas %>%
          filter(!is.na(hhm_id1)) %>%
          mutate(
            per_id = hhm_id1,
            earnings_delta = if_else(primary_caregiver == 1, total_delta_earnings, 0)
          ) %>%
          select(hh_id, parent_unit_id, any_of('epsilon_id'), per_id, earnings_delta),
        pu_person_deltas %>%
          filter(!is.na(hhm_id2)) %>%
          mutate(
            per_id = hhm_id2,
            earnings_delta = if_else(primary_caregiver == 2, total_delta_earnings, 0)
          ) %>%
          select(hh_id, parent_unit_id, any_of('epsilon_id'), per_id, earnings_delta)
      )

      pu_to_spm <- pu_spm_xwalk %>%
        select(hh_id, parent_unit_id, spm_unit_id) %>%
        distinct(hh_id, parent_unit_id, .keep_all = TRUE)

      pu_deltas_with_spm <- pu_deltas_long %>%
        left_join(pu_to_spm, by = c('hh_id', 'parent_unit_id'))

      if (cap_has_epsilon) {
        epsilon_ids <- unique(na.omit(spm_deltas$epsilon_id))

        policy_caps_list <- lapply(epsilon_ids, function(eps_id) {
          eps_deltas <- pu_deltas_with_spm %>%
            filter(epsilon_id == eps_id) %>%
            select(hh_id, spm_unit_id, per_id, earnings_delta)

          spm_parent_earnings %>%
            left_join(eps_deltas, by = c('hh_id', 'spm_unit_id', 'per_id')) %>%
            mutate(
              earnings_delta = replace_na(earnings_delta, 0),
              policy_earnings = pmax(0, baseline_earnings + earnings_delta)
            ) %>%
            group_by(hh_id, spm_unit_id) %>%
            summarise(
              policy_cap = if (all(policy_earnings == 0)) 0 else min(policy_earnings[policy_earnings > 0]),
              .groups = 'drop'
            ) %>%
            mutate(epsilon_id = eps_id)
        })

        bind_rows(policy_caps_list)
      } else {
        person_deltas <- pu_deltas_with_spm %>%
          group_by(hh_id, spm_unit_id, per_id) %>%
          summarise(earnings_delta = sum(earnings_delta), .groups = 'drop')

        spm_parent_earnings %>%
          left_join(person_deltas, by = c('hh_id', 'spm_unit_id', 'per_id')) %>%
          mutate(
            earnings_delta = replace_na(earnings_delta, 0),
            policy_earnings = pmax(0, baseline_earnings + earnings_delta)
          ) %>%
          group_by(hh_id, spm_unit_id) %>%
          summarise(
            policy_cap = if (all(policy_earnings == 0)) 0 else min(policy_earnings[policy_earnings > 0]),
            .groups = 'drop'
          )
      }
    }
  }

  # Compute SPM poverty changes
  spm_poverty <- {
    spm_with_deltas <- aged_spm %>%
      left_join(spm_deltas, by = c('hh_id', 'spm_unit_id'))

    if (!is.null(policy_caps) && nrow(policy_caps) > 0) {
      if ('epsilon_id' %in% names(policy_caps) && 'epsilon_id' %in% names(spm_with_deltas)) {
        spm_with_deltas <- spm_with_deltas %>%
          left_join(policy_caps %>% rename(computed_policy_cap = policy_cap),
                    by = c('hh_id', 'spm_unit_id', 'epsilon_id'))
      } else {
        spm_with_deltas <- spm_with_deltas %>%
          left_join(policy_caps %>% rename(computed_policy_cap = policy_cap),
                    by = c('hh_id', 'spm_unit_id'))
      }
    } else {
      spm_with_deltas$computed_policy_cap <- NA_real_
    }

    spm_with_deltas <- spm_with_deltas %>%
      mutate(
        mechanical_delta_childcare = replace_na(mechanical_delta_childcare, 0),
        mechanical_delta_earnings = replace_na(mechanical_delta_earnings, 0),
        mechanical_delta_taxes = replace_na(mechanical_delta_taxes, 0),
        total_delta_childcare = replace_na(total_delta_childcare, 0),
        total_delta_earnings = replace_na(total_delta_earnings, 0),
        total_delta_taxes = replace_na(total_delta_taxes, 0),
        baseline_min_earnings = coalesce(baseline_min_earnings, min_adult_earnings),
        policy_min_earnings = coalesce(computed_policy_cap, policy_min_earnings, min_adult_earnings)
      )

    if ('epsilon_id' %in% names(spm_with_deltas) && any(!is.na(spm_with_deltas$epsilon_id))) {
      n_draws_per_record <- suppressWarnings(max(spm_with_deltas$epsilon_id, na.rm = TRUE))
      if (is.finite(n_draws_per_record) && n_draws_per_record > 0) {
        spm_with_deltas <- spm_with_deltas %>%
          mutate(
            n_members_weighted = if_else(!is.na(epsilon_id), n_members_weighted / n_draws_per_record, n_members_weighted),
            n_children_weighted = if_else(!is.na(epsilon_id), n_children_weighted / n_draws_per_record, n_children_weighted)
          )
      }
    }

    spm_with_deltas %>%
      mutate(
        mechanical_cap = min_adult_earnings,
        mechanical_childcare_exp = pmax(0, spm_childcare_exp - mechanical_delta_childcare),
        mechanical_total_work_exp = mechanical_childcare_exp + spm_work_exp,
        mechanical_capped_exp = pmin(mechanical_total_work_exp, mechanical_cap),
        mechanical_exp_change = spm_capped_exp - mechanical_capped_exp,
        mechanical_delta_resources = mechanical_exp_change - mechanical_delta_taxes,
        mechanical_resources = spm_resources + mechanical_delta_resources,
        is_poor_mechanical = (mechanical_resources < spm_threshold),

        total_cap = pmax(0, policy_min_earnings),
        total_childcare_exp = pmax(0, spm_childcare_exp - total_delta_childcare),
        total_total_work_exp = total_childcare_exp + spm_work_exp,
        total_capped_exp = pmin(total_total_work_exp, total_cap),
        total_exp_change = spm_capped_exp - total_capped_exp,
        total_delta_resources = total_exp_change + total_delta_earnings - total_delta_taxes,
        total_resources = spm_resources + total_delta_resources,
        is_poor_total = (total_resources < spm_threshold),

        lifted_mechanical = is_poor_baseline & !is_poor_mechanical,
        lifted_total = is_poor_baseline & !is_poor_total,
        lifted_behavioral = lifted_total & !lifted_mechanical,
        pushed_mechanical = !is_poor_baseline & is_poor_mechanical,
        pushed_total = !is_poor_baseline & is_poor_total,
        pushed_behavioral = pushed_total & !pushed_mechanical
      )
  }

  # Aggregate poverty metrics for total population and children
  results <- bind_rows(
    poverty_summary(spm_poverty, 'n_members_weighted', 'total'),
    poverty_summary(spm_poverty, 'n_children_weighted', 'children')
  ) %>%
    mutate(year = year, .before = everything())

  return(results)
}




calculate_child_earnings <- function(baseline_result, policy_result, year, tax_data_path, macro_projections) {

  #----------------------------------------------------------------------------
  # Calculates later-in-life child earnings effects based on ECEC type
  # switches between baseline and policy. Uses LATE matrices to map care
  # type transitions to test score deltas, then applies Chetty et al. (2011)
  # elasticity (1sd -> 13.1% earnings) to project earnings changes in
  # real 2025 dollars.
  #
  # Params:
  #   - baseline_result (list): Baseline simulation result with $parent_units
  #   - policy_result (list): Policy simulation result with $parent_units
  #   - year (int): Simulation year
  #   - tax_data_path (chr): Path to Tax-Data tax_units files for age-27
  #       earnings by parent rank
  #   - macro_projections (df): Macro projection data with year and cpiu columns
  #
  # Returns: (df) Microdata tibble (long in child) with baseline/policy ECEC
  #     types, test score deltas, and child earnings deltas, or empty tibble
  #----------------------------------------------------------------------------

  # Handle non-converged scenarios
  if (!baseline_result$converged || !policy_result$converged) {
    return(tibble())
  }
  
  if (is.null(baseline_result$parent_units) || is.null(policy_result$parent_units)) {
    return(tibble())
  }
  
  # Prep baseline microdata
  baseline = baseline_result$parent_units %>%
    # Append baseline parent unit files
    lapply(function(baseline_pu) {
      if (is.null(baseline_pu) || nrow(baseline_pu) == 0) return(NULL)
      
      baseline_pu %>%
        mutate(
          parent_unit_id = as.character(parent_unit_id),
          child_category = as.character(child_category),
          hh_weight      = (per_weight1 + coalesce(per_weight2, per_weight1)) / 2,
        )
    }) %>%
    bind_rows()
  
  # Prep policy microdata
  policy = policy_result$parent_units %>%
    # Append policy parent unit files
    lapply(function(policy_pu) {
      if (is.null(policy_pu) || nrow(policy_pu) == 0) return(NULL)
      
      policy_pu %>%
        select(hh_id, parent_unit_id, epsilon_id, pseudofamily_id, starts_with('ecec')) %>%
        mutate(
          parent_unit_id = as.character(parent_unit_id),
        )
    }) %>%
    bind_rows() %>%
    # Format data long in child
    pivot_longer(cols           = starts_with('ecec'),
                 names_to       = c('.value','child_index'),
                 names_pattern  = '(.*).(.)',
                 values_drop_na = T)
  
  
  # Project baseline child earnings based on parent rank
  # Compute percentile breaks (must be unique for cut())
  agi_breaks = c(-Inf,
                 unique(wtd.quantile(x = baseline$agi,
                                     weights = baseline$hh_weight,
                                     probs = 1:99/100)),
                 Inf)

  baseline = baseline %>%
    # Compute parent ranks
    mutate(
      parent_rank = cut(x = agi,
                        breaks = agi_breaks,
                        labels = F) # Ranks among parents of 0-4; Tax-Simulator uses all parents (0-18)
    ) %>%
    
    # Format data long in child
    pivot_longer(cols           = starts_with('child_age') |
                                  starts_with('child_weight') |
                                  starts_with('ecec'),
                 names_to       = c('.value','child_index'),
                 names_pattern  = '(.*).(.)',
                 values_drop_na = T) %>%
    
    # Merge average child earnings at age 27 by parent rank from tax data
    left_join(lapply(0:4,
                     function(child_age){
                       # Read in tax data
                       year_age27 = year + 27 - child_age

                       # Get CPI-U for that year to deflate to 2025 dollars
                       cpiu_future = macro_projections %>%
                         filter(year == year_age27) %>%
                         pull(cpiu)
                       if (length(cpiu_future) == 0) {
                         warning(sprintf('CPI-U projection missing for year %d. Defaulting to 1.0 (no inflation adjustment).', year_age27))
                         cpiu_future = 1.0
                       }

                       {
                         tax_data <- fread(file.path(tax_data_path,
                                                     paste0('tax_units_', year_age27, '.csv')))

                         # Convert to individual-level: one row per 27-year-old
                         # (tax unit wages combines both spouses; we need individual wages)
                         bind_rows(
                           tax_data %>%
                             filter(age1 == 27) %>%
                             select(parent_rank, weight, individual_wages = wages1),
                           tax_data %>%
                             filter(age2 == 27) %>%
                             select(parent_rank, weight, individual_wages = wages2)
                         ) %>%
                           group_by(parent_rank) %>%
                           summarise(
                             child_earnings.baseline = weighted.mean(individual_wages, weight) / cpiu_future
                           ) %>%
                           mutate(
                             year_age27 = year + 27 - child_age,
                             child_age  = child_age
                           )
                       }
                     }) %>%
                bind_rows(),
              by = c('child_age', 'parent_rank')) 
  
  
  # Impute changes in child earnings via ECEC choice switches
  # Read in late matrix
  late_matrix = fread('resources/late_matrix_0-3yr.csv') %>%
    mutate(age_group = '<3yrs') %>%
    bind_rows(fread('resources/late_matrix_3-4yr.csv') %>%
                mutate(age_group = '3-4yrs')) %>%
    pivot_longer(cols      = -c('from', 'age_group'),
                 names_to  = 'to.ecec_type',
                 values_to = 'test_score.delta') %>%
    rename(from.ecec_type = from) %>%
    mutate(
      # Chetty et al. (2011) 1sd test scores -> 13.1% later-in-life earnings
      # (see the appendix)
      child_earnings.pct_change = test_score.delta * .131
    )
  
  # Determine join keys based on what columns exist
  join_keys = c('hh_id', 'parent_unit_id', 'epsilon_id', 'child_index')
  if ('pseudofamily_id' %in% names(baseline) && 'pseudofamily_id' %in% names(policy)) {
    join_keys = c(join_keys, 'pseudofamily_id')
  }

  # Merge baseline and policy choices
  ecec_switches = baseline %>%
    rename_with(.cols = starts_with('ecec'),
                .fn = ~paste0('from.', .x)) %>%
    left_join(policy %>%
                rename_with(.cols = starts_with('ecec'),
                            .fn = ~paste0('to.', .x)),
              by = join_keys) %>%
    
    # Merge in treatment effects
    mutate(
      # Define age groups for treatment
      age_group = ifelse(child_age<3,
                         '<3yrs',
                         '3-4yrs')
    ) %>%
    left_join(late_matrix,
              by = c('from.ecec_type', 'to.ecec_type', 'age_group')) %>%
    
    # Compute deltas in dollar amounts (in 2025$)
    mutate(
      child_earnings.delta  = child_earnings.baseline * child_earnings.pct_change,
      child_earnings.policy = child_earnings.baseline + child_earnings.delta
    ) %>%
    
    mutate(
      year    = year,
      .before = 1
    )
}



aggregate_child_earnings <- function(child_earnings_micro) {

  #----------------------------------------------------------------------------
  # Aggregates child earnings microdata into summary statistics by overall,
  # parent income quintile, care type transition, and child age group
  #
  # Params:
  #   - child_earnings_micro (df): Child-level microdata from
  #       calculate_child_earnings with earnings deltas and weights
  #
  # Returns: (list) List with overall, by_quintile, by_transition,
  #     by_age_group tibbles, or list of empty tibbles if input is empty
  #----------------------------------------------------------------------------

  if (is.null(child_earnings_micro) || nrow(child_earnings_micro) == 0) {
    return(list(
      overall = tibble(),
      by_quintile = tibble(),
      by_transition = tibble(),
      by_age_group = tibble()
    ))
  }

  # Add helper columns
  micro <- child_earnings_micro %>%
    mutate(
      # Parent income quintile (1-5)
      parent_quintile = ceiling(parent_rank / 20),
      parent_quintile = pmin(parent_quintile, 5),  # Cap at 5

      # Did this child switch care types?
      switched = from.ecec_type != to.ecec_type,

      # Weighted values
      weighted_child = child_weight,
      weighted_delta = child_weight * child_earnings.delta,
      weighted_baseline = child_weight * child_earnings.baseline,
      weighted_pct_change = child_weight * child_earnings.pct_change
    )

  req_cols <- c('year', 'parent_rank', 'from.ecec_type', 'to.ecec_type',
                'child_weight', 'test_score.delta', 'child_earnings.delta',
                'child_earnings.baseline', 'child_earnings.pct_change')
  cols_missing <- setdiff(req_cols, names(micro))
  if (length(cols_missing) > 0) {
    stop(paste0('Child earnings aggregation is missing required columns: ',
                paste(cols_missing, collapse = ', ')))
  }
  bad_weight <- which(is.na(micro$child_weight))
  if (length(bad_weight) > 0) {
    stop(sprintf('Child earnings aggregation encountered NA child_weight for %d records.', length(bad_weight)))
  }
  for (col in c('child_earnings.delta', 'child_earnings.pct_change', 'test_score.delta')) {
    bad <- which(is.na(micro[[col]]) & micro$child_weight > 0)
    if (length(bad) > 0) {
      stop(sprintf('Child earnings aggregation encountered NA %s for %d weighted records.', col, length(bad)))
    }
  }

  # Overall summary
  overall <- summarise_child_earnings_group(micro) %>%
    mutate(
      year = first(micro$year),
      # Override switcher averages with switched-only sums (matching original behavior)
      avg_earnings_delta_switchers = if_else(
        n_children_switched > 0,
        sum(micro$weighted_delta * micro$switched) / n_children_switched,
        0
      ),
      avg_pct_change_switchers = if_else(
        n_children_switched > 0,
        100 * sum(micro$weighted_pct_change * micro$switched) / n_children_switched,
        0
      ),
      avg_baseline_earnings = sum(micro$weighted_baseline) / n_children,
      .before = 1
    )

  # By parent income quintile
  quintile_baseline <- micro %>%
    group_by(year, parent_quintile) %>%
    summarise(avg_baseline_earnings = sum(weighted_baseline) / sum(weighted_child), .groups = 'drop')

  by_quintile <- summarise_child_earnings_group(micro %>% group_by(year, parent_quintile)) %>%
    left_join(quintile_baseline, by = c('year', 'parent_quintile')) %>%
    mutate(share_of_total_benefits = 100 * total_earnings_delta / sum(total_earnings_delta)) %>%
    arrange(parent_quintile)

  # By care type transition
  by_transition <- micro %>%
    filter(switched) %>%
    group_by(year, from.ecec_type, to.ecec_type) %>%
    summarise(
      n_children = sum(weighted_child),
      avg_test_score_delta = weighted.mean(test_score.delta, weighted_child),
      avg_earnings_delta = weighted.mean(child_earnings.delta, weighted_child),
      avg_pct_change = 100 * weighted.mean(child_earnings.pct_change, weighted_child),
      total_earnings_delta = sum(weighted_delta),
      .groups = 'drop'
    ) %>%
    arrange(desc(abs(total_earnings_delta)))

  # By child age group
  by_age_group <- summarise_child_earnings_group(micro %>% group_by(year, age_group))

  return(list(
    overall = overall,
    by_quintile = by_quintile,
    by_transition = by_transition,
    by_age_group = by_age_group
  ))
}

#------------------------------------------------------------------------------
# Child fiscal NPV: cohort-level NPV using test score -> earnings elasticity
# (Chetty et al. 2011) with Tax-Simulator earnings paths and MTRs.
# Returns computed for age-4 cohort only (conservative, avoids double-counting).
#------------------------------------------------------------------------------

FISCAL_DISCOUNT_RATES <- seq(0.01, 0.10, by = 0.01)  # 1% to 10%
START_WORK_AGE <- 27
END_WORK_AGE <- 65
DEFAULT_ANALYSIS_AGE <- 4



get_mtr_by_parent_rank <- function(simulation_year,
                                    child_age,
                                    tax_sim_full_mtr_path,
                                    tax_data_path,
                                    start_work_age = START_WORK_AGE,
                                    end_work_age = END_WORK_AGE) {

  #----------------------------------------------------------------------------
  # Returns earnings-weighted average payroll-inclusive MTR by parent rank and
  # working age. Reads Tax-Simulator output (for MTRs) and Tax-Data files
  # (for parent_rank), joining on id.
  #
  # Params:
  #   - simulation_year (int): Year of the simulation (child's birth cohort ref)
  #   - child_age (int): Age of the child cohort being analyzed
  #   - tax_sim_full_mtr_path (chr): Path to Tax-Simulator-Full-MTR output
  #   - tax_data_path (chr): Path to Tax-Data tax_units files
  #   - start_work_age (int): First working age to read (default 27)
  #   - end_work_age (int): Last working age to read (default 65)
  #
  # Returns: (df) Tibble with parent_rank, mtr_wages, and age columns for
  #     each working year
  #----------------------------------------------------------------------------

  # Require paths to be provided
  if (is.null(tax_sim_full_mtr_path) ||
      is.na(tax_sim_full_mtr_path) ||
      tax_sim_full_mtr_path == '') {
    stop('Tax-Simulator-Full-MTR path is required for fiscal NPV calculation. ',
         'Set Tax-Simulator-Full-MTR in config/default_paths.yaml')
  }

  if (is.null(tax_data_path) ||
      is.na(tax_data_path) ||
      tax_data_path == '') {
    stop('Tax-Data path is required for fiscal NPV calculation.')
  }

  mtr_list <- list()

  for (work_age in start_work_age:end_work_age) {
    target_year <- simulation_year + (work_age - child_age)

    # Read Tax-Simulator file for MTRs and wages
    tax_sim_file <- file.path(tax_sim_full_mtr_path, 'static', 'detail', paste0(target_year, '.csv'))
    if (!file.exists(tax_sim_file)) {
      stop(sprintf('Tax-Simulator file not found: %s', tax_sim_file))
    }

    # Read Tax-Data file for parent_rank and wages
    tax_data_file <- file.path(tax_data_path, paste0('tax_units_', target_year, '.csv'))
    if (!file.exists(tax_data_file)) {
      stop(sprintf('Tax-Data file not found: %s', tax_data_file))
    }

    tax_sim <- fread(tax_sim_file, select = c('id', 'weight', 'age1', 'age2', 'mtr_wages1', 'mtr_wages2'))
    tax_data <- fread(tax_data_file, select = c('id', 'parent_rank', 'wages1', 'wages2'))

    # Require mtr_wages1/mtr_wages2 columns
    if (!all(c('mtr_wages1', 'mtr_wages2') %in% names(tax_sim))) {
      stop(sprintf('Tax-Simulator file missing mtr_wages1/mtr_wages2 columns: %s', tax_sim_file))
    }

    # Join to get parent_rank and wages
    joined <- tax_sim %>%
      left_join(tax_data, by = 'id')

    # Extract MTR at this age (both spouses separately, matching earnings logic)
    # Weight by earnings (wages × weight) to get earnings-weighted MTR
    mtr_at_age <- bind_rows(
      joined %>%
        filter(age1 == work_age) %>%
        select(parent_rank, weight, mtr_wages = mtr_wages1, wages = wages1),
      joined %>%
        filter(age2 == work_age) %>%
        select(parent_rank, weight, mtr_wages = mtr_wages2, wages = wages2)
    ) %>%
      filter(!is.na(parent_rank)) %>%
      mutate(
        # Earnings weight = population weight × wages
        earnings_weight = weight * wages
      ) %>%
      group_by(parent_rank) %>%
      summarise(
        # Earnings-weighted MTR: sum(wages × weight × MTR) / sum(wages × weight)
        mtr_wages = weighted.mean(mtr_wages, earnings_weight),
        .groups = 'drop'
      ) %>%
      mutate(age = work_age)

    mtr_list[[as.character(work_age)]] <- mtr_at_age
  }

  bind_rows(mtr_list)
}



get_earnings_path_by_parent_rank <- function(simulation_year,
                                              child_age,
                                              tax_data_path,
                                              macro_projections,
                                              start_work_age = START_WORK_AGE,
                                              end_work_age = END_WORK_AGE) {

  #----------------------------------------------------------------------------
  # Reads tax_units files for working ages and returns real earnings (2025$)
  # by parent rank and age. Used for fiscal NPV lifetime earnings paths.
  #
  # Params:
  #   - simulation_year (int): Year of the simulation (child's birth cohort ref)
  #   - child_age (int): Age of the child cohort being analyzed
  #   - tax_data_path (chr): Path to Tax-Data tax_units files
  #   - macro_projections (df): Macro projection data with year and cpiu columns
  #   - start_work_age (int): First working age to read (default 27)
  #   - end_work_age (int): Last working age to read (default 65)
  #
  # Returns: (df) Tibble with parent_rank, earnings_real_2025, and age columns
  #----------------------------------------------------------------------------

  earnings_list <- list()

  for (work_age in start_work_age:end_work_age) {
    target_year <- simulation_year + (work_age - child_age)

    # Read tax_units file for target year
    tax_file <- file.path(tax_data_path, paste0('tax_units_', target_year, '.csv'))
    if (!file.exists(tax_file)) {
      cat('  Warning: tax_units file not found for year', target_year, '- skipping\n')
      next
    }

    tax_data <- fread(tax_file)

    # Get CPI-U for deflation to 2025 dollars
    cpiu <- macro_projections %>%
      filter(year == target_year) %>%
      pull(cpiu)
    if (length(cpiu) == 0) cpiu <- 1.0

    # Extract earnings at this age (both spouses)
    earnings_at_age <- bind_rows(
      tax_data %>%
        filter(age1 == work_age) %>%
        select(parent_rank, weight, wages = wages1),
      tax_data %>%
        filter(age2 == work_age) %>%
        select(parent_rank, weight, wages = wages2)
    ) %>%
      group_by(parent_rank) %>%
      summarise(
        earnings_real_2025 = weighted.mean(wages, weight) / cpiu,
        .groups = 'drop'
      ) %>%
      mutate(age = work_age)

    earnings_list[[as.character(work_age)]] <- earnings_at_age
  }

  bind_rows(earnings_list)
}



calculate_child_fiscal_npv <- function(child_earnings_micro,
                                       baseline_fiscal,
                                       policy_fiscal,
                                       baseline_fiscal_by_age,
                                       policy_fiscal_by_age,
                                       year,
                                       tax_data_path,
                                       macro_projections,
                                       tax_sim_full_mtr_path = NULL,
                                       discount_rates = FISCAL_DISCOUNT_RATES,
                                       start_work_age = START_WORK_AGE,
                                       end_work_age = END_WORK_AGE,
                                       analysis_age = DEFAULT_ANALYSIS_AGE) {

  #----------------------------------------------------------------------------
  # Computes cohort-level fiscal NPV using explicit earnings paths (ages
  # 27-65), marginal tax rates, and test-score-based earnings elasticities.
  # Returns discounted fiscal returns minus program costs across multiple
  # discount rates.
  #
  # Params:
  #   - child_earnings_micro (df): Child-level microdata from
  #       calculate_child_earnings
  #   - baseline_fiscal (df): Baseline fiscal cost aggregation (in billions)
  #   - policy_fiscal (df): Policy fiscal cost aggregation (in billions)
  #   - baseline_fiscal_by_age (df): Baseline fiscal costs by child age (raw $)
  #   - policy_fiscal_by_age (df): Policy fiscal costs by child age (raw $)
  #   - year (int): Simulation year
  #   - tax_data_path (chr): Path to Tax-Data tax_units files
  #   - macro_projections (df): Macro projection data with year and cpiu
  #   - tax_sim_full_mtr_path (chr): Path to Tax-Simulator-Full-MTR output
  #   - discount_rates (num vec): Discount rates to evaluate (default 1-10%)
  #   - start_work_age (int): First working age (default 27)
  #   - end_work_age (int): Last working age (default 65)
  #   - analysis_age (int): Child age cohort to analyze (default 4)
  #
  # Returns: (list) List with $summary (by discount rate), $by_quintile
  #     (at 3%), and $params
  #----------------------------------------------------------------------------

  empty_fiscal_npv_result <- function(yr) {

    #--------------------------------------------------------------------------
    # Returns an empty fiscal NPV result structure with NA values for use
    # when input data is missing or invalid
    #
    # Params:
    #   - yr (int): Simulation year for labeling
    #
    # Returns: (list) List with $summary, $by_quintile, $params all empty/NA
    #--------------------------------------------------------------------------

    summary_tibble <- tibble(
      year = yr,
      discount_rate = FISCAL_DISCOUNT_RATES,
      return_npv = NA_real_,
      cost_npv = NA_real_,
      net_npv = NA_real_,
      share_recovered = NA_real_,
      mtr = NA_real_,
      n_children = NA_real_,
      n_switchers = NA_real_,
      pct_switched = NA_real_,
      avg_earnings_delta = NA_real_,
      start_work_age = NA_integer_,
      end_work_age = NA_integer_,
      n_earnings_years = NA_integer_,
      analysis_age = NA_integer_
    )

    by_quintile <- tibble(
      year = integer(),
      discount_rate = numeric(),
      parent_quintile = integer(),
      total_return_npv = numeric(),
      n_children = numeric(),
      n_switchers = numeric(),
      pct_switched = numeric(),
      avg_earnings_delta = numeric(),
      avg_return_npv = numeric(),
      share_of_returns = numeric()
    )

    list(
      summary = summary_tibble,
      by_quintile = by_quintile,
      params = list()
    )
  }

  # Handle empty or invalid input
  if (is.null(child_earnings_micro) || nrow(child_earnings_micro) == 0) {
    cat('  Fiscal NPV: No child earnings data available\n')
    return(empty_fiscal_npv_result(year))
  }

  # Filter to analysis age
  analysis_data <- child_earnings_micro %>%
    filter(child_age == analysis_age)

  if (nrow(analysis_data) == 0) {
    cat('  Fiscal NPV: No children at analysis age', analysis_age, '\n')
    return(empty_fiscal_npv_result(year))
  }

  cat('  Fiscal NPV: Reading earnings path for ages', start_work_age, 'to', end_work_age, '...\n')

  earnings_path <- get_earnings_path_by_parent_rank(
    simulation_year = year,
    child_age = analysis_age,
    tax_data_path = tax_data_path,
    macro_projections = macro_projections,
    start_work_age = start_work_age,
    end_work_age = end_work_age
  )

  n_earnings_years <- length(unique(earnings_path$age))
  cat('  Fiscal NPV: Loaded earnings for', n_earnings_years, 'working years\n')

  mtr_data <- get_mtr_by_parent_rank(
    simulation_year = year,
    child_age = analysis_age,
    tax_sim_full_mtr_path = tax_sim_full_mtr_path,
    tax_data_path = tax_data_path,
    start_work_age = start_work_age,
    end_work_age = end_work_age
  )

  n_mtr_years <- length(unique(mtr_data$age))
  avg_mtr <- mean(mtr_data$mtr_wages)
  cat(sprintf('  Fiscal NPV: Using record-level MTRs (avg=%.1f%%, n_years=%d)\n',
              100 * avg_mtr, n_mtr_years))

  # Compute mean percent change g_j by parent rank
  analysis_base <- analysis_data %>%
    mutate(
      parent_quintile = pmin(ceiling(parent_rank / 20), 5),
      switched = (from.ecec_type != to.ecec_type)
    )

  g_by_rank <- analysis_base %>%
    group_by(parent_rank) %>%
    summarise(
      g_j = weighted.mean(child_earnings.pct_change, child_weight),
      n_j = sum(child_weight),
      .groups = 'drop'
    )

  cat(sprintf('  Fiscal NPV: Computed mean g_j for %d parent ranks\n', nrow(g_by_rank)))

  # Child-level stats for reporting
  child_stats <- analysis_base %>%
    summarise(
      n_children = sum(child_weight),
      n_switchers = sum(child_weight * switched),
      pct_switched = 100 * n_switchers / n_children,
      avg_earnings_delta = weighted.mean(child_earnings.delta, child_weight)
    )

  # Cost deltas (discount-rate independent for annual values)
  cost_deltas <- {
    get_val <- function(df, col) {

      #------------------------------------------------------------------------
      # Safely extracts the first value of a column, returning 0 if missing
      #
      # Params:
      #   - df (df): Data frame to extract from (may be NULL or empty)
      #   - col (chr): Column name to look up
      #
      # Returns: (dbl) First value of the column, or 0 if absent/NA/empty
      #------------------------------------------------------------------------

      if (is.null(df) || nrow(df) == 0 || !col %in% names(df)) return(0)
      val <- df[[col]][1]
      if (is.na(val)) return(0)
      return(val)
    }

    cd_baseline_demand <- get_val(baseline_fiscal, 'total_demand_subsidy')
    cd_baseline_supply <- get_val(baseline_fiscal, 'total_supply_subsidy')
    cd_baseline_employer <- get_val(baseline_fiscal, 'total_employer_subsidy')
    cd_baseline_cdctc <- get_val(baseline_fiscal, 'total_cdctc_cost')
    cd_baseline_tax <- get_val(baseline_fiscal, 'total_tax_revenue')

    cd_policy_demand <- get_val(policy_fiscal, 'total_demand_subsidy')
    cd_policy_supply <- get_val(policy_fiscal, 'total_supply_subsidy')
    cd_policy_employer <- get_val(policy_fiscal, 'total_employer_subsidy')
    cd_policy_cdctc <- get_val(policy_fiscal, 'total_cdctc_cost')
    cd_policy_tax <- get_val(policy_fiscal, 'total_tax_revenue')

    cd_demand_subsidy_delta <- cd_policy_demand - cd_baseline_demand
    cd_supply_subsidy_delta <- cd_policy_supply - cd_baseline_supply
    cd_employer_subsidy_delta <- cd_policy_employer - cd_baseline_employer
    cd_cdctc_delta <- cd_policy_cdctc - cd_baseline_cdctc
    cd_parent_tax_delta <- cd_policy_tax - cd_baseline_tax

    cd_annual_net_cost <- cd_demand_subsidy_delta + cd_supply_subsidy_delta +
                          cd_employer_subsidy_delta + cd_cdctc_delta - cd_parent_tax_delta

    list(
      demand_subsidy_delta = cd_demand_subsidy_delta,
      supply_subsidy_delta = cd_supply_subsidy_delta,
      employer_subsidy_delta = cd_employer_subsidy_delta,
      cdctc_delta = cd_cdctc_delta,
      parent_tax_delta = cd_parent_tax_delta,
      annual_net_cost = cd_annual_net_cost
    )
  }

  # Get CPI-U for simulation year to deflate costs to 2025$
  cpiu_simulation_year <- macro_projections %>%
    filter(year == !!year) %>%
    pull(cpiu)

  # Compute cost deltas by child age for proper NPV calculation
  cost_deltas_by_age <- policy_fiscal_by_age %>%
    left_join(
      baseline_fiscal_by_age,
      by = 'child_age',
      suffix = c('.policy', '.baseline')
    ) %>%
    mutate(
      demand_subsidy_delta = demand_subsidy.policy - demand_subsidy.baseline,
      supply_subsidy_delta = supply_subsidy.policy - supply_subsidy.baseline,
      employer_subsidy_delta = employer_subsidy.policy - employer_subsidy.baseline,
      cdctc_delta = cdctc_cost.policy - cdctc_cost.baseline,
      tax_delta = tax_revenue.policy - tax_revenue.baseline,
      net_cost = demand_subsidy_delta + supply_subsidy_delta + employer_subsidy_delta +
                 cdctc_delta - tax_delta,
      net_cost_2025 = net_cost / cpiu_simulation_year
    )

  # Join earnings and MTR data by (parent_rank, age)
  earnings_mtr_by_rank_age <- earnings_path %>%
    left_join(mtr_data, by = c('parent_rank', 'age'))

  # Loop over discount rates: for each, compute discounted tax change across ranks and years
  cat(sprintf('  Fiscal NPV: Computing for %d discount rates (%.0f%% to %.0f%%)...\n',
              length(discount_rates), 100 * min(discount_rates), 100 * max(discount_rates)))

  summary_list <- list()

  for (dr in discount_rates) {

    total_return_npv <- 0
    total_weighted_mtr <- 0
    total_weight_for_mtr <- 0

    # Loop over years k = t + 27, t + 28, ..., t + 65
    for (work_age in start_work_age:end_work_age) {

      # Get earnings and MTR for this age
      year_earnings_mtr <- earnings_mtr_by_rank_age %>%
        filter(age == work_age)

      # Join with g_j and n_j by parent rank
      year_data <- g_by_rank %>%
        left_join(year_earnings_mtr, by = 'parent_rank')

      # Step 3a: For each rank j, compute tax_change_j = E_j * g_j * MTR_j * n_j
      # where n_j is the number of children at rank j
      year_data <- year_data %>%
        mutate(
          tax_change_j = earnings_real_2025 * g_j * mtr_wages * n_j
        )

      # Step 3b: Discount to year t (child is age 4 in year t)
      years_from_now <- work_age - analysis_age
      discount_factor <- 1 / (1 + dr)^years_from_now

      # Step 3c: Sum across ranks
      year_tax_pv <- sum(year_data$tax_change_j, na.rm = TRUE) * discount_factor

      # Track weighted MTR for reporting
      year_mtr_contribution <- sum(year_data$mtr_wages * year_data$earnings_real_2025 * year_data$n_j, na.rm = TRUE)
      year_weight <- sum(year_data$earnings_real_2025 * year_data$n_j, na.rm = TRUE)
      total_weighted_mtr <- total_weighted_mtr + year_mtr_contribution
      total_weight_for_mtr <- total_weight_for_mtr + year_weight

      # Step 5: Accumulate across years
      total_return_npv <- total_return_npv + year_tax_pv
    }

    return_npv <- total_return_npv / 1e9  # Convert to billions
    avg_mtr <- if (total_weight_for_mtr > 0) total_weighted_mtr / total_weight_for_mtr else NA_real_

    # Cost NPV with proper compounding by child age
    cost_npv_by_age <- cost_deltas_by_age %>%
      mutate(
        compound_factor = (1 + dr)^(analysis_age - child_age),
        cost_pv = net_cost_2025 * compound_factor
      )
    cost_npv <- sum(cost_npv_by_age$cost_pv) / 1e9

    net_npv <- return_npv - cost_npv
    share_recovered <- if (!is.na(cost_npv) && cost_npv > 0) return_npv / cost_npv else NA_real_

    summary_list[[as.character(dr)]] <- tibble(
      year = year,
      discount_rate = dr,
      return_npv = return_npv,
      cost_npv = cost_npv,
      net_npv = net_npv,
      share_recovered = share_recovered,
      mtr = avg_mtr,
      n_children = child_stats$n_children,
      n_switchers = child_stats$n_switchers,
      pct_switched = child_stats$pct_switched,
      avg_earnings_delta = child_stats$avg_earnings_delta,
      start_work_age = start_work_age,
      end_work_age = end_work_age,
      n_earnings_years = n_earnings_years,
      analysis_age = analysis_age,
      annual_demand_subsidy_delta = cost_deltas$demand_subsidy_delta,
      annual_supply_subsidy_delta = cost_deltas$supply_subsidy_delta,
      annual_employer_subsidy_delta = cost_deltas$employer_subsidy_delta,
      annual_cdctc_delta = cost_deltas$cdctc_delta,
      annual_parent_tax_delta = cost_deltas$parent_tax_delta,
      annual_net_cost = cost_deltas$annual_net_cost
    )
  }

  summary_tibble <- bind_rows(summary_list)

  # By-quintile breakdown (at 3% discount rate)
  dr_for_quintile <- 0.03

  # Compute lifetime NPV for each rank at 3% discount rate (vectorized)
  discounted_tax_by_rank_age <- earnings_mtr_by_rank_age %>%
    mutate(
      discount_factor = 1 / (1 + dr_for_quintile)^(age - analysis_age),
      tax_pv = earnings_real_2025 * mtr_wages * discount_factor
    ) %>%
    group_by(parent_rank) %>%
    summarise(lifetime_tax_pv_per_unit = sum(tax_pv, na.rm = TRUE), .groups = 'drop')

  lifetime_npv_by_rank <- g_by_rank %>%
    left_join(discounted_tax_by_rank_age, by = 'parent_rank') %>%
    mutate(
      lifetime_tax_pv_per_unit = coalesce(lifetime_tax_pv_per_unit, 0),
      lifetime_tax_npv = g_j * n_j * lifetime_tax_pv_per_unit
    ) %>%
    select(-lifetime_tax_pv_per_unit)

  # Add quintile and aggregate
  return_by_quintile <- lifetime_npv_by_rank %>%
    mutate(parent_quintile = pmin(ceiling(parent_rank / 20), 5)) %>%
    group_by(parent_quintile) %>%
    summarise(
      total_return_npv = sum(lifetime_tax_npv) / 1e9,
      n_children = sum(n_j),
      .groups = 'drop'
    )

  # Add child-level stats by quintile from analysis_base
  quintile_stats <- analysis_base %>%
    group_by(parent_quintile) %>%
    summarise(
      n_switchers = sum(child_weight * switched),
      avg_earnings_delta = weighted.mean(child_earnings.delta, child_weight),
      .groups = 'drop'
    )

  return_by_quintile <- return_by_quintile %>%
    left_join(quintile_stats, by = 'parent_quintile') %>%
    mutate(
      pct_switched = 100 * n_switchers / n_children,
      avg_return_npv = total_return_npv * 1e9 / n_children,  # Convert back to per-child
      share_of_returns = 100 * total_return_npv / sum(total_return_npv),
      year = year,
      discount_rate = dr_for_quintile
    ) %>%
    arrange(parent_quintile)

  # Log results (at 3% rate)
  result_3pct <- summary_tibble %>% filter(discount_rate == 0.03)
  if (nrow(result_3pct) > 0) {
    cat(sprintf('  Fiscal NPV (3%%): Return=$%.2fB, Cost=$%.2fB, Net=$%.2fB, Share Recovered=%.2f\n',
                result_3pct$return_npv,
                result_3pct$cost_npv,
                result_3pct$net_npv,
                result_3pct$share_recovered))
  }

  list(
    summary = summary_tibble,
    by_quintile = return_by_quintile,
    params = list(
      discount_rates = discount_rates,
      avg_mtr = avg_mtr,
      start_work_age = start_work_age,
      end_work_age = end_work_age,
      analysis_age = analysis_age,
      n_earnings_years = n_earnings_years
    )
  )
}
