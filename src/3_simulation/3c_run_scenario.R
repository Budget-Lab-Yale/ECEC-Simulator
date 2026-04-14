#------------------------------------------------------------------------------
# 3c_run_scenario.R
#
# Single scenario execution: load policies, solve equilibrium, expand with
# Gumbel shocks, collapse to discrete choices.
# Primary function: run_scenario()
#
# File-scope functions (called from outside this file):
#   - write_price_deltas(), compute_base_matrices(), build_final_components()
#   - extract_selected_values(), compute_group_employment_rate()
#   - crra_utility(), compute_V_crra()
#------------------------------------------------------------------------------



extract_selected_values <- function(df, choices, column_prefix, n_choices) {

  #----------------------------------------------------------------------------
  # Extracts the value from the selected choice column for each row.
  # Uses matrix indexing on {prefix}.1 ... {prefix}.n columns.
  #
  # Params:
  #   - df (df): Parent units dataframe with wide-format columns
  #   - choices (int vec): Integer vector of selected choice indices per row
  #   - column_prefix (chr): Prefix for wide columns (e.g. 'agi', 'taxes')
  #   - n_choices (int): Number of choices (columns {prefix}.1 ... {prefix}.n)
  #
  # Returns: (num vec) Values from the selected choice column for each row
  #----------------------------------------------------------------------------

  n_units <- nrow(df)
  col_names <- paste0(column_prefix, '.', 1:n_choices)

  # Extract as matrix for efficient indexing
  value_matrix <- as.matrix(df[, col_names])

  # Extract value at selected choice for each row
  # Use matrix indexing: cbind(row_indices, col_indices)
  values <- value_matrix[cbind(1:n_units, choices)]

  return(values)
}



compute_group_employment_rate <- function(P, group, parent_units_list, demand_params,
                                          employment_shifts, policy_demand, policy_cdctc,
                                          cpi_growth_factor) {

  #----------------------------------------------------------------------------
  # Computes weighted employment rate for a single demographic group at given
  # prices P and employment shifts.
  #
  # Params:
  #   - P (num vec): Price vector (length 4, one per market sector)
  #   - group (chr): Demographic group identifier (e.g. pc_group value)
  #   - parent_units_list (list): Named list of parent unit dataframes by type
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - employment_shifts (num vec): Named vector of employment shifts by group
  #   - policy_demand (fn): Demand policy function
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor for price scaling
  #
  # Returns: (dbl) Weighted employment rate for the group (scalar)
  #----------------------------------------------------------------------------

  total_emp_weighted <- 0
  total_weight <- 0

  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    n_children <- PARENT_UNIT_N_CHILDREN[i]
    pu_df <- parent_units_list[[pu_name]]

    if (is.null(pu_df) || nrow(pu_df) == 0) next

    # Filter to group
    group_mask <- pu_df$pc_group == group
    if (!any(group_mask)) next

    pu_group <- pu_df[group_mask, , drop = FALSE]

    # Get probability matrix with employment shifts applied
    p_matrix <- get_demand_prob_matrix(
      P = P,
      parent_units_df = pu_group,
      n_children = n_children,
      demand_params = demand_params,
      pu_name = pu_name,
      policy_demand = policy_demand,
      policy_cdctc = policy_cdctc,
      cpi_growth_factor = cpi_growth_factor,
      employment_shifts = employment_shifts
    )

    # Get working choice mask
    working_mask <- get_working_choice_mask(n_children)

    # Sum probabilities of working choices for each unit
    emp_probs <- rowSums(p_matrix[, working_mask, drop = FALSE])

    # Weight by primary caregiver weight
    weights <- if ('per_weight_pc' %in% names(pu_group)) {
      replace_na(pu_group$per_weight_pc, 0)
    } else {
      if_else(pu_group$primary_caregiver == 1, replace_na(pu_group$per_weight1, 0), replace_na(pu_group$per_weight2, 0))
    }

    total_emp_weighted <- total_emp_weighted + sum(emp_probs * weights)
    total_weight <- total_weight + sum(weights)
  }

  if (total_weight <= 0) {
    return(0)
  }

  total_emp_weighted / total_weight
}



#------------------------------------------------------------------------------
# SUPPLY MODEL
#------------------------------------------------------------------------------


compute_supply_residuals <- function(P, Qd, supply_params, policy_supply = identity) {

  #----------------------------------------------------------------------------
  # Computes supply-side residuals for equilibrium targeting.
  # Equilibrium condition: P_supply - c(Qd) = delta_0 * cpi_factor.
  #
  # Params:
  #   - P (num vec): Price vector (length 4, one per market sector)
  #   - Qd (num vec): Quantity demanded by sector (length 4, hours)
  #   - supply_params (list): Supply model parameters (L_req, w, L, e, delta_0,
  #       cpi_factor, nominal_wage_factor, wage_floor, employer_subsidy)
  #   - policy_supply (fn): Supply policy function mapping P to P_supply
  #
  # Returns: (list) delta_gap, actual_delta, target_delta, w, w_market,
  #   floor_binding, L, unit_cost, Qd
  #----------------------------------------------------------------------------


  # -- Step 1: Apply policy to get supplier prices ----

  P_supply <- policy_supply(P)

  if (any(!is.finite(P_supply))) {
    stop('compute_supply_residuals: policy_supply produced non-finite P_supply.')
  }
  if (any(!is.finite(Qd)) || any(Qd < 0)) {
    stop('compute_supply_residuals: Qd contains non-finite or negative values.')
  }


  # -- Step 2: Labor demand and wages ----

  # Labor demand by sector and type (4 x 2 matrix)
  L_by_sector <- supply_params$L_req * Qd

  # Total labor demand by type (length 2)
  L <- colSums(L_by_sector)

  # Market-clearing wages: w = w_0 * nominal_wage_factor * (L / L_0)^(1/eta)
  # Compute in log space to reduce overflow risk when Qd/weights are extreme.
  if (any(!is.finite(supply_params$w)) || any(supply_params$w <= 0) ||
      !is.finite(supply_params$nominal_wage_factor) || supply_params$nominal_wage_factor <= 0 ||
      any(!is.finite(supply_params$L)) || any(supply_params$L <= 0) ||
      any(!is.finite(supply_params$e)) || any(supply_params$e <= 0)) {
    stop('compute_supply_residuals: invalid supply parameters (non-finite or non-positive).')
  }

  ratio <- L / supply_params$L
  ratio <- pmax(ratio, 1e-12)

  MAX_WAGE_PER_HOUR <- 1e6
  log_w_market <- log(supply_params$w) +
                  log(supply_params$nominal_wage_factor) +
                  (1 / supply_params$e) * log(ratio)
  if (any(!is.finite(log_w_market))) {
    stop('compute_supply_residuals: non-finite log_w_market (check L/L0, elasticities).')
  }
  if (any(log_w_market > log(MAX_WAGE_PER_HOUR))) {
    stop('compute_supply_residuals: implied wage exceeds $', MAX_WAGE_PER_HOUR,
         '/hour. This typically indicates extreme weights/demand (wage overflow).')
  }
  w_market <- exp(log_w_market)


  # -- Step 2b: Apply wage floor (if defined) ----

  # Check if wage floor policy is active
  if (!is.null(supply_params$wage_floor)) {
    # Wage floor adjusted for nominal wage growth (keeps floor in current dollars)
    # Base values are in 2019 dollars
    adjusted_floor <- supply_params$wage_floor * supply_params$nominal_wage_factor

    # Apply floor: actual wage is max of market wage and floor
    w <- pmax(w_market, adjusted_floor)

    # Track whether floor is binding (for diagnostics)
    floor_binding <- w_market < adjusted_floor
  } else {
    # No wage floor - market wages apply
    w <- w_market
    floor_binding <- c(FALSE, FALSE)
  }


  # -- Step 2c: Apply employer subsidy (if defined) ----
  #
  # Subsidy reduces effective labor cost for employers (workers still receive w).
  # This flows through to lower unit costs -> lower equilibrium prices -> higher demand.
  # Fiscal cost = hours * L_req * subsidy_rate (computed in fiscal_cost.R)
  #
  # NOTE: supply_params$employer_subsidy is assumed to already be in current-year
  # dollars (same units as w), and is applied only to supply_params$employer_subsidy_sectors.

  if (!is.null(supply_params$employer_subsidy_pct)) {
    # Percentage-based subsidy: effective wage = w * (1 - pct)
    pct <- supply_params$employer_subsidy_pct
    employer_subsidy_sectors <- supply_params$employer_subsidy_sectors
    if (is.null(employer_subsidy_sectors)) {
      employer_subsidy_sectors <- seq_len(nrow(supply_params$L_req))
    }

    w_effective <- w * (1 - pct)
    unit_cost_unsubsidized <- as.vector(supply_params$L_req %*% w)
    unit_cost_subsidized <- as.vector(supply_params$L_req %*% w_effective)
    unit_cost <- unit_cost_unsubsidized
    unit_cost[employer_subsidy_sectors] <- unit_cost_subsidized[employer_subsidy_sectors]
  } else if (!is.null(supply_params$employer_subsidy)) {
    # Dollar-based subsidy: effective wage = w - subsidy (floored at 0.01)
    # IMPORTANT: supply_params$employer_subsidy is assumed to be in CURRENT-YEAR
    # dollars (same units as w). Scaling from base-year is handled upstream.

    employer_subsidy_sectors <- supply_params$employer_subsidy_sectors
    if (is.null(employer_subsidy_sectors)) {
      employer_subsidy_sectors <- seq_len(nrow(supply_params$L_req))
    }

    # Effective wage for cost calculation (floored at small positive to avoid negative)
    w_effective <- pmax(w - supply_params$employer_subsidy, 0.01)

    # Apply subsidy only to covered sectors; uncovered sectors use unsubsidized wages.
    unit_cost_unsubsidized <- as.vector(supply_params$L_req %*% w)
    unit_cost_subsidized <- as.vector(supply_params$L_req %*% w_effective)
    unit_cost <- unit_cost_unsubsidized
    unit_cost[employer_subsidy_sectors] <- unit_cost_subsidized[employer_subsidy_sectors]
  } else {
    unit_cost <- as.vector(supply_params$L_req %*% w)
  }


  # -- Step 3: Unit cost ----

  # Unit labor cost by sector: c[s] = sum_l (L_req[s,l] * w_l), with optional
  # employer subsidy applied only to covered sectors (see Step 2c).


  # -- Step 4: Per-unit residual revenue ----

  # Actual per-unit residual revenue: P_supply - c
  actual_delta <- P_supply - unit_cost

  # Target per-unit residual revenue: delta_0 * cpi_factor (exogenous, grows with CPI)
  target_delta <- supply_params$delta_0 * supply_params$cpi_factor

  # Gap (should be zero at equilibrium)
  delta_gap <- actual_delta - target_delta


  return(list(
    actual_delta  = actual_delta,
    target_delta  = target_delta,
    delta_gap     = delta_gap,
    w             = w,
    w_market      = w_market,
    floor_binding = floor_binding,
    L             = L,
    unit_cost     = unit_cost,
    Qd            = Qd
  ))
}



#------------------------------------------------------------------------------
# DEMAND MODEL: p = exp(alpha + V) / Z where V = beta * u(NI)
#------------------------------------------------------------------------------



crra_utility <- function(c, rho) {

  #----------------------------------------------------------------------------
  # Computes CRRA (constant relative risk aversion) utility. Returns log(c)
  # when rho is approximately 1, otherwise c^(1-rho) / (1-rho). Floors c at 1.
  #
  # Params:
  #   - c (num vec): Consumption values (floored at 1 to avoid log(0))
  #   - rho (dbl): Risk aversion parameter (1 = log utility)
  #
  # Returns: (num vec) CRRA utility values
  #----------------------------------------------------------------------------

  c <- pmax(c, 1)
  if (abs(rho - 1) < 1e-6) {
    return(log(c))
  }
  (c^(1 - rho)) / (1 - rho)
}



compute_V_crra <- function(consumption, beta, rho) {

  #----------------------------------------------------------------------------
  # Computes deterministic utility V = beta * u(consumption) using CRRA.
  #
  # Params:
  #   - consumption (num vec): Net income or consumption values
  #   - beta (dbl): Scale parameter for utility
  #   - rho (dbl): CRRA risk aversion parameter
  #
  # Returns: (num vec) Scaled CRRA utility values
  #----------------------------------------------------------------------------

  beta * crra_utility(consumption, rho)
}



get_demand_prob_matrix <- function(P, parent_units_df, n_children, demand_params,
                                   pu_name = NULL, policy_demand = NULL, policy_cdctc = NULL,
                                   cpi_growth_factor = 1.0, employment_shifts = NULL) {

  #----------------------------------------------------------------------------
  # Returns probability matrix using alpha-based model: p = exp(alpha + V) / Z.
  # Optionally applies employment_shifts to working choices.
  #
  # Params:
  #   - P (num vec): Price vector (length 4, one per market sector)
  #   - parent_units_df (df): Parent units dataframe for a single type
  #   - n_children (int): Number of children (1 or 2)
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - pu_name (chr): Parent unit type name (e.g. 'c1_p1')
  #   - policy_demand (fn): Demand policy function (or NULL for baseline)
  #   - policy_cdctc (fn): CDCTC policy function (or NULL for baseline)
  #   - cpi_growth_factor (dbl): CPI growth factor for price scaling
  #   - employment_shifts (num vec): Named vector of shifts by group (or NULL)
  #
  # Returns: (matrix) n_units x n_choices probability matrix
  #----------------------------------------------------------------------------

  n_choices <- if (n_children == 1) N_CHOICES_1_CHILD else N_CHOICES_2_CHILD

  # Extract type-specific demand parameters
  type_params <- demand_params[[pu_name]]
  beta <- type_params$beta
  rho <- type_params$rho

  # Validate alpha is available (required for alpha-based model)
  if (is.null(type_params$alpha)) {
    stop('get_demand_prob_matrix: alpha matrix not found for ', pu_name, '. ',
         'Re-run calibration to generate alpha matrices.')
  }

  # ALPHA-BASED MODEL: p = exp(alpha + V) / Z
  # This applies to both baseline and counterfactual scenarios

  pc <- compute_policy_components(parent_units_df, P, n_children, demand_params,
                                   policy_demand, policy_cdctc, cpi_growth_factor)
  base <- pc$base

  if (any(!is.finite(base$agi_matrix)) ||
      any(!is.finite(base$taxes_matrix)) ||
      any(!is.finite(base$gross_ecec_cost_matrix))) {
    stop('get_demand_prob_matrix: non-finite values in base matrices (', pu_name, ').')
  }
  if (any(!is.finite(pc$subsidy_matrix))) {
    stop('get_demand_prob_matrix: non-finite subsidy_matrix (', pu_name, ').')
  }
  if (any(!is.finite(pc$cdctc_matrix))) {
    stop('get_demand_prob_matrix: non-finite cdctc_matrix (', pu_name, ').')
  }

  Y_policy <- base$agi_matrix - base$taxes_matrix
  C_policy <- base$gross_ecec_cost_matrix - pc$subsidy_matrix - pc$cdctc_matrix
  NI_policy <- Y_policy - C_policy

  if (any(!is.finite(NI_policy))) {
    stop('get_demand_prob_matrix: non-finite NI_policy (Y - C) (', pu_name, ').')
  }

  # Compute V = beta * u(NI) in REAL 2019 dollars.
  # beta/rho are calibrated on 2019-dollar net income; without deflation,
  # nominal income growth would mechanically change behavior over time.
  cpi_factor_2019 <- parent_units_df[['cpi_factor_2019']]
  if (is.null(cpi_factor_2019)) {
    cpi_factor_2019 <- 1.0
  } else {
    cpi_factor_2019 <- unique(cpi_factor_2019)
    if (length(cpi_factor_2019) != 1 || !is.finite(cpi_factor_2019) || cpi_factor_2019 <= 0) {
      stop('get_demand_prob_matrix: invalid cpi_factor_2019 (must be a single positive finite value) for ', pu_name, '.')
    }
  }

  NI_real_2019 <- NI_policy / cpi_factor_2019
  V_matrix <- compute_V_crra(NI_real_2019, beta, rho)

  if (any(!is.finite(V_matrix))) {
    stop('get_demand_prob_matrix: non-finite V_matrix from CRRA utility (', pu_name, ').')
  }

  # Apply employment shifts if provided (for employment targeting)
  # Adds group-specific delta to working choices for each demographic group
  if (!is.null(employment_shifts) && any(employment_shifts != 0)) {
    working_mask <- get_working_choice_mask(n_children)
    pc_group <- parent_units_df$pc_group

    for (g in names(employment_shifts)) {
      if (employment_shifts[g] != 0) {
        group_rows <- which(pc_group == g)
        if (length(group_rows) > 0) {
          V_matrix[group_rows, working_mask] <- V_matrix[group_rows, working_mask] + employment_shifts[g]
        }
      }
    }
  }

  # Match alpha matrix rows to parent_units_df rows by row_ids
  {
    alpha <- type_params$alpha
    row_ids <- type_params$row_ids
    n_rows <- nrow(parent_units_df)

    # Extract keys from parent_units_df
    has_pseudofamily <- 'pseudofamily_id' %in% names(parent_units_df)

    if (has_pseudofamily) {
      pu_keys <- parent_units_df %>%
        select(hh_id, parent_unit_id, pseudofamily_id) %>%
        mutate(row_idx = row_number())
    } else {
      # For types without pseudofamily_id, use default value of 1
      pu_keys <- parent_units_df %>%
        select(hh_id, parent_unit_id) %>%
        mutate(pseudofamily_id = 1L, row_idx = row_number())
    }

    # Convert row_ids to tibble for joining
    row_ids_tbl <- tibble(row_ids) %>%
      mutate(alpha_idx = row_number())

    # Handle case where row_ids may not have pseudofamily_id
    if (!('pseudofamily_id' %in% names(row_ids_tbl))) {
      row_ids_tbl$pseudofamily_id <- 1L
    }

    # Ensure type consistency for join keys (coerce to character)
    pu_keys <- pu_keys %>%
      mutate(
        hh_id = as.character(hh_id),
        parent_unit_id = as.character(parent_unit_id),
        pseudofamily_id = as.character(pseudofamily_id)
      )
    row_ids_tbl <- row_ids_tbl %>%
      mutate(
        hh_id = as.character(hh_id),
        parent_unit_id = as.character(parent_unit_id),
        pseudofamily_id = as.character(pseudofamily_id)
      )

    # Validate key uniqueness to avoid many-to-many joins (silent row expansion)
    dup_alpha <- row_ids_tbl %>%
      count(hh_id, parent_unit_id, pseudofamily_id) %>%
      filter(n > 1)
    if (nrow(dup_alpha) > 0) {
      stop('match_alpha_to_parent_units: alpha row_ids contain duplicate keys. ',
           'This would create an ambiguous match.')
    }
    dup_pu <- pu_keys %>%
      count(hh_id, parent_unit_id, pseudofamily_id) %>%
      filter(n > 1)
    if (nrow(dup_pu) > 0) {
      stop('match_alpha_to_parent_units: parent_units_df contains duplicate keys. ',
           'This indicates duplicated rows for (hh_id, parent_unit_id, pseudofamily_id).')
    }

    # Match: find alpha_idx for each parent_units_df row
    matched <- pu_keys %>%
      left_join(row_ids_tbl, by = c('hh_id', 'parent_unit_id', 'pseudofamily_id'))

    # Check for unmatched rows (should not happen if data is consistent)
    n_unmatched <- sum(is.na(matched$alpha_idx))
    if (n_unmatched > 0) {
      stop(sprintf(
        'match_alpha_to_parent_units: %d of %d rows have no matching alpha. ',
        n_unmatched, n_rows
      ), 'Check that calibration data covers all simulation households.')
    }

    # Reorder alpha matrix to match parent_units_df row ordering
    alpha_matrix <- alpha[matched$alpha_idx, , drop = FALSE]
  }

  # Compute probabilities: p = exp(alpha + V) / Z
  compute_probs_from_alpha(alpha_matrix, V_matrix)
}



compute_base_matrices <- function(parent_units_df, catalog, P, n_children,
                                   price_wedge.center_low,
                                   price_wedge.center_high,
                                   price_wedge.home,
                                   price_wedge.other_paid = NULL,
                                   other_paid_base_price = NULL,
                                   cpi_growth_factor = 1.0) {

  #----------------------------------------------------------------------------
  # Computes base matrices (agi, taxes, gross_ecec_cost, per-child costs, etc.)
  # from parent unit data and prices. First step in the demand policy pipeline.
  #
  # Params:
  #   - parent_units_df (df): Parent units dataframe for a single type
  #   - catalog (df): Choice catalog with sector/hours/type columns
  #   - P (num vec): Price vector (length 4, one per market sector)
  #   - n_children (int): Number of children (1 or 2)
  #   - price_wedge.center_low (num vec): Price wedge for low-priced centers
  #   - price_wedge.center_high (num vec): Price wedge for high-priced centers
  #   - price_wedge.home (num vec): Price wedge for paid home-based care
  #   - price_wedge.other_paid (num vec): Price wedge for Other Paid care
  #   - other_paid_base_price (dbl): Base price for Other Paid sector (or NULL)
  #   - cpi_growth_factor (dbl): CPI growth factor for Other Paid price scaling
  #
  # Returns: (list) agi_matrix, taxes_matrix, iit_matrix, gross_ecec_cost_matrix,
  #   child1_cost_matrix, child2_cost_matrix, agi1/agi2/iit1/iit2/earnings1/
  #   earnings2 matrices for CDCTC attribution
  #----------------------------------------------------------------------------

  n_choices <- nrow(catalog)
  n_units <- nrow(parent_units_df)

  # Build hours lookup: hours_choice -> annual hours
  hours_annual <- HOURS_ANNUAL

  employment_choices <- catalog$employment_choice

  # Local helper: build n_units x n_choices matrix from employment-choice columns
  build_emp_matrix <- function(col_prefix) {

    #--------------------------------------------------------------------------
    # Builds n_units x n_choices matrix from employment-choice columns.
    # Maps {col_prefix}.none / .pt / .ft to the appropriate choice columns.
    #
    # Params:
    #   - col_prefix (chr): Column prefix (e.g. 'agi', 'total_tax')
    #
    # Returns: (matrix) n_units x n_choices matrix of values
    #--------------------------------------------------------------------------

    mat <- matrix(NA_real_, nrow = n_units, ncol = n_choices)
    for (emp in c('none', 'pt', 'ft')) {
      mask <- employment_choices == emp
      mat[, mask] <- parent_units_df[[paste0(col_prefix, '.', emp)]]
    }
    mat
  }

  agi_matrix      <- build_emp_matrix('agi')
  taxes_matrix    <- build_emp_matrix('total_tax')
  iit_matrix      <- build_emp_matrix('income_tax')
  agi1_matrix     <- build_emp_matrix('agi1')
  agi2_matrix     <- build_emp_matrix('agi2')
  iit1_matrix     <- build_emp_matrix('liab_iit1')
  iit2_matrix     <- build_emp_matrix('liab_iit2')
  earnings1_matrix <- build_emp_matrix('earnings1')
  earnings2_matrix <- build_emp_matrix('earnings2')

  #--------------------------
  # Build gross_ecec_cost matrix with unit-level price heterogeneity
  # Also build per-child cost matrices for tax-unit-level CDCTC attribution
  #--------------------------

  child1_cost_matrix <- matrix(0, nrow = n_units, ncol = n_choices)
  child2_cost_matrix <- matrix(0, nrow = n_units, ncol = n_choices)

  child1_sector <- catalog$child1_market_sector_id
  child1_hours <- unname(hours_annual[catalog$child1_hours_choice])

  # Wedge vectors by sector (length n_units)
  wedge_by_sector <- list(
    `1` = rep(1, n_units),
    `2` = price_wedge.center_low,
    `3` = price_wedge.center_high,
    `4` = price_wedge.home
  )

  for (sector_id in 1:4) {
    if (P[sector_id] == 0) next
    cols <- which(child1_sector == sector_id & child1_hours > 0)
    if (length(cols) == 0) next
    wedge <- wedge_by_sector[[as.character(sector_id)]]
    child1_cost_matrix[, cols] <- tcrossprod(wedge, child1_hours[cols] * P[sector_id])
  }

  if (n_children == 2) {
    child2_sector <- catalog$child2_market_sector_id
    child2_hours <- unname(hours_annual[catalog$child2_hours_choice])

    for (sector_id in 1:4) {
      if (P[sector_id] == 0) next
      cols <- which(child2_sector == sector_id & child2_hours > 0)
      if (length(cols) == 0) next
      wedge <- wedge_by_sector[[as.character(sector_id)]]
      child2_cost_matrix[, cols] <- tcrossprod(wedge, child2_hours[cols] * P[sector_id])
    }
  }

  #--------------------------
  # Other Paid: non-market sector with exogenous price (scaled by CPI)
  # This ensures V_sim includes realistic Other Paid costs, matching V_cal
  #--------------------------

  if (!is.null(other_paid_base_price) && !is.null(price_wedge.other_paid) &&
      !is.na(other_paid_base_price) && other_paid_base_price > 0) {

    # Scale base price by CPI growth from calibration year to simulation year
    other_paid_price <- other_paid_base_price * cpi_growth_factor

    # Get household-specific wedge
    wedge_other_paid <- price_wedge.other_paid
    if (is.null(wedge_other_paid)) wedge_other_paid <- rep(1, n_units)

    # Child 1: Other Paid choices (where market_sector_id is NA and ecec_type is 'Other Paid')
    child1_ecec_type <- catalog$child1_ecec_type
    op_cols <- which(is.na(child1_sector) &
                     child1_ecec_type == 'Other Paid' &
                     child1_hours > 0)
    if (length(op_cols) > 0) {
      child1_cost_matrix[, op_cols] <- tcrossprod(wedge_other_paid, child1_hours[op_cols] * other_paid_price)
    }

    # Child 2: Other Paid choices (for n_children == 2)
    if (n_children == 2) {
      child2_ecec_type <- catalog$child2_ecec_type
      op_cols2 <- which(is.na(child2_sector) &
                        child2_ecec_type == 'Other Paid' &
                        child2_hours > 0)
      if (length(op_cols2) > 0) {
        child2_cost_matrix[, op_cols2] <- tcrossprod(wedge_other_paid, child2_hours[op_cols2] * other_paid_price)
      }
    }
  }

  gross_ecec_cost_matrix <- if (n_children == 2) {
    child1_cost_matrix + child2_cost_matrix
  } else {
    child1_cost_matrix
  }

  # -- Assertions: catch silent data bugs in base matrices ----

  # Price wedges must actually vary across households (catches vector recycling bug:
  # if ifelse() or similar returns a scalar, tcrossprod broadcasts it identically
  # to all rows, making every household pay the same price)
  if (n_units > 1) {
    stopifnot(sd(price_wedge.center_low) > 0)
    stopifnot(sd(price_wedge.center_high) > 0)
    stopifnot(sd(price_wedge.home) > 0)
  }

  # AGI must differ across employment choices (if agi.none == agi.ft for everyone,
  # employment choice columns are being built from the wrong source column)
  if (n_units > 1) {
    none_cols <- which(employment_choices == 'none')
    ft_cols <- which(employment_choices == 'ft')
    if (length(none_cols) > 0 && length(ft_cols) > 0) {
      # At least some households should have different AGI for none vs ft
      agi_diff <- agi_matrix[, ft_cols[1]] - agi_matrix[, none_cols[1]]
      stopifnot(any(agi_diff != 0))
    }
  }

  return(list(
    agi_matrix = agi_matrix,
    taxes_matrix = taxes_matrix,
    iit_matrix = iit_matrix,
    gross_ecec_cost_matrix = gross_ecec_cost_matrix,
    # Per-child cost matrices for tax-unit attribution
    child1_cost_matrix = child1_cost_matrix,
    child2_cost_matrix = child2_cost_matrix,
    # Tax-unit-level matrices for CDCTC
    agi1_matrix = agi1_matrix,
    agi2_matrix = agi2_matrix,
    iit1_matrix = iit1_matrix,
    iit2_matrix = iit2_matrix,
    earnings1_matrix = earnings1_matrix,
    earnings2_matrix = earnings2_matrix
  ))
}



build_final_components <- function(agi_matrix, taxes_matrix, gross_ecec_cost_matrix,
                                    subsidy_matrix, cdctc_matrix) {

  #----------------------------------------------------------------------------
  # Combines base matrices with subsidies/CDCTC to produce final .k columns.
  # Computes net_income, Y (pre-care income), and C (out-of-pocket cost).
  #
  # Params:
  #   - agi_matrix (matrix): n_units x n_choices AGI matrix
  #   - taxes_matrix (matrix): n_units x n_choices total tax matrix
  #   - gross_ecec_cost_matrix (matrix): n_units x n_choices gross ECEC cost
  #   - subsidy_matrix (matrix): n_units x n_choices subsidy amounts
  #   - cdctc_matrix (matrix): n_units x n_choices CDCTC credit amounts
  #
  # Returns: (df) Tibble with agi.k, taxes.k, gross_ecec_cost.k, subsidy.k,
  #   cdctc.k, net_income.k, Y.k, C.k columns for each choice k
  #----------------------------------------------------------------------------

  n_choices <- ncol(agi_matrix)

  #--------------------------
  # Calculate net_income (with CDCTC)
  #--------------------------

  # net_income = agi - taxes - gross_ecec_cost + subsidy + cdctc
  net_income_matrix <- agi_matrix - taxes_matrix - gross_ecec_cost_matrix + subsidy_matrix + cdctc_matrix

  #--------------------------
  # Two-parameter model: Y (pre-care income) and C (out-of-pocket cost)
  #--------------------------

  # Y = agi - taxes (disposable income BEFORE care costs)
  Y_matrix <- agi_matrix - taxes_matrix

  # C = gross_ecec_cost - subsidy - cdctc (out-of-pocket care cost)
  # CDCTC reduces out-of-pocket cost (flows through price channel)
  C_matrix <- gross_ecec_cost_matrix - subsidy_matrix - cdctc_matrix

  # -- Assertion: catch formula bugs ----
  # Y - C must equal net_income (these are two views of the same quantity)
  stopifnot(all(abs(Y_matrix - C_matrix - net_income_matrix) < 1e-6))

  #--------------------------
  # Convert to tibbles with named columns
  #--------------------------

  colnames(agi_matrix) <- paste0('agi.', 1:n_choices)
  colnames(taxes_matrix) <- paste0('taxes.', 1:n_choices)
  colnames(gross_ecec_cost_matrix) <- paste0('gross_ecec_cost.', 1:n_choices)
  colnames(subsidy_matrix) <- paste0('subsidy.', 1:n_choices)
  colnames(cdctc_matrix) <- paste0('cdctc.', 1:n_choices)
  colnames(net_income_matrix) <- paste0('net_income.', 1:n_choices)
  colnames(Y_matrix) <- paste0('Y.', 1:n_choices)
  colnames(C_matrix) <- paste0('C.', 1:n_choices)

  result <- bind_cols(
    as_tibble(agi_matrix),
    as_tibble(taxes_matrix),
    as_tibble(gross_ecec_cost_matrix),
    as_tibble(subsidy_matrix),
    as_tibble(cdctc_matrix),
    as_tibble(net_income_matrix),
    as_tibble(Y_matrix),
    as_tibble(C_matrix)
  )

  return(result)
}



write_price_deltas <- function(baseline_result, result, year, scenario_info) {

  #----------------------------------------------------------------------------
  # Writes price deltas between baseline and counterfactual to disk.
  #
  # Params:
  #   - baseline_result (list): Baseline scenario result with $prices
  #   - result (list): Counterfactual scenario result with $prices
  #   - year (int): Simulation year
  #   - scenario_info (list): Scenario configuration with $paths$output
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  output_dir <- file.path(scenario_info$paths$output, 'models', 'equilibrium')
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  price_delta_df <- tibble(
    sector               = c('Unpaid Center-Based', 'Low-Priced Center-Based',
                             'High-Priced Center-Based', 'Paid Home-Based'),
    baseline_price       = baseline_result$prices,
    counterfactual_price = result$prices,
    delta                = result$prices - baseline_result$prices
  )

  fwrite(
    price_delta_df,
    file.path(output_dir, paste0('price_deltas_', year, '.csv')),
    na = 'NA'
  )
}



run_scenario <- function(scenario_info, supply_params, demand_params, parent_units_list,
                         year, initial_prices, fail_on_non_convergence = TRUE,
                         n_draws_per_record = 100, year_seed = NULL,
                         macro_projections = NULL,
                         target_employment_rates = NULL,
                         baseline_employment_shifts = NULL) {

  #----------------------------------------------------------------------------
  # Runs a single scenario (baseline or counterfactual) for one year. Loads
  # policies, solves equilibrium, expands with Gumbel shocks, collapses to
  # discrete choices, and writes outputs.
  #
  # Params:
  #   - scenario_info (list): Scenario configuration (id, paths, policies, etc.)
  #   - supply_params (list): Supply model parameters (L_req, w, L, e, delta_0)
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - parent_units_list (list): Named list of parent unit dataframes by type
  #   - year (int): Simulation year
  #   - initial_prices (num vec): Initial price guess (length 4, or NULL)
  #   - fail_on_non_convergence (logical): Stop on non-convergence if TRUE
  #   - n_draws_per_record (int): Number of Gumbel draws per household
  #   - year_seed (int): Random seed for reproducibility (or NULL)
  #   - macro_projections (df): Macro projection data for growth factors
  #   - target_employment_rates (num vec): Target rates by group (or NULL)
  #   - baseline_employment_shifts (num vec): Fixed shifts for policy runs
  #
  # Returns: (list) prices, parent_units, Qd, w, L, actual_delta, target_delta,
  #   converged, max_abs_delta_gap_paid, value, optim_result,
  #   employment_shifts, target_employment_rates, supply_subsidy
  #----------------------------------------------------------------------------


  #============================================================================
  # Local helper functions (called multiple times or used as callbacks)
  #============================================================================



  get_sector_hours_matrices <- function(n_children) {

    #--------------------------------------------------------------------------
    # Returns cached matrices mapping choices to sector-hours (H1, H2) for
    # demand aggregation. Caches result in .choice_cache environment.
    #
    # Params:
    #   - n_children (int): Number of children (1 or 2)
    #
    # Returns: (list) H1 (and H2 if n_children == 2), each n_choices x 4
    #--------------------------------------------------------------------------

    cache_key <- paste0('sector_hours_', n_children)

    if (exists(cache_key, envir = .choice_cache)) {
      return(get(cache_key, envir = .choice_cache))
    }

    catalog <- get_choice_catalog(n_children)
    n_choices <- nrow(catalog)

    hours_annual <- HOURS_ANNUAL

    H1 <- matrix(0, nrow = n_choices, ncol = 4)
    for (k in 1:n_choices) {
      sector <- catalog$child1_market_sector_id[k]
      if (!is.na(sector)) H1[k, sector] <- hours_annual[catalog$child1_hours_choice[k]]
    }

    if (n_children == 1) {
      result <- list(H1 = H1)
    } else {
      H2 <- matrix(0, nrow = n_choices, ncol = 4)
      for (k in 1:n_choices) {
        sector <- catalog$child2_market_sector_id[k]
        if (!is.na(sector)) H2[k, sector] <- hours_annual[catalog$child2_hours_choice[k]]
      }
      result <- list(H1 = H1, H2 = H2)
    }

    assign(cache_key, result, envir = .choice_cache)
    return(result)
  }



  emp_prob_with_shift <- function(emp0, delta) {

    #--------------------------------------------------------------------------
    # Computes shifted employment probability using logistic transformation.
    # Maps baseline probability emp0 through exp(delta) scaling in odds space.
    #
    # Params:
    #   - emp0 (num vec): Baseline employment probabilities (clamped to (0,1))
    #   - delta (dbl): Shift parameter in log-odds space
    #
    # Returns: (num vec) Shifted employment probabilities
    #--------------------------------------------------------------------------

    emp0 <- pmin(pmax(emp0, 1e-12), 1 - 1e-12)
    t <- exp(delta)
    (t * emp0) / ((1 - emp0) + t * emp0)
  }



  build_emp0_cache <- function(P_prices, fn_demand, fn_cdctc) {

    #--------------------------------------------------------------------------
    # Builds cache of baseline employment probabilities by demographic group.
    # Iterates over all parent unit types and aggregates emp0 and weights.
    #
    # Params:
    #   - P_prices (num vec): Price vector (length 4)
    #   - fn_demand (fn): Demand policy function
    #   - fn_cdctc (fn): CDCTC policy function
    #
    # Returns: (list) Named list by group, each with emp0 and weight vectors
    #--------------------------------------------------------------------------

    emp0_cache <- lapply(EMPLOYMENT_TARGETING_GROUPS, function(.) list(emp0 = numeric(0), weight = numeric(0)))
    names(emp0_cache) <- EMPLOYMENT_TARGETING_GROUPS

    for (i_emp in seq_along(PARENT_UNIT_NAMES)) {
      pu_name_emp <- PARENT_UNIT_NAMES[i_emp]
      n_children_emp <- PARENT_UNIT_N_CHILDREN[i_emp]
      pu_df_emp <- parent_units_list[[pu_name_emp]]

      if (is.null(pu_df_emp) || nrow(pu_df_emp) == 0) next
      if (!('pc_group' %in% names(pu_df_emp))) next

      p0_matrix <- get_demand_prob_matrix(
        P = P_prices,
        parent_units_df = pu_df_emp,
        n_children = n_children_emp,
        demand_params = demand_params,
        pu_name = pu_name_emp,
        policy_demand = fn_demand,
        policy_cdctc = fn_cdctc,
        cpi_growth_factor = supply_params$cpi_factor,
        employment_shifts = NULL
      )

      working_mask_emp <- get_working_choice_mask(n_children_emp)
      emp0_vec <- rowSums(p0_matrix[, working_mask_emp, drop = FALSE])

      weights_emp <- if ('per_weight_pc' %in% names(pu_df_emp)) {
        replace_na(pu_df_emp$per_weight_pc, 0)
      } else {
        if_else(pu_df_emp$primary_caregiver == 1, replace_na(pu_df_emp$per_weight1, 0), replace_na(pu_df_emp$per_weight2, 0))
      }

      for (g in EMPLOYMENT_TARGETING_GROUPS) {
        idx <- which(pu_df_emp$pc_group == g)
        if (length(idx) == 0) next
        emp0_cache[[g]]$emp0 <- c(emp0_cache[[g]]$emp0, emp0_vec[idx])
        emp0_cache[[g]]$weight <- c(emp0_cache[[g]]$weight, weights_emp[idx])
      }
    }
    emp0_cache
  }



  solve_employment_shifts <- function(P_prices, fn_demand, fn_cdctc) {

    #--------------------------------------------------------------------------
    # Solves employment shifts for all demographic groups at given prices.
    # Builds emp0 cache then solves each group independently via uniroot.
    #
    # Params:
    #   - P_prices (num vec): Price vector (length 4)
    #   - fn_demand (fn): Demand policy function
    #   - fn_cdctc (fn): CDCTC policy function
    #
    # Returns: (num vec) Named vector of employment shifts by group
    #--------------------------------------------------------------------------

    shifts <- setNames(rep(0, length(EMPLOYMENT_TARGETING_GROUPS)), EMPLOYMENT_TARGETING_GROUPS)
    emp0_cache <- build_emp0_cache(P_prices, fn_demand, fn_cdctc)
    for (g in EMPLOYMENT_TARGETING_GROUPS) {
      if (is.na(target_employment_rates[g]) || target_employment_rates[g] <= 0) next
      shifts[g] <- solve_group_employment_shift(
        P = P_prices,
        group = g,
        target_rate = target_employment_rates[g],
        parent_units_list = parent_units_list,
        emp0_cache = emp0_cache,
        tol = 1e-5
      )
    }
    shifts
  }



  solve_group_employment_shift <- function(P, group, target_rate, parent_units_list,
                                           emp0_cache,
                                           bounds = c(-10, 10), tol = 1e-5) {

    #--------------------------------------------------------------------------
    # Solves for employment shift delta_g such that E_g(delta_g) = target_g
    # using uniroot. Returns 0 if group is empty or root not found.
    #
    # Params:
    #   - P (num vec): Price vector (length 4)
    #   - group (chr): Demographic group identifier
    #   - target_rate (dbl): Target employment rate for this group
    #   - parent_units_list (list): Named list of parent unit dataframes
    #   - emp0_cache (list): Precomputed baseline emp0/weight by group
    #   - bounds (num vec): Initial search bounds for uniroot (length 2)
    #   - tol (dbl): Tolerance for uniroot convergence
    #
    # Returns: (dbl) Employment shift delta (0 if unsolvable)
    #--------------------------------------------------------------------------

    # If group empty (or zero weight), do nothing.
    if (is.null(emp0_cache[[group]]) ||
        length(emp0_cache[[group]]$emp0) == 0 ||
        sum(emp0_cache[[group]]$weight, na.rm = TRUE) <= 0) {
      return(0)
    }

    objective <- function(delta) {

      #------------------------------------------------------------------------
      # Objective for uniroot: E_g(delta) - target = 0. Computes weighted
      # employment rate at shifted delta minus target rate.
      #
      # Params:
      #   - delta (dbl): Employment shift in log-odds space
      #
      # Returns: (dbl) Difference between shifted employment rate and target
      #------------------------------------------------------------------------

      emp0 <- emp0_cache[[group]]$emp0
      w <- emp0_cache[[group]]$weight
      emp <- emp_prob_with_shift(emp0, delta)
      (sum(emp * w) / sum(w)) - target_rate
    }

    # Check if bounds bracket the root
    f_lower <- objective(bounds[1])
    f_upper <- objective(bounds[2])

    if (!is.finite(f_lower) || !is.finite(f_upper)) {
      warning(sprintf('solve_group_employment_shift: non-finite values at bounds for group %s', group))
      return(0)
    }

    if (f_lower * f_upper > 0) {
      # Root not bracketed - try expanding bounds
      expanded_bounds <- c(-20, 20)
      f_lower <- objective(expanded_bounds[1])
      f_upper <- objective(expanded_bounds[2])

      if (f_lower * f_upper > 0) {
        warning(sprintf(
          'solve_group_employment_shift: root not bracketed for group %s (f_lower=%.4f, f_upper=%.4f)',
          group, f_lower, f_upper
        ))
        return(0)
      }
      bounds <- expanded_bounds
    }

    # Solve using uniroot
    result <- tryCatch(
      uniroot(objective, interval = bounds, tol = tol),
      error = function(e) {
        warning(sprintf('solve_group_employment_shift: uniroot failed for group %s: %s', group, e$message))
        list(root = 0)
      }
    )

    result$root
  }



  get_total_demand <- function(P, parent_units_df, n_children, demand_params = NULL,
                               pu_name = NULL, policy_demand = NULL, policy_cdctc = NULL,
                               cpi_growth_factor = 1.0, employment_shifts = NULL) {

    #--------------------------------------------------------------------------
    # Aggregates demand by market sector using Qd = colSums(W * (p %*% H)).
    # Returns total demand hours for the 4 market sectors.
    #
    # Params:
    #   - P (num vec): Price vector (length 4)
    #   - parent_units_df (df): Parent units dataframe for a single type
    #   - n_children (int): Number of children (1 or 2)
    #   - demand_params (list): Demand model parameters by parent unit type
    #   - pu_name (chr): Parent unit type name (e.g. 'c1_p1')
    #   - policy_demand (fn): Demand policy function (or NULL)
    #   - policy_cdctc (fn): CDCTC policy function (or NULL)
    #   - cpi_growth_factor (dbl): CPI growth factor for price scaling
    #   - employment_shifts (num vec): Named vector of shifts by group (or NULL)
    #
    # Returns: (num vec) Demand hours by sector (length 4)
    #--------------------------------------------------------------------------

    # Get precomputed sector-hours matrices
    sector_hours <- get_sector_hours_matrices(n_children)
    n_choices <- nrow(sector_hours$H1)

    # Compute probability matrix directly
    p_matrix <- get_demand_prob_matrix(P, parent_units_df, n_children, demand_params,
                                       pu_name, policy_demand, policy_cdctc,
                                       cpi_growth_factor = cpi_growth_factor,
                                       employment_shifts = employment_shifts)

    # Child 1: probability * hours, weighted by child_weight.1, summed by sector
    W1 <- parent_units_df[['child_weight.1']]
    if (any(!is.finite(W1))) {
      stop('get_total_demand: non-finite child_weight.1 encountered (', pu_name, ').')
    }
    Qd <- colSums(W1 * (p_matrix %*% sector_hours$H1))

    # Child 2 (if applicable)
    if (n_children == 2) {
      W2 <- parent_units_df[['child_weight.2']]
      if (any(!is.finite(W2))) {
        stop('get_total_demand: non-finite child_weight.2 encountered (', pu_name, ').')
      }
      Qd <- Qd + colSums(W2 * (p_matrix %*% sector_hours$H2))
    }

    return(Qd)
  }



  equilibrium_objective <- function(P3, year = NA_integer_, supply_params, demand_params, parent_units_list,
                                    policy_supply = identity,
                                    policy_demand = NULL, policy_cdctc = NULL,
                                    target_employment_rates = NULL,
                                    baseline_employment_shifts = NULL) {

    #--------------------------------------------------------------------------
    # Objective for L-BFGS-B equilibrium solver: sum of squared supply-demand
    # delta gaps on the 3 paid sectors. Solves employment shifts at each
    # evaluation (baseline) or uses fixed shifts (policy).
    #
    # Params:
    #   - P3 (num vec): Prices for 3 paid sectors (P[2:4])
    #   - year (int): Simulation year (for error messages)
    #   - supply_params (list): Supply model parameters
    #   - demand_params (list): Demand model parameters by parent unit type
    #   - parent_units_list (list): Named list of parent unit dataframes
    #   - policy_supply (fn): Supply policy function
    #   - policy_demand (fn): Demand policy function (or NULL)
    #   - policy_cdctc (fn): CDCTC policy function (or NULL)
    #   - target_employment_rates (num vec): Target rates by group (or NULL)
    #   - baseline_employment_shifts (num vec): Fixed shifts for policy runs
    #
    # Returns: (dbl) Sum of squared delta gaps (scalar objective value)
    #--------------------------------------------------------------------------

    # Reconstruct full price vector (P[1] = 0 for unpaid sector)
    P <- c(0, P3)

    stage <- 'start'

    tryCatch({
      # Compute employment shifts if targeting is enabled
      # For baseline: solve for shifts at current P (nested optimization)
      # For policy: use fixed baseline_employment_shifts
      employment_shifts <- NULL
      if (!is.null(target_employment_rates)) {
        if (!is.null(baseline_employment_shifts)) {
          employment_shifts <- baseline_employment_shifts
        } else {
          stage <- 'employment_targeting'
          employment_shifts <- solve_employment_shifts(P, policy_demand, policy_cdctc)
        }
      }

      # Compute quantity demanded (sum over all 6 parent unit types)
      Qd <- rep(0, 4)
      for (i in seq_along(PARENT_UNIT_NAMES)) {
        pu_name <- PARENT_UNIT_NAMES[i]
        n_children <- PARENT_UNIT_N_CHILDREN[i]
        if (nrow(parent_units_list[[pu_name]]) > 0) {
          stage <- paste0('demand:', pu_name)
          Qd_i <- get_total_demand(
            P, parent_units_list[[pu_name]], n_children,
            demand_params, pu_name, policy_demand, policy_cdctc,
            cpi_growth_factor = supply_params$cpi_factor,
            employment_shifts = employment_shifts
          )
          if (any(!is.finite(Qd_i)) || any(Qd_i < 0)) {
            stop('Non-finite or negative Qd returned by get_total_demand().')
          }
          Qd <- Qd + Qd_i
        }
      }

      stage <- 'aggregate:Qd'
      if (any(!is.finite(Qd)) || any(Qd < 0)) {
        stop('Aggregated Qd is non-finite or negative.')
      }

      # Compute supply-side residual revenues (applies supply policy internally)
      stage <- 'supply:residuals'
      supply_result <- compute_supply_residuals(P, Qd, supply_params, policy_supply)
      delta_gap_paid <- supply_result$delta_gap[2:4]

      if (any(!is.finite(delta_gap_paid))) {
        stop('Supply residuals produced non-finite delta_gap.')
      }

      stage <- 'objective:sum_squares'
      value <- sum(delta_gap_paid^2)
      if (!is.finite(value)) {
        stop('Objective value is non-finite (sum of squared delta gaps).')
      }

      value
    }, error = function(e) {
      stop(
        paste0(
          'equilibrium_objective failed (year=', year,
          ', P=[', paste(signif(P, 8), collapse = ', '), ']',
          ', stage=', stage, '): ',
          conditionMessage(e)
        ),
        call. = FALSE
      )
    })
  }


  #============================================================================
  # Main body of run_scenario()
  #============================================================================

  # Load policy functions (isolated environments to avoid global namespace pollution)
  do_demand_policy <- load_demand_policy(scenario_info$equilibrium$policy_demand)
  supply_policy    <- load_supply_policy(scenario_info$equilibrium$policy_supply)
  do_supply_policy <- supply_policy$do_supply_policy
  supply_subsidy   <- supply_policy$supply_subsidy
  do_cdctc_policy  <- load_cdctc_policy(scenario_info$policy_cdctc %||% 'baseline')
  do_tax_policy    <- load_policy('policy_tax', scenario_info$policy_tax %||% 'baseline', 'do_tax_policy')

  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    n_children_eff <- PARENT_UNIT_N_CHILDREN[i]
    pu_df <- parent_units_list[[pu_name]]
    if (is.null(pu_df)) {
      stop(
        'simulate_single_year: parent_units_list is missing "', pu_name, '". ',
        'This likely indicates an upstream data build issue.'
      )
    }
    parent_units_list[[pu_name]] <- do_tax_policy(
      pu_df,
      n_children = n_children_eff,
      year = year
    )
  }

  # Apply wage floor config
  if (isTRUE(scenario_info$wage_floor_config$enabled)) {
    supply_params$wage_floor <- scenario_info$wage_floor_config$floor
  }

  # Apply employer subsidy config
  if (isTRUE(scenario_info$employer_subsidy_config$enabled)) {
    config <- scenario_info$employer_subsidy_config
    if (config$type == 'percentage') {
      # Percentage-based: subsidy is a fraction of equilibrium wages
      supply_params$employer_subsidy_pct <- config$rate
      supply_params$employer_subsidy_sectors <- config$covered_sectors
    } else {
      # Dollar-based: convert from base-year to current-year dollars
      subsidy_base_year <- config$base_year %||% 2019
      if (is.null(macro_projections) && subsidy_base_year != 2019) {
        stop(paste0(
          'Employer subsidy scaling requires macro_projections when base_year != 2019. ',
          'Got base_year=', subsidy_base_year, ' for scenario ', scenario_info$id, '.'
        ))
      }
      subsidy_growth_factor <- if (is.null(macro_projections)) {
        supply_params$nominal_wage_factor
      } else {
        get_hourly_wage_growth_factor(macro_projections, subsidy_base_year, year)
      }
      supply_params$employer_subsidy <- config$subsidy * subsidy_growth_factor
      supply_params$employer_subsidy_sectors <- config$covered_sectors
    }
  }

  # Write scenario supply parameters to YAML file for initial state
  {
    supply_dir <- file.path(scenario_info$paths$output, 'supply')
    dir.create(supply_dir, recursive = T, showWarnings = F)

    {
      sectors <- c('unpaid_center_based', 'low_price_center_based',
                   'high_price_center_based', 'paid_home_based')

      labor_requirements <- list()
      per_unit_residual <- list()
      for (i_sp in seq_along(sectors)) {
        labor_requirements[[sectors[i_sp]]] <- list(
          no_ba = supply_params$L_req[i_sp, 1],
          ba    = supply_params$L_req[i_sp, 2]
        )
        per_unit_residual[[sectors[i_sp]]] <- supply_params$delta_0[i_sp]
      }

      yaml_data <- list(
        labor_requirements = labor_requirements,
        wages = list(no_ba = supply_params$w[1], ba = supply_params$w[2]),
        labor_supply = list(no_ba = supply_params$L[1], ba = supply_params$L[2]),
        elasticities = list(no_ba = supply_params$e[1], ba = supply_params$e[2]),
        per_unit_residual = per_unit_residual
      )
    }

    yaml_data$year <- year

    supply_dir %>%
      file.path(paste0('supply_', year, '.yaml')) %>%
      write_yaml(yaml_data, .)
  }

  # Solve for equilibrium prices using optimization
  {
    # Default initial prices if not provided (from constants.R)
    eq_initial_prices <- initial_prices
    if (is.null(eq_initial_prices)) {
      eq_initial_prices <- INITIAL_PRICES
    }

    # Optimize only over P[2:4] (3 paid sectors)
    # P[1] (Unpaid Center-Based) is structurally fixed at 0
    optim_result <- optim(
      par               = eq_initial_prices[2:4],
      fn                = equilibrium_objective,
      method            = 'L-BFGS-B',
      lower             = rep(PRICE_LOWER_BOUND, 3),
      upper             = rep(PRICE_UPPER_BOUND, 3),
      control           = list(factr = OPTIMIZER_TOLERANCE),
      year              = year,
      supply_params     = supply_params,
      demand_params     = demand_params,
      parent_units_list = parent_units_list,
      policy_supply     = do_supply_policy,
      policy_demand     = do_demand_policy,
      policy_cdctc      = do_cdctc_policy,
      target_employment_rates = target_employment_rates,
      baseline_employment_shifts = baseline_employment_shifts
    )

    # Check both optimizer convergence AND residual gap quality
    converged_optimizer <- optim_result$convergence == 0
    residual_acceptable <- FALSE
    max_abs_delta_gap_paid <- NA_real_

    # Also check if objective is near-zero (can happen if we start at the solution)
    # L-BFGS-B may return non-zero codes (e.g., 52 = ABNORMAL_TERMINATION_IN_LNSRCH)
    # when starting at the solution because line search can't find an improving direction
    OBJECTIVE_NEAR_ZERO_THRESHOLD <- 1e-10
    objective_near_zero <- is.finite(optim_result$value) && optim_result$value < OBJECTIVE_NEAR_ZERO_THRESHOLD

    # Compute residual gap quality at the candidate solution.
    # We use the per-hour gap in the equilibrium condition, which is directly
    # interpretable in $/hour.
    # Check residuals if optimizer converged OR if objective is near-zero
    P_candidate <- NULL
    Qd_candidate <- NULL
    supply_candidate <- NULL

    # Track employment shifts for return value
    final_employment_shifts <- NULL

    if (converged_optimizer || objective_near_zero) {
      # Prepend 0 for unpaid sector
      P_candidate <- c(0, optim_result$par)

      # Compute employment shifts at final prices (if targeting enabled)
      if (!is.null(target_employment_rates)) {
        if (!is.null(baseline_employment_shifts)) {
          final_employment_shifts <- baseline_employment_shifts
        } else {
          final_employment_shifts <- solve_employment_shifts(P_candidate, do_demand_policy, do_cdctc_policy)
        }
      }

      # Compute demand at candidate prices WITH employment shifts
      # (This must match what the optimizer was doing)
      Qd_candidate <- rep(0, 4)
      for (i in seq_along(PARENT_UNIT_NAMES)) {
        pu_name <- PARENT_UNIT_NAMES[i]
        n_children <- PARENT_UNIT_N_CHILDREN[i]
        if (nrow(parent_units_list[[pu_name]]) > 0) {
          Qd_candidate <- Qd_candidate + get_total_demand(P_candidate, parent_units_list[[pu_name]], n_children,
                                                          demand_params, pu_name, do_demand_policy, do_cdctc_policy,
                                                          cpi_growth_factor = supply_params$cpi_factor,
                                                          employment_shifts = final_employment_shifts)
        }
      }

      # Compute supply-side residual gaps at candidate solution
      supply_candidate <- compute_supply_residuals(P_candidate, Qd_candidate, supply_params, do_supply_policy)
      delta_gap_paid <- supply_candidate$delta_gap[2:4]
      max_abs_delta_gap_paid <- max(abs(delta_gap_paid))

      residual_acceptable <- is.finite(max_abs_delta_gap_paid) &&
                             max_abs_delta_gap_paid <= EQUILIBRIUM_MAX_ABS_DELTA_GAP
    }

    # True convergence requires residual_acceptable AND either:
    #   - optimizer converged (code 0), OR
    #   - objective is near-zero (started at solution)
    solution_found <- (converged_optimizer || objective_near_zero) && residual_acceptable

    if (solution_found) {

      # Equilibrium successfully solved (details in solver diagnostics file)

      P_eq <- P_candidate
      Qd_final <- Qd_candidate
      supply_final <- supply_candidate

      # Update parent units with equilibrium utilities (including employment shifts)
      # (collapse_to_discrete() will take argmax downstream)
      for (i in seq_along(PARENT_UNIT_NAMES)) {
        pu_name <- PARENT_UNIT_NAMES[i]
        n_children <- PARENT_UNIT_N_CHILDREN[i]
        if (nrow(parent_units_list[[pu_name]]) > 0) {

          # Compute demand using alpha-based structural model
          {
            catalog <- get_choice_catalog(n_children)
            n_units <- nrow(parent_units_list[[pu_name]])

            # Remove any existing component columns to avoid duplicates
            parent_units_clean <- parent_units_list[[pu_name]] %>%
              select(-matches('^(agi|taxes|gross_ecec_cost|subsidy|cdctc|net_income|Y|C|V|p)\\.[0-9]+$'))

            pc <- compute_policy_components(parent_units_clean, P_eq, n_children, demand_params,
                                             do_demand_policy, do_cdctc_policy, supply_params$cpi_factor)
            df_with_policy <- bind_cols(parent_units_clean, pc$components)

            n_choices <- if (n_children == 1) N_CHOICES_1_CHILD else N_CHOICES_2_CHILD

            type_params <- demand_params[[pu_name]]

            if (is.null(type_params$beta)) {
              stop(paste0(
                'Missing demand parameters for type ', pu_name, '. ',
                'Required: beta and rho'
              ))
            }

            beta <- type_params$beta
            rho <- type_params$rho

            if (is.null(type_params$alpha)) {
              stop('get_demand: alpha matrix not found for ', pu_name, '. ',
                   'Re-run calibration to generate alpha matrices.')
            }

            # Extract policy Y (pre-care income) and C (childcare cost)
            y_cols <- paste0('Y.', 1:n_choices)
            c_cols <- paste0('C.', 1:n_choices)
            Y_policy <- as.matrix(df_with_policy[, y_cols])
            C_policy <- as.matrix(df_with_policy[, c_cols])

            # Net income: NI = Y - C
            NI_policy <- Y_policy - C_policy

            # Compute V = beta * u(NI) in REAL 2019 dollars
            cpi_factor_2019 <- parent_units_list[[pu_name]][['cpi_factor_2019']]
            if (is.null(cpi_factor_2019)) {
              cpi_factor_2019 <- 1.0
            } else {
              cpi_factor_2019 <- unique(cpi_factor_2019)
              if (length(cpi_factor_2019) != 1 || !is.finite(cpi_factor_2019) || cpi_factor_2019 <= 0) {
                stop('get_demand: invalid cpi_factor_2019 (must be a single positive finite value) for ', pu_name, '.')
              }
            }

            NI_real_2019 <- NI_policy / cpi_factor_2019
            V_utility <- compute_V_crra(NI_real_2019, beta, rho)

            # Apply employment shifts if provided (for employment targeting)
            if (!is.null(final_employment_shifts) && any(final_employment_shifts != 0)) {
              working_mask <- get_working_choice_mask(n_children)
              pc_group <- parent_units_list[[pu_name]]$pc_group

              for (g in names(final_employment_shifts)) {
                if (final_employment_shifts[g] != 0) {
                  group_rows <- which(pc_group == g)
                  if (length(group_rows) > 0) {
                    V_utility[group_rows, working_mask] <- V_utility[group_rows, working_mask] + final_employment_shifts[g]
                  }
                }
              }
            }

            # Match alpha matrix rows to parent_units_df rows
            {
              alpha <- type_params$alpha
              row_ids <- type_params$row_ids
              n_rows <- nrow(parent_units_list[[pu_name]])

              has_pseudofamily <- 'pseudofamily_id' %in% names(parent_units_list[[pu_name]])

              if (has_pseudofamily) {
                pu_keys <- parent_units_list[[pu_name]] %>%
                  select(hh_id, parent_unit_id, pseudofamily_id) %>%
                  mutate(row_idx = row_number())
              } else {
                pu_keys <- parent_units_list[[pu_name]] %>%
                  select(hh_id, parent_unit_id) %>%
                  mutate(pseudofamily_id = 1L, row_idx = row_number())
              }

              row_ids_tbl <- tibble(row_ids) %>%
                mutate(alpha_idx = row_number())

              if (!('pseudofamily_id' %in% names(row_ids_tbl))) {
                row_ids_tbl$pseudofamily_id <- 1L
              }

              pu_keys <- pu_keys %>%
                mutate(
                  hh_id = as.character(hh_id),
                  parent_unit_id = as.character(parent_unit_id),
                  pseudofamily_id = as.character(pseudofamily_id)
                )
              row_ids_tbl <- row_ids_tbl %>%
                mutate(
                  hh_id = as.character(hh_id),
                  parent_unit_id = as.character(parent_unit_id),
                  pseudofamily_id = as.character(pseudofamily_id)
                )

              dup_alpha <- row_ids_tbl %>%
                count(hh_id, parent_unit_id, pseudofamily_id) %>%
                filter(n > 1)
              if (nrow(dup_alpha) > 0) {
                stop('match_alpha_to_parent_units: alpha row_ids contain duplicate keys. ',
                     'This would create an ambiguous match.')
              }
              dup_pu <- pu_keys %>%
                count(hh_id, parent_unit_id, pseudofamily_id) %>%
                filter(n > 1)
              if (nrow(dup_pu) > 0) {
                stop('match_alpha_to_parent_units: parent_units_df contains duplicate keys. ',
                     'This indicates duplicated rows for (hh_id, parent_unit_id, pseudofamily_id).')
              }

              matched <- pu_keys %>%
                left_join(row_ids_tbl, by = c('hh_id', 'parent_unit_id', 'pseudofamily_id'))

              n_unmatched <- sum(is.na(matched$alpha_idx))
              if (n_unmatched > 0) {
                stop(sprintf(
                  'match_alpha_to_parent_units: %d of %d rows have no matching alpha. ',
                  n_unmatched, n_rows
                ), 'Check that calibration data covers all simulation households.')
              }

              alpha_matrix <- alpha[matched$alpha_idx, , drop = FALSE]
            }

            # Compute probabilities: p = exp(alpha + V) / Z
            p_matrix <- compute_probs_from_alpha(alpha_matrix, V_utility)

            # -- Assertions: probability validity ----
            # Rows must sum to 1 (catches bugs in softmax normalization or
            # alpha matrix misalignment where wrong rows get matched)
            stopifnot(all(abs(rowSums(p_matrix) - 1.0) < 1e-10))

            # Probabilities should vary across households (if all rows identical,
            # alpha matching may have assigned the same alpha to everyone)
            if (nrow(p_matrix) > 1) {
              stopifnot(sd(p_matrix[, 1]) > 0)
            }

            p_df <- as_tibble(p_matrix, .name_repair = 'minimal')
            names(p_df) <- paste0('p.', 1:n_choices)

            # For discrete choice simulation with Gumbel shocks:
            # argmax(V + epsilon) has same distribution as drawing from p
            # when V = log(p) (up to an additive constant)
            V_log_matrix <- log(pmax(p_matrix, LOG_PROB_FLOOR))

            V_df <- as_tibble(V_log_matrix, .name_repair = 'minimal')
            names(V_df) <- paste0('V.', 1:n_choices)

            parent_units_list[[pu_name]] <- bind_cols(df_with_policy, V_df, p_df)
          }
        }
      }

    } else if ((converged_optimizer || objective_near_zero) && !residual_acceptable) {
      # Optimizer converged but equilibrium not satisfied
      cat('  WARNING: Equilibrium not satisfied (see solver diagnostics)\n')

      P_eq         <- NULL
      Qd_final     <- NULL
      for (pu_name in PARENT_UNIT_NAMES) {
        parent_units_list[[pu_name]] <- NULL
      }
      supply_final <- list(w = NULL, L = NULL, actual_delta = NULL, target_delta = NULL)

    } else {
      # Optimizer failed to converge
      cat('  WARNING: Optimization failed (code:', optim_result$convergence, ')\n')

      P_eq         <- NULL
      Qd_final     <- NULL
      for (pu_name in PARENT_UNIT_NAMES) {
        parent_units_list[[pu_name]] <- NULL
      }
      supply_final <- list(w = NULL, L = NULL, actual_delta = NULL, target_delta = NULL)
    }

    result_list <- list(
      prices       = P_eq,
      parent_units = parent_units_list,
      Qd           = Qd_final,
      w            = supply_final$w,
      L            = supply_final$L,
      actual_delta = supply_final$actual_delta,
      target_delta = supply_final$target_delta,
      converged    = solution_found,
      max_abs_delta_gap_paid = max_abs_delta_gap_paid,
      value        = optim_result$value,
      optim_result = optim_result,
      employment_shifts = final_employment_shifts,
      target_employment_rates = target_employment_rates
    )

    # Write solver diagnostics
    {
      output_dir <- file.path(scenario_info$paths$output, 'models', 'equilibrium')
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

      output_file <- file.path(output_dir, paste0('solver_results_', year, '.txt'))

      sink(output_file)

      cat('================================================================================\n')
      cat('EQUILIBRIUM SOLVER DIAGNOSTICS\n')
      cat('================================================================================\n')
      cat('Year:', year, '\n')
      cat('Timestamp:', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
      cat('Method:', 'L-BFGS-B', '\n')
      cat('Nominal wage factor:', round(supply_params$nominal_wage_factor, 4), '\n')
      cat('CPI factor:', round(supply_params$cpi_factor, 4), '\n')
      cat('\n')

      # Data summary
      cat('--------------------------------------------------------------------------------\n')
      cat('DATA SUMMARY\n')
      cat('--------------------------------------------------------------------------------\n')
      total_units <- 0
      for (i_diag in seq_along(PARENT_UNIT_NAMES)) {
        pu_name_diag <- PARENT_UNIT_NAMES[i_diag]
        pu_df_diag <- parent_units_list[[pu_name_diag]]
        n_rows_diag <- if (!is.null(pu_df_diag)) nrow(pu_df_diag) else 0
        cat(sprintf('%-12s: %d\n', pu_name_diag, n_rows_diag))
        total_units <- total_units + n_rows_diag
      }
      cat('Total parent units:', total_units, '\n')
      cat('\n')

      # Convergence status
      cat('--------------------------------------------------------------------------------\n')
      cat('CONVERGENCE STATUS\n')
      cat('--------------------------------------------------------------------------------\n')
      cat('Converged:', result_list$converged, '\n')
      cat('Final objective (SSE of delta_gap, paid sectors):', sprintf('%.6e', result_list$value), '\n')
      if (is.finite(result_list$max_abs_delta_gap_paid)) {
        cat('Max |delta_gap| (paid sectors): $', sprintf('%.6f', result_list$max_abs_delta_gap_paid),
            '/hour (tolerance $', sprintf('%.2f', EQUILIBRIUM_MAX_ABS_DELTA_GAP), '/hour)\n', sep = '')
      }

      if (!is.null(result_list$optim_result)) {
        cat('Convergence code:', result_list$optim_result$convergence, '\n')
        if (!is.null(result_list$optim_result$message)) {
          cat('Message:', result_list$optim_result$message, '\n')
        }
        cat('Function evaluations:', result_list$optim_result$counts['function'], '\n')
        cat('Gradient evaluations:', result_list$optim_result$counts['gradient'], '\n')
      }
      cat('\n')

      category_names <- c('Unpaid Center-Based', 'Low-Priced Center-Based',
                          'High-Priced Center-Based', 'Paid Home-Based')
      labor_names <- c('No BA', 'BA+')

      print_section <- function(title, data, null_msg, header_fmt, row_fmt, sep_width,
                                row_data_fn, footer_fn = NULL) {

        #----------------------------------------------------------------------
        # Prints a titled diagnostic table section to the sink output.
        # Handles NULL data with a fallback message.
        #
        # Params:
        #   - title (chr): Section title
        #   - data (any): Data to display (NULL triggers null_msg)
        #   - null_msg (chr): Message when data is NULL
        #   - header_fmt (list): sprintf format args for header row
        #   - row_fmt (any): Unused (kept for interface consistency)
        #   - sep_width (int): Width of separator line
        #   - row_data_fn (fn): Function(i) returning sprintf args for row i
        #   - footer_fn (fn): Optional footer function (or NULL)
        #
        # Returns: nothing (side effects only)
        #----------------------------------------------------------------------

        cat('--------------------------------------------------------------------------------\n')
        cat(title, '\n')
        cat('--------------------------------------------------------------------------------\n')
        if (is.null(data)) {
          cat(null_msg, '\n')
        } else {
          cat(do.call(sprintf, header_fmt), '\n')
          cat(strrep('-', sep_width), '\n')
          for (i_sec in seq_along(data)) {
            cat(do.call(sprintf, row_data_fn(i_sec)), '\n')
          }
          if (!is.null(footer_fn)) footer_fn()
        }
        cat('\n')
      }

      final_prices <- result_list$prices
      if (is.null(final_prices)) final_prices <- eq_initial_prices

      print_section('PRICES (Market)', final_prices, 'Prices not available',
        header_fmt   = list('%-30s %12s %12s %12s', 'Category', 'Initial', 'Final', 'Change'),
        row_fmt      = NULL, sep_width = 70,
        row_data_fn  = function(i_sec) list('%-30s %12.4f %12.4f %12.4f',
          category_names[i_sec], eq_initial_prices[i_sec], final_prices[i_sec],
          final_prices[i_sec] - eq_initial_prices[i_sec]))

      print_section('WAGES', result_list$w, 'Wages not available (solver did not converge)',
        header_fmt   = list('%-20s %12s %12s', 'Labor Type', 'NSECE 2019', 'Equilibrium'),
        row_fmt      = NULL, sep_width = 50,
        row_data_fn  = function(i_sec) list('%-20s %12.4f %12.4f',
          labor_names[i_sec], supply_params$w[i_sec], result_list$w[i_sec]))

      print_section('QUANTITIES DEMANDED', result_list$Qd, 'Quantities not available (solver did not converge)',
        header_fmt   = list('%-30s %20s', 'Category', 'Qd (hours)'),
        row_fmt      = NULL, sep_width = 55,
        row_data_fn  = function(i_sec) list('%-30s %20s',
          category_names[i_sec], format(round(result_list$Qd[i_sec]), big.mark = ',')),
        footer_fn    = function() cat(sprintf('%-30s %20s\n', 'Total',
          format(round(sum(result_list$Qd)), big.mark = ','))))

      print_section('PER-UNIT RESIDUAL REVENUE (Equilibrium Condition)',
        result_list$actual_delta, 'Residual revenue not available (solver did not converge)',
        header_fmt   = list('%-30s %15s %15s %15s', 'Category', 'Actual', 'Target', 'Gap'),
        row_fmt      = NULL, sep_width = 80,
        row_data_fn  = function(i_sec) list('%-30s %15.4f %15.4f %15.4f',
          category_names[i_sec], result_list$actual_delta[i_sec], result_list$target_delta[i_sec],
          result_list$actual_delta[i_sec] - result_list$target_delta[i_sec]))

      print_section('LABOR DEMAND', result_list$L, 'Labor demand not available (solver did not converge)',
        header_fmt   = list('%-20s %20s %20s', 'Labor Type', 'NSECE 2019', 'Equilibrium'),
        row_fmt      = NULL, sep_width = 65,
        row_data_fn  = function(i_sec) list('%-20s %20s %20s',
          labor_names[i_sec], format(round(supply_params$L[i_sec]), big.mark = ','),
          format(round(result_list$L[i_sec]), big.mark = ',')))

      # Solution quality
      cat('--------------------------------------------------------------------------------\n')
      cat('SOLUTION QUALITY\n')
      cat('--------------------------------------------------------------------------------\n')

      if (isTRUE(result_list$converged)) {
        cat('PASS: max |delta_gap| <= $', sprintf('%.2f', EQUILIBRIUM_MAX_ABS_DELTA_GAP), '/hour\n', sep = '')
      } else if (is.finite(result_list$max_abs_delta_gap_paid)) {
        cat('FAILED: max |delta_gap| = $', sprintf('%.4f', result_list$max_abs_delta_gap_paid),
            '/hour (tolerance $', sprintf('%.2f', EQUILIBRIUM_MAX_ABS_DELTA_GAP), '/hour)\n', sep = '')
      } else {
        cat('FAILED: Solver did not converge\n')
      }

      cat('\n')
      cat('================================================================================\n')

      sink()
    }

    result <- result_list
  }

  result$supply_subsidy <- supply_subsidy

  if (isTRUE(scenario_info$employer_subsidy_config$enabled)) {
    if (!is.null(supply_params$employer_subsidy_pct)) {
      # Percentage-based: compute dollar equivalent from equilibrium wages
      pct <- supply_params$employer_subsidy_pct
      result$employer_subsidy <- result$w * pct
      result$employer_subsidy_sectors <- supply_params$employer_subsidy_sectors
      result$L_req <- supply_params$L_req
    } else {
      result$employer_subsidy <- supply_params$employer_subsidy
      result$employer_subsidy_sectors <- supply_params$employer_subsidy_sectors
      result$L_req <- supply_params$L_req
    }
  }

  if (result$converged) {
    # Expand with Gumbel epsilon shocks then collapse to discrete choices
    for (i in seq_along(PARENT_UNIT_NAMES)) {
      pu_name <- PARENT_UNIT_NAMES[i]
      n_children <- PARENT_UNIT_N_CHILDREN[i]

      if (!is.null(result$parent_units[[pu_name]]) &&
          nrow(result$parent_units[[pu_name]]) > 0) {

        # Same seed for baseline and counterfactual ensures clean comparisons
        seed_for_pu <- if (!is.null(year_seed)) year_seed + i else NULL

        # Defragment heap before large Gumbel expansion allocations
        gc()

        # Expand dataframe with Gumbel-distributed epsilon shocks
        {
          df_expand <- result$parent_units[[pu_name]]

          if (nrow(df_expand) > 0) {
            n_choices <- if (n_children == 1) N_CHOICES_1_CHILD else N_CHOICES_2_CHILD
            n_units <- nrow(df_expand)

            # Set seed if provided (for reproducibility within year)
            if (!is.null(seed_for_pu)) {
              set.seed(seed_for_pu)
            }

            # Expand dataframe by n_draws_per_record
            df_expanded <- df_expand %>%
              slice(rep(1:n(), each = n_draws_per_record)) %>%
              mutate(epsilon_id = rep(1:n_draws_per_record, times = n_units)) %>%
              relocate(epsilon_id, .after = parent_unit_id)

            # Draw Gumbel(0,1) shocks column-by-column to avoid large contiguous
            # allocation (full matrix can exceed 1.9GB for c2plus with 10 draws)
            n_total <- nrow(df_expanded)
            for (eps_j in seq_len(n_choices)) {
              df_expanded[[paste0('epsilon.', eps_j)]] <- -log(-log(runif(n_total)))
            }

            # Adjust ALL weights to preserve aggregates (divide by n_draws_per_record)
            df_expanded <- df_expanded %>%
              mutate(
                child_weight.1 = child_weight.1 / n_draws_per_record
              )

            if ('child_weight.2' %in% names(df_expanded)) {
              df_expanded <- df_expanded %>%
                mutate(child_weight.2 = child_weight.2 / n_draws_per_record)
            }

            if ('per_weight1' %in% names(df_expanded)) {
              df_expanded <- df_expanded %>%
                mutate(per_weight1 = per_weight1 / n_draws_per_record)
            }

            if ('per_weight2' %in% names(df_expanded)) {
              df_expanded <- df_expanded %>%
                mutate(per_weight2 = per_weight2 / n_draws_per_record)
            }

            if ('per_weight_pc' %in% names(df_expanded)) {
              df_expanded <- df_expanded %>%
                mutate(per_weight_pc = per_weight_pc / n_draws_per_record)
            }

            # -- Assertions: weight conservation through Gumbel expansion ----
            # Total weight must be preserved (each record is replicated n_draws
            # times with weight divided by n_draws — the sum should be unchanged).
            # Catches: wrong division order, dividing some weight cols but not
            # others, or accidentally dividing twice.
            original_total_w1 <- sum(df_expand$child_weight.1)
            expanded_total_w1 <- sum(df_expanded$child_weight.1)
            stopifnot(abs(original_total_w1 - expanded_total_w1) / max(original_total_w1, 1) < 1e-6)

            if ('child_weight.2' %in% names(df_expanded)) {
              original_total_w2 <- sum(df_expand$child_weight.2)
              expanded_total_w2 <- sum(df_expanded$child_weight.2)
              stopifnot(abs(original_total_w2 - expanded_total_w2) / max(original_total_w2, 1) < 1e-6)
            }

            result$parent_units[[pu_name]] <- df_expanded
            rm(df_expand, df_expanded)
          }
        }

        # Collapse wide-format choice data to discrete scalar values
        {
          parent_units_df <- result$parent_units[[pu_name]]
          n_choices <- if (n_children == 1) N_CHOICES_1_CHILD else N_CHOICES_2_CHILD

          # Step 1: Compute choice as argmax of V.* + epsilon.* (stochastic discrete choice)
          # Process column-by-column to avoid large contiguous matrix allocations
          n_rows <- nrow(parent_units_df)
          best_util <- rep(-Inf, n_rows)
          choices <- integer(n_rows)
          for (col_j in seq_len(n_choices)) {
            util_j <- parent_units_df[[paste0('V.', col_j)]] +
                       parent_units_df[[paste0('epsilon.', col_j)]]
            better <- util_j > best_util
            best_util[better] <- util_j[better]
            choices[better] <- col_j
          }
          rm(best_util, util_j, better)

          # Decode choice indices to human-readable columns
          {
            catalog <- get_choice_catalog(n_children)

            decoded <- tibble(
              employment_choice = catalog$employment_choice[choices],
              ecec_type.1 = catalog$child1_ecec_type[choices],
              ecec_hours.1 = catalog$child1_hours_choice[choices]
            )

            if (n_children == 2) {
              decoded <- decoded %>%
                mutate(
                  ecec_type.2 = catalog$child2_ecec_type[choices],
                  ecec_hours.2 = catalog$child2_hours_choice[choices]
                )
            }
          }

          # Step 3: Extract scalar values for selected choice
          agi <- extract_selected_values(parent_units_df, choices, 'agi', n_choices)
          taxes <- extract_selected_values(parent_units_df, choices, 'taxes', n_choices)
          gross_ecec_cost <- extract_selected_values(parent_units_df, choices, 'gross_ecec_cost', n_choices)
          subsidy <- extract_selected_values(parent_units_df, choices, 'subsidy', n_choices)
          cdctc <- extract_selected_values(parent_units_df, choices, 'cdctc', n_choices)
          net_income <- extract_selected_values(parent_units_df, choices, 'net_income', n_choices)

          # -- Assertions: extraction correctness ----
          # Accounting identity must survive extraction (catches extracting
          # from the wrong column prefix, or choices indexing the wrong row)
          stopifnot(all(abs(net_income - (agi - taxes - gross_ecec_cost + subsidy + cdctc)) < 1e-4))

          # Step 4: Identify columns to keep (non-wide columns)
          wide_prefixes <- c('u', 'p', 'V', 'epsilon', 'agi', 'taxes', 'gross_ecec_cost', 'subsidy', 'cdctc', 'net_income', 'Y', 'C')
          wide_pattern <- paste0('^(', paste(wide_prefixes, collapse = '|'), ')\\.\\d+$')

          cols_to_keep <- names(parent_units_df)[!grepl(wide_pattern, names(parent_units_df))]

          # Build result dataframe
          collapsed <- parent_units_df %>%
            select(all_of(cols_to_keep)) %>%
            mutate(
              choice            = choices,
              agi               = agi,
              taxes             = taxes,
              gross_ecec_cost   = gross_ecec_cost,
              subsidy           = subsidy,
              cdctc             = cdctc,
              net_income        = net_income,
              employment_choice = decoded$employment_choice,
              ecec_type.1       = decoded$ecec_type.1,
              ecec_hours.1      = decoded$ecec_hours.1
            )

          # Add child 2 columns if applicable
          if (n_children == 2) {
            collapsed <- collapsed %>%
              mutate(
                ecec_type.2  = decoded$ecec_type.2,
                ecec_hours.2 = decoded$ecec_hours.2
              )
          }

          result$parent_units[[pu_name]] <- collapsed
          rm(parent_units_df, collapsed, decoded)
        }

        # Compact heap between parent unit types to reduce fragmentation
        gc()
      }
    }

    # Write supply parameters with equilibrium prices
    {
      supply_dir <- file.path(scenario_info$paths$output, 'supply')
      dir.create(supply_dir, recursive = T, showWarnings = F)

      {
        sectors <- c('unpaid_center_based', 'low_price_center_based',
                     'high_price_center_based', 'paid_home_based')

        labor_requirements <- list()
        per_unit_residual <- list()
        for (i_sp in seq_along(sectors)) {
          labor_requirements[[sectors[i_sp]]] <- list(
            no_ba = supply_params$L_req[i_sp, 1],
            ba    = supply_params$L_req[i_sp, 2]
          )
          per_unit_residual[[sectors[i_sp]]] <- supply_params$delta_0[i_sp]
        }

        yaml_data <- list(
          labor_requirements = labor_requirements,
          wages = list(no_ba = supply_params$w[1], ba = supply_params$w[2]),
          labor_supply = list(no_ba = supply_params$L[1], ba = supply_params$L[2]),
          elasticities = list(no_ba = supply_params$e[1], ba = supply_params$e[2]),
          per_unit_residual = per_unit_residual
        )
      }

      yaml_data$year <- year

      yaml_data$equilibrium_prices <- setNames(
        as.list(result$prices),
        sectors
      )

      supply_dir %>%
        file.path(paste0('supply_', year, '.yaml')) %>%
        write_yaml(yaml_data, .)
    }

    # Write simulation results to disk
    {
      output_dir <- file.path(scenario_info$paths$output, 'data')
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

      for (pu_name in PARENT_UNIT_NAMES) {
        pu_df <- result$parent_units[[pu_name]]
        if (!is.null(pu_df) && nrow(pu_df) > 0) {

          # Remove simulation variables before writing
          {
            cols_to_remove <- intersect(names(pu_df), SIMULATION_VARIABLE_COLS)
            if (length(cols_to_remove) > 0) {
              pu_df <- pu_df %>%
                select(-all_of(cols_to_remove))
            }
          }

          fwrite(
            pu_df,
            file.path(output_dir, paste0('parent_units_', pu_name, '_', year, '.csv')),
            na = 'NA'
          )
        }
      }
    }
  } else {
    # Non-convergence handling
    cat('  ERROR: Equilibrium solver did not converge!\n')
    cat('  Scenario:', scenario_info$id, '\n')
    cat('  Year:', year, '\n')
    cat('  Convergence code:', result$optim_result$convergence, '\n')
    if (!is.null(result$optim_result$message)) {
      cat('  Message:', result$optim_result$message, '\n')
    }
    cat('  Final objective value:', result$value, '\n')
    cat('  Check solver diagnostics at:\n')
    cat('    ', file.path(scenario_info$paths$output, 'models', 'equilibrium',
                          paste0('solver_results_', year, '.txt')), '\n')

    if (fail_on_non_convergence) {
      stop('Equilibrium solver failed to converge. ',
           'Set fail_on_non_convergence = FALSE to continue despite non-convergence, ',
           'or check solver diagnostics for debugging.')
    } else {
      cat('  Continuing despite non-convergence (fail_on_non_convergence = FALSE).\n')
      cat('  Results for this year/scenario will be incomplete.\n')
    }
  }

  return(result)
}
