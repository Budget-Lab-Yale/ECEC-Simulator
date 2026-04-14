#------------------------------------------------------------------------------
# 2a_calibration.R
#
# Demand estimation: donor pool preparation, CRRA calibration, and parameter
# estimation. Primary function: run_calibration()
#
# Called from: main.R (Phase 2)
#------------------------------------------------------------------------------



run_calibration <- function() {

  #----------------------------------------------------------------------------
  # Prepares calibration data and runs demand estimation. Writes calibration
  # data and demand params (alpha, beta, rho) to the estimation folder.
  #
  # Params: none
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  crra_utility <- function(c, rho) {

    #--------------------------------------------------------------------------
    # Computes CRRA (constant relative risk aversion) utility. Values of c
    # below 1 are floored to 1. When rho is approximately 1, returns log(c).
    #
    # Params:
    #   - c (num vec): consumption values
    #   - rho (dbl): risk aversion parameter
    #
    # Returns: (num vec) utility values
    #--------------------------------------------------------------------------

    c <- pmax(c, 1)
    if (abs(rho - 1) < 1e-6) {
      return(log(c))
    }
    (c^(1 - rho)) / (1 - rho)
  }

  compute_V_crra <- function(consumption, beta, rho) {

    #--------------------------------------------------------------------------
    # Computes the value function V = beta * u(consumption) under CRRA utility.
    #
    # Params:
    #   - consumption (matrix): consumption values (n_units x n_choices)
    #   - beta (dbl): utility scaling parameter
    #   - rho (dbl): risk aversion parameter
    #
    # Returns: (matrix) value matrix same dimensions as consumption
    #--------------------------------------------------------------------------

    beta * crra_utility(consumption, rho)
  }


  compute_delta_crra <- function(NI_policy, NI_baseline, beta, rho) {

    #--------------------------------------------------------------------------
    # Computes delta = beta * [u(NI_policy) - u(NI_baseline)] for CRRA
    # utility. Used in tilt model: p1 = p0 * exp(delta) / Z.
    #
    # Params:
    #   - NI_policy (matrix): net income under policy scenario
    #   - NI_baseline (matrix): net income under baseline scenario
    #   - beta (dbl): utility scaling parameter
    #   - rho (dbl): risk aversion parameter
    #
    # Returns: (matrix) delta values for probability tilting
    #--------------------------------------------------------------------------

    beta * (crra_utility(NI_policy, rho) - crra_utility(NI_baseline, rho))
  }



  tilt_probs <- function(p0, delta) {

    #--------------------------------------------------------------------------
    # Tilts baseline probabilities by exp(delta), renormalizing. Computes
    # p1 = p0 * exp(delta) / Z where Z = sum(p0 * exp(delta)).
    #
    # Params:
    #   - p0 (matrix): baseline probability matrix (n_units x n_choices)
    #   - delta (matrix): delta values for tilting (n_units x n_choices)
    #
    # Returns: (matrix) tilted probability matrix (n_units x n_choices)
    #--------------------------------------------------------------------------

    delta_shifted <- delta - apply(delta, 1, max)

    # Apply tilt
    unnorm <- p0 * exp(delta_shifted)
    Z <- rowSums(unnorm)

    unnorm / Z
  }



  build_matrix_by_employment <- function(parent_units_wide, catalog, col_prefix) {

    #--------------------------------------------------------------------------
    # Builds n_units x n_choices matrix mapping {col_prefix}.{none,pt,ft}
    # columns to choice columns via catalog$employment_choice.
    #
    # Params:
    #   - parent_units_wide (df): wide-format parent unit data
    #   - catalog (df): choice catalog with employment_choice column
    #   - col_prefix (chr): column name prefix (e.g., 'agi', 'emtr')
    #
    # Returns: (matrix) n_units x n_choices matrix
    #--------------------------------------------------------------------------

    n_choices <- nrow(catalog)
    n_units <- nrow(parent_units_wide)
    employment_choices <- catalog$employment_choice

    result_matrix <- matrix(NA_real_, nrow = n_units, ncol = n_choices)

    for (emp in c('none', 'pt', 'ft')) {
      col_name <- paste0(col_prefix, '.', emp)
      choice_mask <- employment_choices == emp
      result_matrix[, choice_mask] <- parent_units_wide[[col_name]]
    }

    result_matrix
  }



  compute_delta_tax <- function(delta_earnings, baseline_earnings, emtr_matrix, tax_max) {

    #--------------------------------------------------------------------------
    # Computes total tax change (IIT + payroll) from earnings perturbation
    # using EMTR approach, with OASI capped at tax_max.
    #
    # Params:
    #   - delta_earnings (matrix): change in earnings (n_units x n_choices)
    #   - baseline_earnings (matrix): original earnings (n_units x n_choices)
    #   - emtr_matrix (matrix): effective marginal tax rates (n_units x n_choices)
    #   - tax_max (dbl): OASI taxable earnings cap (Social Security wage base)
    #
    # Returns: (matrix) total tax change (n_units x n_choices)
    #--------------------------------------------------------------------------

    delta_liab_iit <- emtr_matrix * delta_earnings
    delta_liab_medicare <- MEDICARE_TAX_RATE * delta_earnings
    oasi_headroom <- pmax(0, tax_max - baseline_earnings)
    delta_liab_oasi <- OASI_TAX_RATE * pmin(delta_earnings, oasi_headroom)

    delta_liab_iit + delta_liab_medicare + delta_liab_oasi
  }



  fill_marginal_probabilities <- function(prob_matrix, parent_units_wide, enrollment,
                                           catalog, n_children, row_subset = NULL) {

    #--------------------------------------------------------------------------
    # Fills probability matrix using marginal enrollment probabilities. Used
    # for 1-child families or as fallback for 2-child pseudofamilies when
    # joint probabilities are unavailable.
    #
    # Params:
    #   - prob_matrix (matrix): probability matrix to fill (n_units x n_choices)
    #   - parent_units_wide (df): wide-format parent unit data with .row_idx
    #   - enrollment (df): enrollment probability table
    #   - catalog (df): choice catalog
    #   - n_children (int): number of children (1 or 2)
    #   - row_subset (int vec): optional row indices to fill; NULL fills all
    #
    # Returns: (matrix) updated probability matrix
    #--------------------------------------------------------------------------

    pu_data <- if (is.null(row_subset)) parent_units_wide else parent_units_wide[row_subset, ]
    target_rows <- if (is.null(row_subset)) seq_len(nrow(parent_units_wide)) else row_subset

    for (k in 1:nrow(catalog)) {
      emp_choice <- catalog$employment_choice[k]
      p_emp <- parent_units_wide[[paste0('p_employment.', emp_choice)]]
      p_enroll_col <- paste0('p_enrollment.', emp_choice)

      # Child 1 marginal probability
      child1_probs <- lookup_child_enrollment(
        pu_data, enrollment,
        catalog$child1_ecec_type[k], catalog$child1_hours_choice[k],
        'child_id.1', p_enroll_col
      )

      p_choice <- p_emp[target_rows] * child1_probs

      # Child 2 marginal probability (2-child families)
      if (n_children == 2) {
        child2_probs <- lookup_child_enrollment(
          pu_data, enrollment,
          catalog$child2_ecec_type[k], catalog$child2_hours_choice[k],
          'child_id.2', p_enroll_col
        )
        p_choice <- p_choice * child2_probs
      }

      prob_matrix[target_rows, k] <- p_choice
    }

    prob_matrix
  }



  lookup_child_enrollment <- function(parent_units_subset, enrollment, ecec_type,
                                       hours_choice, child_id_col, p_enroll_col) {

    #--------------------------------------------------------------------------
    # Looks up enrollment probability for one child from the enrollment table.
    # Joins parent unit subset to enrollment on (hh_id, parent_unit_id,
    # child_id) filtered by ECEC type and hours choice.
    #
    # Params:
    #   - parent_units_subset (df): parent unit rows with .row_idx column
    #   - enrollment (df): enrollment probability table
    #   - ecec_type (chr): ECEC type to filter on
    #   - hours_choice (chr): hours choice to filter on
    #   - child_id_col (chr): column name for child ID (e.g., 'child_id.1')
    #   - p_enroll_col (chr): column name for enrollment probability
    #
    # Returns: (num vec) enrollment probabilities
    #--------------------------------------------------------------------------

    result <- parent_units_subset %>%
      select(hh_id, parent_unit_id, child_id = !!sym(child_id_col), .row_idx) %>%
      left_join(
        enrollment %>%
          filter(ecec_choice == ecec_type, ecec_hours_choice == hours_choice) %>%
          select(hh_id, parent_unit_id, child_id, p_enrollment = !!sym(p_enroll_col)),
        by = c('hh_id', 'parent_unit_id', 'child_id')
      ) %>%
      mutate(p_enrollment = replace_na(p_enrollment, 0)) %>%
      pull(p_enrollment)

    result
  }



  build_perturbed_net_income <- function(parent_units_wide, catalog,
                                          ecec_cost_matrix, wage_multiplier,
                                          tax_max) {

    #--------------------------------------------------------------------------
    # Builds net_income matrix under perturbed wages for elasticity
    # calculation. ECEC costs are fixed; only AGI and taxes change with
    # wage perturbation.
    #
    # Params:
    #   - parent_units_wide (df): wide-format parent unit data
    #   - catalog (df): choice catalog
    #   - ecec_cost_matrix (matrix): ECEC cost matrix (held fixed)
    #   - wage_multiplier (dbl): wage perturbation factor (e.g., 1.01)
    #   - tax_max (dbl): OASI taxable earnings cap
    #
    # Returns: (matrix) perturbed net income (n_units x n_choices)
    #--------------------------------------------------------------------------

    earnings_matrix <- build_matrix_by_employment(parent_units_wide, catalog, 'earnings_pc_choice')
    emtr_matrix <- build_matrix_by_employment(parent_units_wide, catalog, 'emtr')
    agi_matrix <- build_matrix_by_employment(parent_units_wide, catalog, 'agi')
    tax_matrix <- build_matrix_by_employment(parent_units_wide, catalog, 'total_tax')

    # Compute perturbation effects
    delta_earnings <- earnings_matrix * (wage_multiplier - 1)
    delta_total_tax <- compute_delta_tax(delta_earnings, earnings_matrix, emtr_matrix, tax_max)

    # Net income = (agi + delta_earnings) - (tax + delta_tax) - ecec_cost
    (agi_matrix + delta_earnings) - (tax_matrix + delta_total_tax) - ecec_cost_matrix
  }



  build_calibration_type_data <- function(pu_type, n_children, child_cat,
                                           parent_units_wide, enrollment, enrollment_joint,
                                           supply_prices,
                                           other_paid_base_price = NULL,
                                           wage_perturbation, tax_max) {

    #--------------------------------------------------------------------------
    # Builds calibration data for one parent unit type. Constructs net income,
    # ECEC cost, and probability matrices. Uses price_wedge x supply_price
    # to compute out-of-pocket care costs.
    #
    # Params:
    #   - pu_type (chr): parent unit type name (e.g., 'c1_p1')
    #   - n_children (int): number of children (1 or 2)
    #   - child_cat (chr): child category filter value
    #   - parent_units_wide (df): wide-format parent unit data with taxes
    #   - enrollment (df): enrollment probability table
    #   - enrollment_joint (df): joint enrollment probabilities for 2-child
    #   - supply_prices (num vec): baseline supply prices by sector
    #   - other_paid_base_price (dbl): base price for Other Paid care; NULL
    #       if not applicable
    #   - wage_perturbation (dbl): wage multiplier for elasticity calc
    #   - tax_max (dbl): OASI taxable earnings cap
    #
    # Returns: (df) calibration tibble with net_income, ecec_cost, and
    #   probability columns appended to parent unit data
    #--------------------------------------------------------------------------

    cat('  Processing', pu_type, '...\n')

    pu_subset <- parent_units_wide %>%
      filter(child_category == child_cat)

    if (nrow(pu_subset) == 0) {
      cat('    No observations for', pu_type, '\n')
      return(tibble())
    }

    cat('    n =', nrow(pu_subset), 'parent units\n')

    catalog <- get_choice_catalog(n_children)
    n_choices <- nrow(catalog)

    ecec_cost_matrix <- {
      n_choices_ecec <- nrow(catalog)
      n_units_ecec <- nrow(pu_subset)

      hours_annual <- HOURS_ANNUAL

      # Build cost matrices for each child
      child1_cost_matrix <- matrix(0, nrow = n_units_ecec, ncol = n_choices_ecec)
      child2_cost_matrix <- matrix(0, nrow = n_units_ecec, ncol = n_choices_ecec)

      child1_sector <- catalog$child1_market_sector_id
      child1_hours <- unname(hours_annual[catalog$child1_hours_choice])

      # Wedge vectors by sector (length n_units)
      # Sector 1 = Unpaid Center (wedge = 1, but price = 0 so cost = 0)
      # Sector 2 = Low-Priced Center
      # Sector 3 = High-Priced Center
      # Sector 4 = Paid Home-Based
      wedge_by_sector <- list(
        `1` = rep(1, n_units_ecec),
        `2` = pu_subset$price_wedge.center_low,
        `3` = pu_subset$price_wedge.center_high,
        `4` = pu_subset$price_wedge.home
      )

      # Fill in child 1 costs
      for (sector_id in 1:4) {
        P_sector <- supply_prices[sector_id]
        if (is.na(P_sector) || P_sector == 0) next

        cols <- which(child1_sector == sector_id & child1_hours > 0)
        if (length(cols) == 0) next

        wedge <- wedge_by_sector[[as.character(sector_id)]]
        # cost = wedge x price x hours
        child1_cost_matrix[, cols] <- tcrossprod(wedge * P_sector, child1_hours[cols])
      }

      # Fill in child 2 costs (for 2-child families)
      if (n_children == 2) {
        child2_sector <- catalog$child2_market_sector_id
        child2_hours <- unname(hours_annual[catalog$child2_hours_choice])

        for (sector_id in 1:4) {
          P_sector <- supply_prices[sector_id]
          if (is.na(P_sector) || P_sector == 0) next

          cols <- which(child2_sector == sector_id & child2_hours > 0)
          if (length(cols) == 0) next

          wedge <- wedge_by_sector[[as.character(sector_id)]]
          child2_cost_matrix[, cols] <- tcrossprod(wedge * P_sector, child2_hours[cols])
        }
      }

      # Other Paid: non-market sector with exogenous base price
      if (!is.null(other_paid_base_price) && !is.null(pu_subset$price_wedge.other_paid) &&
          !is.na(other_paid_base_price) && other_paid_base_price > 0) {

        wedge_other_paid <- pu_subset$price_wedge.other_paid
        if (is.null(wedge_other_paid)) wedge_other_paid <- rep(1, n_units_ecec)

        # Child 1: Other Paid choices (market_sector_id is NA for Other Paid)
        child1_ecec_type <- catalog$child1_ecec_type
        op_cols <- which(is.na(child1_sector) &
                         child1_ecec_type == 'Other Paid' &
                         child1_hours > 0)
        if (length(op_cols) > 0) {
          child1_cost_matrix[, op_cols] <- tcrossprod(wedge_other_paid * other_paid_base_price,
                                                       child1_hours[op_cols])
        }

        # Child 2: Other Paid choices (for n_children == 2)
        if (n_children == 2) {
          child2_ecec_type <- catalog$child2_ecec_type
          op_cols2 <- which(is.na(child2_sector) &
                            child2_ecec_type == 'Other Paid' &
                            child2_hours > 0)
          if (length(op_cols2) > 0) {
            child2_cost_matrix[, op_cols2] <- tcrossprod(wedge_other_paid * other_paid_base_price,
                                                          child2_hours[op_cols2])
          }
        }
      }

      # Total cost = child1 + child2
      child1_cost_matrix + child2_cost_matrix
    }

    net_income_matrix <- {
      agi_matrix <- build_matrix_by_employment(pu_subset, catalog, 'agi')
      taxes_matrix <- build_matrix_by_employment(pu_subset, catalog, 'total_tax')
      agi_matrix - taxes_matrix - ecec_cost_matrix
    }

    Y_matrix <- {
      agi_matrix_y <- build_matrix_by_employment(pu_subset, catalog, 'agi')
      taxes_matrix_y <- build_matrix_by_employment(pu_subset, catalog, 'total_tax')
      agi_matrix_y - taxes_matrix_y
    }

    net_income_perturbed_matrix <- build_perturbed_net_income(
      pu_subset, catalog, ecec_cost_matrix, wage_perturbation, tax_max
    )

    Y_perturbed_matrix <- {
      earnings_matrix_yp <- build_matrix_by_employment(pu_subset, catalog, 'earnings_pc_choice')
      emtr_matrix_yp <- build_matrix_by_employment(pu_subset, catalog, 'emtr')
      agi_matrix_yp <- build_matrix_by_employment(pu_subset, catalog, 'agi')
      tax_matrix_yp <- build_matrix_by_employment(pu_subset, catalog, 'total_tax')

      delta_earnings_yp <- earnings_matrix_yp * (wage_perturbation - 1)
      delta_total_tax_yp <- compute_delta_tax(delta_earnings_yp, earnings_matrix_yp, emtr_matrix_yp, tax_max)

      (agi_matrix_yp + delta_earnings_yp) - (tax_matrix_yp + delta_total_tax_yp)
    }

    prob_matrix <- {
      n_choices_prob <- nrow(catalog)
      n_units_prob <- nrow(pu_subset)

      pu_subset_prob <- pu_subset %>%
        mutate(.row_idx = row_number())

      prob_mat <- matrix(0, nrow = n_units_prob, ncol = n_choices_prob)
      use_joint <- n_children == 2 && !is.null(enrollment_joint) && nrow(enrollment_joint) > 0

      if (use_joint) {
        {
          pu_child_lookup <- pu_subset_prob %>%
            select(hh_id, parent_unit_id, child_id.1, child_id.2, .row_idx)

          for (k in 1:nrow(catalog)) {
            emp_choice <- catalog$employment_choice[k]
            p_emp <- pu_subset_prob[[paste0('p_employment.', emp_choice)]]
            p_joint_col <- paste0('p_joint.', emp_choice)

            joint_probs <- enrollment_joint %>%
              filter(
                ecec_choice_1 == catalog$child1_ecec_type[k],
                ecec_hours_choice_1 == catalog$child1_hours_choice[k],
                ecec_choice_2 == catalog$child2_ecec_type[k],
                ecec_hours_choice_2 == catalog$child2_hours_choice[k]
              ) %>%
              select(hh_id, parent_unit_id, child_id_1, child_id_2, p_joint = !!sym(p_joint_col)) %>%
              inner_join(
                pu_child_lookup,
                by = c('hh_id', 'parent_unit_id', 'child_id_1' = 'child_id.1', 'child_id_2' = 'child_id.2')
              ) %>%
              mutate(p_joint = replace_na(p_joint, 0))

            if (nrow(joint_probs) > 0 && !any(is.na(joint_probs$.row_idx))) {
              prob_mat[joint_probs$.row_idx, k] <- p_emp[joint_probs$.row_idx] * joint_probs$p_joint
            }
          }
        }

        # Fall back to marginals for rows where joint doesn't sum to 1
        row_sums <- rowSums(prob_mat)
        rows_needing_fallback <- which(abs(row_sums - 1) > 1e-6)
        if (length(rows_needing_fallback) > 0) {
          prob_mat <- fill_marginal_probabilities(
            prob_mat, pu_subset_prob, enrollment, catalog, n_children,
            row_subset = rows_needing_fallback
          )
        }
      } else {
        prob_mat <- fill_marginal_probabilities(
          prob_mat, pu_subset_prob, enrollment, catalog, n_children
        )
      }

      prob_mat
    }

    # Validate probability rows sum to 1 (no normalization - fail loudly on bad data)
    row_sums <- rowSums(prob_matrix)
    bad_rows <- which(abs(row_sums - 1) > 1e-6)
    if (length(bad_rows) > 0) {
      example_sums <- head(row_sums[bad_rows], 5)
      stop(paste0(
        'Calibration probability rows do not sum to 1. Found ', length(bad_rows),
        ' bad rows (', round(100 * length(bad_rows) / nrow(prob_matrix), 2), '%). ',
        'Example row sums: ', paste(round(example_sums, 6), collapse = ', '), '. ',
        'This indicates missing enrollment data upstream. ',
        'Check that all (child, ecec_type, hours_choice) combinations exist in enrollment table.'
      ))
    }

    # Name matrix columns
    colnames(net_income_matrix) <- paste0('net_income.', 1:n_choices)
    colnames(net_income_perturbed_matrix) <- paste0('net_income_perturbed.', 1:n_choices)
    colnames(ecec_cost_matrix) <- paste0('ecec_cost.', 1:n_choices)
    colnames(prob_matrix) <- paste0('p.', 1:n_choices)
    colnames(Y_matrix) <- paste0('Y.', 1:n_choices)
    colnames(Y_perturbed_matrix) <- paste0('Y_perturbed.', 1:n_choices)

    # C is ecec_cost (out-of-pocket care cost)
    C_matrix <- ecec_cost_matrix
    colnames(C_matrix) <- paste0('C.', 1:n_choices)

    # Combine into final tibble
    pu_subset %>%
      select(-any_of('.row_idx')) %>%
      bind_cols(
        as_tibble(net_income_matrix),
        as_tibble(net_income_perturbed_matrix),
        as_tibble(ecec_cost_matrix),
        as_tibble(prob_matrix),
        as_tibble(Y_matrix),
        as_tibble(Y_perturbed_matrix),
        as_tibble(C_matrix)
      )
  }



  run_demand_estimation <- function(calibration_data_dir,
                                    output_path,
                                    care_cost_elasticity_target = NULL,
                                    wage_elasticity_target = NULL,
                                    paid_care_elasticity_target = DEFAULT_PAID_CARE_ELASTICITY_TARGET,
                                    income_elasticity_target = -0.05,
                                    year = 2019,
                                    price_perturbation = 1.01,
                                    income_perturbation = 0.01,
                                    time_stamp = NULL) {

    #--------------------------------------------------------------------------
    # Calibrates CRRA (beta, rho) using pooled data to match two elasticity
    # targets, then writes demand params YAML and alpha matrices.
    #
    # Params:
    #   - calibration_data_dir (chr): directory containing calibration CSVs
    #   - output_path (chr): path for demand_params YAML output
    #   - care_cost_elasticity_target (dbl): target cost elasticity; NULL to skip
    #   - wage_elasticity_target (dbl): target wage elasticity; NULL to skip
    #   - paid_care_elasticity_target (dbl): target paid care elasticity
    #   - income_elasticity_target (dbl): target income elasticity
    #   - year (int): calibration year
    #   - price_perturbation (dbl): price shock factor for elasticity calc
    #   - income_perturbation (dbl): income shock fraction for elasticity calc
    #   - time_stamp (chr): timestamp for versioned output; NULL to skip
    #
    # Returns: (list) per-type calibration results (invisible)
    #--------------------------------------------------------------------------


    compute_alpha_matrix <- function(p0_matrix, V_matrix) {

      #------------------------------------------------------------------------
      # Computes alpha = log(p0) - V, normalized so alpha_1 = 0. Returns
      # n x (K-1) matrix with first column dropped (always zero after
      # normalization).
      #
      # Params:
      #   - p0_matrix (matrix): observed probability matrix (n x K)
      #   - V_matrix (matrix): value function matrix (n x K)
      #
      # Returns: (matrix) alpha matrix (n x K-1)
      #------------------------------------------------------------------------

      log_p0 <- log(pmax(p0_matrix, 1e-12))

      # Raw alpha: alpha_ij = log(p0_ij) - V_ij
      raw_alpha <- log_p0 - V_matrix

      # Normalize: subtract column 1 from all columns
      # This makes alpha_1 = 0 for all households
      alpha_normalized <- raw_alpha - raw_alpha[, 1]

      # Drop first column (always zero after normalization)
      alpha_normalized[, -1, drop = FALSE]
    }


    # Determine which elasticity to target (priority: cost > wage > paid_care)
    if (!is.null(care_cost_elasticity_target)) {
      target_type <- 'cost'
      first_target <- care_cost_elasticity_target
    } else if (!is.null(wage_elasticity_target)) {
      target_type <- 'wage'
      first_target <- wage_elasticity_target
    } else {
      target_type <- 'paid_care'
      first_target <- paid_care_elasticity_target
    }

    cat('Calibrating demand model...\n')

    type_data <- list()

    for (i in seq_along(PARENT_UNIT_NAMES)) {
      pu_type <- PARENT_UNIT_NAMES[i]
      n_children <- PARENT_UNIT_N_CHILDREN[i]

      csv_path <- file.path(
        calibration_data_dir,
        paste0('calibration_', pu_type, '_', year, '.csv')
      )

      if (!file.exists(csv_path)) {
        next
      }

      calibration_df <- data.table::fread(csv_path)
      catalog <- get_choice_catalog(n_children)

      data <- {
        wage_perturbation_ei <- 1.01

        n_choices_ei <- nrow(catalog)
        n_units_ei <- nrow(calibration_df)

        # Extract column names
        y_cols <- paste0('Y.', 1:n_choices_ei)
        c_cols <- paste0('C.', 1:n_choices_ei)
        y_perturbed_cols <- paste0('Y_perturbed.', 1:n_choices_ei)
        p_cols <- paste0('p.', 1:n_choices_ei)

        # Extract base matrices
        Y_baseline <- as.matrix(calibration_df[, ..y_cols])
        C_baseline <- as.matrix(calibration_df[, ..c_cols])
        p_observed <- as.matrix(calibration_df[, ..p_cols])

        # Wage-perturbed Y (from pre-computed perturbation)
        if (y_perturbed_cols[1] %in% names(calibration_df)) {
          Y_wage <- as.matrix(calibration_df[, ..y_perturbed_cols])
        } else {
          cat('    Note: Y_perturbed not found, approximating\n')
          working_mask_approx <- catalog$employment_choice != 'none'
          Y_wage <- Y_baseline
          # Approximate: wages increase by perturbation, ~70% flows to pre-care income
          Y_wage[, working_mask_approx] <- Y_baseline[, working_mask_approx] *
            (1 + (wage_perturbation_ei - 1) * 0.7)
        }

        # Weights (use parent unit weight)
        weights_ei <- (calibration_df$per_weight1 +
                    coalesce(calibration_df$per_weight2, calibration_df$per_weight1)) / 2

        # Working choices mask
        n_children_ei <- if (n_choices_ei == 45) 1 else 2
        working_choices_ei <- get_working_choice_mask(n_children_ei)

        #----------------------------------------------------------------------
        # Additional fields for dollar-weighted elasticities by AGI group
        #----------------------------------------------------------------------

        # Observed AGI: AGI at the observed employment state
        observed_agi_ei <- case_when(
          calibration_df[['employment_observed']] == 'none' ~ calibration_df[['agi.none']],
          calibration_df[['employment_observed']] == 'pt'   ~ calibration_df[['agi.pt']],
          calibration_df[['employment_observed']] == 'ft'   ~ calibration_df[['agi.ft']],
          TRUE ~ NA_real_
        )

        # Defensive check: warn if any observed_agi is NA
        n_na <- sum(is.na(observed_agi_ei))
        if (n_na > 0) {
          warning(sprintf('observed_agi has %d NA values (unexpected employment_observed values)', n_na))
        }

        # Expected values for dollar-weighting
        E_Y_ei <- rowSums(p_observed * Y_baseline)
        E_C_ei <- rowSums(p_observed * C_baseline)

        #----------------------------------------------------------------------
        # Build hours_paid_matrix: annual paid care hours by choice
        #----------------------------------------------------------------------
        paid_care_types <- c('Low-Priced Center-Based', 'High-Priced Center-Based',
                             'Paid Home-Based', 'Other Paid')

        hours_annual_ei <- HOURS_ANNUAL

        hours_paid_by_choice <- ifelse(
          catalog$child1_ecec_type %in% paid_care_types,
          hours_annual_ei[catalog$child1_hours_choice], 0
        )
        if (n_children_ei == 2) {
          hours_paid_by_choice <- hours_paid_by_choice + ifelse(
            catalog$child2_ecec_type %in% paid_care_types,
            hours_annual_ei[catalog$child2_hours_choice], 0
          )
        }

        hours_paid_matrix_ei <- matrix(hours_paid_by_choice, nrow = n_units_ei, ncol = n_choices_ei,
                                byrow = TRUE)

        #----------------------------------------------------------------------
        # Build work_hours_matrix: annual primary caregiver work hours by choice
        #----------------------------------------------------------------------

        required_cols <- c(
          'hours_pc.none', 'hours_pc.pt', 'hours_pc.ft',
          'weeks_pc.none', 'weeks_pc.pt', 'weeks_pc.ft'
        )

        if (!all(required_cols %in% names(calibration_df))) {
          stop(
            'prepare_estimation_inputs: calibration_df missing work-hours columns. ',
            'Expected: ', paste(required_cols, collapse = ', ')
          )
        }

        annual_work_hours_none <- calibration_df[['hours_pc.none']] * calibration_df[['weeks_pc.none']]
        annual_work_hours_pt <- calibration_df[['hours_pc.pt']] * calibration_df[['weeks_pc.pt']]
        annual_work_hours_ft <- calibration_df[['hours_pc.ft']] * calibration_df[['weeks_pc.ft']]

        # Local helper: fill matrix columns by employment choice mask
        fill_by_employment_ei <- function(vals_none, vals_pt, vals_ft) {

          #--------------------------------------------------------------------
          # Fills an n_units x n_choices matrix by broadcasting employment-
          # specific values into columns matching each employment choice.
          #
          # Params:
          #   - vals_none (num vec): values for 'none' employment choices
          #   - vals_pt (num vec): values for 'pt' employment choices
          #   - vals_ft (num vec): values for 'ft' employment choices
          #
          # Returns: (matrix) n_units x n_choices matrix
          #--------------------------------------------------------------------

          mat <- matrix(0, nrow = n_units_ei, ncol = n_choices_ei)
          emp <- catalog$employment_choice
          if (any(emp == 'none')) mat[, emp == 'none'] <- vals_none
          if (any(emp == 'pt'))   mat[, emp == 'pt']   <- vals_pt
          if (any(emp == 'ft'))   mat[, emp == 'ft']   <- vals_ft
          mat
        }

        work_hours_matrix_ei <- fill_by_employment_ei(
          annual_work_hours_none, annual_work_hours_pt, annual_work_hours_ft
        )

        #----------------------------------------------------------------------
        # Build earnings_matrix: primary caregiver earnings by choice
        #----------------------------------------------------------------------

        earnings_cols <- c('earnings_pc_choice.none', 'earnings_pc_choice.pt', 'earnings_pc_choice.ft')

        if (all(earnings_cols %in% names(calibration_df))) {
          earnings_matrix_ei <- fill_by_employment_ei(
            calibration_df[['earnings_pc_choice.none']],
            calibration_df[['earnings_pc_choice.pt']],
            calibration_df[['earnings_pc_choice.ft']]
          )
        } else {
          warning('earnings_pc_choice columns not found; RTW elasticity will be approximate')
          earnings_matrix_ei <- NULL
        }

        #----------------------------------------------------------------------
        # Build emtr_matrix: effective marginal tax rate by choice
        #----------------------------------------------------------------------

        emtr_cols <- c('emtr.none', 'emtr.pt', 'emtr.ft')

        if (all(emtr_cols %in% names(calibration_df))) {
          emtr_none <- calibration_df[['emtr.none']]
          emtr_pt <- calibration_df[['emtr.pt']]
          emtr_ft <- calibration_df[['emtr.ft']]

          emtr_matrix_ei <- matrix(0, nrow = n_units_ei, ncol = n_choices_ei)
          mask_none <- catalog$employment_choice == 'none'
          mask_pt <- catalog$employment_choice == 'pt'
          mask_ft <- catalog$employment_choice == 'ft'

          if (any(mask_none)) emtr_matrix_ei[, mask_none] <- emtr_none
          if (any(mask_pt)) emtr_matrix_ei[, mask_pt] <- emtr_pt
          if (any(mask_ft)) emtr_matrix_ei[, mask_ft] <- emtr_ft
        } else {
          warning('emtr columns not found; RTW elasticity denominator will use approximation')
          emtr_matrix_ei <- NULL
        }

        list(
          Y_baseline = Y_baseline,
          C_baseline = C_baseline,
          Y_wage = Y_wage,
          p_observed = p_observed,
          weights = weights_ei,
          working_choices = working_choices_ei,
          n_children = n_children_ei,
          wage_perturbation = wage_perturbation_ei,
          observed_agi = observed_agi_ei,
          E_Y = E_Y_ei,
          E_C = E_C_ei,
          hours_paid_matrix = hours_paid_matrix_ei,
          work_hours_matrix = work_hours_matrix_ei,
          earnings_matrix = earnings_matrix_ei,
          emtr_matrix = emtr_matrix_ei
        )
      }

      type_data[[pu_type]] <- data
      type_data[[pu_type]]$calibration_df <- calibration_df
    }

    #=========================================================================
    # Calibration: CRRA (beta, rho) - two targets
    #=========================================================================

    type_results <- list()

    #-------------------------------------------------------------------------
    # Build AGI quintile structure for diagnostic reporting
    #-------------------------------------------------------------------------
    all_agi <- c()
    all_weights <- c()
    all_pu <- c()
    all_idx <- c()

    for (pu_type in names(type_data)) {
      d <- type_data[[pu_type]]
      n <- length(d$observed_agi)
      all_agi <- c(all_agi, d$observed_agi)
      all_weights <- c(all_weights, d$weights)
      all_pu <- c(all_pu, rep(pu_type, n))
      all_idx <- c(all_idx, seq_len(n))
    }

    # Compute population-weighted AGI quintile cutoffs
    sorted_order <- order(all_agi)
    cum_weight <- cumsum(all_weights[sorted_order])
    total_w <- sum(all_weights)

    quintile_cutoffs <- numeric(4)
    for (q in 1:4) {
      target_w <- q * total_w / 5
      idx <- which(cum_weight >= target_w)[1]
      quintile_cutoffs[q] <- all_agi[sorted_order[idx]]
    }


    compute_pooled_elasticity <- function(beta, rho, perturb_fn, log_denom,
                                          pct_mode = FALSE) {

      #------------------------------------------------------------------------
      # Generic pooled elasticity helper. Loops over type_data, computes
      # baseline alpha, applies perturb_fn to get p1, accumulates weighted
      # outcome, and returns arc elasticity.
      #
      # Params:
      #   - beta (dbl): utility scaling parameter
      #   - rho (dbl): risk aversion parameter
      #   - perturb_fn (fn): function(d, alpha, beta, rho) returning
      #       list(val0, val1) with baseline/perturbed weighted outcomes
      #   - log_denom (dbl): perturbation factor for arc elasticity denominator
      #   - pct_mode (logical): if TRUE, use percent-change ratio instead of
      #       log ratio
      #
      # Returns: (dbl) pooled arc elasticity
      #------------------------------------------------------------------------

      acc0 <- 0; acc1 <- 0; tw <- 0

      for (pu_type in names(type_data)) {
        d <- type_data[[pu_type]]

        NI_baseline <- d$Y_baseline - d$C_baseline
        V_baseline <- compute_V_crra(NI_baseline, beta, rho)
        alpha <- compute_alpha_matrix(d$p_observed, V_baseline)

        r <- perturb_fn(d, alpha, beta, rho)
        acc0 <- acc0 + r$val0
        acc1 <- acc1 + r$val1
        tw <- tw + sum(d$weights)
      }

      if (tw == 0) return(NA_real_)

      if (pct_mode) {
        # Percent change ratio (income elasticity)
        return((acc1 - acc0) / acc0 / log_denom)
      }

      # Arc elasticity
      acc0 <- max(acc0, SHARE_FLOOR)
      acc1 <- max(acc1, SHARE_FLOOR)
      (log(acc1) - log(acc0)) / log(log_denom)
    }

    compute_pooled_cost_elasticity_crra <- function(beta, rho) {

      #------------------------------------------------------------------------
      # Computes pooled cost-participation elasticity: d(log Employment) /
      # d(log Cost) across all parent unit types using CRRA utility.
      #
      # Params:
      #   - beta (dbl): utility scaling parameter
      #   - rho (dbl): risk aversion parameter
      #
      # Returns: (dbl) pooled cost-participation elasticity
      #------------------------------------------------------------------------

      compute_pooled_elasticity(beta, rho, function(d, alpha, beta, rho) {
        C_perturbed <- d$C_baseline * price_perturbation
        NI_perturbed <- d$Y_baseline - C_perturbed
        V_perturbed <- compute_V_crra(NI_perturbed, beta, rho)
        p1 <- compute_probs_from_alpha(alpha, V_perturbed)
        wm <- d$working_choices
        list(
          val0 = sum(d$weights * rowSums(d$p_observed[, wm, drop = FALSE])),
          val1 = sum(d$weights * rowSums(p1[, wm, drop = FALSE]))
        )
      }, log_denom = price_perturbation)
    }

    compute_pooled_wage_elasticity_crra <- function(beta, rho) {

      #------------------------------------------------------------------------
      # Computes pooled wage-participation elasticity: d(log Employment) /
      # d(log Wage) across all parent unit types using CRRA utility.
      #
      # Params:
      #   - beta (dbl): utility scaling parameter
      #   - rho (dbl): risk aversion parameter
      #
      # Returns: (dbl) pooled wage-participation elasticity
      #------------------------------------------------------------------------

      compute_pooled_elasticity(beta, rho, function(d, alpha, beta, rho) {
        NI_wage <- d$Y_wage - d$C_baseline
        V_wage <- compute_V_crra(NI_wage, beta, rho)
        p1 <- compute_probs_from_alpha(alpha, V_wage)
        wm <- d$working_choices
        list(
          val0 = sum(d$weights * rowSums(d$p_observed[, wm, drop = FALSE])),
          val1 = sum(d$weights * rowSums(p1[, wm, drop = FALSE]))
        )
      }, log_denom = 1.01)
    }

    compute_pooled_paid_care_elasticity_crra <- function(beta, rho) {

      #------------------------------------------------------------------------
      # Computes pooled paid care elasticity: d(log Paid Care Hours) /
      # d(log Cost) across all parent unit types, dollar-weighted by
      # expected care costs.
      #
      # Params:
      #   - beta (dbl): utility scaling parameter
      #   - rho (dbl): risk aversion parameter
      #
      # Returns: (dbl) pooled paid care elasticity
      #------------------------------------------------------------------------

      compute_pooled_elasticity(beta, rho, function(d, alpha, beta, rho) {
        C_perturbed <- d$C_baseline * price_perturbation
        NI_perturbed <- d$Y_baseline - C_perturbed
        V_perturbed <- compute_V_crra(NI_perturbed, beta, rho)
        p1 <- compute_probs_from_alpha(alpha, V_perturbed)
        w_C <- d$weights * rowSums(d$p_observed * d$C_baseline)
        list(
          val0 = sum(w_C * rowSums(d$p_observed * d$hours_paid_matrix)),
          val1 = sum(w_C * rowSums(p1 * d$hours_paid_matrix))
        )
      }, log_denom = price_perturbation)
    }

    compute_pooled_income_elasticity_crra <- function(beta, rho) {

      #------------------------------------------------------------------------
      # Computes pooled income elasticity: d(log Work Hours) / d(log Income)
      # using percent-change ratio (not arc elasticity). Applies a lump-sum
      # transfer proportional to expected income.
      #
      # Params:
      #   - beta (dbl): utility scaling parameter
      #   - rho (dbl): risk aversion parameter
      #
      # Returns: (dbl) pooled income elasticity
      #------------------------------------------------------------------------

      X0 <- 0; X1 <- 0
      for (pu_type in names(type_data)) {
        d <- type_data[[pu_type]]
        E_Y <- rowSums(d$p_observed * d$Y_baseline)
        lump_sum <- E_Y * income_perturbation
        X0 <- X0 + sum(d$weights * E_Y)
        X1 <- X1 + sum(d$weights * (E_Y + lump_sum))
      }
      pct_X <- (X1 - X0) / X0

      compute_pooled_elasticity(beta, rho, function(d, alpha, beta, rho) {
        E_Y <- rowSums(d$p_observed * d$Y_baseline)
        lump_sum <- E_Y * income_perturbation
        Y_perturbed <- d$Y_baseline + lump_sum
        NI_perturbed <- Y_perturbed - d$C_baseline
        V_perturbed <- compute_V_crra(NI_perturbed, beta, rho)
        p1 <- compute_probs_from_alpha(alpha, V_perturbed)
        list(
          val0 = sum(d$weights * rowSums(d$p_observed * d$work_hours_matrix)),
          val1 = sum(d$weights * rowSums(p1 * d$work_hours_matrix))
        )
      }, log_denom = pct_X, pct_mode = TRUE)
    }

    #-------------------------------------------------------------------------
    # 2D Calibration: target cost elasticity + income elasticity
    #-------------------------------------------------------------------------
    # Compute total population across all types
    total_pop <- sum(sapply(type_data, function(d) sum(d$weights)))

    E0_pooled <- sum(sapply(names(type_data), function(pu_type) {
      d <- type_data[[pu_type]]
      w <- d$weights
      E0_type <- sum(w * rowSums(d$p_observed[, d$working_choices, drop = FALSE])) / sum(w)
      E0_type * sum(d$weights)
    })) / total_pop

    objective_crra <- function(params) {

      #------------------------------------------------------------------------
      # Objective function for 2D CRRA calibration. Minimizes squared error
      # on the primary elasticity target (cost, wage, or paid care) plus
      # income elasticity target. Returns large penalty for out-of-bounds.
      #
      # Params:
      #   - params (num vec): length-2 vector c(beta, rho)
      #
      # Returns: (dbl) sum of squared elasticity errors
      #------------------------------------------------------------------------

      beta <- params[1]
      rho <- params[2]

      # Constraint: rho must be in (0, 0.15)
      if (rho <= 0 || rho >= 0.15) return(1e6)
      if (beta <= 0) return(1e6)

      e_income <- tryCatch(
        compute_pooled_income_elasticity_crra(beta, rho),
        error = function(e) NA_real_
      )

      if (target_type == 'cost') {
        e_first <- tryCatch(
          compute_pooled_cost_elasticity_crra(beta, rho),
          error = function(e) NA_real_
        )
      } else if (target_type == 'wage') {
        e_first <- tryCatch(
          compute_pooled_wage_elasticity_crra(beta, rho),
          error = function(e) NA_real_
        )
      } else {
        e_first <- tryCatch(
          compute_pooled_paid_care_elasticity_crra(beta, rho),
          error = function(e) NA_real_
        )
      }

      if (any(is.na(c(e_first, e_income)))) return(1e6)

      err_first <- (e_first - first_target)^2
      err_income <- (e_income - income_elasticity_target)^2

      # Weight them equally (both in elasticity units)
      err_first + err_income
    }

    # Grid search (2D)
    beta_grid <- c(0.01, 0.05, 0.1, 0.5, 1, 2, 5, 10)
    rho_grid <- c(0.01, 0.03, 0.05, 0.08, 0.1, 0.14)

    best_err <- Inf
    best_params <- c(1, 0.1)

    for (b in beta_grid) {
      for (r in rho_grid) {
        err <- tryCatch(objective_crra(c(b, r)), error = function(e) Inf)
        if (err < best_err) {
          best_err <- err
          best_params <- c(b, r)
        }
      }
    }

    # Refine with Nelder-Mead optimization (no bounds needed)

    opt_result <- optim(
      par = best_params,
      fn = objective_crra,
      method = 'Nelder-Mead',
      control = list(maxit = 1000)
    )

    beta <- opt_result$par[1]
    rho <- opt_result$par[2]

    # Final elasticities
    final_eta_cost <- compute_pooled_cost_elasticity_crra(beta, rho)
    final_eta_wage <- compute_pooled_wage_elasticity_crra(beta, rho)
    final_eta_paid_care <- compute_pooled_paid_care_elasticity_crra(beta, rho)
    final_eta_income <- compute_pooled_income_elasticity_crra(beta, rho)

    # Check targets
    tol_pct <- 0.10  # 10% tolerance for two-target calibration
    if (target_type == 'cost') {
      final_eta_first <- final_eta_cost
    } else if (target_type == 'wage') {
      final_eta_first <- final_eta_wage
    } else {
      final_eta_first <- final_eta_paid_care
    }
    passed_first <- abs(final_eta_first - first_target) / abs(first_target) <= tol_pct
    passed_income <- abs(final_eta_income - income_elasticity_target) /
                      abs(income_elasticity_target) <= tol_pct


    # -- Assertions: calibration produced economically sensible parameters ----

    # The calibrated elasticities must be in the right ballpark of their targets.
    # If the optimizer converged to a local minimum or the objective landscape
    # is flat, elasticities could be wildly off while the optimizer reports
    # "convergence". A 50% miss is a sign something fundamental went wrong.
    if (!is.na(first_target) && first_target != 0) {
      pct_miss_first <- abs(final_eta_first - first_target) / abs(first_target)
      stopifnot(pct_miss_first < 0.01)
    }
    if (!is.na(income_elasticity_target) && income_elasticity_target != 0) {
      pct_miss_income <- abs(final_eta_income - income_elasticity_target) / abs(income_elasticity_target)
      stopifnot(pct_miss_income < 0.01)
    }

    # Beta and rho must be positive (economic requirement: utility is increasing
    # and concave). If either is zero or negative, the model is nonsensical.
    stopifnot(beta > 0)
    stopifnot(rho > 0)

    # Cost elasticity should be negative (higher prices = less enrollment).
    # Wage elasticity should be positive (higher wages = more employment).
    # If signs are wrong, the model will produce perverse policy predictions
    # (e.g., a subsidy that reduces enrollment).
    stopifnot(final_eta_cost < 0)
    stopifnot(final_eta_wage > 0)

    # Replicate to all types
    for (pu_type in names(type_data)) {
      d <- type_data[[pu_type]]
      w_es <- d$weights
      E0_type <- sum(w_es * rowSums(d$p_observed[, d$working_choices, drop = FALSE])) / sum(w_es)

      type_results[[pu_type]] <- list(
        beta = beta,
        rho = rho,
        cost_participation_elasticity = final_eta_cost,
        wage_elasticity = final_eta_wage,
        paid_care_elasticity = final_eta_paid_care,
        income_elasticity = final_eta_income,
        target_cost_elasticity = if (target_type == 'cost') first_target else NA_real_,
        target_wage_elasticity = if (target_type == 'wage') first_target else NA_real_,
        target_paid_care_elasticity = if (target_type == 'paid_care') first_target else NA_real_,
        target_income_elasticity = income_elasticity_target,
        passed_first = passed_first,
        passed_income = passed_income,
        baseline_employment = E0_type,
        convergence = opt_result$convergence,
        population = sum(d$weights)
      )
    }

    #=========================================================================
    # Compute elasticities by AGI quintile
    #=========================================================================

    agi_group_results <- {
      wage_perturbation_agi <- 1.01

      # Pool data from all types into single vectors
      pooled <- list(type_list = list())

      for (pu_type in names(type_data)) {
        d <- type_data[[pu_type]]
        pooled$type_list[[pu_type]] <- d
      }

      # Collect all weights and AGI values for quintile assignment
      all_weights_agi <- unlist(lapply(pooled$type_list, function(d) d$weights))
      all_observed_agi <- unlist(lapply(pooled$type_list, function(d) d$observed_agi))
      all_E_Y_agi <- unlist(lapply(pooled$type_list, function(d) d$E_Y))
      all_E_C_agi <- unlist(lapply(pooled$type_list, function(d) d$E_C))

      agi_quintiles <- {
        agi_vec <- all_observed_agi
        weights_vec <- all_weights_agi

        ord <- order(agi_vec)
        cumwt <- cumsum(weights_vec[ord]) / sum(weights_vec)

        breaks <- numeric(4)
        break_targets <- c(0.2, 0.4, 0.6, 0.8)

        for (i_brk in 1:4) {
          idx_brk <- which(cumwt >= break_targets[i_brk])[1]
          breaks[i_brk] <- agi_vec[ord][idx_brk]
        }

        case_when(
          agi_vec < breaks[1] ~ 'Q1',
          agi_vec < breaks[2] ~ 'Q2',
          agi_vec < breaks[3] ~ 'Q3',
          agi_vec < breaks[4] ~ 'Q4',
          TRUE ~ 'Q5'
        )
      }

      # Create index mapping: which pooled index belongs to which type and row
      type_indices <- list()
      offset <- 0
      for (pu_type in names(pooled$type_list)) {
        d <- pooled$type_list[[pu_type]]
        n <- length(d$weights)
        type_indices[[pu_type]] <- (offset + 1):(offset + n)
        offset <- offset + n
      }

      # Compute elasticities for each quintile
      results_agi <- list()

      # Quintile labels
      quintile_labels <- c('OVERALL', 'Q1', 'Q2', 'Q3', 'Q4', 'Q5')

      for (group in quintile_labels) {

        if (group == 'OVERALL') {
          group_mask <- rep(TRUE, length(all_weights_agi))
        } else {
          group_mask <- (agi_quintiles == group)
        }

        # Skip if too few observations
        pop <- sum(all_weights_agi[group_mask], na.rm = TRUE)
        if (pop < 1000) {
          results_agi[[group]] <- list(
            cost_participation_elasticity = NA_real_,
            paid_care_elasticity = NA_real_,
            income_elasticity = NA_real_,
            wage_elasticity = NA_real_,
            rtw_elasticity = NA_real_,
            population = pop
          )
          next
        }

        # Accumulators for elasticity computation
        # Cost participation elasticity (population-weighted)
        E0_cost_num <- 0; E0_cost_denom <- 0; E_cost_num <- 0

        # Paid care elasticity (dollar-weighted by E_C)
        H0_price_num <- 0; H0_price_denom <- 0; H_price_num <- 0

        # Income elasticity (dollar-weighted by E_Y)
        H0_income_num <- 0; H0_income_denom <- 0; H_income_num <- 0
        sum_wY_baseline <- 0; sum_wY_perturbed <- 0

        # Wage elasticity (dollar-weighted by E_Y)
        E0_wage_num <- 0; E0_wage_denom <- 0; E_wage_num <- 0

        # RTW elasticity (population-weighted)
        E0_rtw_num <- 0; E0_rtw_denom <- 0; E_rtw_num <- 0
        rtw_log_change_num <- 0; rtw_log_change_denom <- 0

        for (pu_type in names(pooled$type_list)) {
          d <- pooled$type_list[[pu_type]]
          idx <- type_indices[[pu_type]]

          # Get group mask for this type
          type_group_mask <- group_mask[idx]

          if (sum(type_group_mask) == 0) next

          # Subset to group
          p0_sub <- d$p_observed[type_group_mask, , drop = FALSE]
          Y_baseline_sub <- d$Y_baseline[type_group_mask, , drop = FALSE]
          C_baseline_sub <- d$C_baseline[type_group_mask, , drop = FALSE]
          Y_wage_sub <- d$Y_wage[type_group_mask, , drop = FALSE]
          weights_sub <- d$weights[type_group_mask]
          E_Y_sub <- d$E_Y[type_group_mask]
          E_C_sub <- d$E_C[type_group_mask]
          hours_paid_sub <- d$hours_paid_matrix[type_group_mask, , drop = FALSE]
          work_hours_sub <- d$work_hours_matrix[type_group_mask, , drop = FALSE]
          working_mask <- d$working_choices

          # Net income at baseline
          NI_baseline <- Y_baseline_sub - C_baseline_sub
          V_baseline <- compute_V_crra(NI_baseline, beta, rho)
          alpha_sub <- compute_alpha_matrix(p0_sub, V_baseline)

          #--- Cost participation elasticity (population-weighted) ---
          P_working_baseline <- rowSums(p0_sub[, working_mask, drop = FALSE])
          E0_cost_num <- E0_cost_num + sum(weights_sub * P_working_baseline)
          E0_cost_denom <- E0_cost_denom + sum(weights_sub)

          # Cost perturbation
          C_perturbed_sub <- C_baseline_sub * price_perturbation
          NI_cost_perturbed <- Y_baseline_sub - C_perturbed_sub
          V_cost <- compute_V_crra(NI_cost_perturbed, beta, rho)
          p_cost_sub <- compute_probs_from_alpha(alpha_sub, V_cost)
          P_working_cost <- rowSums(p_cost_sub[, working_mask, drop = FALSE])
          E_cost_num <- E_cost_num + sum(weights_sub * P_working_cost)

          #--- Paid care elasticity (dollar-weighted by E_C) ---
          w_C <- weights_sub * E_C_sub
          E_hours_baseline <- rowSums(p0_sub * hours_paid_sub)
          H0_price_num <- H0_price_num + sum(w_C * E_hours_baseline)
          H0_price_denom <- H0_price_denom + sum(w_C)

          # Price perturbation already computed for cost
          E_hours_price <- rowSums(p_cost_sub * hours_paid_sub)
          H_price_num <- H_price_num + sum(w_C * E_hours_price)

          #--- Income elasticity (population-weighted) ---
          E_work_hours_baseline <- rowSums(p0_sub * work_hours_sub)
          H0_income_num <- H0_income_num + sum(weights_sub * E_work_hours_baseline)
          H0_income_denom <- H0_income_denom + sum(weights_sub)

          # Income perturbation (CRRA delta) - lump-sum transfer approach
          E_Y_baseline_row <- rowSums(p0_sub * Y_baseline_sub)
          lump_sum_sub <- E_Y_baseline_row * income_perturbation
          Y_perturbed_sub <- Y_baseline_sub + lump_sum_sub  # broadcasts across columns
          NI_income_perturbed <- Y_perturbed_sub - C_baseline_sub
          V_income <- compute_V_crra(NI_income_perturbed, beta, rho)
          p_income_sub <- compute_probs_from_alpha(alpha_sub, V_income)
          E_work_hours_income <- rowSums(p_income_sub * work_hours_sub)
          H_income_num <- H_income_num + sum(weights_sub * E_work_hours_income)

          # For income elasticity denominator
          sum_wY_baseline <- sum_wY_baseline + sum(weights_sub * E_Y_baseline_row)
          sum_wY_perturbed <- sum_wY_perturbed + sum(weights_sub * E_Y_baseline_row * (1 + income_perturbation))

          #--- Wage elasticity (population-weighted) ---
          E0_wage_num <- E0_wage_num + sum(weights_sub * P_working_baseline)
          E0_wage_denom <- E0_wage_denom + sum(weights_sub)

          # Wage perturbation
          NI_wage <- Y_wage_sub - C_baseline_sub
          V_wage <- compute_V_crra(NI_wage, beta, rho)
          p_wage_sub <- compute_probs_from_alpha(alpha_sub, V_wage)
          P_working_wage <- rowSums(p_wage_sub[, working_mask, drop = FALSE])
          E_wage_num <- E_wage_num + sum(weights_sub * P_working_wage)

          #--- RTW elasticity (population-weighted) ---
          E0_rtw_num <- E0_rtw_num + sum(weights_sub * P_working_baseline)
          E0_rtw_denom <- E0_rtw_denom + sum(weights_sub)

          # RTW perturbation: increase MTR by 1pp, reducing Y by 1% of earnings
          Y_rtw_sub <- Y_baseline_sub
          if (!is.null(d$earnings_matrix)) {
            earnings_sub <- d$earnings_matrix[type_group_mask, , drop = FALSE]
            Y_rtw_sub[, working_mask] <- Y_baseline_sub[, working_mask] -
              0.01 * earnings_sub[, working_mask]
          } else {
            # Fallback: approximate using Y as proxy
            Y_rtw_sub[, working_mask] <- Y_baseline_sub[, working_mask] * 0.99
          }
          NI_rtw <- Y_rtw_sub - C_baseline_sub
          V_rtw <- compute_V_crra(NI_rtw, beta, rho)
          p_rtw_sub <- compute_probs_from_alpha(alpha_sub, V_rtw)
          P_working_rtw <- rowSums(p_rtw_sub[, working_mask, drop = FALSE])
          E_rtw_num <- E_rtw_num + sum(weights_sub * P_working_rtw)

          # Accumulate log change in (1-tau) for RTW denominator
          if (!is.null(d$emtr_matrix)) {
            emtr_sub <- d$emtr_matrix[type_group_mask, , drop = FALSE]
            net_of_tax <- 1 - emtr_sub[, working_mask, drop = FALSE]
            net_of_tax <- pmax(net_of_tax, 0.1)
            log_change_matrix <- log(1 - 0.01 / net_of_tax)
            p0_working <- p0_sub[, working_mask, drop = FALSE]
            person_log_change <- rowSums(p0_working * log_change_matrix) / pmax(rowSums(p0_working), 1e-10)
            rtw_log_change_num <- rtw_log_change_num + sum(weights_sub * P_working_baseline * person_log_change)
            rtw_log_change_denom <- rtw_log_change_denom + sum(weights_sub * P_working_baseline)
          }
        }

        # Compute final elasticities

        arc_elasticity <- function(num0, num1, denom, perturbation) {

          #--------------------------------------------------------------------
          # Computes arc elasticity: d(log share) / d(log perturbation),
          # flooring shares at SHARE_FLOOR to avoid log(0).
          #
          # Params:
          #   - num0 (dbl): baseline weighted numerator
          #   - num1 (dbl): perturbed weighted numerator
          #   - denom (dbl): weighted denominator (population)
          #   - perturbation (dbl): perturbation factor (e.g. 1.01 for 1%)
          #
          # Returns: (dbl) arc elasticity value
          #--------------------------------------------------------------------

          v0 <- max(num0 / denom, SHARE_FLOOR)
          v1 <- max(num1 / denom, SHARE_FLOOR)
          (log(v1) - log(v0)) / log(perturbation)
        }

        eta_cost  <- arc_elasticity(E0_cost_num, E_cost_num, E0_cost_denom, price_perturbation)
        eta_price <- arc_elasticity(H0_price_num, H_price_num, H0_price_denom, price_perturbation)
        eta_wage  <- arc_elasticity(E0_wage_num, E_wage_num, E0_wage_denom, wage_perturbation_agi)

        # Income elasticity (percent change, not log)
        H0_income <- max(H0_income_num / H0_income_denom, SHARE_FLOOR)
        H_income  <- max(H_income_num / H0_income_denom, SHARE_FLOOR)
        eta_income <- ((H_income - H0_income) / H0_income) / ((sum_wY_perturbed / sum_wY_baseline) - 1)

        # RTW elasticity: d(log E) / d(log(1-tau))
        E0_rtw <- max(E0_rtw_num / E0_rtw_denom, SHARE_FLOOR)
        E_rtw  <- max(E_rtw_num / E0_rtw_denom, SHARE_FLOOR)

        if (rtw_log_change_denom > 0) {
          avg_log_change_net_of_tax <- min(rtw_log_change_num / rtw_log_change_denom, -1e-6)
          eta_rtw <- (log(E_rtw) - log(E0_rtw)) / avg_log_change_net_of_tax
        } else {
          # Fallback if no EMTR data: assume average MTR of ~25%
          eta_rtw <- (log(E_rtw) - log(E0_rtw)) / log(0.74 / 0.75)
        }

        results_agi[[group]] <- list(
          cost_participation_elasticity = eta_cost,
          paid_care_elasticity = eta_price,
          income_elasticity = eta_income,
          wage_elasticity = eta_wage,
          rtw_elasticity = eta_rtw,
          population = pop
        )
      }

      results_agi
    }

    #=========================================================================
    # Compute and store alpha matrices
    #
    # Alpha captures preference heterogeneity: alpha = log(p0) - V
    # This enables the structural model: p = exp(alpha + V) / Z
    #=========================================================================

    alpha_data <- list()

    for (i in seq_along(PARENT_UNIT_NAMES)) {
      pu_type <- PARENT_UNIT_NAMES[i]
      n_children <- PARENT_UNIT_N_CHILDREN[i]

      # Skip if no data for this type
      if (is.null(type_data[[pu_type]])) next

      d <- type_data[[pu_type]]

      # Reuse cached calibration_df (avoid redundant ~0.5-1GB reload)
      calibration_df <- type_data[[pu_type]]$calibration_df

      # Get choice catalog
      catalog <- get_choice_catalog(n_children)

      V_matrix <- {
        n_choices_cv <- nrow(catalog)

        y_cols_cv <- paste0('Y.', 1:n_choices_cv)
        c_cols_cv <- paste0('C.', 1:n_choices_cv)

        Y_matrix_cv <- as.matrix(calibration_df[, ..y_cols_cv])
        C_matrix_cv <- as.matrix(calibration_df[, ..c_cols_cv])

        NI_matrix_cv <- Y_matrix_cv - C_matrix_cv

        compute_V_crra(NI_matrix_cv, beta, rho)
      }

      # Compute alpha = log(p0) - V
      alpha_matrix <- compute_alpha_matrix(d$p_observed, V_matrix)

      # Extract row identifiers
      row_id_cols <- c('hh_id', 'parent_unit_id')
      if ('pseudofamily_id' %in% names(calibration_df)) {
        row_id_cols <- c(row_id_cols, 'pseudofamily_id')
      }
      row_ids <- calibration_df[, ..row_id_cols]

      # Build alpha data structure
      alpha_data[[pu_type]] <- list(
        alpha = alpha_matrix,
        row_ids = as.data.frame(row_ids),
        calibration_year = year,
        beta = beta,
        rho = rho
      )

      # Verify: exp(alpha + V) should recover p0
      p_recovered <- compute_probs_from_alpha(alpha_matrix, V_matrix)
      max_diff <- max(abs(p_recovered - d$p_observed))
      if (max_diff > 1e-6) {
        warning('Alpha recovery error for ', pu_type, ': ', sprintf('%.2e', max_diff))
      }
    }

    # Free cached calibration_df from type_data
    for (pu_type_cleanup in names(type_data)) {
      type_data[[pu_type_cleanup]]$calibration_df <- NULL
    }

    # Write alpha matrices
    {
      output_dir_alpha <- dirname(output_path)

      for (pu_type in names(alpha_data)) {
        data_alpha <- alpha_data[[pu_type]]

        if (is.null(data_alpha) || nrow(data_alpha$alpha) == 0) {
          next
        }

        # Primary output path
        filename <- paste0('alpha_', pu_type, '_', data_alpha$calibration_year, '.rds')
        output_path_alpha <- file.path(output_dir_alpha, filename)
        saveRDS(data_alpha, output_path_alpha)

      }
    }

    # Write demand params YAML
    {
      # Build the params structure (per-type)
      params <- list(year = as.numeric(year))

      for (pu_type in names(type_results)) {
        r <- type_results[[pu_type]]
        params[[pu_type]] <- list(
          beta = r$beta,
          rho = r$rho
        )
      }

      # Write to primary output path
      yaml::write_yaml(params, output_path)

    }

    # Write calibration details YAML
    {
      output_dir_cd <- dirname(output_path)

      # Build the details structure (per-type)
      details <- list(
        year = as.numeric(year),
        price_perturbation = price_perturbation,
        income_perturbation = income_perturbation,
        income_elasticity_target = income_elasticity_target
      )

      # Add per-type calibration results
      for (pu_type in names(type_results)) {
        r <- type_results[[pu_type]]
        use_wage <- !is.na(r$target_wage_elasticity)
        details[[pu_type]] <- list(
          beta = r$beta,
          rho = r$rho,
          target_cost_elasticity = r$target_cost_elasticity,
          achieved_cost_elasticity = r$cost_participation_elasticity,
          target_wage_elasticity = r$target_wage_elasticity,
          achieved_wage_elasticity = r$wage_elasticity,
          target_income_elasticity = r$target_income_elasticity,
          achieved_income_elasticity = r$income_elasticity,
          passed_first = if (r$passed_first) 'yes' else 'no',
          passed_income = if (r$passed_income) 'yes' else 'no',
          baseline_employment = r$baseline_employment,
          population = r$population,
          convergence_code = r$convergence
        )
      }

      # Add AGI quintile elasticities if provided
      if (!is.null(agi_group_results)) {
        agi_group_list <- list()
        for (group in names(agi_group_results)) {
          r <- agi_group_results[[group]]
          agi_group_list[[group]] <- list(
            cost_participation_elasticity = r$cost_participation_elasticity,
            paid_care_elasticity = r$paid_care_elasticity,
            income_elasticity = r$income_elasticity,
            wage_elasticity = r$wage_elasticity,
            rtw_elasticity = r$rtw_elasticity,
            population = r$population
          )
        }
        details$agi_quintile_elasticities <- agi_group_list
      }

      # Header comment
      header <- paste0(
        '# Per-type demand calibration details (CRRA utility model)\n',
        '# Model: u(c) = c^(1-rho) / (1-rho)  (CRRA utility)\n',
        '#        delta = beta * [u(NI_policy) - u(NI_baseline)]\n',
        '# where NI = Y - C (net income = pre-care income - out-of-pocket care cost)\n',
        '#\n',
        '# Calibration targets:\n',
        '#   - Paid care elasticity (-0.5)\n',
        '#   - Income elasticity (-0.05)\n',
        '#\n',
        '# Five elasticities computed by AGI quintile:\n',
        '#   1. Cost (Part): d(log Employment) / d(log Cost)\n',
        '#   2. Paid Care: d(log Paid Care Hours) / d(log Cost)\n',
        '#   3. Income: d(log Work Hours) / d(log Income)\n',
        '#   4. Wage: d(log Employment) / d(log Wage)\n',
        '#   5. RTW: d(log Employment) / d(log(1-EMTR)) (elasticity w.r.t. net-of-tax rate)\n\n'
      )

      write_yaml_with_header <- function(details, path) {

        #----------------------------------------------------------------------
        # Writes a YAML file with a descriptive comment header prepended.
        #
        # Params:
        #   - details (list): data structure to serialize as YAML
        #   - path (chr): output file path
        #
        # Returns: nothing (side effects only)
        #----------------------------------------------------------------------

        yaml_content <- yaml::as.yaml(details)
        writeLines(paste0(header, yaml_content), path)
      }

      # Write to primary output path
      output_path_cd <- file.path(output_dir_cd, 'calibration_details.yaml')
      write_yaml_with_header(details, output_path_cd)

    }

    # Print concise summary
    cat('  beta =', format(beta, digits = 4), ', rho =', format(rho, digits = 4), '\n')

    invisible(type_results)
  }


  #============================================================================
  # BODY OF run_calibration()
  #============================================================================

  # Get estimation data info
  estimation_info <- get_estimation_info()


  #-------------------------------------------
  # Load processed ACS data (required for calibration)
  #-------------------------------------------

  # Check that ACS data exists
  acs_parent_units_path <- file.path(estimation_info$paths$data, 'acs_parent_units_2019.csv')
  if (!file.exists(acs_parent_units_path)) {
    stop('ACS data not found. Run run_acs_processing() first or specify --acs-interface.')
  }

  cat('Loading processed ACS data for calibration...\n')

  # Load core ACS tables needed for calibration
  processed_acs_2019 <- list()
  acs_tables <- c('households', 'household_members', 'parent_units', 'children', 'tax_units',
                  'enrollment', 'enrollment_joint')
  for (table_name in acs_tables) {
    table_path <- file.path(estimation_info$paths$data, paste0('acs_', table_name, '_2019.csv'))
    if (file.exists(table_path)) {
      processed_acs_2019[[table_name]] <- fread(table_path)
    }
  }


  #-------------------------------------------
  # Prepare calibration data
  # Wide-format data with net_income and choice probabilities for demand estimation
  #-------------------------------------------

  cat('Preparing calibration data for demand estimation...\n')

  # Get Tax-Simulator path for donor pool and wage thresholds
  tax_sim_path <- estimation_info$paths$`Tax-Simulator`

  # Load or build donor pool for 2019
  # Default behavior: always rebuild unless --use-cached-donors flag is set
  use_cached <- if (exists('use_cached_donors')) use_cached_donors else FALSE

  if (use_cached) {
    cat('  Loading donor pool from cache (--use-cached-donors flag set)...\n')
    donor_pool_result <- {
      cache_file <- file.path('./cache/donor_pools', paste0('donor_pool_', 2019, '.rds'))
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        NULL
      }
    }

    if (is.null(donor_pool_result)) {
      stop(paste0(
        'FATAL: --use-cached-donors flag set but cache does not exist.\n',
        '       Expected cache file: ./cache/donor_pools/donor_pool_2019.rds\n',
        '       Either run without --use-cached-donors to rebuild, or check cache path.'
      ))
    }

    cat('  Successfully loaded cached donor pool.\n')
  } else {
    cat('  Building donor pool from Tax-Simulator (default behavior)...\n')
    tax_file_2019 <- file.path(tax_sim_path, 'static/detail', '2019.csv')
    donor_pool_result <- prepare_donor_pool(tax_file_2019, 2019)

    # Save to cache for potential future use with --use-cached-donors
    {
      cache_dir <- './cache/donor_pools'
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      cache_file <- file.path(cache_dir, paste0('donor_pool_', 2019, '.rds'))
      saveRDS(donor_pool_result, cache_file)
    }
    cat('  Donor pool built and cached.\n')
  }

  donor_pool <- donor_pool_result$donors
  pct_thresholds <- donor_pool_result$pct_thresholds

  # Load wage thresholds for EMTR lookup
  wage_thresholds <- {
    threshold_file <- file.path(tax_sim_path, 'static', 'supplemental',
                                 paste0('percentiles_', 2019, '.csv'))
    read_csv(threshold_file, show_col_types = FALSE) %>%
      filter(year == 2019) %>%
      select(percentile, wages)
  }

  # OASI tax max for 2019 (Social Security wage base)
  tax_max_2019 <- 132900


  #-------------------------------------------
  # Load supply prices and compute price_wedge cache
  # This ensures calibration costs = simulation costs via identity:
  #   cost = price_wedge x supply_price x hours
  #-------------------------------------------

  cat('  Loading supply prices and computing price_wedge cache...\n')

  # Load supply params to get baseline prices
  supply_params_path <- file.path(output_root, 'estimation', 'supply', 'supply_2019.yaml')
  if (!file.exists(supply_params_path)) {
    stop('Supply params not found. Run run_nsece_processing() first, or use -C to specify a calibration interface.')
  }
  supply_params_yaml <- yaml::read_yaml(supply_params_path)

  # Compute baseline supply prices: P = unit_cost + delta_0
  L_req <- matrix(c(
    supply_params_yaml$labor_requirements$unpaid_center_based$no_ba,
    supply_params_yaml$labor_requirements$unpaid_center_based$ba,
    supply_params_yaml$labor_requirements$low_price_center_based$no_ba,
    supply_params_yaml$labor_requirements$low_price_center_based$ba,
    supply_params_yaml$labor_requirements$high_price_center_based$no_ba,
    supply_params_yaml$labor_requirements$high_price_center_based$ba,
    supply_params_yaml$labor_requirements$paid_home_based$no_ba,
    supply_params_yaml$labor_requirements$paid_home_based$ba
  ), nrow = 4, byrow = TRUE)

  wages <- c(supply_params_yaml$wages$no_ba, supply_params_yaml$wages$ba)
  unit_costs <- as.vector(L_req %*% wages)

  delta_0 <- c(
    0,  # Unpaid center (free)
    supply_params_yaml$per_unit_residual$low_price_center_based,
    supply_params_yaml$per_unit_residual$high_price_center_based,
    supply_params_yaml$per_unit_residual$paid_home_based
  )

  supply_prices <- unit_costs + delta_0
  supply_prices[1] <- 0  # Unpaid center is free

  cat('  Baseline supply prices: Low=$', round(supply_prices[2], 2),
      ', High=$', round(supply_prices[3], 2),
      ', Home=$', round(supply_prices[4], 2), '\n', sep = '')

  # Load price models and compute price_wedge cache for ACS parent units
  price_models <- {
    cache_dir <- './cache/price_models'
    price_qrf_models <- readRDS(file.path(cache_dir, 'price_qrf_models.rds'))
    list(
      price_qrf_models = price_qrf_models
    )
  }

  if (is.null(price_models)) {
    stop('Price models not found. Run run_nsece_processing() first.')
  }

  # Extract Other Paid base price for calibration cost consistency
  other_paid_base_price <- price_models$price_qrf_models$summary_stats$other_paid_base_price
  cat('  Other Paid base price: $', round(other_paid_base_price, 2), '/hr\n', sep = '')

  price_wedge_cache <- compute_price_wedge_cache(
    parent_units = processed_acs_2019$parent_units,
    children     = processed_acs_2019$children,
    households   = processed_acs_2019$households,
    qrf_models   = price_models$price_qrf_models
  )

  cat('  Computed price_wedge for', nrow(price_wedge_cache), 'parent units\n')

  # Optionally disable price wedge heterogeneity (set all wedges to 1.0)
  if (exists('disable_price_wedge') && isTRUE(disable_price_wedge)) {
    cat('  --no-price-wedge flag set: setting all price wedges to 1.0\n')
    price_wedge_cache <- price_wedge_cache %>%
      mutate(
        price_wedge.center_low  = 1.0,
        price_wedge.center_high = 1.0,
        price_wedge.home        = 1.0,
        price_wedge.other_paid  = 1.0
      )
  }


  calibration_data <- {
    wage_perturbation_cd <- 1.01

    cat('Preparing calibration data...\n')
    cat('  Wage perturbation for elasticity calc:', wage_perturbation_cd, '\n')

    # Validate price_wedge inputs
    if (is.null(supply_prices) || is.null(price_wedge_cache)) {
      stop('prepare_calibration_data: supply_prices and price_wedge_cache are required.\n',
           'These ensure calibration costs match simulation costs exactly.')
    }

    parent_units <- processed_acs_2019$parent_units
    children <- processed_acs_2019$children
    enrollment <- processed_acs_2019$enrollment
    enrollment_joint <- processed_acs_2019$enrollment_joint

    cat('  Imputing taxes for calibration...\n')
    parent_units_with_taxes <- parent_units %>%
      match_tax_donors(donor_pool, pct_thresholds) %>%
      calculate_taxes_with_donors(donor_pool, wage_thresholds, tax_max_2019) %>%
      mutate(
        agi = agi1 + replace_na(agi2, 0),
        total_tax = liab_iit + liab_payroll,
        # Primary caregiver's EMTR and earnings for wage perturbation
        emtr = if_else(primary_caregiver == 1 | is.na(emtr2), emtr1, emtr2),
        earnings_pc_choice = if_else(primary_caregiver == 1 | is.na(earnings2), earnings1, earnings2)
      )

    parent_units_wide <- {
      # Pivot children to wide format (child_id, age, weight by child number)
      children_wide <- children %>%
        group_by(hh_id, parent_unit_id) %>%
        arrange(age) %>%
        mutate(child_num = row_number()) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = c(hh_id, parent_unit_id),
          names_from = child_num,
          values_from = c(child_id, age, child_weight),
          names_prefix = '.',
          names_sep = ''
        ) %>%
        rename_with(~ gsub('age\\.', 'child_age.', .x))

      # Join and apply pseudofamily split
      parent_units_joined <- parent_units_with_taxes %>%
        inner_join(children_wide, by = c('hh_id', 'parent_unit_id')) %>%
        pseudofamily_split(
          n_child = 'n_children_original',
          child_weight_stub = 'child_weight',
          other_weights = c('per_weight1', 'per_weight2')
        ) %>%
        mutate(
          n_children = pmin(n_children_original, 2L),
          # Primary caregiver work hours/weeks for each employment_choice row
          hours_pc = case_when(
            n_parents == 1L ~ hours1,
            primary_caregiver == 1L ~ hours1,
            primary_caregiver == 2L ~ hours2,
            TRUE ~ hours1
          ),
          weeks_pc = case_when(
            n_parents == 1L ~ weeks1,
            primary_caregiver == 1L ~ weeks1,
            primary_caregiver == 2L ~ weeks2,
            TRUE ~ weeks1
          ),
          hours_pc = ifelse(is.na(hours_pc), 0, hours_pc),
          weeks_pc = ifelse(is.na(weeks_pc), 0, weeks_pc)
        )

      # Pivot employment-varying columns to wide format
      parent_units_joined %>%
        pivot_wider(
          id_cols = c(hh_id, parent_unit_id, pseudofamily_id,
                      n_children, n_children_original, child_category, n_parents,
                      per_weight1, per_weight2, primary_caregiver, employment_observed,
                      starts_with('child_id.'), starts_with('child_age.'),
                      starts_with('child_weight.')),
          names_from = employment_choice,
          values_from = c(agi, total_tax, p_employment, emtr, earnings_pc_choice, hours_pc, weeks_pc),
          names_sep = '.'
        )
    }

    # Join price_wedge cache to parent units
    parent_units_wide <- parent_units_wide %>%
      left_join(
        price_wedge_cache %>% select(hh_id, parent_unit_id,
                                      price_wedge.center_low,
                                      price_wedge.center_high,
                                      price_wedge.home,
                                      price_wedge.other_paid),
        by = c('hh_id', 'parent_unit_id')
      ) %>%
      # Fill missing price_wedge with 1.0 (no heterogeneity)
      mutate(
        price_wedge.center_low = coalesce(price_wedge.center_low, 1.0),
        price_wedge.center_high = coalesce(price_wedge.center_high, 1.0),
        price_wedge.home = coalesce(price_wedge.home, 1.0),
        price_wedge.other_paid = coalesce(price_wedge.other_paid, 1.0)
      )

    # Process each parent unit type
    calibration_data_inner <- list()
    for (i in seq_along(PARENT_UNIT_NAMES)) {
      calibration_data_inner[[PARENT_UNIT_NAMES[i]]] <- build_calibration_type_data(
        pu_type       = PARENT_UNIT_NAMES[i],
        n_children    = PARENT_UNIT_N_CHILDREN[i],
        child_cat     = PARENT_UNIT_CHILD_CATEGORY[i],
        parent_units_wide = parent_units_wide,
        enrollment        = enrollment,
        enrollment_joint  = enrollment_joint,
        supply_prices     = supply_prices,
        other_paid_base_price = other_paid_base_price,
        wage_perturbation = wage_perturbation_cd,
        tax_max           = tax_max_2019
      )
    }

    cat('Calibration data preparation complete.\n')
    calibration_data_inner
  }

  # Write calibration data files
  for (pu_type in names(calibration_data)) {
    if (nrow(calibration_data[[pu_type]]) > 0) {
      output_file <- file.path(
        estimation_info$paths$data,
        paste0('calibration_', pu_type, '_2019.csv')
      )
      fwrite(calibration_data[[pu_type]], output_file, na = 'NA')
    }
  }


  #-------------------------------------------
  # Run demand estimation
  # Calibrates CRRA utility model (beta, rho) to match two target elasticities.
  # Targets configured via --calib-target1-name/value and --calib-target2-name/value
  # Defaults: part_cost (-0.15) and hours_income (-0.05)
  #-------------------------------------------

  calib_targets <- {
    calib_target1_name_val <- if (exists('calib_target1_name')) calib_target1_name else 'part_cost'
    calib_target1_value_val <- if (exists('calib_target1_value')) calib_target1_value else NULL
    calib_target2_name_val <- if (exists('calib_target2_name')) calib_target2_name else 'hours_income'
    calib_target2_value_val <- if (exists('calib_target2_value')) calib_target2_value else NULL

    # Valid elasticity names
    valid_names <- c('part_wage', 'part_cost', 'paid_cost', 'hours_income')

    # Validate names
    if (!(calib_target1_name_val %in% valid_names)) {
      stop(paste('Invalid calib_target1_name:', calib_target1_name_val,
                 '\nValid options:', paste(valid_names, collapse = ', ')))
    }
    if (!(calib_target2_name_val %in% valid_names)) {
      stop(paste('Invalid calib_target2_name:', calib_target2_name_val,
                 '\nValid options:', paste(valid_names, collapse = ', ')))
    }

    # Initialize all targets as NULL (not targeted)
    targets <- list(
      care_cost_elasticity_target = NULL,
      wage_elasticity_target = NULL,
      paid_care_elasticity_target = NULL,
      income_elasticity_target = NULL
    )

    # Map short names to function parameter names
    name_to_param <- c(
      'part_wage'    = 'wage_elasticity_target',
      'part_cost'    = 'care_cost_elasticity_target',
      'paid_cost'    = 'paid_care_elasticity_target',
      'hours_income' = 'income_elasticity_target'
    )

    # Default values for each elasticity type
    defaults <- list(
      wage_elasticity_target      = DEFAULT_WAGE_ELASTICITY_TARGET,
      care_cost_elasticity_target = DEFAULT_CARE_COST_ELASTICITY_TARGET,
      paid_care_elasticity_target = DEFAULT_PAID_CARE_ELASTICITY_TARGET,
      income_elasticity_target    = -0.05
    )

    # Set first target
    param1 <- name_to_param[calib_target1_name_val]
    if (is.null(calib_target1_value_val)) {
      targets[[param1]] <- defaults[[param1]]
    } else {
      targets[[param1]] <- calib_target1_value_val
    }

    # Set second target
    param2 <- name_to_param[calib_target2_name_val]
    if (is.null(calib_target2_value_val)) {
      targets[[param2]] <- defaults[[param2]]
    } else {
      targets[[param2]] <- calib_target2_value_val
    }

    # Print what we're targeting
    cat('Calibration targets:\n')
    cat('  Target 1:', calib_target1_name_val, '=', targets[[param1]], '\n')
    cat('  Target 2:', calib_target2_name_val, '=', targets[[param2]], '\n')

    targets
  }

  run_demand_estimation(
    calibration_data_dir        = estimation_info$paths$data,
    output_path                 = file.path(estimation_info$paths$output, 'demand_params_2019.yaml'),
    year                        = 2019,
    time_stamp                  = time_stamp,
    care_cost_elasticity_target = calib_targets$care_cost_elasticity_target,
    wage_elasticity_target      = calib_targets$wage_elasticity_target,
    paid_care_elasticity_target = calib_targets$paid_care_elasticity_target,
    income_elasticity_target    = calib_targets$income_elasticity_target
  )

  cat('Calibration complete!\n')
}
