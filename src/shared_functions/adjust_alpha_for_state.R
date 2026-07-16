#------------------------------------------------------------------------------
# adjust_alpha_for_state.R
#
# State demand re-anchoring for state-level analysis (see
# docs/state_level_analysis.md, section 5).
#
# Solves one utility constant per care type (delta_t, 'Parent Only' is the
# numeraire with delta = 0) such that base-year aggregate annual hours by care
# type over the state's households match a target CSV, then shifts the stored
# alpha matrices by the solved deltas. Because alpha is the model's base-year
# anchor and is carried into every simulation year and scenario, this makes
# the state adjustment structural: baseline and counterfactual move together.
#------------------------------------------------------------------------------


read_state_demand_targets <- function(targets_csv_path, state_postal) {

  #----------------------------------------------------------------------------
  # Reads and validates a state demand target CSV (2019 annual hours by care
  # type). All 7 non-parental care types are required with positive hours.
  #
  # Params:
  #   - targets_csv_path (chr): Path to config/state/demand/<ST>.csv
  #   - state_postal (chr): State postal abbreviation (for error messages)
  #
  # Returns: (named num vec) Annual hours by ecec_type
  #----------------------------------------------------------------------------

  targets_df <- read_csv(targets_csv_path, show_col_types = FALSE)

  required_cols <- c('ecec_type', 'annual_hours')
  if (!all(required_cols %in% names(targets_df))) {
    stop('State demand target file ', targets_csv_path,
         ' must have columns: ', paste(required_cols, collapse = ', '))
  }

  target_types <- setdiff(unique(CHILD_CARE_CHOICES$ecec_type), 'Parent Only')

  missing_types <- setdiff(target_types, targets_df$ecec_type)
  extra_types <- setdiff(targets_df$ecec_type, target_types)
  if (length(missing_types) > 0) {
    stop('State demand targets for ', state_postal, ' missing care type(s): ',
         paste(missing_types, collapse = ', '),
         '. All 7 non-parental care types are required (Parent Only is the numeraire).')
  }
  if (length(extra_types) > 0) {
    stop('State demand targets for ', state_postal, ' contain unknown care type(s): ',
         paste(extra_types, collapse = ', '))
  }
  if (any(duplicated(targets_df$ecec_type))) {
    stop('State demand targets for ', state_postal, ' contain duplicate care types.')
  }

  targets <- setNames(targets_df$annual_hours, targets_df$ecec_type)[target_types]

  if (any(is.na(targets)) || any(targets <= 0)) {
    stop('State demand targets for ', state_postal, ' must be positive annual ',
         'hours for every care type. A zero target would require an infinite ',
         'utility penalty; use a small positive value if near-zero is intended.')
  }

  targets
}



build_choice_hours_map <- function(n_children, target_types) {

  #----------------------------------------------------------------------------
  # Builds, for one parent unit type, the per-child (n_choices x n_types)
  # annual-hours contribution matrices used by the state demand contraction:
  # C_c[j, t] = annual hours of child c under choice j if choice j places
  # child c in care type t, else 0.
  #
  # Params:
  #   - n_children (int): Number of children (1 or 2)
  #   - target_types (chr vec): The 7 targeted (non-parental) care types
  #
  # Returns: (list) with elements:
  #   - C1, C2 (matrix or NULL): Per-child hours contribution matrices
  #   - d_by_choice (fn): function(deltas) returning the length-n_choices
  #       utility shift d_j = sum over children of delta at child's care type
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)
  n_choices <- nrow(catalog)
  n_types <- length(target_types)

  contribution_matrix <- function(type_col, hours_col) {
    hours <- unname(HOURS_ANNUAL[catalog[[hours_col]]])
    C <- matrix(0, nrow = n_choices, ncol = n_types,
                dimnames = list(NULL, target_types))
    for (t in target_types) {
      rows_t <- which(catalog[[type_col]] == t)
      C[rows_t, t] <- hours[rows_t]
    }
    C
  }

  C1 <- contribution_matrix('child1_ecec_type', 'child1_hours_choice')
  C2 <- if (n_children == 2) {
    contribution_matrix('child2_ecec_type', 'child2_hours_choice')
  } else {
    NULL
  }

  # Utility shift by choice: deltas at each child's care type ('Parent Only'
  # and any non-targeted type contribute 0)
  d_by_choice <- function(deltas) {
    d1 <- deltas[match(catalog$child1_ecec_type, target_types)]
    d1[is.na(d1)] <- 0
    d <- d1
    if (n_children == 2) {
      d2 <- deltas[match(catalog$child2_ecec_type, target_types)]
      d2[is.na(d2)] <- 0
      d <- d + d2
    }
    unname(d)
  }

  list(C1 = C1, C2 = C2, d_by_choice = d_by_choice)
}



adjust_alpha_for_state <- function(alpha_data_by_type, state_hh_ids,
                                   targets_csv_path, state_postal) {

  #----------------------------------------------------------------------------
  # Re-anchors the calibrated alpha matrices to state-level base-year care
  # aggregates. Solves the contraction
  #
  #     delta_t <- delta_t + log(H_t^target / H_t(delta))
  #
  # jointly across both parent unit types over the state's households, where
  # H_t(delta) is aggregate annual hours in care type t under probabilities
  # p_ij(delta) proportional to p0_ij * exp(sum_c delta_{type_c(j)}).
  #
  # Fails loudly on: zero base support for a targeted type, and
  # non-convergence within STATE_DEMAND_CONTRACTION_MAX_ITER iterations.
  #
  # Params:
  #   - alpha_data_by_type (list): Per pu type, the calibration alpha rds
  #       contents (alpha, p0, child_weights, row_ids). p0 and child_weights
  #       are required (interfaces predating state support lack them).
  #   - state_hh_ids (vec): hh_ids of the state's households
  #   - targets_csv_path (chr): Path to config/state/demand/<ST>.csv
  #   - state_postal (chr): State postal abbreviation
  #
  # Returns: (list) with elements:
  #   - alpha_by_type (list): Per pu type: alpha matrix subset to state rows,
  #       shifted by the solved deltas (stored normalization preserved)
  #   - row_ids_by_type (list): Per pu type: row_ids subset to state rows
  #   - deltas (named num vec): Solved utility constants by care type
  #   - achieved (named num vec): Model aggregate hours at the solution
  #   - targets (named num vec): Target aggregate hours
  #   - n_iter (int): Contraction iterations used
  #----------------------------------------------------------------------------

  targets <- read_state_demand_targets(targets_csv_path, state_postal)
  target_types <- names(targets)

  # Subset each type's p0 / weights to the state's rows and precompute the
  # choice-to-hours maps
  state_data <- list()
  for (pu_type in names(alpha_data_by_type)) {
    ad <- alpha_data_by_type[[pu_type]]
    if (is.null(ad)) next

    if (is.null(ad$p0) || is.null(ad$child_weights)) {
      stop('Alpha data for ', pu_type, ' lacks p0/child_weights. State-level ',
           'runs require an interface generated after state support was added; ',
           're-run calibration to regenerate alpha files.')
    }

    n_children <- PARENT_UNIT_N_CHILDREN[match(pu_type, PARENT_UNIT_NAMES)]
    in_state <- ad$row_ids$hh_id %in% state_hh_ids
    if (!any(in_state)) next

    w <- ad$child_weights[in_state, , drop = FALSE]
    w1 <- replace(w[['child_weight.1']], is.na(w[['child_weight.1']]), 0)
    w2 <- if ('child_weight.2' %in% names(w)) {
      replace(w[['child_weight.2']], is.na(w[['child_weight.2']]), 0)
    } else {
      NULL
    }

    state_data[[pu_type]] <- c(
      build_choice_hours_map(n_children, target_types),
      list(p0 = ad$p0[in_state, , drop = FALSE], w1 = w1, w2 = w2,
           in_state = in_state)
    )
  }

  if (length(state_data) == 0) {
    stop('No calibration households found for state ', state_postal, '.')
  }

  # Aggregate annual hours by care type at a given delta vector
  aggregate_hours <- function(deltas) {
    H <- setNames(rep(0, length(target_types)), target_types)
    for (sd in state_data) {
      E <- exp(sd$d_by_choice(deltas))
      U <- sweep(sd$p0, 2, E, '*')
      P <- U / rowSums(U)
      H <- H + colSums(P %*% sd$C1 * sd$w1)
      if (!is.null(sd$C2)) {
        H <- H + colSums(P %*% sd$C2 * sd$w2)
      }
    }
    H
  }

  # Zero-support check: a targeted type with no base-year probability mass in
  # the state cannot be reached by any finite delta
  H0 <- aggregate_hours(setNames(rep(0, length(target_types)), target_types))
  zero_support <- target_types[H0 <= 0]
  if (length(zero_support) > 0) {
    stop('State demand adjustment for ', state_postal, ': care type(s) with ',
         'zero base-year support in the state: ',
         paste(zero_support, collapse = ', '),
         '. No finite adjustment can hit a positive target; check the ',
         'imputation and the target CSV.')
  }

  # Contraction
  deltas <- setNames(rep(0, length(target_types)), target_types)
  H <- H0
  n_iter <- 0L
  repeat {
    gap <- log(targets / H)
    if (max(abs(gap)) < STATE_DEMAND_CONTRACTION_TOL) break

    n_iter <- n_iter + 1L
    if (n_iter > STATE_DEMAND_CONTRACTION_MAX_ITER) {
      worst <- which.max(abs(gap))
      stop('State demand contraction for ', state_postal, ' did not converge ',
           'in ', STATE_DEMAND_CONTRACTION_MAX_ITER, ' iterations. Worst type: ',
           target_types[worst], ' (log gap ', sprintf('%.3e', gap[worst]), ').')
    }

    deltas <- deltas + gap
    H <- aggregate_hours(deltas)
  }

  cat('  State demand adjustment (', state_postal, '): converged in ',
      n_iter, ' iterations\n', sep = '')
  for (t in target_types) {
    cat(sprintf('    %-26s delta=%+.4f  target=%.3e  achieved=%.3e\n',
                t, deltas[t], targets[t], H[t]))
  }

  # Shift the stored alpha matrices. Stored alpha has K-1 columns (choice 1
  # dropped, rows normalized to choice 1), so column j corresponds to choice
  # j+1 and the shift preserving the normalization is d_{j+1} - d_1.
  alpha_by_type <- list()
  row_ids_by_type <- list()
  for (pu_type in names(state_data)) {
    sd <- state_data[[pu_type]]
    ad <- alpha_data_by_type[[pu_type]]

    d_choice <- sd$d_by_choice(deltas)
    shift <- d_choice[-1] - d_choice[1]

    alpha_state <- ad$alpha[sd$in_state, , drop = FALSE]
    alpha_by_type[[pu_type]] <- sweep(alpha_state, 2, shift, '+')
    row_ids_by_type[[pu_type]] <- ad$row_ids[sd$in_state, , drop = FALSE]
  }

  list(
    alpha_by_type = alpha_by_type,
    row_ids_by_type = row_ids_by_type,
    deltas = deltas,
    achieved = H,
    targets = targets,
    n_iter = n_iter
  )
}
