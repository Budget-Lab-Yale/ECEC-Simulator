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


# Target groups: a state target file names a GROUP of model care types. All
# singletons except centre care -- tiers fall below or at/above provider-file
# line (1a): urban density on (state runs) density-adjusted line for region x
# urban density x age band (compute_tier_thresholds()), off region x age band
# median; no state source counts them, state
# sources support only the SUM. One delta for the pair pins combined hours, no
# tier-share target: a common additive shift keeps each family's odds between
# tiers, aggregate split still moves as families reweight (model determines it).
STATE_TARGET_GROUPS <- list(
  'Center-Based'        = c('Low-Priced Center-Based', 'High-Priced Center-Based'),
  'Unpaid Center-Based' = 'Unpaid Center-Based',
  'Paid Home-Based'     = 'Paid Home-Based',
  'Other Paid'          = 'Other Paid',
  'Other Unpaid'        = 'Other Unpaid',
  'Unpaid Home-Based'   = 'Unpaid Home-Based')



state_target_group_of <- function(ecec_type) {

  #----------------------------------------------------------------------------
  # Care type -> target group; NA where ungrouped (incl. 'Parent Only').
  #
  # Params:
  #   - ecec_type (chr vec)
  #
  # Returns: (chr vec) group name or NA
  #----------------------------------------------------------------------------

  lookup <- setNames(rep(names(STATE_TARGET_GROUPS), lengths(STATE_TARGET_GROUPS)),
                     unlist(STATE_TARGET_GROUPS, use.names = FALSE))
  unname(lookup[ecec_type])
}



read_state_demand_targets <- function(targets_csv_path, state_postal) {

  #----------------------------------------------------------------------------
  # Reads and validates a state demand target CSV (2019 annual hours by care
  # type). All 6 target GROUPS required, positive; file naming a centre
  # price band rejected.
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

  target_types <- names(STATE_TARGET_GROUPS)

  # Naming a price band means file invented a split
  split_rows <- intersect(targets_df$ecec_type,
                          STATE_TARGET_GROUPS[['Center-Based']])
  if (length(split_rows) > 0) {
    stop('State demand targets for ', state_postal, ' name ',
         paste(split_rows, collapse = ' and '), '. State sources do not ',
         'support a within-state centre price split -- give one combined ',
         '"Center-Based" row instead. Both bands share one adjustment.')
  }

  missing_types <- setdiff(target_types, targets_df$ecec_type)
  extra_types <- setdiff(targets_df$ecec_type, target_types)
  if (length(missing_types) > 0) {
    stop('State demand targets for ', state_postal, ' missing target group(s): ',
         paste(missing_types, collapse = ', '),
         '. All 6 target groups are required (Parent Only is the numeraire).')
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
  #   - target_types (chr vec): The 6 target groups
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
    grp <- state_target_group_of(catalog[[type_col]])
    C <- matrix(0, nrow = n_choices, ncol = n_types,
                dimnames = list(NULL, target_types))
    for (t in target_types) {
      # Both centre bands map to one group -- both fill that column
      rows_t <- which(!is.na(grp) & grp == t)
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
  # Both centre bands take same group's shift -- odds between them fixed
  g1 <- state_target_group_of(catalog$child1_ecec_type)
  g2 <- if (n_children == 2) state_target_group_of(catalog$child2_ecec_type) else NULL

  d_by_choice <- function(deltas) {
    d1 <- deltas[match(g1, target_types)]
    d1[is.na(d1)] <- 0
    d <- d1
    if (n_children == 2) {
      d2 <- deltas[match(g2, target_types)]
      d2[is.na(d2)] <- 0
      d <- d + d2
    }
    unname(d)
  }

  list(C1 = C1, C2 = C2, d_by_choice = d_by_choice)
}



build_sector_hours_map <- function(n_children) {

  #----------------------------------------------------------------------------
  # Per-child (n_choices x 4) annual-hours matrices by SUPPLY sector
  # (SECTOR_MAPPING order, centre bands apart). Same hours as
  # build_choice_hours_map() -- sector hours at solved deltas sum to achieved
  # target hours.
  #
  # Params:
  #   - n_children (int): 1 or 2
  #
  # Returns: (list) S1, S2 (matrix; S2 NULL for 1 child)
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)
  sectors <- SECTOR_MAPPING$ecec_type

  # S[j, s] = child's hours under choice j if j places child in sector s
  sector_matrix <- function(child) {
    hours <- unname(HOURS_ANNUAL[catalog[[paste0('child', child, '_hours_choice')]]])
    type  <- catalog[[paste0('child', child, '_ecec_type')]]
    S <- matrix(0, nrow = nrow(catalog), ncol = length(sectors),
                dimnames = list(NULL, sectors))
    for (s in sectors) {
      rows_s <- which(type == s)
      S[rows_s, s] <- hours[rows_s]
    }
    S
  }

  list(S1 = sector_matrix(1), S2 = if (n_children == 2) sector_matrix(2) else NULL)
}



weighted_child_hours <- function(P, M1, M2, w1, w2) {

  #----------------------------------------------------------------------------
  # Weighted aggregate hours by column of per-child hours maps:
  # sum_i w_ci * (P %*% M_c)[i, ], summed over children. Weights fold into P
  # first -- crossprod(w, P) is 1 x n_choices, so no n_units x n_types
  # intermediate. Algebraically same as colSums(P %*% M * w).
  #
  # Params:
  #   - P (matrix): n_units x n_choices choice probabilities
  #   - M1, M2 (matrix): n_choices x n_cols hours maps per child (M2 NULL for
  #       1-child units)
  #   - w1, w2 (num vec): Per-unit child weights (w2 NULL for 1-child units)
  #
  # Returns: (named num vec) Hours by column of M1
  #----------------------------------------------------------------------------

  h <- crossprod(w1, P) %*% M1
  if (!is.null(M2)) {
    h <- h + crossprod(w2, P) %*% M2
  }
  setNames(as.vector(h), colnames(M1))
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
  #   - sector_hours (named num vec): Base-year hours by sector at solution
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
      build_sector_hours_map(n_children),
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
      H <- H + weighted_child_hours(P, sd$C1, sd$C2, sd$w1, sd$w2)
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

    # Hours can hit zero mid-contraction despite passing zero-support check: a
    # large delta on one type can underflow another's mass. Infinite gap would
    # make max(abs(gap)) NA -- R's generic 'missing value where TRUE/FALSE
    # needed' names no state, type, or cause.
    if (any(!is.finite(gap))) {
      bad <- target_types[!is.finite(gap)]
      stop('State demand contraction for ', state_postal, ': non-finite gap ',
           'at iteration ', n_iter, ' for ', paste(bad, collapse = ', '),
           '. Modelled hours were ',
           paste(sprintf('%s=%.4g', target_types, H), collapse = ', '),
           '. A care type has lost all probability mass, which usually means ',
           'the state holds too few parent units to support every type. ',
           'Raise --sim-sample, or check the target CSV.')
    }

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

  # Sector hours at solution (SECTOR_MAPPING order): centre total pinned,
  # low/high split model's own. Caller builds base-year labor from these --
  # wage curve starts at base-year wage by construction.
  sector_hours <- setNames(rep(0, nrow(SECTOR_MAPPING)), SECTOR_MAPPING$ecec_type)
  for (sd in state_data) {
    E <- exp(sd$d_by_choice(deltas))
    U <- sweep(sd$p0, 2, E, '*')
    P <- U / rowSums(U)
    sector_hours <- sector_hours +
      weighted_child_hours(P, sd$S1, sd$S2, sd$w1, sd$w2)
  }

  # Children + part-time children by target group at solution -- contraction
  # matches hours only, so counts + PT shares are model's own (memo check)
  pt_hours <- unname(HOURS_ANNUAL['pt'])
  in_care <- function(M) if (is.null(M)) NULL else (M > 0) * 1
  in_pt   <- function(M) if (is.null(M)) NULL else (M > 0 & abs(M - pt_hours) < 1e-6) * 1
  children <- setNames(rep(0, length(target_types)), target_types)
  children_pt <- children
  for (sd in state_data) {
    E <- exp(sd$d_by_choice(deltas))
    U <- sweep(sd$p0, 2, E, '*')
    P <- U / rowSums(U)
    children    <- children + weighted_child_hours(P, in_care(sd$C1), in_care(sd$C2), sd$w1, sd$w2)
    children_pt <- children_pt + weighted_child_hours(P, in_pt(sd$C1), in_pt(sd$C2), sd$w1, sd$w2)
  }

  cat('  State demand adjustment (', state_postal, '): converged in ',
      n_iter, ' iterations\n', sep = '')
  for (t in target_types) {
    cat(sprintf('    %-26s delta=%+.4f  target=%.3e  achieved=%.3e  children=%.0f  pt_share=%.3f\n',
                t, deltas[t], targets[t], H[t], children[t],
                if (children[t] > 0) children_pt[t] / children[t] else NA_real_))
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
    sector_hours = sector_hours,
    n_iter = n_iter
  )
}



#------------------------------------------------------------------------------
# Experiment: two-count re-anchoring (env var ECEC_STATE_CALIBRATION=counts,
# read in config.R). Targets children by target group x hours band (pt, ft)
# -- children + part-time share per group -- in place of 6 annual-hours
# targets. 12 deltas, one per group x band ('Parent Only' numeraire; empty
# band -> one group delta, read_state_count_targets()); centre
# bands still share group's delta within each band. Counts on simulator's
# hours grid imply same annual hours as demand/<ST>.csv, so hours targets hold
# too. Targets: config/state/demand_counts/<ST>.csv (ecec_type, children,
# part_time), 00_build_demand_targets.R. Default path above untouched.
#------------------------------------------------------------------------------



get_state_calibration_mode <- function() {

  #----------------------------------------------------------------------------
  # Active re-anchoring mode -- global state_calibration_mode (config.R), else
  # env var ECEC_STATE_CALIBRATION, else 'hours'.
  #
  # Returns: (chr) 'hours' or 'counts'
  #----------------------------------------------------------------------------

  m <- if (exists('state_calibration_mode', envir = globalenv())) {
    get('state_calibration_mode', envir = globalenv())
  } else {
    Sys.getenv('ECEC_STATE_CALIBRATION', 'hours')
  }
  if (is.null(m) || m == '') m <- 'hours'
  if (!m %in% c('hours', 'counts')) {
    stop('ECEC_STATE_CALIBRATION = "', m, '" unknown. Use hours (default) or counts.')
  }
  m
}



state_count_targets_path <- function(targets_csv_path) {

  #----------------------------------------------------------------------------
  # Count-target file beside hours file: config/state/demand/<ST>.csv ->
  # config/state/demand_counts/<ST>.csv.
  #
  # Params:
  #   - targets_csv_path (chr): hours target path
  #
  # Returns: (chr) count target path
  #----------------------------------------------------------------------------

  file.path(dirname(dirname(targets_csv_path)), 'demand_counts',
            basename(targets_csv_path))
}



read_state_count_targets <- function(counts_csv_path, state_postal) {

  #----------------------------------------------------------------------------
  # Reads, validates count targets: 6 target groups, children > 0, 0 <=
  # part_time <= children. Group w/ zero children in one band (e.g. DE Unpaid
  # Home-Based: build's p0 holds no part-time child) -> one '<group> | all'
  # cell on children alone -- zero target needs infinite delta.
  #
  # Params:
  #   - counts_csv_path (chr): config/state/demand_counts/<ST>.csv
  #   - state_postal (chr): for error messages
  #
  # Returns: (named num vec) children by cell '<group> | <band>' (band pt,
  #   ft, or all), 12 cells unless a band is empty
  #----------------------------------------------------------------------------

  if (!file.exists(counts_csv_path)) {
    stop('ECEC_STATE_CALIBRATION=counts needs ', counts_csv_path, ' for ',
         state_postal, '. Build it: Rscript state_data/code/00_build_demand_targets.R --all')
  }
  df <- read_csv(counts_csv_path, show_col_types = FALSE)
  if (!all(c('ecec_type', 'children', 'part_time') %in% names(df))) {
    stop('Count target file ', counts_csv_path,
         ' must have columns: ecec_type, children, part_time')
  }
  groups <- names(STATE_TARGET_GROUPS)
  if (!setequal(df$ecec_type, groups) || any(duplicated(df$ecec_type))) {
    stop('Count targets for ', state_postal, ' must name each of the 6 target ',
         'groups once: ', paste(groups, collapse = ', '))
  }
  df <- df[match(groups, df$ecec_type), ]
  ft <- df$children - df$part_time
  if (any(!is.finite(c(df$children, df$part_time))) || any(df$children <= 0) ||
      any(df$part_time < 0) || any(ft < -1e-9 * df$children)) {
    stop('Count targets for ', state_postal, ': children must be positive, ',
         'part_time between 0 and children, for every group.')
  }
  targets <- c()
  for (i in seq_along(groups)) {
    if (df$part_time[i] > 0 && ft[i] > 0) {
      targets[paste(groups[i], 'pt', sep = ' | ')] <- df$part_time[i]
      targets[paste(groups[i], 'ft', sep = ' | ')] <- ft[i]
    } else {
      targets[paste(groups[i], 'all', sep = ' | ')] <- df$children[i]
    }
  }
  targets
}



build_choice_count_map <- function(n_children, cells) {

  #----------------------------------------------------------------------------
  # Per-child (n_choices x n_cells) indicator maps: C_c[j, k] = 1 if choice j
  # places child c in cell k (target group x hours band), plus utility shift
  # by choice.
  #
  # Params:
  #   - n_children (int): 1 or 2
  #   - cells (chr vec): '<group> | <band>' names
  #
  # Returns: (list) C1, C2 (C2 NULL for 1 child), d_by_choice (fn(deltas) ->
  #   length-n_choices shift, sum of children's cell deltas)
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)

  # Cell index by choice: group x band, else group's 'all' cell, else NA
  cell_idx <- function(child) {
    grp <- state_target_group_of(catalog[[paste0('child', child, '_ecec_type')]])
    k <- match(paste(grp, catalog[[paste0('child', child, '_hours_choice')]],
                     sep = ' | '), cells)
    k_all <- match(paste(grp, 'all', sep = ' | '), cells)
    k[is.na(k) & !is.na(grp)] <- k_all[is.na(k) & !is.na(grp)]
    k
  }
  k1 <- cell_idx(1)
  k2 <- if (n_children == 2) cell_idx(2) else NULL

  indicator <- function(k) {
    C <- matrix(0, nrow = nrow(catalog), ncol = length(cells),
                dimnames = list(NULL, cells))
    C[cbind(which(!is.na(k)), k[!is.na(k)])] <- 1
    C
  }

  d_by_choice <- function(deltas) {
    d <- deltas[k1]
    d[is.na(d)] <- 0
    if (n_children == 2) {
      d2 <- deltas[k2]
      d2[is.na(d2)] <- 0
      d <- d + d2
    }
    unname(d)
  }

  list(C1 = indicator(k1), C2 = if (n_children == 2) indicator(k2) else NULL,
       d_by_choice = d_by_choice)
}



adjust_alpha_for_state_counts <- function(alpha_data_by_type, state_hh_ids,
                                          targets_csv_path, state_postal) {

  #----------------------------------------------------------------------------
  # Two-count re-anchoring (experiment). Same interface, return shape as
  # adjust_alpha_for_state(); contraction on children by cell:
  #
  #     delta_k <- delta_k + log(N_k^target / N_k(delta))
  #
  # Params:
  #   - alpha_data_by_type (list): per pu type, alpha rds (alpha, p0,
  #       child_weights, row_ids)
  #   - state_hh_ids (vec): hh_ids of state's households
  #   - targets_csv_path (chr): config/state/demand/<ST>.csv -- count file
  #       found beside it (state_count_targets_path())
  #   - state_postal (chr)
  #
  # Returns: (list) alpha_by_type, row_ids_by_type, deltas (12 cells),
  #   achieved (children by cell), targets (children by cell), sector_hours,
  #   n_iter
  #----------------------------------------------------------------------------

  targets <- read_state_count_targets(state_count_targets_path(targets_csv_path),
                                      state_postal)
  cells <- names(targets)

  state_data <- list()
  for (pu_type in names(alpha_data_by_type)) {
    ad <- alpha_data_by_type[[pu_type]]
    if (is.null(ad)) next
    if (is.null(ad$p0) || is.null(ad$child_weights)) {
      stop('Alpha data for ', pu_type, ' lacks p0/child_weights -- re-run calibration.')
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
    state_data[[pu_type]] <- c(build_choice_count_map(n_children, cells),
                               build_sector_hours_map(n_children),
                               list(p0 = ad$p0[in_state, , drop = FALSE],
                                    w1 = w1, w2 = w2, in_state = in_state))
  }
  if (length(state_data) == 0) {
    stop('No calibration households found for state ', state_postal, '.')
  }

  # Probabilities at deltas, one pu type
  probs <- function(sd, deltas) {
    U <- sweep(sd$p0, 2, exp(sd$d_by_choice(deltas)), '*')
    U / rowSums(U)
  }
  aggregate_counts <- function(deltas) {
    N <- setNames(rep(0, length(cells)), cells)
    for (sd in state_data) {
      N <- N + weighted_child_hours(probs(sd, deltas), sd$C1, sd$C2, sd$w1, sd$w2)
    }
    N
  }

  deltas <- setNames(rep(0, length(cells)), cells)
  N <- aggregate_counts(deltas)
  if (any(N <= 0)) {
    stop('Two-count adjustment for ', state_postal, ': zero base-year support in ',
         paste(cells[N <= 0], collapse = ', '), '.')
  }
  n_iter <- 0L
  repeat {
    gap <- log(targets / N)
    if (any(!is.finite(gap))) {
      stop('Two-count contraction for ', state_postal, ': non-finite gap at ',
           'iteration ', n_iter, ' for ', paste(cells[!is.finite(gap)], collapse = ', '),
           '. Raise --sim-sample, or check the count file.')
    }
    if (max(abs(gap)) < STATE_DEMAND_CONTRACTION_TOL) break
    n_iter <- n_iter + 1L
    if (n_iter > STATE_DEMAND_CONTRACTION_MAX_ITER) {
      worst <- which.max(abs(gap))
      stop('Two-count contraction for ', state_postal, ' did not converge in ',
           STATE_DEMAND_CONTRACTION_MAX_ITER, ' iterations. Worst cell: ',
           cells[worst], ' (log gap ', sprintf('%.3e', gap[worst]), ').')
    }
    deltas <- deltas + gap
    N <- aggregate_counts(deltas)
  }

  # Sector hours at solution -> base-year labor (3a), as hours path
  sector_hours <- setNames(rep(0, nrow(SECTOR_MAPPING)), SECTOR_MAPPING$ecec_type)
  for (sd in state_data) {
    sector_hours <- sector_hours +
      weighted_child_hours(probs(sd, deltas), sd$S1, sd$S2, sd$w1, sd$w2)
  }

  cat('  State demand adjustment, two-count (', state_postal, '): converged in ',
      n_iter, ' iterations\n', sep = '')
  for (k in cells) {
    cat(sprintf('    %-32s delta=%+.4f  target=%.0f  achieved=%.0f children\n',
                k, deltas[k], targets[k], N[k]))
  }

  # Shift stored alpha (K-1 cols, normalized to choice 1), as hours path
  alpha_by_type <- list()
  row_ids_by_type <- list()
  for (pu_type in names(state_data)) {
    sd <- state_data[[pu_type]]
    ad <- alpha_data_by_type[[pu_type]]
    d_choice <- sd$d_by_choice(deltas)
    alpha_by_type[[pu_type]] <- sweep(ad$alpha[sd$in_state, , drop = FALSE], 2,
                                      d_choice[-1] - d_choice[1], '+')
    row_ids_by_type[[pu_type]] <- ad$row_ids[sd$in_state, , drop = FALSE]
  }

  list(alpha_by_type = alpha_by_type, row_ids_by_type = row_ids_by_type,
       deltas = deltas, achieved = N, targets = targets,
       sector_hours = sector_hours, n_iter = n_iter)
}
