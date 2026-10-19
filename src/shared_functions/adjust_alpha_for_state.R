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
# urban density x age band (compute_tier_thresholds()), off national age bin
# median, no region (compute_tier_thresholds_age_bin()); no state source counts
# them, state
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
                                   targets_csv_path, state_postal,
                                   caregivers = NULL) {

  #----------------------------------------------------------------------------
  # Re-anchors calibrated alpha matrices to state base-year targets. Solves,
  # jointly across both parent unit types over state's households,
  #
  #     delta_t <- delta_t + log(H_t^target / H_t)          (care types)
  #     s_g     <- s_g + logit(e_g^target) - logit(e_g)     (caregiver groups)
  #
  # under p_ij proportional to
  #     p0_ij * exp(sum_c delta_{type_c(j)} + s_{g(i)} * working_j)
  # H_t = aggregate annual hours in care type t; e_g = caregiver employment
  # rate of targeting group g (pc_group). e_g^target = base-year forest rate:
  # p0's working-choice mass (= employment forest p_employment.pt + .ft),
  # weighted as compute_baseline_employment_rates() (3b). Care alone
  # (caregivers NULL) moves employment off forest rate -- care, work correlated
  # in p0 -- yearly employment targets grow from forest rate, so yearly shifts
  # would undo care targets. Joint solve hits both in base year; yearly shifts
  # then carry only change since 2019.
  #
  # Fails loudly on: zero base support for a targeted type, a state row w/out
  # caregiver group, and non-convergence within
  # STATE_DEMAND_CONTRACTION_MAX_ITER iterations.
  #
  # Params:
  #   - alpha_data_by_type (list): Per pu type, calibration alpha rds contents
  #       (alpha, p0, child_weights, row_ids). p0, child_weights required.
  #   - state_hh_ids (vec): hh_ids of state's households
  #   - targets_csv_path (chr): Path to config/state/demand/<ST>.csv
  #   - state_postal (chr): State postal abbreviation
  #   - caregivers (df or NULL): one row per parent unit -- hh_id,
  #       parent_unit_id, pc_group, per_weight_pc (caregiver_groups_for_state()).
  #       NULL = care targets only
  #
  # Returns: (list) with elements:
  #   - alpha_by_type (list): Per pu type: alpha subset to state rows, shifted
  #       by solved deltas + group shifts (stored normalization preserved)
  #   - row_ids_by_type (list): Per pu type: row_ids subset to state rows
  #   - deltas (named num vec): Solved utility constants by care type
  #   - employment_shifts (named num vec or NULL): Solved shift by group
  #   - employment_targets, employment_achieved (named num vec or NULL)
  #   - achieved (named num vec): Model aggregate hours at solution
  #   - targets (named num vec): Target aggregate hours
  #   - sector_hours (named num vec): Base-year hours by sector at solution
  #   - n_iter (int): Iterations used
  #----------------------------------------------------------------------------

  targets <- read_state_demand_targets(targets_csv_path, state_postal)
  target_types <- names(targets)
  joint <- !is.null(caregivers)
  key_of <- function(hh, pu) paste(hh, pu, sep = '|')
  if (joint) {
    need <- c('hh_id', 'parent_unit_id', 'pc_group', 'per_weight_pc')
    if (!all(need %in% names(caregivers))) {
      stop('adjust_alpha_for_state: caregivers needs columns ', paste(need, collapse = ', '), '.')
    }
    cg_key <- key_of(caregivers$hh_id, caregivers$parent_unit_id)
    if (anyDuplicated(cg_key)) stop('adjust_alpha_for_state: caregivers has duplicate parent units.')
  }

  # Subset each type's p0 / weights to state's rows; choice-to-hours maps;
  # caregiver group + weight per row (unit weight / unit's pseudofamily rows)
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

    sd <- c(
      build_choice_hours_map(n_children, target_types),
      build_sector_hours_map(n_children),
      list(p0 = ad$p0[in_state, , drop = FALSE], w1 = w1, w2 = w2,
           in_state = in_state, working = get_working_choice_mask(n_children))
    )
    if (joint) {
      ri <- ad$row_ids[in_state, , drop = FALSE]
      rk <- key_of(ri$hh_id, ri$parent_unit_id)
      m <- match(rk, cg_key)
      if (anyNA(m)) {
        stop('adjust_alpha_for_state (', state_postal, '): ', sum(is.na(m)), ' ', pu_type,
             ' rows w/out caregiver group -- caregivers table misses parent units.')
      }
      n_rows <- as.vector(table(rk)[rk])
      cw <- caregivers$per_weight_pc[m]
      sd$group <- caregivers$pc_group[m]
      sd$cg_weight <- replace(cw, is.na(cw), 0) / n_rows
    }
    state_data[[pu_type]] <- sd
  }

  if (length(state_data) == 0) {
    stop('No calibration households found for state ', state_postal, '.')
  }

  groups <- if (joint) {
    sort(unique(unlist(lapply(state_data, function(sd) sd$group[sd$cg_weight > 0]))))
  } else character(0)

  # Probabilities at given deltas + group shifts
  tilted <- function(sd, deltas, shifts) {
    U <- sweep(sd$p0, 2, exp(sd$d_by_choice(deltas)), '*')
    if (length(shifts)) {
      f <- exp(unname(shifts[sd$group]))
      f[is.na(f)] <- 1
      U[, sd$working] <- U[, sd$working, drop = FALSE] * f
    }
    U / rowSums(U)
  }

  # Aggregate hours by care type + employment rate by group
  aggregates <- function(deltas, shifts) {
    H <- setNames(rep(0, length(target_types)), target_types)
    num <- setNames(rep(0, length(groups)), groups)
    den <- num
    for (sd in state_data) {
      P <- tilted(sd, deltas, shifts)
      H <- H + weighted_child_hours(P, sd$C1, sd$C2, sd$w1, sd$w2)
      if (joint) {
        e <- rowSums(P[, sd$working, drop = FALSE])
        gf <- factor(sd$group, levels = groups)
        # as.vector: tapply gives 1-d array, breaks row-wise multiply in tilted()
        num <- num + as.vector(tapply(e * sd$cg_weight, gf, sum, default = 0))
        den <- den + as.vector(tapply(sd$cg_weight, gf, sum, default = 0))
      }
    }
    list(H = H, e = if (joint) num / den else NULL)
  }

  deltas <- setNames(rep(0, length(target_types)), target_types)
  shifts <- setNames(rep(0, length(groups)), groups)

  # Zero-support check: a targeted type with no base-year probability mass in
  # the state cannot be reached by any finite delta. Employment targets =
  # base-year (p0) rates by construction
  a0 <- aggregates(deltas, shifts)
  zero_support <- target_types[a0$H <= 0]
  if (length(zero_support) > 0) {
    stop('State demand adjustment for ', state_postal, ': care type(s) with ',
         'zero base-year support in the state: ',
         paste(zero_support, collapse = ', '),
         '. No finite adjustment can hit a positive target; check the ',
         'imputation and the target CSV.')
  }
  emp_targets <- a0$e
  logit <- function(p) log(p / (1 - p))

  # Contraction
  a <- a0
  n_iter <- 0L
  repeat {
    gap <- log(targets / a$H)
    gap_e <- if (joint) logit(emp_targets) - logit(a$e) else numeric(0)

    # Hours can hit zero mid-contraction despite passing zero-support check: a
    # large delta on one type can underflow another's mass. Infinite gap would
    # make max(abs(gap)) NA -- R's generic 'missing value where TRUE/FALSE
    # needed' names no state, type, or cause.
    if (any(!is.finite(gap)) || any(!is.finite(gap_e))) {
      bad <- c(target_types[!is.finite(gap)], groups[!is.finite(gap_e)])
      stop('State demand contraction for ', state_postal, ': non-finite gap ',
           'at iteration ', n_iter, ' for ', paste(bad, collapse = ', '),
           '. Modelled hours were ',
           paste(sprintf('%s=%.4g', target_types, a$H), collapse = ', '),
           '. A care type or caregiver group has lost all probability mass, ',
           'which usually means the state holds too few parent units. ',
           'Raise --sim-sample, or check the target CSV.')
    }

    if (max(abs(c(gap, gap_e))) < STATE_DEMAND_CONTRACTION_TOL) break

    n_iter <- n_iter + 1L
    if (n_iter > STATE_DEMAND_CONTRACTION_MAX_ITER) {
      all_gap <- c(gap, gap_e)
      worst <- which.max(abs(all_gap))
      stop('State demand contraction for ', state_postal, ' did not converge ',
           'in ', STATE_DEMAND_CONTRACTION_MAX_ITER, ' iterations. Worst target: ',
           names(all_gap)[worst], ' (log gap ', sprintf('%.3e', all_gap[worst]), ').')
    }

    deltas <- deltas + gap
    if (joint) shifts <- shifts + gap_e
    a <- aggregates(deltas, shifts)
  }
  H <- a$H

  # Sector hours at solution (SECTOR_MAPPING order): centre total pinned,
  # low/high split model's own. Caller builds base-year labor from these --
  # wage curve starts at base-year wage by construction. Children +
  # part-time children by target group -- contraction matches hours only, so
  # counts + PT shares are model's own (memo check)
  sector_hours <- setNames(rep(0, nrow(SECTOR_MAPPING)), SECTOR_MAPPING$ecec_type)
  pt_hours <- unname(HOURS_ANNUAL['pt'])
  in_care <- function(M) if (is.null(M)) NULL else (M > 0) * 1
  in_pt   <- function(M) if (is.null(M)) NULL else (M > 0 & abs(M - pt_hours) < 1e-6) * 1
  children <- setNames(rep(0, length(target_types)), target_types)
  children_pt <- children
  for (sd in state_data) {
    P <- tilted(sd, deltas, shifts)
    sector_hours <- sector_hours + weighted_child_hours(P, sd$S1, sd$S2, sd$w1, sd$w2)
    children    <- children + weighted_child_hours(P, in_care(sd$C1), in_care(sd$C2), sd$w1, sd$w2)
    children_pt <- children_pt + weighted_child_hours(P, in_pt(sd$C1), in_pt(sd$C2), sd$w1, sd$w2)
  }

  cat('  State demand adjustment (', state_postal, '): converged in ',
      n_iter, ' iterations', if (joint) ' (care hours + caregiver employment)' else '',
      '\n', sep = '')
  for (t in target_types) {
    cat(sprintf('    %-26s delta=%+.4f  target=%.3e  achieved=%.3e  children=%.0f  pt_share=%.3f\n',
                t, deltas[t], targets[t], H[t], children[t],
                if (children[t] > 0) children_pt[t] / children[t] else NA_real_))
  }
  if (joint) {
    for (g in groups) {
      cat(sprintf('    employment %-15s shift=%+.4f  target=%.4f  achieved=%.4f\n',
                  g, shifts[g], emp_targets[g], a$e[g]))
    }
  }

  # Shift stored alpha. Stored alpha has K-1 columns (choice 1 dropped, rows
  # normalized to choice 1): column j = choice j+1, shift preserving the
  # normalization = d_{j+1} - d_1, plus row's group shift on working choices
  # (relative to choice 1)
  alpha_by_type <- list()
  row_ids_by_type <- list()
  for (pu_type in names(state_data)) {
    sd <- state_data[[pu_type]]
    ad <- alpha_data_by_type[[pu_type]]

    d_choice <- sd$d_by_choice(deltas)
    alpha_state <- sweep(ad$alpha[sd$in_state, , drop = FALSE], 2, d_choice[-1] - d_choice[1], '+')
    if (joint) {
      s_row <- unname(shifts[sd$group])
      s_row[is.na(s_row)] <- 0
      wk <- as.numeric(sd$working)
      alpha_state <- alpha_state + outer(s_row, wk[-1] - wk[1])
    }
    alpha_by_type[[pu_type]] <- alpha_state
    row_ids_by_type[[pu_type]] <- ad$row_ids[sd$in_state, , drop = FALSE]
  }

  list(
    alpha_by_type = alpha_by_type,
    row_ids_by_type = row_ids_by_type,
    deltas = deltas,
    employment_shifts = if (joint) shifts else NULL,
    employment_targets = emp_targets,
    employment_achieved = a$e,
    achieved = H,
    targets = targets,
    sector_hours = sector_hours,
    n_iter = n_iter
  )
}



caregiver_groups_for_state <- function(parent_units) {

  #----------------------------------------------------------------------------
  # Caregiver targeting group + weight per base-year parent unit, same rule as
  # prepare_parent_units() (3b): pc_group = sex x age bin of primary
  # caregiver (u25, 25_35, 35_45, 45plus), per_weight_pc = caregiver's
  # person weight.
  #
  # Params:
  #   - parent_units (df): base-year parent units -- hh_id, parent_unit_id,
  #       primary_caregiver, male1/2, age1/2, per_weight1/2 (one or more rows
  #       per unit; must agree within unit)
  #
  # Returns: (tibble) hh_id, parent_unit_id, pc_group, per_weight_pc -- one
  #   row per parent unit
  #----------------------------------------------------------------------------

  need <- c('hh_id', 'parent_unit_id', 'primary_caregiver', 'male1', 'male2',
            'age1', 'age2', 'per_weight1', 'per_weight2')
  miss <- setdiff(need, names(parent_units))
  if (length(miss)) {
    stop('caregiver_groups_for_state: parent_units lacks ', paste(miss, collapse = ', '), '.')
  }
  out <- parent_units %>%
    dplyr::transmute(
      hh_id, parent_unit_id,
      pc_male = dplyr::if_else(primary_caregiver == 1, male1, male2),
      pc_age  = dplyr::if_else(primary_caregiver == 1, age1, age2),
      per_weight_pc = dplyr::if_else(primary_caregiver == 1, per_weight1, per_weight2),
      pc_group = paste0(dplyr::if_else(pc_male == 1, 'M', 'F'), '_',
                        dplyr::case_when(pc_age < 25 ~ 'u25', pc_age < 35 ~ '25_35',
                                         pc_age < 45 ~ '35_45', TRUE ~ '45plus'))) %>%
    dplyr::distinct(hh_id, parent_unit_id, pc_group, per_weight_pc)
  if (anyDuplicated(out[c('hh_id', 'parent_unit_id')])) {
    stop('caregiver_groups_for_state: caregiver group or weight differs within a parent unit.')
  }
  if (any(!out$pc_group %in% EMPLOYMENT_TARGETING_GROUPS)) {
    stop('caregiver_groups_for_state: caregiver w/ group outside EMPLOYMENT_TARGETING_GROUPS.')
  }
  out
}



