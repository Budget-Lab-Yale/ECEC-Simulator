#------------------------------------------------------------------------------
# rationing.R
#
# Offer-state machinery for quantity-limited (rationed) public programs.
#
# A rationed demand policy limits program slots by paid sector. Offers are
# drawn PER ELIGIBLE CHILD, per rationed sector, as independent Bernoulli
# draws with sector-specific offer rates r. The equilibrium treats offer
# rates as additional equilibrium objects: r is solved (nested inside each
# price evaluation) so that expected program take-up equals the slot count,
# or r = 1 with slack when the program is undersubscribed.
#
# Key structural facts exploited here:
#   - State-conditional choice probabilities p_s do not depend on r; only the
#     mixture weights w_s(r) do. So per price vector we compute per-state
#     aggregates (Qd_s, takeup_s) once, and solving r is a cheap root-find
#     on precomputed quantities.
#   - The component pipeline is column-local: choice column k's utility under
#     state s depends only on whether child 1 holds an offer for the sector
#     child 1 uses in column k (and same for child 2). So at most 4 variant
#     utility matrices are needed; every state assembles column-wise from them.
#   - Households partition by eligibility pattern: ineligible units face a
#     single state; one-eligible-child units face 2^|R| states; both-eligible
#     units face 4^|R| states (R = sectors with slots).
#
# Take-up counts weighted CHILDREN (slot = child): a child consumes a slot in
# sector j iff it holds an offer for j and enrolls in j (pt or ft).
#
# Called from: 3c (run_scenario equilibrium + discrete stage), 3d (mechanical
# effect), offer-aware employment-rate helpers
#------------------------------------------------------------------------------



get_rationing_slots <- function(rationing, year) {

  #----------------------------------------------------------------------------
  # Resolves the slot vector for a simulation year from rationing metadata.
  # Slots may be a numeric vector (constant across years) or a function of
  # year (slot schedule, e.g. a program ramp).
  #
  # Params:
  #   - rationing (list): Rationing metadata from the policy file with $slots
  #   - year (int): Simulation year
  #
  # Returns: (num vec) Length-4 slot counts by market sector (sector 1 must
  #   be 0; only paid sectors 2:4 can be rationed)
  #----------------------------------------------------------------------------

  slots <- rationing$slots
  if (is.function(slots)) {
    slots <- slots(year)
  }

  if (!is.numeric(slots) || length(slots) != 4 || any(!is.finite(slots)) || any(slots < 0)) {
    stop('get_rationing_slots: rationing$slots must resolve to a non-negative ',
         'numeric vector of length 4 (by market sector).')
  }
  if (slots[1] != 0) {
    stop('get_rationing_slots: sector 1 (Unpaid Center-Based) cannot be rationed; ',
         'slots[1] must be 0.')
  }

  unname(slots)
}



all_offer_subsets <- function(sectors) {

  #----------------------------------------------------------------------------
  # Enumerates all subsets of a sector vector (the possible offer sets for
  # one eligible child).
  #
  # Params:
  #   - sectors (int vec): Rationed sector ids (possibly empty)
  #
  # Returns: (list) All subsets, each an integer vector (includes empty set)
  #----------------------------------------------------------------------------

  n <- length(sectors)
  if (n == 0) return(list(integer(0)))

  flags <- expand.grid(rep(list(c(FALSE, TRUE)), n))
  lapply(seq_len(nrow(flags)), function(i) sectors[unlist(flags[i, ])])
}



offer_state_weight <- function(r, k, m) {

  #----------------------------------------------------------------------------
  # Mixture weight of an offer state: prod_j r_j^k_j * (1 - r_j)^m_j, where
  # k_j = eligible children holding an offer for sector j in this state and
  # m_j = eligible children not holding one. Sectors with k = m = 0 contribute
  # a factor of 1 (0^0 = 1 in R via ^).
  #
  # Params:
  #   - r (num vec): Length-4 offer rates by sector
  #   - k (int vec): Length-4 offer counts for this state
  #   - m (int vec): Length-4 no-offer counts for this state
  #
  # Returns: (dbl) State probability given offer rates
  #----------------------------------------------------------------------------

  prod(r^k * (1 - r)^m)
}



iterate_offer_states <- function(P, pu_df, n_children, pu_name, demand_params,
                                 demand_policy, policy_cdctc, cpi_growth_factor,
                                 employment_shifts, rationed_sectors, callback) {

  #----------------------------------------------------------------------------
  # Core iterator: partitions one parent unit dataframe by program eligibility
  # pattern, computes offer-variant utility matrices (at most 4 per subgroup),
  # enumerates offer states, assembles each state's utility matrix column-wise
  # from the variants, computes state-conditional choice probabilities, and
  # invokes the callback per state.
  #
  # Params:
  #   - P (num vec): Price vector (length 4)
  #   - pu_df (df): Parent units dataframe for a single type
  #   - n_children (int): Number of children (1 or 2)
  #   - pu_name (chr): Parent unit type name
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - demand_policy (list): Loaded demand policy object with $do_demand_policy
  #       and $is_child_program_eligible
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor
  #   - employment_shifts (num vec): Fixed employment shifts (or NULL)
  #   - rationed_sectors (int vec): Sector ids with slots > 0 (possibly empty)
  #   - callback (fn): Called once per (subgroup, state) as
  #       callback(idx, p_s, k_j, m_j, s1, s2, e1, e2) where idx are pu_df row
  #       indices of the subgroup, p_s the state-conditional probability
  #       matrix for those rows, k_j/m_j the length-4 weight exponents, s1/s2
  #       the offer sets, and e1/e2 the subgroup eligibility flags
  #
  # Returns: nothing (side effects via callback only)
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)
  j1 <- catalog$child1_market_sector_id
  j2 <- if (n_children == 2) catalog$child2_market_sector_id else NULL

  # Program eligibility per child (framework-level lottery participation)
  elig_fn <- demand_policy$is_child_program_eligible
  elig1 <- as.logical(elig_fn(pu_df, 1))
  elig2 <- if (n_children == 2) as.logical(elig_fn(pu_df, 2)) else rep(FALSE, nrow(pu_df))
  elig1[is.na(elig1)] <- FALSE
  elig2[is.na(elig2)] <- FALSE

  # Offer-variant utilities on the FULL dataframe (base matrices computed once
  # and reused across variants). The policy applies per-child eligibility
  # internally, so ineligible units' variant utilities equal their no-offer
  # utilities; computing on the full dataframe also keeps the base-matrix
  # sanity assertions (price-wedge variation) meaningful for tiny subgroups.
  variant_flags <- list('00' = c(FALSE, FALSE))
  if (any(elig1)) variant_flags[['10']] <- c(TRUE, FALSE)
  if (any(elig2)) variant_flags[['01']] <- c(FALSE, TRUE)
  if (any(elig1 & elig2)) variant_flags[['11']] <- c(TRUE, TRUE)

  base <- NULL
  V_var <- list()
  for (key in names(variant_flags)) {
    fl <- variant_flags[[key]]
    um <- compute_policy_utility_matrix(
      P, pu_df, n_children, demand_params, pu_name,
      demand_policy$do_demand_policy, policy_cdctc, cpi_growth_factor,
      employment_shifts = employment_shifts,
      offered1 = fl[1], offered2 = fl[2], base = base
    )
    base <- um$base
    V_var[[key]] <- um$V
  }

  alpha_full <- match_alpha_to_parent_units(pu_df, demand_params[[pu_name]], pu_name)

  pattern <- as.integer(elig1) + 2L * as.integer(elig2)

  for (pat in sort(unique(pattern))) {
    idx <- which(pattern == pat)
    e1 <- pat %in% c(1L, 3L)
    e2 <- pat %in% c(2L, 3L)
    alpha_sub <- alpha_full[idx, , drop = FALSE]

    # Enumerate offer states for this eligibility pattern
    s1_sets <- if (e1) all_offer_subsets(rationed_sectors) else list(integer(0))
    s2_sets <- if (e2) all_offer_subsets(rationed_sectors) else list(integer(0))

    for (s1 in s1_sets) {
      # Column-wise: child 1 holds an offer for the sector used in column k
      a_k <- e1 & !is.na(j1) & (j1 %in% s1)

      for (s2 in s2_sets) {
        b_k <- if (e2) !is.na(j2) & (j2 %in% s2) else rep(FALSE, nrow(catalog))

        # Assemble state utility matrix column-wise from variants
        V_s <- V_var[['00']][idx, , drop = FALSE]
        if (e1 && any(a_k & !b_k)) V_s[, a_k & !b_k] <- V_var[['10']][idx, a_k & !b_k, drop = FALSE]
        if (e2 && any(!a_k & b_k)) V_s[, !a_k & b_k] <- V_var[['01']][idx, !a_k & b_k, drop = FALSE]
        if (e1 && e2 && any(a_k & b_k)) V_s[, a_k & b_k] <- V_var[['11']][idx, a_k & b_k, drop = FALSE]

        p_s <- compute_probs_from_alpha(alpha_sub, V_s)

        # Mixture weight exponents by sector
        k_j <- rep(0L, 4)
        m_j <- rep(0L, 4)
        n_elig <- as.integer(e1) + as.integer(e2)
        for (j in rationed_sectors) {
          k_j[j] <- as.integer(e1 && (j %in% s1)) + as.integer(e2 && (j %in% s2))
          m_j[j] <- n_elig - k_j[j]
        }

        callback(idx, p_s, k_j, m_j, s1, s2, e1, e2)
      }
    }
  }

  invisible(NULL)
}



compute_offer_state_aggregates <- function(P, parent_units_list, demand_params,
                                           demand_policy, policy_cdctc,
                                           cpi_growth_factor, employment_shifts,
                                           rationed_sectors) {

  #----------------------------------------------------------------------------
  # Computes per-offer-state demand and take-up aggregates at prices P for all
  # parent unit types. These aggregates are all the nested offer-rate solve
  # needs: total demand and take-up are analytic (polynomial) functions of r
  # given them.
  #
  # Params:
  #   - P (num vec): Price vector (length 4)
  #   - parent_units_list (list): Named list of parent unit dataframes by type
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - demand_policy (list): Loaded demand policy object
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor
  #   - employment_shifts (num vec): Fixed employment shifts (or NULL)
  #   - rationed_sectors (int vec): Sector ids with slots > 0
  #
  # Returns: (list) with elements:
  #   - base_Qd (num vec): Length-4 demand hours from states with weight 1
  #       (ineligible subgroups and, when no sectors are rationed, all states)
  #   - states (list): Per lottery-relevant state: Qd (4-vec, hours),
  #       takeup (4-vec, weighted children), k/m (4-vec weight exponents)
  #----------------------------------------------------------------------------

  base_Qd <- rep(0, 4)
  states <- list()

  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    n_children <- PARENT_UNIT_N_CHILDREN[i]
    pu_df <- parent_units_list[[pu_name]]
    if (is.null(pu_df) || nrow(pu_df) == 0) next

    catalog <- get_choice_catalog(n_children)
    sector_hours <- get_sector_hours_matrices(n_children)
    j1 <- catalog$child1_market_sector_id
    j2 <- if (n_children == 2) catalog$child2_market_sector_id else NULL

    W1_all <- pu_df[['child_weight.1']]
    if (any(!is.finite(W1_all))) {
      stop('compute_offer_state_aggregates: non-finite child_weight.1 (', pu_name, ').')
    }
    W2_all <- if (n_children == 2) pu_df[['child_weight.2']] else NULL
    if (n_children == 2 && any(!is.finite(W2_all))) {
      stop('compute_offer_state_aggregates: non-finite child_weight.2 (', pu_name, ').')
    }

    iterate_offer_states(
      P, pu_df, n_children, pu_name, demand_params,
      demand_policy, policy_cdctc, cpi_growth_factor,
      employment_shifts, rationed_sectors,
      callback = function(idx, p_s, k_j, m_j, s1, s2, e1, e2) {

        W1 <- W1_all[idx]
        Qd_s <- colSums(W1 * (p_s %*% sector_hours$H1))
        if (n_children == 2) {
          W2 <- W2_all[idx]
          Qd_s <- Qd_s + colSums(W2 * (p_s %*% sector_hours$H2))
        }

        # Degenerate state (no lottery variation): contributes with weight 1
        if (all(k_j == 0L) && all(m_j == 0L)) {
          base_Qd <<- base_Qd + Qd_s
          return(invisible(NULL))
        }

        # Take-up: offered children enrolling in the offered sector (child count)
        takeup_s <- rep(0, 4)
        for (j in rationed_sectors) {
          if (e1 && (j %in% s1)) {
            cols_j <- which(!is.na(j1) & j1 == j)
            takeup_s[j] <- takeup_s[j] + sum(W1 * rowSums(p_s[, cols_j, drop = FALSE]))
          }
          if (e2 && (j %in% s2)) {
            cols_j2 <- which(!is.na(j2) & j2 == j)
            takeup_s[j] <- takeup_s[j] + sum(W2_all[idx] * rowSums(p_s[, cols_j2, drop = FALSE]))
          }
        }

        states[[length(states) + 1L]] <<- list(
          Qd = Qd_s, takeup = takeup_s, k = k_j, m = m_j
        )
        invisible(NULL)
      }
    )
  }

  list(base_Qd = base_Qd, states = states)
}



rationed_takeup_at <- function(states, r) {

  #----------------------------------------------------------------------------
  # Total expected program take-up by sector at offer rates r, summing
  # mixture-weighted per-state take-up.
  #
  # Params:
  #   - states (list): State aggregates from compute_offer_state_aggregates
  #   - r (num vec): Length-4 offer rates
  #
  # Returns: (num vec) Length-4 expected take-up (weighted children)
  #----------------------------------------------------------------------------

  takeup <- rep(0, 4)
  for (s in states) {
    takeup <- takeup + offer_state_weight(r, s$k, s$m) * s$takeup
  }
  takeup
}



solve_offer_rates <- function(states, slots, rationed_sectors, r_start = NULL,
                              tol = 1e-9, max_iter = 200) {

  #----------------------------------------------------------------------------
  # Solves sector offer rates r so that expected take-up equals the slot count
  # in each rationed sector, subject to the complementarity condition: if the
  # program is undersubscribed even at r_j = 1, the corner r_j = 1 holds with
  # slack (take-up < slots).
  #
  # Take-up in sector j is monotone increasing in r_j and weakly decreasing in
  # other sectors' rates (offer competition), so projected Gauss-Seidel with a
  # scalar root-find per sector converges.
  #
  # Params:
  #   - states (list): State aggregates from compute_offer_state_aggregates
  #   - slots (num vec): Length-4 slot counts by sector
  #   - rationed_sectors (int vec): Sector ids with slots > 0
  #   - r_start (num vec or NULL): Warm-start rates (length 4)
  #   - tol (dbl): uniroot tolerance on r
  #   - max_iter (int): Maximum Gauss-Seidel sweeps
  #
  # Returns: (num vec) Length-4 offer rates (0 for non-rationed sectors)
  #----------------------------------------------------------------------------

  r <- rep(0, 4)
  if (length(rationed_sectors) == 0) {
    return(r)
  }

  # No lottery-relevant states (no eligible families): all offers extended,
  # zero take-up, corner solution with full slack
  if (length(states) == 0) {
    r[rationed_sectors] <- 1
    return(r)
  }

  r[rationed_sectors] <- if (!is.null(r_start)) {
    pmin(pmax(r_start[rationed_sectors], 0), 1)
  } else {
    0.5
  }

  for (iter in seq_len(max_iter)) {
    r_prev <- r

    for (j in rationed_sectors) {
      gap_j <- function(rj) {
        rr <- r
        rr[j] <- rj
        rationed_takeup_at(states, rr)[j] - slots[j]
      }

      if (gap_j(1) <= 0) {
        # Undersubscribed: everyone eligible holds an offer, slots have slack
        r[j] <- 1
      } else {
        # gap_j(0) = 0 - slots_j < 0 and gap_j(1) > 0: root bracketed in (0, 1)
        r[j] <- uniroot(gap_j, interval = c(0, 1), tol = tol)$root
      }
    }

    if (max(abs(r - r_prev)) < 1e-10) break
  }

  r
}



solve_rationed_equilibrium_demand <- function(P, parent_units_list, demand_params,
                                              demand_policy, policy_cdctc,
                                              cpi_growth_factor, employment_shifts,
                                              slots, r_start = NULL) {

  #----------------------------------------------------------------------------
  # Computes aggregate demand under a quantity-limited program at prices P:
  # builds per-state aggregates, solves the offer rates, and returns the
  # mixture demand plus rationing diagnostics.
  #
  # This is the rationed replacement for the per-type get_total_demand loop
  # inside the equilibrium objective.
  #
  # Params:
  #   - P (num vec): Price vector (length 4)
  #   - parent_units_list (list): Named list of parent unit dataframes by type
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - demand_policy (list): Loaded demand policy object (with rationing)
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor
  #   - employment_shifts (num vec): Fixed employment shifts (or NULL)
  #   - slots (num vec): Length-4 slot counts by sector for this year
  #   - r_start (num vec or NULL): Warm-start offer rates from prior evaluation
  #
  # Returns: (list) with elements:
  #   - Qd (num vec): Length-4 demand hours at solved offer rates
  #   - offer_rates (num vec): Length-4 solved offer rates
  #   - takeup (num vec): Length-4 expected take-up (weighted children)
  #   - rationed_sectors (int vec): Sector ids with slots > 0
  #----------------------------------------------------------------------------

  rationed_sectors <- which(slots > 0)

  aggs <- compute_offer_state_aggregates(
    P, parent_units_list, demand_params, demand_policy, policy_cdctc,
    cpi_growth_factor, employment_shifts, rationed_sectors
  )

  r <- solve_offer_rates(aggs$states, slots, rationed_sectors, r_start = r_start)

  Qd <- aggs$base_Qd
  takeup <- rep(0, 4)
  for (s in aggs$states) {
    w_s <- offer_state_weight(r, s$k, s$m)
    Qd <- Qd + w_s * s$Qd
    takeup <- takeup + w_s * s$takeup
  }

  if (any(!is.finite(Qd)) || any(Qd < 0)) {
    stop('solve_rationed_equilibrium_demand: non-finite or negative mixture Qd.')
  }

  list(
    Qd = Qd,
    offer_rates = r,
    takeup = takeup,
    rationed_sectors = rationed_sectors
  )
}



get_rationed_prob_matrix <- function(P, parent_units_df, n_children, demand_params,
                                     pu_name, demand_policy, policy_cdctc,
                                     cpi_growth_factor, employment_shifts,
                                     offer_rates, rationed_sectors) {

  #----------------------------------------------------------------------------
  # Computes the expected (offer-mixture) choice probability matrix for one
  # parent unit type under a rationed policy at given offer rates:
  #   p_bar = sum_s w_s(r) * p_s
  #
  # All linear aggregates (sector demand, employment rates) are exact
  # functions of p_bar, which makes this the offer-aware replacement for
  # get_demand_prob_matrix in diagnostic helpers.
  #
  # Params:
  #   - P (num vec): Price vector (length 4)
  #   - parent_units_df (df): Parent units dataframe for a single type
  #   - n_children (int): Number of children (1 or 2)
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - pu_name (chr): Parent unit type name
  #   - demand_policy (list): Loaded demand policy object (with rationing)
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor
  #   - employment_shifts (num vec): Fixed employment shifts (or NULL)
  #   - offer_rates (num vec): Length-4 offer rates (from the equilibrium)
  #   - rationed_sectors (int vec): Sector ids with slots > 0
  #
  # Returns: (matrix) n_units x n_choices expected probability matrix
  #----------------------------------------------------------------------------

  n_choices <- if (n_children == 1) N_CHOICES_1_CHILD else N_CHOICES_2_CHILD
  p_bar <- matrix(0, nrow = nrow(parent_units_df), ncol = n_choices)

  iterate_offer_states(
    P, parent_units_df, n_children, pu_name, demand_params,
    demand_policy, policy_cdctc, cpi_growth_factor,
    employment_shifts, rationed_sectors,
    callback = function(idx, p_s, k_j, m_j, s1, s2, e1, e2) {
      w_s <- offer_state_weight(offer_rates, k_j, m_j)
      p_bar[idx, ] <<- p_bar[idx, ] + w_s * p_s
      invisible(NULL)
    }
  )

  # Rows must sum to 1 (mixture of probability distributions)
  stopifnot(all(abs(rowSums(p_bar) - 1.0) < 1e-8))

  p_bar
}



get_policy_offer_draws <- function(baseline_pu, policy_pu, n_children, rationed_sectors) {

  #----------------------------------------------------------------------------
  # Joins the policy run's persisted offer draws onto baseline records
  # (Option A: the mechanical/distributional/poverty decompositions evaluate
  # policy rules using the SAME lottery draws as the behavioral run, matched
  # record-for-record by household and epsilon draw).
  #
  # Params:
  #   - baseline_pu (df): Baseline collapsed parent units (with epsilon_id)
  #   - policy_pu (df): Policy collapsed parent units with offer_child*.j cols
  #   - n_children (int): Number of children (1 or 2)
  #   - rationed_sectors (int vec): Sector ids with slots > 0
  #
  # Returns: (df) One row per baseline_pu row (same order) with the
  #   offer_child*.j columns
  #----------------------------------------------------------------------------

  join_keys <- c('hh_id', 'parent_unit_id', 'pseudofamily_id', 'epsilon_id')
  offer_cols <- c(paste0('offer_child1.', rationed_sectors),
                  if (n_children == 2) paste0('offer_child2.', rationed_sectors))

  missing_cols <- setdiff(c(join_keys, offer_cols), names(policy_pu))
  if (length(missing_cols) > 0) {
    stop('get_policy_offer_draws: policy result is missing columns: ',
         paste(missing_cols, collapse = ', '))
  }

  n_before <- nrow(baseline_pu)
  offers_df <- baseline_pu %>%
    select(all_of(join_keys)) %>%
    left_join(policy_pu %>% select(all_of(c(join_keys, offer_cols))), by = join_keys)

  stopifnot(nrow(offers_df) == n_before)
  if (any(is.na(as.matrix(offers_df[, offer_cols])))) {
    stop('get_policy_offer_draws: baseline records without matching policy offer ',
         'draws (join on ', paste(join_keys, collapse = ', '), ' failed).')
  }

  offers_df
}



extract_offer_conditional_values <- function(pu_clean, P, n_children, demand_params,
                                             policy_demand_obj, policy_cdctc,
                                             cpi_growth_factor, choices, offers_df,
                                             rationed_sectors) {

  #----------------------------------------------------------------------------
  # Evaluates a rationed policy's components at prices P and extracts scalar
  # values at each record's given choice, conditional on the record's offer
  # state (variant-selected subsidy/CDCTC; offer-independent agi/taxes/cost).
  #
  # Used by the mechanical fiscal, distributional, and poverty decompositions,
  # which evaluate policy rules at baseline behavior.
  #
  # Params:
  #   - pu_clean (df): Stripped parent units with policy tax rules applied
  #   - P (num vec): Price vector to evaluate at (baseline or policy prices)
  #   - n_children (int): Number of children (1 or 2)
  #   - demand_params (list): Demand model parameters
  #   - policy_demand_obj (list): Loaded rationed demand policy object
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor
  #   - choices (int vec): Choice index per record (e.g. baseline choices)
  #   - offers_df (df): Offer draws per record from get_policy_offer_draws
  #   - rationed_sectors (int vec): Sector ids with slots > 0
  #
  # Returns: (list) Scalar vectors per record: agi, taxes, gross_ecec_cost,
  #   subsidy, cdctc, net_income, plus offered1/offered2 (offer status at the
  #   given choice's sectors)
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)
  j1 <- catalog$child1_market_sector_id
  j2 <- if (n_children == 2) catalog$child2_market_sector_id else NULL
  elig_fn <- policy_demand_obj$is_child_program_eligible

  n_rec <- length(choices)

  # Evaluate a contiguous block of records. Base matrices are n x n_choices and
  # dominate peak memory at production scale (c2plus: 675 choices), so we
  # evaluate rows in chunks and concatenate the per-record vectors. Every step
  # here is row-local and every output is per-record, so chunking is
  # numerically identical to evaluating all rows at once; only peak memory
  # (one chunk's base matrices, not the full parent-unit count) changes.
  eval_chunk <- function(idx) {

    pu_ch      <- pu_clean[idx, , drop = FALSE]
    choices_ch <- choices[idx]
    offers_ch  <- offers_df[idx, , drop = FALSE]
    n_ch       <- length(idx)

    # Eligibility determines which offer variants are reachable in this chunk
    elig1 <- as.logical(elig_fn(pu_ch, 1))
    elig1[is.na(elig1)] <- FALSE
    elig2 <- if (n_children == 2) {
      e <- as.logical(elig_fn(pu_ch, 2))
      e[is.na(e)] <- FALSE
      e
    } else {
      rep(FALSE, n_ch)
    }

    variant_flags <- list('00' = c(FALSE, FALSE))
    if (any(elig1)) variant_flags[['10']] <- c(TRUE, FALSE)
    if (any(elig2)) variant_flags[['01']] <- c(FALSE, TRUE)
    if (any(elig1 & elig2)) variant_flags[['11']] <- c(TRUE, TRUE)

    # Record-diagonal selector: [i, choices[i]] per record. Collapsing each
    # variant to this per-record vector immediately (rather than retaining a
    # full subsidy AND CDCTC matrix per offer variant) keeps only one
    # base-matrix set live at a time.
    sel <- cbind(seq_len(n_ch), choices_ch)

    base_mats <- NULL
    var_comp <- list()
    for (key in names(variant_flags)) {
      fl <- variant_flags[[key]]
      pcv <- compute_policy_components(pu_ch, P, n_children, demand_params,
                                        policy_demand_obj$do_demand_policy, policy_cdctc,
                                        cpi_growth_factor,
                                        offered1 = fl[1], offered2 = fl[2],
                                        base = base_mats, build_components = FALSE,
                                        check_subsidy_variation = FALSE)
      base_mats <- pcv$base
      var_comp[[key]] <- list(subsidy = pcv$subsidy_matrix[sel],
                              cdctc   = pcv$cdctc_matrix[sel])
    }

    # Offer status at each record's chosen sectors
    j1_k <- j1[choices_ch]
    offered1 <- rep(FALSE, n_ch)
    for (j in rationed_sectors) {
      rows <- which(!is.na(j1_k) & j1_k == j)
      if (length(rows) > 0) {
        offered1[rows] <- offers_ch[[paste0('offer_child1.', j)]][rows] == 1
      }
    }
    offered2 <- rep(FALSE, n_ch)
    if (n_children == 2) {
      j2_k <- j2[choices_ch]
      for (j in rationed_sectors) {
        rows <- which(!is.na(j2_k) & j2_k == j)
        if (length(rows) > 0) {
          offered2[rows] <- offers_ch[[paste0('offer_child2.', j)]][rows] == 1
        }
      }
    }

    # Variant-selected subsidy/CDCTC at the record's choice (var_comp entries
    # are already collapsed to the per-record selected value).
    subsidy <- var_comp[['00']]$subsidy
    cdctc <- var_comp[['00']]$cdctc
    m10 <- offered1 & !offered2
    if (any(m10)) {
      subsidy[m10] <- var_comp[['10']]$subsidy[m10]
      cdctc[m10] <- var_comp[['10']]$cdctc[m10]
    }
    if (n_children == 2) {
      m01 <- !offered1 & offered2
      if (any(m01)) {
        subsidy[m01] <- var_comp[['01']]$subsidy[m01]
        cdctc[m01] <- var_comp[['01']]$cdctc[m01]
      }
      m11 <- offered1 & offered2
      if (any(m11)) {
        subsidy[m11] <- var_comp[['11']]$subsidy[m11]
        cdctc[m11] <- var_comp[['11']]$cdctc[m11]
      }
    }

    list(
      agi = base_mats$agi_matrix[sel],
      taxes = base_mats$taxes_matrix[sel],
      gross_ecec_cost = base_mats$gross_ecec_cost_matrix[sel],
      subsidy = subsidy,
      cdctc = cdctc,
      offered1 = offered1,
      offered2 = offered2
    )
  }

  # Preallocate per-record outputs and fill chunk by chunk
  agi <- numeric(n_rec); taxes <- numeric(n_rec); gross_ecec_cost <- numeric(n_rec)
  subsidy <- numeric(n_rec); cdctc <- numeric(n_rec)
  offered1 <- logical(n_rec); offered2 <- logical(n_rec)

  if (n_rec > 0) {
    for (s in seq(1L, n_rec, by = MFC_CHUNK_ROWS)) {
      idx <- s:min(s + MFC_CHUNK_ROWS - 1L, n_rec)
      r <- eval_chunk(idx)
      agi[idx]             <- r$agi
      taxes[idx]           <- r$taxes
      gross_ecec_cost[idx] <- r$gross_ecec_cost
      subsidy[idx]         <- r$subsidy
      cdctc[idx]           <- r$cdctc
      offered1[idx]        <- r$offered1
      offered2[idx]        <- r$offered2
    }
  }

  list(
    agi = agi,
    taxes = taxes,
    gross_ecec_cost = gross_ecec_cost,
    subsidy = subsidy,
    cdctc = cdctc,
    net_income = agi - taxes - gross_ecec_cost + subsidy + cdctc,
    offered1 = offered1,
    offered2 = offered2
  )
}



prepare_rationed_pu_variants <- function(pu_df, P, n_children, pu_name, demand_params,
                                         demand_policy, policy_cdctc, cpi_growth_factor,
                                         employment_shifts) {

  #----------------------------------------------------------------------------
  # Precomputes everything the rationed discrete stage needs on the UNEXPANDED
  # parent units: per-offer-variant utility/subsidy/CDCTC matrices, the base
  # (offer-independent) matrices, program eligibility flags, and the aligned
  # alpha matrix.
  #
  # Variants are computed on the full dataframe: the policy's subsidy function
  # applies per-child eligibility internally, so ineligible units' variant
  # values equal the no-offer values, and the discrete stage never assigns
  # offers to ineligible children anyway.
  #
  # Params:
  #   - pu_df (df): Parent units dataframe for a single type (unexpanded)
  #   - P (num vec): Equilibrium price vector (length 4)
  #   - n_children (int): Number of children (1 or 2)
  #   - pu_name (chr): Parent unit type name
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - demand_policy (list): Loaded demand policy object (with rationing)
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor
  #   - employment_shifts (num vec): Fixed employment shifts (or NULL)
  #
  # Returns: (list) with elements:
  #   - elig1, elig2 (logical vec): Program eligibility per child
  #   - alpha (matrix): n_units x (K-1) alpha matrix aligned to pu_df rows
  #   - base (list): Offer-independent base matrices (agi, taxes, gross cost)
  #   - variants (list): Keyed '00'/'10'/'01'/'11', each with V, subsidy,
  #       cdctc matrices (only the variants reachable given eligibility)
  #----------------------------------------------------------------------------

  elig_fn <- demand_policy$is_child_program_eligible
  elig1 <- as.logical(elig_fn(pu_df, 1))
  elig1[is.na(elig1)] <- FALSE
  elig2 <- if (n_children == 2) {
    e <- as.logical(elig_fn(pu_df, 2))
    e[is.na(e)] <- FALSE
    e
  } else {
    rep(FALSE, nrow(pu_df))
  }

  # Only compute variants that some record can actually reach
  variant_flags <- list('00' = c(FALSE, FALSE))
  if (any(elig1)) variant_flags[['10']] <- c(TRUE, FALSE)
  if (any(elig2)) variant_flags[['01']] <- c(FALSE, TRUE)
  if (any(elig1 & elig2)) variant_flags[['11']] <- c(TRUE, TRUE)

  base <- NULL
  variants <- list()
  for (key in names(variant_flags)) {
    fl <- variant_flags[[key]]
    um <- compute_policy_utility_matrix(
      P, pu_df, n_children, demand_params, pu_name,
      demand_policy$do_demand_policy, policy_cdctc, cpi_growth_factor,
      employment_shifts = employment_shifts,
      offered1 = fl[1], offered2 = fl[2],
      base = base, keep_components = TRUE
    )
    base <- um$base
    variants[[key]] <- list(
      V = um$V,
      subsidy = um$pc$subsidy_matrix,
      cdctc = um$pc$cdctc_matrix
    )
  }

  alpha <- match_alpha_to_parent_units(pu_df, demand_params[[pu_name]], pu_name)

  list(
    elig1 = elig1,
    elig2 = elig2,
    alpha = alpha,
    base = base,
    variants = variants
  )
}



collapse_rationed_choices <- function(df_expanded, pu_variants, n_children,
                                      offer_rates, rationed_sectors,
                                      n_draws_per_record, offer_seed = NULL) {

  #----------------------------------------------------------------------------
  # Discrete-choice collapse for a rationed policy: draws per-child, per-sector
  # program offers for each expanded record (from a dedicated RNG stream so
  # epsilon draws stay aligned with the baseline), assembles state-conditional
  # utilities column-wise from the precomputed variants, takes the argmax over
  # utility + epsilon, and extracts scalar outcomes at the chosen alternative
  # from the offer-appropriate variant.
  #
  # argmax(alpha + V_s + epsilon) is used directly: it differs from the
  # unrationed argmax(log p + epsilon) only by a row-constant log-normalizer,
  # so the two stages draw from identical choice distributions.
  #
  # Params:
  #   - df_expanded (df): Epsilon-expanded parent units (n_units x n_draws
  #       rows, in slice(rep(...)) order) with epsilon.k columns
  #   - pu_variants (list): Output of prepare_rationed_pu_variants (matrices
  #       indexed by unexpanded unit row)
  #   - n_children (int): Number of children (1 or 2)
  #   - offer_rates (num vec): Length-4 equilibrium offer rates
  #   - rationed_sectors (int vec): Sector ids with slots > 0
  #   - n_draws_per_record (int): Epsilon draws per unexpanded unit
  #   - offer_seed (int or NULL): Seed for the offer RNG stream
  #
  # Returns: (df) Collapsed dataframe with choice, scalar outcomes, decoded
  #   choice columns, offer_child*.j columns, program_eligible1/2, and
  #   program_slot_used.1/.2 flags
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)
  n_choices <- nrow(catalog)
  n_exp <- nrow(df_expanded)
  n_units <- n_exp / n_draws_per_record
  stopifnot(n_units == round(n_units))
  n_units <- as.integer(n_units)

  # Row mapping: expansion used slice(rep(1:n(), each = n_draws_per_record))
  unit_idx <- rep(seq_len(n_units), each = n_draws_per_record)

  j1 <- catalog$child1_market_sector_id
  j2 <- if (n_children == 2) catalog$child2_market_sector_id else NULL

  V <- pu_variants$variants
  elig1_exp <- pu_variants$elig1[unit_idx]
  elig2_exp <- pu_variants$elig2[unit_idx]

  #--------------------------
  # Draw program offers (dedicated RNG stream; masked by eligibility)
  #--------------------------

  if (!is.null(offer_seed)) {
    set.seed(offer_seed)
  }

  offers1 <- matrix(0L, nrow = n_exp, ncol = 4)
  offers2 <- matrix(0L, nrow = n_exp, ncol = 4)
  for (j in rationed_sectors) {
    # Draw for all records (deterministic draw count), then mask by eligibility
    offers1[, j] <- as.integer(runif(n_exp) < offer_rates[j]) * as.integer(elig1_exp)
    if (n_children == 2) {
      offers2[, j] <- as.integer(runif(n_exp) < offer_rates[j]) * as.integer(elig2_exp)
    }
  }

  #--------------------------
  # Argmax over alpha + V_state + epsilon, column by column
  #--------------------------

  # Reconstruct full alpha (first column is the zero normalization column)
  alpha_full <- cbind(0, pu_variants$alpha)

  best_util <- rep(-Inf, n_exp)
  choices <- integer(n_exp)

  for (k in seq_len(n_choices)) {
    # Per-record offer status for the sectors used in this column
    a_k <- if (!is.na(j1[k]) && j1[k] %in% rationed_sectors) {
      offers1[, j1[k]] == 1L
    } else {
      rep(FALSE, n_exp)
    }
    b_k <- if (n_children == 2 && !is.na(j2[k]) && j2[k] %in% rationed_sectors) {
      offers2[, j2[k]] == 1L
    } else {
      rep(FALSE, n_exp)
    }

    # Assemble the state-conditional utility for this column from variants
    v_k <- V[['00']]$V[unit_idx, k]
    m10 <- a_k & !b_k
    if (any(m10)) v_k[m10] <- V[['10']]$V[unit_idx[m10], k]
    if (n_children == 2) {
      m01 <- !a_k & b_k
      if (any(m01)) v_k[m01] <- V[['01']]$V[unit_idx[m01], k]
      m11 <- a_k & b_k
      if (any(m11)) v_k[m11] <- V[['11']]$V[unit_idx[m11], k]
    }

    util_k <- alpha_full[unit_idx, k] + v_k + df_expanded[[paste0('epsilon.', k)]]
    better <- util_k > best_util
    best_util[better] <- util_k[better]
    choices[better] <- k
  }
  rm(best_util)

  #--------------------------
  # Decode choice indices to human-readable columns
  #--------------------------

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

  #--------------------------
  # Extract scalar outcomes at the chosen alternative
  #--------------------------

  sel <- cbind(unit_idx, choices)
  agi <- pu_variants$base$agi_matrix[sel]
  taxes <- pu_variants$base$taxes_matrix[sel]
  gross_ecec_cost <- pu_variants$base$gross_ecec_cost_matrix[sel]

  # Offer status at the chosen alternative (per child)
  j1_of_choice <- j1[choices]
  a_sel <- rep(FALSE, n_exp)
  ok1 <- !is.na(j1_of_choice) & j1_of_choice %in% rationed_sectors
  if (any(ok1)) {
    a_sel[ok1] <- offers1[cbind(which(ok1), j1_of_choice[ok1])] == 1L
  }

  b_sel <- rep(FALSE, n_exp)
  if (n_children == 2) {
    j2_of_choice <- j2[choices]
    ok2 <- !is.na(j2_of_choice) & j2_of_choice %in% rationed_sectors
    if (any(ok2)) {
      b_sel[ok2] <- offers2[cbind(which(ok2), j2_of_choice[ok2])] == 1L
    }
  }

  # Subsidy/CDCTC from the offer-appropriate variant
  subsidy <- V[['00']]$subsidy[sel]
  cdctc <- V[['00']]$cdctc[sel]

  m10 <- a_sel & !b_sel
  if (any(m10)) {
    sel10 <- sel[m10, , drop = FALSE]
    subsidy[m10] <- V[['10']]$subsidy[sel10]
    cdctc[m10] <- V[['10']]$cdctc[sel10]
  }
  if (n_children == 2) {
    m01 <- !a_sel & b_sel
    if (any(m01)) {
      sel01 <- sel[m01, , drop = FALSE]
      subsidy[m01] <- V[['01']]$subsidy[sel01]
      cdctc[m01] <- V[['01']]$cdctc[sel01]
    }
    m11 <- a_sel & b_sel
    if (any(m11)) {
      sel11 <- sel[m11, , drop = FALSE]
      subsidy[m11] <- V[['11']]$subsidy[sel11]
      cdctc[m11] <- V[['11']]$cdctc[sel11]
    }
  }

  net_income <- agi - taxes - gross_ecec_cost + subsidy + cdctc

  # -- Assertions: extraction validity ----
  stopifnot(all(is.finite(net_income)))

  #--------------------------
  # Build collapsed dataframe
  #--------------------------

  wide_prefixes <- c('u', 'p', 'V', 'epsilon', 'agi', 'taxes', 'gross_ecec_cost',
                     'subsidy', 'cdctc', 'net_income', 'Y', 'C')
  wide_pattern <- paste0('^(', paste(wide_prefixes, collapse = '|'), ')\\.\\d+$')
  cols_to_keep <- names(df_expanded)[!grepl(wide_pattern, names(df_expanded))]

  collapsed <- df_expanded %>%
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

  if (n_children == 2) {
    collapsed <- collapsed %>%
      mutate(
        ecec_type.2  = decoded$ecec_type.2,
        ecec_hours.2 = decoded$ecec_hours.2
      )
  }

  # Persist offer state, eligibility, and realized slot usage (consumed by the
  # mechanical/distributional/poverty decompositions and diagnostics)
  for (j in rationed_sectors) {
    collapsed[[paste0('offer_child1.', j)]] <- offers1[, j]
    if (n_children == 2) {
      collapsed[[paste0('offer_child2.', j)]] <- offers2[, j]
    }
  }
  collapsed$program_eligible1 <- as.integer(elig1_exp)
  collapsed$program_eligible2 <- if (n_children == 2) as.integer(elig2_exp) else 0L
  collapsed$program_slot_used.1 <- as.integer(a_sel)
  collapsed$program_slot_used.2 <- if (n_children == 2) as.integer(b_sel) else 0L

  collapsed
}
