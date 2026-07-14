extract_policy_values_at_choice <- function(pu, P, n_children, demand_params,
                                            policy_demand, policy_cdctc,
                                            cpi_growth_factor, choices) {

  #----------------------------------------------------------------------------
  # Evaluates an (unrationed) policy's components at prices P and returns, for
  # each record, the value at that record's given choice: agi, taxes,
  # gross_ecec_cost, subsidy, cdctc, and net_income.
  #
  # This is the offer-free analogue of extract_offer_conditional_values: the
  # decompositions only ever need the component values at a single per-record
  # choice, so we never materialise the wide (n x n_choices) component tibble.
  # Rows are processed in chunks of MFC_CHUNK_ROWS so peak memory is bounded by
  # one chunk's base matrices rather than the full parent-unit count. Every
  # step is row-local, so this is numerically identical to a single pass.
  #
  # Params:
  #   - pu (df): Parent units (stripped, with any policy tax rules applied)
  #   - P (num vec): Price vector to evaluate at
  #   - n_children (int): Number of children (1 or 2)
  #   - demand_params (list): Demand model parameters
  #   - policy_demand (fn): Demand policy function
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor
  #   - choices (int vec): Choice index per record to extract at
  #
  # Returns: (list) per-record vectors: agi, taxes, gross_ecec_cost, subsidy,
  #   cdctc, net_income
  #----------------------------------------------------------------------------

  n_rec <- length(choices)
  agi <- numeric(n_rec); taxes <- numeric(n_rec); gross_ecec_cost <- numeric(n_rec)
  subsidy <- numeric(n_rec); cdctc <- numeric(n_rec)

  if (n_rec > 0) {
    for (s in seq(1L, n_rec, by = MFC_CHUNK_ROWS)) {
      idx <- s:min(s + MFC_CHUNK_ROWS - 1L, n_rec)
      sel <- cbind(seq_along(idx), choices[idx])
      pcv <- compute_policy_components(pu[idx, , drop = FALSE], P, n_children, demand_params,
                                       policy_demand, policy_cdctc, cpi_growth_factor,
                                       build_components = FALSE,
                                       check_subsidy_variation = FALSE)
      agi[idx]             <- pcv$base$agi_matrix[sel]
      taxes[idx]           <- pcv$base$taxes_matrix[sel]
      gross_ecec_cost[idx] <- pcv$base$gross_ecec_cost_matrix[sel]
      subsidy[idx]         <- pcv$subsidy_matrix[sel]
      cdctc[idx]           <- pcv$cdctc_matrix[sel]
    }
  }

  list(
    agi = agi,
    taxes = taxes,
    gross_ecec_cost = gross_ecec_cost,
    subsidy = subsidy,
    cdctc = cdctc,
    net_income = agi - taxes - gross_ecec_cost + subsidy + cdctc
  )
}
