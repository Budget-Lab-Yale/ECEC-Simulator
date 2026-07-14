compute_policy_utility_matrix <- function(P, parent_units_df, n_children, demand_params,
                                          pu_name, policy_demand, policy_cdctc,
                                          cpi_growth_factor = 1.0,
                                          employment_shifts = NULL,
                                          offered1 = FALSE, offered2 = FALSE,
                                          base = NULL,
                                          keep_components = FALSE) {

  #----------------------------------------------------------------------------
  # Runs the policy pipeline and computes the deterministic utility matrix
  # V = beta * u(NI) for one parent unit type, optionally conditional on
  # program offer flags (for quantity-limited policies).
  #
  # This is the single V-construction path shared by the equilibrium demand
  # computation (get_demand_prob_matrix), the rationed offer-state machinery,
  # and the post-equilibrium discrete stage.
  #
  # Params:
  #   - P (num vec): Price vector (length 4, one per market sector)
  #   - parent_units_df (df): Parent units dataframe for a single type
  #   - n_children (int): Number of children (1 or 2)
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - pu_name (chr): Parent unit type name (e.g. 'c1')
  #   - policy_demand (fn): Demand policy function (accepts offered1/offered2)
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI growth factor for price scaling
  #   - employment_shifts (num vec): Named vector of shifts by group (or NULL)
  #   - offered1 (logical): Treat child 1 as holding a program offer in every
  #       rationed sector (FALSE for unrationed policies / no-offer state)
  #   - offered2 (logical): Same for child 2
  #   - base (list or NULL): Precomputed base matrices from a previous call at
  #       the same P (offer-independent; reuse across offer variants)
  #   - keep_components (logical): If TRUE, include the pc pipeline result
  #       (subsidy/cdctc matrices) in the return value
  #
  # Returns: (list) with elements:
  #   - V (matrix): n_units x n_choices utility matrix
  #   - base (list): base matrices (for reuse across offer variants)
  #   - pc (list or NULL): full policy component result if keep_components
  #----------------------------------------------------------------------------

  # Extract type-specific demand parameters
  type_params <- demand_params[[pu_name]]
  beta <- type_params$beta
  rho <- type_params$rho

  pc <- compute_policy_components(parent_units_df, P, n_children, demand_params,
                                   policy_demand, policy_cdctc, cpi_growth_factor,
                                   offered1 = offered1, offered2 = offered2,
                                   base = base, build_components = FALSE)
  base <- pc$base

  if (any(!is.finite(base$agi_matrix)) ||
      any(!is.finite(base$taxes_matrix)) ||
      any(!is.finite(base$gross_ecec_cost_matrix))) {
    stop('compute_policy_utility_matrix: non-finite values in base matrices (', pu_name, ').')
  }
  if (any(!is.finite(pc$subsidy_matrix))) {
    stop('compute_policy_utility_matrix: non-finite subsidy_matrix (', pu_name, ').')
  }
  if (any(!is.finite(pc$cdctc_matrix))) {
    stop('compute_policy_utility_matrix: non-finite cdctc_matrix (', pu_name, ').')
  }

  Y_policy <- base$agi_matrix - base$taxes_matrix
  C_policy <- base$gross_ecec_cost_matrix - pc$subsidy_matrix - pc$cdctc_matrix
  NI_policy <- Y_policy - C_policy

  if (any(!is.finite(NI_policy))) {
    stop('compute_policy_utility_matrix: non-finite NI_policy (Y - C) (', pu_name, ').')
  }

  V_matrix <- compute_utility_from_net_income(
    NI_policy         = NI_policy,
    parent_units_df   = parent_units_df,
    beta              = beta,
    rho               = rho,
    n_children        = n_children,
    pu_name           = pu_name,
    employment_shifts = employment_shifts
  )

  list(
    V = V_matrix,
    base = base,
    pc = if (keep_components) pc else NULL
  )
}
