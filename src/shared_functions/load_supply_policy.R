load_supply_policy <- function(policy_name) {

  #----------------------------------------------------------------------------
  # Loads a supply-side policy function and derives the subsidy vector.
  #
  # Params:
  #   - policy_name (chr): Name of policy file
  #
  # Returns: list with:
  #   - do_supply_policy: function(P) -> P_supply
  #   - supply_subsidy: dbl[4] vector of per-hour subsidies by sector
  #----------------------------------------------------------------------------

  do_supply_policy <- load_policy('policy_supply', policy_name, 'do_supply_policy')

  # Derive supply subsidy from the function (P_supply - P when P = 0)
  supply_subsidy <- do_supply_policy(c(0, 0, 0, 0))

  list(
    do_supply_policy = do_supply_policy,
    supply_subsidy   = supply_subsidy
  )
}
