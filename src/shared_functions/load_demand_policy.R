load_demand_policy <- function(policy_name) {

  #----------------------------------------------------------------------------
  # Loads a demand-side policy function from config/policy_demand/.
  #
  # Params:
  #   - policy_name (chr): Name of the demand policy file (without .R extension)
  #
  # Returns: (fn) do_demand_policy function
  #----------------------------------------------------------------------------

  load_policy('policy_demand', policy_name, 'do_demand_policy')
}
