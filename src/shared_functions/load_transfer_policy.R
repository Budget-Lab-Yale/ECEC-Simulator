load_transfer_policy <- function(policy_name) {

  #----------------------------------------------------------------------------
  # Loads a transfer policy function from config/policy_transfer/.
  # Missing/blank names default to 'baseline' (zero transfers) so runscripts
  # without a policy_transfer column work unchanged.
  #
  # Params:
  #   - policy_name (chr): Policy file name without .R (or NULL/NA/'' for baseline)
  #
  # Returns: (fn) do_transfer_policy function
  #----------------------------------------------------------------------------

  if (is.null(policy_name) || is.na(policy_name) || policy_name == '') {
    policy_name <- 'baseline'
  }
  load_policy('policy_transfer', policy_name, 'do_transfer_policy')
}
