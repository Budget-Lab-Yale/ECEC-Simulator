load_cdctc_policy <- function(policy_name) {

  #----------------------------------------------------------------------------
  # Loads a CDCTC policy function from config/cdctc/.
  #
  # Params:
  #   - policy_name (chr): Name of the CDCTC policy file (without .R extension)
  #
  # Returns: (fn) do_cdctc_policy function
  #----------------------------------------------------------------------------

  load_policy('cdctc', policy_name, 'do_cdctc_policy')
}
