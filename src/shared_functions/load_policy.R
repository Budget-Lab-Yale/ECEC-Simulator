#------------------------------------------------------------------------------
# load_policy.R
#
# Core helper for loading policy functions from R files.
# Sources a policy file into a fresh environment and extracts the
# expected function.
#
# Called from: load_cdctc_policy, load_demand_policy, load_supply_policy,
#              3c_run_scenario.R (directly), 3d_accumulate_results.R (directly)
#------------------------------------------------------------------------------



load_policy <- function(config_dir, policy_name, func_name) {

  #----------------------------------------------------------------------------
  # Core helper for loading policy functions from R files.
  #
  # Params:
  #   - config_dir (chr): Subdirectory under config/
  #   - policy_name (chr): Name of policy file without .R extension
  #   - func_name (chr): Name of function to extract
  #
  # Returns: The extracted function
  #----------------------------------------------------------------------------

  policy_path <- file.path('./config', config_dir, paste0(policy_name, '.R'))

  if (!file.exists(policy_path)) {
    stop(sprintf('Policy file not found: %s', policy_path))
  }

  # Source into a fresh environment
  policy_env <- new.env()
  source(policy_path, local = policy_env)

  # Get the expected function
  if (!exists(func_name, envir = policy_env)) {
    stop(sprintf('Policy file must define %s(): %s', func_name, policy_path))
  }

  result <- get(func_name, envir = policy_env)
  return(result)
}
