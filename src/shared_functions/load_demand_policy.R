load_demand_policy <- function(policy_name) {

  #----------------------------------------------------------------------------
  # Loads a demand-side policy from config/policy_demand/ into an isolated
  # environment and returns a policy object.
  #
  # Every policy file must define do_demand_policy(). Quantity-limited
  # (rationed) policies additionally define:
  #   - rationing: list with $slots — length-4 numeric vector of program slots
  #       by market sector (sector 1 must be 0), or a function(year) returning
  #       one (slot schedule). Other fields (e.g. covered share) are policy-
  #       internal and ignored by the framework.
  #   - is_child_program_eligible(parent_units_df, child_idx): returns a
  #       logical vector marking which units' child child_idx can enter the
  #       program lottery. Must match the per-child eligibility the subsidy
  #       function applies internally.
  #   - do_demand_policy must natively accept offered1/offered2 arguments
  #       (treat child 1/2 as holding an offer in every rationed sector).
  #
  # Unrationed policies need no changes: their do_demand_policy is wrapped so
  # the framework can uniformly pass offered1/offered2 (ignored).
  #
  # Params:
  #   - policy_name (chr): Name of the demand policy file (without .R extension)
  #
  # Returns: (list) with elements:
  #   - do_demand_policy (fn): Subsidy function accepting offered1/offered2
  #   - rationing (list or NULL): Rationing metadata (NULL = unrationed)
  #   - is_child_program_eligible (fn or NULL): Lottery eligibility function
  #----------------------------------------------------------------------------

  policy_path <- file.path('./config/policy_demand', paste0(policy_name, '.R'))

  if (!file.exists(policy_path)) {
    stop(sprintf('Policy file not found: %s', policy_path))
  }

  # Source into a fresh environment
  policy_env <- new.env()
  source(policy_path, local = policy_env)

  if (!exists('do_demand_policy', envir = policy_env)) {
    stop(sprintf('Policy file must define do_demand_policy(): %s', policy_path))
  }

  fn_raw <- get('do_demand_policy', envir = policy_env)
  accepts_offers <- all(c('offered1', 'offered2') %in% names(formals(fn_raw)))

  rationing <- if (exists('rationing', envir = policy_env)) {
    get('rationing', envir = policy_env)
  } else {
    NULL
  }

  elig_fn <- if (exists('is_child_program_eligible', envir = policy_env)) {
    get('is_child_program_eligible', envir = policy_env)
  } else {
    NULL
  }

  # Validate the rationed-policy contract
  if (!is.null(rationing)) {
    if (is.null(rationing$slots)) {
      stop('Rationed policy ', policy_name, ' must define rationing$slots ',
           '(length-4 numeric vector by sector, or function(year)).')
    }
    if (is.null(elig_fn)) {
      stop('Rationed policy ', policy_name, ' must define ',
           'is_child_program_eligible(parent_units_df, child_idx).')
    }
    if (!accepts_offers) {
      stop('Rationed policy ', policy_name, ' must define do_demand_policy() ',
           'with offered1/offered2 arguments (offer-conditional subsidies).')
    }
  }

  # Normalize signature: unrationed policies get a wrapper that absorbs the
  # offered1/offered2 arguments so all demand policies share one interface
  if (accepts_offers) {
    fn <- fn_raw
  } else {
    fn <- function(parent_units_df, catalog, P, n_children,
                   agi_matrix, taxes_matrix, gross_ecec_cost_matrix,
                   child1_cost_matrix = NULL, child2_cost_matrix = NULL,
                   offered1 = FALSE, offered2 = FALSE) {
      fn_raw(parent_units_df = parent_units_df,
             catalog = catalog,
             P = P,
             n_children = n_children,
             agi_matrix = agi_matrix,
             taxes_matrix = taxes_matrix,
             gross_ecec_cost_matrix = gross_ecec_cost_matrix,
             child1_cost_matrix = child1_cost_matrix,
             child2_cost_matrix = child2_cost_matrix)
    }
  }

  list(
    do_demand_policy = fn,
    rationing = rationing,
    is_child_program_eligible = elig_fn
  )
}
