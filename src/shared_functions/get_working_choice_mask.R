get_working_choice_mask <- function(n_children, cache = TRUE) {

  #----------------------------------------------------------------------------
  # Returns a logical vector indicating which choices have employment != 'none'.
  #
  # Working choices are those where the primary caregiver is employed (pt or ft).
  # This mask is used by the three-parameter demand model to identify choices
  # that contribute to the return-to-work calculation.
  #
  # Params:
  #   - n_children (int): number of children (1 or 2)
  #   - cache (logical): whether to use/store cached result
  #
  # Returns:
  #   logical vector of length n_choices: TRUE if employment != 'none'
  #----------------------------------------------------------------------------

  cache_name <- paste0('working_choice_mask_', n_children)

  if (cache && exists(cache_name, envir = .choice_cache)) {
    return(get(cache_name, envir = .choice_cache))
  }

  catalog <- get_choice_catalog(n_children)
  is_working <- catalog$employment_choice != 'none'

  if (cache) {
    assign(cache_name, is_working, envir = .choice_cache)
  }

  return(is_working)
}
