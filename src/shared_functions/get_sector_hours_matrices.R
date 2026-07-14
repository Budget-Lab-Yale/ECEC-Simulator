get_sector_hours_matrices <- function(n_children) {

  #----------------------------------------------------------------------------
  # Returns cached matrices mapping choices to sector-hours (H1, H2) for
  # demand aggregation. Caches result in .choice_cache environment.
  #
  # Params:
  #   - n_children (int): Number of children (1 or 2)
  #
  # Returns: (list) H1 (and H2 if n_children == 2), each n_choices x 4
  #----------------------------------------------------------------------------

  cache_key <- paste0('sector_hours_', n_children)

  if (exists(cache_key, envir = .choice_cache)) {
    return(get(cache_key, envir = .choice_cache))
  }

  catalog <- get_choice_catalog(n_children)
  n_choices <- nrow(catalog)

  hours_annual <- HOURS_ANNUAL

  H1 <- matrix(0, nrow = n_choices, ncol = 4)
  for (k in 1:n_choices) {
    sector <- catalog$child1_market_sector_id[k]
    if (!is.na(sector)) H1[k, sector] <- hours_annual[catalog$child1_hours_choice[k]]
  }

  if (n_children == 1) {
    result <- list(H1 = H1)
  } else {
    H2 <- matrix(0, nrow = n_choices, ncol = 4)
    for (k in 1:n_choices) {
      sector <- catalog$child2_market_sector_id[k]
      if (!is.na(sector)) H2[k, sector] <- hours_annual[catalog$child2_hours_choice[k]]
    }
    result <- list(H1 = H1, H2 = H2)
  }

  assign(cache_key, result, envir = .choice_cache)
  return(result)
}
