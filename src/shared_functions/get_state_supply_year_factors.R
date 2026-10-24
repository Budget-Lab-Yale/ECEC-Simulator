get_state_supply_year_factors <- function(year_path, year) {

  # State supply anchors, 2019-relative; last observed year carried forward.
  # Params: year_path (list) year/no_ba/ba rows; year (int) target year.
  # Returns: (num vec) no_ba, ba wage multipliers. NULL path -> identity.
  if (is.null(year_path)) return(c(no_ba = 1, ba = 1))
  years <- vapply(year_path, function(x) as.integer(x$year), integer(1))
  factors <- t(vapply(year_path, function(x) c(no_ba = x$no_ba, ba = x$ba), numeric(2)))
  if (anyNA(years) || anyDuplicated(years) || !2019L %in% years ||
      any(!is.finite(factors)) || any(factors <= 0)) {
    stop('Invalid state wage path. Rebuild annual supply inputs.')
  }
  if (any(abs(factors[years == 2019, ] - 1) > 1e-10)) {
    stop('State wage path must equal one in 2019. Rebuild annual supply inputs.')
  }
  if (year <= 2019) return(c(no_ba = 1, ba = 1))
  target <- min(year, max(years))
  if (!target %in% years) stop('State wage path missing historical year ', target, '.')
  setNames(as.numeric(factors[years == target, ]), c('no_ba', 'ba'))
}
