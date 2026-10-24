get_regional_price_parity <- function(geo, year,
                                      path = file.path('resources', 'bea',
                                                       'regional_price_parities_annual.csv')) {

  # Annual parity for survey-year matching; no extrapolation or rank mutation.
  # Params: geo (chr vec), year (int vec), path (chr) panel CSV.
  # Returns: (num vec) parity, US = 100. Geography/year lengths must conform.
  n <- max(length(geo), length(year))
  if (!n || !length(geo) %in% c(1L, n) || !length(year) %in% c(1L, n)) {
    stop('RPP geography and year lengths must match or have length one.')
  }
  if (!file.exists(path)) stop('Annual RPP panel missing. Run acquire_annual_supply_data.py --rpp-only.')
  d <- utils::read.csv(path, stringsAsFactors = FALSE)
  key <- paste(d$geo, d$year)
  if (anyDuplicated(key)) stop('Duplicate geography/year in annual RPP panel.')
  at <- match(paste(rep(geo, length.out = n), rep(year, length.out = n)), key)
  value <- d$all_items[at]
  if (any(!is.finite(value) | value <= 0)) {
    stop('Requested RPP geography/year unavailable. Update source panel before adding survey years.')
  }
  value
}
