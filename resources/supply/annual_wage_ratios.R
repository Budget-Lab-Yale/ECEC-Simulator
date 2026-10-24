# Annual local-to-national wage changes; fixed 2019 ACS occupation weights.
suppressPackageStartupMessages({library(readr); library(dplyr)})
source('resources/supply/oes_childcare_wage_ratios_2019.R')

build_annual_wage_ratios <- function(input_path = file.path('resources', 'bls_oews',
                                                           'childcare_occupation_wages_annual.csv'),
                                      output_path = file.path('resources', 'supply',
                                                             'state_childcare_wage_ratios_annual.csv')) {

  # Params: none. Returns: (df) geography/year/group ratios and 2019-relative factors.
  path <- input_path
  if (!file.exists(path)) stop('Annual BLS extract missing. Run acquire_annual_supply_data.py first.')
  d <- read_csv(path, col_types = cols(occ_code = col_character(), .default = col_guess())) %>%
    mutate(h_median = suppressWarnings(as.numeric(h_median)))
  keys <- c('year', 'geo', 'occ_code')
  if (anyDuplicated(d[keys])) stop('Duplicate annual BLS occupation cells.')
  years <- sort(unique(d$year))
  if (!identical(as.integer(years), seq.int(2019L, max(years)))) {
    stop('Annual BLS extract must contain every year from 2019 through last observation.')
  }
  weights <- read_weights()
  rows <- list()
  for (geo in setdiff(unique(d$geo), 'US')) {
    for (group in names(weights)) {
      w <- weights[[group]]
      # Same occupation basket in both geographies and every year.
      usable <- vapply(names(w), function(code) {
        x <- d %>% filter(geo %in% c('US', .env$geo), occ_code == code)
        nrow(x) == 2 * length(years) && all(is.finite(x$h_median) & x$h_median > 0)
      }, logical(1)) & w > 0
      fallback <- !any(usable)
      codes <- if (fallback) ALL_OCC else names(w)[usable]
      basket <- if (fallback) setNames(1, ALL_OCC) else w[usable] / sum(w[usable])
      ratios <- vapply(years, function(y) {
        x <- d %>% filter(year == y, occ_code %in% codes)
        local <- setNames(x$h_median[x$geo == geo], x$occ_code[x$geo == geo])[codes]
        national <- setNames(x$h_median[x$geo == 'US'], x$occ_code[x$geo == 'US'])[codes]
        if (any(!is.finite(c(local, national))) || any(c(local, national) <= 0)) {
          stop('Incomplete annual wage basket for ', geo, '/', group, '/', y)
        }
        sum(basket * local) / sum(basket * national)
      }, numeric(1))
      rows[[length(rows) + 1]] <- tibble(
        geo = geo, year = years, educ_group = group, wage_ratio = ratios,
        relative_to_2019 = ratios / ratios[years == 2019],
        occupations = paste(codes, collapse = ';'), fallback = fallback,
        method = ifelse(years >= 2021, 'MB3', 'legacy'))
    }
  }
  out <- bind_rows(rows) %>% arrange(geo, year, educ_group)
  expected <- c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))
  if (!setequal(unique(out$geo), expected)) stop('Annual wage panel missing state or NYC geography.')
  write_csv(out, output_path)
  cat('Annual wages: ', nrow(out), ' geography/year/education records\n', sep = '')
  invisible(out)
}

if (sys.nframe() == 0) build_annual_wage_ratios()
