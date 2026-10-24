# Validate acquired annual wage panel and persisted simulation inputs.
suppressPackageStartupMessages({library(dplyr); library(readr); library(yaml)})
source('src/shared_functions/get_state_supply_year_factors.R')
a <- read_csv('resources/supply/state_childcare_wage_ratios_annual.csv', show_col_types = FALSE)
stopifnot(nrow(a) == 728, length(unique(a$geo)) == 52,
          all(is.finite(a$wage_ratio) & a$wage_ratio > 0),
          all(is.finite(a$relative_to_2019) & a$relative_to_2019 > 0),
          all(a$relative_to_2019[a$year == 2019] == 1))
for (geo in unique(a$geo)) {
  y <- read_yaml(file.path('config', 'state', 'supply', paste0(geo, '.yaml')))
  stopifnot(length(y$wage_path) == 7)
  for (year in 2019:2025) {
    x <- a %>% filter(.data$geo == .env$geo, .data$year == .env$year)
    want <- setNames(x$relative_to_2019, x$educ_group)[c('no_ba', 'ba')]
    stopifnot(max(abs(get_state_supply_year_factors(y$wage_path, year) - want)) < 1e-12)
  }
  stopifnot(all(get_state_supply_year_factors(y$wage_path, 2030) ==
                  get_state_supply_year_factors(y$wage_path, 2025)))
}
report <- a %>% group_by(geo, educ_group) %>% summarise(
  n_years = n(), occupations = first(occupations), fallback = first(fallback),
  factor_2025 = relative_to_2019[year == 2025],
  min_factor = min(relative_to_2019), max_factor = max(relative_to_2019), .groups = 'drop')
write_csv(report, 'state_data/out/annual_wage_validation.csv')
cat('PASS: 728 production rows, 52 YAML paths, every year/group matches source, carry-forward correct\n')
cat('All-occupation fallback groups:', sum(report$fallback), 'of', nrow(report), '\n')
cat('2025 relative factors range:', range(report$factor_2025), '\n')
