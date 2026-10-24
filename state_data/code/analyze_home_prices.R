# NDCP family-home prices vs model anchors; external diagnostic, no fitted inputs.
# National home anchor uses household net payments; NDCP uses market-rate prices.
# Usage: Rscript state_data/code/analyze_home_prices.R
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
source('state_data/code/11_ndcp_price_comparison.R')
source('state_data/code/analyze_home_staffing.R')

analyze_home_prices <- function() {

  # Returns: (list) detail, summary; age/county weights fixed across scenarios.
  sensitivity <- analyze_home_staffing()
  nd <- read_ndcp(2019L, 'home') %>% filter(st %in% names(STATE_POSTAL_TO_FIPS))
  if (anyDuplicated(nd[c('fips', 'age_group')])) stop('Duplicate NDCP county-age records.')
  pep <- read_csv(PEP_FILE, col_types = cols(.default = col_character())) %>%
    transmute(fips = paste0(STATE, COUNTY), kids0_4 = as.numeric(TOT_POP))
  if (anyDuplicated(pep$fips) || anyNA(pep$kids0_4) || any(pep$kids0_4 < 0)) stop('Invalid county weights.')
  if (length(setdiff(nd$fips, pep$fips))) stop('NDCP county missing from population weights.')
  nd <- nd %>% inner_join(pep, by = 'fips') %>% mutate(geo = st)
  city <- SUBSTATE_GEOGRAPHIES$NYC
  city_fips <- paste0(sprintf('%02d', city$statefip), sprintf('%03d', city$countyfip))
  nd <- bind_rows(nd, nd %>% filter(fips %in% city_fips) %>% mutate(geo = 'NYC'))
  county <- bind_rows(nd %>% mutate(benchmark = 'all'),
    nd %>% filter(!is.na(imputed), !imputed) %>% mutate(benchmark = 'observed')) %>%
    group_by(geo, age_group, benchmark) %>% summarise(
      counties_with_price = sum(is.finite(price) & kids0_4 > 0),
      population_with_price = sum(kids0_4[is.finite(price)]),
      ndcp_weekly = if (any(is.finite(price) & kids0_4 > 0))
        weighted.mean(price[is.finite(price)], kids0_4[is.finite(price)]) else NA_real_,
      .groups = 'drop')
  ag <- bind_rows(lapply(names(AGE_GROUPS), function(a)
    tibble(age = AGE_GROUPS[[a]]$ages, age_group = a)))
  age_weights <- read_csv(P0_FILE, show_col_types = FALSE) %>%
    filter(geo %in% names(STATE_POSTAL_TO_FIPS), kindergarten == 0,
           ecec_type == 'Paid Home-Based') %>% inner_join(ag, by = 'age') %>%
    group_by(age_group) %>% summarise(children = sum(children), .groups = 'drop') %>%
    mutate(age_weight = children / sum(children))
  stopifnot(nrow(age_weights) == 3, all(age_weights$age_weight > 0),
            abs(sum(age_weights$age_weight) - 1) < 1e-12)
  coverage <- expand_grid(geo = sensitivity$geo[sensitivity$scenario == 'capacity_proxy'],
                          age_group = names(AGE_GROUPS), benchmark = c('all', 'observed')) %>%
    left_join(county, by = c('geo', 'age_group', 'benchmark')) %>%
    left_join(age_weights, by = 'age_group')
  observed <- coverage %>% group_by(geo, benchmark) %>% summarise(
    complete = all(is.finite(ndcp_weekly)),
    ndcp_weekly = sum(ndcp_weekly * age_weight), .groups = 'drop')
  detail <- sensitivity %>% select(scenario, geo, anchor_price_hourly) %>%
    inner_join(observed, by = 'geo', relationship = 'many-to-many') %>%
    mutate(model_weekly = 40 * anchor_price_hourly,
      price_basis = 'model_net_payment_anchor_vs_NDCP_market_rate',
      weekly_gap = model_weekly - ndcp_weekly)
  summary <- detail %>% filter(geo %in% names(STATE_POSTAL_TO_FIPS), complete) %>%
    group_by(scenario, benchmark) %>% summarise(n_geographies = n(),
      model_mean_weekly_unweighted = mean(model_weekly),
      ndcp_mean_weekly_unweighted = mean(ndcp_weekly),
      rmse_weekly_unweighted = sqrt(mean(weekly_gap^2)),
      correlation_unweighted = cor(model_weekly, ndcp_weekly), .groups = 'drop')
  write_csv(coverage, 'state_data/out/home_price_benchmark_coverage.csv')
  write_csv(detail, 'state_data/out/home_price_benchmark.csv')
  write_csv(summary, 'state_data/out/home_price_benchmark_summary.csv')
  inputs <- c(NDCP_FILE, PEP_FILE, P0_FILE,
    'state_data/code/analyze_home_prices.R', 'state_data/code/analyze_home_staffing.R',
    'state_data/code/11_ndcp_price_comparison.R',
    'resources/licensing/licensing_2019_home_configurations.csv',
    'resources/supply/licensing_ratios_reviewed_2019.csv',
    list.files('config/state/supply', full.names = TRUE, pattern = '\\.yaml$'))
  writeLines(c('External comparison only; model home anchor derived from household net payments.',
    'NDCP family-home market-rate medians; not identical price concept, no residual refitting.',
    '2019 PEP county child weights; national p0 paid-home child age weights; NYC excluded from fit.',
    'Observed excludes imputation digits 3-5; complete three-age coverage required.',
    paste(inputs, unname(tools::md5sum(inputs)), sep = '\t')),
    'state_data/out/home_price_benchmark_provenance.txt')
  print(as.data.frame(summary), row.names = FALSE)
  invisible(list(detail = detail, summary = summary))
}

if (sys.nframe() == 0) analyze_home_prices()
