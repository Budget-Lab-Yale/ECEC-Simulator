# Historical price anchors vs NDCP, 2019 onward; fixed 2019 county/age weights.
# Usage: Rscript state_data/code/17_historical_price_validation.R <macro_dir>
# Fixed 2019 geographic housing factors; CPI growth over time.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
source('state_data/code/16_ndcp_model_prices.R')
source('src/shared_functions/get_state_supply_year_factors.R')
source('src/shared_functions/get_hourly_wage_growth_factor.R')
source('src/shared_functions/get_cpi_growth_factor.R')

historical_price_validation <- function(macro_dir) {

  # Params: macro_dir (chr) Macro-Projections baseline folder.
  # Returns: (list) detail, summary; writes diagnostic CSVs under state_data/out.
  macro <- bind_rows(read_csv(file.path(macro_dir, 'historical.csv'), show_col_types = FALSE),
                     read_csv(file.path(macro_dir, 'projections.csv'), show_col_types = FALSE))
  if (anyDuplicated(macro$year)) stop('Macro series has duplicate years; resolve historical/projection overlap.')
  weights <- read_csv(OUT_DET, show_col_types = FALSE) %>%
    filter(residual == 'housing_scaled') %>% select(geo, age_group, c_low, c_high)
  nd <- read_ndcp(NULL) %>% filter(year >= 2019, st %in% names(STATE_POSTAL_TO_FIPS))
  pep <- read_csv(PEP_FILE, col_types = cols(.default = col_character())) %>%
    transmute(fips = paste0(STATE, COUNTY), kids = as.numeric(TOT_POP))
  # State rows only; NYC overlaps NY and remains outside national fit statistics.
  observed <- nd %>% inner_join(pep, by = 'fips') %>%
    group_by(year, geo = st, age_group) %>%
    summarise(ndcp_all = if (any(!is.na(price))) weighted.mean(price, kids, na.rm = TRUE) else NA_real_,
              ndcp_observed = if (any(!is.na(price) & !imputed))
                weighted.mean(price[!imputed], kids[!imputed], na.rm = TRUE) else NA_real_,
              .groups = 'drop')
  anchors <- list()
  for (year in sort(unique(observed$year))) {
    wage_growth <- get_hourly_wage_growth_factor(macro, 2019, year)
    inflation <- get_cpi_growth_factor(macro, 2019, year)
    for (geo in unique(observed$geo)) {
      supply <- read_supply_yaml(file.path(SUPPLY_DIR, paste0(geo, '.yaml')))
      labor <- as.vector(supply$L_req %*% (supply$w *
        get_state_supply_year_factors(supply$wage_path, year))) * wage_growth
      price <- labor + supply$delta_0 * inflation
      anchors[[length(anchors) + 1]] <- tibble(
        year = year, geo = geo, variant = 'fixed_housing',
        p_low = price[2], p_high = price[3])
    }
  }
  detail <- bind_rows(anchors) %>% inner_join(weights, by = 'geo', relationship = 'many-to-many') %>%
    inner_join(observed, by = c('year', 'geo', 'age_group')) %>%
    mutate(children = c_low + c_high,
           model_week = 40 * (p_low * c_low + p_high * c_high) / children)
  level <- detail %>% pivot_longer(c(ndcp_all, ndcp_observed), names_to = 'benchmark', values_to = 'ndcp') %>%
    group_by(year, geo, variant, benchmark) %>%
    summarise(complete = all(is.finite(ndcp) & is.finite(model_week) & children > 0),
              model = weighted.mean(model_week, children),
              observed = weighted.mean(ndcp, children), .groups = 'drop') %>% filter(complete)
  # County population and tier mix held at 2019.
  summary <- level %>% group_by(year, variant, benchmark) %>%
    summarise(n_geographies = n(), pearson_unweighted = cor(model, observed),
              rmse_weekly_unweighted = sqrt(mean((model - observed)^2)),
              mean_ratio_unweighted = mean(model) / mean(observed), .groups = 'drop')
  write_csv(detail, file.path(STATE_DATA_ROOT, 'out', 'ndcp_historical_prices.csv'))
  write_csv(summary, file.path(STATE_DATA_ROOT, 'out', 'ndcp_historical_price_summary.csv'))
  cat('Historical price validation: ', nrow(summary), ' year/variant/benchmark summaries\n', sep = '')
  invisible(list(detail = detail, summary = summary))
}

if (sys.nframe() == 0) {
  args <- commandArgs(TRUE)
  if (length(args) != 1) stop('Pass Macro-Projections baseline directory.')
  historical_price_validation(args[1])
}
