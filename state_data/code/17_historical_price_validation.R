# Historical price anchors vs NDCP, 2019 onward; fixed 2019 county/age weights.
# Usage: Rscript state_data/code/17_historical_price_validation.R <macro_dir> [run_root ...]
# Fixed 2019 local residuals (build supply yamls); CPI growth over time.
# Centre + paid home. Optional run roots: simulated equilibrium prices
# (solver_results_<yr>.txt) vs latest NDCP year grown w/ CPI -- no NDCP after 2022.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
source('state_data/code/16_ndcp_model_prices.R')
source('src/shared_functions/get_state_supply_year_factors.R')
source('src/shared_functions/get_hourly_wage_growth_factor.R')
source('src/shared_functions/get_cpi_growth_factor.R')

PRICE_LABELS <- c(p_low = 'Low-Priced Center-Based', p_high = 'High-Priced Center-Based',
                  p_home = 'Paid Home-Based')



ndcp_state_prices <- function(setting) {

  # Params: setting (chr) 'center' or 'home'
  # Returns: (tibble) year, geo, age_group, ndcp_all, ndcp_observed ($/wk, PEP 0-4 weights)
  nd <- read_ndcp(NULL, setting) %>% filter(year >= 2019, st %in% names(STATE_POSTAL_TO_FIPS))
  pep <- read_csv(PEP_FILE, col_types = cols(.default = col_character())) %>%
    transmute(fips = paste0(STATE, COUNTY), kids = as.numeric(TOT_POP))
  # State rows only; NYC overlaps NY and remains outside national fit statistics.
  nd %>% inner_join(pep, by = 'fips') %>%
    group_by(year, geo = st, age_group) %>%
    summarise(ndcp_all = if (any(!is.na(price))) weighted.mean(price, kids, na.rm = TRUE) else NA_real_,
              ndcp_observed = if (any(!is.na(price) & !imputed))
                weighted.mean(price[!imputed], kids[!imputed], na.rm = TRUE) else NA_real_,
              .groups = 'drop')
}



home_weights <- function() {

  # Returns: (tibble) geo, age_group, c_home -- ACS p0 paid-home children, kindergarten dropped
  ag <- bind_rows(lapply(names(AGE_GROUPS), function(a)
    tibble(age = AGE_GROUPS[[a]]$ages, age_group = a)))
  read_csv(P0_FILE, show_col_types = FALSE) %>%
    filter(kindergarten == 0, ecec_type == 'Paid Home-Based') %>%
    inner_join(ag, by = 'age') %>%
    group_by(geo, age_group) %>% summarise(c_home = sum(children), .groups = 'drop')
}



fit_summary <- function(detail, keys) {

  # Params: detail (tibble) model_week, ndcp_all, ndcp_observed, children by geo x age_group
  #         keys (chr) grouping columns besides geo
  # Returns: (tibble) n, Pearson, RMSE, mean ratio per keys x benchmark; complete geos only
  detail %>% pivot_longer(c(ndcp_all, ndcp_observed), names_to = 'benchmark', values_to = 'ndcp') %>%
    group_by(across(all_of(c(keys, 'geo', 'benchmark')))) %>%
    summarise(complete = all(is.finite(ndcp) & is.finite(model_week) & children > 0),
              model = weighted.mean(model_week, children),
              observed = weighted.mean(ndcp, children), .groups = 'drop') %>% filter(complete) %>%
    group_by(across(all_of(c(keys, 'benchmark')))) %>%
    summarise(n_geographies = n(), pearson_unweighted = cor(model, observed),
              rmse_weekly_unweighted = sqrt(mean((model - observed)^2)),
              mean_ratio_unweighted = mean(model) / mean(observed), .groups = 'drop')
}



read_solver_prices <- function(path) {

  # Params: path (chr) solver_results_<yr>.txt
  # Returns: (num) p_low, p_high, p_home -- Final column of PRICES (Market) block, $/child-hour
  x <- readLines(path)
  start <- grep('^PRICES \\(Market\\)', x)[1]
  if (is.na(start)) stop('No PRICES (Market) block: ', path)
  x <- x[start:length(x)]
  sapply(PRICE_LABELS, function(lab) {
    line <- x[startsWith(x, lab)][1]
    if (is.na(line)) stop('No ', lab, ' price: ', path)
    as.numeric(strsplit(trimws(sub(lab, '', line, fixed = TRUE)), '\\s+')[[1]][2])
  })
}



projected_price_check <- function(macro, run_roots, centre_w, home_w, ndcp) {

  # Simulated equilibrium posted prices vs last NDCP year x CPI growth to sim year.
  # Params: macro (df) macro series; run_roots (chr) run output roots;
  #         centre_w, home_w (tibble) age weights; ndcp (list) center/home NDCP tibbles
  # Returns: (list) detail, summary
  files <- unlist(lapply(run_roots, function(r)
    Sys.glob(file.path(r, 'simulation', 'baseline', 'states', '*', 'models', 'equilibrium',
                       'solver_results_*.txt'))))
  if (!length(files)) stop('No solver_results files under run roots.')
  eq <- bind_rows(lapply(files, function(f) {
    p <- read_solver_prices(f)
    tibble(geo = basename(dirname(dirname(dirname(f)))),
           year = as.integer(sub('.*_(\\d{4})\\.txt$', '\\1', f)),
           p_low = p[['p_low']], p_high = p[['p_high']], p_home = p[['p_home']])
  })) %>% filter(geo %in% names(STATE_POSTAL_TO_FIPS))
  if (anyDuplicated(eq[c('geo', 'year')])) stop('Geography-year in more than one run root.')
  last <- max(ndcp$center$year)
  # Fixed-local anchor in sim year: equilibrium / anchor = wage-response movement
  anchor <- bind_rows(lapply(seq_len(nrow(eq)), function(i) {
    yr <- eq$year[i]; g <- eq$geo[i]
    s <- read_supply_yaml(file.path(SUPPLY_DIR, paste0(g, '.yaml')))
    pr <- as.vector(s$L_req %*% (s$w * get_state_supply_year_factors(s$wage_path, yr))) *
      get_hourly_wage_growth_factor(macro, 2019, yr) + s$delta_0 * get_cpi_growth_factor(macro, 2019, yr)
    tibble(geo = g, year = yr, a_low = pr[2], a_high = pr[3], a_home = pr[4])
  }))
  grow <- function(nd) nd %>% filter(year == last) %>% select(-year) %>%
    crossing(sim_year = sort(unique(eq$year))) %>%
    mutate(cpi = sapply(sim_year, function(y) get_cpi_growth_factor(macro, last, y)),
           ndcp_all = ndcp_all * cpi, ndcp_observed = ndcp_observed * cpi) %>%
    rename(year = sim_year)
  centre <- eq %>% inner_join(anchor, by = c('geo', 'year')) %>%
    inner_join(centre_w, by = 'geo', relationship = 'many-to-many') %>%
    inner_join(grow(ndcp$center), by = c('year', 'geo', 'age_group')) %>%
    mutate(setting = 'center', children = c_low + c_high,
           model_week = 40 * (p_low * c_low + p_high * c_high) / children,
           anchor_week = 40 * (a_low * c_low + a_high * c_high) / children)
  home <- eq %>% inner_join(anchor, by = c('geo', 'year')) %>%
    inner_join(home_w, by = 'geo', relationship = 'many-to-many') %>%
    inner_join(grow(ndcp$home), by = c('year', 'geo', 'age_group')) %>%
    mutate(setting = 'home', children = c_home, model_week = 40 * p_home, anchor_week = 40 * a_home)
  detail <- bind_rows(centre, home) %>% mutate(ndcp_base_year = last)
  summary <- fit_summary(detail, c('year', 'setting')) %>%
    left_join(detail %>% group_by(year, setting, geo) %>%
                summarise(m = weighted.mean(model_week, children), a = weighted.mean(anchor_week, children),
                          .groups = 'drop') %>%
                group_by(year, setting) %>%
                summarise(equilibrium_to_anchor_mean = mean(m / a), equilibrium_to_anchor_min = min(m / a),
                          equilibrium_to_anchor_max = max(m / a), .groups = 'drop'),
              by = c('year', 'setting')) %>% mutate(ndcp_base_year = last)
  list(detail = detail, summary = summary)
}



historical_price_validation <- function(macro_dir, run_roots = character()) {

  # Params: macro_dir (chr) Macro-Projections baseline folder.
  #         run_roots (chr) optional simulator run roots for projected-year check
  # Returns: (list) detail, summary, home, projected; writes diagnostic CSVs under state_data/out.
  macro <- bind_rows(read_csv(file.path(macro_dir, 'historical.csv'), show_col_types = FALSE),
                     read_csv(file.path(macro_dir, 'projections.csv'), show_col_types = FALSE))
  if (anyDuplicated(macro$year)) stop('Macro series has duplicate years; resolve historical/projection overlap.')
  weights <- read_csv(OUT_DET, show_col_types = FALSE) %>%
    filter(residual == 'local') %>% select(geo, age_group, c_low, c_high)
  ndcp <- list(center = ndcp_state_prices('center'), home = ndcp_state_prices('home'))
  observed <- ndcp$center
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
        year = year, geo = geo, variant = 'fixed_local',
        p_low = price[2], p_high = price[3], p_home = price[4])
    }
  }
  anchors <- bind_rows(anchors)
  detail <- anchors %>% select(-p_home) %>% inner_join(weights, by = 'geo', relationship = 'many-to-many') %>%
    inner_join(observed, by = c('year', 'geo', 'age_group')) %>%
    mutate(children = c_low + c_high,
           model_week = 40 * (p_low * c_low + p_high * c_high) / children)
  # County population and tier mix held at 2019.
  summary <- fit_summary(detail, c('year', 'variant'))
  write_csv(detail, file.path(STATE_DATA_ROOT, 'out', 'ndcp_historical_prices.csv'))
  write_csv(summary, file.path(STATE_DATA_ROOT, 'out', 'ndcp_historical_price_summary.csv'))
  # Paid home: NDCP family child care (MFCC*), age groups at p0 paid-home children
  home <- anchors %>% select(year, geo, variant, p_home) %>%
    inner_join(home_weights(), by = 'geo', relationship = 'many-to-many') %>%
    inner_join(ndcp$home, by = c('year', 'geo', 'age_group')) %>%
    mutate(children = c_home, model_week = 40 * p_home)
  home_summary <- fit_summary(home, c('year', 'variant'))
  write_csv(home, file.path(STATE_DATA_ROOT, 'out', 'ndcp_historical_home_prices.csv'))
  write_csv(home_summary, file.path(STATE_DATA_ROOT, 'out', 'ndcp_historical_home_price_summary.csv'))
  cat('Historical price validation: ', nrow(summary), ' centre, ', nrow(home_summary),
      ' home year/variant/benchmark summaries\n', sep = '')
  projected <- NULL
  if (length(run_roots)) {
    projected <- projected_price_check(macro, run_roots, weights, home_weights(), ndcp)
    write_csv(projected$detail, file.path(STATE_DATA_ROOT, 'out', 'ndcp_projected_prices.csv'))
    write_csv(projected$summary, file.path(STATE_DATA_ROOT, 'out', 'ndcp_projected_price_summary.csv'))
    writeLines(c(paste('script: state_data/code/17_historical_price_validation.R; run at', format(Sys.time())),
                 paste('macro:', macro_dir), paste('run root:', run_roots)),
               file.path(STATE_DATA_ROOT, 'out', 'ndcp_projected_prices_provenance.txt'))
    cat('Projected price check: ', nrow(projected$summary), ' year/setting/benchmark summaries\n', sep = '')
  }
  invisible(list(detail = detail, summary = summary, home = home_summary, projected = projected))
}

if (sys.nframe() == 0) {
  args <- commandArgs(TRUE)
  if (length(args) < 1) stop('Pass Macro-Projections baseline directory, then optional run roots.')
  historical_price_validation(args[1], args[-1])
}
