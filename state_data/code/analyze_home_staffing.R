# Home capacity vs adult-adjusted staffing; diagnostic only, no supply writes.
# Usage: Rscript state_data/code/analyze_home_staffing.R
suppressPackageStartupMessages({library(dplyr); library(readr); library(yaml)})
source('resources/supply/state_supply_params.R')

analyze_home_staffing <- function() {

  # Scenarios change AR/RI/WA adult assumptions, renormalize over 50 states + DC.
  # Returns: (df) 2019 staffing and anchor-price sensitivities, not equilibria.
  rules <- read_csv(RATIOS, show_col_types = FALSE) %>% filter(setting == 'home')
  stopifnot(nrow(rules) == 52, !anyDuplicated(rules$state),
            all(is.finite(rules$children_per_staff) & rules$children_per_staff > 0))
  population <- read_csv(POP_0_4, col_types = cols(.default = col_character()))
  pop <- tapply(as.numeric(population$TOT_POP), as.integer(population$STATE), sum)
  states <- names(STATE_POSTAL_TO_FIPS)
  weights <- setNames(as.numeric(pop[as.character(STATE_POSTAL_TO_FIPS)]), states)
  capacity <- setNames(rules$children_per_staff, rules$state)
  configurations <- read_csv('resources/licensing/licensing_2019_home_configurations.csv',
                              show_col_types = FALSE)
  chosen <- tibble(state = c('AR', 'RI', 'WA', 'AR'),
    configuration = c('licensed_two_adults', 'family_with_assistant',
                        'experienced_with_assistant', 'licensed_three_adults')) %>%
    left_join(configurations, by = c('state', 'configuration'))
  stopifnot(nrow(chosen) == 4, !anyNA(chosen$required_adults),
    all(chosen$max_preschool_children == capacity[chosen$state]),
    all(chosen$required_adults == c(2, 2, 2, 3)),
    all(chosen$status == 'historical_rule_configuration_verified'))
  scenarios <- list(capacity_proxy = setNames(rep(1, length(capacity)), names(capacity)))
  scenarios$uniform_two_adults <- scenarios$capacity_proxy * 2
  scenarios$AR_RI_WA_two_adults <- scenarios$capacity_proxy
  scenarios$AR_RI_WA_two_adults[c('AR', 'RI', 'WA')] <- 2
  scenarios$AR_three_RI_WA_two <- scenarios$AR_RI_WA_two_adults
  scenarios$AR_three_RI_WA_two['AR'] <- 3
  raw_base <- 1 / capacity
  baseline_index <- raw_base / weighted.mean(raw_base[states], weights)
  rows <- list()
  for (scenario in names(scenarios)) {
    adults <- scenarios[[scenario]]
    raw <- adults / capacity
    denominator <- weighted.mean(raw[states], weights)
    index <- raw / denominator
    stopifnot(abs(weighted.mean(index[states], weights) - 1) < 1e-12)
    for (geo in names(capacity)) {
      y <- read_yaml(file.path('config', 'state', 'supply', paste0(geo, '.yaml')))
      current <- unlist(y$labor_requirements$paid_home_based)
      wage <- unlist(y$wages)[names(current)]
      residual <- y$per_unit_residual$paid_home_based
      factor <- unname(index[geo] / baseline_index[geo])
      current_labor_cost <- sum(current * wage)
      rows[[length(rows) + 1]] <- tibble(
        scenario, geo, capacity = unname(capacity[geo]),
        assumed_adults = unname(adults[geo]),
        national_raw_staff_per_child = denominator,
        staffing_index = unname(index[geo]),
        labor_requirement_factor = factor,
        baseline_worker_hours_per_child_hour = sum(current),
        worker_hours_per_child_hour = sum(current) * factor,
        baseline_anchor_price_hourly = current_labor_cost + residual,
        anchor_price_hourly = current_labor_cost * factor + residual,
        anchor_price_change_pct = 100 * (anchor_price_hourly / baseline_anchor_price_hourly - 1))
    }
  }
  result <- bind_rows(rows)
  controls <- result$scenario %in% c('capacity_proxy', 'uniform_two_adults')
  stopifnot(all(abs(result$labor_requirement_factor[controls] - 1) < 1e-12))
  write_csv(result, 'state_data/out/home_staffing_sensitivity.csv')
  cat('Four scenarios, 52 geographies each; uniform adult-count scaling cancels in normalization.\n')
  cat('National home anchor already includes NSECE helpers.\n')
  cat('Sensitivity changes selected state multipliers only; other states retain capacity proxies.\n')
  print(as.data.frame(result %>% filter(geo %in% c('AR', 'RI', 'WA')) %>%
    select(scenario, geo, staffing_index, labor_requirement_factor, anchor_price_change_pct)), row.names = FALSE)
  invisible(result)
}

if (sys.nframe() == 0) analyze_home_staffing()
