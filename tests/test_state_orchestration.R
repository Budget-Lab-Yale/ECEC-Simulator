# Exercise real state wrappers with lightweight solver/finalizer substitutes.
suppressPackageStartupMessages(library(dplyr))
source('src/3_simulation/3b_run_simulation_year.R')
source('src/3_simulation/slurm.R')
source('src/4_output/4a_finalize_output.R')

make_state <- function(id) list(sim_base_hh = id, base_supply_params = id,
  base_demand_params = id, cached_price_wedge = id, epop_path = paste0(id, '.csv'),
  prev_baseline_prices = id, prev_counterfactual_prices = list(policy = id),
  baseline_emp_rates_2019 = id)
ctx <- list(state_contexts = list(NY = make_state(1), VA = make_state(2)),
  baseline_info = list(paths = list(output = 'baseline')),
  counterfactual_infos = list(policy = list(paths = list(output = 'policy'))),
  summary_accumulators = list(), baseline_results = list(stale = TRUE))
seen <- list()
run_simulation_year_core <- function(view, year) {
  if (is.null(view$current_state)) return('national')
  st <- view$current_state
  seen[[paste(st, year)]] <<- view
  stopifnot(identical(view$baseline_info$paths$output, file.path('baseline', 'states', st)),
    identical(view$counterfactual_infos$policy$paths$output, file.path('policy', 'states', st)),
    is.null(view$state_contexts), length(view$baseline_results) == 0)
  view$prev_baseline_prices <- view$prev_baseline_prices + 10
  view$prev_counterfactual_prices$policy <- view$prev_counterfactual_prices$policy + 20
  view$summary_accumulators[[paste(st, year)]] <- st
  view
}
stopifnot(identical(run_simulation_year(list(state_contexts = NULL), 2026), 'national'))
result <- run_simulation_year(ctx, 2026)
result <- run_simulation_year(result, 2027)
stopifnot(length(result$summary_accumulators) == 4,
  seen[['NY 2027']]$prev_baseline_prices == 11,
  seen[['VA 2027']]$prev_baseline_prices == 12,
  seen[['NY 2027']]$prev_counterfactual_prices$policy == 21,
  identical(ctx$baseline_info$paths$output, 'baseline'))
cat('PASS: sequential state output isolation, accumulated summaries, separate warm starts, national delegation\n')

run_year_standalone_core <- function(year, view, scratch_dir) {
  if (is.null(view$current_state)) return('national')
  st <- view$current_state
  stopifnot(view$sim_base_hh == view$base_supply_params,
            view$cached_price_wedge == view$base_demand_params,
            identical(view$baseline_info$paths$output, file.path('baseline', 'states', st)))
  list(baseline = list(converged = TRUE, prices = view$base_supply_params,
                        allocation = tibble(year = year, n = 1)),
       counterfactuals = list(policy = list(converged = TRUE,
                            allocation = tibble(year = year, n = 2))))
}
stopifnot(identical(run_year_standalone(2026, list(state_contexts = NULL)), 'national'))
parallel <- run_year_standalone(2026, ctx)
stopifnot(identical(parallel$baseline$allocation$state, c('NY', 'VA')),
  identical(parallel$counterfactuals$policy$allocation$state, c('NY', 'VA')),
  parallel$baseline$prices$NY == 1, parallel$baseline$prices$VA == 2,
  all(unlist(parallel$baseline$converged)))
cat('PASS: parallel state output isolation, tagged summaries, per-state scalars, national delegation\n')

finished <- list()
finalize_simulation_core <- function(view) {
  subdir <- get0('finalize_state_subdir', envir = .GlobalEnv)
  key <- if (is.null(subdir)) 'national' else subdir
  finished[[key]] <<- view
}
finalize_simulation(list(summary_accumulators = list(baseline = list(allocation = tibble()))))
finalize_simulation(list(summary_accumulators = list(baseline = parallel$baseline)))
stopifnot('national' %in% names(finished), is.null(finalize_state_subdir))
for (st in c('NY', 'VA')) {
  allocation <- finished[[file.path('states', st)]]$summary_accumulators$baseline$allocation
  stopifnot(nrow(allocation) == 1, !'state' %in% names(allocation))
}
cat('PASS: national finalization, per-state row filtering, output hook cleanup\n')
