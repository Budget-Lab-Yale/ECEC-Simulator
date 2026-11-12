#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
if (identical(args, '--help')) {
  cat(paste0(
    'Usage: Rscript src/4_output/extract_parental_income_archive.R ',
    '--run-root=PATH --output-dir=NEW_PATH --fiscal-years=2026:2055 ',
    '[--workers=1] [--scenarios=universal,means_tested] [--tolerance-b=1e-6]\n',
    'Requires data.table and saved single-draw parent-unit files. ',
    'The output parent must exist; output must be outside the source run.\n'
  ))
  quit(status = 0)
}

get_arg <- function(flag, default = NULL) {
  prefix <- paste0(flag, '=')
  matches <- args[startsWith(args, prefix)]
  if (!length(matches)) return(default)
  if (length(matches) > 1L) stop('Argument supplied more than once: ', flag)
  substring(matches[[1]], nchar(prefix) + 1L)
}

parse_years <- function(value) {
  if (is.null(value) || !nzchar(value)) stop('--fiscal-years is required')
  if (!grepl('^[0-9]{4}(:[0-9]{4}|(,[0-9]{4})*)$', value)) {
    stop('Invalid years: ', value)
  }
  if (grepl(':', value, fixed = TRUE)) {
    endpoints <- as.integer(strsplit(value, ':', fixed = TRUE)[[1]])
    if (length(endpoints) != 2L || anyNA(endpoints) || endpoints[[1]] > endpoints[[2]]) {
      stop('Invalid year range: ', value)
    }
    return(seq.int(endpoints[[1]], endpoints[[2]]))
  }
  years <- as.integer(strsplit(value, ',', fixed = TRUE)[[1]])
  if (!length(years) || anyNA(years) || anyDuplicated(years)) {
    stop('Invalid year list: ', value)
  }
  sort(years)
}

unknown_args <- args[!vapply(args, function(arg) {
  any(startsWith(arg, c(
    '--run-root=', '--output-dir=', '--fiscal-years=', '--workers=',
    '--scenarios=', '--tolerance-b='
  )))
}, logical(1))]
if (length(unknown_args)) stop('Unknown argument(s): ', paste(unknown_args, collapse = ', '))

run_root <- get_arg('--run-root')
output_dir <- get_arg('--output-dir')
fiscal_years <- parse_years(get_arg('--fiscal-years'))
workers_arg <- get_arg('--workers', '1')
tolerance_b <- as.numeric(get_arg('--tolerance-b', '1e-6'))
scenario_arg <- get_arg('--scenarios')

if (is.null(run_root) || !nzchar(run_root)) stop('--run-root is required')
if (is.null(output_dir) || !nzchar(output_dir)) stop('--output-dir is required')
if (!dir.exists(run_root)) stop('Run root not found: ', run_root)
if (file.exists(output_dir)) stop('Output path already exists: ', output_dir)
if (!grepl('^[1-9][0-9]*$', workers_arg)) {
  stop('--workers must be a positive integer')
}
workers <- as.integer(workers_arg)
if (is.na(workers)) stop('--workers is too large')
if (!is.finite(tolerance_b) || tolerance_b <= 0) stop('--tolerance-b must be positive')

run_root <- normalizePath(run_root, winslash = '/', mustWork = TRUE)
output_parent <- normalizePath(dirname(output_dir), winslash = '/', mustWork = TRUE)
output_dir <- file.path(output_parent, basename(output_dir))

# Resolve the existing parent first so symlinks cannot redirect output into
# the read-only source run. A new destination prevents overwriting prior work.
is_below <- function(path, root) startsWith(path, paste0(root, '/'))
if (identical(output_dir, run_root) || is_below(output_dir, run_root)) {
  stop('Output directory cannot be inside the source run')
}
if (workers > 1L && .Platform$OS.type == 'windows') {
  stop('Parallel extraction requires a fork-capable platform; use --workers=1 on Windows')
}

runscript_path <- file.path(run_root, 'metadata', 'runscript.csv')
if (!file.exists(runscript_path)) stop('Runscript metadata not found: ', runscript_path)
run_spec <- fread(file = runscript_path, na.strings = c('', 'NA'))
required_run_cols <- c('id', 'years')
missing_run_cols <- setdiff(required_run_cols, names(run_spec))
if (length(missing_run_cols)) {
  stop('Runscript metadata is missing: ', paste(missing_run_cols, collapse = ', '))
}
if (!'phase_in_years' %in% names(run_spec)) run_spec[, phase_in_years := NA_real_]
if (!nrow(run_spec) || run_spec$id[[1]] != 'baseline' || anyDuplicated(run_spec$id)) {
  stop('Runscript must contain one baseline first and unique scenario IDs')
}

baseline_id <- 'baseline'
policy_ids <- setdiff(run_spec$id, baseline_id)
if (!is.null(scenario_arg) && nzchar(scenario_arg)) {
  requested <- strsplit(scenario_arg, ',', fixed = TRUE)[[1]]
  if (!length(requested) || any(!nzchar(requested)) || anyDuplicated(requested) ||
      endsWith(scenario_arg, ',')) stop('Scenario selection must be nonempty and unique')
  unknown <- setdiff(requested, policy_ids)
  if (length(unknown)) stop('Unknown policy scenario(s): ', paste(unknown, collapse = ', '))
  policy_ids <- requested
}
if (!length(policy_ids)) stop('Runscript must contain at least one policy scenario')
scenario_ids <- c(baseline_id, policy_ids)

source_years <- parse_years(run_spec[id == baseline_id, years][[1]])
if (any(!fiscal_years %in% source_years)) {
  stop('Requested fiscal years fall outside the source horizon')
}
calendar_years <- sort(unique(c(
  fiscal_years,
  fiscal_years[fiscal_years > min(source_years)] - 1L
)))

for (scenario in scenario_ids) {
  scenario_years <- parse_years(run_spec[id == scenario, years][[1]])
  if (!identical(scenario_years, source_years)) {
    stop('Scenario year range does not match baseline: ', scenario)
  }
}

medicare_rate <- 0.0145
oasi_rate <- 0.062
parent_unit_types <- c('c1', 'c2plus')
level_columns <- c(
  'total_parent_earnings',
  'total_medicare_taxable_earnings',
  'total_oasi_taxable_earnings',
  'total_income_tax_and_policy_credit_revenue',
  'total_employee_payroll_revenue',
  'total_employer_payroll_revenue',
  'total_tax_revenue'
)
selected_value <- function(dt, stub) {
  fcase(
    dt$employment_choice == 'none', dt[[paste0(stub, '.none')]],
    dt$employment_choice == 'pt', dt[[paste0(stub, '.pt')]],
    dt$employment_choice == 'ft', dt[[paste0(stub, '.ft')]],
    default = NA_real_
  )
}

summarise_parent_units <- function(path) {
  required <- c(
    'epsilon_id', 'per_weight1', 'per_weight2', 'employment_choice',
    'oasi_tax_max', 'taxes',
    paste0('earnings1.', c('none', 'pt', 'ft')),
    paste0('earnings2.', c('none', 'pt', 'ft'))
  )
  dt <- fread(file = path, select = required, na.strings = c('', 'NA'))
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop('Missing columns in ', path, ': ', paste(missing, collapse = ', '))
  if (anyNA(dt$epsilon_id) || any(dt$epsilon_id != 1)) {
    stop('Expected every epsilon_id to equal one in ', path)
  }
  if (any(is.na(dt$employment_choice)) ||
      any(!dt$employment_choice %in% c('none', 'pt', 'ft'))) {
    stop('Invalid employment choice in ', path)
  }
  if (any(!is.finite(dt$per_weight1)) || any(dt$per_weight1 <= 0) ||
      any(!is.na(dt$per_weight2) &
          (!is.finite(dt$per_weight2) | dt$per_weight2 <= 0)) ||
      any(!is.finite(dt$oasi_tax_max)) || any(dt$oasi_tax_max <= 0) ||
      any(!is.finite(dt$taxes))) {
    stop('Invalid weight, tax cap, or tax value in ', path)
  }

  e1 <- pmax(0, fcoalesce(as.numeric(selected_value(dt, 'earnings1')), 0))
  e2 <- pmax(0, fcoalesce(as.numeric(selected_value(dt, 'earnings2')), 0))
  parent_earnings <- e1 + e2
  oasi_taxable_earnings <-
    pmin(e1, dt$oasi_tax_max) + pmin(e2, dt$oasi_tax_max)
  employee_payroll <-
    medicare_rate * parent_earnings + oasi_rate * oasi_taxable_earnings
  employer_payroll <- employee_payroll
  income_tax_and_policy_credit <- dt$taxes - employee_payroll
  weight <- (dt$per_weight1 + fcoalesce(as.numeric(dt$per_weight2), as.numeric(dt$per_weight1))) / 2

  values <- c(
    total_parent_earnings = sum(weight * parent_earnings),
    total_medicare_taxable_earnings = sum(weight * parent_earnings),
    total_oasi_taxable_earnings = sum(weight * oasi_taxable_earnings),
    total_income_tax_and_policy_credit_revenue =
      sum(weight * income_tax_and_policy_credit),
    total_employee_payroll_revenue = sum(weight * employee_payroll),
    total_employer_payroll_revenue = sum(weight * employer_payroll),
    total_tax_revenue = sum(weight * (dt$taxes + employer_payroll))
  ) / 1e9
  if (any(!is.finite(values))) stop('Non-finite aggregate from ', path)

  list(
    values = values,
    rows_unweighted = nrow(dt),
    parent_units_weighted = sum(weight),
    input_bytes = file.info(path)$size
  )
}

summarise_scenario_year <- function(scenario_id, year_value) {
  pieces <- lapply(parent_unit_types, function(unit_type) {
    path <- file.path(
      run_root, 'simulation', scenario_id, 'data',
      sprintf('parent_units_%s_%d.csv', unit_type, year_value)
    )
    if (!file.exists(path)) stop('Parent-unit file not found: ', path)
    result <- summarise_parent_units(path)
    piece <- as.data.table(as.list(result$values))
    piece[, `:=`(
      unit_type = unit_type,
      rows_unweighted = result$rows_unweighted,
      parent_units_weighted = result$parent_units_weighted,
      input_bytes = result$input_bytes,
      input_path = normalizePath(path, winslash = '/', mustWork = TRUE)
    )]
    piece
  })
  detail <- rbindlist(pieces, fill = TRUE)
  totals <- detail[, lapply(.SD, sum), .SDcols = c(
    level_columns, 'rows_unweighted', 'parent_units_weighted', 'input_bytes'
  )]
  totals[, `:=`(scenario = scenario_id, calendar_year = year_value)]
  detail[, `:=`(scenario = scenario_id, calendar_year = year_value)]
  list(totals = totals, inputs = detail[, .(
    scenario, calendar_year, unit_type, input_path, input_bytes,
    rows_unweighted, parent_units_weighted
  )])
}

tasks <- CJ(scenario = scenario_ids, calendar_year = calendar_years, unique = TRUE)
message('Reading ', nrow(tasks) * length(parent_unit_types), ' parent-unit files')
task_results <- if (workers == 1L) {
  lapply(seq_len(nrow(tasks)), function(index) {
    summarise_scenario_year(tasks$scenario[[index]], tasks$calendar_year[[index]])
  })
} else {
  parallel::mclapply(seq_len(nrow(tasks)), function(index) {
    summarise_scenario_year(tasks$scenario[[index]], tasks$calendar_year[[index]])
  }, mc.cores = workers)
}
failed_tasks <- which(vapply(task_results, inherits, logical(1), what = 'try-error'))
if (length(failed_tasks)) {
  stop('Failed extraction task(s): ', paste(failed_tasks, collapse = ', '))
}

calendar_levels <- rbindlist(lapply(task_results, `[[`, 'totals'), fill = TRUE)
input_manifest <- rbindlist(lapply(task_results, `[[`, 'inputs'), fill = TRUE)
setcolorder(calendar_levels, c(
  'scenario', 'calendar_year', level_columns,
  'rows_unweighted', 'parent_units_weighted', 'input_bytes'
))
setorder(calendar_levels, scenario, calendar_year)

level_tax_residual <- calendar_levels$total_tax_revenue -
  (calendar_levels$total_income_tax_and_policy_credit_revenue +
     calendar_levels$total_employee_payroll_revenue +
     calendar_levels$total_employer_payroll_revenue)
if (any(abs(level_tax_residual) > tolerance_b)) {
  stop('Calendar-level tax-component identity failed')
}
if (any(abs(calendar_levels$total_parent_earnings -
            calendar_levels$total_medicare_taxable_earnings) > tolerance_b)) {
  stop('Calendar-level Medicare earnings identity failed')
}
if (any(abs(calendar_levels$total_employee_payroll_revenue -
            calendar_levels$total_employer_payroll_revenue) > tolerance_b)) {
  stop('Calendar-level employer payroll mirror identity failed')
}

fiscalize <- function(dt, value_cols, count_cols = character()) {
  rbindlist(lapply(unique(dt$scenario), function(scenario_id) {
    scenario_dt <- dt[scenario == scenario_id]
    rbindlist(lapply(fiscal_years, function(fiscal_year) {
      current <- scenario_dt[calendar_year == fiscal_year]
      if (nrow(current) != 1L) stop('Missing current calendar year for ', scenario_id)
      if (fiscal_year == min(source_years)) {
        prior_values <- setNames(rep(0, length(value_cols)), value_cols)
      } else {
        prior <- scenario_dt[calendar_year == fiscal_year - 1L]
        if (nrow(prior) != 1L) stop('Missing prior calendar year for ', scenario_id)
        prior_values <- unlist(prior[, ..value_cols], use.names = TRUE)
      }
      current_values <- unlist(current[, ..value_cols], use.names = TRUE)
      values <- 0.25 * prior_values + 0.75 * current_values
      out <- data.table(scenario = scenario_id, fiscal_year = fiscal_year)
      for (column in value_cols) set(out, j = column, value = values[[column]])
      for (column in count_cols) set(out, j = column, value = current[[column]])
      out
    }))
  }))
}

fiscal_levels <- fiscalize(
  calendar_levels,
  level_columns,
  c('rows_unweighted', 'parent_units_weighted', 'input_bytes')
)
setorder(fiscal_levels, scenario, fiscal_year)

phase_in_factor <- function(scenario, calendar_year) {
  row <- run_spec[id == scenario]
  scenario_years <- parse_years(row$years[[1]])
  index <- match(calendar_year, scenario_years)
  if (is.na(index)) stop('Year outside scenario phase-in path')
  phase_years <- suppressWarnings(as.integer(row$phase_in_years[[1]]))
  if (is.na(phase_years) || phase_years < 1L) return(1)
  min(index / phase_years, 1)
}

calendar_deltas <- rbindlist(lapply(policy_ids, function(scenario_id) {
  rbindlist(lapply(calendar_years, function(year_value) {
    policy <- calendar_levels[
      scenario == scenario_id & calendar_year == year_value
    ]
    baseline <- calendar_levels[
      scenario == baseline_id & calendar_year == year_value
    ]
    if (nrow(policy) != 1L || nrow(baseline) != 1L) {
      stop('Missing policy or baseline calendar level')
    }
    full <- unlist(policy[, ..level_columns], use.names = TRUE) -
      unlist(baseline[, ..level_columns], use.names = TRUE)
    phase <- phase_in_factor(scenario_id, year_value)
    full_names <- paste0('full_', sub('^total_', '', level_columns), '_change')
    out <- as.data.table(as.list(setNames(full, full_names)))
    out[, `:=`(
      scenario = scenario_id,
      calendar_year = year_value,
      calendar_year_phase_in_factor = phase
    )]
    out[, `:=`(
      parent_earnings_change = phase * full_parent_earnings_change,
      medicare_taxable_earnings_change =
        phase * full_medicare_taxable_earnings_change,
      oasi_taxable_earnings_change = phase * full_oasi_taxable_earnings_change,
      behavioral_employee_payroll_change =
        phase * full_employee_payroll_revenue_change,
      behavioral_employer_payroll_change =
        phase * full_employer_payroll_revenue_change
    )]
    out
  }))
}))

delta_fiscal_columns <- setdiff(
  names(calendar_deltas),
  c('scenario', 'calendar_year', 'calendar_year_phase_in_factor')
)
fiscal_deltas <- fiscalize(calendar_deltas, delta_fiscal_columns)
phase_lookup <- calendar_deltas[
  calendar_year %in% fiscal_years,
  .(scenario, fiscal_year = calendar_year, calendar_year_phase_in_factor)
]
fiscal_deltas <- merge(
  fiscal_deltas, phase_lookup,
  by = c('scenario', 'fiscal_year'), all.x = TRUE, sort = FALSE
)

canonical_levels <- rbindlist(lapply(scenario_ids, function(scenario_id) {
  path <- file.path(
    run_root, 'simulation', scenario_id, 'totals', 'levels', 'budget_effect.csv'
  )
  dt <- fread(file = path)[fiscal_year %in% fiscal_years]
  if (!all(c('fiscal_year', 'total_tax_revenue') %in% names(dt))) {
    stop('Canonical level file is missing required fields: ', path)
  }
  if (anyNA(dt$fiscal_year) || anyDuplicated(dt$fiscal_year) ||
      length(dt$fiscal_year) != length(fiscal_years) ||
      any(sort(dt$fiscal_year) != fiscal_years)) {
    stop('Canonical level file does not contain exactly one row per requested year: ',
         path)
  }
  dt[, .(
    scenario = scenario_id,
    fiscal_year,
    canonical_total_tax_revenue = total_tax_revenue
  )]
}))

canonical_deltas <- rbindlist(lapply(policy_ids, function(scenario_id) {
  path <- file.path(
    run_root, 'simulation', scenario_id, 'totals', 'deltas', 'budget_effect.csv'
  )
  history <- fread(file = path)
  required <- c('fiscal_year', 'mechanical_tax_change',
                'behavioral_tax_change', 'total_tax_change')
  if (!all(required %in% names(history))) {
    stop('Canonical delta file is missing required fields: ', path)
  }
  # Invert FY_m = 0.25 * prior_CY_m + 0.75 * current_CY_m, starting
  # with the native zero preceding-year contribution. Only the small canonical
  # table needs earlier rows; no additional parent records are read.
  recovery_years <- source_years[source_years <= max(fiscal_years)]
  history <- history[fiscal_year %in% recovery_years][order(fiscal_year)]
  if (anyNA(history$fiscal_year) || anyDuplicated(history$fiscal_year) ||
      !identical(as.integer(history$fiscal_year), as.integer(recovery_years)) ||
      any(!is.finite(history$mechanical_tax_change))) {
    stop('Canonical mechanical tax history must cover source start through requested years: ', path)
  }
  calendar_mechanical <- numeric(length(recovery_years))
  previous <- 0
  for (i in seq_along(recovery_years)) {
    calendar_mechanical[[i]] <- (history$mechanical_tax_change[[i]] - 0.25 * previous) / 0.75
    previous <- calendar_mechanical[[i]]
  }
  names(calendar_mechanical) <- as.character(recovery_years)
  full_calendar <- calendar_deltas[scenario == scenario_id]
  expected_behavioral <- vapply(fiscal_years, function(year_value) {
    years <- if (year_value == min(source_years)) year_value else c(year_value - 1L, year_value)
    weights <- if (length(years) == 1L) 0.75 else c(0.25, 0.75)
    full <- full_calendar$full_tax_revenue_change[match(years, full_calendar$calendar_year)]
    phases <- vapply(years, function(y) phase_in_factor(scenario_id, y), numeric(1))
    sum(weights * phases * (full - calendar_mechanical[as.character(years)]))
  }, numeric(1))
  dt <- history[fiscal_year %in% fiscal_years]
  if (any(!is.finite(dt$behavioral_tax_change)) || any(!is.finite(dt$total_tax_change)) ||
      any(!is.finite(expected_behavioral))) {
    stop('Non-finite canonical tax decomposition: ', path)
  }
  dt[, .(scenario = scenario_id, fiscal_year, mechanical_tax_change,
         behavioral_tax_change, total_tax_change,
         expected_behavioral_tax_change = expected_behavioral)]
}))

fiscal_levels <- merge(
  fiscal_levels, canonical_levels,
  by = c('scenario', 'fiscal_year'), all.x = TRUE, sort = FALSE
)
fiscal_levels[, level_tax_reconciliation_residual :=
  total_tax_revenue - canonical_total_tax_revenue]

fiscal_deltas <- merge(
  fiscal_deltas, canonical_deltas,
  by = c('scenario', 'fiscal_year'), all.x = TRUE, sort = FALSE
)
canonical_full_tax <- rbindlist(lapply(policy_ids, function(scenario_id) {
  policy <- fiscal_levels[scenario == scenario_id, .(
    fiscal_year, policy_canonical_tax = canonical_total_tax_revenue
  )]
  baseline <- fiscal_levels[scenario == baseline_id, .(
    fiscal_year, baseline_canonical_tax = canonical_total_tax_revenue
  )]
  merge(policy, baseline, by = 'fiscal_year', all = FALSE)[, .(
    scenario = scenario_id,
    fiscal_year,
    canonical_full_tax_change = policy_canonical_tax - baseline_canonical_tax
  )]
}))
fiscal_deltas <- merge(
  fiscal_deltas, canonical_full_tax,
  by = c('scenario', 'fiscal_year'), all.x = TRUE, sort = FALSE
)
fiscal_deltas[, `:=`(
  full_income_tax_and_policy_credit_change =
    full_income_tax_and_policy_credit_revenue_change,
  full_employee_payroll_change = full_employee_payroll_revenue_change,
  full_employer_payroll_change = full_employer_payroll_revenue_change,
  behavioral_income_tax_and_policy_credit_change =
    behavioral_tax_change - behavioral_employee_payroll_change -
      behavioral_employer_payroll_change,
  total_employee_payroll_change = behavioral_employee_payroll_change,
  total_employer_payroll_change = behavioral_employer_payroll_change
)]
fiscal_deltas[, total_income_tax_and_policy_credit_change :=
  total_tax_change - total_employee_payroll_change -
    total_employer_payroll_change]

full_tax_component_residual <- fiscal_deltas$full_tax_revenue_change -
  (fiscal_deltas$full_income_tax_and_policy_credit_change +
     fiscal_deltas$full_employee_payroll_change +
     fiscal_deltas$full_employer_payroll_change)
canonical_tax_identity_residual <- fiscal_deltas$total_tax_change -
  fiscal_deltas$mechanical_tax_change - fiscal_deltas$behavioral_tax_change

reconciliation <- rbindlist(list(
  fiscal_levels[, .(
    scenario, fiscal_year,
    check = 'extracted_level_tax_equals_canonical_level_tax',
    observed = total_tax_revenue,
    expected = canonical_total_tax_revenue,
    residual = level_tax_reconciliation_residual,
    tolerance_b = tolerance_b,
    passed = abs(level_tax_reconciliation_residual) <= tolerance_b
  )],
  fiscal_deltas[, .(
    scenario, fiscal_year,
    check = 'full_tax_components_sum_to_full_tax_change',
    observed = full_income_tax_and_policy_credit_change +
      full_employee_payroll_change + full_employer_payroll_change,
    expected = full_tax_revenue_change,
    residual = full_tax_component_residual,
    tolerance_b = tolerance_b,
    passed = abs(full_tax_component_residual) <= tolerance_b
  )],
  fiscal_deltas[, .(
    scenario, fiscal_year,
    check = 'canonical_total_equals_mechanical_plus_behavioral',
    observed = mechanical_tax_change + behavioral_tax_change,
    expected = total_tax_change,
    residual = canonical_tax_identity_residual,
    tolerance_b = tolerance_b,
    passed = abs(canonical_tax_identity_residual) <= tolerance_b
  )],
  fiscal_deltas[, .(
    scenario, fiscal_year,
    check = 'canonical_behavioral_tax_matches_phase_in',
    observed = behavioral_tax_change,
    expected = expected_behavioral_tax_change,
    residual = behavioral_tax_change - expected_behavioral_tax_change,
    tolerance_b = tolerance_b,
    passed = abs(behavioral_tax_change - expected_behavioral_tax_change) <= tolerance_b
  )],
  fiscal_deltas[, .(
    scenario, fiscal_year,
    check = 'employee_payroll_equals_ecec_employer_mirror',
    observed = total_employee_payroll_change,
    expected = total_employer_payroll_change,
    residual = total_employee_payroll_change - total_employer_payroll_change,
    tolerance_b = tolerance_b,
    passed = abs(total_employee_payroll_change -
                   total_employer_payroll_change) <= tolerance_b
  )],
  fiscal_deltas[, .(
    scenario, fiscal_year,
    check = 'extracted_full_tax_change_equals_canonical_level_difference',
    observed = full_tax_revenue_change,
    expected = canonical_full_tax_change,
    residual = full_tax_revenue_change - canonical_full_tax_change,
    tolerance_b = tolerance_b,
    passed = abs(full_tax_revenue_change - canonical_full_tax_change) <= tolerance_b
  )]
))

if (anyNA(reconciliation$passed) || any(!reconciliation$passed)) {
  print(reconciliation[is.na(passed) | !passed])
  stop('One or more parental-income extraction checks failed')
}

drop_delta_cols <- c(
  'expected_behavioral_tax_change',
  'full_income_tax_and_policy_credit_revenue_change',
  'full_employee_payroll_revenue_change',
  'full_employer_payroll_revenue_change'
)
fiscal_deltas[, (drop_delta_cols) := NULL]
setcolorder(fiscal_deltas, c(
  'scenario', 'fiscal_year', 'calendar_year_phase_in_factor',
  'full_parent_earnings_change', 'parent_earnings_change',
  'full_medicare_taxable_earnings_change', 'medicare_taxable_earnings_change',
  'full_oasi_taxable_earnings_change', 'oasi_taxable_earnings_change',
  'full_income_tax_and_policy_credit_change',
  'full_employee_payroll_change', 'full_employer_payroll_change',
  'behavioral_income_tax_and_policy_credit_change',
  'behavioral_employee_payroll_change', 'behavioral_employer_payroll_change',
  'total_income_tax_and_policy_credit_change',
  'total_employee_payroll_change', 'total_employer_payroll_change',
  'mechanical_tax_change', 'behavioral_tax_change', 'total_tax_change',
  'full_tax_revenue_change', 'canonical_full_tax_change'
))
setorder(fiscal_deltas, scenario, fiscal_year)
setorder(reconciliation, scenario, fiscal_year, check)
setorder(input_manifest, scenario, calendar_year, unit_type)

script_arg <- commandArgs(trailingOnly = FALSE)
script_arg <- sub('^--file=', '', script_arg[startsWith(script_arg, '--file=')])
script_hash <- if (length(script_arg) == 1L && file.exists(script_arg)) {
  unname(tools::md5sum(script_arg))
} else {
  NA_character_
}
manifest <- c(
  paste('created_at', format(Sys.time(), tz = 'America/New_York', usetz = TRUE), sep = '='),
  paste('source_run_root', run_root, sep = '='),
  paste('source_run_id', basename(run_root), sep = '='),
  paste('fiscal_years', paste(fiscal_years, collapse = ','), sep = '='),
  paste('calendar_years_read', paste(calendar_years, collapse = ','), sep = '='),
  paste('scenarios', paste(policy_ids, collapse = ','), sep = '='),
  paste('workers', workers, sep = '='),
  paste('tolerance_b', format(tolerance_b, scientific = TRUE), sep = '='),
  paste('input_files', nrow(input_manifest), sep = '='),
  paste('input_bytes', sum(input_manifest$input_bytes), sep = '='),
  paste('script_md5', script_hash, sep = '='),
  paste('r_version', R.version.string, sep = '=')
)

publish_outputs <- function() {
  stage_dir <- tempfile(
    pattern = paste0('.', basename(output_dir), '.tmp-'),
    tmpdir = output_parent
  )
  if (!dir.create(stage_dir, recursive = FALSE, showWarnings = FALSE)) {
    stop('Could not create staging directory: ', stage_dir)
  }
  on.exit({
    if (dir.exists(stage_dir)) unlink(stage_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  fwrite(calendar_levels, file.path(stage_dir, 'calendar_levels.csv'))
  fwrite(fiscal_levels, file.path(stage_dir, 'fiscal_levels.csv'))
  fwrite(fiscal_deltas, file.path(stage_dir, 'fiscal_deltas.csv'))
  fwrite(reconciliation, file.path(stage_dir, 'reconciliation.csv'))
  fwrite(input_manifest, file.path(stage_dir, 'input_manifest.csv'))
  writeLines(manifest, file.path(stage_dir, 'MANIFEST.txt'))
  writeLines('complete', file.path(stage_dir, 'COMPLETE'))
  if (file.exists(output_dir)) {
    stop('Output path appeared while extraction was running: ', output_dir)
  }
  if (!file.rename(stage_dir, output_dir)) {
    stop('Could not publish completed output directory: ', output_dir)
  }
}

publish_outputs()

message('Wrote parental-income extraction to ', output_dir)
message('Maximum level-tax reconciliation residual (billions): ',
        format(max(abs(fiscal_levels$level_tax_reconciliation_residual)), scientific = TRUE))
