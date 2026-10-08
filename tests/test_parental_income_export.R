#!/usr/bin/env Rscript
# Run from the repository root. Synthetic records only; no cluster inputs.
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(testthat)
})
source('src/shared_functions/constants.R')
source('src/3_simulation/3d_accumulate_results.R')
source('src/4_output/4a_finalize_output.R')

# The existing finalizer keeps its helpers nested. Load its definitions without
# running Excel or the unrelated distributional-output stages.
helpers <- new.env(parent = globalenv())
for (expr in as.list(body(finalize_simulation))[-1L]) {
  if (is.call(expr) && identical(expr[[1]], as.name('<-'))) {
    name <- as.character(expr[[2]])
    if (name == 'baseline_id') break
    eval(expr, helpers)
  }
}

make_records <- function(policy = FALSE, negative = FALSE) {
  x <- data.frame(
    epsilon_id = 1L, per_weight1 = c(2, 4, 6), per_weight2 = c(NA, 6, 8),
    employment_choice = c('none', 'pt', 'ft'), oasi_tax_max = 100000,
    taxes = c(-1000, 2000, 50000), subsidy = 0,
    child_weight.1 = c(2, 4, 6), child_weight.2 = c(1, 2, 3),
    ecec_type.1 = 'Parent Only', ecec_type.2 = 'Parent Only',
    ecec_hours.1 = 'none', ecec_hours.2 = 'none'
  )
  for (state in c('none', 'pt', 'ft')) {
    x[[paste0('earnings1.', state)]] <- c(0, 10000, 200000)
    x[[paste0('earnings2.', state)]] <- c(NA, 5000, 120000)
  }
  if (policy) {
    factor <- if (negative) 0.9 else 1.1
    for (col in grep('^earnings', names(x), value = TRUE)) x[[col]] <- factor * x[[col]]
    # A policy credit is retained in the income-tax residual, even below zero.
    x$taxes <- x$taxes + if (negative) -2000 else 1000
  }
  x
}

# Expected totals in dollars, independently derived from the three records.
# Weights are 2, 5, 7, and the cap applies to each parent separately.
expected_base <- c(earnings = 2315000, oasi = 1475000,
                   employee = 125017.5, income = 232982.5, total_tax = 483017.5)

test_that('native row and annual bases preserve payroll and signed income tax', {
  x <- make_records()
  b <- compute_parent_fiscal_base(x)
  w <- c(2, 5, 7)
  expect_equal(sum(w * b$parent_earnings), expected_base[['earnings']])
  expect_equal(sum(w * b$oasi_taxable_earnings), expected_base[['oasi']])
  expect_equal(sum(w * b$employee_payroll), expected_base[['employee']])
  expect_equal(sum(w * b$income_tax_and_policy_credit), expected_base[['income']])
  expect_equal(compute_employer_payroll_tax(select(x, -taxes)), b$employer_payroll)
  expect_equal(b$income_tax_and_policy_credit[1], -1000)
  # Existing conventions clip negative earnings and coalesce absent earnings.
  x$earnings1.none[1] <- -100
  expect_equal(compute_parent_fiscal_base(x)$parent_earnings[1], 0)
  x$oasi_tax_max[1] <- NA_real_
  expect_error(compute_parent_fiscal_base(x), 'invalid oasi_tax_max')
})

make_fiscal <- function(policy = FALSE, negative = FALSE) {
  bind_rows(lapply(2026:2028, function(year) {
    x <- make_records(policy, negative)
    aggregate_year_fiscal_cost(list(converged = TRUE,
      parent_units = list(c1 = x, c2plus = x)), year)
  }))
}

baseline <- make_fiscal()
positive <- make_fiscal(TRUE)
negative <- make_fiscal(TRUE, TRUE)
phase <- list(years = 2026:2028, phase_in = c(0.2, 0.4, 0.6))
mechanical <- data.frame(year = 2026:2028, mechanical_demand_subsidy = 0,
  mechanical_supply_subsidy = 0, mechanical_employer_subsidy = 0,
  mechanical_cdctc_cost = 0, mechanical_transfer_cost = 0,
  mechanical_tax_change = 28000 / 1e9)

test_that('phase-in precedes fiscal timing and preserves the canonical tax score', {
  expect_equal(baseline$total_parent_earnings, rep(2 * expected_base[['earnings']] / 1e9, 3))
  d <- helpers$compute_budget_deltas(positive, baseline, mechanical, phase, TRUE)
  expect_equal(d$parent_earnings_change,
               phase$phase_in * (positive$total_parent_earnings - baseline$total_parent_earnings))
  fy <- helpers$convert_to_fiscal_year(d)
  expect_equal(fy$calendar_year_phase_in_factor, phase$phase_in)
  expect_equal(fy$parent_earnings_change,
               0.75 * d$parent_earnings_change + 0.25 * lag(d$parent_earnings_change, default = 0))
  expect_equal(fy$total_tax_change, fy$total_income_tax_and_policy_credit_change +
               fy$total_employee_payroll_change + fy$total_employer_payroll_change)
  dn <- helpers$compute_budget_deltas(negative, baseline, mechanical, phase, TRUE)
  expect_true(all(dn$parent_earnings_change < 0))
  # Absent mechanical decomposition retains the pre-existing full-change fallback.
  fallback <- helpers$compute_budget_deltas(positive, baseline, NULL, phase, TRUE)
  expect_equal(fallback$total_employee_payroll_change, fallback$full_employee_payroll_change)
  expect_equal(fallback$total_tax_change, fallback$total_income_tax_and_policy_credit_change +
               fallback$total_employee_payroll_change + fallback$total_employer_payroll_change)
})

test_that('legacy summaries retain their budget outputs without invented bases', {
  old_base <- select(baseline, -all_of(helpers$parent_fiscal_cols))
  old_policy <- select(positive, -all_of(helpers$parent_fiscal_cols))
  old <- helpers$compute_budget_deltas(old_policy, old_base, mechanical, phase, TRUE)
  new <- helpers$compute_budget_deltas(positive, baseline, mechanical, phase, TRUE)
  expect_equal(old, new[, names(old)])
  expect_false('parent_earnings_change' %in% names(old))
  expect_error(helpers$has_parent_fiscal_base(select(baseline, -total_parent_earnings)),
               'Incomplete parent fiscal')
})

archive <- tempfile('parent income archive ')
dir.create(archive)
for (scenario in c('baseline', 'positive', 'negative')) {
  for (part in c('data', 'totals/levels', 'totals/deltas')) {
    dir.create(file.path(archive, 'simulation', scenario, part), recursive = TRUE)
  }
  for (year in 2026:2028) {
    for (unit in c('c1', 'c2plus')) {
      fwrite(make_records(scenario != 'baseline', scenario == 'negative'),
             file.path(archive, 'simulation', scenario, 'data',
                       sprintf('parent_units_%s_%s.csv', unit, year)))
    }
  }
}
dir.create(file.path(archive, 'metadata'))
fwrite(data.table(id = c('baseline', 'positive', 'negative'),
                  years = '2026:2028', phase_in_years = c(NA, 5, 5)),
       file.path(archive, 'metadata', 'runscript.csv'))
helpers$get_scenario_info <- function(id) list(paths = list(output = file.path(archive, 'simulation', id)))
for (scenario in c('baseline', 'positive', 'negative')) {
  fiscal <- switch(scenario, baseline = baseline, positive = positive, negative = negative)
  helpers$write_accumulated_summaries(scenario, list(fiscal_cost = fiscal))
  if (scenario != 'baseline') {
    d <- helpers$compute_budget_deltas(fiscal, baseline, mechanical, phase, TRUE)
    fwrite(helpers$convert_to_fiscal_year(d),
      file.path(archive, 'simulation', scenario, 'totals/deltas/budget_effect.csv'))
  }
}
run_extract <- function(out, years = '2026:2028', extra = character()) {
  args <- c('--vanilla', 'src/4_output/extract_parental_income_archive.R',
    paste0('--run-root=', archive), paste0('--output-dir=', out),
    paste0('--fiscal-years=', years), extra)
  suppressWarnings(system2(file.path(R.home('bin'), 'Rscript'),
    vapply(args, shQuote, character(1)), stdout = TRUE, stderr = TRUE))
}
status <- function(x) if (is.null(attr(x, 'status'))) 0L else attr(x, 'status')
out <- tempfile('parent income output ')

test_that('archive extraction agrees with native fiscal outputs and leaves sources intact', {
  sources <- list.files(archive, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(sources)
  log <- run_extract(out)
  expect_equal(status(log), 0L, info = paste(log, collapse = '\n'))
  expect_true(file.exists(file.path(out, 'COMPLETE')))
  expect_identical(tools::md5sum(sources), before)
  checks <- fread(file.path(out, 'reconciliation.csv'))
  expect_true(all(checks$passed))
  extracted <- fread(file.path(out, 'fiscal_deltas.csv'))
  for (scenario_id in c('positive', 'negative')) {
    native <- fread(file.path(archive, 'simulation', scenario_id, 'totals/deltas/budget_effect.csv'))
    common <- intersect(names(native), names(extracted))
    expect_equal(as.data.frame(extracted[extracted$scenario == scenario_id, ..common]),
                 as.data.frame(native[, ..common]), tolerance = 1e-12)
  }
  # A single-year request still reads the preceding calendar year.
  slice <- tempfile('parent income slice ')
  log <- run_extract(slice, '2028', '--scenarios=negative')
  expect_equal(status(log), 0L, info = paste(log, collapse = '\n'))
  a <- fread(file.path(slice, 'fiscal_deltas.csv'))
  b <- extracted[scenario == 'negative' & fiscal_year == 2028]
  expect_equal(a, b)
  if (.Platform$OS.type != 'windows') {
    parallel_out <- tempfile('parent income parallel ')
    log <- run_extract(parallel_out, extra = '--workers=2')
    expect_equal(status(log), 0L, info = paste(log, collapse = '\n'))
    expect_equal(as.data.frame(fread(file.path(parallel_out, 'fiscal_deltas.csv'))),
                 as.data.frame(extracted))
    unlink(parallel_out, recursive = TRUE)
  }
  unlink(slice, recursive = TRUE)
})

test_that('phase metadata and canonical tax decomposition must agree', {
  path <- file.path(archive, 'metadata/runscript.csv')
  spec <- fread(path)
  changed <- copy(spec); changed[id != 'baseline', phase_in_years := 1]
  fwrite(changed, path)
  target <- tempfile('wrong phase ')
  expect_true(status(run_extract(target)) != 0L)
  expect_false(file.exists(target))
  # A different partial ramp is also checked, not only full-phase years.
  changed[id != 'baseline', phase_in_years := 4]
  fwrite(changed, path)
  expect_true(status(run_extract(target, extra = '--tolerance-b=1e-12')) != 0L)
  expect_false(file.exists(target))
  fwrite(spec, path)
})

test_that('omitted phase column matches native immediate implementation', {
  path <- file.path(archive, 'metadata/runscript.csv')
  spec <- fread(path)
  saved <- list()
  for (scenario_id in c('positive', 'negative')) {
    target <- file.path(archive, 'simulation', scenario_id, 'totals/deltas/budget_effect.csv')
    saved[[scenario_id]] <- fread(target)
    fiscal <- if (scenario_id == 'positive') positive else negative
    immediate <- helpers$compute_budget_deltas(fiscal, baseline, mechanical,
      list(years = 2026:2028, phase_in = rep(1, 3)), TRUE)
    fwrite(helpers$convert_to_fiscal_year(immediate), target)
  }
  changed <- copy(spec); changed[id != 'baseline', phase_in_years := 1]
  fwrite(changed, path)
  explicit <- tempfile('explicit phase ')
  log <- run_extract(explicit)
  expect_equal(status(log), 0L, info = paste(log, collapse = '\n'))
  reference <- as.data.frame(fread(file.path(explicit, 'fiscal_deltas.csv')))
  for (mode in c('absent', 'blank')) {
    if (mode == 'absent') changed[, phase_in_years := NULL] else changed[, phase_in_years := NA_real_]
    fwrite(changed, path)
    target <- tempfile('default phase ')
    log <- run_extract(target)
    expect_equal(status(log), 0L, info = paste(log, collapse = '\n'))
    expect_equal(as.data.frame(fread(file.path(target, 'fiscal_deltas.csv'))), reference)
    single <- tempfile('default phase year ')
    expect_equal(status(run_extract(single, '2028')), 0L)
    expected <- reference[reference$fiscal_year == 2028, ]; rownames(expected) <- NULL
    expect_equal(as.data.frame(fread(file.path(single, 'fiscal_deltas.csv'))), expected)
    unlink(c(target, single), recursive = TRUE)
  }
  # Preserve total = mechanical + behavioral while violating the full-phase
  # relationship with parent records. This must still fail.
  target <- file.path(archive, 'simulation/positive/totals/deltas/budget_effect.csv')
  modified <- fread(target)
  modified[fiscal_year == 2027, `:=`(mechanical_tax_change = mechanical_tax_change + 0.001,
                                   total_tax_change = total_tax_change + 0.001)]
  fwrite(modified, target)
  rejected <- tempfile('inconsistent mechanical ')
  expect_true(status(run_extract(rejected)) != 0L)
  expect_false(file.exists(rejected))
  for (scenario_id in names(saved)) fwrite(saved[[scenario_id]],
    file.path(archive, 'simulation', scenario_id, 'totals/deltas/budget_effect.csv'))
  fwrite(spec, path)
  unlink(explicit, recursive = TRUE)
})

test_that('legacy writer emits the old budget table and warns without a new export', {
  old <- select(baseline, -all_of(helpers$parent_fiscal_cols))
  expect_warning(helpers$write_accumulated_summaries('legacy', list(fiscal_cost = old)),
                 'Skipping parental_income.csv for legacy')
  directory <- file.path(archive, 'simulation/legacy/totals/levels')
  expect_true(file.exists(file.path(directory, 'budget_effect.csv')))
  expect_false(file.exists(file.path(directory, 'parental_income.csv')))
  expected <- as.data.frame(helpers$convert_to_fiscal_year(old))
  expect_equal(as.data.frame(fread(file.path(directory, 'budget_effect.csv'))), expected,
               tolerance = 1e-12)
})

test_that('invalid destinations, draws, and canonical totals never publish output', {
  expect_true(status(run_extract(out)) != 0L)
  expect_true(status(run_extract(file.path(archive, 'forbidden'))) != 0L)
  bad <- tempfile('bad income output ')
  expect_true(status(run_extract(bad, extra = '--scenarios=positive,positive')) != 0L)
  expect_true(status(run_extract(bad, years = '2026.5:2028')) != 0L)
  path <- file.path(archive, 'simulation/baseline/data/parent_units_c1_2026.csv')
  original <- fread(path)
  changed <- copy(original); changed$epsilon_id[1] <- 2L; fwrite(changed, path)
  expect_true(status(run_extract(bad)) != 0L)
  expect_false(file.exists(bad))
  fwrite(original, path)
  path <- file.path(archive, 'simulation/baseline/totals/levels/budget_effect.csv')
  changed <- fread(path); changed$total_tax_revenue[1] <- changed$total_tax_revenue[1] + 1
  fwrite(changed, path)
  expect_true(status(run_extract(bad)) != 0L)
  expect_false(file.exists(bad))
})
unlink(c(archive, out), recursive = TRUE)
cat('Parental-income export tests passed\n')
