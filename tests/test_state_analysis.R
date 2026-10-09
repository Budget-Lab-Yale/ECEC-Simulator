#------------------------------------------------------------------------------
# test_state_analysis.R
#
# Unit tests for the state-level analysis demand contraction
# (adjust_alpha_for_state). Synthetic data only - no model data dependencies.
#
# Run: Rscript tests/test_state_analysis.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(magrittr)
  library(stringr)
})

source('src/shared_functions/constants.R')
source('src/shared_functions/get_choice_catalog.R')
source('src/shared_functions/adjust_alpha_for_state.R')

set.seed(42)

cat('=== test_state_analysis.R ===\n')


#------------------------------
# Synthetic alpha data builder
#------------------------------

make_alpha_data <- function(n_units, n_children, hh_id_start = 1) {

  # Random base-year probabilities (rows sum to 1) and the alpha matrix that
  # reproduces them under V = 0: alpha = log(p0), stored normalized to choice 1
  catalog <- get_choice_catalog(n_children)
  n_choices <- nrow(catalog)

  p0 <- matrix(runif(n_units * n_choices, 0.01, 1), nrow = n_units)
  p0 <- p0 / rowSums(p0)

  raw_alpha <- log(p0)
  alpha <- (raw_alpha - raw_alpha[, 1])[, -1, drop = FALSE]

  weights <- data.frame(child_weight.1 = runif(n_units, 50, 150))
  if (n_children == 2) {
    weights$child_weight.2 <- runif(n_units, 50, 150)
  }

  list(
    alpha = alpha,
    p0 = p0,
    child_weights = weights,
    row_ids = data.frame(
      hh_id = hh_id_start:(hh_id_start + n_units - 1),
      parent_unit_id = 1,
      pseudofamily_id = 1
    )
  )
}


# Aggregate annual hours by care type from probabilities (independent
# reimplementation of the aggregation for verification)
hours_by_type <- function(p, n_children, w1, w2, target_types) {
  catalog <- get_choice_catalog(n_children)
  g1 <- state_target_group_of(catalog$child1_ecec_type)
  g2 <- if (n_children == 2) state_target_group_of(catalog$child2_ecec_type) else NULL
  H <- setNames(rep(0, length(target_types)), target_types)
  for (t in target_types) {
    h1 <- unname(HOURS_ANNUAL[catalog$child1_hours_choice]) *
      (!is.na(g1) & g1 == t)
    H[t] <- H[t] + sum((p %*% h1) * w1)
    if (n_children == 2) {
      h2 <- unname(HOURS_ANNUAL[catalog$child2_hours_choice]) *
        (!is.na(g2) & g2 == t)
      H[t] <- H[t] + sum((p %*% h2) * w2)
    }
  }
  H
}


# Hours for ONE care type, ungrouped -- shows shared centre adjustment leaves
# low/high split alone
hours_one_type <- function(p, n_children, w1, w2, ecec_type) {
  catalog <- get_choice_catalog(n_children)
  h1 <- unname(HOURS_ANNUAL[catalog$child1_hours_choice]) *
    (catalog$child1_ecec_type == ecec_type)
  H <- sum((p %*% h1) * w1)
  if (n_children == 2) {
    h2 <- unname(HOURS_ANNUAL[catalog$child2_hours_choice]) *
      (catalog$child2_ecec_type == ecec_type)
    H <- H + sum((p %*% h2) * w2)
  }
  H
}


#------------------------------
# Build synthetic state
#------------------------------

n_c1 <- 200
n_c2 <- 150
alpha_data_by_type <- list(
  c1     = make_alpha_data(n_c1, 1, hh_id_start = 1),
  c2plus = make_alpha_data(n_c2, 2, hh_id_start = 1001)
)

# The 'state' is a subset of households of each type
state_hh_ids <- c(1:120, 1001:1090)
target_types <- names(STATE_TARGET_GROUPS)
stopifnot(length(target_types) == 6)

in_state_c1 <- alpha_data_by_type$c1$row_ids$hh_id %in% state_hh_ids
in_state_c2 <- alpha_data_by_type$c2plus$row_ids$hh_id %in% state_hh_ids

# Base-year hours over state households at delta = 0
H0 <- hours_by_type(alpha_data_by_type$c1$p0[in_state_c1, ], 1,
                    alpha_data_by_type$c1$child_weights$child_weight.1[in_state_c1],
                    NULL, target_types) +
      hours_by_type(alpha_data_by_type$c2plus$p0[in_state_c2, ], 2,
                    alpha_data_by_type$c2plus$child_weights$child_weight.1[in_state_c2],
                    alpha_data_by_type$c2plus$child_weights$child_weight.2[in_state_c2],
                    target_types)

# Targets: perturb the base aggregates by type-specific factors
scale_factors <- setNames(c(1.30, 0.75, 1.10, 0.90, 1.20, 0.85), target_types)
targets <- H0 * scale_factors

targets_csv <- tempfile(fileext = '.csv')
write_csv(tibble(ecec_type = names(targets), annual_hours = unname(targets)), targets_csv)


#------------------------------
# Test 1: contraction hits the targets
#------------------------------

result <- adjust_alpha_for_state(
  alpha_data_by_type = alpha_data_by_type,
  state_hh_ids       = state_hh_ids,
  targets_csv_path   = targets_csv,
  state_postal       = 'ZZ'
)

stopifnot(max(abs(log(result$achieved / result$targets))) < 1e-6)
cat('PASS: contraction converged to targets (', result$n_iter, ' iterations)\n', sep = '')


#------------------------------
# Test 1b: the combined centre target is hit, and the shared adjustment
# leaves the low/high split alone
#------------------------------
# One delta for two bands pins their SUM, leaves their ratio alone. Anything
# moving ratio here = adjustment reaching where it should not.

centre_bands <- STATE_TARGET_GROUPS[['Center-Based']]
zero <- setNames(rep(0, length(target_types)), target_types)

# State-row choice probabilities for one pu type: p0 x exp(delta), renormalized
state_probs <- function(pu, deltas) {
  ad <- alpha_data_by_type[[pu]]
  m  <- build_choice_hours_map(if (pu == 'c1') 1 else 2, target_types)
  U  <- sweep(ad$p0[ad$row_ids$hh_id %in% state_hh_ids, , drop = FALSE], 2,
              exp(m$d_by_choice(deltas)), '*')
  U / rowSums(U)
}

# Hours per centre band at `deltas`, both pu types
band_hours <- function(deltas) {
  out <- setNames(numeric(length(centre_bands)), centre_bands)
  for (pu in names(alpha_data_by_type)) {
    ad  <- alpha_data_by_type[[pu]]
    nch <- if (pu == 'c1') 1 else 2
    ins <- ad$row_ids$hh_id %in% state_hh_ids
    P   <- state_probs(pu, deltas)
    w2  <- if (nch == 2) ad$child_weights$child_weight.2[ins]
    for (b in centre_bands) {
      out[b] <- out[b] + hours_one_type(P, nch, ad$child_weights$child_weight.1[ins], w2, b)
    }
  }
  out
}

stopifnot(abs(sum(band_hours(result$deltas)) /
                result$targets[['Center-Based']] - 1) < 1e-6)
cat('PASS: combined centre hours hit the single centre target\n')

# SHARED DELTA GUARANTEES: no FAMILY's odds between the two centre bands move.
# Exact, so it's the property worth testing. Pooled hours ratios aren't exact
# -- weighted averages whose weights shift as families move toward or away
# from centre care.
c1_band_odds <- function(deltas) {
  P  <- state_probs('c1', deltas)
  ty <- get_choice_catalog(1)$child1_ecec_type
  rowSums(P[, ty == centre_bands[1], drop = FALSE]) /
    rowSums(P[, ty == centre_bands[2], drop = FALSE])
}

log_dev <- max(abs(log(c1_band_odds(result$deltas) / c1_band_odds(zero))))
stopifnot(log_dev < 1e-12)
cat(sprintf('PASS: every family\'s low/high odds unchanged (max log dev %.1e)\n', log_dev))

# POOLED hours ratio does move -- understood, not asserted away. Two
# mechanisms: families reweight toward centre care; two-child choice carries
# second child's delta, so bands beside different co-child types drift apart
# as other five targets move.
r_before <- Reduce(`/`, band_hours(zero))
r_after  <- Reduce(`/`, band_hours(result$deltas))
cat(sprintf('NOTE: pooled low/high hours ratio moves %+.2f%% -- reweighting,\n',
            100 * (r_after / r_before - 1)))
cat('      not the centre adjustment reaching between the bands.\n')


#------------------------------
# Test 1b2: sector hours at the solution -- the basis of base-year labor
#------------------------------
# 3a builds base-year labor as colSums(L_req * sector_hours), so sector hours
# must be model's own base-year hours: formal targets, centre target split by
# model's own bands.

sh   <- result$sector_hours
tg   <- result$targets
close_to <- function(a, b, tol = 1e-6) all(abs(a / b - 1) < tol)
stopifnot(identical(names(sh), SECTOR_MAPPING$ecec_type),
          close_to(sh[['Unpaid Center-Based']], tg[['Unpaid Center-Based']]),
          close_to(sh[['Paid Home-Based']], tg[['Paid Home-Based']]),
          close_to(sh[['Low-Priced Center-Based']] + sh[['High-Priced Center-Based']],
                   tg[['Center-Based']]),
          close_to(sh[centre_bands], band_hours(result$deltas)[centre_bands], 1e-9))
cat('PASS: sector hours reproduce the formal targets and the model\'s own centre bands\n')


#------------------------------
# Test 1c: a target file naming a price band is rejected
#------------------------------
split_csv <- tempfile(fileext = '.csv')
write_csv(tibble(ecec_type = c('Low-Priced Center-Based', names(targets)[-1]),
                 annual_hours = unname(targets)), split_csv)
stopifnot(tryCatch({ read_state_demand_targets(split_csv, 'ZZ'); FALSE },
                   error = function(e) grepl('price split', conditionMessage(e))))
cat('PASS: a file naming a centre price band is rejected\n')


#------------------------------
# Test 2: adjusted alphas independently reproduce the targets
#------------------------------
# Rebuild probabilities from the returned (stored-normalized) alpha under
# V = 0: p_j proportional to exp(c(0, alpha_stored)). Aggregate hours must
# match the targets - this checks the alpha shift, not just the solver.

probs_from_stored_alpha <- function(alpha_stored) {
  logits <- cbind(0, alpha_stored)
  U <- exp(logits)
  U / rowSums(U)
}

p_c1 <- probs_from_stored_alpha(result$alpha_by_type$c1)
p_c2 <- probs_from_stored_alpha(result$alpha_by_type$c2plus)

H_check <- hours_by_type(p_c1, 1,
                         alpha_data_by_type$c1$child_weights$child_weight.1[in_state_c1],
                         NULL, target_types) +
           hours_by_type(p_c2, 2,
                         alpha_data_by_type$c2plus$child_weights$child_weight.1[in_state_c2],
                         alpha_data_by_type$c2plus$child_weights$child_weight.2[in_state_c2],
                         target_types)

stopifnot(max(abs(log(H_check / targets))) < 1e-6)
cat('PASS: adjusted alphas independently reproduce the targets\n')

# Row subsetting: returned matrices cover exactly the state rows
stopifnot(nrow(result$alpha_by_type$c1) == sum(in_state_c1))
stopifnot(nrow(result$alpha_by_type$c2plus) == sum(in_state_c2))
stopifnot(all(result$row_ids_by_type$c1$hh_id %in% state_hh_ids))
cat('PASS: alpha/row_ids subset to state rows\n')


#------------------------------
# Test 3: no-adjustment identity (targets = base aggregates -> deltas ~ 0)
#------------------------------

identity_csv <- tempfile(fileext = '.csv')
write_csv(tibble(ecec_type = names(H0), annual_hours = unname(H0)), identity_csv)

result_id <- adjust_alpha_for_state(alpha_data_by_type, state_hh_ids, identity_csv, 'ZZ')
stopifnot(max(abs(result_id$deltas)) < 1e-6)
stopifnot(max(abs(result_id$alpha_by_type$c1 -
                  alpha_data_by_type$c1$alpha[in_state_c1, ])) < 1e-6)
cat('PASS: base-aggregate targets leave alphas unchanged\n')


#------------------------------
# Test 4: failure modes fail loudly
#------------------------------

expect_error <- function(expr, pattern, label) {
  err <- tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
  stopifnot(!is.null(err), grepl(pattern, err))
  cat('PASS: ', label, '\n', sep = '')
}

# Zero-support care type: remove all probability mass for one type from p0
zs_data <- alpha_data_by_type
kill_type <- 'Paid Home-Based'
for (pu_type in names(zs_data)) {
  n_children <- if (pu_type == 'c1') 1 else 2
  catalog <- get_choice_catalog(n_children)
  kill_cols <- catalog$child1_ecec_type == kill_type
  if (n_children == 2) kill_cols <- kill_cols | catalog$child2_ecec_type == kill_type
  zs_data[[pu_type]]$p0[, kill_cols] <- 0
  zs_data[[pu_type]]$p0 <- zs_data[[pu_type]]$p0 / rowSums(zs_data[[pu_type]]$p0)
}
expect_error(
  adjust_alpha_for_state(zs_data, state_hh_ids, targets_csv, 'ZZ'),
  'zero base-year support',
  'zero-support care type fails loudly'
)

# Zero/negative target
bad_csv <- tempfile(fileext = '.csv')
write_csv(tibble(ecec_type = names(targets),
                 annual_hours = replace(unname(targets), 1, 0)), bad_csv)
expect_error(
  adjust_alpha_for_state(alpha_data_by_type, state_hh_ids, bad_csv, 'ZZ'),
  'positive annual',
  'zero target fails loudly'
)

# Missing care type row
missing_csv <- tempfile(fileext = '.csv')
write_csv(tibble(ecec_type = names(targets)[-1],
                 annual_hours = unname(targets)[-1]), missing_csv)
expect_error(
  adjust_alpha_for_state(alpha_data_by_type, state_hh_ids, missing_csv, 'ZZ'),
  'missing target group',
  'missing target group fails loudly'
)

# Interface without p0 (predates state support)
no_p0 <- alpha_data_by_type
no_p0$c1$p0 <- NULL
expect_error(
  adjust_alpha_for_state(no_p0, state_hh_ids, targets_csv, 'ZZ'),
  'lacks p0',
  'interface without p0 fails loudly'
)

# No state households
expect_error(
  adjust_alpha_for_state(alpha_data_by_type, c(99991, 99992), targets_csv, 'ZZ'),
  'No calibration households',
  'empty state fails loudly'
)

#------------------------------
# Test 5: sub-state geography (NYC) -- parser and household selection
#------------------------------

source('src/shared_functions/select_geo_hh_ids.R')

# get_run_states() lives in config.R, which runs code when sourced -- take
# only its definition
cfg_exprs <- parse('src/misc/config.R')
is_def <- vapply(cfg_exprs, function(e) {
  is.call(e) && identical(e[[1]], as.name('<-')) &&
    identical(e[[2]], as.name('get_run_states'))
}, logical(1))
stopifnot(sum(is_def) == 1)
eval(cfg_exprs[[which(is_def)]])

rs <- get_run_states(tibble(state = 'NY;NYC;VA'))
stopifnot(identical(names(rs), c('NY', 'NYC', 'VA')),
          unname(rs) == c(36, 36, 51))
cat('PASS: runscript parser accepts NYC, carries STATEFIP 36\n')

expect_error(get_run_states(tibble(state = 'NYX')), 'Unknown state',
             'unknown sub-state code fails loudly')

# NYC boroughs, rest of NY, county unidentified (0), other state on same
# county code
hh <- tibble(hh_id     = 1:7,
             statefip  = c(36, 36, 36, 36, 36, 36, 51),
             countyfip = c(5, 47, 61, 81, 85, 0, 5))
stopifnot(identical(select_geo_hh_ids(hh, 'NY', 36), 1:6),
          identical(select_geo_hh_ids(hh, 'NYC', 36), 1:5),
          identical(select_geo_hh_ids(hh, 'VA', 51), 7L))
cat('PASS: NYC selects the five boroughs, not unidentified NY counties or other states\n')

expect_error(select_geo_hh_ids(select(hh, -countyfip), 'NYC', 36),
             'no countyfip', 'NYC w/out countyfip fails loudly')
expect_error(select_geo_hh_ids(mutate(hh, countyfip = NA), 'NYC', 36),
             'no countyfip', 'NYC w/ all-NA countyfip fails loudly')
stopifnot(identical(select_geo_hh_ids(select(hh, -countyfip), 'NY', 36), 1:6))
cat('PASS: whole-state run needs no countyfip\n')


#------------------------------
# Employment rate growth factor: cached paths, input contract
#------------------------------

source('src/shared_functions/get_employment_rate_growth_factor.R')

# Base-year macro row: 1e6 people per age, married + unmarried
emp_macro <- as_tibble(c(list(year = 2019),
                         setNames(as.list(rep(4e5, 44)), paste0('married_', 16:59)),
                         setNames(as.list(rep(6e5, 44)), paste0('unmarried_', 16:59))))

# Synthetic epop file: all rates 70, except F 16_17 (2 ages) 40 -> 60 and
# F 20_24 (5 ages) 80 -> 70 in 2025
emp_cells <- as.vector(outer(c('epop_m_', 'epop_f_'),
                             unlist(EMPLOYMENT_AGE_BINS, use.names = FALSE), paste0))
emp_epop <- as_tibble(c(list(year = 2019:2026),
                        setNames(rep(list(rep(70, 8)), length(emp_cells)), emp_cells)))
emp_epop$epop_f_16_17 <- c(40, rep(60, 7))
emp_epop$epop_f_20_24 <- c(80, rep(70, 7))
emp_path <- tempfile(fileext = '.csv')
write_csv(emp_epop, emp_path)

g <- get_employment_rate_growth_factor(emp_macro, 2019, 2025, epop_path = emp_path)
stopifnot(identical(names(g), EMPLOYMENT_TARGETING_GROUPS))
# Aggregate first: (2*60 + 2*70 + 5*70) / (2*40 + 2*70 + 5*80) -- not mean of cell ratios
stopifnot(abs(g[['F_u25']] - (2*60 + 2*70 + 5*70) / (2*40 + 2*70 + 5*80)) < 1e-12,
          all(abs(g[-1] - 1) < 1e-12))
cat('PASS: growth = ratio of population-weighted fine-bin aggregates\n')

stopifnot(all(get_employment_rate_growth_factor(emp_macro, 2019, 2019,
                                                epop_path = 'no/such/file.csv') == 1))
cat('PASS: target_year <= base_year returns 1.0 without reading file\n')

n_cache <- length(ls(EPOP_GROWTH_CACHE))
invisible(get_employment_rate_growth_factor(emp_macro, 2019, 2026, epop_path = emp_path))
stopifnot(length(ls(EPOP_GROWTH_CACHE)) == n_cache)
emp_macro2 <- mutate(emp_macro, married_16 = 9e5)
g2 <- get_employment_rate_growth_factor(emp_macro2, 2019, 2025, epop_path = emp_path)
stopifnot(length(ls(EPOP_GROWTH_CACHE)) == n_cache + 1, g2[['F_u25']] != g[['F_u25']])
cat('PASS: cache reused per file x base year, new macro weights -> new entry\n')

for (f in c(NA, 'config/state/employment/NY.csv')) {
  g_real <- get_employment_rate_growth_factor(
    emp_macro, 2019, 2026, epop_path = if (is.na(f)) NULL else f)
  stopifnot(all(is.finite(g_real) & g_real > 0))
}
cat('PASS: national and state epop files give finite growth for 2026\n')

# Input contract: each broken input stops
emp_write <- function(d) { p <- tempfile(fileext = '.csv'); write_csv(d, p); p }
expect_error(get_employment_rate_growth_factor(emp_macro, 2019, 2025, epop_path = 'no/such/file.csv'),
             'not found', 'missing epop file stops')
expect_error(get_employment_rate_growth_factor(emp_macro, 2019, 2025,
                                               epop_path = emp_write(select(emp_epop, -epop_m_30_34))),
             'lacks column.*epop_m_30_34', 'missing epop column stops')
expect_error(get_employment_rate_growth_factor(emp_macro, 2019, 2025,
                                               epop_path = emp_write(mutate(emp_epop, epop_f_40_44 = replace(epop_f_40_44, year == 2025, NA)))),
             'NA or non-positive epop rate.*2025.*F_35_45', 'NA target-year rate stops')
expect_error(get_employment_rate_growth_factor(emp_macro, 2019, 2025,
                                               epop_path = emp_write(mutate(emp_epop, epop_m_55_59 = replace(epop_m_55_59, year == 2019, 0)))),
             'base-year.*M_45plus', 'non-positive base-year rate stops')
expect_error(get_employment_rate_growth_factor(emp_macro, 2019, 2030, epop_path = emp_path),
             'no year 2030', 'missing target year stops')
expect_error(get_employment_rate_growth_factor(mutate(emp_macro, year = 2018), 2018, 2025,
                                               epop_path = emp_path),
             'no base year 2018', 'missing base year stops')
expect_error(get_employment_rate_growth_factor(mutate(emp_macro, year = 2020), 2019, 2025,
                                               epop_path = emp_path),
             '0 rows for base year', 'missing macro base-year row stops')
expect_error(get_employment_rate_growth_factor(select(emp_macro, -unmarried_42), 2019, 2025,
                                               epop_path = emp_path),
             'lack unmarried_42', 'missing macro age column stops')
expect_error(get_employment_rate_growth_factor(mutate(emp_macro, married_25 = NA), 2019, 2025,
                                               epop_path = emp_path),
             'NA population.*25_29', 'NA macro population stops')
expect_error(get_employment_rate_growth_factor(mutate(emp_macro, across(c(married_18, married_19, unmarried_18, unmarried_19), ~ 0)),
                                               2019, 2025, epop_path = emp_path),
             'non-positive population weight.*18_19', 'zero fine-bin weight stops')


#------------------------------
# Income ranks within group (care-choice forest: NSECE by region, ACS by state)
#------------------------------

source('src/shared_functions/assign_income_pctile.R')

# Three states, incomes on different scales -- same rank pattern within each
inc_base <- rexp(5000, 1 / 50000)
inc_grp  <- c(inc_base, 3 * inc_base, inc_base / 2)
grp      <- rep(c(36L, 21L, 51L), each = length(inc_base))
cuts <- lapply(split(inc_grp, grp), function(x) quantile(x, seq(0.01, 0.99, 0.01), names = FALSE))
pct  <- assign_income_pctile(inc_grp, grp, cuts)

stopifnot(all(pct >= 1L & pct <= 100L), is.integer(pct))
# Each group spreads ~1% per bin
for (g in unique(grp)) {
  sh <- tabulate(pct[grp == g], 100) / sum(grp == g)
  stopifnot(max(abs(sh - 0.01)) < 0.005)
}
# Scaled incomes rank identically -- rank is relative to own state
stopifnot(identical(pct[grp == 36L], pct[grp == 21L]), identical(pct[grp == 36L], pct[grp == 51L]))
# One national ranking would not: 3x state sits higher
nat <- assign_income_pctile(inc_grp, rep('US', length(inc_grp)),
                            list(US = quantile(inc_grp, seq(0.01, 0.99, 0.01), names = FALSE)))
stopifnot(mean(nat[grp == 21L]) > mean(nat[grp == 36L]) + 10)
cat('PASS: income ranks uniform within each state, independent of state income level\n')

# Same bins as cut(right = TRUE) on unique cutpoints
old <- as.integer(as.character(cut(inc_base, c(-Inf, cuts[['36']], Inf), labels = 1:100)))
stopifnot(identical(pct[grp == 36L], old))
cat('PASS: matches cut() bins on unique cutpoints\n')

# Tied cutpoints (small state, heavy record) -- no error; NA income/group -> NA
tied <- list(a = sort(c(rep(10, 50), seq(11, 59))))
p_t  <- assign_income_pctile(c(5, 10, 10.5, 100, NA), c('a', 'a', 'a', 'a', 'a'), tied)
stopifnot(identical(p_t, c(1L, 1L, 51L, 100L, NA)))
stopifnot(is.na(assign_income_pctile(50, NA, tied)))
cat('PASS: tied cutpoints take bins, NA income or group gives NA\n')

expect_error(assign_income_pctile(c(1, 2), c('a', 'zz'), tied),
             'no income cutpoints for group.*zz', 'group w/out cutpoints stops')
expect_error(assign_income_pctile(c(1, 2), 'a', tied),
             'income has 2 values, group 1', 'length mismatch stops')


cat('\nAll state-analysis tests passed.\n')
