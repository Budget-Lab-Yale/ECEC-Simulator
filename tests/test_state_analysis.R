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
source('src/shared_functions/get_working_choice_mask.R')

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
# Test 4b: joint re-anchoring -- care hours + caregiver employment together
#------------------------------

# Synthetic caregivers: one row per parent unit, random group and weight
all_ids <- c(alpha_data_by_type$c1$row_ids$hh_id, alpha_data_by_type$c2plus$row_ids$hh_id)
cg_toy <- tibble(hh_id = all_ids, parent_unit_id = 1,
                 pc_group = sample(EMPLOYMENT_TARGETING_GROUPS, length(all_ids), replace = TRUE),
                 per_weight_pc = runif(length(all_ids), 50, 150))
res_j <- adjust_alpha_for_state(alpha_data_by_type, state_hh_ids, targets_csv, 'ZZ', caregivers = cg_toy)

# Independent check from returned alpha (V = 0): hours hit targets, each
# group's employment = its p0 working mass (forest rate), weighted by caregiver
emp_by_group <- function(p_list, cg) {
  num <- 0 * setNames(seq_along(EMPLOYMENT_TARGETING_GROUPS), EMPLOYMENT_TARGETING_GROUPS); den <- num
  for (nm in names(p_list)) {
    n_ch <- if (nm == 'c1') 1 else 2
    ids <- alpha_data_by_type[[nm]]$row_ids$hh_id
    ids <- ids[ids %in% state_hh_ids]
    m <- match(ids, cg$hh_id)
    e <- rowSums(p_list[[nm]][, get_working_choice_mask(n_ch), drop = FALSE])
    for (g in EMPLOYMENT_TARGETING_GROUPS) {
      k <- cg$pc_group[m] == g
      num[g] <- num[g] + sum(e[k] * cg$per_weight_pc[m][k])
      den[g] <- den[g] + sum(cg$per_weight_pc[m][k])
    }
  }
  (num / den)[den > 0]
}
p_from_alpha_j <- function(a) { u <- exp(cbind(0, a)); u / rowSums(u) }
pj <- list(c1 = p_from_alpha_j(res_j$alpha_by_type$c1), c2plus = p_from_alpha_j(res_j$alpha_by_type$c2plus))
p0s <- list(c1 = alpha_data_by_type$c1$p0[in_state_c1, ], c2plus = alpha_data_by_type$c2plus$p0[in_state_c2, ])
Hj <- hours_by_type(pj$c1, 1, alpha_data_by_type$c1$child_weights$child_weight.1[in_state_c1], NULL, target_types) +
      hours_by_type(pj$c2plus, 2, alpha_data_by_type$c2plus$child_weights$child_weight.1[in_state_c2],
                    alpha_data_by_type$c2plus$child_weights$child_weight.2[in_state_c2], target_types)
e_forest <- emp_by_group(p0s, cg_toy)
e_joint  <- emp_by_group(pj, cg_toy)
stopifnot(max(abs(Hj / targets - 1)) < 1e-6,
          max(abs(e_joint - e_forest)) < 1e-6,
          setequal(names(res_j$employment_shifts), names(e_forest)),
          max(abs(res_j$employment_shifts)) > 1e-3)
cat('PASS: joint re-anchoring -- independent alphas hit care-hour targets and forest employment rates by group\n')

# Care-only solve moves employment off forest rate (the problem joint fixes)
p_care <- list(c1 = p_from_alpha_j(result$alpha_by_type$c1), c2plus = p_from_alpha_j(result$alpha_by_type$c2plus))
stopifnot(max(abs(emp_by_group(p_care, cg_toy) - e_forest)) > 1e-4, is.null(result$employment_shifts))
cat('PASS: care-only re-anchoring moves employment off the forest rate; joint solve does not\n')

# Missing caregiver row fails loudly; group helper rule
expect_error(adjust_alpha_for_state(alpha_data_by_type, state_hh_ids, targets_csv, 'ZZ',
                                    caregivers = cg_toy[-1, ]),
             'w/out caregiver group', 'missing caregiver row fails loudly')
cgh <- caregiver_groups_for_state(tibble(
  hh_id = c(1, 1, 2), parent_unit_id = 1, primary_caregiver = c(1, 1, 2),
  male1 = c(0, 0, 0), male2 = c(NA, NA, 1), age1 = c(30, 30, 50), age2 = c(NA, NA, 24),
  per_weight1 = c(10, 10, 5), per_weight2 = c(NA, NA, 7)))
stopifnot(identical(cgh$pc_group, c('F_25_35', 'M_u25')), identical(cgh$per_weight_pc, c(10, 7)))
cat('PASS: caregiver_groups_for_state -- sex x age bin of primary caregiver, caregiver weight, one row per unit\n')

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


#------------------------------
# NYC ranked twice: sub-state rank inputs, price wedge, alpha loader
#------------------------------

source('src/shared_functions/income_rank.R')
source('src/shared_functions/urban_density.R')
source('src/shared_functions/compute_price_wedge_cache.R')
source('src/shared_functions/compute_substate_alpha.R')
random_seed_base <- 76
urban_density_setting <- '0'   # default run setting; tests below flip it, restore

# NY (36): 150 NYC households (Manhattan 061), richer, 150 rest of NY (Albany
# 001); NJ (34): 300. One parent unit, one child under 5 each
n_nyc <- 150
tw_hh <- tibble(hh_id     = 1:600,
                statefip  = rep(c(36L, 36L, 34L), c(n_nyc, 150, 300)),
                countyfip = rep(c(61L, 1L, 3L), c(n_nyc, 150, 300)),
                region    = 1L,
                income    = c(rexp(n_nyc, 1 / 120000), rexp(450, 1 / 60000)))
is_nyc <- tw_hh$countyfip == 61L & tw_hh$statefip == 36L
tw_pu <- tibble(hh_id = tw_hh$hh_id, parent_unit_id = 1L,
                n_parents = rep_len(c(1L, 2L), 600))
tw_ch <- tibble(hh_id = tw_hh$hh_id, parent_unit_id = 1L, child_id = 1L,
                age = rep_len(c(0, 1, 2, 3, 4, 2), 600),
                child_weight = runif(600, 50, 150))

# -- removed schemes stop
for (s in c('state', 'region')) {
  income_rank_scheme <- s
  expect_error(get_income_rank_scheme(), 'unknown', paste0('scheme ', s, ' (removed) stops'))
}

# -- national_rpp: NYC metro RPP, national cutpoints (NYC at NYS RPP)
income_rank_scheme <- 'national_rpp'
nyc_rpp <- get_substate_rpp('NYC')
st_rpp  <- get_state_rpp()
stopifnot(abs(nyc_rpp - 114.899) < 1e-9, abs(st_rpp[['36']] - 109.426) < 1e-9)
invisible(capture.output(si <- substate_rank_inputs(tw_hh$income, tw_hh$region, tw_hh$statefip, is_nyc, 'NYC')))
base_ri <- income_rank_inputs(tw_hh$income, tw_hh$region, tw_hh$statefip, 'acs')
stopifnot(identical(si$cut_income, base_ri$income), all(si$group == 0L),
          isTRUE(all.equal(si$eval_income[is_nyc], tw_hh$income[is_nyc] / 1.14899)),
          identical(si$eval_income[!is_nyc], base_ri$income[!is_nyc]),
          isTRUE(all.equal(base_ri$income[is_nyc], tw_hh$income[is_nyc] / 1.09426)))
cat('PASS: national_rpp -- NYC income / NYC RPP 114.899, cutpoint inputs keep NYS RPP\n')

# -- national: versions identical, NYC version skipped
income_rank_scheme <- 'national'
invisible(capture.output(r <- substate_rank_inputs(tw_hh$income, tw_hh$region, tw_hh$statefip, is_nyc, 'NYC')))
stopifnot(is.null(r))
invisible(capture.output(r <- load_substate_alpha('NYC', tempdir(), 2019)))
stopifnot(is.null(r))
nat_ri <- income_rank_inputs(tw_hh$income, tw_hh$region, tw_hh$statefip, 'acs')
stopifnot(identical(nat_ri$income, tw_hh$income), all(nat_ri$group == 0L))
cat('PASS: national -- NYC ranks same in both versions, own version skipped\n')

expect_error(substate_rank_inputs(1:3, 1:3, 1:3, c(TRUE, NA, FALSE), 'NYC'),
             'one non-NA flag', 'NA geo flag stops')

# -- Keyed hash helpers (income-rank jitter; price wedge draw uses main's
# set.seed + sample)
# Reference values from Python big-int fmix32 (exact 32-bit arithmetic)
stopifnot(fmix_2_32(0xdeadbeef) == 233162409, fmix_2_32(4294967295) == 2180083513)
cat('PASS: fmix_2_32 matches exact 32-bit reference hash\n')

# ACS parent unit ids are text ('1', '1_2', 1b) -> numeric key hhm1 x 1000 +
# hhm2; jitter on text ids = jitter on those keys; bad id -> stop
stopifnot(identical(parent_unit_key(c('1', '1_2', '12_3')), c(1000, 1002, 12003)),
          identical(parent_unit_key(c(5, 7)), c(5, 7)),
          all(is.na(parent_unit_key(c('a', '1_2_3', '1000')))),
          identical(keyed_unif_2_32(c(2395, 2395, 2397), parent_unit_key(c('1_2', '3', '1_2')), 79),
                    keyed_unif_2_32(c(2395, 2395, 2397), c(1002, 3000, 1002), 79)))
stopifnot(inherits(tryCatch(keyed_unif_2_32(1, parent_unit_key('x'), 79), error = function(e) e), 'error'))
cat('PASS: parent_unit_key -- ACS text parent unit ids hash as numeric keys\n')

# Round trip on 1b's id format ('<hhm_id1>' one parent, '<hhm_id1>_<hhm_id2>'
# two; 1b sorts hhm_id1 < hhm_id2), person numbers 1-120 -> keys non-NA, one
# per unit, jitter runs
rt <- rbind(data.frame(h1 = 1:120, h2 = NA_integer_),
            subset(expand.grid(h1 = 1:120, h2 = 1:120), h1 < h2))
rt_ids  <- ifelse(is.na(rt$h2), as.character(rt$h1), paste0(rt$h1, '_', rt$h2))
rt_keys <- parent_unit_key(rt_ids)
stopifnot(is.character(rt_ids), !anyNA(rt_keys), !anyDuplicated(rt_keys),
          identical(rt_keys, rt$h1 * 1000 + ifelse(is.na(rt$h2), 0, rt$h2)),
          all(keyed_unif_2_32(rep(1, nrow(rt)), rt_keys, 79) >= 0))
cat('PASS: 1b parent unit ids round-trip through parent_unit_key, ',
    nrow(rt), ' units\n', sep = '')

k_hh <- rep(1:60000, each = 2); k_pu <- rep(1:2, 60000)
k_u <- keyed_unif_2_32(k_hh, k_pu, random_seed_base + 4)
k_perm <- sample(length(k_hh)); k_sub <- sort(sample(length(k_hh), 5000))
stopifnot(all(k_u >= 0 & k_u < 1), chisq.test(tabulate(1 + floor(k_u * 99), 99))$p.value > 1e-3,
          identical(keyed_unif_2_32(k_hh[k_perm], k_pu[k_perm], random_seed_base + 4), k_u[k_perm]),
          identical(keyed_unif_2_32(k_hh[k_sub], k_pu[k_sub], random_seed_base + 4), k_u[k_sub]),
          mean(keyed_unif_2_32(k_hh, k_pu, random_seed_base + 5) == k_u) < 0.001)
cat('PASS: keyed uniform in [0, 1), uniform, same value under row shuffle / subset, seed moves it\n')

# -- Price wedge cache: wedge = price forest quantile at keyed index, clipped
# [0.1, 5]. Toy forests (ranger quantreg, 1a train_single_qrf() settings bar
# size) on income percentile, region, n_parents, child age
make_toy_qrf <- function(target_fn, seed, n = 1500) {
  with_rng_preserved({
    set.seed(seed)
    d <- data.frame(income_pctile = sample(1:100, n, replace = TRUE),
                    region        = factor(sample(c('1', '2'), n, replace = TRUE), levels = c('1', '2')),
                    n_parents     = sample(1:2, n, replace = TRUE),
                    child_age     = sample(0:4, n, replace = TRUE))
    d$target <- target_fn(d)
    m <- ranger::ranger(target ~ income_pctile + region + n_parents + child_age, data = d,
                        num.trees = 30, mtry = 4, min.node.size = 10, quantreg = TRUE, seed = seed)
    m$region_levels <- levels(d$region)
    m
  })
}
tw_models <- list(
  center_low_price   = make_toy_qrf(function(d) 1 + d$income_pctile / 50 + rnorm(nrow(d), 0, 0.3), 1),
  center_high_price  = make_toy_qrf(function(d) 2 + 0.1 * d$n_parents + rnorm(nrow(d), 0, 0.5), 2),
  home_price         = make_toy_qrf(function(d) 0.8 + 0.05 * d$child_age + rnorm(nrow(d), 0, 0.2), 3),
  other_paid_price   = make_toy_qrf(function(d) 1.2 + rnorm(nrow(d), 0, 0.4), 4),
  income_rank_scheme = 'national', urban_density = '0',
  summary_stats      = list(other_paid_base_price = 10))
# Parent unit percentiles; NA = AGI unknown (-> no wedge)
tw_ip <- tibble(hh_id = tw_hh$hh_id, parent_unit_id = 1L,
                income_pctile = rep_len(c(1L, 20L, 21L, 40L, 55L, 80L, 81L, 100L, NA), 600))
tw_cache <- function(pu = tw_pu, ch = tw_ch, hh = tw_hh, ip = tw_ip, pm = tw_models) {
  compute_price_wedge_cache(pu, ch, hh, pm, ip)
}
wedge_cols <- c('price_wedge.center_low', 'price_wedge.center_high', 'price_wedge.home', 'price_wedge.other_paid')
wedge_models <- c(price_wedge.center_low = 'center_low_price', price_wedge.center_high = 'center_high_price',
                  price_wedge.home = 'home_price', price_wedge.other_paid = 'other_paid_price')

base_w <- tw_cache()
# Main's draw: set.seed(random_seed_base + 2), then sample(1:99, n_complete,
# replace = TRUE) over complete units in table order, one index per unit
# shared by the four care types
base_cmp <- !is.na(tw_ip$income_pctile)
set.seed(random_seed_base + 2)
base_idx <- rep(NA_integer_, nrow(tw_pu))
base_idx[base_cmp] <- sample(1:99, sum(base_cmp), replace = TRUE)
stopifnot(identical(names(base_w), c('hh_id', 'parent_unit_id', wedge_cols)),
          identical(base_w$hh_id, tw_pu$hh_id),
          identical(is.na(base_w$price_wedge.center_low), is.na(tw_ip$income_pctile)),
          all(as.matrix(base_w[!is.na(tw_ip$income_pctile), wedge_cols]) >= 0.1 &
                as.matrix(base_w[!is.na(tw_ip$income_pctile), wedge_cols]) <= 5))

# Reference: forest predicts quantile idx / 100 at each unit's own features,
# one unit at a time (what vectorised cache must equal)
ref_i <- which(!is.na(tw_ip$income_pctile))[1:150]
for (cl in wedge_cols) {
  ref <- vapply(ref_i, function(i) {
    f <- data.frame(income_pctile = tw_ip$income_pctile[i],
                    region = factor(tw_hh$region[i], levels = c('1', '2')),
                    n_parents = tw_pu$n_parents[i], child_age = tw_ch$age[i])
    q <- predict(tw_models[[wedge_models[[cl]]]], data = f, type = 'quantiles',
                 quantiles = base_idx[i] / 100)$predictions
    pmax(0.1, pmin(5, q[1, 1]))
  }, numeric(1))
  stopifnot(isTRUE(all.equal(base_w[[cl]][ref_i], ref)))
}
cat('PASS: price wedge cache -- wedge = price forest quantile (main sample(1:99) index / 100) at unit features, ',
    'equals per-unit forest prediction, clipped [0.1, 5]\n', sep = '')

# Wedge rises w/ drawn index among units w/ same features (all four types, one
# index shared); forest responds to income percentile (center_low)
fkey <- paste(tw_ip$income_pctile, tw_pu$n_parents, tw_ch$age)
mono <- vapply(split(which(!is.na(tw_ip$income_pctile)), fkey[!is.na(tw_ip$income_pctile)]), function(i) {
  i <- i[order(base_idx[i])]
  all(vapply(wedge_cols, function(cl) all(diff(base_w[[cl]][i]) >= -1e-12), logical(1)))
}, logical(1))
stopifnot(all(mono), any(table(fkey) > 3))
lo <- tw_ip$income_pctile %in% c(1L, 20L); hi <- tw_ip$income_pctile %in% c(81L, 100L)
stopifnot(mean(base_w$price_wedge.center_low[hi]) > mean(base_w$price_wedge.center_low[lo]) + 0.5)
cat('PASS: price wedge cache -- non-decreasing in drawn index for same features, center_low rises w/ income\n')

# Clipping: forest w/ wedges beyond [0.1, 5] -> bounds
clip_pm <- tw_models
clip_pm$center_low_price <- make_toy_qrf(function(d) ifelse(runif(nrow(d)) < 0.5, 0.001, 50), 5)
clip_w <- tw_cache(pm = clip_pm)
stopifnot(all(clip_w$price_wedge.center_low >= 0.1 & clip_w$price_wedge.center_low <= 5, na.rm = TRUE),
          any(clip_w$price_wedge.center_low == 0.1, na.rm = TRUE),
          any(clip_w$price_wedge.center_low == 5, na.rm = TRUE),
          identical(clip_w$price_wedge.center_high, base_w$price_wedge.center_high))
cat('PASS: price wedge cache clips multipliers to [0.1, 5]\n')

# Main's behavior: draw depends on global RNG only through set.seed inside the
# call (same inputs -> same wedge whatever the global state before), and on
# unit order / set (shuffled rows or a state subset draw a different stream)
set.seed(1); r_a <- tw_cache()
set.seed(2); r_b <- tw_cache()
stopifnot(identical(r_a, base_w), identical(r_b, base_w))
shuf <- sample(nrow(tw_pu))
r_shuf <- tw_cache(tw_pu[shuf, ], tw_ch[sample(nrow(tw_ch)), ], ip = tw_ip[sample(nrow(tw_ip)), ])
ny_ids <- tw_hh$hh_id[tw_hh$statefip == 36L]
r_ny <- tw_cache(tw_pu %>% filter(hh_id %in% ny_ids), tw_ch %>% filter(hh_id %in% ny_ids),
                 tw_hh %>% filter(hh_id %in% ny_ids))
stopifnot(!identical(r_shuf[match(base_w$hh_id, r_shuf$hh_id), ], base_w),
          identical(is.na(r_ny$price_wedge.home), is.na(base_w$price_wedge.home[base_w$hh_id %in% ny_ids])))
cat('PASS: price wedge cache -- main draw: seeded inside call, depends on unit order / set\n')

# Units w/out child under 5 get NA; children 5+ / extra rows change nothing
tw_ch8 <- bind_rows(tw_ch %>% filter(hh_id > 10),
                    tibble(hh_id = 1:20, parent_unit_id = 1L, child_id = 2L, age = 8, child_weight = 1e6))
r8 <- tw_cache(ch = tw_ch8)
stopifnot(all(is.na(as.matrix(r8[r8$hh_id <= 10, wedge_cols]))),
          identical(is.na(r8$price_wedge.home[r8$hh_id > 10]), is.na(base_w$price_wedge.home[base_w$hh_id > 10])))
# Employment-choice expansion (3 rows per unit) -> one row per unit
r3 <- tw_cache(bind_rows(tw_pu, tw_pu, tw_pu))
stopifnot(identical(r3, base_w))
# Two children under 5: forest takes mean age
tw_ch2 <- bind_rows(tw_ch, tw_ch %>% filter(hh_id <= 50) %>% mutate(child_id = 2L, age = 4))
r2c <- tw_cache(ch = tw_ch2)
stopifnot(identical(r2c[r2c$hh_id > 50, ], base_w[base_w$hh_id > 50, ]),
          !identical(r2c[r2c$hh_id <= 50, ], base_w[base_w$hh_id <= 50, ]))
cat('PASS: price wedge cache -- NA w/out child under 5, one row per unit, mean child age\n')

# Percentile: NA (AGI unknown) or unit absent from income_pctiles -> no wedge
# (callers fill 1.0); region NA -> no wedge
r_miss <- tw_cache(ip = tw_ip %>% filter(hh_id > 100))
stopifnot(all(is.na(as.matrix(r_miss[r_miss$hh_id <= 100, wedge_cols]))),
          identical(is.na(r_miss$price_wedge.home[-(1:100)]), is.na(base_w$price_wedge.home[-(1:100)])))
r_reg <- tw_cache(hh = tw_hh %>% mutate(region = if_else(hh_id <= 5, NA_integer_, region)))
stopifnot(all(is.na(as.matrix(r_reg[r_reg$hh_id <= 5, wedge_cols]))),
          identical(is.na(r_reg$price_wedge.home[-(1:5)]), is.na(base_w$price_wedge.home[-(1:5)])))
cat('PASS: price wedge cache -- unit w/ NA percentile, absent from income_pctiles, or w/out region gets no wedge\n')

set.seed(5); invisible(tw_cache()); r_after <- runif(3)
set.seed(random_seed_base + 2); invisible(sample(1:99, sum(base_cmp), replace = TRUE))
stopifnot(!identical(r_after, { set.seed(5); runif(3) }))   # call moves global stream (as main)
cat('PASS: price wedge cache moves global RNG stream (set.seed inside call, as main)\n')

# Scale: 300,000 units predict one forest row per distinct feature row, not per
# unit -- finishes in seconds; timing printed, not asserted
big_n <- 300000
big_pu <- tibble(hh_id = 1:big_n, parent_unit_id = 1L, n_parents = sample(1:2, big_n, replace = TRUE))
big_ch <- tibble(hh_id = 1:big_n, parent_unit_id = 1L, age = sample(0:4, big_n, replace = TRUE))
big_hh <- tibble(hh_id = 1:big_n, region = sample(1:2, big_n, replace = TRUE))
big_ip <- tibble(hh_id = 1:big_n, parent_unit_id = 1L, income_pctile = sample(1:100, big_n, replace = TRUE))
big_t <- system.time(big_w <- compute_price_wedge_cache(big_pu, big_ch, big_hh, tw_models, big_ip))
stopifnot(nrow(big_w) == big_n, !anyNA(as.matrix(big_w[, wedge_cols])))
cat('PASS: price wedge cache -- ', format(big_n, scientific = FALSE), ' units in ', round(big_t[['elapsed']], 1), ' s\n', sep = '')

expect_error(tw_cache(pm = tw_models[c('center_low_price', 'center_high_price', 'home_price')]),
             'Rerun NSECE processing', 'cache w/out all four forests stops')
expect_error(tw_cache(pm = list(quantile_grid = list())),
             'Rerun NSECE processing', 'old grid cache object stops')
expect_error(tw_cache(hh = tw_hh %>% select(-region)), 'households needs hh_id, region',
             'households w/out region stops')

# -- Scheme mismatch, unlabeled cache stop
mis_pm <- tw_models; mis_pm$income_rank_scheme <- 'national_rpp'
expect_error(tw_cache(pm = mis_pm),
             'built under income-rank scheme "national_rpp", this run uses "national"',
             'price forest from other income-rank scheme stops')
unl_pm <- tw_models; unl_pm$income_rank_scheme <- NULL
expect_error(tw_cache(pm = unl_pm), 'scheme "unlabeled"', 'unlabeled price forest stops')
expect_error(compute_price_wedge_cache_state(tw_pu, tw_ch, tw_hh, tw_models, NULL),
             'income_pctiles needs hh_id', 'missing income_pctiles stops')

# -- Urban-density mismatch stops (price forest built under other setting);
# unlabeled forest = built before switch = density on
den_pm <- tw_models; den_pm$urban_density <- '1'
expect_error(tw_cache(pm = den_pm), 'built under ECEC_URBAN_DENSITY=1, this run uses 0',
             'price forest built w/ density on stops under density off')
unl_den <- tw_models; unl_den$urban_density <- NULL
expect_error(tw_cache(pm = unl_den), 'unlabeled: built before the switch, density on',
             'unlabeled price forest stops under density off')
urban_density_setting <- '1'
stopifnot(identical(tw_cache(pm = den_pm), base_w), identical(tw_cache(pm = unl_den), base_w))
expect_error(tw_cache(), 'built under ECEC_URBAN_DENSITY=0, this run uses 1',
             'price forest built w/ density off stops under density on')
urban_density_setting <- '0'
cat('PASS: price wedge cache -- price forest from other urban-density setting stops, unlabeled = density on\n')

# -- ACS parent unit AGI percentiles for price forest (price_wedge_income_pctiles()),
# as original simulator: unit AGI = baseline_agi1 + baseline_agi2, weight =
# mean parent weights. national scheme: cutpoints = weighted quantiles of AGI
# + keyed jitter over every unit w/ AGI; AGI <= 0 -> bin 1; AGI NA -> NA
tw_pua <- tw_pu %>%
  mutate(baseline_agi1 = tw_hh$income * 0.6,
         baseline_agi2 = if_else(n_parents == 2L, tw_hh$income * 0.4, NA_real_),
         per_weight1   = runif(600, 50, 150),
         per_weight2   = if_else(n_parents == 2L, runif(600, 50, 150), NA_real_))
tw_pua0 <- tw_pua
tw_pua0$baseline_agi1[c(5, 6)] <- c(0, -100); tw_pua0$baseline_agi2[c(5, 6)] <- NA
tw_pua0$baseline_agi1[7] <- NA; tw_pua0$baseline_agi2[7] <- NA
ip_nat <- price_wedge_income_pctiles(tw_pua0, tw_hh)
agi0 <- tw_pua0$baseline_agi1 + coalesce(tw_pua0$baseline_agi2, 0)
agi0[7] <- NA
wt0 <- (tw_pua0$per_weight1 + coalesce(tw_pua0$per_weight2, tw_pua0$per_weight1)) / 2
ok_k <- !is.na(agi0)
jit <- keyed_unif_2_32(tw_pua0$hh_id, parent_unit_key(tw_pua0$parent_unit_id), random_seed_base + 4)
man_cut <- Hmisc::wtd.quantile(agi0[ok_k] + jit[ok_k], wt0[ok_k], seq(0.01, 0.99, 0.01))
man_p <- assign_income_pctile(agi0, rep(0L, 600), list(`0` = man_cut))
stopifnot(identical(names(ip_nat), c('hh_id', 'parent_unit_id', 'income_pctile')),
          identical(ip_nat$hh_id, tw_pua0$hh_id), identical(ip_nat$income_pctile, man_p),
          all(ip_nat$income_pctile[5:6] == 1L), is.na(ip_nat$income_pctile[7]), all(jit >= 0 & jit < 1))
# Weighted quintile shares ~ 20% each
qs <- tapply(wt0[ok_k], ceiling(man_p[ok_k] / 20), sum) / sum(wt0[ok_k])
stopifnot(max(abs(qs - 0.2)) < 0.03)
# Row order irrelevant; employment-choice rows collapse to first row per unit
ip_shuf <- price_wedge_income_pctiles(tw_pua0[sample(600), ], tw_hh[sample(600), ])
stopifnot(identical(ip_shuf[match(ip_nat$hh_id, ip_shuf$hh_id), ], ip_nat))
ip_dup <- price_wedge_income_pctiles(bind_rows(tw_pua0, tw_pua0 %>% mutate(baseline_agi1 = baseline_agi1 + 1e6)), tw_hh)
stopifnot(identical(ip_dup, ip_nat))
# Second parent adds to AGI: unit w/ two parents ranks on the sum
two_k <- !is.na(tw_pua0$baseline_agi2) & ok_k
stopifnot(any(two_k), all(agi0[two_k] > tw_pua0$baseline_agi1[two_k]))
expect_error(price_wedge_income_pctiles(select(tw_pua0, -per_weight1), tw_hh), 'parent_units needs columns',
             'parent_units w/out weights stops')
cat('PASS: price_wedge_income_pctiles -- unit baseline AGI (both parents), weighted national cutpoints w/ keyed jitter, ',
    'AGI <= 0 bin 1, NA AGI NA, row order irrelevant\n', sep = '')
set.seed(5); r1 <- runif(3)
set.seed(5); invisible(price_wedge_income_pctiles(tw_pua0, tw_hh)); r2 <- runif(3)
stopifnot(identical(r1, r2))
cat('PASS: price_wedge_income_pctiles leaves global RNG stream untouched\n')

# Geo version: NULL under national; national_rpp -- AGI / state RPP for
# cutpoints, NYC units at NYC RPP vs same cutpoints, rest identical to
# national version
invisible(capture.output(ip_g <- price_wedge_income_pctiles(tw_pua, tw_hh, 'NYC')))
stopifnot(is.null(ip_g))
income_rank_scheme <- 'national_rpp'
agi <- tw_pua$baseline_agi1 + coalesce(tw_pua$baseline_agi2, 0)
wt <- (tw_pua$per_weight1 + coalesce(tw_pua$per_weight2, tw_pua$per_weight1)) / 2
jit <- keyed_unif_2_32(tw_pua$hh_id, parent_unit_key(tw_pua$parent_unit_id), random_seed_base + 4)
ip_rpp <- price_wedge_income_pctiles(tw_pua, tw_hh)
invisible(capture.output(ip_nyc <- price_wedge_income_pctiles(tw_pua, tw_hh, 'NYC')))
ri_rpp <- income_rank_inputs(agi, tw_hh$region, tw_hh$statefip, 'acs')
rpp_cut <- Hmisc::wtd.quantile(ri_rpp$income + jit, wt, seq(0.01, 0.99, 0.01))
stopifnot(identical(ip_rpp$income_pctile,
                    assign_income_pctile(ri_rpp$income, rep(0L, 600), list(`0` = rpp_cut))),
          identical(ip_nyc$income_pctile[!is_nyc], ip_rpp$income_pctile[!is_nyc]),
          identical(ip_nyc$income_pctile[is_nyc],
                    assign_income_pctile(agi[is_nyc] / 1.14899, rep(0L, sum(is_nyc)),
                                         list(`0` = rpp_cut))),
          all(ip_nyc$income_pctile[is_nyc] <= ip_rpp$income_pctile[is_nyc]),
          any(ip_nyc$income_pctile[is_nyc] < ip_rpp$income_pctile[is_nyc]))
cat('PASS: price_wedge_income_pctiles -- national_rpp AGI / state RPP; NYC own version at NYC RPP vs national cutpoints, ',
    'rest unchanged; NULL under national\n', sep = '')
income_rank_scheme <- 'national'

# -- RNG restored around sub-state extras
set.seed(5); r1 <- runif(3)
set.seed(5); invisible(with_rng_preserved({ set.seed(99); runif(10) })); r2 <- runif(3)
stopifnot(identical(r1, r2))
cat('PASS: with_rng_preserved restores global RNG stream\n')

# -- 3a loader: missing rds -> NULL, scheme mismatch stops, match loads,
# unlabeled / removed-scheme rds stops w/ recalibrate message
income_rank_scheme <- 'national_rpp'
est_dir <- file.path(tempdir(), 'substate_alpha_test')
dir.create(est_dir, showWarnings = FALSE)
invisible(capture.output(r <- load_substate_alpha('NYC', est_dir, 2019)))
stopifnot(is.null(r))
invisible(capture.output(r <- load_substate_alpha('NY', est_dir, 2019)))
stopifnot(is.null(r))
write_alpha_rds <- function(scheme, density = '0') {
  for (t in PARENT_UNIT_NAMES) {
    a <- list(alpha = matrix(0, 1, 1), geo = 'NYC')
    a$income_rank_scheme <- scheme
    a$urban_density <- density
    saveRDS(a, substate_alpha_path(est_dir, t, 2019, 'NYC'))
  }
}
write_alpha_rds('national_rpp')
invisible(capture.output(r <- load_substate_alpha('NYC', est_dir, 2019)))
stopifnot(identical(names(r), PARENT_UNIT_NAMES), r$c1$geo == 'NYC')
cat('PASS: load_substate_alpha -- NULL w/out rds or for whole states, loads matching scheme\n')
write_alpha_rds('national')
expect_error(load_substate_alpha('NYC', est_dir, 2019),
             'built under income-rank scheme "national", this run uses "national_rpp"',
             'sub-state alpha from other scheme stops')
for (s in list(NULL, 'state', 'region')) {
  write_alpha_rds(s)
  expect_error(load_substate_alpha('NYC', est_dir, 2019), 'longer be used. Recalibrate',
               paste0('sub-state alpha w/ scheme ', if (is.null(s)) 'unlabeled' else s,
                      ' stops, asks to recalibrate'))
}

# Urban-density setting of sub-state alpha vs run: mismatch stops, unlabeled
# = built before switch = density on
write_alpha_rds('national_rpp', density = '1')
expect_error(load_substate_alpha('NYC', est_dir, 2019),
             'built under ECEC_URBAN_DENSITY=1, this run uses 0',
             'sub-state alpha built w/ density on stops under density off')
write_alpha_rds('national_rpp', density = NULL)
expect_error(load_substate_alpha('NYC', est_dir, 2019),
             'unlabeled: built before the switch, density on',
             'unlabeled sub-state alpha stops under density off')
urban_density_setting <- '1'
invisible(capture.output(r <- load_substate_alpha('NYC', est_dir, 2019)))
stopifnot(identical(names(r), PARENT_UNIT_NAMES))
write_alpha_rds('national_rpp', density = '0')
expect_error(load_substate_alpha('NYC', est_dir, 2019),
             'built under ECEC_URBAN_DENSITY=0, this run uses 1',
             'sub-state alpha built w/ density off stops under density on')
urban_density_setting <- '0'
cat('PASS: load_substate_alpha -- urban-density setting mismatch stops, unlabeled = density on\n')
unlink(est_dir, recursive = TRUE)

# -- 2a helper: NYC alpha w/ national wedge rows, national beta / rho, NYC rows only
source('src/shared_functions/compute_probs_from_alpha.R')
income_rank_scheme <- 'national_rpp'
est_dir <- file.path(tempdir(), 'substate_alpha_calib')
dir.create(est_dir, showWarnings = FALSE)
writeLines('hh_id,child_id', file.path(est_dir, 'acs_enrollment_2019_NYC.csv'))
tw_puw <- tibble(hh_id = tw_hh$hh_id, parent_unit_id = 1L, pseudofamily_id = 1L,
                 child_category = '1', child_weight.1 = runif(600, 50, 150)) %>%
  left_join(base_w, by = c('hh_id', 'parent_unit_id')) %>%
  mutate(across(all_of(wedge_cols), ~ coalesce(.x, 1.0)))   # callers fill 1.0 (2a)
# Stand-in for 2a build_calibration_type_data(): cost = wedge x 100 x choice
# index, p0 uniform -- alpha then reads back wedge used
mock_build <- function(pu_type, n_children, child_cat, parent_units_wide, enrollment,
                       enrollment_joint, supply_prices, other_paid_base_price,
                       wage_perturbation, tax_max) {
  s <- filter(parent_units_wide, child_category == child_cat)
  if (nrow(s) == 0) return(tibble())
  K <- nrow(get_choice_catalog(n_children))
  mk <- function(m, stub) setNames(as_tibble(m, .name_repair = 'minimal'), paste0(stub, 1:K))
  bind_cols(s,
            mk(matrix(50000, nrow(s), K), 'Y.'),
            mk(outer(s$price_wedge.center_low, 1:K) * 100, 'C.'),
            mk(matrix(1 / K, nrow(s), K), 'p.'))
}
mock_V <- function(x, beta, rho) beta * log(pmax(x, 1))
saveRDS(list(row_ids = as.data.frame(tw_puw[, c('hh_id', 'parent_unit_id', 'pseudofamily_id')]),
             child_weights = as.data.frame(tw_puw[, 'child_weight.1']), beta = 2.5, rho = 1),
        file.path(est_dir, 'alpha_c1_2019.rds'))
run_helper <- function() {
  capture.output(r <- compute_substate_alphas(
    parent_units_wide = tw_puw, acs_tables = list(households = tw_hh, parent_units = tw_pua, children = tw_ch),
    build_type_data = mock_build, compute_V = mock_V,
    supply_prices = c(0, 1, 1, 1), other_paid_base_price = 1, wage_perturbation = 1.01,
    tax_max = 1, data_dir = est_dir, estimation_dir = est_dir, year = 2019))
  r
}
stopifnot(identical(run_helper(), 'NYC'))
ad <- readRDS(substate_alpha_path(est_dir, 'c1', 2019, 'NYC'))
geo_w <- tw_puw[is_nyc, ]
stopifnot(identical(ad$row_ids$hh_id, tw_hh$hh_id[is_nyc]), ad$beta == 2.5,
          ad$income_rank_scheme == 'national_rpp', ad$geo == 'NYC',
          isTRUE(all.equal(ad$child_weights$child_weight.1, tw_puw$child_weight.1[is_nyc])))
V_exp <- 2.5 * log(50000 - outer(geo_w$price_wedge.center_low, 1:45) * 100)
stopifnot(max(abs(ad$alpha - (-(V_exp - V_exp[, 1])[, -1]))) < 1e-10,
          max(abs(compute_probs_from_alpha(ad$alpha, V_exp) - 1 / 45)) < 1e-12)
cat('PASS: compute_substate_alphas -- NYC rows, national wedge rows in V, national beta, alpha recovers p0\n')

# w/ price forests: NYC wedge rows rebuilt on NYC's own income ranks
rpp_pm <- tw_models; rpp_pm$income_rank_scheme <- 'national_rpp'
invisible(capture.output({
  compute_substate_alphas(
    parent_units_wide = tw_puw, acs_tables = list(households = tw_hh, parent_units = tw_pua, children = tw_ch),
    build_type_data = mock_build, compute_V = mock_V,
    supply_prices = c(0, 1, 1, 1), other_paid_base_price = 1, wage_perturbation = 1.01,
    tax_max = 1, data_dir = est_dir, estimation_dir = est_dir, year = 2019,
    price_qrf_models = rpp_pm)
  ip_nyc <- price_wedge_income_pctiles(tw_pua, tw_hh, 'NYC')
}))
ad_q <- readRDS(substate_alpha_path(est_dir, 'c1', 2019, 'NYC'))
nyc_w <- compute_price_wedge_cache(tw_pua[is_nyc, ], tw_ch[is_nyc, ], tw_hh[is_nyc, ], rpp_pm, ip_nyc)
cl_nyc <- coalesce(nyc_w$price_wedge.center_low, 1)
V_q <- 2.5 * log(50000 - outer(cl_nyc, 1:45) * 100)
stopifnot(max(abs(ad_q$alpha - (-(V_q - V_q[, 1])[, -1]))) < 1e-10,
          identical(ad_q$row_ids$hh_id, tw_hh$hh_id[is_nyc]),
          identical(nyc_w$hh_id, tw_hh$hh_id[is_nyc]),
          any(abs(cl_nyc - geo_w$price_wedge.center_low) > 1e-9),
          ad_q$urban_density == '0')
cat('PASS: compute_substate_alphas -- w/ price forests, NYC wedge rows from NYC own income percentiles, density setting recorded\n')

# Misaligned national rds stops; scheme w/ identical versions writes nothing
nat_bad <- readRDS(file.path(est_dir, 'alpha_c1_2019.rds'))
nat_bad$child_weights$child_weight.1 <- rev(nat_bad$child_weights$child_weight.1)
saveRDS(nat_bad, file.path(est_dir, 'alpha_c1_2019.rds'))
expect_error(run_helper(), 'do not match', 'sub-state rows misaligned w/ national rds stops')
unlink(substate_alpha_path(est_dir, 'c1', 2019, 'NYC'))
income_rank_scheme <- 'national'
stopifnot(length(run_helper()) == 0,
          !file.exists(substate_alpha_path(est_dir, 'c1', 2019, 'NYC')))
cat('PASS: compute_substate_alphas -- nothing written under national scheme\n')
unlink(est_dir, recursive = TRUE)
rm(income_rank_scheme, urban_density_setting)


#------------------------------
# Density-adjusted tier line (compute_tier_thresholds(), urban_density.R)
#------------------------------

source('src/shared_functions/urban_density.R')

# Equal weights. Region 1 high-density 0-1 prices 4, 5, 6; region 2 high 7,
# 8, 9.5; national density 2 = {3}, density 3 = {2}. Expected medians from
# Hmisc wtd.quantile directly -- formula checked, not quantile rule
tp <- tibble(
  region  = c(1, 1, 1, 2, 2, 2, 1, 2),
  density = c(1, 1, 1, 1, 1, 1, 2, 3),
  age_bin = '0-1',
  price   = c(4, 5, 6, 7, 8, 9.5, 3, 2),
  center_weight = 1, enrollment_fte = 1)
# Zero / NA price rows ignored
tp <- bind_rows(tp, tibble(region = 1, density = 3, age_bin = '0-1', price = c(0, NA),
                           center_weight = 1, enrollment_fte = 1))
wm <- function(x) unname(Hmisc::wtd.quantile(x, weights = rep(1, length(x)), probs = 0.5))
th <- compute_tier_thresholds(tp)
m1 <- wm(c(4, 5, 6, 7, 8, 9.5))
stopifnot(nrow(th) == 6,
          isTRUE(all.equal(th$threshold[th$region == 1 & th$density == 1], wm(c(4, 5, 6)))),
          isTRUE(all.equal(th$threshold[th$region == 2 & th$density == 1], wm(c(7, 8, 9.5)))),
          isTRUE(all.equal(th$threshold[th$region == 1 & th$density == 2], wm(c(4, 5, 6)) * 3 / m1)),
          isTRUE(all.equal(th$threshold[th$region == 2 & th$density == 3], wm(c(7, 8, 9.5)) * 2 / m1)),
          th$n_centers[th$region == 1 & th$density == 3] == 0)
# Weights move medians: heavy enrollment on 6 -> region 1 high median 6
th_w <- compute_tier_thresholds(tp %>% mutate(enrollment_fte = if_else(price %in% 6, 10, 1)))
stopifnot(th_w$median_high[th_w$region == 1][1] == 6)
cat('PASS: compute_tier_thresholds -- regional high-density median x national density ratio, weighted\n')

# Urban density off = main: national median posted price by age bin only, no
# region, same weights, positive prices only (compute_tier_thresholds_age_bin()).
# Positive prices 3, 4, 5, 6 (region 1) and 2, 7, 8, 9.5 (region 2), one bin
tr <- compute_tier_thresholds_age_bin(tp)
stopifnot(nrow(tr) == 1, tr$age_bin == '0-1', tr$n_centers == 8L,
          isTRUE(all.equal(tr$threshold, wm(c(3, 4, 5, 6, 2, 7, 8, 9.5)))),
          identical(names(tr), c('age_bin', 'n_centers', 'threshold')))
tr_w <- compute_tier_thresholds_age_bin(tp %>% mutate(enrollment_fte = if_else(price %in% 9.5, 100, 1)))
stopifnot(tr_w$threshold == 9.5)
tp_a <- bind_rows(tp, tp %>% mutate(age_bin = '2-3', price = price * 2))
tr_a <- compute_tier_thresholds_age_bin(tp_a)
stopifnot(nrow(tr_a) == 2, isTRUE(all.equal(tr_a$threshold[tr_a$age_bin == '2-3'], 2 * tr$threshold)))
cat('PASS: compute_tier_thresholds_age_bin -- age bin weighted median, positive prices, no region\n')

# Urban-density switch: global urban_density_setting (config.R) else env var
# ECEC_URBAN_DENSITY else '0'; interface markers; built-under check
urban_density_setting <- '0'
stopifnot(identical(get_urban_density_setting(), '0'), !urban_density_enabled())
urban_density_setting <- ''
stopifnot(identical(get_urban_density_setting(), '0'))
urban_density_setting <- '1'
stopifnot(identical(get_urban_density_setting(), '1'), urban_density_enabled())
urban_density_setting <- '2'
expect_error(get_urban_density_setting(), 'unknown. Use 0 \\(default\\) or 1', 'density setting 2 stops')
rm(urban_density_setting)
old_env <- Sys.getenv('ECEC_URBAN_DENSITY', NA)
Sys.unsetenv('ECEC_URBAN_DENSITY')
stopifnot(identical(get_urban_density_setting(), '0'))
Sys.setenv(ECEC_URBAN_DENSITY = '1')
stopifnot(identical(get_urban_density_setting(), '1'))
if (is.na(old_env)) Sys.unsetenv('ECEC_URBAN_DENSITY') else Sys.setenv(ECEC_URBAN_DENSITY = old_env)
# Income-rank scheme default follows the switch (RPP adjustment = state
# machinery): off -> 'national' (nominal, = main), on -> 'national_rpp';
# explicit ECEC_INCOME_RANK wins. Global income_rank_scheme absent here
stopifnot(!exists('income_rank_scheme', envir = globalenv()))
old_rank_env <- Sys.getenv('ECEC_INCOME_RANK', NA)
Sys.unsetenv('ECEC_INCOME_RANK')
urban_density_setting <- '0'
stopifnot(identical(default_income_rank_scheme(), 'national'), identical(get_income_rank_scheme(), 'national'))
urban_density_setting <- '1'
stopifnot(identical(default_income_rank_scheme(), 'national_rpp'), identical(get_income_rank_scheme(), 'national_rpp'))
Sys.setenv(ECEC_INCOME_RANK = 'national')
stopifnot(identical(get_income_rank_scheme(), 'national'))
urban_density_setting <- '0'
Sys.setenv(ECEC_INCOME_RANK = 'national_rpp')
stopifnot(identical(get_income_rank_scheme(), 'national_rpp'))
if (is.na(old_rank_env)) Sys.unsetenv('ECEC_INCOME_RANK') else Sys.setenv(ECEC_INCOME_RANK = old_rank_env)
cat('PASS: income-rank scheme default follows urban-density switch (off nominal, on RPP), env var overrides\n')
urban_density_setting <- '0'
expect_error(require_urban_density_on('state script'), 'state script needs urban density on.*ECEC_URBAN_DENSITY=1',
             'state script stops w/ density off')
check_urban_density_built('0', 'piece', '1a')
expect_error(check_urban_density_built('1', 'piece', '1a'), 'piece built under ECEC_URBAN_DENSITY=1, this run uses 0.*rerun 1a',
             'piece built w/ density on stops under off')
expect_error(check_urban_density_built(NULL, 'piece', '1a'), 'unlabeled', 'unlabeled piece = density on, stops under off')
ud_dir <- file.path(tempdir(), 'ud_markers'); dir.create(ud_dir, showWarnings = FALSE)
invisible(write_urban_density_marker(ud_dir, '1a'))
stopifnot(identical(readLines(file.path(ud_dir, 'urban_density_1a.txt')), '0'))
check_interface_urban_density(ud_dir, '1a')
expect_error(check_interface_urban_density(ud_dir), 'stage 1b.*built under ECEC_URBAN_DENSITY=1',
             'missing 1b marker = legacy density on, stops under off')
urban_density_setting <- '1'
require_urban_density_on('state script')
expect_error(check_interface_urban_density(ud_dir, '1a'), 'stage 1a.*built under ECEC_URBAN_DENSITY=0, this run uses 1',
             '1a marker density off stops under on')
check_interface_urban_density(ud_dir, '1b')
unlink(ud_dir, recursive = TRUE)
rm(urban_density_setting)
cat('PASS: urban-density switch -- setting from global / env var, default off, markers, mismatch stops\n')


#------------------------------
# State-price alpha experiment (reprice_alpha_to_state_supply.R)
#------------------------------

source('src/shared_functions/reprice_alpha_to_state_supply.R')
source('src/shared_functions/get_sector_hours_matrices.R')
rp_V <- function(x, beta, rho) beta * log(pmax(x, 1))

# State base supply prices: unit cost (L_req %*% w) + residual, sector 1 = 0
rp_sp <- list(L_req = rbind(c(0.04, 0.05), c(0.16, 0.04), c(0.13, 0.06), c(0.18, 0.04)),
              w = c(10, 18), delta_0 = c(NA, 0.5, 4.6, 2.9))
stopifnot(isTRUE(all.equal(state_base_supply_prices(rp_sp),
                           c(0, 0.16 * 10 + 0.04 * 18 + 0.5, 0.13 * 10 + 0.06 * 18 + 4.6,
                             0.18 * 10 + 0.04 * 18 + 2.9))))

# Mode switch: default state, national off, unknown stops
old_mode <- if (exists('state_alpha_price_mode', envir = globalenv())) get('state_alpha_price_mode', envir = globalenv()) else NULL
for (m in c('national', 'state')) {
  assign('state_alpha_price_mode', m, envir = globalenv())
  stopifnot(identical(get_state_alpha_price_mode(), m))
}
assign('state_alpha_price_mode', 'bogus', envir = globalenv())
stopifnot(inherits(tryCatch(get_state_alpha_price_mode(), error = function(e) e), 'error'))
if (is.null(old_mode)) rm('state_alpha_price_mode', envir = globalenv()) else assign('state_alpha_price_mode', old_mode, envir = globalenv())

rp_build <- function(n_children, n_units, hh_start) {
  catalog <- get_choice_catalog(n_children)
  K <- nrow(catalog)
  wedge <- list(center_low = runif(n_units, 0.5, 1.5), center_high = runif(n_units, 0.5, 1.5),
                home = runif(n_units, 0.5, 1.5), other_paid = runif(n_units, 0.5, 1.5))
  Y <- matrix(runif(n_units * K, 40000, 90000), n_units)
  P_nat <- c(0, 3.5, 7.8, 5.0); P_state <- c(0, 2.6, 6.0, 4.0)
  op <- 5.4
  C_nat <- compute_care_cost_matrix(catalog, n_children, wedge, P_nat, op)
  V_nat <- rp_V(Y - C_nat, 2.5, 0.05)
  p0 <- matrix(runif(n_units * K, 0.01, 1), n_units); p0 <- p0 / rowSums(p0)
  raw <- log(p0) - V_nat
  alpha <- (raw - raw[, 1])[, -1, drop = FALSE]
  ids <- data.frame(hh_id = hh_start:(hh_start + n_units - 1), parent_unit_id = 1, pseudofamily_id = 1)
  wts <- data.frame(child_weight.1 = runif(n_units, 50, 150))
  if (n_children == 2) wts$child_weight.2 <- runif(n_units, 50, 150)
  list(ad = list(alpha = alpha, p0 = p0, child_weights = wts, row_ids = ids, beta = 2.5, rho = 0.05),
       Y = Y, NI = Y - C_nat, wedge = wedge, P_nat = P_nat, P_state = P_state, op = op, K = K, ids = ids)
}
rp_write_csv <- function(b, dir, type, ni = b$NI) {
  d <- cbind(b$ids, setNames(as.data.frame(b$Y), paste0('Y.', 1:b$K)),
             setNames(as.data.frame(ni), paste0('net_income.', 1:b$K)))
  utils::write.csv(d, file.path(dir, paste0('calibration_', type, '_2019.csv')), row.names = FALSE)
}
rp_wedge_rows <- function(b) {
  tibble(hh_id = b$ids$hh_id, parent_unit_id = b$ids$parent_unit_id,
         price_wedge.center_low = b$wedge$center_low, price_wedge.center_high = b$wedge$center_high,
         price_wedge.home = b$wedge$home, price_wedge.other_paid = b$wedge$other_paid)
}
rp_dir <- file.path(tempdir(), 'reprice_alpha_test'); dir.create(rp_dir, showWarnings = FALSE)
b1 <- rp_build(1, 40, 1); b2 <- rp_build(2, 8, 1001)
rp_write_csv(b1, rp_dir, 'c1'); rp_write_csv(b2, rp_dir, 'c2plus')
rp_ads <- list(c1 = b1$ad, c2plus = b2$ad)
rp_wedges <- bind_rows(rp_wedge_rows(b1), rp_wedge_rows(b2))
rp_ids <- c(1:30, 1001:1006)    # geography: first 30 one-child units, first 6 two-child units
rp_call <- function(ads = rp_ads, P_state = b1$P_state, csv_dir = rp_dir) {
  reprice_alpha_to_supply(ads, rp_ids, csv_dir, rp_wedges, b1$P_nat, P_state, b1$op,
                          state_postal = 'XX', v_fn = rp_V)
}
r_same <- rp_call(P_state = b1$P_nat)
for (t in c('c1', 'c2plus')) {
  ins <- rp_ads[[t]]$row_ids$hh_id %in% rp_ids
  # State prices = national prices: alpha unchanged
  stopifnot(max(abs(r_same$alpha_data_by_type[[t]]$alpha - rp_ads[[t]]$alpha[ins, ])) < 1e-9)
}
r <- rp_call()
for (t in c('c1', 'c2plus')) {
  ins <- rp_ads[[t]]$row_ids$hh_id %in% rp_ids
  a_new <- r$alpha_data_by_type[[t]]
  # Rows restricted to the geography, alpha moved, identity: p0 recovered at state V
  stopifnot(nrow(a_new$alpha) == sum(ins), max(abs(a_new$alpha - rp_ads[[t]]$alpha[ins, ])) > 1e-3,
            max(abs(compute_probs_from_alpha(a_new$alpha, r$V_by_type[[t]]) - rp_ads[[t]]$p0[ins, ])) < 1e-9)
}
stopifnot(all(r$diagnostics$identity_error < 1e-9), all(r$diagnostics$ni_check_error < 1e-9))
# Direction. Stored alpha is normalised to choice 1, a high-price center choice
# whose price falls. A choice with no care cost (employment none, Parent Only)
# keeps its V, so its alpha moves by -dV_1, positive when state prices are lower
cat1 <- get_choice_catalog(1)
po <- which(cat1$child1_ecec_type == 'Parent Only' & cat1$employment_choice == 'none')
stopifnot(length(po) == 1, po > 1)
d_po <- r$alpha_data_by_type$c1$alpha[, po - 1] - rp_ads$c1$alpha[rp_ads$c1$row_ids$hh_id %in% rp_ids, po - 1]
stopifnot(all(d_po > 0))
# Net income check: calibration net income that disagrees with rebuilt Y - C stops
b1_bad <- b1; ni_bad <- b1$NI; ni_bad[3, 5] <- ni_bad[3, 5] + 50
rp_dir_bad <- file.path(tempdir(), 'reprice_alpha_test_bad'); dir.create(rp_dir_bad, showWarnings = FALSE)
rp_write_csv(b1_bad, rp_dir_bad, 'c1', ni = ni_bad); rp_write_csv(b2, rp_dir_bad, 'c2plus')
stopifnot(inherits(tryCatch(rp_call(csv_dir = rp_dir_bad), error = function(e) e), 'error'))
# Missing calibration row stops
stopifnot(inherits(tryCatch(rp_call(ads = list(c1 = modifyList(b1$ad, list(row_ids = transform(b1$ad$row_ids, parent_unit_id = 99)))),
                                    csv_dir = rp_dir), error = function(e) e), 'error'))

# Frozen base year: no re-anchoring shift -> sector hours at state V equal the
# hours implied by p0; a 1% different expectation stops
expected <- rep(0, 4)
for (t in c('c1', 'c2plus')) {
  nc <- PARENT_UNIT_N_CHILDREN[match(t, PARENT_UNIT_NAMES)]
  sh <- get_sector_hours_matrices(nc)
  w <- r$alpha_data_by_type[[t]]$child_weights
  e <- crossprod(w$child_weight.1, r$alpha_data_by_type[[t]]$p0) %*% sh$H1
  if (nc == 2) e <- e + crossprod(w$child_weight.2, r$alpha_data_by_type[[t]]$p0) %*% sh$H2
  expected <- expected + as.vector(e)
}
alpha_only <- lapply(r$alpha_data_by_type, function(x) x$alpha)
capture.output(gap <- check_state_base_year(alpha_only, r$V_by_type, r$alpha_data_by_type, expected, 'XX'))
stopifnot(gap < 1e-9)
capture.output(bad <- tryCatch(check_state_base_year(alpha_only, r$V_by_type, r$alpha_data_by_type, expected * 1.01, 'XX'),
                               error = function(e) e))
stopifnot(inherits(bad, 'error'))
cat('PASS: state-price alpha -- supply prices, switch, unchanged at equal prices, p0 identity at state V, net income check, frozen base year\n')


cat('\nAll state-analysis tests passed.\n')
