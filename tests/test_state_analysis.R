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
# Test 4b: two-count experiment (ECEC_STATE_CALIBRATION=counts) -- children
# by group x pt/ft, separate code path
#------------------------------

stopifnot(get_state_calibration_mode() == 'hours')
cat('PASS: re-anchoring mode defaults to hours\n')

# Children by cell from probabilities -- independent reimplementation
counts_by_cell <- function(p, n_children, w1, w2) {
  catalog <- get_choice_catalog(n_children)
  N <- c()
  for (g in names(STATE_TARGET_GROUPS)) for (b in c('pt', 'ft')) {
    cell <- paste(g, b, sep = ' | ')
    in1 <- catalog$child1_ecec_type %in% STATE_TARGET_GROUPS[[g]] & catalog$child1_hours_choice == b
    N[cell] <- sum((p %*% in1) * w1)
    if (n_children == 2) {
      in2 <- catalog$child2_ecec_type %in% STATE_TARGET_GROUPS[[g]] & catalog$child2_hours_choice == b
      N[cell] <- N[cell] + sum((p %*% in2) * w2)
    }
  }
  N
}
state_counts <- function(p_c1, p_c2) {
  cw1 <- alpha_data_by_type$c1$child_weights
  cw2 <- alpha_data_by_type$c2plus$child_weights
  counts_by_cell(p_c1, 1, cw1$child_weight.1[in_state_c1], NULL) +
    counts_by_cell(p_c2, 2, cw2$child_weight.1[in_state_c2], cw2$child_weight.2[in_state_c2])
}

N0 <- state_counts(alpha_data_by_type$c1$p0[in_state_c1, ],
                   alpha_data_by_type$c2plus$p0[in_state_c2, ])
# Cells ordered group x (pt, ft); factors move counts AND part-time shares
count_targets <- N0 * c(1.25, 1.10, 0.80, 0.95, 1.10, 1.05,
                        0.90, 0.85, 1.15, 1.20, 0.85, 0.90)
groups <- names(STATE_TARGET_GROUPS)
pt_t <- count_targets[paste(groups, 'pt', sep = ' | ')]
ft_t <- count_targets[paste(groups, 'ft', sep = ' | ')]

# Hours file + count file in sibling dirs, as config/state/{demand,demand_counts}
counts_root <- file.path(tempdir(), 'state_counts_test')
dir.create(file.path(counts_root, 'demand'), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(counts_root, 'demand_counts'), recursive = TRUE, showWarnings = FALSE)
hours_csv <- file.path(counts_root, 'demand', 'ZZ.csv')
file.copy(targets_csv, hours_csv, overwrite = TRUE)
write_csv(tibble(ecec_type = groups, children = unname(pt_t + ft_t), part_time = unname(pt_t)),
          file.path(counts_root, 'demand_counts', 'ZZ.csv'))

res_counts <- adjust_alpha_for_state_counts(alpha_data_by_type, state_hh_ids, hours_csv, 'ZZ')
stopifnot(max(abs(log(res_counts$achieved / count_targets[names(res_counts$achieved)]))) < 1e-6)
cat('PASS: two-count contraction converged to children by group x pt/ft (',
    res_counts$n_iter, ' iterations)\n', sep = '')

# Returned alphas reproduce the counts independently (V = 0)
p_from_alpha <- function(a) { e <- exp(cbind(0, a)); e / rowSums(e) }
N_alpha <- state_counts(p_from_alpha(res_counts$alpha_by_type$c1),
                        p_from_alpha(res_counts$alpha_by_type$c2plus))
stopifnot(max(abs(N_alpha / count_targets - 1)) < 1e-6)
cat('PASS: two-count alphas independently reproduce the counts and part-time shares\n')

# Counts on hours grid imply hours: sector hours sum to implied formal hours
implied_hours <- setNames(unname(pt_t) * HOURS_ANNUAL[['pt']] +
                            unname(ft_t) * HOURS_ANNUAL[['ft']], groups)
stopifnot(abs(sum(res_counts$sector_hours) /
                sum(implied_hours[c('Center-Based', 'Unpaid Center-Based',
                                    'Paid Home-Based')]) - 1) < 1e-6)
cat('PASS: two-count sector hours equal hours the counts imply\n')

# Empty band (DE Unpaid Home-Based: no part-time child) -> one group cell on
# children alone
write_csv(tibble(ecec_type = groups, children = unname(pt_t + ft_t),
                 part_time = replace(unname(pt_t), 6, 0)),
          file.path(counts_root, 'demand_counts', 'ZZ.csv'))
res_all <- adjust_alpha_for_state_counts(alpha_data_by_type, state_hh_ids, hours_csv, 'ZZ')
k_all <- paste(groups[6], 'all', sep = ' | ')
stopifnot(length(res_all$targets) == 11, k_all %in% names(res_all$targets),
          max(abs(log(res_all$achieved / res_all$targets))) < 1e-6,
          abs(res_all$achieved[[k_all]] / (pt_t[[6]] + ft_t[[6]]) - 1) < 1e-6)
cat('PASS: two-count, empty band -> group matched on children alone\n')

# Missing count file fails loudly
expect_error(
  adjust_alpha_for_state_counts(alpha_data_by_type, state_hh_ids, targets_csv, 'ZZ'),
  'needs',
  'two-count run without count file fails loudly'
)

#------------------------------
# Test 4c: unpaid-care remap experiment (ECEC_REMAP_UNPAID=1)
#------------------------------

source('src/shared_functions/remap_unpaid_care.R')

# Take function definitions only from files that run code / need full model
take_defs <- function(path, names) {
  for (e in parse(path)) {
    if (is.call(e) && identical(e[[1]], as.name('<-')) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% names) eval(e, envir = globalenv())
  }
}
take_defs('src/3_simulation/3c_run_scenario.R', 'compute_supply_residuals')
take_defs('src/3_simulation/3d_accumulate_results.R',
          c('add_unpaid_remap_fiscal', 'get_employer_subsidy_rate'))

sp_toy <- list(L_req = matrix(c(0.20, 0.15, 0.12, 0.10, 0.05, 0.08, 0.10, 0.02), 4, 2),
               w = c(12, 18), L = c(4e5, 2e5), e = c(1.5, 2), delta_0 = c(0, 1, 2, 1),
               cpi_factor = 1, nominal_wage_factor = 1)
P_toy  <- c(0, 6, 9, 5)
Qd_toy <- c(2e6, 1.5e6, 1e6, 8e5)

stopifnot(!unpaid_remap_on())
off <- compute_supply_residuals(P_toy, Qd_toy, sp_toy)
stopifnot(identical(off$Qd, Qd_toy),
          identical(off$L, colSums(sp_toy$L_req * Qd_toy)),
          identical(remap_unpaid_demand(Qd_toy), Qd_toy))
fis_toy <- tibble(year = 2025, total_demand_subsidy = 1)
stopifnot(identical(add_unpaid_remap_fiscal(fis_toy, list(unpaid_remap_price = 7)), fis_toy))
emp_res <- list(employer_subsidy = c(1, 2), L_req = sp_toy$L_req, employer_subsidy_sectors = 1:4)
stopifnot(identical(get_employer_subsidy_rate(emp_res),
                    as.vector(sp_toy$L_req %*% c(1, 2))))
cat('PASS: remap off -- supply residuals, fiscal table, employer rates unchanged\n')

remap_unpaid_care <- '1'
on <- compute_supply_residuals(P_toy, Qd_toy, sp_toy)
Qd_remap <- c(0, Qd_toy[1] + Qd_toy[2], Qd_toy[3:4])
stopifnot(identical(on$Qd, Qd_remap),
          isTRUE(all.equal(on$L, colSums(sp_toy$L_req * Qd_remap))))
cat('PASS: remap on -- unpaid-center hours land in the low-price sector (labor, wages)\n')

# Discrete choices: c1 unit pt unpaid; c2 unit ft unpaid + pt low-price
pu_toy <- list(
  c1 = tibble(ecec_type.1 = c('Unpaid Center-Based', 'Parent Only'),
              ecec_hours.1 = c('pt', 'none'), per_weight1 = c(100, 50), per_weight2 = NA_real_),
  c2plus = tibble(ecec_type.1 = 'Unpaid Center-Based', ecec_hours.1 = 'ft',
                  ecec_type.2 = 'Low-Priced Center-Based', ecec_hours.2 = 'pt',
                  per_weight1 = 80, per_weight2 = 120))
price_toy <- 7.5
hours_toy <- 100 * HOURS_ANNUAL[['pt']] + 100 * HOURS_ANNUAL[['ft']]
fis_on <- add_unpaid_remap_fiscal(fis_toy, list(parent_units = pu_toy,
                                                unpaid_remap_price = price_toy))
stopifnot(isTRUE(all.equal(fis_on$unpaid_remap_hours, hours_toy)),
          isTRUE(all.equal(fis_on$total_unpaid_remap_payment, price_toy * hours_toy / 1e9)))
stopifnot(all(get_employer_subsidy_rate(emp_res)[1:2] ==
                as.vector(sp_toy$L_req %*% c(1, 2))[2]))
cat('PASS: remap on -- public payment = low-price supplier price x remapped hours\n')
rm(remap_unpaid_care)

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
source('src/shared_functions/compute_price_wedge_cache.R')
source('src/shared_functions/compute_substate_alpha.R')
random_seed_base <- 76

# NY (36): 150 NYC households (Manhattan 061), richer, 150 rest of NY (Albany
# 001); NJ (34): 300. One parent unit, one child under 5 each
n_nyc <- 150
tw_hh <- tibble(hh_id     = 1:600,
                statefip  = rep(c(36L, 36L, 34L), c(n_nyc, 150, 300)),
                countyfip = rep(c(61L, 1L, 3L), c(n_nyc, 150, 300)),
                region    = 1L,
                income    = c(rexp(n_nyc, 1 / 120000), rexp(450, 1 / 60000)))
is_nyc <- tw_hh$countyfip == 61L & tw_hh$statefip == 36L
tw_pu <- tibble(hh_id = tw_hh$hh_id, parent_unit_id = 1L, n_parents = 1L)
tw_ch <- tibble(hh_id = tw_hh$hh_id, parent_unit_id = 1L, child_id = 1L, age = 2,
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

# -- Price wedge quantile grid: weighted empirical inverse CDF
# Hand example: x 1..4, weights 1, 1, 2, 4 (total 8) -> cumulative shares
# 0.125, 0.25, 0.5, 1. q(p) = smallest x w/ share >= p
hq <- price_wedge_quantile_grid(c(3, 1, 4, 2), c(2, 1, 4, 1),
                                probs = c(0.01, 0.125, 0.126, 0.25, 0.3, 0.5, 0.51, 0.99, 1))
stopifnot(identical(hq, c(1, 1, 2, 2, 3, 3, 4, 4, 4)))
# Default grid: 99 values; equal weights on 1..100 -> grid = 1..99
stopifnot(identical(price_wedge_quantile_grid(as.numeric(1:100), rep(1, 100)), as.numeric(1:99)))
# Weights as frequencies: weight-3 value = three copies
gx <- rexp(200); gw <- sample(1:3, 200, replace = TRUE)
stopifnot(identical(price_wedge_quantile_grid(gx, gw), price_wedge_quantile_grid(rep(gx, gw), rep(1, sum(gw)))))
# NA rows dropped, row order irrelevant
stopifnot(identical(price_wedge_quantile_grid(c(gx, NA), c(gw, 1)), price_wedge_quantile_grid(gx, gw)),
          identical(price_wedge_quantile_grid(rev(gx), rev(gw)), price_wedge_quantile_grid(gx, gw)))
cat('PASS: price_wedge_quantile_grid matches hand example, frequency-weight copies, ignores NA / order\n')

# Monotone, values observed wedges
lx <- rlnorm(3000, 0, 0.6); lw <- runif(3000, 10, 500)
lg <- price_wedge_quantile_grid(lx, lw)
stopifnot(length(lg) == 99, all(diff(lg) >= 0), all(lg %in% lx))
cat('PASS: price_wedge_quantile_grid monotone, 99 observed values\n')

expect_error(price_wedge_quantile_grid(c(1, 2), c(0, 0)), 'positive total', 'zero total weight stops')
expect_error(price_wedge_quantile_grid(c(1, 2), 1), 'one weight per value', 'length mismatch stops')

# -- Price wedge draws keyed on (hh_id, parent_unit_id), own streams
# Reference values from Python big-int fmix32 (exact 32-bit arithmetic)
stopifnot(fmix_2_32(0xdeadbeef) == 233162409, fmix_2_32(4294967295) == 2180083513,
          identical(draw_price_pctile_idx(c(1, 2, 1, 123456789, 4294967295), c(1, 1, 2, 3, 1), 79),
                    c(9L, 80L, 39L, 7L, 86L)))
cat('PASS: draw_price_pctile_idx matches exact 32-bit reference hash\n')

# ACS parent unit ids are text ('1', '1_2', 1b) -> numeric key hhm1 x 1000 +
# hhm2; draw on text ids = draw on those keys; bad id -> stop
stopifnot(identical(parent_unit_key(c('1', '1_2', '12_3')), c(1000, 1002, 12003)),
          identical(parent_unit_key(c(5, 7)), c(5, 7)),
          all(is.na(parent_unit_key(c('a', '1_2_3', '1000')))),
          identical(draw_price_pctile_idx(c(2395, 2395, 2397), c('1_2', '3', '1_2'), 79),
                    draw_price_pctile_idx(c(2395, 2395, 2397), c(1002, 3000, 1002), 79)))
stopifnot(inherits(tryCatch(draw_price_pctile_idx(1, 'x', 79), error = function(e) e), 'error'))
cat('PASS: parent_unit_key -- ACS text parent unit ids hash as numeric keys\n')

# Round trip on 1b's own id builder: every one-parent id + every sorted pair
# (1b sorts hhm_id1 < hhm_id2), person numbers 1-120 -> keys non-NA, one per
# unit, draw runs. Catches 1b id format change before cluster calibration.
rt <- rbind(data.frame(h1 = 1:120, h2 = NA_integer_),
            subset(expand.grid(h1 = 1:120, h2 = 1:120), h1 < h2))
rt_ids  <- make_parent_unit_id(rt$h1, rt$h2)
rt_keys <- parent_unit_key(rt_ids)
stopifnot(is.character(rt_ids), !anyNA(rt_keys), !anyDuplicated(rt_keys),
          identical(rt_keys, rt$h1 * 1000 + ifelse(is.na(rt$h2), 0, rt$h2)),
          all(draw_price_pctile_idx(rep(1, nrow(rt)), rt_ids, 79) %in% 1:99))
cat('PASS: make_parent_unit_id -- 1b ids round-trip through parent_unit_key, ',
    nrow(rt), ' units\n', sep = '')

k_hh <- rep(1:60000, each = 2); k_pu <- rep(1:2, 60000)
k_idx <- draw_price_pctile_idx(k_hh, k_pu, random_seed_base + 3)
perm <- sample(length(k_hh)); sub_i <- sort(sample(length(k_hh), 5000))
stopifnot(all(k_idx %in% 1:99), length(unique(k_idx)) == 99,
          chisq.test(tabulate(k_idx, 99))$p.value > 1e-3,
          identical(draw_price_pctile_idx(k_hh[perm], k_pu[perm], random_seed_base + 3), k_idx[perm]),
          identical(draw_price_pctile_idx(k_hh[sub_i], k_pu[sub_i], random_seed_base + 3), k_idx[sub_i]),
          mean(draw_price_pctile_idx(k_hh, k_pu, random_seed_base + 4) == k_idx) < 0.03)
cat('PASS: keyed draw uniform on 1:99, same index under row shuffle / subset, seed moves it\n')

# -- Price wedge cache: wedge = grid[[type]][keyed idx], clipped [0.1, 5]
# Grids encode index: center_low = 1 + idx / 100 (decodes idx), center_high
# spans 0.01..5.9 (clip both ends), home / other_paid constant
tw_grid <- list(center_low  = 1 + (1:99) / 100,
                center_high = seq(0.01, 5.9, length.out = 99),
                home        = rep(0.8, 99),
                other_paid  = rep(1.2, 99))
# Quintile grids encode quintile + index: center_low q -> 0.5 q + idx / 1000,
# other_paid q -> 0.2 q + idx / 1000. Base tests: no percentile -> unconditional
tw_qgrid <- list(center_low = lapply(1:5, function(q) 0.5 * q + (1:99) / 1000),
                 other_paid = lapply(1:5, function(q) 0.2 * q + (1:99) / 1000))
tw_pq <- list(quantile_grid = tw_grid, quantile_grid_by_quintile = tw_qgrid,
              income_rank_scheme = 'national', summary_stats = list(other_paid_base_price = 10))
tw_ip_na <- tibble(hh_id = tw_hh$hh_id, income_pctile = NA_integer_)
tw_cache <- function(pu = tw_pu, ch = tw_ch, ip = tw_ip_na, pq = tw_pq) {
  compute_price_wedge_cache(pu, ch, pq, ip)
}
wedge_cols <- c('price_wedge.center_low', 'price_wedge.center_high', 'price_wedge.home', 'price_wedge.other_paid')

base_w <- tw_cache()
base_idx <- draw_price_pctile_idx(tw_pu$hh_id, tw_pu$parent_unit_id, random_seed_base + 3)
stopifnot(identical(names(base_w), c('hh_id', 'parent_unit_id', wedge_cols)),
          identical(base_w$hh_id, tw_pu$hh_id),
          identical(round((base_w$price_wedge.center_low - 1) * 100), as.numeric(base_idx)),
          isTRUE(all.equal(base_w$price_wedge.center_high, pmax(0.1, pmin(5, tw_grid$center_high[base_idx])))),
          all(base_w$price_wedge.home == 0.8), all(base_w$price_wedge.other_paid == 1.2))
cat('PASS: price wedge cache -- columns unchanged, wedge = grid at keyed index, all types share index\n')

# Clipping: grid beyond [0.1, 5] -> bounds
clip_pq <- tw_pq; clip_pq$quantile_grid$center_low <- c(rep(0.001, 50), rep(50, 49))
clip_w <- tw_cache(pq = clip_pq)
stopifnot(all(clip_w$price_wedge.center_low %in% c(0.1, 5)),
          identical(clip_w$price_wedge.center_low == 0.1, base_idx <= 50),
          all(base_w$price_wedge.center_high >= 0.1 & base_w$price_wedge.center_high <= 5))
cat('PASS: price wedge cache clips multipliers to [0.1, 5]\n')

# Rows shuffled, NY-only subset, income reversed -> same wedge per unit
shuf <- sample(nrow(tw_pu))
r_shuf <- tw_cache(tw_pu[shuf, ], tw_ch[sample(nrow(tw_ch)), ])
ny_ids <- tw_hh$hh_id[tw_hh$statefip == 36L]
r_ny <- tw_cache(tw_pu %>% filter(hh_id %in% ny_ids), tw_ch %>% filter(hh_id %in% ny_ids))
stopifnot(identical(r_shuf[match(base_w$hh_id, r_shuf$hh_id), ], base_w),
          identical(r_ny, base_w %>% filter(hh_id %in% ny_ids)))
cat('PASS: price wedge cache same per unit under shuffled rows, state subset\n')

# Units w/out child under 5 get NA; children 5+ / extra rows change nothing
tw_ch8 <- bind_rows(tw_ch %>% filter(hh_id > 10),
                    tibble(hh_id = 1:20, parent_unit_id = 1L, child_id = 2L, age = 8, child_weight = 1e6))
r8 <- tw_cache(ch = tw_ch8)
stopifnot(all(is.na(as.matrix(r8[r8$hh_id <= 10, wedge_cols]))),
          identical(r8[r8$hh_id > 10, ], base_w[base_w$hh_id > 10, ]))
# Employment-choice expansion (3 rows per unit) -> one row per unit
r3 <- tw_cache(bind_rows(tw_pu, tw_pu, tw_pu))
stopifnot(identical(r3, base_w))
cat('PASS: price wedge cache -- NA w/out child under 5, one row per unit\n')

expect_error(tw_cache(pq = list(quantile_grid = tw_grid[1:3])),
             'Rerun NSECE processing', 'cache w/out all four grids stops')
expect_error(tw_cache(pq = list(center_low_price = list())),
             'Rerun NSECE processing', 'old QRF cache object stops')
expect_error(tw_cache(pq = list(quantile_grid = tw_grid, income_rank_scheme = 'national')),
             'lacks quantile_grid_by_quintile.*Rerun NSECE', 'cache w/out quintile grids stops')
bad_q <- tw_pq; bad_q$quantile_grid_by_quintile$other_paid <- tw_qgrid$other_paid[1:4]
expect_error(tw_cache(pq = bad_q), 'lacks quantile_grid_by_quintile', 'cache w/ 4 quintile grids stops')

# -- Scheme mismatch, unlabeled cache stop
mis_pq <- tw_pq; mis_pq$income_rank_scheme <- 'national_rpp'
expect_error(tw_cache(pq = mis_pq),
             'built under income-rank scheme "national_rpp", this run uses "national"',
             'price grids from other income-rank scheme stop')
unl_pq <- tw_pq; unl_pq$income_rank_scheme <- NULL
expect_error(tw_cache(pq = unl_pq), 'scheme "unlabeled"', 'unlabeled price grids stop')
expect_error(compute_price_wedge_cache(tw_pu, tw_ch, tw_pq, NULL),
             'income_pctiles needs hh_id', 'missing income_pctiles stops')

# -- Quintile grids (1a): one per quintile, thin cell -> unconditional
# 2 provider rows per household; q1-q4 40 households each, q5 10 (< 30),
# 15 households w/out rank (NA) enter unconditional grid only
qg_n <- c(40, 40, 40, 40, 10, 15)
qg_hh <- rep(seq_len(sum(qg_n)), each = 2)
qg_q <- rep(rep(c(1:5, NA), qg_n), each = 2)
qg_x <- rlnorm(length(qg_hh), log(ifelse(is.na(qg_q), 3, qg_q)), 0.3)
qg_w <- runif(length(qg_hh), 10, 200)
qg <- price_wedge_quintile_grids(qg_x, qg_w, qg_hh, qg_q)
qg_uncond <- price_wedge_quantile_grid(qg_x, qg_w)
stopifnot(identical(qg$n_hh, c(40L, 40L, 40L, 40L, 10L)), identical(qg$fallback, 5L),
          identical(qg$grids[[5]], qg_uncond),
          all(vapply(1:4, function(q) {
            i <- !is.na(qg_q) & qg_q == q
            identical(qg$grids[[q]], price_wedge_quantile_grid(qg_x[i], qg_w[i]))
          }, logical(1))),
          all(diff(vapply(qg$grids[1:4], median, numeric(1))) > 0))
# Cell size counts households, not rows: 20 households x 2 rows -> 20 < 30
qg2 <- price_wedge_quintile_grids(qg_x[1:40], qg_w[1:40], qg_hh[1:40], qg_q[1:40])
stopifnot(qg2$n_hh[1] == 20L, 1L %in% qg2$fallback,
          identical(qg2$grids[[1]], price_wedge_quantile_grid(qg_x[1:40], qg_w[1:40])))
# Row order irrelevant
qp <- sample(length(qg_x))
stopifnot(identical(price_wedge_quintile_grids(qg_x[qp], qg_w[qp], qg_hh[qp], qg_q[qp]), qg))
cat('PASS: price_wedge_quintile_grids -- grid per quintile, < 30 households -> unconditional, ',
    'counts households, ignores row order\n', sep = '')
expect_error(price_wedge_quintile_grids(1:3, 1:3, 1:3, c(1, 6, NA)), 'must be 1-5',
             'quintile outside 1-5 stops')

# -- Cache w/ household percentiles: center_low / other_paid read quintile
# grid at shared idx; center_high / home unchanged; NA percentile -> unconditional
tw_ip <- tibble(hh_id = tw_hh$hh_id, income_pctile = rep_len(c(1L, 20L, 21L, 40L, 55L, 80L, 81L, 100L, NA), 600))
tw_q <- as.integer(ceiling(tw_ip$income_pctile / 20))
q_w <- tw_cache(ip = tw_ip)
has_q <- !is.na(tw_q)
stopifnot(identical(names(q_w), c('hh_id', 'parent_unit_id', wedge_cols)),
          isTRUE(all.equal(q_w$price_wedge.center_low[has_q], 0.5 * tw_q[has_q] + base_idx[has_q] / 1000)),
          isTRUE(all.equal(q_w$price_wedge.other_paid[has_q], 0.2 * tw_q[has_q] + base_idx[has_q] / 1000)),
          identical(q_w$price_wedge.center_low[!has_q], base_w$price_wedge.center_low[!has_q]),
          identical(q_w$price_wedge.other_paid[!has_q], base_w$price_wedge.other_paid[!has_q]),
          identical(q_w$price_wedge.center_high, base_w$price_wedge.center_high),
          identical(q_w$price_wedge.home, base_w$price_wedge.home),
          identical(sort(unique(tw_q[has_q])), 1:5))
# Household missing from income_pctiles -> unconditional
q_miss <- tw_cache(ip = tw_ip %>% filter(hh_id > 100))
stopifnot(identical(q_miss$price_wedge.center_low[1:100], base_w$price_wedge.center_low[1:100]),
          identical(q_miss[-(1:100), ], q_w[-(1:100), ]))
cat('PASS: price wedge cache -- center_low / other_paid from household quintile grid at shared idx, ',
    'center_high / home unchanged, no percentile -> unconditional\n', sep = '')

# Clip applies to quintile grids too
cq_pq <- tw_pq; cq_pq$quantile_grid_by_quintile$center_low[[1]] <- rep(9, 99)
cq_w <- tw_cache(ip = tw_ip, pq = cq_pq)
stopifnot(all(cq_w$price_wedge.center_low[which(tw_q == 1L)] == 5))
cat('PASS: price wedge cache clips quintile-grid multipliers to [0.1, 5]\n')

# Shuffled rows (units, children, percentiles), state subset -> same wedge
r_qs <- tw_cache(tw_pu[shuf, ], tw_ch[sample(nrow(tw_ch)), ], tw_ip[sample(nrow(tw_ip)), ])
r_qny <- tw_cache(tw_pu %>% filter(hh_id %in% ny_ids), tw_ch %>% filter(hh_id %in% ny_ids), tw_ip)
stopifnot(identical(r_qs[match(q_w$hh_id, r_qs$hh_id), ], q_w),
          identical(r_qny, q_w %>% filter(hh_id %in% ny_ids)))
cat('PASS: price wedge cache w/ quintiles same per unit under shuffled rows, state subset\n')

set.seed(5); r1 <- runif(3)
set.seed(5); invisible(tw_cache()); invisible(tw_cache(ip = tw_ip)); r2 <- runif(3)
stopifnot(identical(r1, r2))
cat('PASS: price wedge cache leaves global RNG stream untouched\n')

# -- ACS income percentiles for price quintile (price_wedge_income_pctiles())
# national scheme: cutpoints = child-weighted quantiles of income + keyed
# jitter over children under 5 w/ positive income
tw_hh0 <- tw_hh; tw_hh0$income[c(5, 6)] <- c(0, -100)
ip_nat <- price_wedge_income_pctiles(tw_hh0, tw_ch)
ok_k <- tw_hh0$income > 0
jit <- keyed_unif_2_32(tw_ch$hh_id, tw_ch$child_id, random_seed_base + 4)
man_cut <- Hmisc::wtd.quantile(tw_hh0$income[ok_k] + jit[ok_k], tw_ch$child_weight[ok_k],
                               seq(0.01, 0.99, 0.01))
man_p <- assign_income_pctile(tw_hh0$income, rep(0L, 600), list(`0` = man_cut))
man_p[!ok_k] <- NA_integer_
stopifnot(identical(ip_nat$hh_id, tw_hh0$hh_id), identical(ip_nat$income_pctile, man_p),
          all(is.na(ip_nat$income_pctile[5:6])), all(jit >= 0 & jit < 1))
# Child-weighted quintile shares ~ 20% each
qs <- tapply(tw_ch$child_weight[ok_k], ceiling(man_p[ok_k] / 20), sum) / sum(tw_ch$child_weight[ok_k])
stopifnot(max(abs(qs - 0.2)) < 0.03)
# Row order irrelevant; households w/out child under 5 dropped
ip_shuf <- price_wedge_income_pctiles(tw_hh0[sample(600), ], tw_ch[sample(600), ])
stopifnot(identical(ip_shuf[match(ip_nat$hh_id, ip_shuf$hh_id), ], ip_nat))
ip_old <- price_wedge_income_pctiles(tw_hh0, tw_ch %>% mutate(age = if_else(hh_id <= 3, 7, age)))
stopifnot(!any(1:3 %in% ip_old$hh_id))
cat('PASS: price_wedge_income_pctiles -- national cutpoints w/ keyed jitter, NA w/out positive income, ',
    'row order irrelevant\n', sep = '')
set.seed(5); r1 <- runif(3)
set.seed(5); invisible(price_wedge_income_pctiles(tw_hh0, tw_ch)); r2 <- runif(3)
stopifnot(identical(r1, r2))
cat('PASS: price_wedge_income_pctiles leaves global RNG stream untouched\n')

# Geo version: NULL under national; national_rpp -- NYC at NYC RPP vs same
# cutpoints, rest identical to national version
invisible(capture.output(ip_g <- price_wedge_income_pctiles(tw_hh, tw_ch, 'NYC')))
stopifnot(is.null(ip_g))
income_rank_scheme <- 'national_rpp'
ip_rpp <- price_wedge_income_pctiles(tw_hh, tw_ch)
invisible(capture.output(ip_nyc <- price_wedge_income_pctiles(tw_hh, tw_ch, 'NYC')))
ri_rpp <- income_rank_inputs(tw_hh$income, tw_hh$region, tw_hh$statefip, 'acs')
rpp_cut <- Hmisc::wtd.quantile(ri_rpp$income + jit, tw_ch$child_weight, seq(0.01, 0.99, 0.01))
stopifnot(identical(ip_rpp$income_pctile,
                    assign_income_pctile(ri_rpp$income, rep(0L, 600), list(`0` = rpp_cut))),
          identical(ip_nyc$income_pctile[!is_nyc], ip_rpp$income_pctile[!is_nyc]),
          identical(ip_nyc$income_pctile[is_nyc],
                    assign_income_pctile(tw_hh$income[is_nyc] / 1.14899, rep(0L, sum(is_nyc)),
                                         list(`0` = rpp_cut))),
          all(ip_nyc$income_pctile[is_nyc] <= ip_rpp$income_pctile[is_nyc]),
          any(ip_nyc$income_pctile[is_nyc] < ip_rpp$income_pctile[is_nyc]))
cat('PASS: price_wedge_income_pctiles -- NYC own version at NYC RPP vs national cutpoints, ',
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
write_alpha_rds <- function(scheme) {
  for (t in PARENT_UNIT_NAMES) {
    a <- list(alpha = matrix(0, 1, 1), geo = 'NYC')
    a$income_rank_scheme <- scheme
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
unlink(est_dir, recursive = TRUE)

# -- 2a helper: NYC alpha w/ national wedge rows, national beta / rho, NYC rows only
source('src/shared_functions/compute_probs_from_alpha.R')
income_rank_scheme <- 'national_rpp'
est_dir <- file.path(tempdir(), 'substate_alpha_calib')
dir.create(est_dir, showWarnings = FALSE)
writeLines('hh_id,child_id', file.path(est_dir, 'acs_enrollment_2019_NYC.csv'))
tw_puw <- tibble(hh_id = tw_hh$hh_id, parent_unit_id = 1L, pseudofamily_id = 1L,
                 child_category = '1', child_weight.1 = runif(600, 50, 150)) %>%
  left_join(base_w, by = c('hh_id', 'parent_unit_id'))
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
    parent_units_wide = tw_puw, acs_tables = list(households = tw_hh, parent_units = tw_pu, children = tw_ch),
    build_type_data = mock_build, compute_V = mock_V,
    supply_prices = c(0, 1, 1, 1), other_paid_base_price = 1, wage_perturbation = 1.01,
    tax_max = 1, data_dir = est_dir, estimation_dir = est_dir, year = 2019))
  r
}
stopifnot(identical(run_helper(), 'NYC'))
ad <- readRDS(substate_alpha_path(est_dir, 'c1', 2019, 'NYC'))
geo_w <- base_w[is_nyc, ]
stopifnot(identical(ad$row_ids$hh_id, tw_hh$hh_id[is_nyc]), ad$beta == 2.5,
          ad$income_rank_scheme == 'national_rpp', ad$geo == 'NYC',
          isTRUE(all.equal(ad$child_weights$child_weight.1, tw_puw$child_weight.1[is_nyc])))
V_exp <- 2.5 * log(50000 - outer(geo_w$price_wedge.center_low, 1:45) * 100)
stopifnot(max(abs(ad$alpha - (-(V_exp - V_exp[, 1])[, -1]))) < 1e-10,
          max(abs(compute_probs_from_alpha(ad$alpha, V_exp) - 1 / 45)) < 1e-12)
cat('PASS: compute_substate_alphas -- NYC rows, national wedge rows in V, national beta, alpha recovers p0\n')

# w/ price grids: NYC wedge rows rebuilt on NYC's own income ranks
rpp_pq <- tw_pq; rpp_pq$income_rank_scheme <- 'national_rpp'
invisible(capture.output({
  compute_substate_alphas(
    parent_units_wide = tw_puw, acs_tables = list(households = tw_hh, parent_units = tw_pu, children = tw_ch),
    build_type_data = mock_build, compute_V = mock_V,
    supply_prices = c(0, 1, 1, 1), other_paid_base_price = 1, wage_perturbation = 1.01,
    tax_max = 1, data_dir = est_dir, estimation_dir = est_dir, year = 2019,
    price_quantiles = rpp_pq)
  ip_nyc <- price_wedge_income_pctiles(tw_hh, tw_ch, 'NYC')
}))
ad_q <- readRDS(substate_alpha_path(est_dir, 'c1', 2019, 'NYC'))
q_nyc <- as.integer(ceiling(ip_nyc$income_pctile[match(tw_hh$hh_id[is_nyc], ip_nyc$hh_id)] / 20))
cl_nyc <- 0.5 * q_nyc + base_idx[is_nyc] / 1000
V_q <- 2.5 * log(50000 - outer(cl_nyc, 1:45) * 100)
stopifnot(max(abs(ad_q$alpha - (-(V_q - V_q[, 1])[, -1]))) < 1e-10,
          identical(ad_q$row_ids$hh_id, tw_hh$hh_id[is_nyc]))
cat('PASS: compute_substate_alphas -- w/ price grids, NYC wedge rows from NYC own income quintiles\n')

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
rm(income_rank_scheme)


#------------------------------
# NSECE unknown payment (1a assign_primary_care()) -- counting unknown rows
# take drawn label; label payment can't change same from any draw
#------------------------------

source('src/shared_functions/assign_primary_care.R')
apc <- function(regular, hours, choice, kind, imputed = rep(NA_character_, length(regular))) {
  r <- assign_primary_care(tibble(regular = regular, hours = hours, ecec_choice = choice,
                                  unknown_kind = kind, imputed_choice = imputed))
  paste(r$ecec_choice, r$hours)
}
U <- NA_character_
# Label that payment cannot change -> same label from every draw
stopifnot(apc(c(1, 1), c(10, 30), c(U, 'Low-Priced Center-Based'), c('home', U),
              c('Unpaid Home-Based', U)) == 'Low-Priced Center-Based 30',
          apc(c(1, 1), c(10, 30), c(U, 'Low-Priced Center-Based'), c('home', U),
              c('Paid Home-Based', U)) == 'Low-Priced Center-Based 30',
          apc(c(1, 1), c(5, 30), c(U, 'Unpaid Center-Based'), c('other', U),
              c('Other Paid', U)) == 'Unpaid Center-Based 30',
          apc(c(1, 1), c(5, 30), c(U, 'Unpaid Center-Based'), c('other', U),
              c('Other Unpaid', U)) == 'Unpaid Center-Based 30')
# Other Paid pt either way -- band invariant, primary hours follow draw (3 vs 15)
stopifnot(apc(c(1, 1), c(12, 3), c(U, 'Other Paid'), c('other', U), c('Other Unpaid', U)) == 'Other Paid 3',
          apc(c(1, 1), c(12, 3), c(U, 'Other Paid'), c('other', U), c('Other Paid', U)) == 'Other Paid 15')
cat('PASS: assign_primary_care -- unknown payment that cannot change label: same label from any draw\n')
# Counting unknown row w/out draw -> NA
stopifnot(apc(1, 25, U, 'center') == 'NA NA',
          apc(c(1, 1), c(10, 30), c(U, 'Low-Priced Center-Based'), c('home', U)) == 'NA NA')
# Non-regular unknown row -> Parent Only (no draw needed); no rows -> Parent
# Only; no counting row -> Parent Only (no NA bucket since 2026-10-12 --
# regularity resolved upstream, as 02)
stopifnot(apc(0, 0, U, 'home') == 'Parent Only 0',
          apc(numeric(0), numeric(0), character(0), character(0)) == 'Parent Only 0',
          apc(NA, 10, U, U) == 'Parent Only 0')
# Both paid-centre tiers -> High, whatever hours (simulator standard, main 1a)
stopifnot(apc(c(1, 1), c(10, 20), c('High-Priced Center-Based', 'Low-Priced Center-Based'), c(U, U)) ==
            'High-Priced Center-Based 10')
cat('PASS: assign_primary_care -- missing draw NA, Parent Only / NA buckets, both tiers -> High\n')

# Provider care type (classify_nsece_provider(), 1a + 02): home-based needs
# outside home + no prior relationship; unknown payment -> no label, kind set
source('src/shared_functions/resolve_regular_care.R')
cp <- classify_nsece_provider(tibble(
  is_k8 = c(F, F, F, F, T), is_center = c(T, F, F, F, F), is_home = c(F, T, T, T, F),
  is_paid = c(T, T, F, F, F), pay_unknown = c(F, F, F, T, F),
  in_child_home = c(F, F, T, F, F), has_prior_rel = F), 'Center-Based')
stopifnot(identical(cp$ecec_choice, c('Center-Based', 'Paid Home-Based', 'Other Unpaid', NA, 'K-8')),
          identical(cp$unknown_kind, c(NA, NA, NA, 'home', NA)))
cat('PASS: classify_nsece_provider -- centre / home / other / unknown payment / K-8\n')

# Unknown-payment imputation (impute_unknown_payment()) -- reproducible,
# global RNG untouched, cells from known rows, used only for ambiguous kids
source('src/shared_functions/impute_unknown_payment.R')
imp_arr <- tibble(
  hh_id = c(1, 1, 2, 2, 3, 4, 5, 6), child_id = 1, provider_id = c(1, 2, 1, 2, 1, 1, 1, 1),
  kind  = c('other', 'center', 'other', 'other', 'other', 'center', 'center', 'center'),
  paid  = c(NA, TRUE, NA, FALSE, TRUE, NA, TRUE, FALSE),
  tier  = c(NA, 'Low', NA, NA, NA, NA, 'High', NA),
  counts = TRUE, age = c(2, 2, 3, 3, 2, 1, 1, 0), region = 1,
  child_weight = c(1, 1, 2, 2, 3, 1, 1, 3))
set.seed(11); g0 <- runif(1); set.seed(11)
i1 <- impute_unknown_payment(imp_arr)
g1 <- runif(1)
i2 <- impute_unknown_payment(imp_arr[sample(nrow(imp_arr)), ])
stopifnot(identical(g0, g1), identical(i1$draws, i2$draws),
          nrow(i1$draws) == 3,
          # other 2-3: known rows hh 2 (unpaid, wt 2), hh 3 (paid, wt 3) -> 0.6
          isTRUE(all.equal(i1$paid_cells$paid_share[i1$paid_cells$kind == 'other'], 0.6)),
          # centre 0-1: hh 5 paid (wt 1), hh 6 unpaid (wt 3) -> 0.25; tier 0-1 all High
          isTRUE(all.equal(i1$paid_cells$paid_share[i1$paid_cells$kind == 'center' &
                                                      i1$paid_cells$age_cell == '0-1'], 0.25)),
          all(i1$draws$imp_tier[i1$draws$kind == 'center' & i1$draws$imp_paid] %in% 'High'),
          all(is.na(i1$draws$imp_tier[i1$draws$kind != 'center' | !i1$draws$imp_paid])))
cat('PASS: impute_unknown_payment -- same draws on reshuffled input, global RNG restored, weighted cells\n')

# Questionnaire paid status (paid_q, HH9_PAID) wins over draw; other rows'
# draws unchanged. Paid centre w/ imputed hours (tier_unknown) -> tier
# alone drawn, own stream -- main draws unchanged
i3 <- impute_unknown_payment(mutate(imp_arr, paid_q = c(FALSE, NA, TRUE, NA, NA, TRUE, NA, NA)))
d1 <- arrange(i1$draws, hh_id); d3 <- arrange(i3$draws, hh_id)
stopifnot(identical(d3$imp_paid, c(FALSE, TRUE, TRUE)),
          identical(d3$u_paid, d1$u_paid), identical(d3$u_tier, d1$u_tier),
          # hh 4: centre, paid by questionnaire -> tier from 0-1 cell (all High)
          d3$imp_tier[d3$hh_id == 4] == 'High',
          !any(d3$tier_only))
i4 <- impute_unknown_payment(mutate(imp_arr, tier_unknown = hh_id == 5,
                                    tier = if_else(hh_id == 5, NA_character_, tier)))
d4 <- arrange(i4$draws, hh_id)
stopifnot(nrow(d4) == 4, identical(d4$tier_only, c(FALSE, FALSE, FALSE, TRUE)),
          identical(d4$u_paid[1:3], d1$u_paid), identical(d4$imp_paid[1:3], d1$imp_paid),
          d4$imp_paid[4], is.na(d4$u_paid[4]),
          # hh 5 no longer tier donor -> only donor hh 1 (Low, 2-3 cell);
          # 0-1 cell empty, regional + national -> no tier
          is.na(d4$imp_tier[4]))
i5 <- impute_unknown_payment(mutate(imp_arr, tier_unknown = hh_id == 3))
stopifnot(!any(i5$draws$tier_only))   # tier_unknown on non-centre row ignored
cat('PASS: impute_unknown_payment -- HH9_PAID answer wins, tier-only draws on own stream\n')
# Tier cells: region x density x age (>= 30 donors) -> density x age -> age.
# Donors age 2-3: region 1 density 1: 30 rows all High; region 2 density 1:
# 5 rows all Low (thin -> density 1 x age: 30 High + 5 Low = 30/35);
# density 3: 10 rows Low in region 1 (thin both finer levels -> age:
# 30 / 45 High). Paid share forced 1 by known paid donors only.
td <- tibble(hh_id = 100 + 1:45, child_id = 1, provider_id = 1, kind = 'center', paid = TRUE,
             tier = c(rep('High', 30), rep('Low', 15)), counts = TRUE, age = 2,
             region = c(rep(1, 30), rep(2, 5), rep(1, 10)),
             density = c(rep(1, 35), rep(3, 10)), child_weight = 1)
tu <- tibble(hh_id = 1:3, child_id = 1, provider_id = 1, kind = 'center', paid = NA, tier = NA,
             counts = TRUE, age = 2, region = c(1, 2, 1), density = c(1, 1, 3), child_weight = 1)
ti <- impute_unknown_payment(bind_rows(td, tu))
tdv <- ti$draws %>% arrange(hh_id)
stopifnot(identical(tdv$tier_level, c('region x density x age', 'density x age', 'age')),
          isTRUE(all.equal(tdv$high_share, c(1, 30 / 35, 30 / 45))),
          all(tdv$imp_paid))
# Without density column: age cell only, paid uniforms unchanged
ti0 <- impute_unknown_payment(bind_rows(td, tu) %>% select(-density))
stopifnot(all(ti0$draws$tier_level == 'age'), identical(ti0$draws$u_paid, ti$draws$u_paid),
          identical(ti0$draws$imp_paid, ti$draws$imp_paid))
cat('PASS: impute_unknown_payment -- tier cells region x density x age -> density x age -> age (>= 30 donors)\n')
apc_imp <- function(regular, hours, choice, kind, imputed) {
  r <- assign_primary_care(tibble(regular = regular, hours = hours, ecec_choice = choice,
                                  unknown_kind = kind, imputed_choice = imputed))
  paste(r$ecec_choice, r$hours, r$imputed)
}
# Counting unknown row -> imputed flag set, even when label invariant
stopifnot(apc_imp(c(1, 1), c(5, 30), c(U, 'Unpaid Center-Based'), c('other', U),
                  c('Other Paid', U)) == 'Unpaid Center-Based 30 TRUE',
          apc_imp(c(1, 1), c(5, 30), c('Other Unpaid', 'Unpaid Center-Based'), c(U, U),
                  c(U, U)) == 'Unpaid Center-Based 30 FALSE')
# Label payment can change -> drawn label used
stopifnot(apc_imp(c(1, 1), c(30, 5), c(U, 'Other Unpaid'), c('other', U),
                  c('Other Paid', U)) == 'Other Paid 30 TRUE',
          apc_imp(c(1, 1), c(30, 5), c(U, 'Other Unpaid'), c('other', U),
                  c('Other Unpaid', U)) == 'Other Unpaid 35 TRUE',
          apc_imp(1, 25, U, 'center', 'Low-Priced Center-Based') == 'Low-Priced Center-Based 25 TRUE',
          # Ambiguous, no draw -> NA
          apc_imp(1, 25, U, 'center', U) == 'NA NA FALSE')
stopifnot(identical(imputed_pay_label(c('center', 'center', 'home', 'other'), c(TRUE, FALSE, TRUE, NA),
                                      c('Low', NA, NA, NA)),
                    c('Low-Priced Center-Based', 'Unpaid Center-Based', 'Paid Home-Based', NA)),
          imputed_pay_label('center', TRUE, NA, paid_centre = 'Center-Based') == 'Center-Based')
cat('PASS: assign_primary_care -- drawn labels on counting unknown rows\n')


#------------------------------
# NSECE regular care (resolve_regular_care(), 1a + 02): zero-hour regular
# provider counts w/ imputed hours (fix 1); REGCARE -1 / -2 regular when TOC
# 1-5 (fix 2); REGCARE 0 stays out
#------------------------------

source('src/shared_functions/resolve_regular_care.R')
rc_arr <- tibble(
  hh_id = 1:9, child_id = 1, provider_id = 1,
  regcare  = c(1, 1, 1, 1, 1, -2, -1, -1, 0),
  type_raw = c(4, 4, 4, 4, 2, 2, 6, 5, 4),
  hours    = c(10, 30, 45, 0, 25, -1, 0, NA, 0),
  age = 2, child_weight = c(1, 2, 1, 1, 1, 1, 1, 1, 1))
rc <- resolve_regular_care(rc_arr)$arr
# Centre 2-3 donors: 10 (wt 1), 30 (wt 2), 45 (wt 1) -> drawn hours one of them
stopifnot(identical(rc$regular, c(1, 1, 1, 1, 1, 1, 0, 1, 0)),
          rc$hours[4] %in% c(10, 30, 45), rc$hours_imputed[4],
          !any(rc$hours_imputed[c(1:3, 5, 7, 9)]),
          # REGCARE -2, TOC 2 -> regular, hours from individual cell (25)
          rc$regular_by_type[6], rc$hours[6] == 25,
          # REGCARE -1, TOC 6 (K-8) -> not regular; TOC 5 -> regular, no TOC 5
          # donors -> NA hours (no pooled cell either)
          !rc$regular_by_type[7], rc$regular_by_type[8], is.na(rc$hours[8]),
          # REGCARE 0 -> out, hours as reported
          rc$regular[9] == 0, rc$hours[9] == 0)
stopifnot(weighted_median(c(1, 2, 3), c(1, 1, 1)) == 2, weighted_median(c(5, 1), c(3, 1)) == 5,
          is.na(weighted_median(numeric(0), numeric(0))))
# Imputed hours make child count: Parent Only before, centre after
stopifnot(apc(rc$regular[4], 30, 'Unpaid Center-Based', U) == 'Unpaid Center-Based 30',
          apc(1, 0, 'Unpaid Center-Based', U) == 'Parent Only 0')
cat('PASS: resolve_regular_care -- zero-hour regular rows imputed, REGCARE -1/-2 by TOC 1-5, REGCARE 0 out\n')

# TOC gate (2026-10-12): only TOC 1-5 non-parental care (NSECE's definition)
# -- TOC 7 / 8 / NA never regular even w/ REGCARE 1, no hours donors, no
# payment donors / draw targets; TOC 6 (K-8) w/ REGCARE 1 still regular
tg_arr <- tibble(
  hh_id = 1:6, child_id = 1, provider_id = 1,
  regcare  = c(1, 1, 1, 1, 1, -2),
  type_raw = c(8, 7, NA, 6, 5, 8),
  hours    = c(20, 15, 10, 30, 0, 12),
  age = 2, child_weight = 1)
tg <- resolve_regular_care(tg_arr)$arr
stopifnot(identical(tg$regular, c(0, 0, 0, 1, 1, 0)),
          !any(tg$hours_imputed[c(1:4, 6)]),
          # TOC 5 zero-hour row: TOC 8 / 7 / NA rows (same 'other' group) not
          # donors -> no donor, NA hours
          tg$hours_imputed[5], is.na(tg$hours[5]),
          identical(regular_care_flag(c(1, 1, 1, -1, -2), c(8, 7, 6, 8, 3)), c(0, 0, 1, 0, 1)))
# Child in care through TOC 8 alone -> Parent Only
stopifnot(apc(tg$regular[1], 20, 'Other Unpaid', U) == 'Parent Only 0')
# Payment imputation: TOC 8 row (kind 'other', counts FALSE) neither donor nor target
tg_imp <- impute_unknown_payment(tibble(
  hh_id = 1:3, child_id = 1, provider_id = 1, kind = 'other',
  paid = c(TRUE, FALSE, NA), tier = NA_character_,
  counts = c(FALSE, TRUE, FALSE), age = 2, region = 1, child_weight = 1))
stopifnot(nrow(tg_imp$draws) == 0, tg_imp$paid_cells$n == 1,
          tg_imp$paid_cells$paid_share == 0)
cat('PASS: resolve_regular_care -- TOC 7 / 8 / NA never regular, not donors; TOC 6 REGCARE 1 kept\n')

# Drawn hours (2026-10-12): weighted donor quantile at keyed uniform on
# (hh_id, child_id, provider_id). Toy centre 0-1 cell: 600 donors 37.5% pt
# by weight, 4000 imputed rows
stopifnot(identical(weighted_quantile_draw(c(10, 30), c(1, 3), c(0, 0.2499, 0.25, 0.9999)),
                    c(10, 10, 30, 30)),
          all(is.na(weighted_quantile_draw(numeric(0), numeric(0), c(0.1, 0.5)))))
set.seed(4242)
dn_h <- c(rep(c(8, 12, 15), each = 100), rep(c(30, 40, 50), each = 100))
# pt weight 300 of 800 -> 37.5%
dn_w <- c(rep(1, 300), rep(c(5 / 3, 5 / 3, 5 / 3), each = 100))
dr_arr <- bind_rows(
  tibble(hh_id = 1:600, child_id = 1, provider_id = 1, regcare = 1, type_raw = 4,
         hours = dn_h, age = 0, child_weight = dn_w)[sample(600), ],
  tibble(hh_id = 1001:5000, child_id = rep(1:2, 2000), provider_id = rep(1:4, 1000),
         regcare = 1, type_raw = 4, hours = 0, age = 0, child_weight = 1))
rng_before <- .Random.seed
dr <- resolve_regular_care(dr_arr)
rng_after <- .Random.seed
dr_imp <- dr$arr %>% filter(hours_imputed)
perm <- sample(nrow(dr_arr))
dr_shuf <- resolve_regular_care(dr_arr[perm, ])$arr %>% filter(hours_imputed)
imp_pt <- mean(dr_imp$hours < 20)
stopifnot(identical(rng_before, rng_after),
          nrow(dr_imp) == 4000, all(dr_imp$hours %in% dn_h),
          abs(dr$cells$pt_share - 0.375) < 1e-12,
          abs(imp_pt - 0.375) < 0.025,
          # same draw per arrangement under row shuffle, subset
          identical(dr_shuf$hours[order(dr_shuf$hh_id)], dr_imp$hours[order(dr_imp$hh_id)]),
          identical(resolve_regular_care(dr_arr[c(1:600, 3401:4001), ])$arr$hours[601:1201],
                    dr_imp$hours[match(dr_arr$hh_id[3401:4001], dr_imp$hh_id)]),
          # other seed -> other draws
          mean(resolve_regular_care(dr_arr, seed = 1L)$arr$hours[601:4600] == dr_imp$hours) < 0.6)
cat(sprintf('PASS: resolve_regular_care drawn hours -- donor pt 37.5%%, imputed pt %.1f%%, shuffle-invariant, global RNG untouched\n',
            100 * imp_pt))


#------------------------------
# NSECE label draws (nsece_mi_draws.R, 1a + 02): draw stage M times, draw m
# seeds base + m x NSECE_MI_SEED_STEP; draw 0 = single draw before; stacked
# child weights sum to original; result independent of row order
#------------------------------

source('src/shared_functions/nsece_mi_draws.R')
stopifnot(NSECE_MI_DRAWS >= 2, NSECE_MI_SEED_STEP > 1,
          nsece_mi_seed(IMPUTE_PAY_SEED, 0) == IMPUTE_PAY_SEED,
          nsece_mi_seed(REGULAR_HOURS_SEED, 0) == REGULAR_HOURS_SEED,
          nsece_mi_seed(5L, 3L) == 5L + 3L * NSECE_MI_SEED_STEP)

# Toy draw stage as 1a: hours (resolve_regular_care()), paid status
# (impute_unknown_payment()), primary arrangement (assign_primary_care()).
# hh 1-40: centre donors (hours 10 / 30, paid half); hh 41-60: zero-hour
# regular centre, payment unknown -> hours, paid status drawn; hh 61-70:
# known unpaid centre, 25 hrs -> same label every draw
mi_arr <- bind_rows(
  tibble(hh_id = 1:40, hours = rep(c(10, 30), 20), net = rep(c(0, 50), each = 20)),
  tibble(hh_id = 41:60, hours = 0, net = NA_real_),
  tibble(hh_id = 61:70, hours = 25, net = 0)) %>%
  mutate(child_id = 1, provider_id = 1, regcare = 1, type_raw = 4, age = 2,
         child_weight = 1 + hh_id %% 3)
mi_label <- function(a, m = NULL) {
  # m NULL -> functions' default seeds (single draw before MI)
  rc <- if (is.null(m)) resolve_regular_care(a)$arr else
    resolve_regular_care(a, seed = nsece_mi_seed(REGULAR_HOURS_SEED, m))$arr
  imp_in <- rc %>%
    transmute(hh_id, child_id, provider_id, kind = 'center', paid = if_else(is.na(net), NA, net > 0),
              tier = if_else(net %in% 50, 'High', NA_character_),
              counts = regular == 1 & !is.na(hours) & hours > 0, age, region = 1, child_weight)
  imp <- if (is.null(m)) impute_unknown_payment(imp_in) else
    impute_unknown_payment(imp_in, seed = nsece_mi_seed(IMPUTE_PAY_SEED, m))
  rc %>%
    left_join(select(imp$draws, hh_id, child_id, provider_id, imp_paid, imp_tier),
              by = c('hh_id', 'child_id', 'provider_id')) %>%
    mutate(unknown_kind = if_else(is.na(net), 'center', NA_character_),
           ecec_choice = case_when(is.na(net) ~ NA_character_, net > 0 ~ 'High-Priced Center-Based',
                                   TRUE ~ 'Unpaid Center-Based'),
           imputed_choice = imputed_pay_label(unknown_kind, imp_paid, imp_tier)) %>%
    group_by(hh_id, child_id) %>%
    group_modify(~ assign_primary_care(.x)) %>%
    ungroup() %>%
    select(hh_id, child_id, ecec_choice, hours) %>%
    arrange(hh_id, child_id)
}
mi_stack <- function(a) stack_mi_draws(lapply(seq_len(NSECE_MI_DRAWS) - 1L, function(m) mi_label(a, m)))
mi <- mi_stack(mi_arr)
mi_w <- mi %>% left_join(select(mi_arr, hh_id, child_id, child_weight), by = c('hh_id', 'child_id'))
mi_var <- mi %>% group_by(hh_id) %>% summarise(k = n_distinct(paste(ecec_choice, hours)), .groups = 'drop')
stopifnot(
  # Draw 0 = single draw w/ default seeds, child for child
  identical(select(filter(mi, draw == 0), hh_id, child_id, ecec_choice, hours), mi_label(mi_arr)),
  # M rows per child, draw weights 1 / M, weighted total exact
  nrow(mi) == NSECE_MI_DRAWS * nrow(mi_arr), all(count(mi, hh_id)$n == NSECE_MI_DRAWS),
  isTRUE(all.equal(sum(mi_w$child_weight * mi_w$draw_weight), sum(mi_arr$child_weight))),
  # Drawn children vary across draws, certain children never
  any(mi_var$k[mi_var$hh_id %in% 41:60] > 1), all(mi_var$k[mi_var$hh_id %in% c(1:40, 61:70)] == 1),
  # Row order free
  identical(mi_stack(mi_arr[sample(nrow(mi_arr)), ]), mi))
# Stack refuses draws w/ different records or duplicate keys
stopifnot(inherits(try(stack_mi_draws(list(mi_label(mi_arr), mi_label(mi_arr)[-1, ])), silent = TRUE), 'try-error'),
          inherits(try(stack_mi_draws(list(bind_rows(mi_label(mi_arr), mi_label(mi_arr)[1, ]))), silent = TRUE),
                   'try-error'))
cat('PASS: NSECE label draws -- draw 0 = single draw, M rows per child, weights sum to original, row order free\n')


#------------------------------
# NSECE parent sex (infer_parent_sex(), 1a; fix 3) + caregiver employment
# (nsece_person_employment(), fix 4)
#------------------------------

source('src/shared_functions/nsece_parent_attributes.R')
ps <- infer_parent_sex(
  tibble(hh_id = c(1, 1, 2, 3, 3, 4, 5), hhm_id = c(1, 2, 2, 2, 3, 2, 1)),
  tibble(hh_id = 1:5, resp_male = c(0, 1, 1, NA, NA),
         n_mothers = c(1, 1, 1, 0, 1), n_fathers = c(1, 1, 1, 0, 0)))
ps_m <- function(h, m) ps$male[ps$hh_id == h & ps$hhm_id == m]
# hh 1: respondent mother -> partner father; hh 2: respondent not a parent,
# 2 counted parents vs 1 linked -> NA; hh 3: counts 0 vs 2 parents ->
# inconsistent NA; hh 5: respondent parent, sex unknown, only mother in
# hh -> 0. ps4: respondent not a parent, one father -> parent 2 male
stopifnot(ps_m(1, 1) == 0, ps_m(1, 2) == 1, is.na(ps_m(2, 2)),
          is.na(ps_m(3, 2)), is.na(ps_m(3, 3)), ps_m(5, 1) == 0)
ps4 <- infer_parent_sex(tibble(hh_id = 4, hhm_id = 2),
                        tibble(hh_id = 4, resp_male = 0, n_mothers = 0, n_fathers = 1))
stopifnot(ps4$male == 1, ps4$sex_source == 'household counts')
emp <- nsece_person_employment(
  worked     = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 3),
  wst_hours  = c(20, 35, 0, NA, 0, 0, 0, 0, 0, 40),
  last_work  = c(NA, NA, NA, NA, 2019, 2018.5, 2017, NA, 9999.99, NA),
  last_hours = c(NA, NA, NA, NA, 1, 3, 3, NA, 2, NA),
  last_work_cutoff = 2018.25)
# 9999.99 = still holds job (User's Guide p.159) -> recent, banded by D5C
stopifnot(identical(emp, c('pt', 'ft', NA, NA, 'pt', 'ft', 'none', 'none', 'pt', NA)))
stopifnot(isTRUE(all.equal(nsece_interview_time(c(10, 20, 5), c(2, -3, NA)),
                           c(2018 + 16 / 12, NA, NA))))
stopifnot(identical(nsece_primary_caregiver(c(NA, 2, 2, 2, 2), c(1, 1, 0, 1, NA), c(NA, 0, 1, 1, 0)),
                    c(1L, 2L, 1L, 1L, 2L)),
          is.na(nsece_primary_caregiver(2, 1, NA)))
cat('PASS: NSECE parent sex from household counts, employment bands (ACS concept), caregiver = mother\n')


#------------------------------
# 1b prepare_data_for_rf() -- family size from every child 0-4, before
# unlabelled children drop; family w/ unlabelled kept child never trains
# one-child forest
#------------------------------

local({
  body_1b <- body(eval(parse('src/1_processing/1b_acs_processing.R')[[1]][[3]]))
  for (e in as.list(body_1b)) {
    if (is.call(e) && identical(e[[1]], as.name('<-')) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% c('derive_employment_choice', 'prepare_data_for_rf',
                                    'get_primary_caregiver')) {
      eval(e, envir = globalenv())
    }
  }
})
stopifnot(exists('prepare_data_for_rf'), exists('derive_employment_choice'))

# hh 1: 2 kids, one NA -> out; hh 2: 1 kid -> one-child; hh 3: 2 kids -> two-child;
# hh 4: 3 kids, oldest NA (censored) -> two-child, is_3plus; hh 5: 3 kids,
# youngest NA -> out; hh 6: 1 kid NA -> out
pr_ch <- tibble(hh_id    = c(1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6),
                child_id = c(1, 2, 1, 1, 2, 1, 2, 3, 1, 2, 3, 1),
                parent_unit_id = '1', child_weight = 100,
                age      = c(1, 3, 2, 0, 4, 0, 2, 4, 0, 2, 4, 3), male = 1)
pr_en <- pr_ch %>%
  transmute(hh_id, child_id,
            ecec_choice = if_else(paste(hh_id, child_id) %in% c('1 2', '4 3', '5 1', '6 1'),
                                  NA_character_, 'Parent Only'),
            ecec_hours_choice = if_else(is.na(ecec_choice), NA_character_, 'none'))
pr_pu <- tibble(hh_id = 1:6, parent_unit_id = '1', hhm_id1 = 1, hhm_id2 = NA_real_,
                male1 = 0, male2 = NA_real_, emp1 = 'ft', emp2 = NA_character_)
pr_hm <- tibble(hh_id = 1:6, hhm_id = 1, age = 30)
pr_hh <- tibble(hh_id = 1:6, income = 1e4 * 1:6, n_members = 4, region = 1, density = 1, rank_group = 1)
pr_rf <- prepare_data_for_rf(pr_ch, pr_pu, pr_hh, pr_hm, pr_en,
                             list(`1` = seq(1e4, 6e4, length.out = 99)), 'rank_group',
                             'NSECE', is_nsece = TRUE)
one <- bind_rows(pr_rf$one_child)
two <- bind_rows(pr_rf$two_child)
stopifnot(identical(sort(unique(one$hh_id)), 2), identical(sort(unique(two$hh_id)), c(3, 4)))
stopifnot(two$n_children_original[two$hh_id == 4] == 3, two$is_3plus[two$hh_id == 4] == 1,
          !any(grepl('NA', two$ecec_class)))
cat('PASS: prepare_data_for_rf -- family w/ unlabelled kept child out of both forests, size counted before drop\n')

# NSECE caregiver employment (fix 4): caregiver = mother in two-parent unit
# (parent 2 here), band from her emp2; mother unknown or band NA -> family
# out (never coded 'none')
# hh 5: two-child family, mother's band NA -> whole family out
ce_ch <- tibble(hh_id = c(1:5, 5), child_id = c(1, 1, 1, 1, 1, 2), parent_unit_id = '1_2',
                child_weight = 100, age = c(2, 2, 2, 2, 1, 3), male = 0)
ce_en <- ce_ch %>% transmute(hh_id, child_id, ecec_choice = 'Parent Only', ecec_hours_choice = 'none')
ce_pu <- tibble(hh_id = 1:5, parent_unit_id = '1_2', hhm_id1 = 1, hhm_id2 = 2,
                male1 = c(1, 1, 1, 0, 0), male2 = c(0, NA, 0, 1, 1),
                emp1 = c('ft', 'ft', 'ft', 'pt', NA), emp2 = c('none', 'pt', NA, 'ft', 'ft'))
ce_hm <- tibble(hh_id = rep(1:5, each = 2), hhm_id = rep(1:2, 5), age = 30)
ce_hh <- tibble(hh_id = 1:5, income = 1e4 * 1:5, n_members = 4, region = 1, density = 1, rank_group = 1)
ce_rf <- prepare_data_for_rf(ce_ch, ce_pu, ce_hh, ce_hm, ce_en,
                             list(`1` = seq(1e4, 5e4, length.out = 99)), 'rank_group',
                             'NSECE', is_nsece = TRUE)
ce <- ce_rf$one_child$`2`
stopifnot(identical(sort(as.numeric(ce$hh_id)), c(1, 4)),
          as.character(ce$employment_choice_pc[ce$hh_id == 1]) == 'none',
          as.character(ce$employment_choice_pc[ce$hh_id == 4]) == 'pt',
          nrow(ce_rf$two_child$`2`) == 0)
cat('PASS: prepare_data_for_rf -- NSECE caregiver = mother, her band; unknown mother / band -> out\n')

# Density feature passes through as factor w/ fixed levels 1-3
stopifnot(identical(levels(one$density), c('1', '2', '3')), all(as.character(one$density) == '1'),
          identical(levels(two$density), c('1', '2', '3')))
cat('PASS: prepare_data_for_rf -- density feature carried, levels 1-3 fixed\n')

# Stacked label draws (1a, nsece_mi_draws.R): 2 draws. hh 2 (one child)
# and hh 3 (two children) labels differ by draw; family size counted within
# draw (copies never make one-child family two-child), two-child classes
# paired within draw, weights x draw_weight -> totals unchanged
pr_en2 <- stack_mi_draws(list(
  pr_en,
  pr_en %>% mutate(ecec_choice = if_else(hh_id %in% c(2, 3) & !is.na(ecec_choice), 'Other Unpaid',
                                         ecec_choice),
                   ecec_hours_choice = if_else(hh_id %in% c(2, 3) & !is.na(ecec_choice), 'ft',
                                               ecec_hours_choice))))
pr_rf2 <- prepare_data_for_rf(pr_ch, pr_pu, pr_hh, pr_hm, pr_en2,
                              list(`1` = seq(1e4, 6e4, length.out = 99)), 'rank_group',
                              'NSECE', is_nsece = TRUE)
one2 <- bind_rows(pr_rf2$one_child)
two2 <- bind_rows(pr_rf2$two_child)
stopifnot(identical(sort(unique(one2$hh_id)), 2), identical(sort(unique(two2$hh_id)), c(3, 4)),
          nrow(one2) == 2, nrow(two2) == 4, all(two2$n_children_original[two2$hh_id == 3] == 2),
          identical(sort(two2$ecec_class[two2$hh_id == 3]),
                    c('Other Unpaid_ft__Other Unpaid_ft', 'Parent Only_none__Parent Only_none')),
          isTRUE(all.equal(sum(one2$model_weight), sum(one$model_weight))),
          isTRUE(all.equal(sum(two2$model_weight), sum(two$model_weight))))
cat('PASS: prepare_data_for_rf -- stacked label draws: family size, pairing within draw, weights / M\n')


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


#------------------------------
# Tract -> PUMA urban density, ACS draw (urban_density.R)
#------------------------------

stopifnot(identical(density_from_urban_share(c(1, 0.85, 0.8499, 0.3, 0.2999, 0, NA)),
                    c(1L, 1L, 2L, 2L, 3L, 3L, NA)))
# 1 degree latitude = 69.09 miles
stopifnot(abs(haversine_mi(40, -75, 41, -75) - EARTH_RADIUS_MI * pi / 180) < 1e-9)

# Distance weighting. Tracts on parallel 40N, east of anchor A by 1, 1.5, 5
# miles. A, B, C radius 0.3 (floor 0.2 for anchor A). D 5 miles off, radius
# 0.3 -> outside 2-mile circle; radius 3.5 -> overlaps (5 - 3.5 <= 2).
mi_lon <- 1 / (EARTH_RADIUS_MI * pi / 180 * cos(40 * pi / 180))
tt <- tibble(geoid = c('A', 'B', 'C', 'D'), lat = 40, lon = -75 + c(0, 1, 1.5, 5) * mi_lon,
             r_mi = 0.3, pop = c(100, 100, 200, 1000), urban = c(100, 0, 200, 0))
dAB <- haversine_mi(40, tt$lon[1], 40, tt$lon[2]); dAC <- haversine_mi(40, tt$lon[1], 40, tt$lon[3])
dAD <- haversine_mi(40, tt$lon[1], 40, tt$lon[4])
sA <- tract_urban_share(tt)
exp_A <- (100 / 0.2 + 0 / dAB + 200 / dAC) / (100 / 0.2 + 100 / dAB + 200 / dAC)
stopifnot(isTRUE(all.equal(sA$urban_share[sA$geoid == 'A'], exp_A)),
          sA$n_scatter[sA$geoid == 'A'] == 3, sA$n_scatter[sA$geoid == 'D'] == 1,
          sA$urban_share[sA$geoid == 'D'] == 0)
sA2 <- tract_urban_share(tt %>% mutate(r_mi = c(0.3, 0.3, 0.3, 3.5)))
exp_A2 <- (100 / 0.2 + 200 / dAC) / (100 / 0.2 + 100 / dAB + 200 / dAC + 1000 / dAD)
stopifnot(isTRUE(all.equal(sA2$urban_share[sA2$geoid == 'A'], exp_A2)),
          sA2$n_scatter[sA2$geoid == 'A'] == 4)
# Row order irrelevant; community w/out population -> NA; bad input -> stop
stopifnot(isTRUE(all.equal(tract_urban_share(tt[4:1, ]) %>% arrange(geoid), sA %>% arrange(geoid))))
stopifnot(is.na(tract_urban_share(tibble(geoid = 'Z', lat = 40, lon = -75, r_mi = 1, pop = 0, urban = 0))$urban_share))
stopifnot(inherits(tryCatch(tract_urban_share(tt %>% mutate(urban = pop + 1)), error = function(e) e), 'error'))

# PUMA aggregation, children-weighted. PUMA 1: children 10 (1), 30 (3), 0
# (NA, no population) -> 0.25 / 0 / 0.75. PUMA 2: one tract level 2
td_t <- tibble(statefip = 1, puma = c(1, 1, 1, 2), density = c(1L, 3L, NA, 2L), children = c(10, 30, 0, 5))
pd <- aggregate_puma_density(td_t)
stopifnot(nrow(pd) == 2, pd$n_tracts[1] == 3, pd$children[1] == 40,
          isTRUE(all.equal(c(pd$share_1[1], pd$share_2[1], pd$share_3[1]), c(0.25, 0, 0.75))),
          pd$share_2[2] == 1)
stopifnot(inherits(tryCatch(aggregate_puma_density(td_t %>% mutate(children = 1)), error = function(e) e), 'error'))

# Modal level: largest share, tie -> denser
stopifnot(identical(modal_density(c(0.6, 0.2, 0.1, 0.4, 0.5), c(0.3, 0.5, 0.2, 0.4, 0.0)),
                    c(1L, 2L, 3L, 1L, 1L)))
stopifnot(inherits(tryCatch(modal_density(c(0.5, NA), c(0.5, 0)), error = function(e) e), 'error'))

# Mixture: toy predictor P(class A) = 0.9 / 0.5 / 0.1 at density 1 / 2 / 3.
# Shares (0.6, 0.3, 0.1) -> 0.54 + 0.15 + 0.01 = 0.70; pure PUMA -> its level;
# no share columns -> predictor on own density; rows independent
toy_fn <- function(d) {
  pa <- c(0.9, 0.5, 0.1)[as.integer(as.character(d$density))]
  cbind(A = pa, B = 1 - pa)
}
mx <- tibble(density = factor(c(1, 3, 2), levels = 1:3),
             share_1 = c(0.6, 1, 0), share_2 = c(0.3, 0, 0))
pm <- mix_density_predictions(mx, toy_fn)
stopifnot(isTRUE(all.equal(unname(pm[, 'A']), c(0.70, 0.9, 0.1))),
          isTRUE(all.equal(unname(rowSums(pm)), rep(1, 3))),
          isTRUE(all.equal(mix_density_predictions(select(mx, density), toy_fn),
                           toy_fn(select(mx, density)))),
          isTRUE(all.equal(mix_density_predictions(mx[3:1, ], toy_fn), pm[3:1, ])))
stopifnot(inherits(tryCatch(mix_density_predictions(mutate(mx, share_1 = NA), toy_fn),
                            error = function(e) e), 'error'))

# Committed table: one row per PUMA, shares sum to 1, covers every ACS
# household's (statefip, puma) in crosswalk
pdt <- read_csv('resources/acs/puma_urban_density_2010.csv', show_col_types = FALSE)
xw_t <- utils::read.csv(gzfile('resources/acs/US_ACS__household_county_puma_crosswalk__2019.csv.gz'),
                        colClasses = 'numeric') %>% distinct(statefip = STATEFIP, puma = PUMA)
stopifnot(!any(duplicated(pdt[c('statefip', 'puma')])),
          all(abs(pdt$share_1 + pdt$share_2 + pdt$share_3 - 1) < 1e-5),
          all(pdt$children > 0),
          nrow(anti_join(xw_t, pdt, by = c('statefip', 'puma'))) == 0)
cat('PASS: tract_urban_share / aggregate_puma_density / mix_density_predictions -- distance weighting, overlap, child-weighted PUMA shares, predictions averaged over PUMA mix\n')


cat('\nAll state-analysis tests passed.\n')
