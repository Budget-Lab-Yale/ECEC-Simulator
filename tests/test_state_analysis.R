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


# Hours for ONE care type, ungrouped. Used to show the shared centre
# adjustment leaves the low/high split alone.
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
# The whole point of one delta for two bands: it pins their SUM and does not
# touch their ratio. Anything that moves the ratio here is the adjustment
# reaching where it should not.

centre_bands <- STATE_TARGET_GROUPS[['Center-Based']]

band_hours <- function(deltas, only = names(alpha_data_by_type)) {
  out <- setNames(numeric(length(centre_bands)), centre_bands)
  for (pu in only) {
    ad <- alpha_data_by_type[[pu]]
    nch <- if (pu == 'c1') 1 else 2
    ins <- ad$row_ids$hh_id %in% state_hh_ids
    m <- build_choice_hours_map(nch, target_types)
    E <- exp(m$d_by_choice(deltas))
    U <- sweep(ad$p0[ins, , drop = FALSE], 2, E, '*')
    P <- U / rowSums(U)
    w1 <- ad$child_weights$child_weight.1[ins]
    w2 <- if (nch == 2) ad$child_weights$child_weight.2[ins] else NULL
    for (b in centre_bands) {
      out[b] <- out[b] + hours_one_type(P, nch, w1, w2, b)
    }
  }
  out
}

zero <- setNames(rep(0, length(target_types)), target_types)

stopifnot(abs(sum(band_hours(result$deltas)) /
                result$targets[['Center-Based']] - 1) < 1e-6)
cat('PASS: combined centre hours hit the single centre target\n')

# WHAT THE SHARED DELTA GUARANTEES: no FAMILY's odds between the two centre
# bands move. That is exact, and it is the property worth testing. Pooled
# hours ratios are not exact -- they are weighted averages whose weights shift
# as families move toward or away from centre care -- so testing those would
# be testing the wrong thing.
c1_band_odds <- function(deltas) {
  ad <- alpha_data_by_type$c1
  ins <- ad$row_ids$hh_id %in% state_hh_ids
  cat1 <- get_choice_catalog(1)
  m <- build_choice_hours_map(1, target_types)
  U <- sweep(ad$p0[ins, , drop = FALSE], 2, exp(m$d_by_choice(deltas)), '*')
  P <- U / rowSums(U)
  lo <- rowSums(P[, cat1$child1_ecec_type == centre_bands[1], drop = FALSE])
  hi <- rowSums(P[, cat1$child1_ecec_type == centre_bands[2], drop = FALSE])
  lo / hi
}

odds_before <- c1_band_odds(zero)
odds_after  <- c1_band_odds(result$deltas)
stopifnot(max(abs(log(odds_after / odds_before))) < 1e-12)
cat(sprintf('PASS: every family\'s low/high odds unchanged (max log dev %.1e)\n',
            max(abs(log(odds_after / odds_before)))))

# The POOLED hours ratio does move, and it should be understood rather than
# asserted away. Two mechanisms: families reweight toward centre care, and a
# two-child choice carries a second child's delta, so bands sitting beside
# different co-child types drift apart as the other five targets move.
r_before <- Reduce(`/`, band_hours(zero))
r_after  <- Reduce(`/`, band_hours(result$deltas))
cat(sprintf('NOTE: pooled low/high hours ratio moves %+.2f%% -- reweighting,\n',
            100 * (r_after / r_before - 1)))
cat('      not the centre adjustment reaching between the bands.\n')


#------------------------------
# Test 1b2: sector hours at the solution -- the basis of base-year labor
#------------------------------
# 3a builds base-year labor as colSums(L_req * sector_hours), so the sector
# hours must be the model's own base-year hours: the formal targets, with the
# centre target split by the model's own bands.

sh <- result$sector_hours
stopifnot(identical(names(sh), SECTOR_MAPPING$ecec_type))
stopifnot(abs(sh[['Unpaid Center-Based']] / result$targets[['Unpaid Center-Based']] - 1) < 1e-6,
          abs(sh[['Paid Home-Based']] / result$targets[['Paid Home-Based']] - 1) < 1e-6,
          abs((sh[['Low-Priced Center-Based']] + sh[['High-Priced Center-Based']]) /
                result$targets[['Center-Based']] - 1) < 1e-6)
stopifnot(max(abs(sh[centre_bands] / band_hours(result$deltas)[centre_bands] - 1)) < 1e-9)
cat('PASS: sector hours reproduce the formal targets and the model\'s own centre bands\n')


#------------------------------
# Test 1c: a target file naming a price band is rejected
#------------------------------
split_csv <- tempfile(fileext = '.csv')
write_csv(tibble(ecec_type = c('Low-Priced Center-Based', names(targets)[-1]),
                 annual_hours = unname(targets)), split_csv)
ok <- tryCatch({
  read_state_demand_targets(split_csv, 'ZZ'); FALSE
}, error = function(e) grepl('price split', conditionMessage(e)))
stopifnot(ok)
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

# Synthetic households: NYC boroughs, rest of NY, county unidentified (0),
# and another state on the same county code
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


cat('\nAll state-analysis tests passed.\n')
