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
  H <- setNames(rep(0, length(target_types)), target_types)
  for (t in target_types) {
    h1 <- unname(HOURS_ANNUAL[catalog$child1_hours_choice]) *
      (catalog$child1_ecec_type == t)
    H[t] <- H[t] + sum((p %*% h1) * w1)
    if (n_children == 2) {
      h2 <- unname(HOURS_ANNUAL[catalog$child2_hours_choice]) *
        (catalog$child2_ecec_type == t)
      H[t] <- H[t] + sum((p %*% h2) * w2)
    }
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
target_types <- setdiff(unique(CHILD_CARE_CHOICES$ecec_type), 'Parent Only')

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
scale_factors <- setNames(c(1.30, 0.75, 1.10, 0.90, 1.20, 0.85, 1.05), target_types)
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
  'missing care type',
  'missing care type fails loudly'
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

cat('\nAll state-analysis tests passed.\n')
