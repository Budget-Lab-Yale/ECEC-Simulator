#------------------------------------------------------------------------------
# alpha_drift.R -- paid-centre share of children, re-anchored 2019 base year
# vs simulated 2026 baseline, per geography, for one ECEC_STATE_ALPHA_PRICE
# setting. Rebuilds alpha exactly as the run did (initialize_simulation()
# under run's code + env), then evaluates saved 2026 baseline outputs.
#
# CLUSTER ONLY (calibration interface + run outputs on Bouchet). Definitions
# fixed before any result was looked at (2026-10-19 review):
#
# Universe: children 0-4 in the geography's calibration parent units (child
#   slots .1 / .2; 3+ child families as pseudofamily pairs, child weight
#   divided by m - 1, so each child counted once). Weights: ACS child weights
#   (2019 alpha files; 2026 aged weights in saved parent units), unrounded.
# Paid-centre child: care sector Low- or High-Priced Center-Based (sectors 2-3
#   of SECTOR_MAPPING), either hours band. Same indicator both years:
#   get_sector_hours_matrices()$H1 / $H2 > 0.
# Denominator: all universe children (Parent Only and every care type),
#   same definition both years.
# 2019 (smooth): base-year probabilities after re-anchoring -- p0 tilted by
#   the solved care-type deltas (adjust_alpha_for_state()), identical under
#   both alpha settings (contraction uses p0 only). Captured from the
#   initialize_simulation() call itself.
# 2026 smooth: expected probabilities exp(alpha + V) / Z at saved equilibrium
#   prices (supply/supply_2026.yaml, 15 digits), re-anchored alpha from this
#   init, solver's own parent units + CPI factor (prepare_year_context(),
#   same call + state view as 3b), saved employment shifts
#   (employment_targeting_2026.csv), baseline policies --
#   get_demand_prob_matrix(), same call the solver uses. Saved
#   parent_units_2026.rds not used for smooth: one row per Gumbel draw,
#   simulation columns (cpi_factor_2019 etc.) stripped before write.
#   Gate: sector hours from these probabilities vs solver Qd (printed to whole
#   hours) -> qd_rel_gap; smooth figures accepted only when <= QD_TOL (1e-4)
#   -> qd_check_pass.
# 2026 drawn: Gumbel-drawn choices in saved parent units (ecec_type.1 / .2),
#   same weights -- reported separately, never mixed w/ smooth.
# Output per geo: numerator, denominator, share, pp change (2026 - 2019),
#   smooth + drawn. Summary: equal-geography mean, mean absolute change,
#   min, max; population-weighted (2019 denominator) mean, mean absolute.
#   50 states + DC in summary; NYC row written, left out (overlaps NY).
#
# Usage (from a run copy w/ run's code + config, env as run):
#   ECEC_STATE_ALPHA_PRICE=<state|national> ECEC_URBAN_DENSITY=1 \
#   ECEC_INCOME_RANK=national ECEC_STATE_AGING=national \
#   Rscript state_data/cluster/alpha_drift.R -r state_targets_all -C <calib ts> \
#     --drift-run <run ts> --drift-src-root <shared output root> --drift-out <dir>
#   config/default_paths.yaml roots.output must point at a scratch root that
#   holds <calib ts>/estimation (copied) -- init writes metadata there, never
#   into shared run folders.
#------------------------------------------------------------------------------

ARGS <- commandArgs(trailingOnly = TRUE)
arg_val <- function(flag) {
  i <- which(ARGS == flag)
  if (!length(i)) stop('alpha_drift.R: missing ', flag)
  ARGS[i + 1]
}
DRIFT_RUN  <- arg_val('--drift-run')
SRC_ROOT   <- arg_val('--drift-src-root')
OUT_DIR    <- arg_val('--drift-out')
YEAR       <- 2026L
MODE       <- Sys.getenv('ECEC_STATE_ALPHA_PRICE', 'state')
PAID_CENTRE_SECTORS <- 2:3
# Reconstruction gate, set before results: smooth 2026 sector hours must match
# solver Qd (printed to whole hours) within 1e-4 relative -- else geo's smooth
# figures not accepted (qd_check_pass FALSE)
QD_TOL <- 1e-4
# Strip own flags before config.R parses the rest
drop <- unlist(lapply(c('--drift-run', '--drift-src-root', '--drift-out'),
                      function(f) { i <- which(ARGS == f); c(i, i + 1) }))
assign('commandArgs', function(trailingOnly = FALSE) {
  if (trailingOnly) ARGS[-drop] else c(base::commandArgs(FALSE)[1], ARGS[-drop])
}, envir = globalenv())

random_seed_base <<- 76
set.seed(random_seed_base)
suppressPackageStartupMessages({
  library(tidyverse); library(data.table); library(yaml); library(Hmisc)
  library(ranger); library(openxlsx)
})
source('./src/misc/config.R')
walk(list.files('./src', pattern = '\\.R$', recursive = TRUE),
     ~ if (!.x %in% c('main.R', 'misc/config.R', '4_output/report_figures.R',
                      '4_output/report_figures_child_ubi.R',
                      '4_output/report_figures_thresh85.R')) source(file.path('./src/', .x)))
run_states <- get_run_states(runscript)

# Estimation inputs: copy calibration interface into this scratch output root
src_est <- file.path(default_paths$roots$output, calibration_interface, 'estimation')
if (!dir.exists(src_est)) stop('Copy ', calibration_interface, '/estimation into ',
                               default_paths$roots$output, ' first.')
file.copy(src_est, output_root, recursive = TRUE)

# Capture each re-anchoring: inputs + solution -> 2019 probabilities
captured <- list()
.orig_adjust <- adjust_alpha_for_state
adjust_alpha_for_state <- function(alpha_data_by_type, state_hh_ids, targets_csv_path,
                                   state_postal) {
  r <- .orig_adjust(alpha_data_by_type, state_hh_ids, targets_csv_path, state_postal)
  captured[[state_postal]] <<- list(res = r, alpha_data = alpha_data_by_type,
                                    hh = state_hh_ids, targets = targets_csv_path)
  r
}

sim_ctx <- initialize_simulation()
stopifnot(setequal(names(captured), names(sim_ctx$state_contexts)))



paid_child_sums <- function(P, n_children, w1, w2) {

  #----------------------------------------------------------------------------
  # Weighted paid-centre children + all children for a probability matrix.
  #
  # Params: P (matrix) n_units x n_choices; n_children (int) 1 or 2;
  #   w1, w2 (num vec) child weights (w2 NULL for 1-child)
  # Returns: (num vec) c(paid, all)
  #----------------------------------------------------------------------------

  sh <- get_sector_hours_matrices(n_children)
  in1 <- (rowSums(sh$H1[, PAID_CENTRE_SECTORS, drop = FALSE]) > 0) * 1
  paid <- sum(w1 * as.vector(P %*% in1))
  all <- sum(w1)
  if (n_children == 2) {
    in2 <- (rowSums(sh$H2[, PAID_CENTRE_SECTORS, drop = FALSE]) > 0) * 1
    paid <- paid + sum(w2 * as.vector(P %*% in2))
    all <- all + sum(w2)
  }
  c(paid = paid, all = all)
}



base_year_sums <- function(cap) {

  #----------------------------------------------------------------------------
  # 2019 re-anchored probabilities (p0 x exp(deltas by choice), normalized),
  # same construction as adjust_alpha_for_state()'s solution.
  #
  # Params: cap (list) captured re-anchoring
  # Returns: (num vec) c(paid, all)
  #----------------------------------------------------------------------------

  target_types <- names(cap$res$targets)
  tot <- c(paid = 0, all = 0)
  for (pu_type in names(cap$alpha_data)) {
    ad <- cap$alpha_data[[pu_type]]
    n_children <- PARENT_UNIT_N_CHILDREN[match(pu_type, PARENT_UNIT_NAMES)]
    in_state <- ad$row_ids$hh_id %in% cap$hh
    if (!any(in_state)) next
    map <- build_choice_hours_map(n_children, target_types)
    E <- exp(map$d_by_choice(cap$res$deltas))
    U <- sweep(ad$p0[in_state, , drop = FALSE], 2, E, '*')
    P <- U / rowSums(U)
    w <- ad$child_weights[in_state, , drop = FALSE]
    w1 <- replace(w[['child_weight.1']], is.na(w[['child_weight.1']]), 0)
    w2 <- if (n_children == 2) replace(w[['child_weight.2']], is.na(w[['child_weight.2']]), 0) else NULL
    tot <- tot + paid_child_sums(P, n_children, w1, w2)
  }
  tot
}



read_solver <- function(path) {

  #----------------------------------------------------------------------------
  # Final market prices + Qd hours from solver_results_<year>.txt.
  #
  # Returns: (list) P (num 4), Qd (num 4), SECTOR_MAPPING order
  #----------------------------------------------------------------------------

  x <- readLines(path)
  grab <- function(start, col) {
    i <- grep(start, x)[1]
    sapply(SECTOR_MAPPING$ecec_type, function(t) {
      l <- x[(i + 1):(i + 8)][startsWith(trimws(x[(i + 1):(i + 8)]), t)][1]
      v <- strsplit(trimws(sub(t, '', l, fixed = TRUE)), '\\s+')[[1]]
      as.numeric(gsub(',', '', v[col]))
    })
  }
  list(P = grab('^PRICES \\(Market\\)', 2), Qd = grab('^QUANTITIES DEMANDED', 1))
}



st_dir <- function(st) file.path(SRC_ROOT, DRIFT_RUN, 'simulation', 'baseline', 'states', st)
binfo <- sim_ctx$baseline_info
fn_demand   <- load_demand_policy(binfo$equilibrium$policy_demand)$do_demand_policy
fn_cdctc    <- load_cdctc_policy(binfo$policy_cdctc %||% 'baseline')
fn_transfer <- if (exists('load_transfer_policy')) load_transfer_policy(binfo$policy_transfer) else NULL


rows <- list()
for (st in names(sim_ctx$state_contexts)) {
  ctx <- sim_ctx$state_contexts[[st]]
  y19 <- base_year_sums(captured[[st]])

  sv <- read_solver(file.path(st_dir(st), 'models', 'equilibrium',
                              paste0('solver_results_', YEAR, '.txt')))
  et <- fread(file.path(st_dir(st), 'models', 'equilibrium',
                        paste0('employment_targeting_', YEAR, '.csv')))
  shifts <- setNames(et$delta_shift, et$group)
  # Full-precision equilibrium prices (3c writes supply yaml at 15 digits)
  sy <- read_yaml(file.path(st_dir(st), 'supply', paste0('supply_', YEAR, '.yaml')))
  P_eq <- as.numeric(unlist(sy$equilibrium_prices[c(
    'unpaid_center_based', 'low_price_center_based', 'high_price_center_based', 'paid_home_based')]))
  stopifnot(length(P_eq) == 4, max(abs(P_eq - sv$P)) < 1e-4)
  pus <- readRDS(file.path(st_dir(st), 'data', paste0('parent_units_', YEAR, '.rds')))

  # Solver's own inputs: same prepare_year_context() call as 3b, state view.
  # Saved units lack simulation columns (cpi_factor_2019 etc., stripped
  # before write) and hold one row per Gumbel draw -- not solver input
  yc <- prepare_year_context(
    sim_base_hh        = ctx$sim_base_hh,
    macro_projections  = sim_ctx$macro_projections,
    base_supply_params = ctx$base_supply_params,
    base_demand_params = ctx$base_demand_params,
    baseline_info      = binfo,
    cached_price_wedge = ctx$cached_price_wedge,
    year               = YEAR,
    aging_base_year    = acs_base_year,
    seed_offset        = seed_offset
  )

  sm <- c(paid = 0, all = 0); dr <- c(paid = 0, all = 0); qd <- rep(0, 4)
  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]; n_children <- PARENT_UNIT_N_CHILDREN[i]
    df <- yc$parent_units_list[[pu_name]]
    dfx <- as.data.frame(pus[[pu_name]])
    if (is.null(df) || !nrow(df)) next
    P <- get_demand_prob_matrix(P_eq, df, n_children, yc$demand_params, pu_name,
                                fn_demand, fn_cdctc,
                                cpi_growth_factor = yc$supply_params$cpi_factor,
                                employment_shifts = shifts, policy_transfer = fn_transfer)
    w1 <- df[['child_weight.1']]
    w2 <- if (n_children == 2) replace(df[['child_weight.2']], is.na(df[['child_weight.2']]), 0) else NULL
    sm <- sm + paid_child_sums(P, n_children, w1, w2)
    sh <- get_sector_hours_matrices(n_children)
    qd <- qd + colSums(w1 * (P %*% sh$H1))
    if (n_children == 2) qd <- qd + colSums(w2 * (P %*% sh$H2))
    paid_lab <- SECTOR_MAPPING$ecec_type[PAID_CENTRE_SECTORS]
    # Drawn: expanded rows, per-draw weights
    x1 <- dfx[['child_weight.1']]
    dr['paid'] <- dr['paid'] + sum(x1 * (dfx[['ecec_type.1']] %in% paid_lab))
    dr['all']  <- dr['all'] + sum(x1)
    if (n_children == 2) {
      x2 <- replace(dfx[['child_weight.2']], is.na(dfx[['child_weight.2']]), 0)
      dr['paid'] <- dr['paid'] + sum(x2 * (dfx[['ecec_type.2']] %in% paid_lab))
      dr['all']  <- dr['all'] + sum(x2)
    }
  }

  rows[[st]] <- tibble(
    geo = st, alpha_price = MODE, run = DRIFT_RUN,
    paid_2019 = y19['paid'], children_2019 = y19['all'], share_2019 = y19['paid'] / y19['all'],
    paid_2026_smooth = sm['paid'], children_2026_smooth = sm['all'],
    share_2026_smooth = sm['paid'] / sm['all'],
    paid_2026_drawn = dr['paid'], children_2026_drawn = dr['all'],
    share_2026_drawn = dr['paid'] / dr['all'],
    pp_change_smooth = 100 * (sm['paid'] / sm['all'] - y19['paid'] / y19['all']),
    pp_change_drawn  = 100 * (dr['paid'] / dr['all'] - y19['paid'] / y19['all']),
    qd_rel_gap = max(abs(qd - sv$Qd) / pmax(1, sv$Qd)),
    qd_check_pass = max(abs(qd - sv$Qd) / pmax(1, sv$Qd)) <= QD_TOL)
  cat(sprintf('%s 2019 %.4f  2026 smooth %.4f drawn %.4f  Qd gap %.1e\n', st,
              y19['paid'] / y19['all'], sm['paid'] / sm['all'], dr['paid'] / dr['all'],
              rows[[st]]$qd_rel_gap))
}

detail <- bind_rows(rows)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
fwrite(detail, file.path(OUT_DIR, paste0('alpha_drift_', MODE, '.csv')))

summ <- detail %>%
  filter(geo != 'NYC') %>%
  summarise(alpha_price = MODE, run = DRIFT_RUN, n_geos = n(),
            across(c(pp_change_smooth, pp_change_drawn),
                   list(mean_equal = mean, mean_abs_equal = ~ mean(abs(.x)),
                        min = min, max = max,
                        mean_pop = ~ weighted.mean(.x, children_2019),
                        mean_abs_pop = ~ weighted.mean(abs(.x), children_2019))),
            geo_min_smooth = geo[which.min(pp_change_smooth)],
            geo_max_smooth = geo[which.max(pp_change_smooth)],
            max_qd_rel_gap = max(qd_rel_gap),
            all_qd_check_pass = all(qd_check_pass))
fwrite(summ, file.path(OUT_DIR, paste0('alpha_drift_', MODE, '_summary.csv')))
cat('wrote', file.path(OUT_DIR, paste0('alpha_drift_', MODE, '*.csv')), '\n')
