#------------------------------------------------------------------------------
# baseline_consistency.R -- two checks on what a state baseline reproduces,
# per geography, for one run (2026-10-19 open questions). Rebuilds alpha as
# the run did (initialize_simulation() under run's code + env), as
# alpha_drift.R does.
#
# CLUSTER ONLY. Definitions fixed before any result was looked at:
#
# A. Implied children (out: baseline_children_<mode>.csv). Re-anchoring
#   matches hours only. Per geo x target care type (6 rows, two centre tiers
#   one row): target children + part-time children from
#   config/state/demand_counts/<ST>.csv (00_build_demand_targets.R) vs
#   re-anchored 2019 children + part-time children -- p0 tilted by solved
#   deltas, child-weighted (alpha-file weights), same in_care / in_pt maps
#   adjust_alpha_for_state() prints. Unrounded. Columns: diff, pct_diff.
#
# B. Employment vs care (out: baseline_employment_<mode>.csv,
#   baseline_care_shift_<mode>.csv).
#   B1 (2019): employment rate by targeting group (pc_group) under
#     re-anchored 2019 probabilities vs forest base rate (p_employment.pt +
#     .ft) -- base rate = what simulation-year targets grow from. Weights +
#     group from solver's 2026 parent units (prepare_year_context(), weights
#     per compute_baseline_employment_rates()), same for both rates, so gap =
#     re-anchoring's effect on employment alone.
#   B2 (2026): care hours by target type + employment rate by group at saved
#     equilibrium prices (supply_2026.yaml) w/ saved employment shifts vs
#     shifts = 0 -- how far targeting moves care demand at fixed prices.
#
# Usage: as alpha_drift.R (same env, flags, scratch root):
#   Rscript state_data/cluster/baseline_consistency.R -r state_targets_all \
#     -C <calib ts> --drift-run <run ts> --drift-src-root <root> --drift-out <dir>
#------------------------------------------------------------------------------

ARGS <- commandArgs(trailingOnly = TRUE)
arg_val <- function(flag) {
  i <- which(ARGS == flag)
  if (!length(i)) stop('baseline_consistency.R: missing ', flag)
  ARGS[i + 1]
}
DRIFT_RUN <- arg_val('--drift-run')
SRC_ROOT  <- arg_val('--drift-src-root')
OUT_DIR   <- arg_val('--drift-out')
YEAR      <- 2026L
MODE      <- Sys.getenv('ECEC_STATE_ALPHA_PRICE', 'state')
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

src_est <- file.path(default_paths$roots$output, calibration_interface, 'estimation')
if (!dir.exists(src_est)) stop('Copy ', calibration_interface, '/estimation into ',
                               default_paths$roots$output, ' first.')
file.copy(src_est, output_root, recursive = TRUE)

captured <- list()
.orig_adjust <- adjust_alpha_for_state
adjust_alpha_for_state <- function(alpha_data_by_type, state_hh_ids, targets_csv_path,
                                   state_postal, ...) {
  r <- .orig_adjust(alpha_data_by_type, state_hh_ids, targets_csv_path, state_postal, ...)
  captured[[state_postal]] <<- list(res = r, alpha_data = alpha_data_by_type,
                                    hh = state_hh_ids, targets = targets_csv_path)
  r
}
sim_ctx <- initialize_simulation()
stopifnot(setequal(names(captured), names(sim_ctx$state_contexts)))

KEY <- c('hh_id', 'parent_unit_id', 'pseudofamily_id')
key_of <- function(d) {
  d <- as.data.frame(d)
  if (!'pseudofamily_id' %in% names(d)) d$pseudofamily_id <- 1L
  d$pseudofamily_id[is.na(d$pseudofamily_id)] <- 1L
  do.call(paste, c(lapply(d[KEY], as.character), sep = '\r'))
}



reanchored_2019 <- function(cap) {

  #----------------------------------------------------------------------------
  # Re-anchored 2019 probabilities per pu type (p0 x exp(deltas by choice)),
  # w/ row keys, child weights, target-type children + part-time children.
  #
  # Params: cap (list) captured re-anchoring
  # Returns: (list) by_type (P, keys, n_children), children, children_pt
  #----------------------------------------------------------------------------

  types <- names(cap$res$targets)
  pt_hours <- unname(HOURS_ANNUAL['pt'])
  in_care <- function(M) if (is.null(M)) NULL else (M > 0) * 1
  in_pt   <- function(M) if (is.null(M)) NULL else (M > 0 & abs(M - pt_hours) < 1e-6) * 1
  ch <- setNames(rep(0, length(types)), types); ch_pt <- ch
  by_type <- list()
  for (pu_type in names(cap$alpha_data)) {
    ad <- cap$alpha_data[[pu_type]]
    n_children <- PARENT_UNIT_N_CHILDREN[match(pu_type, PARENT_UNIT_NAMES)]
    in_state <- ad$row_ids$hh_id %in% cap$hh
    if (!any(in_state)) next
    map <- build_choice_hours_map(n_children, types)
    E <- exp(map$d_by_choice(cap$res$deltas))
    U <- sweep(ad$p0[in_state, , drop = FALSE], 2, E, '*')
    P <- U / rowSums(U)
    w <- ad$child_weights[in_state, , drop = FALSE]
    w1 <- replace(w[['child_weight.1']], is.na(w[['child_weight.1']]), 0)
    w2 <- if (n_children == 2) replace(w[['child_weight.2']], is.na(w[['child_weight.2']]), 0) else NULL
    ch    <- ch + weighted_child_hours(P, in_care(map$C1), in_care(map$C2), w1, w2)
    ch_pt <- ch_pt + weighted_child_hours(P, in_pt(map$C1), in_pt(map$C2), w1, w2)
    by_type[[pu_type]] <- list(P = P, keys = key_of(ad$row_ids[in_state, , drop = FALSE]),
                               n_children = n_children)
  }
  list(by_type = by_type, children = ch, children_pt = ch_pt)
}



emp_weight <- function(df) {

  #----------------------------------------------------------------------------
  # Caregiver weight, as compute_baseline_employment_rates() (3b).
  # Returns: (num vec)
  #----------------------------------------------------------------------------

  pc <- if ('per_weight_pc' %in% names(df)) df$per_weight_pc else rep(NA_real_, nrow(df))
  alt <- ifelse(df$primary_caregiver == 1, df$per_weight1, df$per_weight2)
  w <- ifelse(!is.na(pc), pc, alt)
  replace(w, is.na(w), 0)
}



group_rate <- function(emp, w, g) {
  # Weighted employment rate by targeting group. Returns: (named num vec)
  r <- tapply(emp * w, g, sum) / tapply(w, g, sum)
  r[intersect(EMPLOYMENT_TARGETING_GROUPS, names(r))]
}



st_dir <- function(st) file.path(SRC_ROOT, DRIFT_RUN, 'simulation', 'baseline', 'states', st)
binfo <- sim_ctx$baseline_info
fn_demand   <- load_demand_policy(binfo$equilibrium$policy_demand)$do_demand_policy
fn_cdctc    <- load_cdctc_policy(binfo$policy_cdctc %||% 'baseline')
fn_transfer <- if (exists('load_transfer_policy')) load_transfer_policy(binfo$policy_transfer) else NULL
SECTOR_KEYS <- c('unpaid_center_based', 'low_price_center_based',
                 'high_price_center_based', 'paid_home_based')

out_a <- list(); out_b1 <- list(); out_b2 <- list()
for (st in names(sim_ctx$state_contexts)) {
  ctx <- sim_ctx$state_contexts[[st]]
  cap <- captured[[st]]
  ra <- reanchored_2019(cap)
  types <- names(cap$res$targets)

  # A. implied children vs target counts
  tc <- read.csv(file.path('config', 'state', 'demand_counts', paste0(st, '.csv')))
  tc <- tc[match(types, tc$ecec_type), ]
  out_a[[st]] <- tibble(geo = st, ecec_type = types,
                        target_children = tc$children, model_children = unname(ra$children),
                        target_part_time = tc$part_time, model_part_time = unname(ra$children_pt)) %>%
    mutate(diff = model_children - target_children,
           pct_diff = 100 * diff / target_children,
           pt_share_target = target_part_time / target_children,
           pt_share_model = model_part_time / model_children)

  # Solver's 2026 inputs
  yc <- prepare_year_context(
    sim_base_hh = ctx$sim_base_hh, macro_projections = sim_ctx$macro_projections,
    base_supply_params = ctx$base_supply_params, base_demand_params = ctx$base_demand_params,
    baseline_info = binfo, cached_price_wedge = ctx$cached_price_wedge,
    year = YEAR, aging_base_year = acs_base_year, seed_offset = seed_offset)
  sy <- read_yaml(file.path(st_dir(st), 'supply', paste0('supply_', YEAR, '.yaml')))
  P_eq <- as.numeric(unlist(sy$equilibrium_prices[SECTOR_KEYS]))
  et <- fread(file.path(st_dir(st), 'models', 'equilibrium',
                        paste0('employment_targeting_', YEAR, '.csv')))
  shifts <- setNames(et$delta_shift, et$group)
  zero <- setNames(rep(0, length(shifts)), names(shifts))

  emp_f <- c(); emp_19 <- c(); emp_26s <- c(); emp_260 <- c(); wv <- c(); gv <- c()
  hrs_s <- setNames(rep(0, length(types)), types); hrs_0 <- hrs_s
  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]; n_children <- PARENT_UNIT_N_CHILDREN[i]
    df <- yc$parent_units_list[[pu_name]]
    if (is.null(df) || !nrow(df) || is.null(ra$by_type[[pu_name]])) next
    df <- as.data.frame(df)
    wm <- get_working_choice_mask(n_children)
    # B1: re-anchored 2019 probabilities matched to these rows
    rb <- ra$by_type[[pu_name]]
    m <- match(key_of(df), rb$keys)
    stopifnot(!anyNA(m))
    emp_19 <- c(emp_19, rowSums(rb$P[m, wm, drop = FALSE]))
    emp_f  <- c(emp_f, replace(df$p_employment.pt, is.na(df$p_employment.pt), 0) +
                       replace(df$p_employment.ft, is.na(df$p_employment.ft), 0))
    wv <- c(wv, emp_weight(df)); gv <- c(gv, df$pc_group)
    # B2: 2026 w/ and w/o shifts, saved prices
    map <- build_choice_hours_map(n_children, types)
    w1 <- replace(df$child_weight.1, is.na(df$child_weight.1), 0)
    w2 <- if (n_children == 2) replace(df$child_weight.2, is.na(df$child_weight.2), 0) else NULL
    for (s in c('with', 'without')) {
      P <- get_demand_prob_matrix(P_eq, df, n_children, yc$demand_params, pu_name,
                                  fn_demand, fn_cdctc,
                                  cpi_growth_factor = yc$supply_params$cpi_factor,
                                  employment_shifts = if (s == 'with') shifts else zero,
                                  policy_transfer = fn_transfer)
      h <- weighted_child_hours(P, map$C1, map$C2, w1, w2)
      e <- rowSums(P[, wm, drop = FALSE])
      if (s == 'with') { hrs_s <- hrs_s + h; emp_26s <- c(emp_26s, e) }
      else             { hrs_0 <- hrs_0 + h; emp_260 <- c(emp_260, e) }
    }
  }
  tgt <- setNames(et$target_rate, et$group)
  r_f <- group_rate(emp_f, wv, gv); r_19 <- group_rate(emp_19, wv, gv)
  r_s <- group_rate(emp_26s, wv, gv); r_0 <- group_rate(emp_260, wv, gv)
  g <- names(r_f)
  out_b1[[st]] <- tibble(geo = st, group = g,
                         forest_base_rate_2019 = r_f[g], reanchored_rate_2019 = r_19[g],
                         gap_2019_pp = 100 * (r_19[g] - r_f[g]),
                         target_2026 = tgt[g], rate_2026_with_shifts = r_s[g],
                         rate_2026_no_shifts = r_0[g], shift = shifts[g])
  out_b2[[st]] <- tibble(geo = st, ecec_type = types,
                         target_hours_2019 = unname(cap$res$targets),
                         hours_2026_with_shifts = unname(hrs_s),
                         hours_2026_no_shifts = unname(hrs_0)) %>%
    mutate(pct_change_from_shifts = 100 * (hours_2026_with_shifts / hours_2026_no_shifts - 1))
  cat(sprintf('%s  A max |pct| %.2f  B1 max |gap| %.2f pp  B2 max |pct| %.2f\n', st,
              max(abs(out_a[[st]]$pct_diff)), max(abs(out_b1[[st]]$gap_2019_pp)),
              max(abs(out_b2[[st]]$pct_change_from_shifts))))
}

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
fwrite(bind_rows(out_a),  file.path(OUT_DIR, paste0('baseline_children_', MODE, '.csv')))
fwrite(bind_rows(out_b1), file.path(OUT_DIR, paste0('baseline_employment_', MODE, '.csv')))
fwrite(bind_rows(out_b2), file.path(OUT_DIR, paste0('baseline_care_shift_', MODE, '.csv')))
cat('wrote baseline_{children,employment,care_shift}_', MODE, '.csv in ', OUT_DIR, '\n', sep = '')
