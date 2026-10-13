#------------------------------------------------------------------------------
# resolve_regular_care.R
#
# Which NSECE child x provider arrangements count as regular care, w/ what
# weekly hours (2026-10-12). Two rules:
#   1. Regularity -- HH9_REGCARE 1 counts, 0 doesn't. Undetermined (-1 DK /
#      refused, -2 unable to determine) -> NSECE's own regular-care test
#      (HHC9_NPC_USEREGPROVIDER): counts when HH9_TYPEOFCARE_AGG in 1-5.
#      Before: -1 / -2 rows label NA in 1a, Parent Only in 02. Type gate on
#      every row (2026-10-12): only TOC 1-5 counts as non-parental care,
#      NSECE's own definition -- TOC 7 (irregular), 8 (type unknown), other
#      codes, NA never regular, whatever REGCARE says. TOC 6 (K-8) w/
#      REGCARE 1 still regular -> K-8 label, dropped downstream as before.
#      Before: TOC 8 w/ REGCARE 1 counted Other (35 children 0-4, 0.57%
#      weighted, in care through TOC 8 alone).
#   2. Hours -- HH9_HRS_WEEK_PROV is reference-week calendar hours. Regular
#      provider w/ zero / missing reference-week hours (regular but not last
#      week, last-week provider w/ no calendar hours for child, calendar
#      missing) still regular care: hours drawn from weighted donor
#      distribution (REGCARE = 1 rows w/ hours > 0, child weights) in same
#      TOC group x age cell -- keyed uniform on (hh_id, child_id,
#      provider_id), seed REGULAR_HOURS_SEED, weighted donor quantile at it.
#      Keeps donor pt / ft mix (2026-10-12; cell weighted median before --
#      every imputed row in cell same hours, e.g. individual 0-1 donors
#      36.9% pt, all 374 imputed rows 30 hrs). Before both: such children
#      Parent Only (6.7% of children 0-4).
#
# Called from: 1a_nsece_processing.R (labels -> assign_primary_care()),
# state_data/code/02_derive_nsece_constants.R (constants). One function ->
# both count same arrangements w/ same hours, child for child. Both call it
# NSECE_MI_DRAWS times, draw m seed nsece_mi_seed(REGULAR_HOURS_SEED, m) --
# hours averaged over draws (nsece_mi_draws.R, 2026-10-12). Needs
# keyed_hash_2_32(), keyed_unif_2_32() (compute_price_wedge_cache.R).
#------------------------------------------------------------------------------

# Dedicated hours-draw seed -- keyed hash, no RNG stream, global RNG untouched
REGULAR_HOURS_SEED <- 20261013L



regular_care_toc_group <- function(type_raw) {

  #----------------------------------------------------------------------------
  # HH9_TYPEOFCARE_AGG -> hours imputation group. TOC only (not CARELOC /
  # PRIORREL) -- same group in 1a, 02 whatever their kind coding.
  #
  # Params:
  #   - type_raw (num vec): HH9_TYPEOFCARE_AGG
  #
  # Returns: (chr vec) 'center' (4), 'individual' (1-3), 'k8' (6), 'other'
  #   (5, 7, 8, other codes, NA -- only 5 ever regular, regular_care_flag())
  #----------------------------------------------------------------------------

  case_when(
    type_raw %in% 4     ~ 'center',
    type_raw %in% 1:3   ~ 'individual',
    type_raw %in% 6     ~ 'k8',
    TRUE                ~ 'other'
  )
}



regular_care_flag <- function(regcare, type_raw) {

  #----------------------------------------------------------------------------
  # Regularity of one arrangement. REGCARE 1 -> 1 when TOC 1-6 (6 = K-8,
  # labelled, dropped downstream), else 0; -1 / -2 -> NSECE test (TOC 1-5
  # -> 1, else 0); 0, other codes, NA -> 0. TOC 7, 8, NA never regular --
  # NSECE's regular care = TOC 1-5 only (2026-10-12; TOC 8 w/ REGCARE 1
  # regular before).
  #
  # Params:
  #   - regcare (num vec): raw HH9_REGCARE
  #   - type_raw (num vec): HH9_TYPEOFCARE_AGG
  #
  # Returns: (num vec) 0 / 1
  #----------------------------------------------------------------------------

  case_when(
    regcare %in% 1         ~ as.numeric(type_raw %in% 1:6),
    regcare %in% c(-1, -2) ~ as.numeric(type_raw %in% 1:5),
    TRUE                   ~ 0
  )
}



weighted_median <- function(x, w) {

  #----------------------------------------------------------------------------
  # Weighted median -- smallest x whose cumulative weight share reaches 0.5.
  # Deterministic, no interpolation.
  #
  # Params:
  #   - x (num vec): values
  #   - w (num vec): weights (> 0)
  #
  # Returns: (num) median; NA when x empty
  #----------------------------------------------------------------------------

  if (length(x) == 0) return(NA_real_)
  o <- order(x)
  x <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  x[which(cw >= 0.5 - 1e-12)[1]]
}



weighted_quantile_draw <- function(x, w, u) {

  #----------------------------------------------------------------------------
  # Inverse weighted CDF at u -- smallest x whose cumulative weight share
  # exceeds u. u ~ U[0, 1) -> P(x_k) = w_k / sum(w), donor distribution
  # kept. Deterministic given u, no interpolation.
  #
  # Params:
  #   - x (num vec): donor values
  #   - w (num vec): donor weights (> 0)
  #   - u (num vec): uniforms in [0, 1)
  #
  # Returns: (num vec) one draw per u; NA when x empty
  #----------------------------------------------------------------------------

  if (length(x) == 0) return(rep(NA_real_, length(u)))
  o <- order(x)
  x <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  x[pmin(findInterval(u, cw) + 1L, length(x))]
}



regular_hours_unif <- function(hh_id, child_id, provider_id, seed = REGULAR_HOURS_SEED) {

  #----------------------------------------------------------------------------
  # Uniform [0, 1) per child x provider arrangement: keyed_hash_2_32() of
  # (hh_id, child_id), then keyed_unif_2_32() of (hash, provider_id). Same
  # value in any row order, subset; no RNG stream.
  #
  # Params:
  #   - hh_id (num vec): household ids (non-negative integers)
  #   - child_id (num vec): child ids, same length
  #   - provider_id (num vec): provider ids, same length
  #   - seed (int): stream seed
  #
  # Returns: (dbl vec) in [0, 1), one per arrangement
  #----------------------------------------------------------------------------

  h <- keyed_hash_2_32(hh_id, child_id, seed, 'regular_hours_unif')
  keyed_unif_2_32(h, provider_id, seed)
}



resolve_regular_care <- function(arr, seed = REGULAR_HOURS_SEED) {

  #----------------------------------------------------------------------------
  # Applies both rules to child x provider rows (HH9_CHILDREN = 1). Donors
  # for hours: REGCARE = 1 w/ TOC 1-6 (regular), reported hours > 0,
  # positive weight, known age.
  # Cell TOC group x age cell (impute_pay_age_cell()); empty cell -> TOC
  # group over all ages. Imputed row's hours: weighted donor quantile at
  # regular_hours_unif() -- independent of row order, global RNG.
  #
  # Params:
  #   - arr (df): hh_id, child_id, provider_id, regcare (num: raw
  #       HH9_REGCARE), type_raw (num: HH9_TYPEOFCARE_AGG), hours (num: raw
  #       HH9_HRS_WEEK_PROV; negative / NA = missing), age (num, years),
  #       child_weight (num)
  #   - seed (int): hours-draw seed
  #
  # Returns: (list)
  #   - arr (tibble): input w/ regular (num 0 / 1), hours (effective: reported
  #       when > 0 or row not regular, else imputed), hours_reported (num; NA
  #       when missing), hours_imputed (lgl), regular_by_type (lgl: REGCARE
  #       -1 / -2 row counted regular by TOC test)
  #   - cells (tibble): toc_group, age_cell, median_hours (donor weighted
  #       median, diagnostic only -- not imputed value), n (raw donor
  #       rows), wt (weighted donor children), pt_share (weighted donor share
  #       < 20 hrs)
  #----------------------------------------------------------------------------

  arr <- arr %>%
    mutate(
      hours_reported  = if_else(!is.na(hours) & hours >= 0, as.numeric(hours), NA_real_),
      regular         = regular_care_flag(regcare, type_raw),
      regular_by_type = !(regcare %in% c(0, 1)) & regular == 1,
      .toc            = regular_care_toc_group(type_raw),
      .cell           = impute_pay_age_cell(age),
      .wt             = if_else(!is.na(child_weight) & child_weight > 0, child_weight, 0)
    )

  # Donors regular rows only -- TOC 7 / 8 REGCARE 1 rows out (not care)
  donors <- arr %>%
    filter(regcare %in% 1, regular == 1, !is.na(hours_reported), hours_reported > 0,
           !is.na(.cell), .wt > 0)
  cells <- donors %>%
    group_by(toc_group = .toc, age_cell = .cell) %>%
    summarise(median_hours = weighted_median(hours_reported, .wt), n = n(),
              wt = sum(.wt), pt_share = sum(.wt * (hours_reported < 20)) / sum(.wt),
              .groups = 'drop')
  arr <- arr %>%
    mutate(hours_imputed = regular == 1 & (is.na(hours_reported) | hours_reported == 0),
           .draw = NA_real_)

  # Donor pool per imputed row: own cell when it has donors, else TOC group
  # over all ages; none -> NA hours
  imp_i <- which(arr$hours_imputed)
  if (length(imp_i) > 0) {
    u <- regular_hours_unif(arr$hh_id[imp_i], arr$child_id[imp_i],
                            arr$provider_id[imp_i], seed)
    toc <- arr$.toc[imp_i]
    cell <- arr$.cell[imp_i]
    in_cell <- paste(toc, cell) %in% paste(cells$toc_group, cells$age_cell)
    pool <- if_else(in_cell, paste('cell', toc, cell), paste('pooled', toc))
    for (p in unique(pool)) {
      j <- pool == p
      dn <- if (in_cell[j][1]) donors[donors$.toc == toc[j][1] & donors$.cell %in% cell[j][1], ]
            else donors[donors$.toc == toc[j][1], ]
      arr$.draw[imp_i[j]] <- weighted_quantile_draw(dn$hours_reported, dn$.wt, u[j])
    }
  }

  arr <- arr %>%
    mutate(hours = if_else(hours_imputed, .draw, hours_reported)) %>%
    select(-.toc, -.cell, -.wt, -.draw)

  list(arr = arr, cells = cells)
}
