#------------------------------------------------------------------------------
# compute_price_wedge_cache.R
#
# Computes price heterogeneity (price_wedge) cache for all parent units: each
# unit's wedge = care type's weighted empirical NSECE wedge quantile at unit's
# keyed percentile draw. Low-price center, Other Paid: quantile from unit's
# income-quintile grid; high-price center, home: one unconditional grid.
# Also holds price_wedge_quantile_grid(), price_wedge_quintile_grids() (1a
# builds grids w/ them), price_wedge_income_pctiles() (ACS income ranks for
# quintile).
#
# Called from: 1a (run_nsece_processing, grids), 2a (run_calibration),
# 3a (initialize_simulation), compute_substate_alpha.R (geo's own wedges)
#------------------------------------------------------------------------------



PRICE_WEDGE_TYPES <- c('center_low', 'center_high', 'home', 'other_paid')

# Types w/ income-quintile grids -- household-held-out 5-fold CV (rerun
# 2026-10-13 on urbanicity tiers, national_rpp): pinball loss vs unconditional
# grid 11.3% lower pooled for center_low (4 of 5 folds, fold 2 4.5% higher),
# 8.0% for other_paid (every fold). center_high, home: ~0.5% gain, quintiles
# 1-3 < 30 training households
PRICE_WEDGE_QUINTILE_TYPES <- c('center_low', 'other_paid')

# Quintile cell w/ fewer distinct NSECE households -> unconditional grid
PRICE_WEDGE_MIN_CELL_HH <- 30L



price_wedge_quantile_grid <- function(x, w, probs = seq(0.01, 0.99, 0.01)) {

  #----------------------------------------------------------------------------
  # Weighted empirical quantiles -- inverse of weighted step CDF: q(p) =
  # smallest x w/ cumulative weight share >= p. No interpolation, every value
  # an observed wedge. Replaces price QRF (2026-10-12): household-held-out CV
  # gave negative pseudo-R2, negative pinball skill vs constant quantile for
  # every care type, income-rank scheme.
  #
  # Params:
  #   - x (num vec): observed values (NSECE price wedges)
  #   - w (num vec): weights, same length (child_weight), >= 0, sum > 0
  #   - probs (num vec): probabilities in (0, 1] (default 0.01..0.99)
  #
  # Returns: num vec, one quantile per prob, non-decreasing in probs
  #----------------------------------------------------------------------------

  if (length(x) != length(w)) {
    stop('price_wedge_quantile_grid: x has ', length(x), ' values, w ', length(w),
         '. Pass one weight per value.')
  }
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]
  w <- w[keep]
  if (length(x) == 0 || any(w < 0) || sum(w) <= 0) {
    stop('price_wedge_quantile_grid: need >= 1 non-NA value w/ non-negative weights, ',
         'positive total. Check NSECE provider rows for care type (1a SECTION 3).')
  }
  o <- order(x)
  x <- x[o]
  cum_share <- cumsum(w[o]) / sum(w)
  # Tolerance: share rounding must not skip value whose share equals p
  idx <- findInterval(probs - 1e-12, cum_share) + 1L
  x[pmin(idx, length(x))]
}



price_wedge_quintile_grids <- function(x, w, hh_id, quintile,
                                       min_hh = PRICE_WEDGE_MIN_CELL_HH) {

  #----------------------------------------------------------------------------
  # Per income quintile 1..5: weighted wedge quantile grid of quintile's rows
  # (price_wedge_quantile_grid()). Cell w/ fewer than min_hh distinct
  # households -> unconditional grid (all rows, incl. NA quintile).
  #
  # Params:
  #   - x (num vec): NSECE price wedges
  #   - w (num vec): weights (child_weight), same length
  #   - hh_id (vec): household id per row, same length (cell size counts
  #       distinct households, not provider rows)
  #   - quintile (int vec): income quintile 1-5 per row, NA = no rank (no
  #       positive income) -- row enters unconditional grid only
  #   - min_hh (int): minimum distinct households per cell
  #
  # Returns: (list) grids (list of 5 num vecs, 99 each), n_hh (int vec, 5,
  #   distinct households per cell), ess (num vec, 5, Kish effective sample
  #   size (sum w)^2 / sum w^2 over cell rows), max_w_share (num vec, 5,
  #   largest row weight / cell weight), fallback (int vec, quintiles on
  #   unconditional grid) -- ess, max_w_share diagnostics only, rule uses n_hh
  #----------------------------------------------------------------------------

  if (length(unique(c(length(x), length(w), length(hh_id), length(quintile)))) != 1) {
    stop('price_wedge_quintile_grids: x, w, hh_id, quintile need same length (',
         length(x), ', ', length(w), ', ', length(hh_id), ', ', length(quintile), ').')
  }
  if (any(!is.na(quintile) & !quintile %in% 1:5)) {
    stop('price_wedge_quintile_grids: quintile values must be 1-5 or NA.')
  }
  uncond <- price_wedge_quantile_grid(x, w)
  ok <- !is.na(x) & !is.na(w)
  n_hh <- vapply(1:5, function(q) {
    length(unique(hh_id[ok & !is.na(quintile) & quintile == q]))
  }, integer(1))
  grids <- lapply(1:5, function(q) {
    if (n_hh[q] < min_hh) return(uncond)
    i <- !is.na(quintile) & quintile == q
    price_wedge_quantile_grid(x[i], w[i])
  })
  cell_w <- lapply(1:5, function(q) w[ok & !is.na(quintile) & quintile == q])
  ess <- vapply(cell_w, function(v) if (length(v)) sum(v)^2 / sum(v^2) else 0, numeric(1))
  max_w_share <- vapply(cell_w, function(v) if (length(v)) max(v) / sum(v) else NA_real_, numeric(1))
  list(grids = grids, n_hh = n_hh, ess = ess, max_w_share = max_w_share,
       fallback = which(n_hh < min_hh))
}



price_wedge_income_pctiles <- function(households, children, geo = NULL) {

  #----------------------------------------------------------------------------
  # ACS household income percentile (1-100) for price wedge quintile, same
  # rank inputs as 1b care forest (income_rank.R): national cutpoints =
  # child-weighted quantiles of rank income over children under 5 w/
  # positive income; household binned by assign_income_pctile(). geo (e.g.
  # 'NYC'): geo's own version as 1b sub-state p0 -- cutpoints from
  # substate_rank_inputs() cut_income, geo's households evaluated at
  # eval_income (national_rpp: income / geo RPP). Tie jitter in [0, 1) keyed
  # on (hh_id, child_id), seed random_seed_base + 4 (keyed_unif_2_32()) --
  # no RNG stream, any row order gives same ranks, global RNG untouched. 1b
  # jitters w/ runif(), so household right at cutpoint may land one
  # percentile off forest's rank.
  #
  # Params:
  #   - households (df): ACS households (hh_id, income, region, statefip;
  #       countyfip for geo) -- full calibration sample, before subsample
  #   - children (df): ACS children (hh_id, child_id, age, child_weight)
  #   - geo (chr): sub-state code for geo's own version, NULL = national
  #
  # Returns: tibble (hh_id, income_pctile), one row per household w/ child
  #   under 5; income_pctile NA w/out positive income. NULL when geo given
  #   and scheme leaves geo's versions identical (national)
  #----------------------------------------------------------------------------

  hh <- households %>%
    distinct(hh_id, .keep_all = TRUE)
  young_ids <- unique(children$hh_id[children$age < 5])

  if (is.null(geo)) {
    hh <- hh %>% filter(hh_id %in% young_ids)
    ri <- income_rank_inputs(hh$income, hh$region, hh$statefip, 'acs')
    cut_income <- ri$income
    eval_income <- ri$income
    group <- ri$group
  } else {
    geo_ids <- select_geo_hh_ids(hh, geo, SUBSTATE_GEOGRAPHIES[[geo]]$statefip)
    hh <- hh %>% filter(hh_id %in% young_ids)
    si <- substate_rank_inputs(hh$income, hh$region, hh$statefip, hh$hh_id %in% geo_ids, geo)
    if (is.null(si)) return(NULL)
    cut_income <- si$cut_income
    eval_income <- si$eval_income
    group <- si$group
  }

  # Cutpoints: children under 5 w/ positive income, keyed jitter
  kids <- children %>%
    filter(age < 5) %>%
    select(hh_id, child_id, child_weight) %>%
    inner_join(tibble(hh_id = hh$hh_id, cut_income = cut_income, group = group), by = 'hh_id') %>%
    filter(!is.na(cut_income), cut_income > 0, !is.na(group))
  if (nrow(kids) == 0) {
    stop('price_wedge_income_pctiles: no ACS children under 5 w/ positive income. ',
         'Check households income / children age, child_weight.')
  }
  kids$jitter <- keyed_unif_2_32(kids$hh_id, kids$child_id, random_seed_base + 4)
  cutpoints <- lapply(split(kids, kids$group), function(d) {
    Hmisc::wtd.quantile(
      x      = d$cut_income + d$jitter,
      weight = d$child_weight,
      probs  = seq(0.01, 0.99, 0.01)
    )
  })

  pctile <- assign_income_pctile(eval_income, group, cutpoints)
  pctile[is.na(hh$income) | hh$income <= 0] <- NA_integer_
  tibble(hh_id = hh$hh_id, income_pctile = pctile)
}



compute_price_wedge_cache <- function(parent_units, children, price_quantiles, income_pctiles) {

  #----------------------------------------------------------------------------
  # Computes price heterogeneity (price_wedge) cache for all parent units.
  #
  # Draws price_wedge values ONCE at initialization -- cache persists across
  # simulation years, each household keeps stable wedge.
  #
  # Four wedges:
  #   - price_wedge.center_low: low-priced center-based care (sector 2)
  #   - price_wedge.center_high: high-priced center-based care (sector 3)
  #   - price_wedge.home: paid home-based care (sector 4)
  #   - price_wedge.other_paid: Other Paid care (non-market sector)
  #
  # Wedge for unit u, type k, clipped to [0.1, 5]:
  #   - center_low, other_paid (PRICE_WEDGE_QUINTILE_TYPES):
  #     quantile_grid_by_quintile[[k]][[q_u]][idx_u]; q_u = ceiling(income
  #     percentile / 20) of unit's household (price_wedge_income_pctiles(),
  #     same scheme, rank inputs as care forest). No percentile (no positive
  #     income, household missing from income_pctiles) -> quantile_grid[[k]]
  #   - center_high, home: quantile_grid[[k]][idx_u]
  # Grids = weighted empirical NSECE wedge quantiles at 1..99 percent (1a,
  # price_wedge_quantile_grid(), price_wedge_quintile_grids()). Same idx_u for
  # all four types: unit at 80th pct for centers also at 80th for home.
  # idx_u: hash of (hh_id, parent_unit_id) w/ seed random_seed_base + 3
  # (draw_price_pctile_idx()), no RNG stream -- given income_pctiles, unit's
  # wedge same in national, per-state caches, any row order, subsample.
  # Global RNG untouched. Only units w/ child under 5, known n_parents get
  # wedge, others NA (callers fill 1.0).
  #
  # Params:
  #   - parent_units (tibble): base parent units (hh_id, parent_unit_id, n_parents)
  #   - children (tibble): children w/ hh_id, parent_unit_id, age
  #   - price_quantiles (list): cache/price_models/price_wedge_quantiles.rds
  #       (1a) -- quantile_grid: named list (center_low, center_high, home,
  #       other_paid), each 99 non-decreasing wedges; quantile_grid_by_quintile:
  #       named list (center_low, other_paid), each 5 grids of 99;
  #       income_rank_scheme (chr)
  #   - income_pctiles (tibble): hh_id, income_pctile (1-100 or NA) --
  #       price_wedge_income_pctiles() on full calibration sample (geo's own
  #       version for sub-state run)
  #
  # Returns: tibble with (hh_id, parent_unit_id, price_wedge.center_low, price_wedge.center_high, price_wedge.home, price_wedge.other_paid)
  #----------------------------------------------------------------------------

  rebuild_msg <- 'Rerun NSECE processing (1a) to rebuild cache/price_models/price_wedge_quantiles.rds.'
  grids <- price_quantiles$quantile_grid
  if (is.null(grids) || !all(PRICE_WEDGE_TYPES %in% names(grids)) ||
      !all(vapply(grids[PRICE_WEDGE_TYPES], length, integer(1)) == 99)) {
    stop('compute_price_wedge_cache: price_quantiles lacks quantile_grid w/ 99 values for ',
         paste(PRICE_WEDGE_TYPES, collapse = ', '), '. ', rebuild_msg)
  }
  q_grids <- price_quantiles$quantile_grid_by_quintile
  if (is.null(q_grids) || !all(PRICE_WEDGE_QUINTILE_TYPES %in% names(q_grids)) ||
      !all(vapply(q_grids[PRICE_WEDGE_QUINTILE_TYPES], function(g) {
        length(g) == 5 && all(vapply(g, length, integer(1)) == 99)
      }, logical(1)))) {
    stop('compute_price_wedge_cache: price_quantiles lacks quantile_grid_by_quintile ',
         '(5 grids of 99) for ', paste(PRICE_WEDGE_QUINTILE_TYPES, collapse = ', '), '. ',
         rebuild_msg)
  }
  built_scheme <- price_quantiles$income_rank_scheme
  run_scheme <- get_income_rank_scheme()
  if (is.null(built_scheme) || !identical(built_scheme, run_scheme)) {
    stop('compute_price_wedge_cache: price wedge quintile grids built under income-rank scheme "',
         if (is.null(built_scheme)) 'unlabeled' else built_scheme, '", this run uses "',
         run_scheme, '". Rerun NSECE processing (1a) w/ ECEC_INCOME_RANK=', run_scheme,
         ', or run w/ scheme grids were built under.')
  }
  if (!is.data.frame(income_pctiles) ||
      !all(c('hh_id', 'income_pctile') %in% names(income_pctiles))) {
    stop('compute_price_wedge_cache: income_pctiles needs hh_id, income_pctile columns ',
         '(price_wedge_income_pctiles()).')
  }

  # One row per parent unit (deduplicate from employment choice expansion)
  parent_units_unique <- parent_units %>%
    distinct(hh_id, parent_unit_id, .keep_all = TRUE)

  # Units w/ child under 5
  has_young <- children %>%
    filter(age < 5) %>%
    distinct(hh_id, parent_unit_id) %>%
    mutate(has_young = TRUE)

  # Household income quintile, NA w/out percentile
  hh_quintile <- income_pctiles %>%
    distinct(hh_id, .keep_all = TRUE) %>%
    transmute(hh_id, income_quintile = as.integer(ceiling(income_pctile / 20)))

  cache_df <- parent_units_unique %>%
    select(hh_id, parent_unit_id, n_parents) %>%
    left_join(has_young, by = c('hh_id', 'parent_unit_id')) %>%
    left_join(hh_quintile, by = 'hh_id')

  complete_mask <- !is.na(cache_df$has_young) & !is.na(cache_df$n_parents)

  for (k in PRICE_WEDGE_TYPES) cache_df[[paste0('price_wedge.', k)]] <- NA_real_

  if (any(complete_mask)) {
    drawn_pctile_idx <- draw_price_pctile_idx(cache_df$hh_id[complete_mask],
                                              cache_df$parent_unit_id[complete_mask],
                                              random_seed_base + 3)
    q_u <- cache_df$income_quintile[complete_mask]
    for (k in PRICE_WEDGE_TYPES) {
      w <- grids[[k]][drawn_pctile_idx]
      if (k %in% PRICE_WEDGE_QUINTILE_TYPES) {
        for (q in 1:5) {
          i <- which(!is.na(q_u) & q_u == q)
          w[i] <- q_grids[[k]][[q]][drawn_pctile_idx[i]]
        }
      }
      cache_df[[paste0('price_wedge.', k)]][complete_mask] <- pmax(0.1, pmin(5.0, w))
    }
  }

  # Only columns needed for joining
  cache_df %>%
    select(hh_id, parent_unit_id, price_wedge.center_low, price_wedge.center_high, price_wedge.home, price_wedge.other_paid)
}



mul_mod_2_32 <- function(a, b) {

  #----------------------------------------------------------------------------
  # (a * b) mod 2^32, exact in doubles -- b split in 16-bit halves, every
  # product < 2^49.
  #
  # Params:
  #   - a (dbl): integer values in [0, 2^32)
  #   - b (dbl): integer values in [0, 2^32), recycled
  #
  # Returns: dbl vector, integer values in [0, 2^32)
  #----------------------------------------------------------------------------

  b_hi <- b %/% 65536
  b_lo <- b %% 65536
  (((a * b_hi) %% 65536) * 65536 + a * b_lo) %% 4294967296
}



xor_2_32 <- function(a, b) {

  #----------------------------------------------------------------------------
  # Bitwise XOR of unsigned 32-bit values held as doubles -- bitwXor() takes
  # signed 32-bit ints only, so XOR each 16-bit half.
  #
  # Params:
  #   - a (dbl): integer values in [0, 2^32)
  #   - b (dbl): integer values in [0, 2^32), recycled
  #
  # Returns: dbl vector, integer values in [0, 2^32)
  #----------------------------------------------------------------------------

  hi <- bitwXor(as.integer(a %/% 65536), as.integer(b %/% 65536))
  lo <- bitwXor(as.integer(a %% 65536), as.integer(b %% 65536))
  hi * 65536 + lo
}



fmix_2_32 <- function(h) {

  #----------------------------------------------------------------------------
  # MurmurHash3 32-bit finalizer (fmix32): full avalanche -- each input bit
  # flips each output bit w/ prob ~1/2.
  #
  # Params:
  #   - h (dbl): integer values in [0, 2^32)
  #
  # Returns: dbl vector, integer values in [0, 2^32)
  #----------------------------------------------------------------------------

  h <- xor_2_32(h, h %/% 65536)
  h <- mul_mod_2_32(h, 2246822507)   # 0x85ebca6b
  h <- xor_2_32(h, h %/% 8192)
  h <- mul_mod_2_32(h, 3266489909)   # 0xc2b2ae35
  xor_2_32(h, h %/% 65536)
}



draw_price_pctile_idx <- function(hh_id, parent_unit_id, seed) {

  #----------------------------------------------------------------------------
  # Price wedge quantile index (1:99) per parent unit, keyed on
  # (hh_id, parent_unit_id) + seed -- no RNG stream. Unit gets same index
  # in any table holding it: national, per-state, sub-state (NYC), any row
  # order, subsample, income-rank scheme. Global RNG untouched.
  #
  # Hash, all mod 2^32: h = fmix32(fmix32(seed * 0x9e3779b9 XOR hh_id) XOR
  # parent_unit_id * 0x27d4eb2f); idx = 1 + floor(99 h / 2^32). Uniform on
  # 1:99 up to 99 / 2^32 rounding.
  #
  # Params:
  #   - hh_id (num): household ids, non-negative integers (taken mod 2^32)
  #   - parent_unit_id (num): parent unit ids, non-negative integers, same length
  #   - seed (int): stream seed (cache: random_seed_base + 3)
  #
  # Returns: int vector, values in 1:99, one per key
  #----------------------------------------------------------------------------

  h <- keyed_hash_2_32(hh_id, parent_unit_key(parent_unit_id), seed, 'draw_price_pctile_idx')
  as.integer(1 + (h * 99) %/% 4294967296)
}



parent_unit_key <- function(parent_unit_id) {

  #----------------------------------------------------------------------------
  # Numeric hash key for parent unit id. ACS ids are text: '<hhm_id1>' (one
  # parent) or '<hhm_id1>_<hhm_id2>' (two, 1b) -> hhm_id1 x 1000 + hhm_id2
  # (0 w/out second parent). Person numbers < 1000 -> one key per unit, no
  # collision. Numeric ids pass through unchanged.
  #
  # Params:
  #   - parent_unit_id (chr | num): parent unit ids
  #
  # Returns: (num vec) non-negative integer keys; NA where id unparseable
  #   (keyed_hash_2_32() then stops)
  #----------------------------------------------------------------------------

  if (is.numeric(parent_unit_id)) return(parent_unit_id)
  parts <- strsplit(as.character(parent_unit_id), '_', fixed = TRUE)
  vapply(parts, function(p) {
    v <- suppressWarnings(as.numeric(p))
    if (length(v) < 1 || length(v) > 2 || anyNA(v) || any(v >= 1000)) return(NA_real_)
    v[1] * 1000 + if (length(v) == 2) v[2] else 0
  }, numeric(1))
}



make_parent_unit_id <- function(hhm_id1, hhm_id2) {

  #----------------------------------------------------------------------------
  # ACS parent unit id (1b): '<hhm_id1>' one parent, '<hhm_id1>_<hhm_id2>'
  # two. Lives beside parent_unit_key() -- tests check one inverts other.
  #
  # Params:
  #   - hhm_id1 (int): first parent's person number (PERNUM)
  #   - hhm_id2 (int): second parent's person number, NA w/out one
  #
  # Returns: (chr vec) parent unit ids
  #----------------------------------------------------------------------------

  dplyr::if_else(is.na(hhm_id2), as.character(hhm_id1), paste0(hhm_id1, '_', hhm_id2))
}



keyed_hash_2_32 <- function(key1, key2, seed, caller) {

  #----------------------------------------------------------------------------
  # 32-bit hash of key pair + seed, all mod 2^32: h = fmix32(fmix32(seed *
  # 0x9e3779b9 XOR key1) XOR key2 * 0x27d4eb2f). Shared by
  # draw_price_pctile_idx(), keyed_unif_2_32().
  #
  # Params:
  #   - key1 (num): non-negative integers (taken mod 2^32)
  #   - key2 (num): non-negative integers, same length
  #   - seed (int): stream seed
  #   - caller (chr): function name for error messages
  #
  # Returns: dbl vector, integer values in [0, 2^32), one per key pair
  #----------------------------------------------------------------------------

  if (length(key1) != length(key2)) {
    stop(caller, ': hh_id has ', length(key1), ' values, second key ',
         length(key2), '. Pass one pair per record.')
  }
  keys <- c(as.numeric(key1), as.numeric(key2))
  if (anyNA(keys) || any(keys < 0) || any(keys != floor(keys))) {
    stop(caller, ': keys must be non-negative integers w/out NA. ',
         'Check hh_id / parent_unit_id / child_id in input tables.')
  }
  seed_mix <- mul_mod_2_32(as.numeric(seed) %% 4294967296, 2654435769)   # 0x9e3779b9
  h <- fmix_2_32(xor_2_32(seed_mix, as.numeric(key1) %% 4294967296))
  fmix_2_32(xor_2_32(h, mul_mod_2_32(as.numeric(key2) %% 4294967296,
                                     668265263)))   # 0x27d4eb2f
}



keyed_unif_2_32 <- function(key1, key2, seed) {

  #----------------------------------------------------------------------------
  # Uniform [0, 1) per key pair, h / 2^32 (keyed_hash_2_32()) -- no RNG
  # stream, same value in any row order, subset. Income-rank tie jitter
  # (price_wedge_income_pctiles()).
  #
  # Params:
  #   - key1 (num): non-negative integers (hh_id)
  #   - key2 (num): non-negative integers, same length (child_id)
  #   - seed (int): stream seed
  #
  # Returns: dbl vector in [0, 1), one per key pair
  #----------------------------------------------------------------------------

  keyed_hash_2_32(key1, key2, seed, 'keyed_unif_2_32') / 4294967296
}
