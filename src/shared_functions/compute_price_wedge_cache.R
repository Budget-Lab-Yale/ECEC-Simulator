#------------------------------------------------------------------------------
# compute_price_wedge_cache.R
#
# Computes price heterogeneity (price_wedge) cache for all parent units: each
# unit's wedge = price forest (quantile regression forest, QRF, 1a) quantile
# at unit's random percentile draw (main: set.seed + sample, one draw per unit
# shared by care types), forest features income percentile, region, n_parents,
# mean child age. Same forest for every run (restored 2026-10-14; empirical
# quantile grids replaced it 2026-10-12).
# Also holds price_wedge_income_pctiles() (ACS parent unit baseline AGI ranks,
# as original simulator; RPP-adjusted per income-rank scheme) and keyed-hash
# helpers (keyed_unif_2_32(), parent_unit_key()) -- income-rank jitter, NSECE
# label draws.
#
# Called from: 2a (run_calibration), 3a (initialize_simulation),
# compute_substate_alpha.R (geo's own wedges)
#------------------------------------------------------------------------------



PRICE_WEDGE_TYPES <- c('center_low', 'center_high', 'home', 'other_paid')

# Forest list names in cache/price_models/price_qrf_models.rds, one per wedge type
PRICE_WEDGE_MODEL_KEYS <- c(center_low = 'center_low_price', center_high = 'center_high_price',
                            home = 'home_price', other_paid = 'other_paid_price')

# Forest quantile levels -- drawn index 1..99 picks level idx / 100
PRICE_WEDGE_QUANTILE_LEVELS <- seq(0.01, 0.99, 0.01)



price_wedge_income_pctiles <- function(parent_units, households, geo = NULL) {

  #----------------------------------------------------------------------------
  # ACS parent unit AGI percentile (1-100) for price forest, as original
  # simulator (main compute_price_wedge_cache.R): unit's baseline AGI =
  # baseline_agi1 + baseline_agi2 (0 when no second parent), weight =
  # mean of per_weight1, per_weight2 (per_weight1 w/out second parent). Care
  # forest (1b) ranks household income instead. Price adjustment as scheme
  # (income_rank.R): national_rpp divides AGI by state RPP / 100, national
  # keeps nominal. National cutpoints = weighted quantiles (probs 0.01..0.99)
  # of rank AGI + keyed jitter over every unit w/ AGI; unit binned by
  # assign_income_pctile() (AGI <= 0 -> bin 1, as main's cut()). Jitter in
  # [0, 1) keyed on (hh_id, parent_unit_id), seed random_seed_base + 4
  # (keyed_unif_2_32()) -- no RNG stream, any row order gives same ranks,
  # global RNG untouched. geo (e.g. 'NYC'): geo's own version as 1b sub-state
  # p0 -- cutpoints from substate_rank_inputs() cut_income, geo's units
  # evaluated at eval_income (national_rpp: AGI / geo RPP).
  #
  # Params:
  #   - parent_units (df): ACS parent units (hh_id, parent_unit_id,
  #       baseline_agi1, baseline_agi2, per_weight1, per_weight2) -- full
  #       calibration sample, before subsample; employment-choice rows
  #       collapse to first row per unit, as main
  #   - households (df): ACS households (hh_id, region, statefip; countyfip
  #       for geo)
  #   - geo (chr): sub-state code for geo's own version, NULL = national
  #
  # Returns: tibble (hh_id, parent_unit_id, income_pctile), one row per unit;
  #   income_pctile NA when AGI or region / statefip unknown. NULL when geo
  #   given and scheme leaves geo's versions identical (national)
  #----------------------------------------------------------------------------

  need <- c('hh_id', 'parent_unit_id', 'baseline_agi1', 'baseline_agi2', 'per_weight1', 'per_weight2')
  if (!all(need %in% names(parent_units))) {
    stop('price_wedge_income_pctiles: parent_units needs columns ', paste(need, collapse = ', '), '.')
  }
  pu <- parent_units %>%
    distinct(hh_id, parent_unit_id, .keep_all = TRUE) %>%
    transmute(hh_id, parent_unit_id,
              agi    = baseline_agi1 + replace_na(baseline_agi2, 0),
              weight = (per_weight1 + coalesce(per_weight2, per_weight1)) / 2) %>%
    left_join(households %>% distinct(hh_id, .keep_all = TRUE) %>%
                select(hh_id, region, statefip, any_of('countyfip')), by = 'hh_id')

  if (is.null(geo)) {
    ri <- income_rank_inputs(pu$agi, pu$region, pu$statefip, 'acs')
    cut_income <- ri$income
    eval_income <- ri$income
    group <- ri$group
  } else {
    geo_ids <- select_geo_hh_ids(households %>% distinct(hh_id, .keep_all = TRUE), geo,
                                 SUBSTATE_GEOGRAPHIES[[geo]]$statefip)
    si <- substate_rank_inputs(pu$agi, pu$region, pu$statefip, pu$hh_id %in% geo_ids, geo)
    if (is.null(si)) return(NULL)
    cut_income <- si$cut_income
    eval_income <- si$eval_income
    group <- si$group
  }

  # Cutpoints: every unit w/ AGI and positive weight, keyed jitter
  ok <- !is.na(cut_income) & !is.na(group) & !is.na(pu$weight) & pu$weight > 0
  if (!any(ok)) {
    stop('price_wedge_income_pctiles: no ACS parent unit w/ AGI and weight. ',
         'Check parent_units baseline_agi1, per_weight1.')
  }
  jitter <- keyed_unif_2_32(pu$hh_id, parent_unit_key(pu$parent_unit_id), random_seed_base + 4)
  kk <- tibble(x = cut_income[ok] + jitter[ok], w = pu$weight[ok], group = group[ok])
  cutpoints <- lapply(split(kk, kk$group), function(d) {
    Hmisc::wtd.quantile(x = d$x, weight = d$w, probs = seq(0.01, 0.99, 0.01))
  })

  pctile <- assign_income_pctile(eval_income, group, cutpoints)
  tibble(hh_id = pu$hh_id, parent_unit_id = pu$parent_unit_id, income_pctile = pctile)
}



compute_price_wedge_cache <- function(parent_units, children, households, price_qrf_models,
                                      income_pctiles) {

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
  # Wedge for unit u, type k, clipped to [0.1, 5]: quantile idx_u / 100 of
  # type k's price forest (ranger, quantreg) at unit's features -- unit's
  # baseline AGI percentile (price_wedge_income_pctiles(), as original
  # simulator, RPP-adjusted per scheme), region, n_parents, mean age of
  # children under 5. Unit w/ NA percentile (AGI unknown) or absent from
  # income_pctiles -> no wedge. AGI <= 0 -> bin 1.
  # Same idx_u for all four types: unit at 80th pct for centers also at 80th
  # for home. idx_u: sample(1:99, replace = TRUE) in unit order after
  # set.seed(random_seed_base + 2), as main -- no keyed hash. Moves global RNG
  # state (seed set inside); wedge depends on row order, set of complete
  # units (national, per-state, sub-state caches draw separately).
  # Speed: forest predicts full 99-level grid once per distinct feature row
  # (income percentile x region x n_parents x mean child age, thousands of
  # rows), units pick their level -- not once per unit.
  # Only units w/ child under 5, known n_parents, region, percentile get
  # wedge, others NA (callers fill 1.0).
  #
  # Params:
  #   - parent_units (tibble): base parent units (hh_id, parent_unit_id, n_parents)
  #   - children (tibble): children w/ hh_id, parent_unit_id, age
  #   - households (tibble): households w/ hh_id, region
  #   - price_qrf_models (list): cache/price_models/price_qrf_models.rds (1a)
  #       -- center_low_price, center_high_price, home_price, other_paid_price
  #       (ranger forests w/ region_levels), summary_stats, income_rank_scheme
  #       (chr), urban_density (chr)
  #   - income_pctiles (tibble): hh_id, parent_unit_id, income_pctile (1-100
  #       or NA) -- price_wedge_income_pctiles() on full calibration sample
  #       (geo's own version for sub-state run)
  #
  # Returns: tibble with (hh_id, parent_unit_id, price_wedge.center_low, price_wedge.center_high, price_wedge.home, price_wedge.other_paid)
  #----------------------------------------------------------------------------

  rebuild_msg <- 'Rerun NSECE processing (1a) to rebuild cache/price_models/price_qrf_models.rds.'
  if (!all(PRICE_WEDGE_MODEL_KEYS %in% names(price_qrf_models)) ||
      !all(vapply(price_qrf_models[PRICE_WEDGE_MODEL_KEYS], inherits, logical(1), what = 'ranger'))) {
    stop('compute_price_wedge_cache: price_qrf_models lacks ranger forests ',
         paste(PRICE_WEDGE_MODEL_KEYS, collapse = ', '), '. ', rebuild_msg)
  }
  built_scheme <- price_qrf_models$income_rank_scheme
  run_scheme <- get_income_rank_scheme()
  if (is.null(built_scheme) || !identical(built_scheme, run_scheme)) {
    stop('compute_price_wedge_cache: price forest built under income-rank scheme "',
         if (is.null(built_scheme)) 'unlabeled' else built_scheme, '", this run uses "',
         run_scheme, '". Rerun NSECE processing (1a) w/ ECEC_INCOME_RANK=', run_scheme,
         ', or run w/ scheme forest was built under.')
  }
  check_urban_density_built(price_qrf_models$urban_density, 'Price forest (price_qrf_models.rds)', '1a')
  if (!is.data.frame(income_pctiles) ||
      !all(c('hh_id', 'parent_unit_id', 'income_pctile') %in% names(income_pctiles))) {
    stop('compute_price_wedge_cache: income_pctiles needs hh_id, parent_unit_id, income_pctile columns ',
         '(price_wedge_income_pctiles()).')
  }
  if (!all(c('hh_id', 'region') %in% names(households))) {
    stop('compute_price_wedge_cache: households needs hh_id, region columns.')
  }
  loadNamespace('ranger')

  # Set seed for reproducible random operations (main)
  set.seed(random_seed_base + 2)

  # One row per parent unit (deduplicate from employment choice expansion)
  parent_units_unique <- parent_units %>%
    distinct(hh_id, parent_unit_id, .keep_all = TRUE)

  # Mean child age per unit w/ child under 5
  child_ages <- children %>%
    filter(age < 5) %>%
    group_by(hh_id, parent_unit_id) %>%
    summarise(child_age = mean(age, na.rm = TRUE), .groups = 'drop')

  # Unit percentile (baseline AGI)
  unit_pctile <- income_pctiles %>%
    distinct(hh_id, parent_unit_id, .keep_all = TRUE) %>%
    transmute(hh_id, parent_unit_id, income_pctile = as.integer(income_pctile))

  region_levels <- price_qrf_models$center_low_price$region_levels
  cache_df <- parent_units_unique %>%
    select(hh_id, parent_unit_id, n_parents) %>%
    left_join(child_ages, by = c('hh_id', 'parent_unit_id')) %>%
    left_join(households %>% distinct(hh_id, .keep_all = TRUE) %>% select(hh_id, region),
              by = 'hh_id') %>%
    left_join(unit_pctile, by = c('hh_id', 'parent_unit_id')) %>%
    mutate(region_factor = factor(region, levels = region_levels))

  complete_mask <- !is.na(cache_df$income_pctile) & !is.na(cache_df$region_factor) &
    !is.na(cache_df$n_parents) & !is.na(cache_df$child_age)

  for (k in PRICE_WEDGE_TYPES) cache_df[[paste0('price_wedge.', k)]] <- NA_real_

  if (any(complete_mask)) {
    feat <- cache_df[complete_mask, ] %>%
      transmute(income_pctile, region = region_factor, n_parents, child_age)

    # Distinct feature rows -- forest predicts these only
    feat_key <- paste(feat$income_pctile, as.integer(feat$region), feat$n_parents, feat$child_age,
                      sep = '|')
    first_row <- !duplicated(feat_key)
    feat_u <- feat[first_row, ]
    row_to_u <- match(feat_key, feat_key[first_row])

    # ONE random percentile per unit, shared by all four care types (main):
    # unit at 80th pct for centers is at 80th for home
    n_complete <- sum(complete_mask)
    drawn_pctile_idx <- sample(1:99, n_complete, replace = TRUE)

    for (k in PRICE_WEDGE_TYPES) {
      # Distinct rows x 99 quantile grid, then each unit's drawn level
      q <- predict(price_qrf_models[[PRICE_WEDGE_MODEL_KEYS[[k]]]], data = feat_u,
                   type = 'quantiles', quantiles = PRICE_WEDGE_QUANTILE_LEVELS)$predictions
      w <- q[cbind(row_to_u, drawn_pctile_idx)]
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
  # keyed_unif_2_32().
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
