#------------------------------------------------------------------------------
# urban_density.R
#
# Community urban density (3 levels) -- NSECE HH9_COMM_URBAN_DENSITY /
# CB9_COMM_URBAN_DENSITY codes:
#   1 high density of urban population (urban share >= 0.85)
#   2 moderate (0.30 <= urban share < 0.85)
#   3 rural (urban share < 0.30)
# NSECE share (2019 household user guide, sec. 15): 2010 Census tract counts,
# tracts overlapping 2-mile circle around anchor tract's population centroid,
# weights inversely proportional to anchor-to-tract distance.
#
# ACS proxy -- PUMS has no tract, smallest geography 2010 PUMA:
#   - tract_urban_share(): NSECE measure for every 2010 tract
#   - aggregate_puma_density(): child-weighted (2010 children 0-4) share of
#     PUMA's tracts at each level
# Table built by resources/acs/puma_urban_density_2010.R. No longer read by
# the simulator (2026-10-14): density left care forest (importance
# ~0), ACS PUMA mixing removed; builder + these helpers kept.
#
# Uses:
#   - density-adjusted paid-centre tier line (compute_tier_thresholds(), 1a;
#     NSECE community density only, no ACS data) -- switch on only
#
# Switch (2026-10-14): env var ECEC_URBAN_DENSITY (config.R ->
# urban_density_setting), '0' default, '1' on -- state runs. Off = main: no
# region or density in tier line -- national age bin median
# (compute_tier_thresholds_age_bin()). On = all state machinery: region x
# density x age tier line cells, RPP income ranks. Setting recorded in every calibration-interface stage (1a, 1b
# markers; price forest rds; alpha rds) -- 2a, 3a, compute_substate_alpha.R
# stop on mismatch (check_urban_density_built()), as income-rank scheme.
#------------------------------------------------------------------------------

URBAN_DENSITY_SETTINGS <- c('0', '1')

# NSECE cuts on urban share: >= HIGH -> 1, >= MODERATE -> 2, else 3
URBAN_DENSITY_CUT_HIGH     <- 0.85
URBAN_DENSITY_CUT_MODERATE <- 0.30

# NSECE community radius (miles) around anchor tract's population centroid
URBAN_DENSITY_RADIUS_MI <- 2

# Mean Earth radius, miles (haversine)
EARTH_RADIUS_MI <- 3958.8




get_urban_density_setting <- function() {

  #----------------------------------------------------------------------------
  # Active urban-density setting -- global urban_density_setting (config.R),
  # else env var ECEC_URBAN_DENSITY, else '0'.
  #
  # Returns: (chr) '0' (off, national default) or '1' (on, state runs)
  #----------------------------------------------------------------------------

  s <- if (exists('urban_density_setting', envir = globalenv())) {
    get('urban_density_setting', envir = globalenv())
  } else {
    Sys.getenv('ECEC_URBAN_DENSITY', '0')
  }
  if (is.null(s) || s == '') s <- '0'
  if (!s %in% URBAN_DENSITY_SETTINGS) {
    stop('ECEC_URBAN_DENSITY = "', s, '" unknown. Use 0 (default) or 1.')
  }
  s
}



urban_density_enabled <- function() {

  #----------------------------------------------------------------------------
  # Urban density on? (get_urban_density_setting() == '1')
  #
  # Returns: (lgl)
  #----------------------------------------------------------------------------

  get_urban_density_setting() == '1'
}



require_urban_density_on <- function(caller) {

  #----------------------------------------------------------------------------
  # Stops unless urban density on -- state_data scripts read density / tier
  # inputs built w/ it.
  #
  # Params:
  #   - caller (chr): script name for message
  #
  # Returns: nothing; stops when setting '0'
  #----------------------------------------------------------------------------

  if (!urban_density_enabled()) {
    stop(caller, ' needs urban density on. Set ECEC_URBAN_DENSITY=1 ',
         '(state setup, state_data/README.md) and rerun.')
  }
  invisible(NULL)
}



check_urban_density_built <- function(built, what, rebuild_step) {

  #----------------------------------------------------------------------------
  # Stops when run's urban-density setting differs from setting an interface
  # piece was built under. Unlabeled piece (built before switch, 2026-10-14)
  # = '1': every interface since 2026-10-12 used density.
  #
  # Params:
  #   - built (chr): label stored w/ piece, NULL if none
  #   - what (chr): piece name for message (file path or description)
  #   - rebuild_step (chr): step to rerun, e.g. '1a'
  #
  # Returns: nothing; stops on mismatch
  #----------------------------------------------------------------------------

  run <- get_urban_density_setting()
  label <- if (is.null(built)) '1' else as.character(built)
  if (!identical(label, run)) {
    stop(what, ' built under ECEC_URBAN_DENSITY=', label,
         if (is.null(built)) ' (unlabeled: built before the switch, density on)' else '',
         ', this run uses ', run, '. Set ECEC_URBAN_DENSITY=', label,
         ', or rerun ', rebuild_step, ' and later stages under ECEC_URBAN_DENSITY=', run, '.')
  }
  invisible(NULL)
}



write_urban_density_marker <- function(data_dir, step) {

  #----------------------------------------------------------------------------
  # Records run's urban-density setting beside a stage's data files.
  #
  # Params:
  #   - data_dir (chr): estimation/data
  #   - step (chr): '1a' or '1b'
  #
  # Returns: (chr, invisible) marker path
  #----------------------------------------------------------------------------

  path <- file.path(data_dir, paste0('urban_density_', step, '.txt'))
  writeLines(get_urban_density_setting(), path)
  invisible(path)
}



check_interface_urban_density <- function(data_dir, steps = c('1a', '1b')) {

  #----------------------------------------------------------------------------
  # Stops when a stage's marker (write_urban_density_marker()) differs from
  # run's setting. Missing marker = legacy, '1' (check_urban_density_built()).
  #
  # Params:
  #   - data_dir (chr): estimation/data
  #   - steps (chr vec): stages to check
  #
  # Returns: nothing; stops on mismatch
  #----------------------------------------------------------------------------

  for (s in steps) {
    path <- file.path(data_dir, paste0('urban_density_', s, '.txt'))
    built <- if (file.exists(path)) trimws(readLines(path, n = 1, warn = FALSE)) else NULL
    check_urban_density_built(built, paste0('Calibration interface stage ', s, ' (', data_dir, ')'), s)
  }
  invisible(NULL)
}




density_from_urban_share <- function(urban_share) {

  #----------------------------------------------------------------------------
  # Urban share -> NSECE density code, NSECE cuts.
  #
  # Params:
  #   - urban_share (num vec): urban population / total population, 0-1
  #
  # Returns: (int vec) 1 high, 2 moderate, 3 rural; NA where share NA
  #----------------------------------------------------------------------------

  if (any(urban_share < 0 | urban_share > 1, na.rm = TRUE)) {
    stop('density_from_urban_share: urban share outside 0-1. Pass a share, not a percent.')
  }
  dplyr::case_when(
    urban_share >= URBAN_DENSITY_CUT_HIGH     ~ 1L,
    urban_share >= URBAN_DENSITY_CUT_MODERATE ~ 2L,
    !is.na(urban_share)                       ~ 3L
  )
}



haversine_mi <- function(lat1, lon1, lat2, lon2) {

  #----------------------------------------------------------------------------
  # Great-circle distance, miles (EARTH_RADIUS_MI).
  #
  # Params:
  #   - lat1, lon1, lat2, lon2 (num vec): decimal degrees, same length
  #
  # Returns: (num vec) distance in miles
  #----------------------------------------------------------------------------

  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * EARTH_RADIUS_MI * asin(pmin(1, sqrt(a)))
}



tract_urban_share <- function(tracts, radius_mi = URBAN_DENSITY_RADIUS_MI) {

  #----------------------------------------------------------------------------
  # NSECE community urban share per anchor tract i (user guide sec. 15):
  #
  #   share_i = sum_j w_ij U_j / sum_j w_ij P_j,  w_ij = 1 / max(d_ij, f_i)
  #
  # U, P = 2010 urban, total population; d_ij = haversine distance between
  # population centroids. Scatter set j: anchor + tracts overlapping circle
  # of radius_mi around anchor centroid. Guide gives no polygons here --
  # overlap approximated by equal-area disk: tract j in set when
  # d_ij - r_j <= radius_mi, r_j = sqrt(land area / pi). Guide silent on
  # anchor's own distance (0 -> infinite weight) -- floor f_i = 2/3 r_i,
  # mean distance from disk centre to its points; floor applies to every j.
  #
  # Params:
  #   - tracts (df): geoid (chr), lat, lon (population centroid, degrees),
  #     r_mi (equal-area radius, miles), pop, urban (num) -- one row per tract
  #   - radius_mi (num): community radius, miles
  #
  # Returns: (tibble) geoid, urban_share (NA when weighted population 0),
  #   n_scatter (tracts in community, incl. anchor)
  #----------------------------------------------------------------------------

  need <- c('geoid', 'lat', 'lon', 'r_mi', 'pop', 'urban')
  if (!all(need %in% names(tracts))) {
    stop('tract_urban_share: tracts needs columns ', paste(need, collapse = ', '), '.')
  }
  if (anyDuplicated(tracts$geoid)) stop('tract_urban_share: duplicate tract geoid.')
  if (anyNA(tracts[need])) stop('tract_urban_share: NA in tract inputs -- check centroid / area / population join.')
  if (any(tracts$urban > tracts$pop)) stop('tract_urban_share: urban population above total population.')

  dt <- data.table::as.data.table(tracts[need])
  dt[, id := .I]
  # Scatter box: reach radius_mi + r_j around scatter centroid, in degrees
  deg_mi <- EARTH_RADIUS_MI * pi / 180
  dt[, dlat := (radius_mi + r_mi) / deg_mi]
  dt[, dlon := dlat / cos(pmin(89, abs(lat) + dlat) * pi / 180)]
  sc <- dt[, .(scatter = id, lat_lo = lat - dlat, lat_hi = lat + dlat,
               lon_lo = lon - dlon, lon_hi = lon + dlon)]
  an <- dt[, .(anchor = id, alat = lat, alon = lon)]
  pairs <- sc[an, on = .(lat_lo <= alat, lat_hi >= alat, lon_lo <= alon, lon_hi >= alon),
              nomatch = NULL, allow.cartesian = TRUE, .(anchor = i.anchor, scatter = x.scatter)]

  pairs[, `:=`(d = haversine_mi(dt$lat[anchor], dt$lon[anchor], dt$lat[scatter], dt$lon[scatter]))]
  pairs <- pairs[anchor == scatter | d - dt$r_mi[scatter] <= radius_mi]
  pairs[, w := 1 / pmax(d, 2 / 3 * dt$r_mi[anchor])]
  out <- pairs[, .(num = sum(w * dt$urban[scatter]), den = sum(w * dt$pop[scatter]),
                   n_scatter = .N), by = anchor]
  # Zero-area anchor alone: floor 0, d 0 -> w Inf; share = own urban / pop
  self_inf <- out[!is.finite(num) | !is.finite(den), anchor]
  if (length(self_inf) > 0) {
    out[anchor %in% self_inf, `:=`(num = dt$urban[anchor], den = dt$pop[anchor])]
  }
  out <- out[data.table::data.table(anchor = dt$id), on = 'anchor']
  tibble::tibble(geoid = dt$geoid[out$anchor],
                 urban_share = ifelse(out$den > 0, out$num / out$den, NA_real_),
                 n_scatter = out$n_scatter)
}



aggregate_puma_density <- function(tract_density) {

  #----------------------------------------------------------------------------
  # Tract density levels -> share of PUMA's children 0-4 at each level.
  #
  # Params:
  #   - tract_density (df): statefip, puma (int), density (1-3; NA only for
  #     tracts w/out children), children (num, 2010 children 0-4) -- one
  #     row per tract
  #
  # Returns: (tibble) statefip, puma, n_tracts, children, share_1, share_2,
  #   share_3 (sum 1) -- one row per PUMA
  #----------------------------------------------------------------------------

  td <- tract_density %>%
    dplyr::mutate(statefip = as.integer(statefip), puma = as.integer(puma))
  if (anyNA(td$statefip) || anyNA(td$puma)) stop('aggregate_puma_density: tract w/out PUMA.')
  if (any(is.na(td$density) & td$children > 0)) {
    stop('aggregate_puma_density: tract w/ children but no density -- community w/out population.')
  }
  if (!all(td$density %in% c(1:3, NA))) stop('aggregate_puma_density: density codes must be 1-3.')
  out <- td %>%
    dplyr::group_by(statefip, puma) %>%
    dplyr::summarise(n_tracts = dplyr::n(),
                     share_1  = sum(children[density %in% 1L]),
                     share_2  = sum(children[density %in% 2L]),
                     share_3  = sum(children[density %in% 3L]),
                     children = sum(children),
                     .groups  = 'drop') %>%
    dplyr::mutate(dplyr::across(c(share_1, share_2, share_3), ~ .x / children)) %>%
    dplyr::select(statefip, puma, n_tracts, children, share_1, share_2, share_3) %>%
    dplyr::arrange(statefip, puma)
  if (any(out$children <= 0)) {
    stop('aggregate_puma_density: PUMA w/out children 0-4 -- no shares to draw from: ',
         paste(head(paste(out$statefip, out$puma)[out$children <= 0], 10), collapse = ', '), '.')
  }
  out
}



compute_tier_thresholds <- function(prices) {

  #----------------------------------------------------------------------------
  # Density-adjusted paid-centre tier line per region x density x age bin:
  #
  #   line(r, d, a) = m(r, a | d = 1) x M(d, a) / M(1, a)
  #
  # m = region x age median among high-density centres, M = national median
  # by density x age. Medians weighted (center_weight x enrollment_fte),
  # positive prices only. d = 1 -> regional high-density median as is.
  # National ratio, not cell median -- region x rural cells hold 5-35
  # centres.
  #
  # Params:
  #   - prices (df): region, density (1-3), age_bin, price (hourly),
  #     center_weight, enrollment_fte -- one row per centre x age bin
  #
  # Returns: (tibble) region, density, age_bin, n_centers (positive-price
  #   centres in cell), median_high (m), national_ratio (M(d)/M(1)),
  #   threshold
  #----------------------------------------------------------------------------

  p <- prices %>%
    dplyr::filter(!is.na(price), price > 0) %>%
    dplyr::mutate(w = center_weight * enrollment_fte)
  if (anyNA(p$density) || !all(p$density %in% 1:3)) {
    stop('compute_tier_thresholds: centre density codes must be 1-3 (CB9_COMM_URBAN_DENSITY).')
  }
  wmed <- function(x, w) unname(Hmisc::wtd.quantile(x, weights = w, probs = 0.5))

  base <- p %>%
    dplyr::filter(density == 1) %>%
    dplyr::group_by(region, age_bin) %>%
    dplyr::summarise(median_high = wmed(price, w), .groups = 'drop')
  nat <- p %>%
    dplyr::group_by(density, age_bin) %>%
    dplyr::summarise(nat_median = wmed(price, w), .groups = 'drop') %>%
    dplyr::group_by(age_bin) %>%
    dplyr::mutate(national_ratio = nat_median / nat_median[density == 1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(density, age_bin, national_ratio)
  counts <- p %>% dplyr::count(region, density, age_bin, name = 'n_centers')

  out <- tidyr::expand_grid(base, density = 1:3) %>%
    dplyr::left_join(nat, by = c('density', 'age_bin')) %>%
    dplyr::left_join(counts, by = c('region', 'density', 'age_bin')) %>%
    dplyr::mutate(n_centers = dplyr::coalesce(n_centers, 0L),
                  threshold = median_high * national_ratio) %>%
    dplyr::select(region, density, age_bin, n_centers, median_high, national_ratio, threshold) %>%
    dplyr::arrange(region, density, age_bin)
  if (anyNA(out$threshold)) {
    stop('compute_tier_thresholds: density x age bin w/out national median -- no positive-price centres.')
  }
  out
}



compute_tier_thresholds_age_bin <- function(prices) {

  #----------------------------------------------------------------------------
  # Paid-centre tier line w/ urban density off -- national median posted
  # price by age bin only, no region (main 1a). Median weighted
  # (center_weight x enrollment_fte), positive prices only.
  #
  # Params:
  #   - prices (df): age_bin, price (hourly), center_weight, enrollment_fte --
  #     one row per centre x age bin (other columns ignored)
  #
  # Returns: (tibble) age_bin, n_centers (positive-price centres in cell),
  #   threshold (median)
  #----------------------------------------------------------------------------

  prices %>%
    dplyr::filter(!is.na(price), price > 0) %>%
    dplyr::group_by(age_bin) %>%
    dplyr::summarise(
      n_centers = dplyr::n(),
      threshold = unname(Hmisc::wtd.quantile(x = price, weights = center_weight * enrollment_fte,
                                             probs = 0.5)),
      .groups   = 'drop'
    ) %>%
    dplyr::arrange(age_bin)
}
