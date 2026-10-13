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
#   - mix_density_predictions(): forest probabilities averaged over the
#     PUMA's three levels, weighted by shares (2026-10-13; one keyed draw
#     per household before)
# Table built by resources/acs/puma_urban_density_2010.R.
#
# Uses:
#   - density-adjusted paid-centre tier line (compute_tier_thresholds(), 1a)
#   - care-forest feature `density` (1b)
#------------------------------------------------------------------------------

# NSECE cuts on urban share: >= HIGH -> 1, >= MODERATE -> 2, else 3
URBAN_DENSITY_CUT_HIGH     <- 0.85
URBAN_DENSITY_CUT_MODERATE <- 0.30

# NSECE community radius (miles) around anchor tract's population centroid
URBAN_DENSITY_RADIUS_MI <- 2

# Mean Earth radius, miles (haversine)
EARTH_RADIUS_MI <- 3958.8




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



modal_density <- function(share_1, share_2) {

  #----------------------------------------------------------------------------
  # Level w/ largest PUMA share (tie -> denser level). Placeholder `density`
  # column of ACS rows -- forest predictions average over all three levels
  # (mix_density_predictions()), modal level never enters them.
  #
  # Params:
  #   - share_1, share_2 (num): PUMA shares at levels 1, 2, same length
  #
  # Returns: (int vec) 1 high, 2 moderate, 3 rural
  #----------------------------------------------------------------------------

  if (length(share_1) != length(share_2)) stop('modal_density: share_1, share_2 must have same length.')
  if (anyNA(share_1) || anyNA(share_2)) stop('modal_density: NA share -- household w/out PUMA match.')
  share_3 <- 1 - share_1 - share_2
  dplyr::case_when(share_1 >= share_2 & share_1 >= share_3 ~ 1L,
                   share_2 >= share_3                       ~ 2L,
                   TRUE                                     ~ 3L)
}



mix_density_predictions <- function(data, predict_fn) {

  #----------------------------------------------------------------------------
  # Care-forest class probabilities averaged over PUMA density mix:
  #
  #   P(class | X, PUMA) = sum over u of share(PUMA, u) x P(class | X, u)
  #
  # Rows w/ share_1, share_2 (ACS) -> predict_fn run once per level w/
  # `density` set to that level, weighted by shares (share_3 = 1 - share_1 -
  # share_2). No share columns (NSECE, own density) -> predict_fn(data) as is.
  # Assumes PUMA mix holds for every household in PUMA -- doesn't recover
  # within-PUMA link of density to income / family type.
  #
  # Params:
  #   - data (df): prediction frame; `density` factor levels 1:3
  #   - predict_fn (function): data frame -> probability matrix (rows =
  #       rows of data, same columns every call)
  #
  # Returns: (matrix) class probabilities, rows = rows of data
  #----------------------------------------------------------------------------

  if (!all(c('share_1', 'share_2') %in% names(data))) return(predict_fn(data))
  if (anyNA(data$share_1) || anyNA(data$share_2)) {
    stop('mix_density_predictions: NA PUMA share -- household w/out PUMA match.')
  }
  s <- cbind(data$share_1, data$share_2, pmax(0, 1 - data$share_1 - data$share_2))
  out <- NULL
  for (u in 1:3) {
    d <- data
    d$density <- factor(u, levels = 1:3)
    p <- predict_fn(d) * s[, u]
    out <- if (is.null(out)) p else out + p
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
