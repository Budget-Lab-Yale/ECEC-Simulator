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
# Uses:
#   - density-adjusted paid-centre tier line (compute_tier_thresholds(), 1a;
#     NSECE community density only, no ACS data) -- switch on only
#
# Switch: env var ECEC_URBAN_DENSITY (config.R ->
# urban_density_setting), '0' default, '1' on -- state runs. Off = main: no
# region or density in tier line -- national age bin median
# (compute_tier_thresholds_age_bin()). On = all state machinery: region x
# density x age tier line cells, RPP income ranks. Setting recorded in every calibration-interface stage (1a, 1b
# markers; price forest rds; alpha rds) -- 2a, 3a, compute_substate_alpha.R
# stop on mismatch (check_urban_density_built()), as income-rank scheme.
#------------------------------------------------------------------------------

URBAN_DENSITY_SETTINGS <- c('0', '1')




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
