#------------------------------------------------------------------------------
# 03_national_sensitivity.R -- the build again, w/ national NSECE constants.
#
# The regional build is the default and writes out/<GEO>.csv. This reruns every
# geography two more ways, at the NSCH rate and at +/- 1.96 standard errors:
#
#   hybrid     regional shares (formal, centre-home, informal mix) w/ NATIONAL
#              part-time shares -- same children per type, national hours per
#              child on the simulator's grid
#   national   every NSECE constant national -- shares and hours
#
# It writes out/national_sensitivity.csv only; the simulator output files stay
# untouched.
#
# The pooled national shares come from 02_nsece_constants.R, via
# out/nsece_universe.csv. The regional run must reproduce the committed
# out/<GEO>_interval.csv exactly, or this stops.
#
# Usage:
#   Rscript state_data/03_national_sensitivity.R
#------------------------------------------------------------------------------

source('state_data/00_main.R')     # main() is guarded; this only loads

# THE SAME UNIVERSE AS THE BUILD. 00_main.R reads universe A, so the national
# arm must too -- otherwise this run differs from the default in two ways at
# once, geography and universe, and the difference it prints is not the
# geography difference it reports. It read universe B until 2026-09-23.
UNIVERSE_A <- 'A: NSECE, any regular care'

derived  <- readr::read_csv(file.path(STATE_DATA_ROOT, 'out', 'nsece_universe.csv'),
                            show_col_types = FALSE)
national <- derived[derived$universe == UNIVERSE_A & derived$key == 'national', ]



national_value <- function(quantity) {

  #----------------------------------------------------------------------------
  # One pooled national share from out/nsece_universe.csv.
  #
  # Params:
  #   - quantity (chr): 'formal_share' or 'centre_home_split'
  #
  # Returns: (num)
  #----------------------------------------------------------------------------

  x <- national$value[national$quantity == quantity]
  if (length(x) != 1L || !is.finite(x)) {
    stop('missing national ', quantity, ' in out/nsece_universe.csv -- ',
         'rerun 02_nsece_constants.R')
  }
  x
}

FORMAL_SHARE_NATIONAL <- national_value('formal_share')
CENTRE_SHARE_NATIONAL <- national_value('centre_home_split')



regional_hours <- function(spec) {

  #----------------------------------------------------------------------------
  # The default build, quietly.
  #
  # Params:
  #   - spec (list)
  #
  # Returns: (named num) annual hours by care type
  #----------------------------------------------------------------------------

  invisible(utils::capture.output(x <- build_geo(spec)))
  setNames(x$annual_hours, x$ecec_type)
}



national_hours <- function(spec) {

  #----------------------------------------------------------------------------
  # The same six steps w/ national NSECE constants throughout.
  #
  # Params:
  #   - spec (list)
  #
  # Returns: (named num) annual hours by care type
  #----------------------------------------------------------------------------

  total     <- round(population_0_4(spec$fips, spec$counties) *
                     spec$nonparental_rate * NSECE_UNIVERSE_RATIO)
  formal    <- total * FORMAL_SHARE_NATIONAL
  programme <- programme_children(spec)$total    # netted, as the default build
  if (programme >= formal) stop(spec$geo, ': programmes exceed national formal total')

  other    <- formal - programme
  informal <- total - formal
  children <- c('Center-Based'        = other * CENTRE_SHARE_NATIONAL,
                'Unpaid Center-Based' = programme,
                'Paid Home-Based'     = other * (1 - CENTRE_SHARE_NATIONAL),
                informal * NSECE_RESIDUAL_MIX)[ECEC_TYPES]
  children * NSECE_GRID_HOURS_NATIONAL[ECEC_TYPES]
}



hybrid_hours <- function(spec) {

  #----------------------------------------------------------------------------
  # The default build's children per type, times national weekly hours.
  #
  # Params:
  #   - spec (list)
  #
  # Returns: (named num) annual hours by care type
  #----------------------------------------------------------------------------

  invisible(utils::capture.output(x <- build_geo(spec)))
  setNames(x$children * NSECE_GRID_HOURS_NATIONAL[x$ecec_type], x$ecec_type)
}



at_rate <- function(spec, rate, fn) {
  # A bound where programmes exceed formal care is NA, as in 00_main.R's
  # interval file -- data reject that rate. Any other error propagates.
  s <- spec
  s$nonparental_rate <- rate
  tryCatch(fn(s), error = function(e) {
    if (grepl('meets or exceeds|exceed national formal', conditionMessage(e))) {
      setNames(rep(NA_real_, length(ECEC_TYPES)), ECEC_TYPES)
    } else {
      stop(e)
    }
  })
}



compare_geo <- function(geo) {

  #----------------------------------------------------------------------------
  # Both scenarios for one geography, at the rate and at +/- 1.96 SE. Stops
  # unless the regional run matches the committed interval file.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (tibble) one row per care type plus Total
  #----------------------------------------------------------------------------

  spec <- load_spec(geo)
  se   <- spec$nonparental_rate_se
  if (is.null(se) || !is.finite(se)) stop(geo, ': missing NSCH standard error')
  lo_rate <- spec$nonparental_rate - 1.96 * se
  hi_rate <- spec$nonparental_rate + 1.96 * se

  reg    <- regional_hours(spec)
  reg_lo <- at_rate(spec, lo_rate, regional_hours)
  reg_hi <- at_rate(spec, hi_rate, regional_hours)
  hyb    <- hybrid_hours(spec)
  hyb_lo <- at_rate(spec, lo_rate, hybrid_hours)
  hyb_hi <- at_rate(spec, hi_rate, hybrid_hours)
  nat    <- national_hours(spec)
  nat_lo <- at_rate(spec, lo_rate, national_hours)
  nat_hi <- at_rate(spec, hi_rate, national_hours)

  committed <- readr::read_csv(file.path(STATE_DATA_ROOT, 'out',
                                         paste0(geo, '_interval.csv')),
                               show_col_types = FALSE)
  # all-NA bound column reads as logical -- coerce before comparing
  same <- function(a, b) isTRUE(all.equal(unname(a), as.numeric(b), tolerance = 1e-6))
  if (!same(reg, committed$annual_hours) || !same(reg_lo, committed$low) ||
      !same(reg_hi, committed$high)) {
    stop(geo, ': regional run differs from out/', geo, '_interval.csv -- ',
         'rebuild w/ 00_main.R --all --interval first')
  }

  tibble::tibble(geo           = geo,
                 ecec_type     = c(ECEC_TYPES, 'Total'),
                 regional      = c(unname(reg),    sum(reg)),
                 hybrid        = c(unname(hyb),    sum(hyb)),
                 national      = c(unname(nat),    sum(nat)),
                 regional_low  = c(unname(reg_lo), sum(reg_lo)),
                 regional_high = c(unname(reg_hi), sum(reg_hi)),
                 hybrid_low    = c(unname(hyb_lo), sum(hyb_lo)),
                 hybrid_high   = c(unname(hyb_hi), sum(hyb_hi)),
                 national_low  = c(unname(nat_lo), sum(nat_lo)),
                 national_high = c(unname(nat_hi), sum(nat_hi)))
}



comparison <- dplyr::bind_rows(lapply(GEOGRAPHIES, compare_geo)) %>%
  dplyr::mutate(hybrid_change_pct = 100 * (hybrid / regional - 1),
                change_pct        = 100 * (national / regional - 1))

# Point columns always finite. A bound may be NA (infeasible rate), never
# Inf/NaN, and a present bound must bracket its point.
bounds <- as.matrix(comparison[, 6:11])
bad <- any(!is.finite(as.matrix(comparison[, 3:5]))) ||
  any(is.nan(bounds) | is.infinite(bounds)) ||
  any(comparison$hybrid_low > comparison$hybrid, na.rm = TRUE) ||
  any(comparison$hybrid_high < comparison$hybrid, na.rm = TRUE) ||
  any(comparison$regional_low > comparison$regional, na.rm = TRUE) ||
  any(comparison$regional_high < comparison$regional, na.rm = TRUE) ||
  any(comparison$national_low > comparison$national, na.rm = TRUE) ||
  any(comparison$national_high < comparison$national, na.rm = TRUE)
if (bad) stop('invalid comparison or NSCH bounds')

out <- file.path(STATE_DATA_ROOT, 'out', 'national_sensitivity.csv')
readr::write_csv(comparison, out, na = 'NA')
cat('wrote ', out, '\n', sep = '')
print(as.data.frame(comparison[comparison$ecec_type == 'Total', ]),
      row.names = FALSE)
