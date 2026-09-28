#------------------------------------------------------------------------------
# 01_nsch_rate.R -- derives the non-parental rate and its standard error.
#
# The one soft input in the build. load_spec() (00_main.R) reads both
# constants live from out/nsch_rate_base.csv -- the BASE_WAVE row of what
# this script derives -- rather than from a copy in states/<GEO>.R, so there
# is nothing to go stale between this script and the geography it feeds.
#
# THE STANDARD ERROR USES THE SURVEY DESIGN, not Kish's effective sample size.
# Census documents NSCH as stratified address sample, instructs analysts to
# declare stratum, household, and child weight FWC:
#   https://www.census.gov/programs-surveys/nsch/technical-documentation/methodology/2019.html
# Kish treats sample as unequally weighted SRS. Captures weighting loss, never
# stratification gain -- wrong estimator, not conservative one, can read high
# or low. Both appear below.
#
# State a DOMAIN, not separate survey. Design declared once over every record,
# then subset. Subsetting data first drops strata domain doesn't reach,
# understating variance.
#
# Usage:
#   Rscript state_data/code/01_nsch_rate.R    # table + out/nsch_rate.csv,
#                                              # out/nsch_rate_base.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(haven); library(survey)
})

STATE_DATA_ROOT <- 'state_data'
BASE_WAVE       <- '2019'           # single wave the build uses -- base year
WAVES           <- c(2018, 2019, 2020)

# Geography -> state FIPS: every state + DC, from the simulator's own table.
# NYC has no NSCH records of its own and takes NY's, so it is not a row here;
# states/NYC.R copies the NY constants.
source('src/shared_functions/constants.R')
GEO_FIPS <- STATE_POSTAL_TO_FIPS

# Lonely strata. Declaring FIPSST x STRATUM gives two strata per state, both
# populated in every wave, so no stratum is lonely and the option never fires.
# It is set so that a thinner extract fails loudly rather than silently.
options(survey.lonely.psu = 'fail')



nsch_wave <- function(year) {

  #----------------------------------------------------------------------------
  # One wave, cut to the columns the rate needs.
  #
  # Params:
  #   - year (int)
  #
  # Returns: (data.frame) fipsst, sc_age_years, k6q20, fwc, stratum, hhid
  #----------------------------------------------------------------------------

  f <- file.path(STATE_DATA_ROOT, 'raw',
                 sprintf('US_NSCH__child_health_survey_topical__%d.dta', year))
  if (!file.exists(f)) stop('missing NSCH wave: ', f)

  d <- haven::read_dta(f)
  d <- as.data.frame(lapply(d, function(x)
         if (inherits(x, 'haven_labelled')) as.numeric(x) else x))

  need <- c('fipsst', 'sc_age_years', 'k6q20', 'fwc', 'stratum', 'hhid')
  miss <- setdiff(need, names(d))
  if (length(miss)) stop(year, ': NSCH extract lacks ', paste(miss, collapse = ', '))

  d$year <- year
  d[, c(need, 'year')]
}



nsch_design <- function(d) {

  #----------------------------------------------------------------------------
  # Declares the sample design over EVERY record of one wave.
  #
  # Strata are FIPSST x STRATUM: NSCH stratifies by state, then splits each
  # state's frame into stratum 1 (address flagged as likely to hold a child)
  # and stratum 2A (the rest). Both carry very different weights, which is the
  # whole reason the design matters here.
  #
  # HHID is the primary sampling unit. NSCH samples ONE child per responding
  # household, so hhid is unique per row in these extracts and the clustering
  # term contributes nothing. It is declared anyway -- an extract holding
  # siblings would need it, and it costs nothing when it is degenerate.
  #
  # No finite population correction. The sampling fractions are tiny, so an
  # fpc would only shrink the errors reported here.
  #
  # Params:
  #   - d (data.frame): one wave, every state
  #
  # Returns: (survey.design)
  #----------------------------------------------------------------------------

  d$str <- interaction(d$fipsst, d$stratum, drop = TRUE)
  survey::svydesign(ids = ~hhid, strata = ~str, weights = ~fwc,
                    data = d, nest = TRUE)
}



rate_for <- function(des, fips) {

  #----------------------------------------------------------------------------
  # Non-parental rate for one state, children 0-4.
  #
  # k6q20: 1 yes, 2 no. A child outside the domain keeps its record and its
  # weight, and enters the design w/ an indicator of NA -- which is what makes
  # this a domain estimate rather than a subsample.
  #
  # Params:
  #   - des (survey.design): the whole wave
  #   - fips (int)
  #
  # Returns: (list) rate, se_design, se_kish, n, n_eff
  #----------------------------------------------------------------------------

  sub <- subset(des, fipsst == fips & sc_age_years <= 4 & k6q20 %in% c(1, 2))
  m   <- survey::svymean(~I(k6q20 == 1), sub, na.rm = TRUE)

  w <- sub$variables$fwc
  n <- length(w)
  n_eff <- sum(w)^2 / sum(w^2)
  p <- unname(coef(m)[2])

  list(rate = p, se_design = unname(survey::SE(m)[2]),
       se_kish = sqrt(p * (1 - p) / n_eff), n = n, n_eff = n_eff)
}



pooled_for <- function(waves, fips) {

  #----------------------------------------------------------------------------
  # Same estimate over several waves stacked. Year joins the stratum, so each
  # wave keeps its own design and the pool is a stratified estimate rather
  # than a pile of records.
  #
  # The build uses the single 2019 wave (BASE_WAVE), not a pool. Pools
  # printed beside it for reference only. 2020 excluded from every pool --
  # fielded through pandemic.
  #
  # Params:
  #   - waves (list of data.frame), fips (int)
  #
  # Returns: (list) as rate_for()
  #----------------------------------------------------------------------------

  d <- dplyr::bind_rows(waves)
  d$str <- interaction(d$fipsst, d$stratum, d$year, drop = TRUE)
  des <- survey::svydesign(ids = ~hhid, strata = ~str, weights = ~fwc,
                           data = d, nest = TRUE)
  rate_for(des, fips)
}



main <- function(args = commandArgs(trailingOnly = TRUE)) {

  #----------------------------------------------------------------------------
  # Entry point -- non-parental rate, design SE, Kish SE per geography for
  # single waves 2018, 2019, 2020 and pools 2018-19, 2019-20, 2018-20;
  # prints table, writes out/nsch_rate.csv and out/nsch_rate_base.csv (the
  # BASE_WAVE row only, geo + rate + se_design, what load_spec() reads).
  #
  # Params:
  #   - args (chr): CLI args, unused -- kept so main() still matches
  #     commandArgs() when run as a script
  #
  # Returns: (tibble) all single-wave, pooled rows, invisibly
  #----------------------------------------------------------------------------

  waves <- setNames(lapply(WAVES, nsch_wave), WAVES)
  desg  <- lapply(waves, nsch_design)

  ## single waves ------------------------------------------------------------
  single <- dplyr::bind_rows(lapply(names(GEO_FIPS), function(g) {
    dplyr::bind_rows(lapply(WAVES, function(y) {
      r <- rate_for(desg[[as.character(y)]], GEO_FIPS[[g]])
      tibble::tibble(geo = g, wave = as.character(y), records = r$n,
                     effective = round(r$n_eff), rate = r$rate,
                     se_design = r$se_design, se_kish = r$se_kish)
    }))
  }))

  ## pools -------------------------------------------------------------------
  pools <- list('2018-19' = c(2018, 2019), '2019-20' = c(2019, 2020),
                '2018-20' = c(2018, 2019, 2020))
  pooled <- dplyr::bind_rows(lapply(names(GEO_FIPS), function(g) {
    dplyr::bind_rows(lapply(names(pools), function(p) {
      r <- pooled_for(waves[as.character(pools[[p]])], GEO_FIPS[[g]])
      tibble::tibble(geo = g, wave = p, records = r$n,
                     effective = round(r$n_eff), rate = r$rate,
                     se_design = r$se_design, se_kish = r$se_kish)
    }))
  }))

  all <- dplyr::bind_rows(single, pooled)

  cat('\nNSCH k6q20, children 0-4, child-weighted. Rate and both errors in\n')
  cat('percentage points. se_design declares FIPSST x STRATUM, HHID, FWC.\n\n')
  print(as.data.frame(all %>% dplyr::mutate(
          rate = round(100 * rate, 1), se_design = round(100 * se_design, 2),
          se_kish = round(100 * se_kish, 2))), row.names = FALSE)

  dir.create(file.path(STATE_DATA_ROOT, 'out'), showWarnings = FALSE,
             recursive = TRUE)
  f <- file.path(STATE_DATA_ROOT, 'out', 'nsch_rate.csv')
  readr::write_csv(all, f)
  cat('\nwrote ', f, '\n', sep = '')

  # BASE_WAVE only, full precision, the two columns load_spec() reads. NYC is
  # not a row here -- it has no NSCH records of its own; load_spec() reads
  # NY's row for it, the same way states/NYC.R used to copy NY's constants.
  base <- all[all$wave == BASE_WAVE, c('geo', 'rate', 'se_design')]
  fb <- file.path(STATE_DATA_ROOT, 'out', 'nsch_rate_base.csv')
  readr::write_csv(base, fb)
  cat('wrote ', fb, '\n', sep = '')

  invisible(all)
}


if (sys.nframe() == 0) main()
