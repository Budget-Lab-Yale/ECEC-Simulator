#------------------------------------------------------------------------------
# 01_nsch_rate.R -- derives the non-parental rate and its standard error.
#
# The one soft input in the build. 00_main.R reads it as two constants per
# geography, `nonparental_rate` and `nonparental_rate_se` in states/<GEO>.R.
# This script is where both come from.
#
# THE STANDARD ERROR USES THE SURVEY DESIGN, not Kish's effective sample size.
# Census documents NSCH as a stratified address sample, and instructs analysts
# to declare the stratum, the household, and the child weight FWC:
#   https://www.census.gov/programs-surveys/nsch/technical-documentation/methodology/2019.html
# Kish treats the sample as unequally weighted SRS. It captures the weighting
# loss, never the stratification gain, so it is the wrong estimator rather than
# a conservative one -- it can read high or low. Both appear below.
#
# A state is a DOMAIN, not a separate survey. The design is declared once over
# every record, then subset. Subsetting the data first drops the strata a
# domain does not reach, understating the variance.
#
# Usage:
#   Rscript state_data/01_nsch_rate.R            # table + out/nsch_rate.csv
#   Rscript state_data/01_nsch_rate.R --check    # compare to states/<GEO>.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(haven); library(survey)
})

STATE_DATA_ROOT <- 'state_data'
BASE_WAVE       <- '2018-19'        # pooled wave the build uses -- base year + year before
WAVES           <- c(2018, 2019, 2020)

# Geography -> state FIPS. NYC has no NSCH records of its own and takes NY's,
# so it is not a row here; states/NYC.R copies the NY constants.
GEO_FIPS <- c(KY = 21, NJ = 34, NY = 36, VA = 51,
              AZ =  4, CA =  6, CO =  8, CT =  9, IL = 17, IN = 18, MA = 25,
              MN = 27, NM = 35, TX = 48, VT = 50, WA = 53, WI = 55, WV = 54)

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
  # The build uses the 2018-19 pool (BASE_WAVE). 2020 excluded -- fielded
  # through pandemic. Other pools printed beside it for reference.
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

  ## --check: do states/<GEO>.R carry what this script derives? ---------------
  if ('--check' %in% args) {
    cat('\nchecking states/<GEO>.R against the ', BASE_WAVE, ' wave\n', sep = '')
    fails <- 0
    for (g in c(names(GEO_FIPS), 'NYC')) {
      src <- file.path(STATE_DATA_ROOT, 'states', paste0(g, '.R'))
      txt <- paste(readLines(src, warn = FALSE), collapse = '\n')
      got <- function(k) as.numeric(sub(
        paste0('(?s).*\\b', k, '\\s*=\\s*([0-9.]+).*'), '\\1', txt, perl = TRUE))

      src_geo <- if (g == 'NYC') 'NY' else g    # the city borrows NY's row
      ref <- all[all$geo == src_geo & all$wave == BASE_WAVE, ]
      ok_r <- abs(got('nonparental_rate')    - round(ref$rate, 4))      < 5e-5
      ok_s <- abs(got('nonparental_rate_se') - round(ref$se_design, 4)) < 5e-5
      cat(sprintf('   %-4s rate %s  se %s\n', g,
                  if (ok_r) '[ok]  ' else '[FAIL]',
                  if (ok_s) '[ok]  ' else '[FAIL]'))
      if (!ok_r) cat(sprintf('        file %.4f, derived %.4f\n',
                             got('nonparental_rate'), ref$rate))
      if (!ok_s) cat(sprintf('        file %.4f, derived %.4f\n',
                             got('nonparental_rate_se'), ref$se_design))
      fails <- fails + (!ok_r) + (!ok_s)
    }
    cat('\n', if (fails == 0) 'CONSTANTS MATCH THE SURVEY' else
        paste(fails, 'CONSTANT(S) STALE'), '\n')
    if (fails > 0) quit(status = 1)
  }

  invisible(all)
}


if (sys.nframe() == 0) main()
