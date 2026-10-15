#------------------------------------------------------------------------------
# 01_nsch_participation_rate.R -- non-parental rate, its standard error:
# build's one soft input. load_spec() (00_build_demand_targets.R) reads
# BASE_WAVE row live from out/nsch_participation_rate_base.csv -- no copy in
# states.R to go stale.
#
# BASE = 2016-19 POOL (2026-10-06), not 2019 alone. One wave is half noise:
# state 2019 rates spread 7.8 pts (SD), median design SE 5.8 pts, so only
# 46% of observed spread is real. Pool: median SE 2.8, 86% real. Waves
# 2016-19 flat (state mean 54.0, 54.5, 54.2, 54.9%) -- pooling moves level
# little (54.4 vs 54.9). 2020-22 depressed by COVID (48.2, 48.7, 51.7);
# 2023 on drops k6q20. No pre-COVID window centred on 2019 exists.
#
# SE USES THE SURVEY DESIGN, not Kish's effective sample size. Census
# documents NSCH as stratified address sample, instructs analysts to declare
# stratum, household, child weight FWC:
#   https://www.census.gov/programs-surveys/nsch/technical-documentation/methodology/2019.html
# Kish treats sample as unequally weighted SRS -- captures weighting loss,
# never stratification gain: wrong estimator, not conservative one, can read
# high or low. Both printed.
#
# State a DOMAIN, not separate survey: design declared over every record,
# then subset. Subsetting data first drops strata domain doesn't reach,
# understating variance.
#
# Usage: Rscript state_data/code/01_nsch_participation_rate.R
#   prints table, writes out/nsch_participation_rate_by_wave.csv,
#   out/nsch_participation_rate_base.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(haven); library(survey)
})

# Not 00_build_demand_targets.R -- it reads this script's output. Geography ->
# state FIPS: every state + DC, simulator's own table. NYC has no NSCH records
# of its own, not a row here -- nsch_rate_of() (00_build_demand_targets.R)
# scales NY's row for it.
source('state_data/code/shared_setup.R')
source('src/shared_functions/constants.R')
GEO_FIPS <- STATE_POSTAL_TO_FIPS

BASE_WAVE <- '2016-19'              # pool the build uses (header, BASE)
WAVES     <- 2016:2022              # 2023 drops k6q20

# Pools; BASE_WAVE's is the build's, rest reference only. 2020-22 fielded
# through / after pandemic.
POOLS <- list('2018-19' = c(2018, 2019), '2019-20' = c(2019, 2020),
              '2018-20' = c(2018, 2019, 2020),
              '2016-19' = 2016:2019,
              '2018-19+22' = c(2018, 2019, 2022),
              '2017-19+22' = c(2017, 2018, 2019, 2022))

# Lonely strata. FIPSST x STRATUM gives two strata per state, both populated
# in every wave -- no stratum lonely, option never fires. Set so a thinner
# extract fails loudly, not silently.
options(survey.lonely.psu = 'fail')



nsch_wave <- function(year) {

  #----------------------------------------------------------------------------
  # One wave, cut to columns rate needs.
  #
  # Params:
  #   - year (int)
  #
  # Returns: (data.frame) fipsst, sc_age_years, k6q20, fwc, stratum, hhid, year
  #----------------------------------------------------------------------------

  f <- file.path(STATE_DATA_ROOT, 'raw', 'US',
                 sprintf('US_NSCH__child_health_survey_topical__%d.dta', year))
  if (!file.exists(f)) stop('missing NSCH wave: ', f)

  d <- haven::read_dta(f)
  d <- as.data.frame(lapply(d, function(x)
         if (inherits(x, 'haven_labelled')) as.numeric(x) else x))

  need <- c('fipsst', 'sc_age_years', 'k6q20', 'fwc', 'stratum', 'hhid')
  miss <- setdiff(need, names(d))
  if (length(miss)) stop(year, ': NSCH extract lacks ', paste(miss, collapse = ', '))

  d$year <- year
  # Labels only -- stratum numeric some waves, character ('2A') others
  d$stratum <- as.character(d$stratum)
  d$hhid    <- as.character(d$hhid)
  d[, c(need, 'year')]
}



nsch_design <- function(d) {

  #----------------------------------------------------------------------------
  # Sample design over EVERY record of one wave.
  #
  # Strata = FIPSST x STRATUM: NSCH stratifies by state, splits each state's
  # frame into stratum 1 (address flagged as likely to hold a child), 2A
  # (rest) -- very different weights, whole reason design matters.
  #
  # HHID = primary sampling unit. NSCH samples ONE child per responding
  # household, so hhid unique per row here, clustering term contributes
  # nothing -- declared for an extract holding siblings.
  #
  # No finite population correction: sampling fractions tiny, fpc would only
  # shrink errors.
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
  # Non-parental rate for one state, children 0-4. k6q20: 1 yes, 2 no. Child
  # outside domain keeps record, weight, enters design w/ NA indicator --
  # domain estimate, not subsample.
  #
  # Params:
  #   - des (survey.design): whole wave(s); fips (int)
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
  # Same estimate over stacked waves. Year joins stratum, so each wave keeps
  # own design -- pool a stratified estimate, not a pile of records. Design
  # rebuilt per call, as at HEAD -- keeps HEAD's float order.
  #
  # Params:
  #   - waves (list of data.frame); fips (int)
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
  # Entry point -- rate, design SE, Kish SE per geography for each wave in
  # WAVES, each pool in POOLS; prints table, writes
  # out/nsch_participation_rate_by_wave.csv,
  # out/nsch_participation_rate_base.csv.
  #
  # Params:
  #   - args (chr): CLI args, unused
  #
  # Returns: (tibble) single-wave rows then pooled rows, invisibly
  #----------------------------------------------------------------------------

  waves <- setNames(lapply(WAVES, nsch_wave), WAVES)
  desg  <- lapply(waves, nsch_design)

  # Rows geography-major, wave-minor; estimate(w, fips) gives rate_for() list.
  rows_for <- function(keys, estimate) {
    dplyr::bind_rows(lapply(names(GEO_FIPS), function(g) {
      dplyr::bind_rows(lapply(keys, function(w) {
        r <- estimate(w, GEO_FIPS[[g]])
        tibble::tibble(geo = g, wave = w, records = r$n,
                       effective = round(r$n_eff), rate = r$rate,
                       se_design = r$se_design, se_kish = r$se_kish)
      }))
    }))
  }
  all <- dplyr::bind_rows(
    rows_for(names(desg), function(w, f) rate_for(desg[[w]], f)),
    rows_for(names(POOLS), function(w, f) pooled_for(waves[as.character(POOLS[[w]])], f)))

  cat('\nNSCH k6q20, children 0-4, child-weighted. Rate and both errors in\n')
  cat('percentage points. se_design declares FIPSST x STRATUM, HHID, FWC.\n\n')
  show_df(all %>% dplyr::mutate(rate      = round(100 * rate, 1),
                                se_design = round(100 * se_design, 2),
                                se_kish   = round(100 * se_kish, 2)))

  dir.create(file.path(STATE_DATA_ROOT, 'out'), showWarnings = FALSE,
             recursive = TRUE)
  f <- file.path(STATE_DATA_ROOT, 'out', 'nsch_participation_rate_by_wave.csv')
  readr::write_csv(all, f)
  cat('\nwrote ', f, '\n', sep = '')

  # BASE_WAVE only, full precision, two columns load_spec() reads.
  base <- all %>%
    dplyr::filter(wave == BASE_WAVE) %>%
    dplyr::select(geo, rate, se_design)
  fb <- file.path(STATE_DATA_ROOT, 'out', 'nsch_participation_rate_base.csv')
  readr::write_csv(base, fb)
  cat('wrote ', fb, '\n', sep = '')

  invisible(all)
}


if (sys.nframe() == 0) main()
