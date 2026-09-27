#------------------------------------------------------------------------------
# state_epop_projections.R -- per-state epop paths for config/state/employment/.
#
# emp_projections.R builds the NATIONAL file, resources/epop/epop_projections.csv:
# 26 columns, 13 age bins x two sexes, 1994-2100, historical from Haver and
# projected on CBO LTBO labour-force participation. config/state/employment/<ST>.csv
# takes the same schema. Nothing had ever been written to it.
#
# WHY A RATIO, NOT A LEVEL. The simulator reads this for the employment-rate
# GROWTH path only -- base-year levels come from the state-filtered ACS
# (docs/state_level_analysis.md, section 8). Writing raw ACS levels would also import a level
# difference that is about the survey, not the state: ACS runs 1-3 points below
# the CPS-based national series for men and above it for women, because the two
# ask about different reference periods. So each state's path is
#
#     state_epop(t) = national_epop(t) x [ ACS state(t) / ACS national(t) ]
#
# The ACS-versus-CPS level gap appears in numerator and denominator of the ratio
# and cancels. What survives is the state's position relative to the nation,
# which is the thing ACS measures well and the national file cannot carry.
#
# OUTSIDE 2006-2024 the ratio is held at its nearest observed year, so a state
# follows the national trajectory with a fixed offset before ACS begins and
# after it ends. Any other choice would be extrapolating a state trend over
# seventy years on nineteen observations.
#
# Usage:
#   Rscript resources/epop/state_epop_projections.R
#   Rscript resources/epop/state_epop_projections.R --check
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

EPOP_DIR  <- file.path('resources', 'epop')
NATIONAL  <- file.path(EPOP_DIR, 'epop_projections.csv')
ACS       <- file.path(EPOP_DIR,
                       'US_ACS__epop_by_state_sex_age__2006-2024.csv')
OUT_DIR   <- file.path('config', 'state', 'employment')
DIAG      <- file.path(EPOP_DIR, 'state_epop_diagnostics.csv')

# A state x sex x bin x year cell under this many unweighted ACS persons is not
# trusted on its own; its ratio is replaced by that state's mean ratio for the
# same sex and bin across all years. Bins like 16-17 in Vermont are thin even
# in ACS.
MIN_N <- 100

# Postal -> FIPS, from the simulator's own table.
source('src/shared_functions/constants.R')
POSTAL_TO_FIPS <- STATE_POSTAL_TO_FIPS

# Every state + DC -- demand build covers all 51. No Puerto Rico records in these ACS data.
# NYC county-defined, simulator selects households by state FIPS -- absent.
STATES <- names(POSTAL_TO_FIPS)

BINS <- c('16_17', '18_19', '20_24', '25_29', '30_34', '35_39', '40_44',
          '45_49', '50_54', '55_59', '60_64', '65plus')



acs_ratios <- function(acs) {

  #----------------------------------------------------------------------------
  # State epop over national epop, per sex x bin x year, both from ACS.
  #
  # Params:
  #   - acs (tibble): long ACS cells
  #
  # Returns: (tibble) state_fips, sex, age_bin, year, ratio, n, thin
  #----------------------------------------------------------------------------

  nat <- acs %>%
    group_by(year, sex, age_bin) %>%
    summarise(nat_epop = sum(employed_wt) / sum(pop_wt), .groups = 'drop')

  st <- acs %>%
    mutate(epop = employed_wt / pop_wt) %>%
    left_join(nat, by = c('year', 'sex', 'age_bin')) %>%
    mutate(ratio = epop / nat_epop, thin = n < MIN_N)

  # A thin cell takes that state's own mean ratio over the years where the same
  # sex and bin is not thin. If every year is thin, the ratio falls back to 1 --
  # i.e. the state follows the nation exactly for that cell, which is the
  # honest answer when ACS cannot see it.
  st %>%
    group_by(state_fips, sex, age_bin) %>%
    mutate(fallback = {
      ok <- ratio[!thin]
      if (length(ok)) mean(ok) else 1
    }) %>%
    ungroup() %>%
    mutate(ratio_used = if_else(thin, fallback, ratio))
}



build <- function(check_only = FALSE) {

  #----------------------------------------------------------------------------
  # Writes config/state/employment/<ST>.csv for every state in STATES.
  #
  # Params:
  #   - check_only (logical): TRUE skips writing <ST>.csv files, DIAG --
  #     computes, prints diagnostics only
  #
  # Returns: (tibble) diagnostics, invisibly
  #----------------------------------------------------------------------------

  for (f in c(NATIONAL, ACS)) {
    if (!file.exists(f)) {
      cat('MISSING INPUT: ', f, '\n', sep = '')
      quit(status = 1)
    }
  }

  natl <- read_csv(NATIONAL, show_col_types = FALSE)
  acs  <- read_csv(ACS, show_col_types = FALSE)

  r <- acs_ratios(acs)

  natl_long <- natl %>%
    pivot_longer(-year, names_to = 'col', values_to = 'nat') %>%
    filter(grepl('^epop_[mf]_', col)) %>%
    mutate(sex = sub('^epop_([mf])_.*$', '\\1', col),
           age_bin = sub('^epop_[mf]_', '', col)) %>%
    filter(age_bin %in% BINS)

  acs_years <- range(r$year)
  diags <- list()

  for (st in STATES) {
    fips <- POSTAL_TO_FIPS[[st]]
    rs <- r %>% filter(state_fips == fips) %>%
      select(sex, age_bin, year, ratio_used, n, thin)

    # hold the ratio flat outside the ACS window
    edge <- rs %>%
      group_by(sex, age_bin) %>%
      summarise(lo = ratio_used[which.min(year)],
                hi = ratio_used[which.max(year)], .groups = 'drop')

    out <- natl_long %>%
      left_join(rs, by = c('sex', 'age_bin', 'year')) %>%
      left_join(edge, by = c('sex', 'age_bin')) %>%
      mutate(ratio_final = case_when(
               !is.na(ratio_used)      ~ ratio_used,
               year <  acs_years[1]    ~ lo,
               year >  acs_years[2]    ~ hi,
               TRUE                    ~ 1),
             value = nat * ratio_final) %>%
      select(year, col, value) %>%
      pivot_wider(names_from = col, values_from = value)

    # The 16plus totals are not a bin; carry the national value scaled by the
    # state's population-weighted mean ratio over the bins, so the aggregate
    # stays consistent with the parts.
    for (sx in c('m', 'f')) {
      agg <- acs %>%
        filter(state_fips == fips, sex == sx) %>%
        group_by(year) %>%
        summarise(st_epop = sum(employed_wt) / sum(pop_wt), .groups = 'drop')
      nat_agg <- acs %>%
        filter(sex == sx) %>%
        group_by(year) %>%
        summarise(nat_epop = sum(employed_wt) / sum(pop_wt), .groups = 'drop')
      ag <- left_join(agg, nat_agg, by = 'year') %>%
        mutate(ratio = st_epop / nat_epop)
      lo <- ag$ratio[which.min(ag$year)]; hi <- ag$ratio[which.max(ag$year)]
      colname <- paste0('epop_', sx, '_16plus')
      natcol <- natl[[colname]]
      rr <- ag$ratio[match(natl$year, ag$year)]
      rr[is.na(rr) & natl$year < acs_years[1]] <- lo
      rr[is.na(rr) & natl$year > acs_years[2]] <- hi
      rr[is.na(rr)] <- 1
      out[[colname]] <- natcol * rr
    }

    out <- out %>% select(year, all_of(names(natl)[-1]))

    if (!check_only) {
      dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
      write_csv(out, file.path(OUT_DIR, paste0(st, '.csv')), na = '')
    }

    thin_share <- mean(rs$thin)
    d19 <- out[out$year == 2019, ]
    n19 <- natl[natl$year == 2019, ]
    diags[[st]] <- tibble::tibble(
      state = st,
      thin_cell_share = round(thin_share, 4),
      min_n = min(rs$n),
      epop_m_16plus_2019 = round(d19$epop_m_16plus, 2),
      epop_f_16plus_2019 = round(d19$epop_f_16plus, 2),
      nat_m_2019 = n19$epop_m_16plus,
      nat_f_2019 = n19$epop_f_16plus)
  }

  dg <- bind_rows(diags)
  if (!check_only) write_csv(dg, DIAG)

  cat('== state epop, ', nrow(dg), ' states, ACS ', acs_years[1], '-',
      acs_years[2], '\n\n', sep = '')
  print(as.data.frame(dg), row.names = FALSE)
  cat('\nmin-n guard: a cell under ', MIN_N,
      ' ACS persons takes that state\'s mean ratio\n', sep = '')
  if (!check_only) {
    cat('wrote ', OUT_DIR, '/<ST>.csv and ', DIAG, '\n', sep = '')
  }
  invisible(dg)
}



if (sys.nframe() == 0) {
  build(check_only = '--check' %in% commandArgs(TRUE))
}
