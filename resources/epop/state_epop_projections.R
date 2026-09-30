#------------------------------------------------------------------------------
# state_epop_projections.R -- per-state epop paths for config/state/employment/.
#
# emp_projections.R builds the NATIONAL file, resources/epop/epop_projections.csv:
# 26 columns, 13 age bins x two sexes, 1994-2100, historical from Haver and
# projected on CBO LTBO labour-force participation. config/state/employment/<ST>.csv
# takes the same schema.
#
# WHY A RATIO, NOT A LEVEL. The simulator reads this for the employment-rate
# GROWTH path only -- base-year levels come from the state-filtered ACS
# (docs/state_level_analysis.md, section 8). So each state's path is
#
#     state_epop(t) = national_epop(t) x R(t)
#     R(t)          = LAUS state epop(t) / CPS national epop(t)
#
# ONLY THE CHANGE IN R MATTERS. Simulator uses growth since 2019, so a state's
# growth = national growth x R(t) / R(2019). A constant R gives national growth
# exactly; the state path adds only the state's change in position since 2019.
#
# WHY LAUS. BLS LAUS state ratios are model-based estimates that smooth CPS
# sampling noise, so single years are reliable without pooling (bls_laus_epop.R
# writes them). LAUS has no sex or age detail, so R is one number per state and
# year, applied to every bin -- each employment group moves by the same state
# adjustment. No public source projects state employment rates by sex and age.
#
# OUTSIDE 2006-2024 R is held at its nearest observed year, so a state follows
# the national trajectory with a fixed offset before LAUS data begin and after
# they end. Runs start in 2026, so they use R(2024) / R(2019).
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
LAUS      <- file.path(EPOP_DIR, 'US_BLS__laus_state_epop__2006-2024.csv')
OUT_DIR   <- file.path('config', 'state', 'employment')
DIAG      <- file.path(EPOP_DIR, 'state_epop_diagnostics.csv')

# Postal -> FIPS, from the simulator's own table.
source('src/shared_functions/constants.R')

# Every state + DC -- demand build covers all 51.
# NYC county-defined, simulator selects households by state FIPS -- absent.
STATES <- names(STATE_POSTAL_TO_FIPS)



laus_ratios <- function(laus) {

  #----------------------------------------------------------------------------
  # R(t) per state: LAUS state epop over CPS national epop, annual means.
  #
  # Params:
  #   - laus (tibble): geo, year, epop; geo 'US' is the national series
  #
  # Returns: (tibble) geo, year, ratio
  #----------------------------------------------------------------------------

  us <- laus %>% filter(geo == 'US') %>% select(year, us_epop = epop)
  laus %>%
    filter(geo != 'US') %>%
    left_join(us, by = 'year') %>%
    transmute(geo, year, ratio = epop / us_epop)
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

  for (f in c(NATIONAL, LAUS)) {
    if (!file.exists(f)) {
      cat('MISSING INPUT: ', f, '\n', sep = '')
      quit(status = 1)
    }
  }

  natl <- read_csv(NATIONAL, show_col_types = FALSE)
  r    <- laus_ratios(read_csv(LAUS, show_col_types = FALSE))

  missing <- setdiff(STATES, r$geo)
  if (length(missing)) stop('no LAUS series for: ', paste(missing, collapse = ', '))

  laus_years <- range(r$year)
  epop_cols  <- grep('^epop_[mf]_', names(natl), value = TRUE)
  diags <- list()

  for (st in STATES) {
    rs <- r %>% filter(geo == st)

    # R by simulator year, held flat outside the LAUS window.
    rr <- rs$ratio[match(natl$year, rs$year)]
    rr[natl$year < laus_years[1]] <- rs$ratio[rs$year == laus_years[1]]
    rr[natl$year > laus_years[2]] <- rs$ratio[rs$year == laus_years[2]]

    out <- natl
    for (col in epop_cols) out[[col]] <- natl[[col]] * rr

    if (!check_only) {
      dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
      write_csv(out, file.path(OUT_DIR, paste0(st, '.csv')), na = '')
    }

    diags[[st]] <- tibble::tibble(
      state      = st,
      ratio_2019 = round(rs$ratio[rs$year == 2019], 4),
      ratio_2024 = round(rs$ratio[rs$year == 2024], 4),
      adjustment = round(rs$ratio[rs$year == 2024] / rs$ratio[rs$year == 2019], 4))
  }

  dg <- bind_rows(diags)
  if (!check_only) write_csv(dg, DIAG)

  cat('== state epop, ', nrow(dg), ' states, LAUS ', laus_years[1], '-',
      laus_years[2], '\n\n', sep = '')
  print(as.data.frame(dg), row.names = FALSE)
  cat('\nadjustment = R(2024) / R(2019): the multiplier on national growth in',
      'the runs\n')
  if (!check_only) {
    cat('wrote ', OUT_DIR, '/<ST>.csv and ', DIAG, '\n', sep = '')
  }
  invisible(dg)
}



if (sys.nframe() == 0) {
  build(check_only = '--check' %in% commandArgs(TRUE))
}
