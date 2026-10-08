#------------------------------------------------------------------------------
# state_epop_projections.R -- per-state epop paths for config/state/employment/.
#
# emp_projections.R builds the NATIONAL file, resources/epop/epop_projections.csv:
# 26 columns, 12 age bins + 16plus x two sexes, 1994-2100, historical from
# Haver and projected on CBO LTBO labour-force participation.
# config/state/employment/<ST>.csv keeps year + the 20 cells simulator reads
# (SIM_CELLS: ages 16-59 x two sexes) + pop_<bin>: geography's ACS population
# per fine bin, both sexes (single year, clamped to ACS years) --
# get_employment_rate_growth_factor() weights fine bins into broad groups w/
# base-year row (2026-10-07; national file has none -> national macro weights).
#
# METHOD -- each geography's own ACS cell rates, carried by national growth.
# Per geography g, year t, sex x age cell c:
#
#   local_gc(y) = (employed_gc(y-1) + employed_gc(y)) /
#                 (pop_gc(y-1) + pop_gc(y))            ACS, two years pooled
#   relative_gc(y) = local_gc(y) / local_USc(y)       vs ACS nation, same cell
#   rate_gc(t)  = national_c(t) x relative_gc(a),
#                 a = t clamped to 2007..2024 (ACS years)
#
# So geography's own position against nation, measured in ACS, applied to
# national (CPS, CBO) cell rate; after 2024 position held. ACS vs CPS level
# differences cancel -- only state deviation from nation is local. Two-year
# pooling halves single-year sampling noise in small state x sex x age cells.
# (US_ACS__epop_by_state_sex_age__2006-2024.csv, acs_epop.py; NYC rows by
# STATEFIP + COUNTYFIP.)
#
# WHAT REACHES THE SIMULATOR. Simulator reads this for employment-rate GROWTH
# only -- base-year levels come from state-filtered ACS (docs/
# state_level_analysis.md, section 8). A cell's growth 2019 -> t (t > 2024) =
# national_c(t) / national_c(2019) x relative_gc(2024) / relative_gc(2019):
# national growth x change in state's position 2019-2024.
#
# Not written: 60_64, 65plus (national file blank from 2026, simulator
# ignores), 16plus (aggregate, simulator ignores).
#
# Usage:
#   Rscript resources/epop/state_epop_projections.R [--check]
#   --check: compute, print, write nothing
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

EPOP_DIR  <- file.path('resources', 'epop')
NATIONAL  <- file.path(EPOP_DIR, 'epop_projections.csv')
ACS_EPOP  <- file.path(EPOP_DIR, 'US_ACS__epop_by_state_sex_age__2006-2024.csv')
OUT_DIR   <- file.path('config', 'state', 'employment')

# STATE_POSTAL_TO_FIPS, SUBSTATE_GEOGRAPHIES, EMPLOYMENT_AGE_BINS
source('src/shared_functions/constants.R')

# Cells get_employment_rate_growth_factor() reads -- EMPLOYMENT_AGE_BINS fine
# bins, both sexes
SIM_CELLS <- as.vector(outer(c('epop_m_', 'epop_f_'),
                             unlist(EMPLOYMENT_AGE_BINS, use.names = FALSE),
                             paste0))

STATES    <- names(STATE_POSTAL_TO_FIPS)   # 50 + DC -- demand build covers all 51
SUBSTATES <- names(SUBSTATE_GEOGRAPHIES)   # NYC -- own ACS rows (COUNTYFIP)



local_rates <- function(acs) {

  #----------------------------------------------------------------------------
  # Two-year pooled ACS employment rate and population share per geography,
  # year, cell. First ACS year has no prior year -- dropped.
  #
  # Params:
  #   - acs (tibble): year, state_fips (FIPS or sub-state code), sex, age_bin,
  #     employed_wt, pop_wt
  #
  # Returns: (tibble) geo (incl. 'US' = 50 states + DC), year, col
  #          (epop_<sex>_<age_bin>), rate (percent), share (of geography's 16+
  #          population, year)
  #----------------------------------------------------------------------------

  fips_to_postal <- setNames(names(STATE_POSTAL_TO_FIPS), STATE_POSTAL_TO_FIPS)
  st <- acs %>%
    mutate(state_fips = as.character(state_fips),
           geo = ifelse(state_fips %in% SUBSTATES, state_fips,
                        unname(fips_to_postal[state_fips])),
           col = paste0('epop_', sex, '_', age_bin)) %>%
    filter(!is.na(geo))
  us <- st %>%
    filter(geo %in% STATES) %>%
    group_by(year, col) %>%
    summarise(employed_wt = sum(employed_wt), pop_wt = sum(pop_wt), .groups = 'drop') %>%
    mutate(geo = 'US')
  bind_rows(select(st, geo, year, col, employed_wt, pop_wt), us) %>%
    arrange(geo, col, year) %>%
    group_by(geo, col) %>%
    mutate(rate  = 100 * (employed_wt + lag(employed_wt)) / (pop_wt + lag(pop_wt)),
           pop2  = pop_wt + lag(pop_wt)) %>%
    ungroup() %>%
    filter(!is.na(rate)) %>%
    group_by(geo, year) %>%
    mutate(share = pop2 / sum(pop2)) %>%
    ungroup() %>%
    select(geo, year, col, rate, share)
}



build <- function(check_only = FALSE) {

  #----------------------------------------------------------------------------
  # Writes OUT_DIR/<GEO>.csv for STATES + SUBSTATES; prints diagnostics.
  #
  # Params:
  #   - check_only (logical): TRUE skips all writes
  #
  # Returns: (tibble) diagnostics, invisibly
  #----------------------------------------------------------------------------

  for (f in c(NATIONAL, ACS_EPOP)) {
    if (!file.exists(f)) {
      cat('MISSING INPUT: ', f, '\n', sep = '')
      quit(status = 1)
    }
  }

  natl <- read_csv(NATIONAL, show_col_types = FALSE)
  if (length(setdiff(SIM_CELLS, names(natl)))) stop(NATIONAL, ' lacks simulator cells')
  natl <- natl[, c('year', SIM_CELLS)]
  cell_cols <- SIM_CELLS
  loc <- local_rates(read_csv(ACS_EPOP, show_col_types = FALSE))
  acs_years <- range(loc$year)

  missing_geo <- setdiff(c(STATES, SUBSTATES), unique(loc$geo))
  if (length(missing_geo)) stop('no ACS rows for: ', paste(missing_geo, collapse = ', '),
                                ' -- rerun acs_epop.py')
  missing_cols <- setdiff(cell_cols, unique(loc$col))
  if (length(missing_cols)) stop('ACS lacks cells: ', paste(missing_cols, collapse = ', '))
  bad <- loc %>% filter(col %in% cell_cols, !is.finite(rate))
  if (nrow(bad)) stop('non-finite ACS rate in ', nrow(bad), ' cells, e.g. ',
                      bad$geo[1], ' ', bad$col[1], ' ', bad$year[1])

  anchor  <- pmin(pmax(natl$year, acs_years[1]), acs_years[2])
  diags <- list()

  # Fine-bin population weights per geography (pop_<bin>, both sexes, ACS
  # single year at anchor) -- simulator combines fine bins into broad groups w/
  # these in place of national macro weights (get_employment_rate_growth_factor())
  fips_to_postal <- setNames(names(STATE_POSTAL_TO_FIPS), STATE_POSTAL_TO_FIPS)
  fine_bins <- unlist(EMPLOYMENT_AGE_BINS, use.names = FALSE)
  pop <- read_csv(ACS_EPOP, show_col_types = FALSE) %>%
    mutate(state_fips = as.character(state_fips),
           geo = ifelse(state_fips %in% SUBSTATES, state_fips,
                        unname(fips_to_postal[state_fips]))) %>%
    filter(!is.na(geo), age_bin %in% fine_bins) %>%
    group_by(geo, year, age_bin) %>%
    summarise(pop = sum(pop_wt), .groups = 'drop')
  pop_years <- range(pop$year)
  pop_anchor <- pmin(pmax(natl$year, pop_years[1]), pop_years[2])

  lus <- filter(loc, geo == 'US')
  for (g in c(STATES, SUBSTATES)) {
    lg  <- filter(loc, geo == g)
    out <- natl
    for (col in cell_cols) {
      lc <- filter(lg, col == !!col)
      lu <- filter(lus, col == !!col)
      relative <- lc$rate[match(anchor, lc$year)] / lu$rate[match(anchor, lu$year)]
      out[[col]] <- natl[[col]] * relative
    }
    if (anyNA(out[out$year >= acs_years[1], cell_cols])) stop(g, ': missing rate in some years')
    pg <- filter(pop, geo == g)
    for (b in fine_bins) {
      pb <- filter(pg, age_bin == b)
      out[[paste0('pop_', b)]] <- pb$pop[match(pop_anchor, pb$year)]
    }
    if (anyNA(out[, paste0('pop_', fine_bins)])) stop(g, ': missing ACS population in some fine bins')

    if (!check_only) {
      dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
      write_csv(out, file.path(OUT_DIR, paste0(g, '.csv')), na = '')
    }

    # Growth 2019 -> 2026 of 16-59 rate (2024 shares), geography over nation
    young <- cell_cols
    w <- sapply(young, function(cl) filter(lg, col == cl, year == acs_years[2])$share)
    agg <- function(d, y) sum(unlist(d[d$year == y, young]) * w) / sum(w)
    diags[[g]] <- tibble::tibble(
      geo        = g,
      rate_2019  = round(agg(out, 2019), 2),
      rate_2024  = round(agg(out, 2024), 2),
      growth_vs_national_2026 =
        round((agg(out, 2026) / agg(out, 2019)) / (agg(natl, 2026) / agg(natl, 2019)), 4))
  }

  dg <- bind_rows(diags)
  cat('== state epop, ', nrow(dg), ' geographies: own ACS cell rates (two-year ',
      'pooled), national growth outside ', acs_years[1], '-', acs_years[2], '\n\n', sep = '')
  print(as.data.frame(dg), row.names = FALSE)
  cat('\ngrowth_vs_national_2026 = 16-59 rate growth 2019 -> 2026, geography',
      'over nation (2024 population shares)\n')
  if (!check_only) {
    cat('wrote ', OUT_DIR, '/<GEO>.csv\n', sep = '')
  }
  invisible(dg)
}



if (sys.nframe() == 0) {
  build(check_only = '--check' %in% commandArgs(TRUE))
}
