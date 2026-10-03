#------------------------------------------------------------------------------
# state_epop_projections.R -- per-state epop paths for config/state/employment/.
#
# emp_projections.R builds the NATIONAL file, resources/epop/epop_projections.csv:
# 26 columns, 12 age bins + 16plus x two sexes, 1994-2100, historical from
# Haver and projected on CBO LTBO labour-force participation.
# config/state/employment/<ST>.csv takes the same schema.
#
# METHOD -- composition x CBO cell rates, scaled to the national rate. Per state
# s, year t, sex x age cell c:
#
#   E_s(t)      = sum_c share_sc(t) x CBO_c(t)   rate state would have if its
#                                                 people had national cell rates
#   N(t)        = sum_c share_nc(t) x CBO_c(t)   same, national composition
#   k_s(t)      = N(t) / E_s(t)
#   rate_sc(t)  = CBO_c(t) x k_s(t)              every cell scaled uniformly, so
#                                                 state aggregate = N(t)
#
# share_sc = ACS population 16+ in cell c / state population 16+
# (US_ACS__epop_by_state_sex_age__2006-2024.csv, acs_epop.py); national
# composition = sum over states. So a state w/ national composition gets k = 1
# and the national path exactly.
#
# WHAT REACHES THE SIMULATOR. Simulator reads this for employment-rate GROWTH
# only -- base-year levels come from state-filtered ACS (docs/
# state_level_analysis.md, section 8). A cell's growth = CBO growth x
# k_s(t) / k_s(2019): state paths differ from national only through change in
# state age-sex composition. State labour-market signal does not enter.
#
# OUTSIDE 2006-2024 composition held at nearest ACS year -- runs from 2026
# use 2024 composition.
#
# NYC. ACS composition file is state-level; NYC takes New York's composition,
# so New York's path, until a county-level composition is built.
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
ACS_POP   <- file.path(EPOP_DIR, 'US_ACS__epop_by_state_sex_age__2006-2024.csv')
OUT_DIR   <- file.path('config', 'state', 'employment')

# STATE_POSTAL_TO_FIPS, SUBSTATE_GEOGRAPHIES, parent_geo()
source('src/shared_functions/constants.R')

STATES    <- names(STATE_POSTAL_TO_FIPS)   # 50 + DC -- demand build covers all 51
SUBSTATES <- names(SUBSTATE_GEOGRAPHIES)   # NYC -- takes parent state's composition



composition_shares <- function(acs, years) {

  #----------------------------------------------------------------------------
  # Population share of each sex x age cell in each state, 16+, for every
  # simulator year -- ACS years as observed, held at nearest ACS year outside.
  # geo 'US' = sum over states.
  #
  # Params:
  #   - acs (tibble): year, state_fips, sex, age_bin, pop_wt
  #   - years (int vec): simulator years
  #
  # Returns: (tibble) geo, year, col (epop_<sex>_<age_bin>), share
  #----------------------------------------------------------------------------

  fips_to_postal <- setNames(names(STATE_POSTAL_TO_FIPS), STATE_POSTAL_TO_FIPS)
  st <- acs %>%
    mutate(geo = unname(fips_to_postal[as.character(state_fips)])) %>%
    filter(!is.na(geo))
  us <- st %>%
    group_by(year, sex, age_bin) %>%
    summarise(pop_wt = sum(pop_wt), .groups = 'drop') %>%
    mutate(geo = 'US')

  obs <- bind_rows(select(st, geo, year, sex, age_bin, pop_wt), us) %>%
    group_by(geo, year) %>%
    mutate(share = pop_wt / sum(pop_wt)) %>%
    ungroup() %>%
    transmute(geo, acs_year = year, col = paste0('epop_', sex, '_', age_bin), share)

  acs_years <- range(obs$acs_year)
  tibble(year = years,
         acs_year = pmin(pmax(years, acs_years[1]), acs_years[2])) %>%
    inner_join(obs, by = 'acs_year', relationship = 'many-to-many') %>%
    select(geo, year, col, share)
}



build <- function(check_only = FALSE) {

  #----------------------------------------------------------------------------
  # Writes OUT_DIR/<ST>.csv for STATES + SUBSTATES; prints diagnostics.
  #
  # Params:
  #   - check_only (logical): TRUE skips all writes
  #
  # Returns: (tibble) diagnostics, invisibly
  #----------------------------------------------------------------------------

  for (f in c(NATIONAL, ACS_POP)) {
    if (!file.exists(f)) {
      cat('MISSING INPUT: ', f, '\n', sep = '')
      quit(status = 1)
    }
  }

  natl <- read_csv(NATIONAL, show_col_types = FALSE)
  cell_cols <- grep('^epop_[mf]_(?!16plus)', names(natl), value = TRUE, perl = TRUE)
  comp <- composition_shares(read_csv(ACS_POP, show_col_types = FALSE), natl$year)

  missing_cols <- setdiff(cell_cols, unique(comp$col))
  if (length(missing_cols)) stop('ACS composition lacks cells: ', paste(missing_cols, collapse = ', '))
  missing_geo <- setdiff(c(STATES, 'US'), unique(comp$geo))
  if (length(missing_geo)) stop('no ACS composition for: ', paste(missing_geo, collapse = ', '))

  # National CBO cell rates, long
  cbo <- natl %>%
    select(year, all_of(cell_cols)) %>%
    pivot_longer(-year, names_to = 'col', values_to = 'cbo')

  # E_g(t) = composition-implied rate, every geography incl. US. National
  # file leaves 60_64 and 65plus blank from 2026 (CBO projects neither) --
  # sum runs over cells w/ a rate that year, shares renormalised over them,
  # same cells for state and nation. Blank cells stay blank in the output.
  implied <- comp %>%
    inner_join(cbo, by = c('year', 'col')) %>%
    filter(!is.na(cbo)) %>%
    group_by(geo, year) %>%
    summarise(rate = sum(share * cbo) / sum(share), .groups = 'drop')
  natl_rate <- implied %>% filter(geo == 'US') %>% select(year, natl_rate = rate)
  k <- implied %>%
    filter(geo != 'US') %>%
    left_join(natl_rate, by = 'year') %>%
    mutate(k = natl_rate / rate)

  # State aggregate = national rate by construction: k x E_s = N. No check --
  # a missing geography stops above (missing_geo), a missing year below
  # (anyNA(kk)).

  diags <- list()

  for (st in c(STATES, SUBSTATES)) {
    ks <- filter(k, geo == parent_geo(st))
    kk <- ks$k[match(natl$year, ks$year)]
    if (anyNA(kk)) stop(st, ': no scale factor for some simulator years')

    # Every cell column, plus 16plus columns: national x k. Simulator reads
    # fine bins only (get_employment_rate_growth_factor.R); 16plus kept for
    # schema.
    out <- natl
    for (col in c(cell_cols, 'epop_m_16plus', 'epop_f_16plus')) {
      out[[col]] <- natl[[col]] * kk
    }

    if (!check_only) {
      dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
      write_csv(out, file.path(OUT_DIR, paste0(st, '.csv')), na = '')
    }

    y19 <- ks$year == 2019
    y24 <- ks$year == 2024
    diags[[st]] <- tibble::tibble(
      state        = st,
      implied_2019 = round(ks$rate[y19], 2),
      natl_2019    = round(ks$natl_rate[y19], 2),
      k_2019       = round(ks$k[y19], 4),
      k_2024       = round(ks$k[y24], 4),
      adjustment   = round(ks$k[y24] / ks$k[y19], 4))
  }

  dg <- bind_rows(diags)

  cat('== state epop, ', nrow(dg), ' geographies: ACS composition x CBO cells, ',
      'scaled to the national rate\n\n', sep = '')
  print(as.data.frame(dg), row.names = FALSE)
  cat('\nadjustment = k(2024) / k(2019): the multiplier on national growth in',
      'the runs\n')
  if (!check_only) {
    cat('wrote ', OUT_DIR, '/<ST>.csv\n', sep = '')
  }
  invisible(dg)
}



if (sys.nframe() == 0) {
  build(check_only = '--check' %in% commandArgs(TRUE))
}
