#------------------------------------------------------------------------------
# state_supply_params.R -- writes config/state/supply/<ST>.yaml.
# Full rationale: docs/state_level_analysis.md (supply section).
#
# WHY. Supply curve w_market = w_0 * nominal_wage_factor * (L / L_0)^(1/e),
# L = colSums(L_req * Qd). State run: Qd state-sized (household filter keeps
# ACS weights), but L_0 national w/out override -- curve evaluated at state's
# SHARE of national labour, not 1. KY, NJ, NY, VA run 2026-09-24: ratio 0.021
# to 0.084, KY no-BA wage $6.40 vs base $12.06, BA wage BELOW no-BA in every
# state (elasticity 2 vs 4 -- same ratio bites twice as hard).
# config/state/supply/<ST>.yaml, when present, replaces whole parameter set
# (parse_supply_params_yaml()) -- files carry national parameters w/ two
# quantities re-based to state:
#
# L_0 (labor_supply) = national NSECE L_0 x state share of national ACS
# childcare workers, per measured education group. FALLBACK only: when demand re-anchored,
# 3a_initialize_simulation.R replaces it w/ colSums(L_req * model base-year
# sector hours) from adjust_alpha_for_state() -- ratio exactly 1 in base year;
# every geography has config/state/demand/<GEO>.csv.
#
# w_0 (wages) = national NSECE wage x ACS state / national hourly mean per
# group (acs_childcare_wage_by_education_2019.py: four childcare occupations,
# education read off each worker, cell under 30 workers -> ratio 1). Ratio
# keeps NSECE's wage definition, cancels differences common to every state;
# at L = L_0 market wage = state's own base-year wage.
#
# NOT CHANGED: labor_requirements, elasticities, per_unit_residual stay
# national. P_supply = unit_cost + delta_0 * cpi_factor -- delta_0 per-unit
# DOLLAR residual for all non-labour cost (rent, food, insurance,
# administration, margin). State price moves by full labour-cost difference,
# non-labour margin stays at national dollar value: conservative choice --
# nothing here says it varies by state, rent does not scale w/ childcare
# wages; scaling delta_0 by wage ratio would assume it does.
#
# Usage: Rscript resources/supply/state_supply_params.R [--check]
#   --check: compute, print, write nothing
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr)
})

# parent_geo(), SUBSTATE_GEOGRAPHIES
source('src/shared_functions/constants.R')

DIR      <- file.path('resources', 'supply')
NATIONAL <- file.path(DIR, 'US_NSECE__national_supply_parameters__2019.yaml')
WAGES    <- file.path(DIR, 'state_childcare_wage_ratios_acs.csv')   # acs_childcare_wage_by_education_2019.py
DEMAND   <- file.path('config', 'state', 'demand')
OUT_DIR  <- file.path('config', 'state', 'supply')

GROUPS <- c(no_ba = 'no_ba', ba = 'ba')   # named -> lapply() keeps names



require_input <- function(path, hint = '') {

  #----------------------------------------------------------------------------
  # Exits status 1 w/ 'MISSING INPUT' line when path absent.
  #
  # Params:
  #   - path (chr): input file
  #   - hint (chr): appended to message
  #
  # Returns: nothing
  #----------------------------------------------------------------------------

  if (!file.exists(path)) {
    cat('MISSING INPUT: ', path, hint, '\n', sep = '')
    quit(status = 1)
  }
}



main <- function() {

  #----------------------------------------------------------------------------
  # Writes OUT_DIR/<ST>.yaml per demand file in DEMAND; prints table.
  # --check skips writes. Exits 1 if NATIONAL, WAGES, or every demand file
  # missing.
  #
  # Returns: (tibble) one diagnostics row per state, invisibly
  #----------------------------------------------------------------------------

  check_only <- '--check' %in% commandArgs(TRUE)

  require_input(NATIONAL)
  natl <- read_yaml(NATIONAL)
  require_input(WAGES, ' -- run acs_childcare_wage_by_education_2019.py summarise first')
  wr <- read_csv(WAGES, show_col_types = FALSE)

  # Sub-state geography (NYC) w/out own WAGES row falls back to parent state
  wage_ratio <- function(st, grp, col = 'wage_ratio') {
    if (!any(wr$state == st)) st <- parent_geo(st)
    r <- wr[[col]][wr$state == st & wr$educ_group == grp]
    if (length(r) != 1 || !is.finite(r) || r <= 0) {
      stop('no usable ', col, ' for ', st, '/', grp)
    }
    r
  }

  files <- list.files(DEMAND, pattern = '^[A-Z]{2,3}[.]csv$', full.names = TRUE)
  if (!length(files)) {
    cat('no state demand files in ', DEMAND, '\n', sep = '')
    quit(status = 1)
  }

  rows <- list()
  for (f in files) {
    st <- sub('[.]csv$', '', basename(f))

    # ACS carries NYC (county-selected) -- own row, own share
    L_acs <- lapply(GROUPS, function(g) {
      natl$labor_supply[[g]] * wage_ratio(st, g, col = 'emp_share')
    })
    r <- lapply(GROUPS, function(g) wage_ratio(st, g))
    w <- lapply(GROUPS, function(g) natl$wages[[g]] * r[[g]])

    if (!check_only) {
      dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
      out <- natl
      out$labor_supply <- L_acs
      out$wages        <- w
      write_yaml(out, file.path(OUT_DIR, paste0(st, '.yaml')))
    }

    rows[[st]] <- tibble::tibble(
      state                  = st,
      L0_no_ba               = L_acs$no_ba,
      L0_ba                  = L_acs$ba,
      natl_L0_no_ba          = natl$labor_supply$no_ba,
      wage_ratio_no_ba       = r$no_ba,
      wage_ratio_ba          = r$ba,
      w_no_ba                = w$no_ba,
      w_ba                   = w$ba)
  }

  d <- bind_rows(rows) %>% arrange(state)

  cat('== state labour supply (ACS fallback) and wages\n\n')
  d %>%
    transmute(state,
              L0_no_ba = format(round(L0_no_ba), big.mark = ','),
              L0_ba    = format(round(L0_ba), big.mark = ','),
              wage_ratio_no_ba = round(wage_ratio_no_ba, 3), w_no_ba = round(w_no_ba, 2),
              wage_ratio_ba    = round(wage_ratio_ba, 3),    w_ba    = round(w_ba, 2)) %>%
    as.data.frame() %>%
    print(row.names = FALSE)

  cat('\n   national wages: no_ba $', sprintf('%.2f', natl$wages$no_ba),
      '  ba $', sprintf('%.2f', natl$wages$ba), '\n', sep = '')
  cat('   wage_ratio = ACS state / national hourly mean. A ratio of exactly\n',
      '   1.000 is a cell under 30 ACS workers, which keeps the national wage.\n',
      sep = '')
  if (!check_only) {
    cat('wrote ', OUT_DIR, '/<ST>.yaml\n', sep = '')
  }
  invisible(d)
}



if (sys.nframe() == 0) main()
