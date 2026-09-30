#------------------------------------------------------------------------------
# state_supply_params.R -- writes config/state/supply/<ST>.yaml.
#
# WHY THESE FILES EXIST. The simulator's labour supply curve is
#
#     w_market = w_0 * nominal_wage_factor * (L / L_0)^(1/e)
#
# with L = colSums(L_req * Qd). In a state run Qd is state-sized, because the
# household filter keeps ACS weights as they are. L_0, though, comes from the
# supply parameter set, and with no state override that set is the NATIONAL one
# -- so the curve is evaluated at the state's SHARE of national labour, not at
# 1. A run of KY, NJ, NY and VA on 2026-09-24 confirmed it: the ratio came out
# 0.021 to 0.084, and Kentucky's no-BA wage solved at $6.40 against a base of
# $12.06. The BA wage solved BELOW the no-BA wage in every state, because its
# elasticity is 2 rather than 4 and the same ratio bites twice as hard.
#
# The simulator already provides the remedy as an INPUT: when
# config/state/supply/<ST>.yaml exists, parse_supply_params_yaml() replaces the
# whole parameter set for that state. So these files carry the national
# parameters unchanged, with ONE quantity re-based to the state:
#
#     labor_supply = colSums(L_req * state base-year hours)
#
# That is the same definition 1a_nsece_processing.R uses nationally -- labour
# requirements times base-year hours -- applied to the state's own base-year
# hours, which are the committed demand targets. It puts the ratio back at
# about 1 in the base year, which is where the curve was calibrated.
#
# FALLBACK ONLY when demand is re-anchored. 3a_initialize_simulation.R then
# replaces labor_supply with colSums(L_req * model base-year sector hours) from
# adjust_alpha_for_state() -- the model's own centre split, not LOW_FRACTION --
# so the ratio is exactly 1 in the base year. This value is used as is only for
# a state with no config/state/demand/<ST>.csv.
#
# WAGES ALSO STATE-SPECIFIC. state_childcare_wages.R measures hourly earnings
# of childcare workers by state, education from 2018, 2019 records of ACS
# 2015-2019 5-year (wage + business income, self-employed included); this
# scales national wage by state's ratio to national figure. ACS national level
# $12.75 no-BA, $19.49 BA
# against NSECE $12.06, $19.70 -- RATIO keeps NSECE's definition, which rest of
# parameter set built on, cancels any difference common to every state.
#
# Setting w_0 to the state wage is what the curve wants: at L = L_0 the wage is
# then the state's own base-year wage, which is the pair the curve is anchored
# on. A cell under 50 ACS workers takes ratio 1, so that state keeps the
# national wage for that education group rather than swinging on a handful of
# records -- nine cells in 2018-19 records (BA: AK, DC, DE, ID, MT, ND, RI, WY;
# no-BA: VT).
#
# WHAT IS DELIBERATELY NOT CHANGED. labor_requirements, elasticities and
# per_unit_residual stay national.
#
# per_unit_residual is the one worth stating. The equilibrium is
#
#     P_supply = unit_cost + delta_0 * cpi_factor
#
# so delta_0 is a per-unit residual in DOLLARS, covering everything that is not
# labour: rent, food, insurance, administration, margin. Moving wages moves
# unit_cost, and the state's price moves with it by the full labour-cost
# difference while the non-labour margin stays at its national dollar value.
# That is a choice, and it is the conservative one: nothing measured here says
# the non-labour margin varies by state, and rent plainly does not scale with
# childcare wages. Scaling delta_0 by the wage ratio would assume it does.
#
# Usage:
#   Rscript resources/supply/state_supply_params.R
#   Rscript resources/supply/state_supply_params.R --check
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr)
})

DIR      <- file.path('resources', 'supply')
NATIONAL <- file.path(DIR, 'US_NSECE__national_supply_parameters__2019.yaml')
WAGES    <- file.path(DIR, 'state_childcare_wage_ratios.csv')
DEMAND   <- file.path('config', 'state', 'demand')
OUT_DIR  <- file.path('config', 'state', 'supply')
DIAG     <- file.path(DIR, 'state_supply_diagnostics.csv')

# The demand file carries ONE Center-Based row covering both price bands, since
# no state publishes the dispersion a split would need. Splitting it here needs
# a rule, and the rule is NSECE's own: each band's share of children times its
# mean weekly hours. Splitting by children alone would be wrong -- the bands
# differ in hours (27.5 against 40.4 a week).
#
# Read from state_data/out/nsece_universe.csv, universe A -- not re-typed, so
# cannot drift from the derivation.
NSECE_UNIVERSE <- file.path('state_data', 'out', 'nsece_universe.csv')
band <- read_csv(NSECE_UNIVERSE, show_col_types = FALSE) %>%
  filter(startsWith(universe, 'A:'),
         key %in% c('High-Priced Center-Based', 'Low-Priced Center-Based'),
         quantity %in% c('price_band_share', 'weekly_hours'))
band_val <- function(q, k) {

  #----------------------------------------------------------------------------
  # One universe-A price-band value from nsece_universe.csv.
  #
  # Params:
  #   - q (chr): quantity, 'price_band_share' or 'weekly_hours'
  #   - k (chr): key, 'High-Priced Center-Based' or 'Low-Priced Center-Based'
  #
  # Returns: (num) value; stops unless exactly one row matches
  #----------------------------------------------------------------------------

  v <- band$value[band$quantity == q & band$key == k]
  if (length(v) != 1) stop('nsece_universe.csv: no single row for ', q, ' / ', k)
  v
}
HI_SHARE <- band_val('price_band_share', 'High-Priced Center-Based')
LO_SHARE <- band_val('price_band_share', 'Low-Priced Center-Based')
HI_HOURS <- band_val('weekly_hours', 'High-Priced Center-Based')
LO_HOURS <- band_val('weekly_hours', 'Low-Priced Center-Based')
LOW_FRACTION <- (LO_SHARE * LO_HOURS) /
                (LO_SHARE * LO_HOURS + HI_SHARE * HI_HOURS)

SECTOR_ORDER <- c('unpaid_center_based', 'low_price_center_based',
                  'high_price_center_based', 'paid_home_based')



state_labor_supply <- function(natl, demand_path) {

  #----------------------------------------------------------------------------
  # L_0 for one state: labour requirements times the state's base-year hours.
  #
  # Params:
  #   - natl (list): the national parameter set
  #   - demand_path (chr): config/state/demand/<ST>.csv
  #
  # Returns: (list) no_ba, ba
  #----------------------------------------------------------------------------

  d <- read_csv(demand_path, show_col_types = FALSE)
  h <- setNames(d$annual_hours, d$ecec_type)

  cb <- unname(h['Center-Based'])
  hours <- c(
    unpaid_center_based     = unname(h['Unpaid Center-Based']),
    low_price_center_based  = cb * LOW_FRACTION,
    high_price_center_based = cb * (1 - LOW_FRACTION),
    paid_home_based         = unname(h['Paid Home-Based']))

  if (any(!is.finite(hours)) || any(hours <= 0)) {
    stop(demand_path, ': a supply sector has no positive base-year hours.')
  }

  lr <- natl$labor_requirements
  list(no_ba = sum(vapply(SECTOR_ORDER,
                          function(s) lr[[s]]$no_ba * hours[[s]], 0)),
       ba    = sum(vapply(SECTOR_ORDER,
                          function(s) lr[[s]]$ba * hours[[s]], 0)))
}



main <- function() {

  #----------------------------------------------------------------------------
  # Entry point -- per state demand file in DEMAND, writes OUT_DIR/<ST>.yaml:
  # national parameter set w/ labor_supply re-based to state base-year hours,
  # wages scaled by state wage ratio. Writes DIAG, prints table.
  #
  # Params: none -- --check on CLI skips all writes; exits 1 if NATIONAL,
  #   WAGES, or any demand file missing
  #
  # Returns: (tibble) one diagnostics row per state, invisibly
  #----------------------------------------------------------------------------

  check_only <- '--check' %in% commandArgs(TRUE)

  if (!file.exists(NATIONAL)) {
    cat('MISSING INPUT: ', NATIONAL, '\n', sep = '')
    quit(status = 1)
  }
  natl <- read_yaml(NATIONAL)

  if (!file.exists(WAGES)) {
    cat('MISSING INPUT: ', WAGES,
        ' -- run state_childcare_wages.R first\n', sep = '')
    quit(status = 1)
  }
  wr <- read_csv(WAGES, show_col_types = FALSE)

  wage_ratio <- function(st, grp) {
    r <- wr$wage_ratio[wr$state == st & wr$educ_group == grp]
    if (length(r) != 1 || !is.finite(r) || r <= 0) {
      stop('no usable wage ratio for ', st, '/', grp, ' in ', WAGES)
    }
    r
  }

  files <- list.files(DEMAND, pattern = '^[A-Z]{2}[.]csv$', full.names = TRUE)
  if (!length(files)) {
    cat('no state demand files in ', DEMAND, '\n', sep = '')
    quit(status = 1)
  }

  rows <- list()
  for (f in files) {
    st <- sub('[.]csv$', '', basename(f))
    L  <- state_labor_supply(natl, f)

    rn <- wage_ratio(st, 'no_ba')
    rb <- wage_ratio(st, 'ba')
    w  <- list(no_ba = natl$wages$no_ba * rn, ba = natl$wages$ba * rb)

    out <- natl
    out$labor_supply <- list(no_ba = L$no_ba, ba = L$ba)
    out$wages        <- w

    if (!check_only) {
      dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
      write_yaml(out, file.path(OUT_DIR, paste0(st, '.yaml')))
    }

    rows[[st]] <- tibble::tibble(
      state          = st,
      L0_no_ba       = L$no_ba,
      L0_ba          = L$ba,
      natl_L0_no_ba  = natl$labor_supply$no_ba,
      share_of_natl  = L$no_ba / natl$labor_supply$no_ba,
      wage_ratio_no_ba = rn,
      wage_ratio_ba    = rb,
      w_no_ba          = w$no_ba,
      w_ba             = w$ba)
  }

  d <- bind_rows(rows) %>% arrange(state)
  if (!check_only) write_csv(d, DIAG)

  cat('== state labour supply, from the committed demand targets\n')
  cat('   Center-Based hours split: low ', sprintf('%.4f', LOW_FRACTION),
      ' / high ', sprintf('%.4f', 1 - LOW_FRACTION), '\n\n', sep = '')
  print(as.data.frame(d %>% mutate(
    L0_no_ba = format(round(L0_no_ba), big.mark = ','),
    L0_ba    = format(round(L0_ba), big.mark = ','),
    share_of_natl = round(share_of_natl, 5),
    wage_ratio_no_ba = round(wage_ratio_no_ba, 3),
    wage_ratio_ba    = round(wage_ratio_ba, 3),
    w_no_ba = round(w_no_ba, 2),
    w_ba    = round(w_ba, 2)) %>%
    select(state, L0_no_ba, share_of_natl, wage_ratio_no_ba, w_no_ba,
           wage_ratio_ba, w_ba)), row.names = FALSE)

  cat('\n   national wages: no_ba $', sprintf('%.2f', natl$wages$no_ba),
      '  ba $', sprintf('%.2f', natl$wages$ba), '\n', sep = '')
  cat('   share_of_natl is what L/L_0 collapsed to before these files existed.\n')
  cat('   a wage ratio of exactly 1.000 is a cell under 50 ACS workers,\n')
  cat('   which keeps the national wage rather than a handful of records.\n')
  if (!check_only) {
    cat('wrote ', OUT_DIR, '/<ST>.yaml and ', DIAG, '\n', sep = '')
  }
  invisible(d)
}



if (sys.nframe() == 0) main()
