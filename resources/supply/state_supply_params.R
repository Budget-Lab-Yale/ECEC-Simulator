#------------------------------------------------------------------------------
# state_supply_params.R -- writes config/state/supply/<ST>.yaml.
# Full rationale: docs/state_level_analysis.md (supply section).
#
# WHY. Supply curve w_market = w_0 * nominal_wage_factor * (L / L_0)^(1/e),
# L = colSums(L_req * Qd). State run: Qd state-sized (household filter keeps
# ACS weights), but L_0 national w/out override -- curve evaluated at state's
# SHARE of national labour, not 1. KY, NJ, NY, VA run 2026-09-24: ratio 0.021
# to 0.084, KY no-BA wage $6.40 vs base $12.06, BA wage BELOW no-BA in every
# state (elasticity 2 vs 4 -- same ratio has twice the effect).
# config/state/supply/<ST>.yaml, when present, replaces whole parameter set
# (parse_supply_params_yaml()) -- files carry national parameters w/ wages,
# labor_requirements re-based to state.
#
# L_0 (labor_supply) NOT in file. 3a_initialize_simulation.R sets it per state
# run = colSums(L_req * model base-year sector hours) from
# adjust_alpha_for_state() -- ratio exactly 1 in base year. Needs
# config/state/demand/<GEO>.csv; override w/out demand file stops run.
#
# w_0 (wages) = national NSECE wage x OES May 2019 state / national
# childcare wage per group (oes_childcare_wage_ratios_2019.R: OES hourly
# medians of childcare workers 39-9011, preschool teachers 25-2011,
# preschool and childcare administrators 11-9031, mixed at national ACS
# occupation shares of each education group; NYC = New York MSA 35620).
# Ratio keeps NSECE's wage definition, cancels differences common to every
# state; at L = L_0 market wage = state's own base-year wage.
#
# L_req (labor_requirements) = national NSECE worker-hours per child-hour x
# state observed staffing index, staffing_index(observed_indices()). Index =
# NSECE 2019 observed staff per child by Census region x urban density
# (nsece_staffing_region_density.R; centre classrooms age-standardised,
# listed homes) at state's 2010 child-under-5 density mix, / PEP 2019
# children-0-4-weighted mean over 51 states. Home index blended toward 1 by
# NSECE licensed share of paid home-based children (cells = listed homes;
# unlisted paid home care at national level). Centre index scales all three
# centre sectors, home index paid_home_based, both education groups alike.
# WHY observed, not legal: observed staffing follows legal ratios weakly --
# NSECE log slope 0.21 across regions, ACS 0.10-0.23 across states within
# region (state_data/code/analyze_observed_staffing.R).
# Legal-ratio index (licensing_indices()) -- diagnostic only: reviewed October
# 1, 2019 rules, frozen in licensing_ratios_reviewed_2019.csv (builder
# licensing_ratios_2019.R, rule sources resources/licensing/, state_data/raw/
# removed 2026-11-04; commit f96978c holds them).
#
# NOT CHANGED: elasticities stay national (no_ba 4, ba 2) -- assumed values
# (Borowsky et al. 2022, NBER w30140); no state evidence supports a proxy.
# delta_0 (per_unit_residual; non-labour cost + margin, P_supply = unit_cost +
# delta_0 * cpi_factor) = local 2019 price - local labour cost, as 1a computes
# national residual (NSECE price - L_req %*% w). Local price: LOCAL_PRICES
# (ndcp_local_prices_2019.R -- NDCP county prices at NSECE national level;
# IN, NM, MS home: NSECE region x urban density). Local labour cost: geo's
# 2019 w, L_req above. Centre: one factor on both national tier residuals --
# tier-mix-weighted price (p0 low-tier share) = local centre price, national
# low/high gap kept. Home: residual = local home price - home labour cost.
# Unpaid centre residual untouched (none). Residual <= 0 stops build.
# CHANGE vs main (one national residual per sector: 0.510055 low, 4.6145667
# high, 2.8273432 home). WHY: residual = non-labour cost + margin, local as
# wages, staffing. --national-residual restores national delta_0 (diagnostic).
#
# Usage: Rscript resources/supply/state_supply_params.R [--check]
#          [--national-residual] [--out <dir>]
#   --check: compute, print, write nothing
#   --national-residual: residual test -- national delta_0 in every geo (main)
#   --out <dir>: output dir (default config/state/supply)
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr)
})

# parent_geo(), SUBSTATE_GEOGRAPHIES
source('src/shared_functions/constants.R')
source('src/shared_functions/get_state_supply_year_factors.R')

DIR      <- file.path('resources', 'supply')
NATIONAL <- file.path(DIR, 'US_NSECE__national_supply_parameters__2019.yaml')
WAGES    <- file.path(DIR, 'state_childcare_wage_ratios_oes.csv')   # oes_childcare_wage_ratios_2019.R
DEMAND   <- file.path('config', 'state', 'demand')
OUT_DIR  <- file.path('config', 'state', 'supply')
ARGS     <- commandArgs(TRUE)
if ('--out' %in% ARGS) OUT_DIR <- ARGS[which(ARGS == '--out') + 1]
NATIONAL_RESIDUAL <- '--national-residual' %in% ARGS
LOCAL_PRICES <- file.path(DIR, 'US_DOL_NDCP__local_care_prices__2019.csv')   # ndcp_local_prices_2019.R

GROUPS <- c(no_ba = 'no_ba', ba = 'ba')   # named -> lapply() keeps names

# licensing_indices() input -- licensed children per staff by state, setting,
# age year (frozen, see header); county children 0-4 (both indices)
RATIOS  <- file.path(DIR, 'licensing_ratios_reviewed_2019.csv')
POP_0_4 <- file.path('resources', 'census_population',
                     'US_PEP__population_children_age_0_4_by_county__2019.csv')
# Licensed share of children in paid home-based care, NSECE 2019
# (state_data/cluster/nsece_home_licensed_share.R, measure children_0_5)
HOME_SHARE <- file.path(DIR, 'US_NSECE__home_licensed_share__2019.csv')
# Observed staff per child, NSECE region x urban density cells
# (state_data/cluster/nsece_staffing_region_density.R)
STAFFING_CELLS <- file.path(DIR, 'US_NSECE__staffing_region_density__2019.csv')
# density_mix(), REGION_OF -- sourced into own environment by observed_indices()
NDCP_LOCAL <- file.path(DIR, 'ndcp_local_prices_2019.R')

# NSECE_CENTRE_ENROLLED (NSECE 2019 centre enrolment by age 0-4),
# NSECE_AGE_YEAR (age 0-4 -> age-year row in RATIOS), NSECE_AGES_0_4 --
# state_data/code/shared_nsece_ages.R
source('state_data/code/shared_nsece_ages.R')
CENTRE_SECTORS <- c('unpaid_center_based', 'low_price_center_based',
                    'high_price_center_based')
HOME_SECTORS   <- 'paid_home_based'



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



home_licensed_share <- function(path = HOME_SHARE) {

  #----------------------------------------------------------------------------
  # Licensed share of children 0-5 in paid home-based care, NSECE 2019 listed
  # vs paid unlisted home-based providers (measure children_0_5).
  #
  # Params:
  #   - path (chr): nsece_home_licensed_share.R output
  #
  # Returns: (dbl) share in (0, 1)
  #----------------------------------------------------------------------------

  require_input(path, ' -- run state_data/cluster/nsece_home_licensed_share.R on cluster')
  s <- read_csv(path, show_col_types = FALSE) %>%
    filter(measure == 'children_0_5') %>%
    pull(licensed_share)
  if (length(s) != 1 || !is.finite(s) || s <= 0 || s >= 1) {
    stop(path, ': no valid children_0_5 licensed share')
  }
  s
}



pop_0_4_by_state <- function() {

  #----------------------------------------------------------------------------
  # PEP 2019 children 0-4 by state (county rows summed), 50 states + DC --
  # national-mean weights for both staffing indices.
  #
  # Returns: (tibble) state, pop
  #----------------------------------------------------------------------------

  require_input(POP_0_4)
  pop <- read_csv(POP_0_4, col_types = cols(.default = col_character())) %>%
    mutate(state = names(STATE_POSTAL_TO_FIPS)[match(as.integer(STATE),
                                                     STATE_POSTAL_TO_FIPS)],
           pop   = as.numeric(TOT_POP)) %>%
    group_by(state) %>%
    dplyr::summarise(pop = sum(pop), .groups = 'drop')
  if (anyNA(pop$state) || nrow(pop) != length(STATE_POSTAL_TO_FIPS) ||
      any(!is.finite(pop$pop) | pop$pop <= 0)) {
    stop(POP_0_4, ': expected 50 states + DC')
  }
  pop
}



licensing_indices <- function(ratios = RATIOS, age_map = NSECE_AGE_YEAR,
                              home_share = home_licensed_share()) {

  #----------------------------------------------------------------------------
  # Legal-ratio staffing index (diagnostic), every geography in ratios file.
  # Centre: staff per child = sum_a w_a / r(geo, map(a)) / sum_a w_a, w =
  # NSECE_CENTRE_ENROLLED ages 0-4, map = age_map, r = licensed children per
  # staff; index = geo value / PEP children-0-4-weighted mean over 50 states
  # + DC. Home: 1 / max children, same weighting over states that license
  # homes; geo w/ home value NA (no home licence) -> 1. Licensing binds
  # licensed homes only -- home index = home_share x licensing index +
  # (1 - home_share), unlicensed paid home care at national level; national
  # mean stays 1. NYC carries own rows, not in national mean (inside NY).
  # Rules: October 1, 2019. Centre ratio per year of age 0-4, ordinary age
  # bands month-weighted; NY
  # blends NYS, NYC rules by NYC child share. Home capacity: one adult,
  # mixed-age -- max preschool children w/ infant present. NYC own centre
  # rules, NYS home rules.
  #
  # Params:
  #   - ratios (chr): csv w/ state, setting, age_group, children_per_staff
  #   - age_map (chr): age_0..age_4 -> centre age_group in ratios
  #     (NSECE_AGE_YEAR)
  #   - home_share (dbl): licensed share of paid home-based children
  #
  # Returns: (tibble) state, centre, home -- indices
  #----------------------------------------------------------------------------

  require_input(ratios)
  rt <- read_csv(ratios, col_types = cols(.default = col_character())) %>%
    mutate(children_per_staff = suppressWarnings(as.numeric(children_per_staff)))
  if (anyNA(rt[c('state', 'setting', 'age_group')]) ||
      anyDuplicated(rt[c('state', 'setting', 'age_group')]) ||
      !setequal(rt$state, c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))) ||
      any(!rt$setting %in% c('center', 'home')) ||
      any(!is.finite(rt$children_per_staff[rt$setting == 'center'])) ||
      any(rt$children_per_staff <= 0, na.rm = TRUE)) {
    stop(ratios, ': invalid staffing keys, coverage, or ratios')
  }
  pop <- pop_0_4_by_state()

  # Staff per child, every geography in ratios
  centre <- rt %>%
    filter(setting == 'center') %>%
    inner_join(tibble::tibble(age_group = unname(age_map[NSECE_AGES_0_4]),
                              w = unname(NSECE_CENTRE_ENROLLED[NSECE_AGES_0_4])),
               by = 'age_group', relationship = 'many-to-many') %>%
    group_by(state) %>%
    dplyr::summarise(s = sum(w / children_per_staff) / sum(w),
                     n = n(), .groups = 'drop')
  home <- rt %>%
    filter(setting == 'home') %>%
    transmute(state, s = 1 / children_per_staff)

  if (any(centre$n != length(NSECE_AGES_0_4)) || anyNA(centre$s)) {
    stop(ratios, ': centre ratio missing for some age point')
  }
  if (!setequal(centre$state, home$state)) {
    stop(ratios, ': centre, home rows cover different geographies')
  }

  natl_mean <- function(d) {
    d <- inner_join(d, pop, by = 'state') %>% filter(!is.na(s))
    sum(d$s * d$pop) / sum(d$pop)
  }

  centre %>%
    transmute(state, centre = s / natl_mean(centre)) %>%
    inner_join(home %>% transmute(state, home = s / natl_mean(home)), by = 'state') %>%
    mutate(home = home_share * ifelse(is.na(home), 1, home) + (1 - home_share))
}



observed_indices <- function(cells = STAFFING_CELLS, home_share = home_licensed_share()) {

  #----------------------------------------------------------------------------
  # Observed staffing index, 50 states + DC + NYC. Geo staff per child =
  # NSECE region x urban density cell values (est_used; centre classroom
  # values age-standardised) at geo's 2010 child-under-5 mix by density
  # (density_mix()); index = geo value / PEP children-0-4-weighted mean over
  # 50 states + DC. NYC: Northeast cells at own mix, not in national mean.
  # Home cells: listed (licensed) homes only -- home index = home_share x
  # observed index + (1 - home_share), unlisted paid home care at national
  # level, as licensing_indices().
  #
  # Params:
  #   - cells (chr): nsece_staffing_region_density.R output
  #   - home_share (dbl): licensed share of paid home-based children
  #
  # Returns: (tibble) state, centre, home -- indices
  #----------------------------------------------------------------------------

  require_input(cells, ' -- run state_data/cluster/nsece_staffing_region_density.R on cluster')
  e <- new.env()
  sys.source(NDCP_LOCAL, envir = e)
  mix <- e$density_mix()
  geos <- c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))
  region <- c(e$REGION_OF, setNames(rep(e$REGION_OF[['NY']], length(SUBSTATE_GEOGRAPHIES)),
                                    names(SUBSTATE_GEOGRAPHIES)))
  if (!setequal(unique(mix$geo), geos) || any(!geos %in% names(region))) {
    stop('density mix or region map does not cover 50 states + DC + sub-state geographies')
  }

  cl <- read_csv(cells, show_col_types = FALSE) %>% filter(region > 0, density > 0)
  if (nrow(cl) != 24 || anyNA(cl$est_used) || any(cl$est_used <= 0) ||
      !setequal(cl$setting, c('center', 'home'))) {
    stop(cells, ': want 2 settings x 4 regions x 3 densities, positive est_used')
  }

  pop <- pop_0_4_by_state()

  v <- mix %>%
    mutate(region = unname(region[geo])) %>%
    inner_join(cl %>% select(setting, region, density, est_used), by = c('region', 'density'),
               relationship = 'many-to-many') %>%
    group_by(state = geo, setting) %>%
    dplyr::summarise(s = sum(share * est_used), .groups = 'drop') %>%
    left_join(pop, by = 'state')
  if (nrow(v) != 2 * length(geos)) stop(cells, ': cell missing for some geo x density')

  v %>%
    group_by(setting) %>%
    mutate(idx = s / weighted.mean(s[!is.na(pop)], pop[!is.na(pop)])) %>%
    ungroup() %>%
    select(state, setting, idx) %>%
    tidyr::pivot_wider(names_from = setting, values_from = idx) %>%
    transmute(state, centre = center, home = home_share * home + (1 - home_share))
}



staffing_index <- function(L_req, geo, indices = observed_indices()) {

  #----------------------------------------------------------------------------
  # Scales national labor_requirements by geo's staffing index (default
  # observed_indices(); licensing_indices() for legal-ratio diagnostics).
  # Prints one line per geo.
  #
  # Params:
  #   - L_req (list): national labor_requirements, sector -> no_ba / ba
  #   - geo (chr): postal or sub-state code w/ row in indices
  #   - indices (tibble): state, centre, home
  #
  # Returns: (list) L_req, centre sectors x centre index, home sectors x home
  #   index
  #----------------------------------------------------------------------------

  ix <- indices
  if (!geo %in% ix$state) stop('staffing index: no centre or home value for ', geo)
  i_c <- ix$centre[ix$state == geo]
  i_h <- ix$home[ix$state == geo]

  cat(sprintf('   staffing index %-4s centre %.3f  home %.3f\n', geo, i_c, i_h))

  unknown <- setdiff(names(L_req), c(CENTRE_SECTORS, HOME_SECTORS))
  if (length(unknown)) stop('labor_requirements sector w/out index: ', unknown)
  for (k in names(L_req)) {
    i <- if (k %in% CENTRE_SECTORS) i_c else i_h
    L_req[[k]] <- lapply(L_req[[k]], function(x) x * i)
  }
  L_req
}



local_residual <- function(geo, L_req, w, natl_resid, prices) {

  #----------------------------------------------------------------------------
  # Local per-unit residual = local 2019 price - local labour cost. Centre:
  # factor m on both national tier residuals, tier-weighted price = local
  # centre price. Home: local home price - home labour cost. National
  # residual under --national-residual.
  #
  # Params:
  #   - geo (chr): postal or sub-state code w/ LOCAL_PRICES rows
  #   - L_req (list): geo labor_requirements, sector -> no_ba / ba
  #   - w (list): geo 2019 wages no_ba / ba
  #   - natl_resid (list): national per_unit_residual
  #   - prices (df): LOCAL_PRICES
  #
  # Returns: (list) resid (per_unit_residual), centre_factor, home_factor
  #----------------------------------------------------------------------------

  if (NATIONAL_RESIDUAL) return(list(resid = natl_resid, centre_factor = 1, home_factor = 1))
  lab <- function(s) sum(unlist(L_req[[s]])[names(GROUPS)] * unlist(w)[names(GROUPS)])
  row <- function(s) {
    x <- prices[prices$geo == geo & prices$setting == s, ]
    if (nrow(x) != 1 || !is.finite(x$target_price) || x$target_price <= 0) {
      stop(LOCAL_PRICES, ': no usable ', s, ' price for ', geo, ' -- rerun ndcp_local_prices_2019.R')
    }
    x
  }
  ce <- row('center'); ho <- row('home'); sl <- ce$low_tier_share
  r_lo <- natl_resid$low_price_center_based; r_hi <- natl_resid$high_price_center_based
  m <- (ce$target_price - (sl * lab('low_price_center_based') + (1 - sl) * lab('high_price_center_based'))) /
    (sl * r_lo + (1 - sl) * r_hi)
  r_home <- ho$target_price - lab('paid_home_based')
  if (m <= 0 || r_home <= 0) {
    stop(geo, ': local price below local labour cost (centre factor ', round(m, 3), ', home residual ',
         round(r_home, 3), '). Check staffing, wages, local price.')
  }
  resid <- natl_resid
  resid$low_price_center_based  <- m * r_lo
  resid$high_price_center_based <- m * r_hi
  resid$paid_home_based         <- r_home
  list(resid = resid, centre_factor = m, home_factor = r_home / natl_resid$paid_home_based)
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
  require_input(WAGES, ' -- run oes_childcare_wage_ratios_2019.R first')
  wr <- read_csv(WAGES, show_col_types = FALSE)

  # Sub-state geography (NYC) w/out own WAGES row falls back to parent state
  wage_ratio <- function(st, grp) {
    if (!any(wr$state == st)) st <- parent_geo(st)
    r <- wr$wage_ratio[wr$state == st & wr$educ_group == grp]
    if (length(r) != 1 || !is.finite(r) || r <= 0) {
      stop('no usable wage_ratio for ', st, '/', grp)
    }
    r
  }

  files <- list.files(DEMAND, pattern = '^[A-Z]{2,3}[.]csv$', full.names = TRUE)
  if (!length(files)) {
    cat('no state demand files in ', DEMAND, '\n', sep = '')
    quit(status = 1)
  }

  indices <- observed_indices()
  require_input(LOCAL_PRICES, ' -- run resources/supply/ndcp_local_prices_2019.R first')
  local_prices <- read_csv(LOCAL_PRICES, show_col_types = FALSE)
  annual_wages <- file.path(DIR, 'state_childcare_wage_ratios_annual.csv')
  aw <- if (file.exists(annual_wages)) read_csv(annual_wages, show_col_types = FALSE) else NULL
  rows <- list()
  outputs <- list()
  for (f in files) {
    st <- sub('[.]csv$', '', basename(f))

    r <- lapply(GROUPS, function(g) wage_ratio(st, g))
    w <- lapply(GROUPS, function(g) natl$wages[[g]] * r[[g]])
    # Computed in check mode too
    L_req <- staffing_index(natl$labor_requirements, st, indices)
    # Residual: local price - local labour cost; unpaid centre unchanged
    lr <- local_residual(st, L_req, w, natl$per_unit_residual, local_prices)
    resid <- lr$resid

    # Validate complete payload in check mode too; write after all geographies pass.
    # No labor_supply -- 3a sets state L_0 from base-year sector hours
    out <- natl
    out$labor_supply <- NULL
    out$wages        <- w
    if (!is.null(aw)) {
      st_wages <- aw %>% filter(geo == st)
      if (!nrow(st_wages)) stop('Annual wage panel lacks ', st)
      years <- sort(unique(st_wages$year))
      if (anyNA(st_wages$year) || !identical(as.integer(years), seq.int(2019L, max(years))) ||
          any(!is.finite(st_wages$relative_to_2019) | st_wages$relative_to_2019 <= 0)) {
        stop('Invalid or incomplete annual wage panel for ', st)
      }
      out$wage_path <- lapply(years, function(y) {
        x <- st_wages %>% filter(year == y)
        if (nrow(x) != 2 || !setequal(x$educ_group, names(GROUPS))) {
          stop('Incomplete annual wage panel for ', st, '/', y)
        }
        list(year = as.integer(y), no_ba = x$relative_to_2019[x$educ_group == 'no_ba'],
             ba = x$relative_to_2019[x$educ_group == 'ba'])
      })
      get_state_supply_year_factors(out$wage_path, max(years))
    }
    out$labor_requirements <- L_req
    out$per_unit_residual  <- resid
    outputs[[st]] <- out


    rows[[st]] <- tibble::tibble(
      state                  = st,
      wage_ratio_no_ba       = r$no_ba,
      wage_ratio_ba          = r$ba,
      w_no_ba                = w$no_ba,
      w_ba                   = w$ba,
      centre_residual_factor = lr$centre_factor,
      home_residual_factor   = lr$home_factor)
  }

  d <- bind_rows(rows) %>% arrange(state)

  cat('== state wages\n\n')
  d %>%
    transmute(state,
              wage_ratio_no_ba = round(wage_ratio_no_ba, 3), w_no_ba = round(w_no_ba, 2),
              wage_ratio_ba    = round(wage_ratio_ba, 3),    w_ba    = round(w_ba, 2)) %>%
    as.data.frame() %>%
    print(row.names = FALSE)

  cat('\n   national wages: no_ba $', sprintf('%.2f', natl$wages$no_ba),
      '  ba $', sprintf('%.2f', natl$wages$ba), '\n', sep = '')
  cat('   wage_ratio = OES May 2019 state / national childcare wage\n',
      '   (occupation hourly medians at national ACS education weights).\n',
      sep = '')
  if (!check_only) {
    dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
    for (st in names(outputs)) {
      write_yaml(outputs[[st]], file.path(OUT_DIR, paste0(st, '.yaml')), precision = 16)
    }
    cat('wrote ', OUT_DIR, '/<ST>.yaml\n', sep = '')
  }
  invisible(d)
}



if (sys.nframe() == 0) main()
