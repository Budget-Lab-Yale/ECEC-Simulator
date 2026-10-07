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
# (parse_supply_params_yaml()) -- files carry national parameters w/ wages,
# labor_requirements, per_unit_residual re-based to state.
#
# L_0 (labor_supply) NOT in file. 3a_initialize_simulation.R sets it per state
# run = colSums(L_req * model base-year sector hours) from
# adjust_alpha_for_state() -- ratio exactly 1 in base year. Needs
# config/state/demand/<GEO>.csv; override w/out demand file stops run.
#
# w_0 (wages) = national NSECE wage x ACS state / national median hourly
# earnings per group (acs_childcare_wage_by_education_2019.py: four childcare
# occupations, education read off each worker, 2018-19 records of 2015-19
# 5-year file, cell under 30 records takes all-worker ratio). Ratio keeps NSECE's wage
# definition, cancels differences common to every state; at L = L_0 market
# wage = state's own base-year wage.
#
# L_req (labor_requirements) = national NSECE worker-hours per child-hour x
# state licensing staffing index, staffing_index(). Centre index = required
# staff per child (1 / licensed children-per-staff), averaged over ages 0-4 w/
# NSECE 2019 centre enrolment age mix, / same quantity averaged over 51
# states w/ PEP 2019 children 0-4 weights. Home index = 1 / licensed max
# children per family child care home, / 0-4-weighted mean over states that
# license homes; state w/out home licence -> 1. Centre index scales all three
# centre sectors, home index paid_home_based, both education groups alike.
# Ratios: HHS ACF Data Explorer (2014 licensing study) via CAP 2018 Table 1
# (centres; infant, toddler = 35 months, preschool = 3 years -- no separate
# 4-year rule, age 4 takes preschool) and PN3 2020 Table 6 (homes); NYC
# centres own Article 47 rules (DOHMH, June 2019), NYC homes NYS rules.
#
# delta_0 (per_unit_residual) = national x RPP / 100. P_supply = unit_cost +
# delta_0 * cpi_factor -- delta_0 per-unit DOLLAR residual for all non-labour
# cost (rent, food, insurance, administration, margin), scaled by BEA Regional
# Price Parity, all items, 2019 (RPP; NYC = New York metro area, BEA has no
# city RPP). Unpaid sector has no residual (NA) -- stays NA.
#
# NOT CHANGED: elasticities stay national (no_ba 4, ba 2) -- assumed values
# (Borowsky et al. 2022, NBER w30140); no state evidence supports a proxy.
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

# rpp_scale() input -- BEA RPP, all items, 2019, per geography
RPP <- file.path(DIR, 'US_BEA__regional_price_parities_all_items__2019.csv')

# staffing_index() inputs -- licensed children per staff by state, setting, age
# band; county children 0-4
RATIOS  <- file.path('state_data', 'raw', 'US',
                     'US_ACF__center_and_home_child_staff_ratios_by_age__2014.csv')
POP_0_4 <- file.path('state_data', 'raw', 'US',
                     'US_PEP__population_children_age_0_4_by_county__2019.csv')

# NSECE_CENTRE_ENROLLED (NSECE 2019 centre enrolment by age 0-4),
# NSECE_AGE_BAND (age 0-4 -> licensing age band in RATIOS), NSECE_AGES_0_4 --
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



rpp_scale <- function(residual, geo) {

  #----------------------------------------------------------------------------
  # Scales national per_unit_residual (non-labour $ per child-hour) by geo's
  # RPP / 100; NA entries (unpaid sector) stay NA.
  #
  # Params:
  #   - residual (list): national per_unit_residual by sector
  #   - geo (chr): state postal code or sub-state code (NYC)
  #
  # Returns: (list) per_unit_residual for geo
  #----------------------------------------------------------------------------

  rpp <- read_csv(RPP, show_col_types = FALSE)
  f <- rpp$rpp_all_items_2019[rpp$geo == geo] / 100
  if (length(f) != 1 || !is.finite(f)) stop('no RPP for ', geo, ' in ', RPP)
  lapply(residual, function(x) if (is.numeric(x) && !is.na(x)) x * f else x)
}



staffing_index <- function(L_req, geo) {

  #----------------------------------------------------------------------------
  # Scales national labor_requirements by geo's licensing staffing index.
  # Centre: staff per child = sum_a w_a / r(geo, band(a)) / sum_a w_a, w =
  # NSECE_CENTRE_ENROLLED ages 0-4, band = NSECE_AGE_BAND, r = licensed
  # children per staff (RATIOS); index = geo value / PEP
  # children-0-4-weighted mean over 50 states + DC. Home:
  # 1 / max children, same weighting over states that license homes; geo w/
  # home value NA (no home licence) -> index 1. NYC carries own RATIOS rows,
  # not in national mean (inside NY). Prints one line per geo.
  #
  # Params:
  #   - L_req (list): national labor_requirements, sector -> no_ba / ba
  #   - geo (chr): postal or sub-state code w/ RATIOS rows
  #
  # Returns: (list) L_req, centre sectors x centre index, home sectors x home
  #   index
  #----------------------------------------------------------------------------

  require_input(RATIOS)
  require_input(POP_0_4)
  rt <- read_csv(RATIOS, col_types = cols(.default = col_character())) %>%
    mutate(children_per_staff = suppressWarnings(as.numeric(children_per_staff)))

  pop <- read_csv(POP_0_4, col_types = cols(.default = col_character())) %>%
    mutate(state = names(STATE_POSTAL_TO_FIPS)[match(as.integer(STATE),
                                                     STATE_POSTAL_TO_FIPS)],
           pop   = as.numeric(TOT_POP)) %>%
    group_by(state) %>%
    dplyr::summarise(pop = sum(pop), .groups = 'drop')
  if (anyNA(pop$state) || nrow(pop) != length(STATE_POSTAL_TO_FIPS)) {
    stop(POP_0_4, ': expected 50 states + DC')
  }

  # Staff per child, every geography in RATIOS
  centre <- rt %>%
    filter(setting == 'center') %>%
    inner_join(tibble::tibble(age_group = unname(NSECE_AGE_BAND),
                              w = unname(NSECE_CENTRE_ENROLLED[NSECE_AGES_0_4])),
               by = 'age_group', relationship = 'many-to-many') %>%
    group_by(state) %>%
    dplyr::summarise(s = sum(w / children_per_staff) / sum(w),
                     n = n(), .groups = 'drop')
  home <- rt %>%
    filter(setting == 'home') %>%
    transmute(state, s = 1 / children_per_staff)

  if (any(centre$n != length(NSECE_AGE_BAND)) || anyNA(centre$s)) {
    stop(RATIOS, ': centre ratio missing for some age band')
  }

  natl_mean <- function(d) {
    d <- inner_join(d, pop, by = 'state') %>% filter(!is.na(s))
    sum(d$s * d$pop) / sum(d$pop)
  }

  if (!geo %in% centre$state || !geo %in% home$state) {
    stop(RATIOS, ': no centre or home rows for ', geo)
  }
  i_c <- centre$s[centre$state == geo] / natl_mean(centre)
  i_h <- home$s[home$state == geo] / natl_mean(home)
  if (is.na(i_h)) i_h <- 1

  cat(sprintf('   staffing index %-4s centre %.3f  home %.3f\n', geo, i_c, i_h))

  unknown <- setdiff(names(L_req), c(CENTRE_SECTORS, HOME_SECTORS))
  if (length(unknown)) stop('labor_requirements sector w/out index: ', unknown)
  for (k in names(L_req)) {
    i <- if (k %in% CENTRE_SECTORS) i_c else i_h
    L_req[[k]] <- lapply(L_req[[k]], function(x) x * i)
  }
  L_req
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

  rows <- list()
  for (f in files) {
    st <- sub('[.]csv$', '', basename(f))

    r <- lapply(GROUPS, function(g) wage_ratio(st, g))
    w <- lapply(GROUPS, function(g) natl$wages[[g]] * r[[g]])
    # Computed in check mode too, so --check exercises both
    L_req    <- staffing_index(natl$labor_requirements, st)
    residual <- rpp_scale(natl$per_unit_residual, st)

    if (!check_only) {
      dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
      # No labor_supply -- 3a sets state L_0 from base-year sector hours
      out <- natl
      out$labor_supply <- NULL
      out$wages        <- w
      out$labor_requirements <- L_req
      out$per_unit_residual  <- residual
      write_yaml(out, file.path(OUT_DIR, paste0(st, '.yaml')))
    }

    rows[[st]] <- tibble::tibble(
      state                  = st,
      wage_ratio_no_ba       = r$no_ba,
      wage_ratio_ba          = r$ba,
      w_no_ba                = w$no_ba,
      w_ba                   = w$ba)
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
  cat('   wage_ratio = ACS state / national median hourly earnings, 2018-19\n',
      '   records; cell under 30 records takes all-worker ratio.\n',
      sep = '')
  if (!check_only) {
    cat('wrote ', OUT_DIR, '/<ST>.yaml\n', sep = '')
  }
  invisible(d)
}



if (sys.nframe() == 0) main()
