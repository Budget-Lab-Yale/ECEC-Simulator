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
# state licensing staffing index, staffing_index(). Centre index = required
# staff per child (1 / licensed children-per-staff), averaged over ages 0-4 w/
# NSECE 2019 centre enrolment age mix, / same quantity averaged over 51
# states w/ PEP 2019 children 0-4 weights. Home index = 1 / licensed max
# children per family child care home, / 0-4-weighted mean over states that
# license homes; state w/out home licence -> 1. Centre index scales all three
# centre sectors, home index paid_home_based, both education groups alike.
# Ratios: reviewed October 1, 2019 rules, licensing_ratios_reviewed_2019.csv.
# 2017 study retained as baseline/fallback; source audit refreshed by 00_.
# Five representative ages (NSECE_AGE_POINT): 11 months, 18 months, 35 months,
# 3 years, 4 years for ages 0-4; one aggregate center index, no new choices.
# Home capacity excludes school-age-only additions; inverse capacity proxy
# doesn't adjust for assistants. Conditions retained in licensing registry.
# NYC own center rules, NYS home rules; excluded from national mean (inside NY).
# Historical 2014/2017, three/five-point comparisons remain diagnostics only.

#
# NOT CHANGED: elasticities stay national (no_ba 4, ba 2) -- assumed values
# (Borowsky et al. 2022, NBER w30140); no state evidence supports a proxy.
# delta_0 (per_unit_residual; non-labour cost + margin, P_supply = unit_cost +
# delta_0 * cpi_factor) = national residual x state BEA housing-services RPP /
# 100 (US_BEA__regional_price_parities_housing_services__2019.csv, line
# 'RPPs: Services: Housing'; NYC = New York MSA 35620). Applies to low-price
# centre, high-price centre, paid home; unpaid centre residual untouched (0).
# CHANGE vs main (one national residual per sector: 0.510055 low, 4.6145667
# high, 2.8273432 home). WHY: national
# residual gives every state same non-wage cost; housing-services prices proxy
# geographic variation in non-labour costs. NDCP check: state_data/code/
# 16_ndcp_model_prices.R (corr 0.89 housing-scaled vs 0.83 national, 46 states,
# out/ndcp_model_prices_summary.csv). Factor 1 = national mean, k = 1 (not fitted).
# --national-residual restores national delta_0.
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
HOUSING  <- file.path('resources', 'bea', 'US_BEA__regional_price_parities_housing_services__2019.csv')

GROUPS <- c(no_ba = 'no_ba', ba = 'ba')   # named -> lapply() keeps names

# staffing_index() inputs -- licensed children per staff by state, setting, age
# point (licensing_ratios_2019.R); county children 0-4
RATIOS  <- file.path(DIR, 'licensing_ratios_reviewed_2019.csv')
POP_0_4 <- file.path('resources', 'census_population',
                     'US_PEP__population_children_age_0_4_by_county__2019.csv')

# NSECE_CENTRE_ENROLLED (NSECE 2019 centre enrolment by age 0-4),
# NSECE_AGE_POINT (age 0-4 -> licensing age point in RATIOS), NSECE_AGE_BAND
# (age 0-4 -> 2014 band), NSECE_AGES_0_4 --
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



licensing_indices <- function(ratios = RATIOS, age_map = NSECE_AGE_POINT) {

  #----------------------------------------------------------------------------
  # Licensing staffing index, every geography in ratios file.
  # Centre: staff per child = sum_a w_a / r(geo, map(a)) / sum_a w_a, w =
  # NSECE_CENTRE_ENROLLED ages 0-4, map = age_map, r = licensed children per
  # staff; index = geo value / PEP children-0-4-weighted mean over 50 states
  # + DC. Home: 1 / max children, same weighting over states that license
  # homes; geo w/ home value NA (no home licence) -> index 1. NYC carries own
  # rows, not in national mean (inside NY).
  #
  # Params:
  #   - ratios (chr): csv w/ state, setting, age_group, children_per_staff
  #   - age_map (chr): age_0..age_4 -> centre age_group in ratios;
  #     NSECE_AGE_POINT (2017 file, 5 points), NSECE_AGE_BAND (2014 file)
  #
  # Returns: (tibble) state, centre, home -- indices
  #----------------------------------------------------------------------------

  require_input(ratios)
  require_input(POP_0_4)
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
    mutate(home = ifelse(is.na(home), 1, home))
}



staffing_index <- function(L_req, geo, indices = licensing_indices()) {

  #----------------------------------------------------------------------------
  # Scales national labor_requirements by geo's licensing staffing index
  # (licensing_indices() on RATIOS). Prints one line per geo.
  #
  # Params:
  #   - L_req (list): national labor_requirements, sector -> no_ba / ba
  #   - geo (chr): postal or sub-state code w/ RATIOS rows
  #
  # Returns: (list) L_req, centre sectors x centre index, home sectors x home
  #   index
  #----------------------------------------------------------------------------

  ix <- indices
  if (!geo %in% ix$state) stop(RATIOS, ': no centre or home rows for ', geo)
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



residual_factor <- function(geo) {

  #----------------------------------------------------------------------------
  # Housing-services RPP / 100 for geo (HOUSING; NYC = New York MSA). Scales
  # national per_unit_residual. 1 under --national-residual.
  #
  # Params:
  #   - geo (chr): postal or sub-state code w/ HOUSING row
  #
  # Returns: (dbl) factor, > 0
  #----------------------------------------------------------------------------

  if (NATIONAL_RESIDUAL) return(1)
  require_input(HOUSING)
  h <- read_csv(HOUSING, show_col_types = FALSE)
  v <- h$rpp_housing_services_2019[h$geo == geo]
  if (length(v) != 1 || !is.finite(v) || v <= 0) stop(HOUSING, ': no usable row for ', geo)
  v / 100
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

  indices <- licensing_indices()
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
    # Residual: unpaid centre (0) unchanged; paid sectors x housing factor
    f_res <- residual_factor(st)
    resid <- lapply(natl$per_unit_residual, function(x) x * f_res)

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
      residual_factor        = f_res)
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
