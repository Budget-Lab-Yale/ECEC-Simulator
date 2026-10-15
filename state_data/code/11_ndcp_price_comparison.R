#------------------------------------------------------------------------------
# 11_ndcp_price_comparison.R -- NDCP 2019 county centre prices by age group,
# child-weighted to each geography, set beside model high-priced centre share
# by same age group.
#
# DIAGNOSTIC ONLY -- writes nothing simulator or 00_ reads.
#
# Source: DOL Women's Bureau National Database of Childcare Prices 2008-2022,
# county file, STUDYYEAR 2019 (92 MB, git-ignored -- copy from Bouchet
# shared/raw_data/DOL-NDCP/v1/2008-2022/):
#   raw/local/US_DOL__national_database_childcare_prices_county__2008-2022.xlsx
# Definitions, NDCP Technical Report (ICF, Oct 2020) pp. 5-6, App. D
# (raw/US/US_DOL__ndcp_technical_report__2020-10.pdf):
#   MCINFANT / MCTODDLER / MCPRESCHOOL  weekly full-time median centre price,
#     $ per week, for 0-23 / 24-35 / 36-54 months; mode of six-month groups
#     (MCBto5 ... MC48to53) in range, highest mode on tie
#   *_flag  AGGREGATION flag, not imputation: 1 all six-month prices equal,
#     2 mode, 3 highest of tied modes
#   iMC<age>  five-digit IMPUTATION flag per six-month group, digit = 1 when
#     step ran: 1 price from other age group, 2 converted to weekly mode,
#     3 county price from statewide data (regression on ACS), 4 median from
#     75th percentile, 5 interpolated between MRS years
#   full time  no NDCP-wide definition; each state's market rate survey sets
#     it (Maine 21-30 hrs, Indiana 35, Michigan 45, New Jersey 51.7; several
#     states let respondents decide). Hourly -> weekly x 40, daily x 5.
# Here: county age-group price "observed" when every six-month group in range
# has digits 3-5 all 0 (2019 MRS value for county or its rate cluster);
# digits 1-2 (age fill, unit conversion) ignored.
#
# Weights: children 0-4, Census PEP 2019 county
#   (raw/US/US_PEP__population_children_age_0_4_by_county__2019.csv). NDCP
#   has no under-5 count; TOTALPOP (all ages) kept only to reproduce old memo
#   figure. Result = child-weighted MEAN OF COUNTY MEDIANS -- not a state
#   median: no provider-level prices, county medians of different sizes.
#   Weighted median of county medians shown too, same caveat.
#
# Model side: p0 children (ACS child-weighted) by care type, age
#   (raw/US/US_ACS__p0_children_by_care_type_hours_age__2019.csv), kindergarten
#   children dropped. high_share = High-Priced / (High- + Low-Priced) centre
#   children. Age map: infant = age 0-1, toddler = 2, preschool = 3-4 (NDCP
#   preschool stops at 54 months -- age 4 half out).
# Age-standardised row ('std'): fixed weights infant 0.4, toddler 0.2,
#   preschool 0.4 (years of age covered) on both price, high_share.
#
# Geographies: 50 states + DC + NYC (five boroughs, SUBSTATE_GEOGRAPHIES); NY =
# whole state; NY_rest = NY minus NYC (NDCP counties, p0 children) -- NYC vs
# NY overlap, NYC vs NY_rest doesn't. Correlations over 50 states + DC,
# unweighted, three price variants: nominal; real (/ state RPP / 100, BEA all
# items 2019 -- broad basket, not childcare); observed (child-weighted mean
# over observed counties only, states w/ any observed county).
#
# Usage: Rscript state_data/code/11_ndcp_price_comparison.R
#   writes out/ndcp_price_comparison.csv, out/ndcp_price_correlation.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})

# STATE_POSTAL_TO_FIPS, SUBSTATE_GEOGRAPHIES; STATE_DATA_ROOT, show_df()
source('src/shared_functions/constants.R')
source('state_data/code/shared_setup.R')



# Weighted median: smallest x whose cumulative weight share reaches 0.5.
# Deterministic, no interpolation. x (num vec), w (num vec, > 0); returns NA when x empty
weighted_median <- function(x, w) {
  if (length(x) == 0) return(NA_real_)
  o <- order(x)
  x <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  x[which(cw >= 0.5 - 1e-12)[1]]
}

NDCP_FILE <- file.path(STATE_DATA_ROOT, 'raw', 'local',
                       'US_DOL__national_database_childcare_prices_county__2008-2022.xlsx')
PEP_FILE  <- file.path(STATE_DATA_ROOT, 'raw', 'US',
                       'US_PEP__population_children_age_0_4_by_county__2019.csv')
P0_FILE   <- file.path(STATE_DATA_ROOT, 'raw', 'US',
                       'US_ACS__p0_children_by_care_type_hours_age__2019.csv')
RPP_FILE  <- file.path('resources', 'supply',
                       'US_BEA__regional_price_parities_all_items__2019.csv')
OUT_CMP   <- file.path(STATE_DATA_ROOT, 'out', 'ndcp_price_comparison.csv')
OUT_COR   <- file.path(STATE_DATA_ROOT, 'out', 'ndcp_price_correlation.csv')

# Age group -> NDCP price column, six-month groups in range, model ages
AGE_GROUPS <- list(
  infant    = list(price = 'MCINFANT', six = c('MCBto5', 'MC6to11', 'MC12to17', 'MC18to23'),
                   ages = 0:1),
  toddler   = list(price = 'MCTODDLER', six = c('MC24to29', 'MC30to35'), ages = 2),
  preschool = list(price = 'MCPRESCHOOL', six = c('MC36to41', 'MC42to47', 'MC48to53'),
                   ages = 3:4)
)
STD_WEIGHTS <- c(infant = 0.4, toddler = 0.2, preschool = 0.4)



read_ndcp <- function() {

  #----------------------------------------------------------------------------
  # NDCP 2019 county rows, long by age group.
  #
  # Returns: (tibble) fips, st, age_group, price ($/wk), totalpop, digit
  #   indicators d3, d4, d5 (any six-month group in range), imputed (any)
  #----------------------------------------------------------------------------

  if (!file.exists(NDCP_FILE)) stop('MISSING INPUT: ', NDCP_FILE,
                                    ' -- copy from Bouchet shared/raw_data/DOL-NDCP/')
  # All text: five-digit flags keep leading zeros
  d <- readxl::read_excel(NDCP_FILE, sheet = 1, col_types = 'text')
  d <- d[d$STUDYYEAR == '2019', ]
  sf <- sprintf('%02d', as.integer(d$STATE_FIPS))
  cf <- as.integer(d$COUNTY_FIPS_CODE)
  # COUNTY_FIPS_CODE full 5-digit in file; guard against 3-digit form
  fips <- if (max(cf, na.rm = TRUE) < 1000) paste0(sf, sprintf('%03d', cf)) else sprintf('%05d', cf)

  dplyr::bind_rows(lapply(names(AGE_GROUPS), function(a) {
    g <- AGE_GROUPS[[a]]
    codes <- as.matrix(d[, paste0('i', g$six)])
    dig <- function(k) apply(substr(codes, k, k) == '1', 1, any)
    tibble::tibble(fips = fips, st = d$STATE_ABBREVIATION, age_group = a,
                   price = as.numeric(d[[g$price]]),
                   totalpop = as.numeric(d$TOTALPOP),
                   d3 = dig(3), d4 = dig(4), d5 = dig(5))
  })) %>%
    dplyr::mutate(imputed = d3 | d4 | d5)
}



summarise_prices <- function(nd) {

  #----------------------------------------------------------------------------
  # Child-weighted price stats + imputation weight shares by geo, age group.
  #
  # Params: nd (tibble) read_ndcp() joined to PEP kids0_4, geo
  # Returns: (tibble) one row per geo x age group
  #----------------------------------------------------------------------------

  nd %>%
    dplyr::group_by(geo, age_group) %>%
    dplyr::summarise(
      n_counties      = dplyr::n(),
      kids0_4_total   = sum(kids0_4),     # new name -- reusing kids0_4 masks column
      pct_kids_no_price = 100 * sum(kids0_4[is.na(price)]) / sum(kids0_4),
      price_child_wmean  = weighted.mean(price[!is.na(price)], kids0_4[!is.na(price)]),
      price_child_wmedian = weighted_median(price[!is.na(price)], kids0_4[!is.na(price)]),
      price_totalpop_wmean = weighted.mean(price[!is.na(price)], totalpop[!is.na(price)]),
      price_obs_wmean = if (any(!is.na(price) & !imputed))
        weighted.mean(price[!is.na(price) & !imputed], kids0_4[!is.na(price) & !imputed])
        else NA_real_,
      pct_kids_observed = 100 * sum(kids0_4[!is.na(price) & !imputed]) / sum(kids0_4[!is.na(price)]),
      pct_kids_d3_statewide = 100 * sum(kids0_4[!is.na(price) & d3]) / sum(kids0_4[!is.na(price)]),
      pct_kids_d4_from_p75  = 100 * sum(kids0_4[!is.na(price) & d4]) / sum(kids0_4[!is.na(price)]),
      pct_kids_d5_interp    = 100 * sum(kids0_4[!is.na(price) & d5]) / sum(kids0_4[!is.na(price)]),
      .groups = 'drop')
}



main <- function() {

  #----------------------------------------------------------------------------
  # Builds comparison table, correlations; writes OUT_CMP, OUT_COR.
  #
  # Returns: (list) cmp, cor tibbles, invisibly
  #----------------------------------------------------------------------------

  pep <- readr::read_csv(PEP_FILE, col_types = readr::cols(.default = 'c')) %>%
    dplyr::transmute(fips = paste0(STATE, COUNTY), kids0_4 = as.numeric(TOT_POP))
  nd <- read_ndcp() %>% dplyr::filter(st %in% names(STATE_POSTAL_TO_FIPS))
  unmatched <- setdiff(nd$fips, pep$fips)
  if (length(unmatched)) stop('NDCP counties without PEP row: ',
                              paste(unique(unmatched), collapse = ', '))
  nd <- dplyr::inner_join(nd, pep, by = 'fips')

  # Geographies: each state (all counties), each sub-state geo (its counties)
  sub <- dplyr::bind_rows(lapply(names(SUBSTATE_GEOGRAPHIES), function(g) {
    s <- SUBSTATE_GEOGRAPHIES[[g]]
    f <- paste0(sprintf('%02d', s$statefip), sprintf('%03d', s$countyfip))
    nd %>% dplyr::filter(fips %in% f) %>% dplyr::mutate(geo = g)
  }))
  # NY_rest: NY counties outside NYC
  nyc_fips <- unique(sub$fips[sub$geo == 'NYC'])
  ny_rest <- nd %>% dplyr::filter(st == 'NY', !fips %in% nyc_fips) %>%
    dplyr::mutate(geo = 'NY_rest')
  nd_geo <- dplyr::bind_rows(nd %>% dplyr::mutate(geo = st), sub, ny_rest)
  us <- nd %>% dplyr::mutate(geo = 'US')
  prices <- summarise_prices(dplyr::bind_rows(nd_geo, us))

  # Model high-priced share by age group
  ag <- dplyr::bind_rows(lapply(names(AGE_GROUPS), function(a)
    tibble::tibble(age = AGE_GROUPS[[a]]$ages, age_group = a)))
  p0 <- readr::read_csv(P0_FILE, show_col_types = FALSE) %>%
    dplyr::filter(kindergarten == 0,
                  ecec_type %in% c('High-Priced Center-Based', 'Low-Priced Center-Based')) %>%
    dplyr::inner_join(ag, by = 'age')
  # NY_rest = NY minus NYC children, by care type, hours, age
  p0_rest <- p0 %>% dplyr::filter(geo == 'NY') %>%
    dplyr::left_join(p0 %>% dplyr::filter(geo == 'NYC') %>%
                       dplyr::select(ecec_type, hours_choice, age, kindergarten, nyc = children),
                     by = c('ecec_type', 'hours_choice', 'age', 'kindergarten')) %>%
    dplyr::mutate(children = children - dplyr::coalesce(nyc, 0), geo = 'NY_rest') %>%
    dplyr::select(-nyc)
  p0 <- dplyr::bind_rows(p0, p0_rest,
                         p0 %>% dplyr::filter(geo %in% names(STATE_POSTAL_TO_FIPS)) %>%
                           dplyr::mutate(geo = 'US'))
  model <- p0 %>%
    dplyr::group_by(geo, age_group) %>%
    dplyr::summarise(
      paid_centre_children = sum(children),
      high_share = sum(children[ecec_type == 'High-Priced Center-Based']) / sum(children),
      .groups = 'drop')

  cmp <- dplyr::inner_join(prices, model, by = c('geo', 'age_group'))
  std <- cmp %>%
    dplyr::group_by(geo) %>%
    dplyr::summarise(
      price_child_wmean = sum(STD_WEIGHTS[age_group] * price_child_wmean),
      high_share = sum(STD_WEIGHTS[age_group] * high_share),
      .groups = 'drop') %>%
    dplyr::mutate(age_group = 'std')     # after -- naming it first masks column
  cmp <- dplyr::bind_rows(cmp, std) %>%
    dplyr::mutate(age_group = factor(age_group, c(names(AGE_GROUPS), 'std'))) %>%
    dplyr::arrange(geo, age_group) %>%
    dplyr::mutate(age_group = as.character(age_group))

  # States w/ no NDCP 2019 centre price in any county drop out of correlations
  states <- cmp %>% dplyr::filter(geo %in% names(STATE_POSTAL_TO_FIPS))
  no_price <- sort(unique(states$geo[is.na(states$price_child_wmean) |
                                       is.nan(states$price_child_wmean)]))
  rpp <- readr::read_csv(RPP_FILE, show_col_types = FALSE) %>%
    dplyr::filter(bea_geography == 'state') %>%
    dplyr::select(geo, rpp = rpp_all_items_2019)
  # Three price variants, one row each per state x age group ('std' only nominal,
  # real -- observed has no fixed-weight version)
  variants <- states %>%
    dplyr::filter(!geo %in% no_price) %>%
    dplyr::left_join(rpp, by = 'geo') %>%
    dplyr::transmute(geo, age_group, high_share,
                     nominal = price_child_wmean,
                     real = price_child_wmean / (rpp / 100),
                     observed = price_obs_wmean) %>%
    tidyr::pivot_longer(c(nominal, real, observed), names_to = 'price_variant',
                        values_to = 'price') %>%
    dplyr::filter(!is.na(price), !(price_variant == 'observed' & age_group == 'std'))
  cor_tab <- variants %>%
    dplyr::group_by(price_variant, age_group) %>%
    dplyr::summarise(
      n_geos = dplyr::n(),
      price_min = min(price), price_max = max(price),
      high_share_min = min(high_share), high_share_max = max(high_share),
      pearson  = stats::cor(price, high_share),
      spearman = stats::cor(price, high_share, method = 'spearman'),
      .groups = 'drop') %>%
    dplyr::mutate(age_group = factor(age_group, c(names(AGE_GROUPS), 'std')),
                  price_variant = factor(price_variant, c('nominal', 'real', 'observed'))) %>%
    dplyr::arrange(price_variant, age_group) %>%
    dplyr::mutate(age_group = as.character(age_group),
                  price_variant = as.character(price_variant))

  write.csv(cmp, OUT_CMP, row.names = FALSE)
  write.csv(cor_tab, OUT_COR, row.names = FALSE)

  cat('== NDCP 2019 centre median price ($/week), child-weighted mean of county',
      'medians (PEP 0-4), vs model high-priced share\n\n')
  show_df(cmp %>% dplyr::filter(geo %in% c('US', 'NY', 'NYC', 'NY_rest'), age_group != 'std') %>%
            dplyr::transmute(geo, age_group, n_counties,
                             wmean = round(price_child_wmean), wmedian = round(price_child_wmedian),
                             totalpop_wmean = round(price_totalpop_wmean),
                             obs_pct = round(pct_kids_observed, 1),
                             high_share = round(high_share, 3)))
  cat('\n== across 50 states + DC w/ an NDCP price (unweighted correlation);',
      'no 2019 price:', paste(no_price, collapse = ', '), '\n\n')
  show_df(cor_tab %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))
  cat('\nwrote ', OUT_CMP, '\nwrote ', OUT_COR, '\n', sep = '')
  invisible(list(cmp = cmp, cor = cor_tab))
}



if (sys.nframe() == 0) main()
