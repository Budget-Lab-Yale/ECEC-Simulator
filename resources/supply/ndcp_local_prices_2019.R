#------------------------------------------------------------------------------
# ndcp_local_prices_2019.R -- 2019 local care price per child-hour, centre and
# paid home, every geography (50 states, DC, NYC). Feeds local per-unit
# residual in state_supply_params.R: residual = local price - local labour cost.
#
# NDCP side: 2019 county median full-time weekly price (centre MC*, family
# child care MFCC*; infant, toddler, preschool -- 11_ndcp_price_comparison.R
# AGE_GROUPS), all values incl. NDCP-imputed. Geo x age group price = mean of
# county prices weighted by Census PEP 2019 children 0-4, counties w/ a price.
# NYC = five NYC counties.
#
# Geo price = age groups weighted by model children in the setting (p0, ACS
# 2019, kindergarten dropped): centre = Low- + High-Priced Center-Based,
# home = Paid Home-Based. Age group w/out NDCP price -> geo price over
# observed groups x national ratio (all groups / observed groups, national
# age-group prices at geo's weights) -- missing age does not shift level.
# Geo w/out any 2019 price: nearest NDCP year(s) 2016-2022 (ties averaged),
# each age group x national 2019 / that-year price. No NDCP price in any year
# 2008-2022 (IN, NM): ndcp_ratio imputed -- log NDCP ratio regressed on log
# OEWS childcare wage ratios (no BA, BA) + log BEA 2019 RPP over the states w/
# a ratio, per setting (leave-one-out mean abs error 12.5% centre, 12.0% home,
# vs 20.2%, 25.7% for the earlier NSECE region x density price; scratch test
# 2026-11-06). Same formula as other geos from there.
#
# Level: NSECE national price kept (beta, rho calibrated at NSECE prices) --
# NDCP gives geography only, as OES does for wages (oes_childcare_wage_ratios_
# 2019.R: w_0 = NSECE wage x state / national ratio, composition fixed at
# national weights). target price = NSECE national price x ndcp_ratio, where
# ndcp_ratio = NDCP geo price / NDCP US price, both at NATIONAL model age
# weights (US = PEP-0-4-weighted county mean, 2019), NSECE centre price at
# national tier mix (low-tier share of model centre children, 50 states +
# DC), home: NSECE home price. Ratio moves w/ local prices only, not w/ geo's
# age or tier mix (Anna, 2026-11-06). Before: NDCP geo price at geo's own age
# weights x one level (NSECE price at each geo's tier mix / NDCP, both
# child-weighted over states).
#
# Usage: Rscript resources/supply/ndcp_local_prices_2019.R
#   writes resources/supply/US_DOL_NDCP__local_care_prices__2019.csv (+ .provenance.txt)
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(yaml)})

# read_ndcp(), AGE_GROUPS, NDCP_FILE, PEP_FILE, P0_FILE, SUBSTATE_GEOGRAPHIES
source('state_data/code/11_ndcp_price_comparison.R')

NATIONAL <- file.path('resources', 'supply', 'US_NSECE__national_supply_parameters__2019.yaml')
OUT      <- file.path('resources', 'supply', 'US_DOL_NDCP__local_care_prices__2019.csv')
TRACTS   <- file.path('resources', 'census_population', 'US_Census__tract_urban_rural_children_under5__2010.csv')
WAGES    <- file.path('resources', 'supply', 'state_childcare_wage_ratios_oes.csv')   # oes_childcare_wage_ratios_2019.R
RPP      <- file.path('resources', 'bea', 'US_BEA__regional_price_parities_all_items__2019.csv')
YEARS    <- 2016:2022   # nearest-year fallback window
# Census region (NSECE CB9 / HH9 _REGION: 1 Northeast, 2 Midwest, 3 South, 4 West)
REGION_OF <- c(CT = 1, ME = 1, MA = 1, NH = 1, RI = 1, VT = 1, NJ = 1, NY = 1, PA = 1,
               IL = 2, IN = 2, MI = 2, OH = 2, WI = 2, IA = 2, KS = 2, MN = 2, MO = 2, NE = 2, ND = 2, SD = 2,
               DE = 3, FL = 3, GA = 3, MD = 3, NC = 3, SC = 3, VA = 3, DC = 3, WV = 3, AL = 3, KY = 3,
               MS = 3, TN = 3, AR = 3, LA = 3, OK = 3, TX = 3,
               AZ = 4, CO = 4, ID = 4, MT = 4, NV = 4, NM = 4, UT = 4, WY = 4, AK = 4, CA = 4, HI = 4,
               OR = 4, WA = 4)
SETTING_TYPES <- list(center = c('Low-Priced Center-Based', 'High-Priced Center-Based'),
                      home   = 'Paid Home-Based')



national_prices <- function(path = NATIONAL) {

  #----------------------------------------------------------------------------
  # NSECE 2019 national price per child-hour by sector: L_req %*% w + residual.
  #
  # Params:
  #   - path (chr): national supply yaml
  #
  # Returns: (num vec) low, high, home
  #----------------------------------------------------------------------------

  y <- read_yaml(path)
  p <- function(s) sum(unlist(y$labor_requirements[[s]])[c('no_ba', 'ba')] *
                         unlist(y$wages)[c('no_ba', 'ba')]) + y$per_unit_residual[[s]]
  c(low = p('low_price_center_based'), high = p('high_price_center_based'), home = p('paid_home_based'))
}



geo_prices <- function(setting, weights, pep) {

  #----------------------------------------------------------------------------
  # NDCP weekly price per geo for one setting, model age weights, missing age
  # groups adjusted by national ratio.
  #
  # Params:
  #   - setting (chr): 'center' or 'home'
  #   - weights (tibble): geo, age_group, w (model children)
  #   - pep (tibble): fips, kids0_4
  #
  # Returns: (tibble) geo, ndcp_week, age_groups_observed
  #----------------------------------------------------------------------------

  nd <- read_ndcp(NULL, setting) %>% filter(st %in% names(STATE_POSTAL_TO_FIPS), year %in% YEARS) %>%
    inner_join(pep, by = 'fips')
  nyc <- SUBSTATE_GEOGRAPHIES[['NYC']]
  nyc_fips <- paste0(sprintf('%02d', nyc$statefip), sprintf('%03d', nyc$countyfip))
  nd <- bind_rows(nd %>% mutate(geo = st), nd %>% filter(fips %in% nyc_fips) %>% mutate(geo = 'NYC'))
  gya <- nd %>% filter(!is.na(price)) %>% group_by(geo, year, age_group) %>%
    summarise(p = weighted.mean(price, kids0_4), .groups = 'drop')
  usy <- nd %>% filter(!is.na(price), geo != 'NYC') %>% group_by(year, age_group) %>%
    summarise(p_us = weighted.mean(price, kids0_4), .groups = 'drop')
  # Year per geo: 2019, else nearest NDCP year(s) w/ a price (ties averaged),
  # scaled to 2019 by national age-group price change
  pick <- gya %>% distinct(geo, year) %>% mutate(dist = abs(year - 2019L)) %>%
    group_by(geo) %>% filter(dist == min(dist)) %>% ungroup()
  ga <- gya %>% inner_join(pick, by = c('geo', 'year')) %>%
    left_join(usy, by = c('year', 'age_group')) %>%
    left_join(usy %>% filter(year == 2019L) %>% select(age_group, p_us19 = p_us), by = 'age_group') %>%
    group_by(geo, age_group) %>%
    summarise(p = mean(p * p_us19 / p_us), ndcp_years = paste(sort(unique(year)), collapse = ';'), .groups = 'drop')
  us <- usy %>% filter(year == 2019L) %>% select(age_group, p_us)
  weights %>%
    left_join(ga, by = c('geo', 'age_group')) %>%
    left_join(us, by = 'age_group') %>%
    group_by(geo) %>%
    summarise(age_groups_observed = sum(!is.na(p)),
              ndcp_years = paste(unique(na.omit(ndcp_years)), collapse = ';'),
              obs   = sum((w * p)[!is.na(p)]) / sum(w[!is.na(p)]),
              ratio = sum(w * p_us) / sum(w) / (sum((w * p_us)[!is.na(p)]) / sum(w[!is.na(p)])),
              us_week = sum(w * p_us) / sum(w),
              .groups = 'drop') %>%
    mutate(ndcp_week = ifelse(age_groups_observed > 0, obs * ratio, NA_real_)) %>%
    select(geo, ndcp_week, us_week, age_groups_observed, ndcp_years)
}



parent_geo_postal <- function(geo) if (geo %in% names(SUBSTATE_GEOGRAPHIES)) 'NY' else geo



density_mix <- function(path = TRACTS) {

  #----------------------------------------------------------------------------
  # Geo child-under-5 shares by NSECE community urban density, 2010 tracts:
  # tract urban share >= 0.85 -> 1, >= 0.30 -> 2, else 3 (urban_density.R;
  # tract's own share stands in for NSECE's 2-mile circle). NYC = 5 counties.
  #
  # Params:
  #   - path (chr): acquire_tract_urban_children_2010.R output
  #
  # Returns: (tibble) geo, density, share
  #----------------------------------------------------------------------------

  tr <- read_csv(path, col_types = cols(.default = 'c')) %>%
    mutate(across(c(pop_total, pop_urban, children_under5), as.numeric),
           u = ifelse(pop_total > 0, pop_urban / pop_total, 0),
           density = ifelse(u >= 0.85, 1L, ifelse(u >= 0.30, 2L, 3L)),
           geo = names(STATE_POSTAL_TO_FIPS)[match(as.integer(state), STATE_POSTAL_TO_FIPS)])
  nyc <- SUBSTATE_GEOGRAPHIES[['NYC']]
  bind_rows(tr, tr %>% filter(as.integer(state) == nyc$statefip[1], as.integer(county) %in% nyc$countyfip) %>%
              mutate(geo = 'NYC')) %>%
    group_by(geo, density) %>% summarise(k = sum(children_under5), .groups = 'drop') %>%
    group_by(geo) %>% mutate(share = k / sum(k)) %>% ungroup() %>% select(geo, density, share)
}



main <- function() {

  #----------------------------------------------------------------------------
  # Writes OUT: one row per geo x setting.
  #
  # Returns: (tibble) output rows, invisibly
  #----------------------------------------------------------------------------

  for (f in c(NDCP_FILE, PEP_FILE, P0_FILE, NATIONAL)) if (!file.exists(f)) stop('MISSING INPUT: ', f)
  pn <- national_prices()
  pep <- read_csv(PEP_FILE, col_types = cols(.default = 'c')) %>%
    transmute(fips = paste0(STATE, COUNTY), state = as.integer(STATE), kids0_4 = as.numeric(TOT_POP))
  ag <- bind_rows(lapply(names(AGE_GROUPS), function(a) tibble(age = AGE_GROUPS[[a]]$ages, age_group = a)))
  p0 <- read_csv(P0_FILE, show_col_types = FALSE) %>% filter(kindergarten == 0) %>% inner_join(ag, by = 'age')

  out <- bind_rows(lapply(names(SETTING_TYPES), function(s) {
    cs <- p0 %>% filter(ecec_type %in% SETTING_TYPES[[s]])
    # National age weights (50 states + DC), same for every geo
    nat_w <- cs %>% filter(geo %in% names(STATE_POSTAL_TO_FIPS)) %>% group_by(age_group) %>%
      summarise(w = sum(children), .groups = 'drop')
    wt <- tidyr::crossing(geo = unique(cs$geo), nat_w)
    tier <- p0 %>% filter(ecec_type %in% SETTING_TYPES$center) %>% group_by(geo) %>%
      summarise(low_tier_share = sum(children[ecec_type == 'Low-Priced Center-Based']) / sum(children),
                .groups = 'drop')
    cn <- p0 %>% filter(ecec_type %in% SETTING_TYPES$center, geo %in% names(STATE_POSTAL_TO_FIPS))
    nat_low <- sum(cn$children[cn$ecec_type == 'Low-Priced Center-Based']) / sum(cn$children)
    nsece_national <- if (s == 'center') nat_low * pn['low'] + (1 - nat_low) * pn['high'] else pn['home']
    geo_prices(s, wt, pep %>% select(fips, kids0_4)) %>% left_join(tier, by = 'geo') %>%
      mutate(setting = s, nsece_national = unname(nsece_national), ndcp_ratio = ndcp_week / us_week,
             target_price = nsece_national * ndcp_ratio,
             source = ifelse(is.na(ndcp_week), NA_character_, paste0('ndcp_', gsub(';', '_', ndcp_years))))
  })) %>%
    select(geo, setting, ndcp_week, us_week, ndcp_ratio, age_groups_observed, ndcp_years, low_tier_share,
           nsece_national, target_price, source) %>%
    arrange(setting, geo)

  # No NDCP price in any year (IN, NM): ndcp_ratio from OEWS wage ratios + RPP
  wr <- read_csv(WAGES, show_col_types = FALSE) %>% select(geo = state, educ_group, wage_ratio) %>%
    tidyr::pivot_wider(names_from = educ_group, values_from = wage_ratio)
  rp <- read_csv(RPP, show_col_types = FALSE) %>% filter(bea_geography == 'state') %>%
    select(geo, rpp = rpp_all_items_2019)
  x <- out %>% left_join(wr, by = 'geo') %>% left_join(rp, by = 'geo')
  miss <- which(is.na(out$target_price))
  for (s in unique(out$setting[miss])) {
    fit_rows <- x$setting == s & x$geo %in% names(STATE_POSTAL_TO_FIPS) & !is.na(x$ndcp_ratio)
    fit <- lm(log(ndcp_ratio) ~ log(no_ba) + log(ba) + log(rpp), data = x[fit_rows, ])
    i <- miss[out$setting[miss] == s]
    out$ndcp_ratio[i]   <- exp(predict(fit, newdata = x[i, ]))
    out$target_price[i] <- out$nsece_national[i] * out$ndcp_ratio[i]
    out$source[i]       <- 'imputed_oews_rpp'
    cat(s, ': ndcp_ratio imputed for', paste(out$geo[i], collapse = ', '), '--',
        paste(round(out$ndcp_ratio[i], 3), collapse = ', '), '(R2', round(summary(fit)$r.squared, 3),
        'over', sum(fit_rows), 'states)\n')
  }

  geos <- c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))
  for (s in names(SETTING_TYPES)) {
    x <- out[out$setting == s, ]
    if (!setequal(x$geo, geos) || any(!is.finite(x$target_price) | x$target_price <= 0)) {
      stop(s, ': geographies missing or local price invalid')
    }
  }
  write_csv(out, OUT)
  inputs <- c(NDCP_FILE, PEP_FILE, P0_FILE, NATIONAL, WAGES, RPP)
  writeLines(c('script: resources/supply/ndcp_local_prices_2019.R',
               paste('run at:', format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')),
               paste0('input md5: ', unname(tools::md5sum(inputs)), '  ', inputs)),
             paste0(OUT, '.provenance.txt'))
  cat('wrote', OUT, '\n')
  print(out %>% group_by(setting, source) %>%
          summarise(n = n(), min = min(target_price), max = max(target_price), .groups = 'drop'))
  invisible(out)
}



if (sys.nframe() == 0) main()
