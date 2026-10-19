#------------------------------------------------------------------------------
# 16_ndcp_model_prices.R -- model base-year (2019) posted centre price vs NDCP
# 2019 county centre prices, by geography, under two non-labour residuals:
# housing-scaled (config/state/supply, build) and national (main).
#
# DIAGNOSTIC ONLY -- writes nothing simulator or 00_ reads. Definitions fixed
# before any result was looked at (2026-10-19 review):
#
# Price compared: POSTED sector price, not household payment. Model side =
#   base-year supply price per child-hour, L_req %*% w + per_unit_residual
#   (state_base_supply_prices(), same formula 3a uses for state-price alpha),
#   2019 dollars, base wages -- not equilibrium prices, no price wedge. x 40
#   -> $ per 40-hour week (NDCP converts hourly x 40, daily x 5).
# Tier aggregation: model centre price for age group a, geo g =
#   (P_low C_low,a + P_high C_high,a) / (C_low,a + C_high,a), C = ACS p0
#   children (child-weighted, kindergarten dropped, both hours bands) from
#   raw/US/US_ACS__p0_children_by_care_type_hours_age__2019.csv. p0 tier mix =
#   fixed comparison convention, not equal to re-anchored mix -- common centre
#   delta does not preserve aggregate tier mix (households differ in tier
#   probabilities, change total centre use by different amounts).
#   p0 file: cluster/acs_p0_shares_by_age.R on calibration interface
#   202610151431, ECEC_URBAN_DENSITY=1, ECEC_INCOME_RANK=national (Bouchet
#   runs/1015-main/p0_by_age.csv, logs/1015-main/post.sbatch; same md5).
# NDCP side: MCINFANT / MCTODDLER / MCPRESCHOOL, STUDYYEAR 2019, weekly
#   full-time median centre price by county (definitions: 11_ndcp_price_
#   comparison.R header). Geo price for age group = mean of county medians
#   weighted by Census PEP 2019 children 0-4, over counties w/ a price.
#   Main = every NDCP value (NDCP-imputed included); 'observed' sensitivity =
#   counties whose age-group prices carry no imputation digit 3-5.
# Age alignment: infant = ages 0-1, toddler = 2, preschool = 3-4 (NDCP
#   preschool stops at 54 months). Geo price = age groups weighted by model
#   paid-centre children (C_low + C_high) by age group in geo -- same weights
#   both sides. Geo needs an NDCP price in all three age groups, else excluded.
# Geographies: 50 states + DC compared; NYC row written, left out of summary
#   stats (overlaps NY). Excluded geos listed in summary.
# Summary per residual variant x NDCP variant, identical geos + weights for
#   both residuals: n, equal-weight means, child-weighted means (PEP 0-4),
#   ratio of means, mean log ratio, Pearson + Spearman (unweighted), RMSE.
#
# Inputs: NDCP county xlsx (git-ignored; raw/local/ or raw/US/), PEP county
#   0-4, p0 by age, config/state/supply/<ST>.yaml; national-residual yamls
#   generated to a temp dir by resources/supply/state_supply_params.R
#   --national-residual --out <tmp>.
#
# Usage: Rscript state_data/code/16_ndcp_model_prices.R
#   writes out/ndcp_model_prices.csv (geo x residual x age group),
#   out/ndcp_model_prices_summary.csv, out/ndcp_model_prices_provenance.txt
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(yaml)})

# read_ndcp(), AGE_GROUPS, constants, STATE_DATA_ROOT (11 runs main() only as script)
source('state_data/code/11_ndcp_price_comparison.R')
source('src/shared_functions/reprice_alpha_to_state_supply.R')

# NDCP file: documented home raw/local/, local copy may sit in raw/US/
NDCP_FILE <- Filter(file.exists, c(
  file.path(STATE_DATA_ROOT, 'raw', 'local', basename(NDCP_FILE)),
  file.path(STATE_DATA_ROOT, 'raw', 'US', basename(NDCP_FILE))))[1]
if (is.na(NDCP_FILE)) stop('MISSING INPUT: NDCP county xlsx -- copy from Bouchet ',
                           'shared/raw_data/DOL-NDCP/v1/2008-2022/ into state_data/raw/local/')

SUPPLY_DIR <- file.path('config', 'state', 'supply')
OUT_DET  <- file.path(STATE_DATA_ROOT, 'out', 'ndcp_model_prices.csv')
OUT_SUM  <- file.path(STATE_DATA_ROOT, 'out', 'ndcp_model_prices_summary.csv')
OUT_PROV <- file.path(STATE_DATA_ROOT, 'out', 'ndcp_model_prices_provenance.txt')
HOURS_PER_WEEK <- 40
SECTORS <- c('unpaid_center_based', 'low_price_center_based',
             'high_price_center_based', 'paid_home_based')



read_supply_yaml <- function(path) {

  #----------------------------------------------------------------------------
  # Supply yaml -> list(L_req, w, delta_0) in SECTOR order, the shape
  # state_base_supply_prices() takes (3a parser is internal to 3a).
  #
  # Params: path (chr) supply yaml
  # Returns: (list) L_req 4 x 2, w (2), delta_0 (4, NA = none)
  #----------------------------------------------------------------------------

  y <- yaml::read_yaml(path)
  L <- t(sapply(SECTORS, function(s) c(y$labor_requirements[[s]]$no_ba,
                                       y$labor_requirements[[s]]$ba)))
  d0 <- sapply(SECTORS, function(s) {
    v <- y$per_unit_residual[[s]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  })
  list(L_req = L, w = c(y$wages$no_ba, y$wages$ba), delta_0 = d0)
}



model_prices <- function(dir, label) {

  #----------------------------------------------------------------------------
  # Base-year low / high centre price per child-hour, each geo yaml in dir.
  #
  # Params: dir (chr) supply yaml dir; label (chr) residual variant name
  # Returns: (tibble) geo, residual, p_low, p_high ($/child-hour, 2019)
  #----------------------------------------------------------------------------

  files <- list.files(dir, pattern = '\\.yaml$', full.names = TRUE)
  bind_rows(lapply(files, function(f) {
    P <- state_base_supply_prices(read_supply_yaml(f))
    tibble(geo = sub('\\.yaml$', '', basename(f)), residual = label,
           p_low = P[2], p_high = P[3])
  }))
}



md5 <- function(f) unname(tools::md5sum(f))



main <- function() {

  #----------------------------------------------------------------------------
  # Builds detail + summary tables; writes OUT_DET, OUT_SUM, OUT_PROV.
  #
  # Returns: (list) detail, summary tibbles, invisibly
  #----------------------------------------------------------------------------

  # National-residual yamls, same code as build files, temp dir
  tmp <- file.path(tempdir(), 'supply_national_residual')
  dir.create(tmp, showWarnings = FALSE)
  rc <- system2('Rscript', c('resources/supply/state_supply_params.R',
                             '--national-residual', '--out', tmp), stdout = FALSE)
  if (rc != 0) stop('state_supply_params.R --national-residual failed (exit ', rc, ')')
  # Generator must reproduce build yamls exactly when run w/o flag -- else
  # the two variants differ by more than the residual
  tmp_b <- file.path(tempdir(), 'supply_build')
  dir.create(tmp_b, showWarnings = FALSE)
  rc <- system2('Rscript', c('resources/supply/state_supply_params.R', '--out', tmp_b),
                stdout = FALSE)
  if (rc != 0) stop('state_supply_params.R failed (exit ', rc, ')')
  build_files <- list.files(SUPPLY_DIR, pattern = '\\.yaml$')
  stale <- build_files[md5(file.path(SUPPLY_DIR, build_files)) !=
                         md5(file.path(tmp_b, build_files))]
  if (length(stale)) stop('config/state/supply out of date vs generator: ',
                          paste(stale, collapse = ', '))

  mp <- bind_rows(model_prices(SUPPLY_DIR, 'housing_scaled'),
                  model_prices(tmp, 'national'))

  # Model paid-centre children by tier, age group
  ag <- bind_rows(lapply(names(AGE_GROUPS), function(a)
    tibble(age = AGE_GROUPS[[a]]$ages, age_group = a)))
  tiers <- read_csv(P0_FILE, show_col_types = FALSE) %>%
    filter(kindergarten == 0,
           ecec_type %in% c('Low-Priced Center-Based', 'High-Priced Center-Based')) %>%
    inner_join(ag, by = 'age') %>%
    group_by(geo, age_group) %>%
    summarise(c_low  = sum(children[ecec_type == 'Low-Priced Center-Based']),
              c_high = sum(children[ecec_type == 'High-Priced Center-Based']),
              .groups = 'drop')

  # NDCP geo x age group prices, all values + observed only
  pep <- read_csv(PEP_FILE, col_types = cols(.default = 'c')) %>%
    transmute(fips = paste0(STATE, COUNTY), kids0_4 = as.numeric(TOT_POP))
  nd <- read_ndcp() %>% filter(st %in% names(STATE_POSTAL_TO_FIPS)) %>%
    inner_join(pep, by = 'fips')
  nyc <- SUBSTATE_GEOGRAPHIES[['NYC']]
  nyc_fips <- paste0(sprintf('%02d', nyc$statefip), sprintf('%03d', nyc$countyfip))
  nd <- bind_rows(nd %>% mutate(geo = st),
                  nd %>% filter(fips %in% nyc_fips) %>% mutate(geo = 'NYC'))
  ndcp <- nd %>%
    group_by(geo, age_group) %>%
    summarise(
      ndcp_all = if (any(!is.na(price)))
        weighted.mean(price[!is.na(price)], kids0_4[!is.na(price)]) else NA_real_,
      ndcp_observed = if (any(!is.na(price) & !imputed))
        weighted.mean(price[!is.na(price) & !imputed], kids0_4[!is.na(price) & !imputed])
        else NA_real_,
      kids0_4_priced = sum(kids0_4[!is.na(price)]),
      .groups = 'drop')
  kids <- nd %>% filter(age_group == 'infant') %>% group_by(geo) %>%
    summarise(kids0_4 = sum(kids0_4), .groups = 'drop')

  detail <- mp %>%
    inner_join(tiers, by = 'geo', relationship = 'many-to-many') %>%
    left_join(ndcp, by = c('geo', 'age_group')) %>%
    mutate(paid_centre_children = c_low + c_high,
           model_week = HOURS_PER_WEEK * (p_low * c_low + p_high * c_high) / paid_centre_children) %>%
    arrange(residual, geo, age_group)

  # Geo level: age groups weighted by model paid-centre children
  geo_tab <- function(ndcp_col) {
    detail %>%
      group_by(residual, geo) %>%
      summarise(n_age = sum(!is.na(.data[[ndcp_col]])),
                model_week = sum(paid_centre_children * model_week) / sum(paid_centre_children),
                ndcp_week = sum(paid_centre_children * .data[[ndcp_col]]) / sum(paid_centre_children),
                .groups = 'drop') %>%
      left_join(kids, by = 'geo')
  }

  summ <- bind_rows(lapply(c('ndcp_all', 'ndcp_observed'), function(v) {
    g <- geo_tab(v)
    in_scope <- g$geo %in% names(STATE_POSTAL_TO_FIPS)
    excl <- sort(unique(g$geo[in_scope & g$n_age < length(AGE_GROUPS)]))
    keep <- g %>% filter(geo %in% names(STATE_POSTAL_TO_FIPS), !geo %in% excl)
    # Same geos for both residuals by construction (geo set from yaml names)
    stopifnot(setequal(keep$geo[keep$residual == 'housing_scaled'],
                       keep$geo[keep$residual == 'national']))
    keep %>%
      group_by(residual) %>%
      summarise(
        ndcp_variant = v,
        n_geos = n(),
        model_mean_equal = mean(model_week),
        ndcp_mean_equal = mean(ndcp_week),
        ratio_of_means_equal = mean(model_week) / mean(ndcp_week),
        model_mean_child = weighted.mean(model_week, kids0_4),
        ndcp_mean_child = weighted.mean(ndcp_week, kids0_4),
        ratio_of_means_child = weighted.mean(model_week, kids0_4) / weighted.mean(ndcp_week, kids0_4),
        mean_log_ratio_equal = mean(log(model_week / ndcp_week)),
        rmse_equal = sqrt(mean((model_week - ndcp_week)^2)),
        pearson = cor(model_week, ndcp_week),
        spearman = cor(model_week, ndcp_week, method = 'spearman'),
        excluded = paste(excl, collapse = ';'),
        .groups = 'drop')
  }))

  write.csv(detail, OUT_DET, row.names = FALSE)
  write.csv(summ, OUT_SUM, row.names = FALSE)

  git <- function(...) tryCatch(system2('git', c(...), stdout = TRUE), error = function(e) NA)
  # Code hashed too -- commit id alone misses uncommitted scripts
  code <- c('state_data/code/16_ndcp_model_prices.R',
            'state_data/code/11_ndcp_price_comparison.R',
            'state_data/code/shared_setup.R',
            'src/shared_functions/constants.R',
            'src/shared_functions/reprice_alpha_to_state_supply.R',
            'resources/supply/state_supply_params.R')
  inputs <- c(NDCP_FILE, PEP_FILE, P0_FILE, file.path(SUPPLY_DIR, build_files))
  writeLines(c(
    paste('script: state_data/code/16_ndcp_model_prices.R'),
    paste('run at:', format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')),
    paste('git commit:', git('rev-parse', 'HEAD')),
    paste('git dirty files:', paste(git('status', '--porcelain', '--untracked-files=no'), collapse = ' | ')),
    paste('R:', R.version.string),
    'settings: model price = 2019 base supply price x 40; tier weights = p0 children;',
    '  NDCP = 2019 county medians, PEP 0-4 weights; ages infant 0-1, toddler 2, preschool 3-4;',
    '  geo = age groups weighted by model paid-centre children; NYC out of summary',
    paste('p0 input source: cluster/acs_p0_shares_by_age.R, calibration interface 202610151431,',
          'ECEC_URBAN_DENSITY=1, ECEC_INCOME_RANK=national'),
    paste('git status of code files:',
          paste(git('status', '--porcelain', '--', code), collapse = ' | ')),
    'code (md5):',
    paste0('  ', md5(code), '  ', code),
    'inputs (md5):',
    paste0('  ', md5(inputs), '  ', inputs)
  ), OUT_PROV)

  cat('wrote ', OUT_DET, '\nwrote ', OUT_SUM, '\nwrote ', OUT_PROV, '\n', sep = '')
  invisible(list(detail = detail, summary = summ))
}



if (sys.nframe() == 0) main()
