# Numerical contracts for annual state supply inputs and reviewed staffing.
source('src/shared_functions/get_state_supply_year_factors.R')
source('src/shared_functions/get_regional_price_parity.R')

path <- list(list(year = 2019L, no_ba = 1, ba = 1),
             list(year = 2020L, no_ba = 1.1, ba = 0.9))
stopifnot(identical(get_state_supply_year_factors(NULL, 2030), c(no_ba = 1, ba = 1)))
stopifnot(all(get_state_supply_year_factors(path, 2019) == 1))
stopifnot(all(get_state_supply_year_factors(path, 2030) == c(1.1, 0.9)))
bad <- list(path[[1]], list(year = 2021L, no_ba = 1.1, ba = 0.9))
stopifnot(inherits(tryCatch(get_state_supply_year_factors(bad, 2020), error = identity), 'error'))
bad[[1]]$ba <- 1.2
stopifnot(inherits(tryCatch(get_state_supply_year_factors(bad, 2021), error = identity), 'error'))

base <- read.csv('resources/bea/US_BEA__regional_price_parities_all_items__2019.csv')
stopifnot(max(abs(get_regional_price_parity(base$geo, 2019) - base$rpp_all_items_2019)) < 1e-8)
stopifnot(inherits(tryCatch(get_regional_price_parity('NY', 2035), error = identity), 'error'))
stopifnot(length(get_regional_price_parity(c('NY', 'NYC'), c(2019, 2024))) == 2)

audit <- read.csv('state_data/out/licensing_2019_audit.csv', stringsAsFactors = FALSE)
rules <- read.csv('resources/supply/licensing_ratios_reviewed_2019.csv', stringsAsFactors = FALSE)
keys <- c('state', 'setting', 'age_group')
joined <- merge(audit, rules, by = keys)
reviewed <- joined$status.x %in% c('verified_archive', 'verified_history', 'verified_conditional')
expected <- ifelse(reviewed, joined$verified_value, joined$baseline_value)
stopifnot(nrow(joined) == 312, all(joined$fallback == !reviewed))
stopifnot(isTRUE(all.equal(joined$children_per_staff, expected, check.attributes = FALSE)))
cat('PASS: annual state wage anchors, carry-forward, missing-year guards, RPP vintage, reviewed/fallback staffing\n')

# Controlled panel verifies geography-vs-US normalization and fixed-basket suppression.
source('resources/supply/annual_wage_ratios.R')
geo <- c('US', names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))
fixture <- expand.grid(year = 2019:2020, geo = geo,
                       occ_code = c('00-0000', OCC), stringsAsFactors = FALSE)
fixture$h_median <- match(fixture$occ_code, c('00-0000', OCC)) * 10 *
  ifelse(fixture$geo == 'US', 1, 2) *
  ifelse(fixture$year == 2020 & fixture$geo != 'US', 1.2, 1)
fixture$h_median[fixture$geo == 'AK' & fixture$year == 2020 & fixture$occ_code == OCC[1]] <- NA
fixture$method <- 'legacy'
in_path <- tempfile(fileext = '.csv')
out_path <- tempfile(fileext = '.csv')
readr::write_csv(fixture, in_path)
annual <- build_annual_wage_ratios(in_path, out_path)
stopifnot(all(abs(annual$relative_to_2019[annual$year == 2019] - 1) < 1e-12))
stopifnot(all(abs(annual$relative_to_2019[annual$year == 2020] - 1.2) < 1e-12))
stopifnot(!any(grepl(OCC[1], annual$occupations[annual$geo == 'AK'], fixed = TRUE)))
cat('PASS: annual wage extraction contract, fixed occupation basket across suppressed cells, 2019 normalization\n')
