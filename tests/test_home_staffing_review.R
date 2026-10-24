# Home price column isolation, configuration audit coverage, unchanged supply.
suppressPackageStartupMessages({library(testthat); library(dplyr); library(readr)})
source('state_data/code/11_ndcp_price_comparison.R')
source('state_data/code/review_home_staffing_inputs.R')

test_that('home prices use home imputation flags and preserve center defaults', {
  x <- data.frame(STUDYYEAR = '2019', STATE_FIPS = '36', COUNTY_FIPS_CODE = '36061',
                  STATE_ABBREVIATION = 'NY', TOTALPOP = '100')
  for (setting in c('center', 'home')) {
    for (g in ndcp_age_columns(setting)) {
      x[[g$price]] <- if (setting == 'home') '170' else '250'
      for (col in paste0('i', g$six)) x[[col]] <- '00000'
    }
  }
  x$iMFCCBto5 <- '00100'
  center <- ndcp_county_rows(x)
  home <- ndcp_county_rows(x, 'home')
  expect_equal(center$price, rep(250, 3))
  expect_false(any(center$imputed))
  expect_equal(home$price, rep(170, 3))
  expect_equal(home$imputed, c(TRUE, FALSE, FALSE))
  expect_equal(home$fips, rep('36061', 3))
  x$iMFCCBto5 <- NA_character_
  expect_true(is.na(ndcp_county_rows(x, 'home')$imputed[1]))
  expect_error(ndcp_county_rows(x, 'invalid'))
})

test_that('all home inputs audited without promoting unresolved assumptions', {
  supply <- list.files('config/state/supply', full.names = TRUE, pattern = '\\.yaml$')
  before <- tools::md5sum(supply)
  audit <- review_home_staffing_inputs()
  expect_equal(nrow(audit), 52)
  expect_false(any(audit$ready_for_production))
  expect_equal(audit$minimum_documented_adults[match(c('AR', 'RI', 'WA'), audit$geo)], c(2, 2, 2))
  expect_equal(audit$maximum_documented_adults[audit$geo == 'AR'], 3)
  expect_true(all(audit$adult_count_status[is.na(audit$minimum_documented_adults)] ==
                    'unresolved_configuration_mapping'))
  expect_identical(tools::md5sum(supply), before)
})

cat('Home review regressions passed.\n')
