#-----------------------------------------------------------------------------
# Do the NSECE constants in 00_main.R match out/nsece_universe.csv?
#
# 02_nsece_constants.R runs on the cluster, so its output reaches the repo by
# hand. Nothing tied the constants back to it -- 00_main.R could carry numbers
# no file on disk derives. This closes that: every constant the build uses is
# compared to the universe-A row it came from, key by key.
#
# Nothing is transcribed here. The constants come from sourcing 00_main.R
# (main() is guarded); the derivation comes from out/nsece_universe.csv.
#
#   Rscript state_data/tests/check_constants_match_derivation.R
#
# Exit status is 1 on any mismatch, any missing key, any extra key.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readr); library(dplyr)
})

CSV <- 'state_data/out/nsece_universe.csv'

# The build reads universe A -- NSECE's own five-hour screener. B is the
# 10-hour subset, kept for the universe ratio and for sensitivity.
UNIVERSE_A <- 'A: NSECE, any regular care'

TOL <- 1e-12   # same double, not a rounded copy

source('state_data/code/00_main.R')

d <- read_csv(CSV, col_types = cols(.default = col_character()))
d$value <- as.numeric(d$value)
d$n     <- as.numeric(d$n)
a <- d[d$universe == UNIVERSE_A, ]

failures <- 0
checked  <- 0



report <- function(ok, what, got, want) {

  #----------------------------------------------------------------------------
  # Counts one check in global `checked`; on failure bumps global `failures`,
  # prints FAIL line with both values at 17 digits.
  #
  # Params:
  #   - ok (logical): check passed; non-TRUE (incl. NA) counts as failure
  #   - what (chr): label, e.g. 'quantity[key]'
  #   - got (num): value in build
  #   - want (num): value in CSV
  #
  # Returns: (NULL) called for side effects
  #----------------------------------------------------------------------------

  checked <<- checked + 1
  if (!isTRUE(ok)) {
    failures <<- failures + 1
    cat(sprintf('FAIL %-52s build %-22s csv %s\n', what,
                format(got, digits = 17), format(want, digits = 17)))
  }
}



lookup <- function(quantity, key) {

  #----------------------------------------------------------------------------
  # Universe-A row of CSV for one quantity, key.
  #
  # Params:
  #   - quantity (chr): `quantity` column value
  #   - key (chr): `key` column value
  #
  # Returns: (tibble) single matching row, NULL unless exactly one
  #----------------------------------------------------------------------------

  r <- a[a$quantity == quantity & a$key == key, ]
  if (nrow(r) != 1) return(NULL)
  r
}



check_vec <- function(vec, quantity, keymap = identity) {

  #----------------------------------------------------------------------------
  # One named vector against one quantity, key by key; missing CSV row
  # counts as failure.
  #
  # Params:
  #   - vec (num): named constant from 00_main.R
  #   - quantity (chr): CSV quantity to compare against
  #   - keymap (function): maps vector name to CSV key; default identity
  #
  # Returns: (NULL) updates globals `checked`, `failures`
  #----------------------------------------------------------------------------

  for (nm in names(vec)) {
    r <- lookup(quantity, keymap(nm))
    if (is.null(r)) {
      checked <<- checked + 1; failures <<- failures + 1
      cat(sprintf('FAIL %-52s no csv row\n',
                  paste0(quantity, '[', nm, ']')))
      next
    }
    report(abs(vec[[nm]] - r$value) < TOL,
           paste0(quantity, '[', nm, ']'), vec[[nm]], r$value)
  }
}



check_tbl <- function(tbl, valcol, quantity) {

  #----------------------------------------------------------------------------
  # One region x type table against one quantity, value AND record count;
  # CSV key 'region:type', missing row counts as failure.
  #
  # Params:
  #   - tbl (tibble): constant from 00_main.R with region, type, n columns
  #   - valcol (chr): column of tbl holding value
  #   - quantity (chr): CSV quantity to compare against
  #
  # Returns: (NULL) updates globals `checked`, `failures`
  #----------------------------------------------------------------------------

  for (i in seq_len(nrow(tbl))) {
    key <- paste0(tbl$region[i], ':', tbl$type[i])
    r <- lookup(quantity, key)
    if (is.null(r)) {
      checked <<- checked + 1; failures <<- failures + 1
      cat(sprintf('FAIL %-52s no csv row\n', paste0(quantity, '[', key, ']')))
      next
    }
    report(abs(tbl[[valcol]][i] - r$value) < TOL,
           paste0(quantity, '[', key, ']'), tbl[[valcol]][i], r$value)
    report(tbl$n[i] == r$n,
           paste0(quantity, '[', key, '].n'), tbl$n[i], r$n)
  }
}

cat('== constants in 00_main.R vs', CSV, '\n')
cat('   universe:', UNIVERSE_A, '\n\n')

# Step 1. The universe ratio carries the NSCH rate up to universe A.
r <- lookup('children_all_over_10plus', 'national')
if (is.null(r)) {
  checked <- checked + 1; failures <- failures + 1
  cat('FAIL children_all_over_10plus[national]               no csv row\n')
} else {
  report(abs(NSECE_UNIVERSE_RATIO - r$value) < TOL,
         'NSECE_UNIVERSE_RATIO', NSECE_UNIVERSE_RATIO, r$value)
}

# Steps 2, 3, 4. Regional shares.
check_vec(NSECE_FORMAL_SHARE,        'formal_share')
check_vec(NSECE_UNPAID_CENTRE_SHARE, 'unpaid_centre_share')
check_vec(NSECE_CENTRE_HOME_SPLIT,   'centre_home_split')

# Step 5. National informal mix, before 00_main.R rescales it to sum to 1.
check_vec(NSECE_RESIDUAL_MIX * sum(NSECE_RESIDUAL_MIX), 'informal_mix')

# Step 6. National part-time shares, and the national hours fallback.
check_vec(NSECE_PART_TIME, 'part_time_share')

# NSECE_ANNUAL_HOURS is 50 x the national weekly mean, except Center-Based,
# which blends the two paid-centre price bands by their shares.
for (ty in setdiff(ECEC_TYPES, 'Center-Based')) {
  r <- lookup('weekly_hours', ty)
  if (is.null(r)) {
    checked <- checked + 1; failures <- failures + 1
    cat(sprintf('FAIL %-52s no csv row\n', paste0('weekly_hours[', ty, ']')))
    next
  }
  report(abs(NSECE_ANNUAL_HOURS[[ty]] - 50 * r$value) < 1e-9,
         paste0('NSECE_ANNUAL_HOURS[', ty, ']'),
         NSECE_ANNUAL_HOURS[[ty]], 50 * r$value)
}

hi_s <- lookup('price_band_share', 'High-Priced Center-Based')
lo_s <- lookup('price_band_share', 'Low-Priced Center-Based')
hi_h <- lookup('weekly_hours',     'High-Priced Center-Based')
lo_h <- lookup('weekly_hours',     'Low-Priced Center-Based')
if (is.null(hi_s) || is.null(lo_s) || is.null(hi_h) || is.null(lo_h)) {
  checked <- checked + 1; failures <- failures + 1
  cat('FAIL NSECE_ANNUAL_HOURS[Center-Based]                 price bands absent\n')
} else {
  want <- (hi_s$value * 50 * hi_h$value + lo_s$value * 50 * lo_h$value) /
          (hi_s$value + lo_s$value)
  report(abs(NSECE_ANNUAL_HOURS[['Center-Based']] - want) < 1e-9,
         'NSECE_ANNUAL_HOURS[Center-Based]',
         NSECE_ANNUAL_HOURS[['Center-Based']], want)
}

# Regional tables, value and record count.
check_tbl(NSECE_MIX_REGION,       'share',    'informal_mix_region')
check_tbl(NSECE_HOURS_REGION,     'weekly',   'weekly_hours_region')
check_tbl(NSECE_PART_TIME_REGION, 'pt_share', 'part_time_share_region')

# Nothing in universe A is left unaccounted for. A quantity the build stopped
# reading is as much a drift signal as one it reads wrongly.
used <- c('children_all_over_10plus', 'formal_share', 'unpaid_centre_share',
          'centre_home_split', 'informal_mix', 'part_time_share',
          'weekly_hours', 'price_band_share', 'informal_mix_region',
          'weekly_hours_region', 'part_time_share_region')
extra <- setdiff(unique(a$quantity), used)
if (length(extra)) {
  failures <- failures + 1
  cat('FAIL universe A carries quantities no constant reads:',
      paste(extra, collapse = ', '), '\n')
}

cat(sprintf('\n%d comparison(s), %d failure(s)\n', checked, failures))
if (failures > 0) {
  cat('CHECK FAILED\n')
  quit(status = 1)
}
cat('CHECK PASSED\n')
