#-----------------------------------------------------------------------------
# Do the NSECE constants in 00_build_demand_targets.R (universe ratio) and
# 05_compare_nsece_shares.R (regional shares) match out/nsece_constants.csv?
#
# 02_derive_nsece_constants.R runs on cluster, so its output reaches repo by
# hand -- 00_ or 05_ could carry numbers no file on disk derives. Every
# constant is compared to universe-A row it came
# from, key by key. Nothing transcribed here: constants from sourcing
# 05_compare_nsece_shares.R, which sources 00_ (both main() guarded).
#
# Also PROGRAM_AGE5_SHARE (00_) against NHES-ECPP 2019 where the local file
# exists.
#
#   Rscript state_data/tests/check_constants_match_derivation.R
#
# Exit status is 1 on any mismatch, any missing key, any extra key.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({library(readr); library(dplyr)})

CSV <- 'state_data/out/nsece_constants.csv'

# Build reads universe A -- NSECE's own five-hour screener. B = 10-hour
# subset, kept for universe ratio, sensitivity. UNIVERSE_A: shared_setup.R
# (via 05 -> 00).

TOL <- 1e-12   # same double, not a rounded copy

source('state_data/code/05_compare_nsece_shares.R')

d <- read_csv(CSV, col_types = cols(.default = col_character())) %>%
  mutate(value = as.numeric(value), n = as.numeric(n))
a <- d[d$universe == UNIVERSE_A, ]   # base subset -- filter() would drop NA rows

failures <- 0
checked  <- 0



report <- function(ok, what, got, want) {

  #----------------------------------------------------------------------------
  # Counts one check; non-TRUE `ok` (incl. NA) counts failure, prints both
  # values at 17 digits. Updates globals `checked`, `failures`.
  #
  # Params: ok (logical), what (chr): label, got (num): build, want (num): CSV
  #----------------------------------------------------------------------------

  checked <<- checked + 1
  if (!isTRUE(ok)) {
    failures <<- failures + 1
    cat(sprintf('FAIL %-52s build %-22s csv %s\n', what,
                format(got, digits = 17), format(want, digits = 17)))
  }
}



# report() on |got - want| < tol
near <- function(what, got, want, tol = TOL) report(abs(got - want) < tol, what, got, want)



# Counts one failed check, prints `line` as is
fail <- function(line) {
  checked  <<- checked + 1
  failures <<- failures + 1
  cat(line)
}



# Universe-A row for quantity, key; NULL unless exactly one
lookup <- function(quantity, key) {
  r <- a[a$quantity == quantity & a$key == key, ]
  if (nrow(r) == 1) r
}



check_rows <- function(quantity, keys, got, n = NULL) {

  #----------------------------------------------------------------------------
  # Build values against one quantity, key by key; missing CSV row = failure.
  # With `n`, record count checked too.
  #
  # Params: quantity (chr), keys (chr): CSV keys, got (num): build value per
  #   key, n (num): build record count per key, NULL to skip
  #----------------------------------------------------------------------------

  for (i in seq_along(keys)) {
    what <- paste0(quantity, '[', keys[i], ']')
    r <- lookup(quantity, keys[i])
    if (is.null(r)) { fail(sprintf('FAIL %-52s no csv row\n', what)); next }
    near(what, got[[i]], r$value)
    if (!is.null(n)) report(n[i] == r$n, paste0(what, '.n'), n[i], r$n)
  }
}

check_vec <- function(vec, quantity) check_rows(quantity, names(vec), vec[names(vec)])
check_tbl <- function(tbl, valcol, quantity) {
  if (nrow(tbl)) check_rows(quantity, paste0(tbl$region, ':', tbl$type), tbl[[valcol]], tbl$n)
}

cat('== constants in 00_build_demand_targets.R vs', CSV, '\n')
cat('   universe:', UNIVERSE_A, '\n\n')

# Step 1. Universe ratio carries NSCH rate up to universe A.
r <- lookup('children_all_over_10plus', 'national')
if (is.null(r)) {
  fail('FAIL children_all_over_10plus[national]               no csv row\n')
} else {
  near('NSECE_UNIVERSE_RATIO', NSECE_UNIVERSE_RATIO, r$value)
}

# Step 1. Short share of part-time children -- state five-hour factor.
r <- lookup('short_share_of_part_time', 'national')
if (is.null(r)) {
  fail('FAIL short_share_of_part_time[national]               no csv row\n')
} else {
  near('NSECE_SHORT_SHARE_OF_PT', NSECE_SHORT_SHARE_OF_PT, r$value)
}

# Steps 2, 3, 4. Regional shares.
check_vec(NSECE_FORMAL_SHARE,        'formal_share')
check_vec(NSECE_UNPAID_CENTRE_SHARE, 'unpaid_centre_share')
check_vec(NSECE_CENTRE_HOME_SPLIT,   'centre_home_split')

# Step 3. National informal mix, before 00_build_demand_targets.R rescales it
# to sum to 1.
check_vec(NSECE_RESIDUAL_MIX * sum(NSECE_RESIDUAL_MIX), 'informal_mix')

# Step 6. National part-time shares, national hours fallback.
check_vec(NSECE_PART_TIME, 'part_time_share')

# NSECE_ANNUAL_HOURS = 50 x national weekly mean. 02_ writes one Center-Based
# row (paid centres pooled).
for (ty in ECEC_TYPES) {
  r <- lookup('weekly_hours', ty)
  if (is.null(r)) {
    fail(sprintf('FAIL %-52s no csv row\n', paste0('weekly_hours[', ty, ']')))
  } else {
    near(paste0('NSECE_ANNUAL_HOURS[', ty, ']'), NSECE_ANNUAL_HOURS[[ty]], 50 * r$value, 1e-9)
  }
}

# Regional tables, value and record count.
check_tbl(NSECE_MIX_REGION,       'share',    'informal_mix_region')
check_tbl(NSECE_HOURS_REGION,     'weekly',   'weekly_hours_region')
check_tbl(NSECE_PART_TIME_REGION, 'pt_share', 'part_time_share_region')

# Nothing in universe A left unaccounted for. Quantity build stopped reading
# = as much a drift signal as one it reads wrongly.
used <- c('children_all_over_10plus', 'short_share_of_part_time', 'formal_share', 'unpaid_centre_share',
          'centre_home_split', 'informal_mix', 'part_time_share',
          'weekly_hours', 'informal_mix_region',
          'weekly_hours_region', 'part_time_share_region',
          # read live from the CSV by 05_ short_share_split(), no constant
          'short_share_of_part_time_region', 'short_share_of_part_time_type',
          # read live by 06_ (regional benchmark)
          'share_of_children_region')
extra <- setdiff(unique(a$quantity), used)
if (length(extra)) {
  failures <- failures + 1
  cat('FAIL universe A carries quantities no constant reads:',
      paste(extra, collapse = ', '), '\n')
}

# AGE5_NHES_NATIONAL (00_) -- NHES-ECPP 2019, recomputed by its recipe:
# children not yet in kindergarten, center program now, ages 3+ at
# 2018-12-31, share 5+, weight FEWT, national. File in raw/US/
# -- absent, block says so, counts nothing.
NHES_FILE <- file.path('resources', 'nhes',
                       'US_NHES__ecpp_public_use_file__2019.csv')
if (file.exists(NHES_FILE)) {
  nh <- read_csv(NHES_FILE, show_col_types = FALSE)
  base <- nh$CPNNOWX == 1 & nh$AGE2018 >= 3
  prog <- list(prek = nh$CPPLACEX == 2, head_start = nh$CPHEADST == 1)
  for (p in names(prog)) {
    x <- nh[base & prog[[p]], ]
    want <- sum(x$FEWT[x$AGE2018 >= 5]) / sum(x$FEWT)
    near(sprintf('AGE5_NHES_NATIONAL[%s]', p), AGE5_NHES_NATIONAL[[p]], want, 1e-12)
    report(nrow(x) >= 30, sprintf('AGE5_NHES_NATIONAL[%s].n >= 30', p), nrow(x), 30)
  }
} else {
  cat('\n(skipped AGE5_NHES_NATIONAL -- no', NHES_FILE, ')\n')
}

# Five-year-old shares (00_ program_age5_share()): every share in [0, 1]
for (g in c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))) {
  s <- program_age5_share(load_spec(g))
  report(all(s >= 0 & s <= 1), sprintf('age5 share in [0, 1] (%s)', g), max(s), 1)
}

cat(sprintf('\n%d comparison(s), %d failure(s)\n', checked, failures))
if (failures > 0) {
  cat('CHECK FAILED\n')
  quit(status = 1)
}
cat('CHECK PASSED\n')
