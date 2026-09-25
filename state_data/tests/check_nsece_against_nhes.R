#-----------------------------------------------------------------------------
# NSECE against an independent survey, at the geography both support.
#
# Every behavioural constant in this build comes from ONE survey. Nothing tested
# any of them against a second source, so an error in NSECE, or in the code
# reading it, would reach every geography unchallenged.
#
# NHES-ECPP 2019 is the only other survey that measures HOURS of care. Its
# CAREHOURX is continuous, so it can be cut at any threshold -- which makes it
# the only source that can reproduce NSECE_UNIVERSE_RATIO, the constant whose
# whole job is converting the 10-hour universe to the five-hour one.
#
# WHAT THIS ASSERTS. Only the universe ratio, because only the universe ratio is
# like-for-like: NSECE universe B is a TOTAL-hours cut at 10, and NHES
# CAREHOURX is total hours too. The two sources DISAGREE, by about 9%. That
# disagreement is recorded here, not silently corrected, so the check fails if
# EITHER source moves -- including if someone changes the constant without
# resolving why the two differ.
#
# WHAT THIS ONLY REPORTS. Mean hours and the part-time share. NSECE gives these
# per care type; NHES pools every arrangement into one figure. They are printed
# side by side for context and are NOT asserted, because they do not measure
# the same thing.
#
#   Rscript state_data/tests/check_nsece_against_nhes.R
#
# Exit status is 1 if the recorded gap drifts, or an input is missing.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readr); library(dplyr)
})

NSECE <- 'state_data/out/nsece_universe.csv'
NHES  <- 'state_data/raw/US_NHES__ecpp_children_0_4_by_hours_universe__2019.csv'

UNIVERSE_A <- 'A: NSECE, any regular care'

# THE RECORDED DISAGREEMENT. NHES's five-hour ratio over NSECE's, as measured
# 2026-09-24: NSECE 1.09805, NHES 1.19395. Recording it makes the gap a fact
# the suite defends rather than a number in a conversation.
GAP_EXPECTED <- 1.0873337
GAP_TOL      <- 5e-4

source('state_data/00_main.R')     # main() is guarded; this only loads

for (f in c(NSECE, NHES)) {
  if (!file.exists(f)) {
    cat('MISSING INPUT: ', f, '\n', sep = '')
    quit(status = 1)
  }
}

nsece <- read_csv(NSECE, col_types = cols(.default = col_character()))
nsece$value <- as.numeric(nsece$value)
nhes  <- read_csv(NHES, col_types = cols(
  universe = col_character(), universe_label = col_character(),
  region = col_character(), n = col_double(),
  weighted_children = col_double(), mean_weekly_hours = col_double(),
  share_under_20_hrs = col_double()))

nhes_val <- function(universe, region, col) {
  r <- nhes[nhes$universe == universe & nhes$region == region, ]
  if (nrow(r) != 1) return(NA_real_)
  r[[col]]
}

failures <- 0

cat('== NSECE against NHES-ECPP 2019, Census region and national\n')
cat('   the finest geography BOTH support -- neither carries a state\n\n')


## the one like-for-like comparison -------------------------------------------

nsece_ratio <- nsece$value[nsece$universe == UNIVERSE_A &
                             nsece$quantity == 'children_all_over_10plus']
if (length(nsece_ratio) != 1 || !is.finite(nsece_ratio)) {
  cat('FAIL  no universe ratio in ', NSECE, '\n', sep = '')
  quit(status = 1)
}

# The build's constant must be the one this compares, or the comparison is
# about a number nothing uses.
if (abs(NSECE_UNIVERSE_RATIO - nsece_ratio) > 1e-12) {
  cat('FAIL  00_main.R carries a different ratio than out/ does\n')
  failures <- failures + 1
}

ge5  <- nhes_val('ge5',  'national', 'weighted_children')
ge10 <- nhes_val('ge10', 'national', 'weighted_children')
any5 <- nhes_val('any',  'national', 'weighted_children')
if (!is.finite(ge5) || !is.finite(ge10)) {
  cat('FAIL  NHES national rows missing\n')
  quit(status = 1)
}

nhes_ratio <- ge5 / ge10
gap        <- nhes_ratio / nsece_ratio

cat('   THE UNIVERSE RATIO -- five-hour universe over ten-hour\n')
cat(sprintf('     NSECE  (universe A / universe B)   %.6f\n', nsece_ratio))
cat(sprintf('     NHES   (5+ hrs / 10+ hrs)          %.6f\n', nhes_ratio))
cat(sprintf('     NHES   (any hours / 10+ hrs)       %.6f\n', any5 / ge10))
cat(sprintf('     NHES over NSECE                    %.6f  (%+.1f%%)\n',
            gap, 100 * (gap - 1)))

if (abs(gap - GAP_EXPECTED) > GAP_TOL) {
  cat(sprintf('\nFAIL  the gap moved: expected %.7f, got %.7f\n',
              GAP_EXPECTED, gap))
  cat('      One of the two sources changed. Resolve WHY before\n')
  cat('      re-recording GAP_EXPECTED -- the point of this check is that\n')
  cat('      the disagreement cannot drift unnoticed.\n')
  failures <- failures + 1
} else {
  cat(sprintf('\n   [ok]  gap matches the recorded %.7f, within %.0e\n',
              GAP_EXPECTED, GAP_TOL))
}

cat('\n   The two do not agree, and that is the finding. NSECE says the\n')
cat('   five-hour universe is 9.8% larger than the ten-hour one; NHES says\n')
cat('   19.4%. The ratio changes the step 1 control total. Program care holds\n')
cat('   fixed, so the change goes to the rest of formal and to informal care.\n')
cat('   NHES enters on looser terms (an arrangement "at least once a week"),\n')
cat('   which inflates both sides and would push its ratio DOWN -- so the\n')
cat('   construct difference does not explain the gap.\n')


## context only ---------------------------------------------------------------

cat('\n== CONTEXT, not asserted: NHES pools every arrangement into one\n')
cat('   figure; NSECE reports per care type. Not the same quantity.\n\n')

ctx <- nhes %>%
  filter(region != 'national') %>%
  select(universe, region, n, mean_weekly_hours, share_under_20_hrs) %>%
  arrange(universe, region) %>%
  mutate(mean_weekly_hours  = round(mean_weekly_hours, 1),
         share_under_20_hrs = round(share_under_20_hrs, 3))
print(as.data.frame(ctx), row.names = FALSE)

cat('\n   NSECE part-time share by type, universe A (national), for scale:\n')
pt <- NSECE_PART_TIME[ECEC_TYPES]
for (i in seq_along(pt)) {
  cat(sprintf('     %-22s %.3f\n', names(pt)[i], pt[[i]]))
}

cat(sprintf('\n%d failure(s)\n', failures))
if (failures > 0) {
  cat('CHECK FAILED\n')
  quit(status = 1)
}
cat('CHECK PASSED\n')
