#-----------------------------------------------------------------------------
# Do the build-derived numbers in the memo's PROSE still match the build?
#
# check_memo_matches_build.R reads the results table, never a sentence, so a
# figure quoted in running text could go stale while every check stays green.
# A general checker for prose numbers is not possible -- most are not build
# outputs -- so this holds an explicit REGISTER of the ones that are. Adding a
# build-derived figure to the prose means adding a row here.
#
#   Rscript state_data/tests/check_memo_prose_numbers.R
#
# Exit status is 1 on any mismatch.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({library(readr); library(dplyr)})

source('state_data/00_main.R')     # main() is guarded; this only loads
source('state_data/03_head_start_in_schools.R')   # guarded; MATCH_METRES

TEX <- file.path(STATE_DATA_ROOT, 'docs', 'SUMMARY_method.tex')
OUT <- file.path(STATE_DATA_ROOT, 'out')

# Whitespace collapsed -- a pattern must not depend on where a line wraps.
tex <- gsub('[[:space:]]+', ' ', paste(readLines(TEX, warn = FALSE), collapse = ' '))



informal_share_range <- function(of) {

  #----------------------------------------------------------------------------
  # Informal care's share, across the five geographies, as whole percents.
  #
  # TWO DIFFERENT NUMBERS. Informal hours run higher than informal children, because the three
  # informal types carry longer mean weeks than the formal ones.
  #
  # Params:
  #   - of (chr): 'hours' or 'children'
  #
  # Returns: (chr vec) low and high, rounded as the memo prints them
  #----------------------------------------------------------------------------

  if (of == 'hours') {
    s <- vapply(GEO, function(g) {
      d <- read_csv(file.path(OUT, paste0(g, '.csv')), show_col_types = FALSE)
      sum(d$annual_hours[d$ecec_type %in% INFORMAL]) / sum(d$annual_hours)
    }, numeric(1))
  } else {
    # Children are fixed by step 2 alone: informal = 1 - the regional formal
    # share. Only the two regions these geographies sit in can appear.
    s <- 1 - NSECE_FORMAL_SHARE[c('northeast', 'south')]
  }
  format(round(100 * range(s)))
}


GEO      <- c('KY', 'NJ', 'NY', 'NYC', 'VA')
INFORMAL <- c('Other Unpaid', 'Other Paid', 'Unpaid Home-Based')



# Build of g w/ a changed spec or universe ratio -- sensitivities quoted in
# prose. Ratio restored on exit, so later rows see the real build.
build_with <- function(g, ratio = NSECE_UNIVERSE_RATIO, hs_share = NULL) {
  s <- load_spec(g)
  if (!is.null(hs_share)) s$hs_in_schools_share <- hs_share
  keep <- NSECE_UNIVERSE_RATIO
  assign('NSECE_UNIVERSE_RATIO', ratio, envir = globalenv())
  on.exit(assign('NSECE_UNIVERSE_RATIO', keep, envir = globalenv()))
  q <- utils::capture.output(r <- build_geo(s))
  setNames(r$annual_hours, r$ecec_type)
}



# NHES five-hour over ten-hour ratio, national -- as check_nsece_against_nhes.R
# computes it.
nhes_ratio <- function() {
  n <- read_csv(file.path(STATE_DATA_ROOT, 'raw',
                          'US_NHES__ecpp_children_0_4_by_hours_universe__2019.csv'),
                show_col_types = FALSE)
  w <- function(u) n$weighted_children[n$universe == u & n$region == 'national']
  w('ge5') / w('ge10')
}



# Total-hours change w/ NHES ratio in place of NSECE's, low--high, 1 decimal.
nhes_total_range <- function() {
  r <- nhes_ratio()
  p <- vapply(GEO, function(g) sum(build_with(g, ratio = r)) / sum(build_with(g)) - 1,
              numeric(1))
  sprintf('%.1f', 100 * range(p))
}



# No-deduction change, one care type, for the four geographies w/ a deduction.
nodeduct_pct <- function(type) {
  vapply(c('KY', 'NJ', 'NY', 'VA'), function(g) {
    100 * (build_with(g, hs_share = 0)[[type]] / build_with(g)[[type]] - 1)
  }, numeric(1))
}


# THE REGISTER. One row per build-derived figure quoted in running text.
#   what     -- for the failure message
#   pattern  -- sprintf template; each %s takes one expected figure
#   value    -- what the build says it should be, one element per %s
REGISTER <- list(
  list(what    = 'step 5, informal share of children',
       pattern = 'accounts for %s--%s\\%% of the estimated children',
       value   = informal_share_range('children')),
  # The grid flattens the hours spread, so the five geographies now round to
  # one figure. A range would read as two numbers that are the same.
  list(what    = 'step 5, informal share of hours',
       pattern = 'and %s\\%% of modeled annual hours',
       value   = unique(informal_share_range('hours'))),

  # Step 3: the regional NSECE cross-check and the school deduction.
  list(what    = 'step 3, NSECE unpaid centre share NE and S',
       pattern = 'Center-Based: %s in the Northeast and %s in the South',
       value   = sprintf('%.3f', NSECE_UNPAID_CENTRE_SHARE[c('northeast', 'south')])),
  list(what    = 'step 1, total hours w/ NHES ratio',
       pattern = 'total annual hours rise %s--%s\\%% in the five geographies',
       value   = nhes_total_range()),
  list(what    = 'step 3, Unpaid Center-Based w/out deduction',
       pattern = 'Unpaid Center-Based hours rise %s\\%% in KY, %s\\%% in NJ, %s\\%% in NYS, and %s\\%% in VA',
       value   = sprintf('%.1f', nodeduct_pct('Unpaid Center-Based'))),
  # NYS's fall rounds to 0.0 -- memo says 'less than 0.1%' for it.
  list(what    = 'step 3, Center-Based w/out deduction',
       pattern = 'Home-Based hours fall by %s\\%%, %s\\%%, less than 0.1\\%%, and %s\\%%',
       value   = sprintf('%.1f', -nodeduct_pct('Center-Based')[c('KY', 'NJ', 'VA')])),
  list(what    = 'step 3, match radius',
       pattern = 'geocodes are within %s meters',
       value   = format(MATCH_METRES)),
  # NSECE constants quoted in prose. Centre share once printed the ten-hour
  # universe's values (0.876, 0.805) while the build used five-hour ones.
  list(what    = 'step 1, universe ratio',
       pattern = 'Universe ratio: %s.',
       value   = sprintf('%.3f', NSECE_UNIVERSE_RATIO)),
  list(what    = 'step 2, formal share NE and S',
       pattern = 'formal share is %s\\%% in the Northeast (NJ, NYS, NYC) and %s\\%% in the South',
       value   = sprintf('%.1f', 100 * NSECE_FORMAL_SHARE[c('northeast', 'south')])),
  list(what    = 'step 4, centre share NE and S',
       pattern = 'center share is %s in the Northeast and %s in the South',
       value   = sprintf('%.3f', NSECE_CENTRE_HOME_SPLIT[c('northeast', 'south')]))
)


cat('checking ', length(REGISTER), ' build-derived figure(s) quoted in prose\n\n',
    sep = '')

fails <- 0
for (r in REGISTER) {
  want <- do.call(sprintf, c(list(r$pattern), as.list(r$value)))
  if (grepl(want, tex, fixed = TRUE)) {
    cat(sprintf('  [ok]   %-42s %s\n', r$what,
                paste(r$value, collapse = '--')))
  } else {
    # Show what the memo says instead, so the fix is one edit rather than a hunt.
    loose <- gsub('%s', '([0-9.,]+)', r$pattern, fixed = TRUE)
    m <- regmatches(tex, regexpr(loose, tex))
    cat(sprintf('  [FAIL] %-42s build says %s\n', r$what,
                paste(r$value, collapse = '--')))
    cat('           memo says: ', if (length(m)) m else '(phrase not found)',
        '\n', sep = '')
    fails <- fails + 1
  }
}

if (fails == 0) {
  cat('\nevery registered prose figure matches the build\n')
} else {
  cat('\n', fails, ' prose figure(s) are stale. The tables can be right while\n',
      'a sentence is wrong -- that is what this check exists for.\n', sep = '')
  quit(status = 1)
}
