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

source('state_data/code/00_main.R')     # main() is guarded; this only loads
source('state_data/code/03_head_start_in_schools.R')   # guarded; MATCH_METRES

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



# THE REGISTER. One row per build-derived figure quoted in running text.
#   what     -- for the failure message
#   pattern  -- sprintf template; each %s takes one expected figure
#   value    -- what the build says it should be, one element per %s
REGISTER <- list(
  list(what    = 'step 3, informal share of children',
       pattern = 'accounts for %s--%s\\%% of the estimated children',
       value   = informal_share_range('children')),
  # The grid flattens the hours spread, so the five geographies now round to
  # one figure. A range would read as two numbers that are the same.
  list(what    = 'step 3, informal share of hours',
       pattern = 'and %s\\%% of the modeled annual hours',
       value   = unique(informal_share_range('hours'))),

  # Step 4: slots to children, and the school overlap.
  list(what    = 'step 4a, children per funded slot HS and EHS',
       pattern = 'funded slot: %s for Head Start and %s for Early Head Start',
       value   = sprintf('%.3f', c(HS_ENROLLED_PER_SLOT,
                                   (1 - EHS_PREGNANT_SHARE) * EHS_ENROLLED_PER_SLOT))),
  list(what    = 'step 4b, flagged overlap share',
       pattern = 'flagged share is %s\\%% in KY, %s\\%% in NJ, %s\\%% in NYS, %s\\%% in NYC, and %s\\%% in VA',
       value   = sprintf('%.1f', 100 * vapply(GEO, hs_in_schools_share_of, numeric(1)))),
  list(what    = 'step 4b, match radius',
       pattern = 'geocodes are within %s meters',
       value   = format(MATCH_METRES)),
  # NSECE constants quoted in prose. Centre share once printed the ten-hour
  # universe's values (0.876, 0.805) while the build used five-hour ones.
  list(what    = 'step 1, universe ratio',
       pattern = 'Universe ratio: %s.',
       value   = sprintf('%.3f', NSECE_UNIVERSE_RATIO)),
  # Step 8: the state adjustment to national employment growth, R(2024)/R(2019),
  # from the LAUS input state_epop_projections.R reads (unrounded).
  list(what    = 'step 8, employment growth adjustment KY NJ NY VA',
       pattern = 'The state adjustment, the last term, is %s in KY, %s in NJ, %s in NY, and %s in VA',
       value   = local({
         d <- read_csv('resources/epop/US_BLS__laus_state_epop__2006-2024.csv', show_col_types = FALSE)
         e <- function(g, y) d$epop[d$geo == g & d$year == y]
         sprintf('%.3f', vapply(c('KY', 'NJ', 'NY', 'VA'), function(g)
           (e(g, 2024) / e('US', 2024)) / (e(g, 2019) / e('US', 2019)), numeric(1)))
       })),
  list(what    = 'step 8, adjustment range, all 51',
       pattern = '(%s to %s across all 50 states and DC)',
       value   = local({
         d <- read_csv('resources/epop/US_BLS__laus_state_epop__2006-2024.csv', show_col_types = FALSE)
         e <- function(y) { x <- d[d$year == y, ]; setNames(x$epop / x$epop[x$geo == 'US'], x$geo) }
         a <- (e(2024) / e(2019))[setdiff(unique(d$geo), 'US')]
         sprintf('%.3f', range(a))
       })),
  list(what    = 'step 2, formal share, four regions',
       pattern = 'formal share is %s\\%% in the Northeast, %s\\%% in the Midwest, %s\\%% in the South, and %s\\%% in the West',
       value   = sprintf('%.1f', 100 * NSECE_FORMAL_SHARE[c('northeast', 'midwest', 'south', 'west')])),
  list(what    = 'step 5, centre share, four regions',
       pattern = 'who are Center-Based. It is %s in the Northeast, %s in the Midwest, %s in the South, and %s in the West',
       value   = sprintf('%.3f', NSECE_CENTRE_HOME_SPLIT[c('northeast', 'midwest', 'south', 'west')]))
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
