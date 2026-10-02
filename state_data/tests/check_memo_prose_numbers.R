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

TEX <- file.path(STATE_DATA_ROOT, 'docs', 'SUMMARY_method.tex')
OUT <- file.path(STATE_DATA_ROOT, 'out')

# Whitespace collapsed -- a pattern must not depend on where a line wraps.
tex <- gsub('[[:space:]]+', ' ', paste(readLines(TEX, warn = FALSE), collapse = ' '))



GEO  <- MEMO_GEOGRAPHIES
ST4  <- setdiff(GEO, 'NYC')
f3   <- function(x) sprintf('%.3f', x)
f2   <- function(x) sprintf('%.2f', x)
big  <- function(x) format(round(x), big.mark = ',', scientific = FALSE)

# Inputs the updates memo quotes. Each is the file the build writes or reads,
# so a rebuild that moves a figure fails here until the memo moves with it.
EPOP_DIAG <- read_csv('resources/epop/state_epop_diagnostics.csv', show_col_types = FALSE)
OEWS_R    <- read_csv('resources/supply/state_childcare_wage_ratios_oews.csv', show_col_types = FALSE)
OEWS_EMP  <- read_csv('resources/supply/US_OEWS__childcare_employment_and_wages_by_state__2019.csv',
                      show_col_types = FALSE)
SUP_DIAG  <- read_csv('resources/supply/state_supply_diagnostics.csv', show_col_types = FALSE)
NATL_SUP  <- yaml::read_yaml('resources/supply/US_NSECE__national_supply_parameters__2019.yaml')
STATES51  <- setdiff(OEWS_R$state, 'NYC')

# Employment: adjustment k(2024)/k(2019). Memo's LAUS figures describe a
# removed method -- no input left to check them against, not registered.
adj_new <- setNames(EPOP_DIAG$adjustment, EPOP_DIAG$state)

# Wage ratios by group, states only
ratio <- function(tab, st, g) tab$wage_ratio[tab$state == st & tab$educ_group == g]
rng   <- function(g) range(OEWS_R$wage_ratio[OEWS_R$educ_group == g & OEWS_R$state %in% STATES51])

# Validity: national NSECE labour hours / 2,080 against national OEWS workers
OEWS_HOURS <- 2080
us_w   <- OEWS_EMP[OEWS_EMP$state == 'US', ]
oews_w <- tapply(us_w$tot_emp, us_w$educ_group, sum)
fte    <- c(no_ba = NATL_SUP$labor_supply$no_ba, ba = NATL_SUP$labor_supply$ba) / OEWS_HOURS

# Labour quantity: OEWS-based over demand-based, no-BA, whole extremes
l0  <- SUP_DIAG[SUP_DIAG$state %in% STATES51, ]
nm  <- c(setNames(datasets::state.name, datasets::state.abb), DC = 'District of Columbia')
lo  <- l0[which.min(l0$oews_over_demand_no_ba), ]
hi  <- l0[which.max(l0$oews_over_demand_no_ba), ]

# Crosswalk households, unweighted
xw_n <- length(readLines(gzfile('resources/acs/US_ACS__household_county_puma_crosswalk__2019.csv.gz'))) - 1


# Build every geography under both share sources, quietly.
build_all <- function(src) {
  SHARE_SOURCE <<- src
  lapply(setNames(GEOGRAPHIES, GEOGRAPHIES), function(g) {
    quiet <- utils::capture.output(r <- build_geo(load_spec(g))); r
  })
}
B_P0 <- build_all('p0')
B_NS <- build_all('nsece')
SHARE_SOURCE <- 'p0'
diag_rng <- function(b, k) range(vapply(b, function(r) attr(r, 'diagnostics')[[k]], numeric(1)))
kids     <- function(b, g, t) { r <- b[[g]]; r$children[r$ecec_type == t] }

# WV under the first p0 order: p0 formal share x non-parental total, against
# the program children -- the case that made the build divide only the rest.
# Rest of NY State: NY build minus NYC build, against a direct build from
# rest-of-state p0 (NY p0 minus NYC p0 -- weighted counts, so additive).
# Largest hours gap over care types, percent.
ros_gap <- local({
  ny <- B_P0[['NY']]; nyc <- B_P0[['NYC']]
  dn <- attr(ny, 'diagnostics'); dc <- attr(nyc, 'diagnostics')
  np <- dn$nonparental_total - dc$nonparental_total
  pr <- dn$program_net - dc$program_net
  a <- p0_children_of('NY'); b <- p0_children_of('NYC')
  pt <- setNames(a$pt - b$pt, a$ecec_type)[ECEC_TYPES]
  ft <- setNames(a$ft - b$ft, a$ecec_type)[ECEC_TYPES]
  rest <- setdiff(ECEC_TYPES, 'Unpaid Center-Based')
  kids <- c((np - pr) * (pt + ft)[rest] / sum((pt + ft)[rest]),
            'Unpaid Center-Based' = pr)[ECEC_TYPES]
  s <- pt / (pt + ft)
  direct <- kids * (s * GRID_PT_HOURS + (1 - s) * GRID_FT_HOURS) * 50
  diff <- setNames(ny$annual_hours - nyc$annual_hours, ny$ecec_type)[ECEC_TYPES]
  max(abs(diff / direct - 1)) * 100
})

wv <- local({
  spec <- load_spec('WV')
  d <- attr(B_P0[['WV']], 'diagnostics')
  p <- p0_children_of('WV'); n <- setNames(p$pt + p$ft, p$ecec_type)
  c(formal = sum(n[FORMAL_TYPES]) / sum(n[ECEC_TYPES]) * d$nonparental_total,
    program = program_children(spec)$total)
})

# Register check rows
REG <- read_csv(file.path(OUT, 'register_check.csv'), show_col_types = FALSE)
k1  <- function(x) ifelse(is.na(x), '--', sprintf('%.1f', x / 1000))
pc  <- function(x) sprintf('$%s%d\\%%$', ifelse(x >= 0, '+', '-'), as.integer(round(abs(100 * x))))
reg_row <- function(g, label) {
  r <- REG[REG$geo == g, ]
  c(label, k1(r$register_centre_children), k1(r$build_centre), pc(r$gap_centre),
    k1(r$register_home_children), k1(r$build_paid_home),
    if (is.na(r$gap_home)) '--' else pc(r$gap_home))
}


# THE REGISTER. One row per build-derived figure quoted in running text.
#   what     -- for the failure message
#   pattern  -- sprintf template; each %s takes one expected figure
#   value    -- what the build says it should be, one element per %s
REGISTER <- list(
  list(what    = 'section 5, adjustment KY NJ NY VA, all-state range',
       pattern = 'is %s in KY, %s in NJ, %s in NY, and %s in VA (%s to %s across all 50 states and DC)',
       value   = c(f3(adj_new[ST4]), f3(range(adj_new[STATES51])))),
  list(what    = 'section 4, OEWS ratio range by group',
       pattern = 'the OEWS ratio runs from %s to %s without a degree and from %s to %s with one',
       value   = c(f3(rng('no_ba')), f3(rng('ba')))),
  list(what    = 'section 4, ratio table KY',
       pattern = 'KY & %s & %s',
       value   = f3(c(ratio(OEWS_R, 'KY', 'no_ba'), ratio(OEWS_R, 'KY', 'ba')))),
  list(what    = 'section 4, ratio table NJ',
       pattern = 'NJ & %s & %s',
       value   = f3(c(ratio(OEWS_R, 'NJ', 'no_ba'), ratio(OEWS_R, 'NJ', 'ba')))),
  list(what    = 'section 4, ratio table NYS',
       pattern = 'NYS & %s & %s',
       value   = f3(c(ratio(OEWS_R, 'NY', 'no_ba'), ratio(OEWS_R, 'NY', 'ba')))),
  list(what    = 'section 4, ratio table VA',
       pattern = 'VA & %s & %s',
       value   = f3(c(ratio(OEWS_R, 'VA', 'no_ba'), ratio(OEWS_R, 'VA', 'ba')))),
  list(what    = 'section 4, validity no degree',
       pattern = 'No degree & %s & %s & %s',
       value   = c(big(fte['no_ba']), big(oews_w['no_ba']), f2(fte['no_ba'] / oews_w['no_ba']))),
  list(what    = 'section 4, validity degree',
       pattern = 'Degree & %s & %s & %s',
       value   = c(big(fte['ba']), big(oews_w['ba']), f2(fte['ba'] / oews_w['ba']))),
  list(what    = 'section 4, validity total',
       pattern = 'Total & %s & %s & %s',
       value   = c(big(sum(fte)), big(sum(oews_w)), f2(sum(fte) / sum(oews_w)))),
  list(what    = 'section 4, labour quantity ratio range',
       pattern = 'the ratio of the two runs from %s (%s) to %s (%s)',
       value   = c(f2(lo$oews_over_demand_no_ba), nm[[lo$state]],
                   f2(hi$oews_over_demand_no_ba), nm[[hi$state]])),
  list(what    = 'section 2, formal and centre share ranges, both sources',
       pattern = 'the formal share is now %s to %s, against %s to %s with the NSECE shares. The center share is %s to %s, against %s to %s',
       value   = f3(c(diag_rng(B_P0, 'formal_share'), diag_rng(B_NS, 'formal_share'),
                      diag_rng(B_P0, 'centre_share'), diag_rng(B_NS, 'centre_share')))),
  list(what    = 'section 2, formal share and program share, DC and WV',
       pattern = '%s in DC and %s in West Virginia, where program children are %s\\%% and %s\\%% of that care',
       value   = local({
         d <- function(g) attr(B_P0[[g]], 'diagnostics')
         c(f3(c(d('DC')$formal_share, d('WV')$formal_share)),
           sprintf('%.0f', 100 * c(d('DC')$program_net / d('DC')$nonparental_total,
                                   d('WV')$program_net / d('WV')$nonparental_total)))
       })),
  list(what    = 'section 2, formal share range, five memo geographies',
       pattern = 'In the five memo geographies the formal share is %s to %s',
       value   = f3(range(vapply(B_P0[GEO], function(r) attr(r, 'diagnostics')$formal_share, numeric(1))))),
  list(what    = 'section 2, NYS Paid Home-Based, both sources',
       pattern = 'Paid Home-Based has %s children, against %s with the NSECE shares',
       value   = big(c(kids(B_P0, 'NY', 'Paid Home-Based'), kids(B_NS, 'NY', 'Paid Home-Based')))),
  list(what    = 'section 2, p0 non-parental children over NSCH total, range and median',
       pattern = 'non-parental care is %s to %s times the NSCH count (median %s)',
       value   = local({
         r <- vapply(GEOGRAPHIES, function(g) {
           d <- p0_children_of(g)
           sum(d$pt + d$ft) / attr(B_P0[[g]], 'diagnostics')$nonparental_total
         }, numeric(1))
         f2(c(range(r), stats::median(r)))
       })),
  list(what    = 'section 2, NYS minus NYC against a direct rest-of-state build',
       pattern = 'The difference is at most %s\\%% of the hours in a care type',
       value   = sprintf('%.1f', ros_gap)),
  list(what    = 'section 2, WV first-order formal vs programs',
       pattern = 'smaller than the program children: %s against %s',
       value   = big(c(wv[['formal']], wv[['program']]))),
  list(what    = 'section 6, register row KY',  pattern = '%s & %s & %s & %s & %s & %s & %s', value = reg_row('KY', 'KY')),
  list(what    = 'section 6, register row NJ',  pattern = '%s & %s & %s & %s & %s & %s & %s', value = reg_row('NJ', 'NJ')),
  list(what    = 'section 6, register row NYS', pattern = '%s & %s & %s & %s & %s & %s & %s', value = reg_row('NY', 'NYS')),
  list(what    = 'section 6, register row NYC', pattern = '%s & %s & %s & %s & %s & %s & %s', value = reg_row('NYC', 'NYC')),
  list(what    = 'section 6, register row VA',  pattern = '%s & %s & %s & %s & %s & %s & %s', value = reg_row('VA', 'VA')),
  list(what    = 'section 3, crosswalk households',
       pattern = 'All %s households in the simulator',
       value   = big(xw_n))
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
