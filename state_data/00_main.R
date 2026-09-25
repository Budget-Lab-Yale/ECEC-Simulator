#------------------------------------------------------------------------------
# 00_main.R -- builds state-level demand inputs for the simulator.
#
# Writes one file per geography: out/<GEO>.csv, six rows, annual hours by care
# type. docs/SUMMARY_method.pdf says what each number means.
#
# THE METHOD, per geography:
#   1. children in regular care = population 0-4 x NSCH rate x universe ratio
#   2. formal = that x regional NSECE formal share; informal = the rest
#   3. Unpaid Center-Based = program counts, less the Head Start a district
#      runs in its own pre-K school (CCD counts those children too)
#   4. rest of formal split centre / home by the regional NSECE share
#   5. informal split by the regional NSECE mix
#   6. annual hours = children x the simulator's own hours grid x 50, split
#      part-time / full-time by the regional NSECE share
#
# states/<GEO>.R is DATA: program counts, NSCH rate w/ standard error,
# Census region, and the school-overlap share. Everything else lives here and
# reaches every geography.
#
# Usage:
#   Rscript state_data/00_main.R --geo KY
#   Rscript state_data/00_main.R --all
#   Rscript state_data/00_main.R --all --check --interval
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
})

STATE_DATA_ROOT <- 'state_data'
BASE_YEAR       <- 2019    # simulator calibration year -- every input pins here

`%||%` <- function(x, y) if (is.null(x)) y else x



#=============================================================================
# CONSTANTS
#=============================================================================

# Care types written out. Centre care is one row -- the simulator's own
# low/high price split is not a state-level quantity.
ECEC_TYPES <- c('Center-Based', 'Unpaid Center-Based', 'Paid Home-Based',
                'Other Unpaid', 'Other Paid', 'Unpaid Home-Based')

# NSECE CONSTANTS. All from the NSECE 2019 household file (ICPSR 37941,
# dataset 0009), child-weighted, over EVERY child 0-4 w/ a regular
# non-parental arrangement -- NSECE's own five-hour screener, universe A.
#
# WHY A, NOT THE 10-HOUR UNIVERSE. Simulator's universe = A:
# src/1_processing/1a_nsece_processing.R keeps every NSECE child w/ positive
# hours, adds no threshold of its own -- NSECE screener's five-hour floor
# still applies. Matching it, build describes population simulator holds.
# NSCH k6q20 can only measure the 10+ population, so step 1 carries its rate
# up to this universe w/ NSECE_UNIVERSE_RATIO.
# 02_nsece_constants.R derives every one; full precision and record
# counts in out/nsece_universe.csv.
#
# Primary type per child, by the simulator's own rule
# (src/1_processing/1a_nsece_processing.R): paid centre > unpaid centre >
# paid home > other paid > unpaid home > other unpaid; hours break ties.


# NATIONAL annual hours per child -- weekly mean in the primary arrangement
# x 50. Fallback for a regional cell under NSECE_MIN_RECORDS. Centre-Based is
# the mean over both paid-centre price bands.
NSECE_ANNUAL_HOURS <- c(
  'Center-Based'        = (0.07008002232267292 * 50 * 27.454813402111718 +
                           0.06556113476684644 * 50 * 40.405040897598035) /
                          (0.07008002232267292 + 0.06556113476684644),
  'Unpaid Center-Based' = 50 * 28.733041207982050,
  'Paid Home-Based'     = 50 * 38.389082189644240,
  'Other Unpaid'        = 50 * 38.587712836110920,
  'Other Paid'          = 50 * 31.085307060488336,
  'Unpaid Home-Based'   = 50 * 42.447909360764570)

# Step 1. Weighted children in ANY regular non-parental care over weighted
# children at 10+ hrs/week. Carries the NSCH rate, which asks about 10+ hours,
# up to the universe the simulator holds. 2,814 children.
NSECE_UNIVERSE_RATIO <- 1.0980541895845115

# Step 2. Share of children in regular care whose primary type is formal --
# Center-Based, Unpaid Center-Based, Paid Home-Based -- by Census region.
# Records: NE 456, MW 539, S 1,062, W 757.
NSECE_FORMAL_SHARE <- c(northeast = 0.5652653175231646,
                        midwest   = 0.5292284900870536,
                        south     = 0.53156573832827,
                        west      = 0.48328153589694967)

# NATIONAL informal mix. Fallback for a regional cell under NSECE_MIN_RECORDS.
NSECE_RESIDUAL_MIX <- local({
  w <- c('Other Unpaid'      = 0.6041435278360107,
         'Other Paid'        = 0.3607068983264482,
         'Unpaid Home-Based' = 0.03514957383754115)
  w / sum(w)
})

# A regional cell under this many unweighted NSECE children takes the national
# value -- South Unpaid Home-Based rests on 4 children, Northeast on 8.
NSECE_MIN_RECORDS <- 30

# Step 3 CROSS-CHECK only. Unpaid Center-Based over all formal care, by
# region -- weighted children whose primary type is Unpaid Center-Based over
# weighted children whose primary type is formal. Step 3 itself uses each
# state's own program counts; this says what the regional average gives.
# Formal records: NE 235, MW 254, S 482, W 279 (Unpaid Center-Based 84, 93,
# 163, 106) -- no fallback needed.
NSECE_UNPAID_CENTRE_SHARE <- c(northeast = 0.29858157955075193,
                               midwest   = 0.30548525748105915,
                               south     = 0.3027785115943172,
                               west      = 0.27074481379107573)

# Step 4. Paid centres (both price bands) over paid centres + Paid Home-Based.
# Records: NE 154, MW 175, S 337, W 187 -- no fallback needed.
NSECE_CENTRE_HOME_SPLIT <- c(northeast = 0.8799388331780533,
                             midwest   = 0.6971700039202228,
                             south     = 0.8163487199353228,
                             west      = 0.8101736350960986)

# Step 5. Share within informal care, w/ unweighted records.
NSECE_MIX_REGION <- tibble::tribble(
  ~region,     ~type,               ~share,             ~n,
  'midwest',   'Other Paid',        0.3871987148073660, 100,
  'midwest',   'Other Unpaid',      0.5590314949309603, 159,
  'midwest',   'Unpaid Home-Based', 0.0537697902616737,  11,
  'northeast', 'Other Paid',        0.2955260001660253,  63,
  'northeast', 'Other Unpaid',      0.6244886593258383, 140,
  'northeast', 'Unpaid Home-Based', 0.0799853405081364,   8,
  'south',     'Other Paid',        0.4056400756041331, 221,
  'south',     'Other Unpaid',      0.5832300437771258, 329,
  'south',     'Unpaid Home-Based', 0.0111298806187411,   5,
  'west',      'Other Paid',        0.3128212299126265, 152,
  'west',      'Other Unpaid',      0.6636263920592722, 292,
  'west',      'Unpaid Home-Based', 0.0235523780281013,  14)

# Step 6. Mean weekly hours in the primary arrangement, paid centres pooled
# into one Center-Based row (no price-band blend).
NSECE_HOURS_REGION <- tibble::tribble(
  ~region,     ~type,                 ~weekly,             ~n,
  'midwest',   'Center-Based',         29.9188971275273020, 108,
  'midwest',   'Unpaid Center-Based',  30.4488858327566800,  94,
  'midwest',   'Paid Home-Based',      38.8896376330629536,  67,
  'midwest',   'Other Unpaid',         30.6266013460264230, 159,
  'midwest',   'Other Paid',           26.7065241000148140, 100,
  'midwest',   'Unpaid Home-Based',    28.3171331998730444,  11,
  'northeast', 'Center-Based',         31.9334570995736900, 121,
  'northeast', 'Unpaid Center-Based',  25.8468155624064830,  91,
  'northeast', 'Paid Home-Based',      36.6526630678393133,  33,
  'northeast', 'Other Unpaid',         49.3387552672910350, 140,
  'northeast', 'Other Paid',           30.5873911679934450,  63,
  'northeast', 'Unpaid Home-Based',    37.5245928047053852,   8,
  'south',     'Center-Based',         36.7361109194010000, 282,
  'south',     'Unpaid Center-Based',  31.0387651114622360, 170,
  'south',     'Paid Home-Based',      39.7654817852239191,  55,
  'south',     'Other Unpaid',         38.5135236507778400, 329,
  'south',     'Other Paid',           34.8266217806642700, 221,
  'south',     'Unpaid Home-Based',    86.4870335563659600,   5,
  'west',      'Center-Based',         33.2066425388585460, 144,
  'west',      'Unpaid Center-Based',  24.8778191128340300, 112,
  'west',      'Paid Home-Based',      36.1402993658343732,  43,
  'west',      'Other Unpaid',         36.8625726463482760, 292,
  'west',      'Other Paid',           28.2511294700214100, 152,
  'west',      'Unpaid Home-Based',    49.7970525898259382,  14)


# THE SIMULATOR'S HOURS GRID. src/shared_functions/constants.R places a child
# at 10 or 40 hrs/week and nothing between, so the build states its target on
# the same grid: a type's hours are its part-time children x 10 plus its
# full-time children x 40. Children, part-time shares only feed the
# calculation -- out/<GEO>.csv carries hours alone, simulator matches hours,
# may reach them w/ other child counts or 10/40 mix.
GRID_PT_HOURS <- 10
GRID_FT_HOURS <- 40

# NATIONAL part-time share. Fallback for a regional cell under
# NSECE_MIN_RECORDS -- Unpaid Home-Based rests on 5 to 14 children per region.
NSECE_PART_TIME <- c(
  'Center-Based'        = 0.24156066533388276,
  'Unpaid Center-Based' = 0.32374404913138010,
  'Paid Home-Based'     = 0.07704556374948256,
  'Other Unpaid'        = 0.29237982925281690,
  'Other Paid'          = 0.28619765711509220,
  'Unpaid Home-Based'   = 0.11530630069636505)

# Share of a type's children whose primary arrangement runs under 20 hrs/week
# -- the cut 1a_nsece_processing.R uses, so the build and the simulator call
# the same children part-time.
NSECE_PART_TIME_REGION <- tibble::tribble(
  ~region,     ~type,                 ~pt_share,           ~n,
  'midwest',   'Center-Based',         0.3672560769094304, 108,
  'midwest',   'Unpaid Center-Based',  0.3514149565490429,  94,
  'midwest',   'Paid Home-Based',      0.1161727070892445,  67,
  'midwest',   'Other Unpaid',         0.3778589556081480, 159,
  'midwest',   'Other Paid',           0.4180859752229404, 100,
  'midwest',   'Unpaid Home-Based',    0.2810657094393909,  11,
  'northeast', 'Center-Based',         0.2639149081693626, 121,
  'northeast', 'Unpaid Center-Based',  0.3325585112527899,  91,
  'northeast', 'Paid Home-Based',      0.1458057480669377,  33,
  'northeast', 'Other Unpaid',         0.1561441499432618, 140,
  'northeast', 'Other Paid',           0.2027316709775340,  63,
  'northeast', 'Unpaid Home-Based',    0.0000000000000000,   8,
  'south',     'Center-Based',         0.1945631077170473, 282,
  'south',     'Unpaid Center-Based',  0.2727975025485991, 170,
  'south',     'Paid Home-Based',      0.0413640094796634,  55,
  'south',     'Other Unpaid',         0.2619140572593956, 329,
  'south',     'Other Paid',           0.2232781759903599, 221,
  'south',     'Unpaid Home-Based',    0.2165571415343821,   5,
  'west',      'Center-Based',         0.1965070478204198, 144,
  'west',      'Unpaid Center-Based',  0.3944863316507277, 112,
  'west',      'Paid Home-Based',      0.0343117709923390,  43,
  'west',      'Other Unpaid',         0.3717828059788612, 292,
  'west',      'Other Paid',           0.3364892962106961, 152,
  'west',      'Unpaid Home-Based',    0.0000000000000000,  14)



regional_part_time <- function(region) {

  #----------------------------------------------------------------------------
  # Part-time share by care type for one region. A cell under
  # NSECE_MIN_RECORDS takes the national share.
  #
  # Params:
  #   - region (chr)
  #
  # Returns: (named num) part-time share by care type
  #----------------------------------------------------------------------------

  r <- NSECE_PART_TIME_REGION[NSECE_PART_TIME_REGION$region == region, ]
  s <- setNames(r$pt_share, r$type)
  thin <- setNames(r$n < NSECE_MIN_RECORDS, r$type)
  s[thin] <- NSECE_PART_TIME[names(s)[thin]]
  s[ECEC_TYPES]
}



regional_grid_hours <- function(region) {

  #----------------------------------------------------------------------------
  # Annual hours ONE child of each type carries on the simulator's grid:
  # part-time share x 10 + full-time share x 40, x 50 weeks.
  #
  # Params:
  #   - region (chr)
  #
  # Returns: (named num) annual hours per child, by care type
  #----------------------------------------------------------------------------

  pt <- regional_part_time(region)
  (pt * GRID_PT_HOURS + (1 - pt) * GRID_FT_HOURS) * 50
}



regional_mix <- function(region) {

  #----------------------------------------------------------------------------
  # Informal mix for one region. A cell under NSECE_MIN_RECORDS takes the
  # national share; the other cells keep their regional ratio, rescaled so
  # the three sum to 1.
  #
  # Params:
  #   - region (chr)
  #
  # Returns: (named num) share by informal type
  #----------------------------------------------------------------------------

  r <- NSECE_MIX_REGION[NSECE_MIX_REGION$region == region, ]
  s <- setNames(r$share, r$type)
  thin <- setNames(r$n < NSECE_MIN_RECORDS, r$type)
  if (!any(thin)) return(s / sum(s))
  fixed <- NSECE_RESIDUAL_MIX[names(s)[thin]]
  rest  <- s[!thin] / sum(s[!thin]) * (1 - sum(fixed))
  c(rest, fixed)[names(NSECE_RESIDUAL_MIX)]
}



regional_annual_hours <- function(region) {

  #----------------------------------------------------------------------------
  # Annual hours per child-equivalent for one region: 50 x regional weekly
  # mean, or the national figure where the cell is under NSECE_MIN_RECORDS.
  #
  # Params:
  #   - region (chr)
  #
  # Returns: (named num) annual hours by care type
  #----------------------------------------------------------------------------

  r <- NSECE_HOURS_REGION[NSECE_HOURS_REGION$region == region, ]
  h <- setNames(50 * r$weekly, r$type)
  thin <- r$type[r$n < NSECE_MIN_RECORDS]
  h[thin] <- NSECE_ANNUAL_HOURS[thin]
  h[ECEC_TYPES]
}



FORMAL_TYPES <- c('Center-Based', 'Unpaid Center-Based', 'Paid Home-Based')



#=============================================================================
# SHARED INPUTS
#=============================================================================

# WHERE THE DATA IS. Defaults sit inside this directory, which is what a fresh
# clone wants. config_local.yaml (git-ignored) can point `raw` elsewhere; a
# wrong path fails on the first read with the path in the message.
PATHS <- local({
  f <- file.path(STATE_DATA_ROOT, 'config_local.yaml')
  d <- list(raw = file.path(STATE_DATA_ROOT, 'raw'))
  if (!file.exists(f) || !requireNamespace('yaml', quietly = TRUE)) return(d)
  cfg <- yaml::read_yaml(f)$paths
  list(raw = cfg$raw %||% d$raw)
})


raw_path <- function(...) file.path(PATHS$raw, ...)


read_chr <- function(path) {

  #----------------------------------------------------------------------------
  # Reads a CSV all-character. Type guesses vary by locale, by which rows
  # land first -- coercion stays explicit.
  #
  # Params:
  #   - path (chr)
  #
  # Returns: (tibble)
  #----------------------------------------------------------------------------

  if (!file.exists(path)) stop('missing raw file: ', path)

  # readr warns once per session for any ragged row, which says nothing about
  # WHICH way the row is ragged. Inspect instead, and judge:
  #
  #   fewer fields than the header -- benign. The publisher left a trailing
  #     column off. readr fills NA, every named column keeps its own value.
  #   more fields than the header -- NOT benign. Fields are dropped off the
  #     end, so a column may hold another column's value. Stop.
  d <- withCallingHandlers(
    readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                    show_col_types = FALSE),
    warning = function(w) {
      if (grepl('parsing issues', conditionMessage(w))) invokeRestart('muffleWarning')
    })

  p <- readr::problems(d)
  if (nrow(p)) {
    ncol_of <- function(x) suppressWarnings(as.integer(sub(' columns$', '', x)))
    exp_n <- ncol_of(p$expected)
    act_n <- ncol_of(p$actual)

    # Anything that is not a column count is a shape this code has not seen.
    # Never let it fall through the comparison below as NA.
    odd <- which(is.na(exp_n) | is.na(act_n))
    if (length(odd)) {
      stop(basename(path), ': ', length(odd), ' parsing issue(s) are not a ',
           'column count -- first says expected "', p$expected[odd[1]],
           '", got "', p$actual[odd[1]], '" at row ', p$row[odd[1]], '.')
    }

    long <- which(act_n > exp_n)
    if (length(long)) {
      stop(basename(path), ': ', length(long), ' row(s) carry MORE fields than ',
           'the header declares, so values are dropped or shifted. First at row ',
           p$row[long[1]], '. Fix the export, do not edit the raw file.')
    }
  }
  d
}


num <- function(x) suppressWarnings(as.numeric(x))


population_0_4 <- function(fips, counties = NULL) {

  #----------------------------------------------------------------------------
  # Children 0-4, Census PEP vintage 2019, summed to the geography.
  #
  # `counties` names a sub-state geography (NYC's five boroughs); NULL takes
  # the state.
  #
  # Params:
  #   - fips (int): state FIPS
  #   - counties (chr vec|NULL): county names w/out the 'County' suffix
  #
  # Returns: (num) children 0-4
  #----------------------------------------------------------------------------

  pep <- read_chr(raw_path(
    'US_PEP__population_children_age_0_4_by_county__2019.csv')) %>%
    dplyr::filter(num(STATE) == fips) %>%
    dplyr::mutate(county = sub(' County$', '', CTYNAME), pop = num(TOT_POP))

  if (!is.null(counties)) {
    missing <- setdiff(counties, pep$county)
    if (length(missing) > 0) {
      stop('population: county not found -- ', paste(missing, collapse = ', '))
    }
    pep <- dplyr::filter(pep, county %in% counties)
  }

  sum(pep$pop)
}



#=============================================================================
# THE BUILD
#=============================================================================





program_children <- function(spec) {

  #----------------------------------------------------------------------------
  # Step 3 total -- program counts less the children CCD already counts.
  #
  # Pre-K (CCD) + Head Start + Early Head Start counts a child twice when a
  # school district runs a Head Start class in one of its own pre-K schools:
  # that class sits in the school's CCD membership AND in the federal Head
  # Start count. 04_head_start_in_schools.R measures that share of each
  # state's Head Start slots from national files -- ACF service locations x
  # CCD pre-K by school x CCD district names x EDGE geocodes -- so every
  # state is measured the same way, w/out depending on what a state reported
  # to anyone.
  #
  # NOT DEDUCTED: Head Start a district runs somewhere other than a pre-K
  # school of its own, and Head Start in a school of a DIFFERENT district --
  # neither enters that school's membership. A grantee renting a school
  # classroom is not deducted either; its children are not the school's.
  #
  # NIEER's own unduplicated figure (Table 4) is the wrong correction here:
  # it nets against STATE pre-K in every setting, while this build counts CCD
  # pre-K in public schools only, and only seven states report it.
  #
  # Params:
  #   - spec (list): from states/<GEO>.R
  #
  # Returns: (list) gross, share, netted, total
  #----------------------------------------------------------------------------

  p     <- setNames(spec$programs$n_children, spec$programs$program)
  gross <- sum(p)
  share <- spec$hs_in_schools_share
  if (is.null(share) || !is.finite(share) || share < 0 || share > 1) {
    stop(spec$geo, ': spec lacks a valid hs_in_schools_share -- run ',
         '04_head_start_in_schools.R, see states/KY.R')
  }
  netted <- unname(p['Head Start']) * share

  list(gross = gross, share = share, netted = netted, total = gross - netted)
}



nieer_overlap <- function(spec) {

  #----------------------------------------------------------------------------
  # Head Start children NIEER also finds in state pre-K, for one geography.
  #
  # nieer_undup_34 holds NIEER's 3s+4s counts without and with Head Start
  # (State of Preschool 2019, Table 4). The difference is Head Start net of
  # children already counted in state pre-K, so gross Head Start less that
  # difference is the overlap. Only some states report the pair; the rest carry
  # a placeholder that makes the difference equal gross Head Start, which
  # returns zero.
  #
  # DIAGNOSTIC ONLY. Step 3 does not deduct this -- see the note at its call
  # site. Nothing else reads these fields, and printing them is what keeps a
  # measured, documented figure from rotting unseen in nineteen spec files.
  #
  # Params:
  #   - spec (list): from states/<GEO>.R
  #
  # Returns: (num) children, or NA where the pair is absent
  #----------------------------------------------------------------------------

  u <- spec$nieer_undup_34
  if (is.null(u) || !all(c('without_hs', 'with_hs') %in% names(u)) ||
      !all(is.finite(u))) return(NA_real_)

  p  <- setNames(spec$programs$n_children, spec$programs$program)
  hs <- unname(p['Head Start'])
  if (!is.finite(hs) || hs <= 0) return(NA_real_)

  ov <- hs - (u[['with_hs']] - u[['without_hs']])
  if (!is.finite(ov) || ov < 0) return(NA_real_)

  # A difference equal to gross Head Start means NIEER published a sum rather
  # than an unduplicated count -- it did not measure the overlap for that
  # state. Texas and Virginia look like this exactly; Minnesota and Wisconsin
  # miss by one child, which is rounding in whole-child figures, not a measured
  # overlap of one. Reporting either as a measured zero would claim knowledge
  # the yearbook does not have, so both read as absent. The rule leaves the
  # seven states the yearbook actually deduplicates -- CT, IL, KY, MA, NJ, NY,
  # WV -- plus NYC, which inherits New York's.
  if (ov / hs < 0.001) return(NA_real_)
  ov
}



build_geo <- function(spec) {

  #----------------------------------------------------------------------------
  # Runs one geography's spec. Six steps, in the order the memo gives them.
  #
  # Params:
  #   - spec (list): from states/<GEO>.R
  #
  # Returns: (tibble) ecec_type, children, annual_hours + attributes carrying
  #          the diagnostics
  #----------------------------------------------------------------------------

  cat('\n== ', spec$geo, ' -- ', spec$name, ' (', spec$region, ')\n', sep = '')

  ## 1. children in regular care ---------------------------------------------
  # NSCH k6q20 counts children at 10+ hrs/week. The simulator holds every
  # regular arrangement, so the rate is carried up to that universe.
  n_children <- population_0_4(spec$fips, spec$counties)
  nsch_total <- n_children * spec$nonparental_rate
  np_total   <- round(nsch_total * NSECE_UNIVERSE_RATIO)
  cat('  children 0-4          ', format(n_children, big.mark = ','), '\n')
  cat('  x non-parental rate   ', sprintf('%.1f%%', 100 * spec$nonparental_rate),
      ' -> ', format(round(nsch_total), big.mark = ','), ' at 10+ hrs/week\n', sep = '')
  cat(sprintf('  x universe ratio      %.4f -> %s in any regular care\n',
              NSECE_UNIVERSE_RATIO, format(np_total, big.mark = ',')))

  ## 2. formal and informal totals -------------------------------------------
  formal   <- NSECE_FORMAL_SHARE[[spec$region]] * np_total
  informal <- np_total - formal

  ## 3. Unpaid Center-Based -- program counts, less the CCD double count ---
  pc        <- program_children(spec)
  program <- pc$total
  cat('  programs            ', format(pc$gross, big.mark = ','),
      ' (', paste(spec$programs$program, collapse = ', '), ')\n', sep = '')
  cat(sprintf('  Head Start in own school %s of Head Start slots -> less %s -> %s\n',
              sprintf('%.1f%%', 100 * pc$share),
              format(round(pc$netted), big.mark = ','),
              format(round(program), big.mark = ',')))

  # Cross-check: what the regional NSECE share would give instead.
  unpaid_share <- NSECE_UNPAID_CENTRE_SHARE[[spec$region]]
  cat(sprintf('  vs regional NSECE     %s (share %.3f) -- build / NSECE %.2f\n',
              format(round(formal * unpaid_share), big.mark = ','), unpaid_share,
              program / (formal * unpaid_share)))

  # THE OVERLAP THIS BUILD DOES NOT DEDUCT. Step 3 removes Head Start a district
  # runs in one of its own pre-K schools, because CCD counts those children too.
  # It does NOT remove Head Start children who are also enrolled in STATE
  # pre-K. NIEER Table 4 measures that second overlap, and states/<GEO>.R carries
  # the pair, but only some states report it and it nets against state pre-K in
  # every setting while this build counts CCD pre-K in public schools only --
  # so applying it would correct states unevenly, for a reason that is not about
  # care. Printed, not applied: the figure was expensive to collect and it is
  # the larger of the two wherever a state reports it.
  ov <- nieer_overlap(spec)
  if (is.finite(ov)) {
    cat(sprintf('  NIEER state pre-K overlap %s (%.1f%% of Head Start) -- measured, NOT deducted\n',
                format(round(ov), big.mark = ','),
                100 * ov / unname(setNames(spec$programs$n_children,
                                           spec$programs$program)['Head Start'])))
  } else {
    cat('  NIEER state pre-K overlap  not reported for this geography\n')
  }

  # The programs alone cannot exceed the formal total. If they do, nothing is
  # left for the centre-home split -- an input problem, stated as one.
  if (program >= formal) {
    stop(spec$geo, ': program enrolment ', format(round(program), big.mark = ','),
         ' meets or exceeds the formal total ',
         format(round(formal), big.mark = ','),
         '. Check the control total, the regional formal share and the ',
         'program figures.')
  }
  other_formal <- formal - program

  ## 4. centres and homes -- regional NSECE split ----------------------------
  split <- NSECE_CENTRE_HOME_SPLIT[[spec$region]]
  cat(sprintf('  centre share          %.3f (NSECE %s)\n', split, spec$region))

  ## 5. informal care -- regional NSECE mix, national fallback ---------------
  children <- c('Center-Based'        = other_formal * split,
                'Unpaid Center-Based' = program,
                'Paid Home-Based'     = other_formal * (1 - split),
                informal * regional_mix(spec$region))[ECEC_TYPES]
  cat('  informal              ', format(round(informal), big.mark = ','),
      sprintf(' -- %.1f%% of non-parental care\n', 100 * informal / np_total))

  ## 6. annual hours -- the simulator's grid, split part-time / full-time ----
  pt         <- regional_part_time(spec$region)
  grid_hours <- regional_grid_hours(spec$region)
  out <- tibble::tibble(ecec_type    = ECEC_TYPES,
                        children     = unname(children),
                        part_time    = unname(children * pt[ECEC_TYPES]),
                        annual_hours = unname(children) *
                                         unname(grid_hours[ECEC_TYPES]))

  # What NSECE's raw mean hours would have given. Printed, never targeted: the
  # simulator cannot place a child anywhere but 10 or 40 hrs/week.
  raw <- unname(children) * unname(regional_annual_hours(spec$region))
  cat(sprintf('  part-time share       %s\n',
              paste(sprintf('%s %.0f%%', substr(ECEC_TYPES, 1, 6),
                            100 * pt[ECEC_TYPES]), collapse = '  ')))
  cat(sprintf('  hours on the grid     %.0fm vs %.0fm on NSECE raw means (%+.1f%%)\n',
              sum(out$annual_hours) / 1e6, sum(raw) / 1e6,
              100 * (sum(out$annual_hours) / sum(raw) - 1)))

  structure(out,
            diagnostics = list(geo = spec$geo, children_0_4 = n_children,
                               nonparental_total = np_total,
                               centre_share = split, residual = informal,
                               unpaid_share = unpaid_share,
                               hs_in_schools_share = pc$share,
                               hs_netted = pc$netted,
                               program_gross = pc$gross,
                               program_net = pc$total,
                               centre_based = other_formal * split + program,
                               licensed_home = other_formal * (1 - split)))
}




#=============================================================================
# ORCHESTRATION
#=============================================================================

GEOGRAPHIES <- c('KY', 'NJ', 'NY', 'NYC', 'VA',
                 # full sections, Sep 2026 Flashpoint + Expansion States memos
                 'AZ', 'CA', 'CO', 'CT', 'IL', 'IN', 'MA', 'MN', 'NM', 'TX',
                 'VT', 'WA', 'WI', 'WV')

# Simulator reads state demand targets here (src/3_simulation/
# 3a_initialize_simulation.R). Whole states only -- simulator selects
# households by STATEFIP, so a county-defined geography (NYC) has no reader.
SIMULATOR_DEMAND_DIR <- file.path('config', 'state', 'demand')


load_spec <- function(geo) {
  f <- file.path(STATE_DATA_ROOT, 'states', paste0(geo, '.R'))
  if (!file.exists(f)) stop('no spec for ', geo, ' at ', f)
  source(f, local = FALSE)
  get(paste0('SPEC_', geo))
}


write_geo <- function(res, geo, spec) {

  #----------------------------------------------------------------------------
  # Writes out/<GEO>.csv. Whole state -> same file also to
  # config/state/demand/<GEO>.csv, where simulator reads it.
  #
  # Params:
  #   - res (tibble) central build, geo (chr), spec (list) its SPEC_<GEO>
  #
  # Returns: (chr) path of out/ file
  #----------------------------------------------------------------------------

  dir.create(file.path(STATE_DATA_ROOT, 'out'), showWarnings = FALSE,
             recursive = TRUE)
  rows <- dplyr::select(res, ecec_type, annual_hours)
  f <- file.path(STATE_DATA_ROOT, 'out', paste0(geo, '.csv'))
  readr::write_csv(rows, f)
  cat('  wrote                 ', f, '\n')

  if (is.null(spec$counties)) {
    dir.create(SIMULATOR_DEMAND_DIR, showWarnings = FALSE, recursive = TRUE)
    sim_f <- file.path(SIMULATOR_DEMAND_DIR, paste0(geo, '.csv'))
    readr::write_csv(rows, sim_f)
    cat('  wrote                 ', sim_f, '\n')
  } else {
    cat('  simulator copy         none --', geo, 'is county-defined\n')
  }
  f
}



write_interval <- function(spec, point, geo) {

  #----------------------------------------------------------------------------
  # Sidecar carrying the sampling error of the one soft input through to every
  # output row.
  #
  # The non-parental rate rests on ~120 families per geography and carries a
  # standard error of five to six points. Every count is a share of the total
  # that rate produces, so that error reaches every row. It is NOT proportional:
  # program counts hold fixed while the other formal types absorb the
  # difference, so the build is re-run at each bound rather than scaled.
  #
  # The interval covers sampling error in the rate ALONE. It says nothing about
  # sampling error in the regional NSECE shares, which is not quantified here.
  #
  # Params:
  #   - spec (list), point (tibble) the central build, geo (chr)
  #
  # Returns: (chr) path written
  #----------------------------------------------------------------------------

  se <- spec$nonparental_rate_se
  if (is.null(se)) {
    cat('  no interval           ', geo, 'declares no standard error\n')
    return(invisible(NULL))
  }

  lo <- interval_bound(spec, spec$nonparental_rate - 1.96 * se)
  hi <- interval_bound(spec, spec$nonparental_rate + 1.96 * se)

  out <- tibble::tibble(ecec_type    = point$ecec_type,
                        annual_hours = point$annual_hours,
                        low          = unname(lo[point$ecec_type]),
                        high         = unname(hi[point$ecec_type]))
  f <- file.path(STATE_DATA_ROOT, 'out', paste0(geo, '_interval.csv'))
  readr::write_csv(out, f, na = 'NA')
  cat(sprintf('  interval              rate %.1f%% (%.1f to %.1f), hours +/- %.0f%%\n',
              100 * spec$nonparental_rate,
              100 * (spec$nonparental_rate - 1.96 * se),
              100 * (spec$nonparental_rate + 1.96 * se),
              100 * (sum(out$high) / sum(out$annual_hours) - 1)))
  if (anyNA(lo)) {
    cat(sprintf('  interval low bound    NONE -- programs exceed formal care below rate %.1f%%\n',
                100 * min_feasible_rate(spec)))
  }
  f
}



interval_bound <- function(spec, rate) {

  #----------------------------------------------------------------------------
  # Build at one end of rate interval. NA per care type where rate leaves
  # formal care at or below program total -- data reject that rate, since
  # program counts alone need more formal care. min_feasible_rate() gives
  # the floor.
  #
  # Params:
  #   - spec (list), rate (num)
  #
  # Returns: (named num) annual hours by care type, NA if infeasible
  #----------------------------------------------------------------------------

  if (rate <= min_feasible_rate(spec)) {
    return(setNames(rep(NA_real_, length(ECEC_TYPES)), ECEC_TYPES))
  }
  s <- spec
  s$nonparental_rate <- rate
  # Step 1 rounds the control total -- a rate a hair above the floor can still
  # land on it. Only that error maps to NA; anything else propagates.
  r <- tryCatch({ quiet <- utils::capture.output(x <- build_geo(s)); x },
                error = function(e) {
                  if (grepl('meets or exceeds the formal total', conditionMessage(e))) NULL
                  else stop(e)
                })
  if (is.null(r)) return(setNames(rep(NA_real_, length(ECEC_TYPES)), ECEC_TYPES))
  setNames(r$annual_hours, r$ecec_type)
}



min_feasible_rate <- function(spec, formal_share = NSECE_FORMAL_SHARE[[spec$region]]) {

  #----------------------------------------------------------------------------
  # Lowest NSCH rate at which formal care exceeds netted program total.
  #
  # Carries NSECE_UNIVERSE_RATIO, as step 1 does. Without it this floor and
  # build_geo() disagree, and a bound reads NA in one place and a number in
  # the other.
  #
  # Params:
  #   - spec (list), formal_share (num) default regional
  #
  # Returns: (num) rate
  #----------------------------------------------------------------------------

  program_children(spec)$total /
    (population_0_4(spec$fips, spec$counties) * NSECE_UNIVERSE_RATIO * formal_share)
}


check_geo <- function(res, geo) {

  #----------------------------------------------------------------------------
  # Invariants every build must hold. Cheap, so they run on every build.
  #
  # Params:
  #   - res (tibble), geo (chr)
  #
  # Returns: (int) failures
  #----------------------------------------------------------------------------

  d <- attr(res, 'diagnostics')
  fail <- 0
  say  <- function(ok, msg) {
    cat('   ', if (ok) '[ok]  ' else '[FAIL]', msg, '\n'); if (!ok) 1 else 0
  }

  fail <- fail + say(all(res$children > 0), 'every care type positive')
  fail <- fail + say(nrow(res) == length(ECEC_TYPES),
                     paste(length(ECEC_TYPES), 'rows'))
  fail <- fail + say(abs(sum(res$children) - d$nonparental_total) < 1,
                     'children sum to the non-parental total')
  fail <- fail + say(d$residual / d$nonparental_total > 0.2 &&
                       d$residual / d$nonparental_total < 0.7,
                     'informal share between 20% and 70%')
  fail <- fail + say(all(res$annual_hours > 0), 'hours positive')
  fail
}


main <- function(args = commandArgs(trailingOnly = TRUE)) {

  geos <- if ('--all' %in% args) {
    GEOGRAPHIES
  } else if ('--geo' %in% args) {
    strsplit(args[which(args == '--geo') + 1], ',')[[1]]
  } else {
    cat('usage: Rscript state_data/00_main.R [--geo KY[,NJ]] [--all] [--check]\n')
    cat('geographies: ', paste(GEOGRAPHIES, collapse = ', '), '\n')
    return(invisible(NULL))
  }

  run_checks    <- '--check' %in% args
  want_interval <- '--interval' %in% args
  results <- list()
  failures <- 0

  for (g in geos) {
    spec <- load_spec(g)
    res  <- build_geo(spec)
    write_geo(res, g, spec)
    if (want_interval) write_interval(spec, res, g)
    if (run_checks) failures <- failures + check_geo(res, g)
    results[[g]] <- res
  }

  cat('\n== summary\n')
  summ <- dplyr::bind_rows(lapply(names(results), function(g) {
    d <- attr(results[[g]], 'diagnostics')
    tibble::tibble(geo = g, children_0_4 = d$children_0_4,
                   non_parental = d$nonparental_total,
                   centre_based = round(d$centre_based),
                   licensed_home = round(d$licensed_home),
                   informal = round(d$residual),
                   centre_share = round(d$centre_share, 3))
  }))
  print(as.data.frame(summ), row.names = FALSE)

  if (run_checks) {
    cat('\n', if (failures == 0) 'ALL CHECKS PASS' else
        paste(failures, 'CHECK(S) FAILED'), '\n')
    if (failures > 0) quit(status = 1)
  }
  invisible(results)
}


if (sys.nframe() == 0) main()
