#------------------------------------------------------------------------------
# 00_build_demand_targets.R -- state-level demand inputs for the simulator:
# config/state/demand/<GEO>.csv, six rows, annual hours by care type. Method
# memo (docs/SUMMARY_method.pdf, kept local) defines each number.
#
# METHOD, per geography. Step numbers name quantities across state_data/
# (README, SOURCES.csv) -- fixed, though p0 runs them in
# order 1, 4, 2-3-5, 6.
#   1. children in regular care = population 0-4 x NSCH rate x universe ratio
#   4. Unpaid Center-Based = program counts, less Head Start a district runs in
#      its own pre-K school (CCD counts those children too)
#   2, 3, 5. rest (step 1 less step 4) split over other five types by
#      simulator p0 summed over geography's own ACS households. Formal share,
#      informal mix, center-home split all results.
#   6. annual hours = children x simulator's hours grid x 50, split part-time /
#      full-time by p0's part-time share per type
# Regional NSECE shares: comparison only, 05_compare_nsece_shares.R.
#
# states/<GEO>.R = hand-researched DATA (geo_spec()). NSCH rate and
# school-overlap share read live from out/ (01_, 03_ write them) -- nothing
# hand-copied to drift. Everything else here, shared by every geography.
#
# Usage: Rscript state_data/code/00_build_demand_targets.R
#          --geo KY | --all [--check]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble)
})

# STATE_POSTAL_TO_FIPS, SUBSTATE_GEOGRAPHIES, parent_geo() -- build, simulator
# name geographies one way. shared_setup.R: STATE_DATA_ROOT, REGIONS,
# cli_value(), show_df(), `%||%`.
source('src/shared_functions/constants.R')
source('state_data/code/shared_setup.R')



#==== CONSTANTS ===============================================================

# Care types written out. Center care one row -- simulator's low/high price
# split not a state-level quantity.
ECEC_TYPES <- c('Center-Based', 'Unpaid Center-Based', 'Paid Home-Based',
                'Other Unpaid', 'Other Paid', 'Unpaid Home-Based')

# NSECE CONSTANT. NSECE 2019 household file (ICPSR 37941, dataset 0009),
# child-weighted, over EVERY child 0-4 w/ a regular non-parental arrangement
# -- NSECE's own five-hour screener, universe A. 02_derive_nsece_constants.R
# derives it; full precision, record counts in out/nsece_constants.csv.
#
# WHY A, NOT 10-HOUR UNIVERSE. Simulator's universe = A:
# src/1_processing/1a_nsece_processing.R keeps every NSECE child w/ positive
# hours, no threshold of its own -- screener's five-hour floor still applies.
# NSCH k6q20 measures only the 10+ population, so step 1 carries its rate up
# w/ NSECE_UNIVERSE_RATIO.
#
# Primary type per child, simulator's rule (1a_nsece_processing.R): paid
# center > unpaid center > paid home > other paid > unpaid home > other
# unpaid; hours break ties.

# Step 1. Weighted children in ANY regular non-parental care over weighted
# children at 10+ hrs/week (what NSCH asks). 2,814 children.
NSECE_UNIVERSE_RATIO <- 1.0980541895845115

# SIMULATOR'S HOURS GRID. src/shared_functions/constants.R places a child at
# 10 or 40 hrs/week, nothing between, so build states target on same grid:
# part-time children x 10 + full-time children x 40. Demand file carries
# hours alone -- simulator matches hours, may reach them w/ other child counts
# or 10/40 mix.
GRID_PT_HOURS <- 10
GRID_FT_HOURS <- 40

#==== SHARE MODEL -- simulator p0 over the state's ACS households =============

# p0 summary, one row per geography x simulator care type x hours choice,
# weighted children 0-4. Built on cluster from a full-sample interface.
P0_SHARES_FILE <- 'US_ACS__p0_children_by_care_type_and_hours__2019.csv'

# Simulator care type -> build row. Both center price bands -> one row (demand
# file has one Center-Based target). Parent Only dropped.
P0_TYPE_MAP <- c('Low-Priced Center-Based'  = 'Center-Based',
                 'High-Priced Center-Based' = 'Center-Based',
                 'Unpaid Center-Based'      = 'Unpaid Center-Based',
                 'Paid Home-Based'          = 'Paid Home-Based',
                 'Other Unpaid'             = 'Other Unpaid',
                 'Other Paid'               = 'Other Paid',
                 'Unpaid Home-Based'        = 'Unpaid Home-Based')

FORMAL_TYPES   <- c('Center-Based', 'Unpaid Center-Based', 'Paid Home-Based')

# build_geo() stop phrase when programs fill the total they come out of.
PROGRAMS_TOO_LARGE <- 'meets or exceeds'



p0_rows <- function(geo, types = NULL, hint = '') {

  #----------------------------------------------------------------------------
  # Geography's rows of P0_SHARES_FILE, all character.
  #
  # Params:
  #   - geo (chr); types (chr vec|NULL): ecec_type values kept, NULL all
  #   - hint (chr): appended to missing-file message
  #
  # Returns: (tibble); stops if file missing or no rows
  #----------------------------------------------------------------------------

  f <- raw_path(P0_SHARES_FILE)
  if (!file.exists(f)) stop('no ', f, ' -- run state_data/cluster/acs_p0_shares.R', hint)
  d <- read_chr(f) %>% dplyr::filter(geo == !!geo)
  if (!is.null(types)) d <- dplyr::filter(d, ecec_type %in% !!types)
  if (!nrow(d)) stop('no p0 rows for ', geo, ' in ', f)
  d
}



p0_children_of <- function(geo) {

  #----------------------------------------------------------------------------
  # Geography's p0 children by build row, hours choice.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (tibble) ecec_type, pt, ft -- weighted children 0-4
  #----------------------------------------------------------------------------

  d <- p0_rows(geo, names(P0_TYPE_MAP),
               hint = ' on the cluster') %>%
    dplyr::mutate(row = unname(P0_TYPE_MAP[ecec_type]), children = num(children))
  odd <- setdiff(unique(d$hours_choice), c('pt', 'ft'))
  if (length(odd)) {
    stop(geo, ': non-parental care w/ hours choice ', paste(odd, collapse = ', '),
         ' -- expected pt or ft only')
  }

  d %>%
    dplyr::group_by(ecec_type = row) %>%
    dplyr::summarise(pt = sum(children[hours_choice == 'pt']),
                     ft = sum(children[hours_choice == 'ft']), .groups = 'drop') %>%
    dplyr::right_join(tibble::tibble(ecec_type = ECEC_TYPES), by = 'ecec_type') %>%
    dplyr::mutate(pt = dplyr::coalesce(pt, 0), ft = dplyr::coalesce(ft, 0))
}



geo_shares <- function(spec) {

  #----------------------------------------------------------------------------
  # Shares steps 2, 3, 5, 6 use: p0 over geography's own ACS households.
  #
  # Params:
  #   - spec (list): from load_spec()
  #
  # Returns: (list) rest_mix (named num, five types but Unpaid Center-Based --
  #   what build_geo() divides), pt (named num, ECEC_TYPES)
  #----------------------------------------------------------------------------

  d <- p0_children_of(spec$geo)
  n <- setNames(d$pt + d$ft, d$ecec_type)
  if (any(n[ECEC_TYPES] <= 0)) {
    stop(spec$geo, ': p0 holds no children in ',
         paste(ECEC_TYPES[n[ECEC_TYPES] <= 0], collapse = ', '))
  }
  # Program counts set Unpaid Center-Based; p0 divides the other five.
  rest <- setdiff(ECEC_TYPES, 'Unpaid Center-Based')
  list(rest_mix = n[rest] / sum(n[rest]),
       pt       = setNames(d$pt / (d$pt + d$ft), d$ecec_type)[ECEC_TYPES])
}



#==== SHARED INPUTS ===========================================================

# Raw-data root. Default inside state_data/ (fresh clone); git-ignored
# config_local.yaml can point `raw` elsewhere -- wrong path fails on first read.
PATHS <- local({
  f <- file.path(STATE_DATA_ROOT, 'config_local.yaml')
  d <- list(raw = file.path(STATE_DATA_ROOT, 'raw'))
  if (!file.exists(f) || !requireNamespace('yaml', quietly = TRUE)) return(d)
  cfg <- yaml::read_yaml(f)$paths
  list(raw = cfg$raw %||% d$raw)
})



# Path of a raw file: <raw root>/<scope>/<file> (raw_scope(), shared_setup.R).
# Returns: (chr)
raw_path <- function(f) file.path(PATHS$raw, raw_scope(f), f)



read_chr <- function(path) {

  #----------------------------------------------------------------------------
  # Reads CSV all-character -- type guesses vary by locale, by which rows land
  # first, so coercion stays explicit.
  #
  # readr warns once per session for any ragged row, never says WHICH way:
  #   fewer fields than header -- benign. Publisher left trailing column off,
  #     readr fills NA, every named column keeps own value.
  #   more fields than header -- NOT benign. Fields dropped off end, column
  #     may hold another column's value. Stop.
  #
  # Params:
  #   - path (chr)
  #
  # Returns: (tibble)
  #----------------------------------------------------------------------------

  if (!file.exists(path)) stop('missing raw file: ', path)
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

    # Issue not a column count = unseen shape; never let it reach comparison
    # below as NA.
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



# Returns: (num) x coerced, unparseable -> NA w/out warning
num <- function(x) suppressWarnings(as.numeric(x))



# Returns: (chr) x w/ thousands commas; caller rounds where wanted
big_n <- function(x) format(x, big.mark = ',')



population_0_4 <- function(fips, county_fips = NULL) {

  #----------------------------------------------------------------------------
  # Children 0-4, Census PEP vintage 2019, summed to geography.
  #
  # Params:
  #   - fips (int): state FIPS
  #   - county_fips (chr vec|NULL): 5-digit county FIPS (e.g. '36005') defining
  #     a sub-state geography -- same codes simulator selects ACS households
  #     on; NULL takes the state
  #
  # Returns: (num) children 0-4
  #----------------------------------------------------------------------------

  pep <- read_chr(raw_path(
    'US_PEP__population_children_age_0_4_by_county__2019.csv')) %>%
    dplyr::filter(num(STATE) == fips) %>%
    dplyr::mutate(cfips = paste0(STATE, COUNTY), pop = num(TOT_POP))

  if (!is.null(county_fips)) {
    missing <- setdiff(county_fips, pep$cfips)
    if (length(missing) > 0) {
      stop('population: county FIPS not found -- ', paste(missing, collapse = ', '))
    }
    pep <- dplyr::filter(pep, cfips %in% county_fips)
  }

  sum(pep$pop)
}



#==== THE BUILD ===============================================================

# HEAD START SLOTS -> ENROLLED CHILDREN. Specs hold FUNDED SLOTS (NIEER) as
# sources print them; step 4 converts. National only -- no public state
# series. OHS PIR 2018-19: Services Snapshots (HS, EHS, AIAN) + Biennial
# Report to Congress FY2019, Appendix C. All in raw/US/.
#
# PREGNANT WOMEN. EHS funded slots incl. 5,988 of 166,693 for pregnant women
# (EHS snapshot, 'Total Funded Pregnant Women'). Removed first.
EHS_FUNDED          <- 166693
EHS_FUNDED_PREGNANT <- 5988
EHS_PREGNANT_SHARE  <- EHS_FUNDED_PREGNANT / EHS_FUNDED        # 0.0359

# ENROLLED PER SLOT, point in time: year-end enrolment = cumulative enrolment
# less children who left, did not re-enroll (App. C A.15, A.19, A.20), over
# funded child slots. Cumulative / funded wrong ratio -- counts leavers and
# replacements, a flow not a stock. Year-end, not October 1 like CCD.
#
# App. C pools HS, EHS, AIAN; snapshots give HS, EHS funded w/out AIAN, AIAN
# as one total (22,894, 282 pregnant women). AIAN child slots split by its
# cumulative age mix: 0-2 6,433, 3-5 18,648.
AIAN_CHILD_SLOTS <- 22894 - 282
AIAN_EHS_SLOTS   <- AIAN_CHILD_SLOTS * 6433 / (6433 + 18648)
HS_ENROLLED_PER_SLOT  <- (775902 - 112939) /
                         (675734 + AIAN_CHILD_SLOTS - AIAN_EHS_SLOTS)   # 0.957
EHS_ENROLLED_PER_SLOT <- (230069 - 76419) /
                         (EHS_FUNDED - EHS_FUNDED_PREGNANT + AIAN_EHS_SLOTS)  # 0.923



program_children <- function(spec) {

  #----------------------------------------------------------------------------
  # Step 4 total -- program counts less children CCD already counts.
  #
  # Slots first: EHS loses pregnant-women share, HS, EHS scale by enrolled per
  # slot. Then deduct Head Start a school district runs in one of its own
  # pre-K schools -- that class sits in the school's CCD membership AND the
  # federal count. 03_head_start_school_overlap.R measures that share of each
  # state's slots from national files (ACF service locations x CCD pre-K by
  # school x CCD district names x EDGE geocodes) -- every state measured same
  # way, w/out depending on what a state reported to anyone.
  #
  # NOT DEDUCTED: Head Start a district runs outside its own pre-K schools, or
  # in a DIFFERENT district's school, or a grantee renting a school classroom
  # -- none enters that school's membership. Nor Head Start children also in
  # STATE pre-K: NIEER's unduplicated figure (Table 4) nets against state
  # pre-K in every setting, build counts CCD pre-K in public schools only,
  # NIEER reports it for only some states -- would correct states unevenly,
  # for a reason not about care.
  #
  # Params:
  #   - spec (list): from load_spec()
  #
  # Returns: (list) children (by program, enrolled), gross, share, netted, total
  #----------------------------------------------------------------------------

  p <- setNames(spec$programs$n_children, spec$programs$program)
  p['Head Start']       <- p[['Head Start']] * HS_ENROLLED_PER_SLOT
  p['Early Head Start'] <- p[['Early Head Start']] * (1 - EHS_PREGNANT_SHARE) *
                           EHS_ENROLLED_PER_SLOT
  gross <- sum(p)
  share <- spec$hs_in_schools_share
  if (is.null(share) || !is.finite(share) || share < 0 || share > 1) {
    stop(spec$geo, ': spec lacks a valid hs_in_schools_share -- run ',
         '03_head_start_school_overlap.R -- load_spec() reads it from ',
         'out/head_start_school_overlap.csv')
  }
  netted <- unname(p['Head Start']) * share

  list(children = p, gross = gross, share = share, netted = netted,
       total = gross - netted)
}



build_geo <- function(spec) {

  #----------------------------------------------------------------------------
  # Runs one geography's spec, printing each step.
  #
  # Params:
  #   - spec (list): from load_spec()
  #
  # Returns: (tibble) ecec_type, children, part_time, annual_hours; attribute
  #          `diagnostics` (list; carries program_children() components:
  #          program_children, hs_in_schools_share, hs_netted, program_gross,
  #          program_net)
  #----------------------------------------------------------------------------

  cat('\n== ', spec$geo, ' -- ', spec$name, ' (', spec$region, ')\n', sep = '')

  ## 1. children in regular care -- NSCH 10+ hrs/week rate carried up to
  ##    simulator's any-regular-arrangement universe
  n_children <- population_0_4(spec$fips, spec$county_fips)
  nsch_total <- n_children * spec$nonparental_rate
  np_total   <- round(nsch_total * NSECE_UNIVERSE_RATIO)
  cat('  children 0-4          ', big_n(n_children), '\n')
  cat('  x non-parental rate   ', sprintf('%.1f%%', 100 * spec$nonparental_rate),
      ' -> ', big_n(round(nsch_total)), ' at 10+ hrs/week\n', sep = '')
  cat(sprintf('  x universe ratio      %.4f -> %s in any regular care\n',
              NSECE_UNIVERSE_RATIO, big_n(np_total)))

  ## 4 first -- p0 divides only what programs leave
  pc      <- program_children(spec)
  program <- pc$total
  cat('  programs            ', big_n(pc$gross),
      ' (', paste(spec$programs$program, collapse = ', '), ')\n', sep = '')
  cat(sprintf('  Head Start in own school %s of enrolled Head Start -> less %s -> %s\n',
              sprintf('%.1f%%', 100 * pc$share),
              big_n(round(pc$netted)),
              big_n(round(program))))

  ## 2-5. NSCH sets total, programs set Unpaid Center-Based, p0's mix of the
  ## OTHER FIVE types divides rest. Formal a result, not an input -- fixing a
  ## p0 formal share first, then carving programs out, broke WV on
  ## 2026-10-01: p0 formal 0.471 x 46,196 = 21,739 children < programs 21,860.
  sh <- geo_shares(spec)
  if (program >= np_total) {
    stop(spec$geo, ': program enrolment ', big_n(round(program)),
         ' ', PROGRAMS_TOO_LARGE, ' the non-parental total ',
         big_n(round(np_total)),
         '. Check the control total and the program figures.')
  }
  children <- c((np_total - program) * sh$rest_mix,
                'Unpaid Center-Based' = program)[ECEC_TYPES]

  formal   <- sum(children[FORMAL_TYPES])
  informal <- np_total - formal
  split    <- children[['Center-Based']] /
                (children[['Center-Based']] + children[['Paid Home-Based']])
  cat(sprintf('  formal share          %.3f\n', formal / np_total))
  cat('  informal              ', big_n(round(informal)),
      sprintf(' -- %.1f%% of non-parental care\n', 100 * informal / np_total))

  cat(sprintf('  centre share          %.3f\n', split))

  ## 6. annual hours on simulator's grid, split part-time / full-time
  pt         <- sh$pt
  grid_hours <- (pt * GRID_PT_HOURS + (1 - pt) * GRID_FT_HOURS) * 50
  out <- tibble::tibble(ecec_type    = ECEC_TYPES,
                        children     = unname(children),
                        part_time    = unname(children * pt[ECEC_TYPES]),
                        annual_hours = unname(children) *
                                         unname(grid_hours[ECEC_TYPES]))

  cat(sprintf('  part-time share       %s\n',
              paste(sprintf('%s %.0f%%', substr(ECEC_TYPES, 1, 6),
                            100 * pt[ECEC_TYPES]), collapse = '  ')))
  structure(out,
            diagnostics = list(geo = spec$geo, children_0_4 = n_children,
                               nonparental_total = np_total,
                               formal_share = formal / np_total,  # realised
                               centre_share = split, residual = informal,
                               hs_in_schools_share = pc$share,
                               hs_netted = pc$netted,
                               program_children = pc$children,  # by program, gross
                               program_gross = pc$gross,
                               program_net = pc$total,
                               centre_based = children[['Center-Based']] + program,
                               licensed_home = children[['Paid Home-Based']]))
}



#==== ORCHESTRATION ===========================================================

# Every simulator geography -- states + DC (STATE_POSTAL_TO_FIPS), sub-state
# (SUBSTATE_GEOGRAPHIES), both constants.R. Memo's five first, rest in FIPS
# order. Puerto Rico in neither -- no NSCH rate, NSECE region, or simulator
# households.
MEMO_GEOGRAPHIES <- c('KY', 'NJ', 'NY', 'NYC', 'VA')
GEOGRAPHIES <- c(MEMO_GEOGRAPHIES,
                 setdiff(c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES)),
                         MEMO_GEOGRAPHIES))

# Simulator reads demand targets here (src/3_simulation/
# 3a_initialize_simulation.R), NYC incl. -- selects NYC households by
# STATEFIP + COUNTYFIP (SUBSTATE_GEOGRAPHIES).
SIMULATOR_DEMAND_DIR <- file.path('config', 'state', 'demand')



out_row <- function(file, geo, script) {

  #----------------------------------------------------------------------------
  # Geography's one row of out/<file>, which `script` writes.
  #
  # Params:
  #   - file (chr); geo (chr); script (chr): named in missing-file message
  #
  # Returns: (tibble) one row; stops if file missing or not exactly one row
  #----------------------------------------------------------------------------

  f <- file.path(STATE_DATA_ROOT, 'out', file)
  if (!file.exists(f)) stop('no ', f, ' -- run ', script, ' first')
  d <- readr::read_csv(f, show_col_types = FALSE)
  row <- d[d$geo == geo, ]
  if (nrow(row) != 1) stop('no ', file, ' row for ', geo)
  row
}



nsch_rate_of <- function(geo) {

  #----------------------------------------------------------------------------
  # Geography's NSCH rate from out/nsch_participation_rate_2019.csv
  # (01_nsch_participation_rate.R, BASE_WAVE). Sub-state geography (NYC) has
  # no NSCH records -- takes parent's row x p0 factor (p0 non-parental share
  # in geo / in parent), so rate follows geo's own households (income, work,
  # family type) under parent's NSCH level.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (list) rate, p0_factor (1 for a state)
  #----------------------------------------------------------------------------

  g   <- parent_geo(geo)
  row <- out_row('nsch_participation_rate_2019.csv', g,
                 '01_nsch_participation_rate.R')
  k <- if (g == geo) 1 else p0_nonparental_share(geo) / p0_nonparental_share(g)
  list(rate = row$rate[[1]] * k, p0_factor = k)
}



p0_nonparental_share <- function(geo) {

  #----------------------------------------------------------------------------
  # Share of weighted children 0-4 p0 places in any non-parental care (Parent
  # Only rows in denominator).
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (num) share
  #----------------------------------------------------------------------------

  d <- p0_rows(geo)
  w <- num(d$children)
  sum(w[d$ecec_type != 'Parent Only']) / sum(w)
}



geo_spec <- function(geo, name, fips, region, prek, head_start,
                     early_head_start, ...) {

  #----------------------------------------------------------------------------
  # SPEC_<GEO> for states/<GEO>.R: hand-researched inputs only. load_spec()
  # adds NSCH rate, SE, Head Start in-own-school share from out/.
  #
  # Params:
  #   - geo, name (chr); fips (num): state FIPS
  #   - region (chr): lower-case Census region -- 05_compare_nsece_shares.R keys on it
  #   - prek (num): public pre-K children (CCD; NIEER state pre-K where CCD
  #     has no count)
  #   - head_start (num): Head Start funded slots, ages 3-4 (NIEER)
  #   - early_head_start (num): Early Head Start funded slots (NIEER)
  #   - ...: fields placed between fips, region (NYC: counties, county_fips)
  #
  # Returns: (list) geo, name, fips, ..., region, programs (tibble: program,
  #          n_children)
  #----------------------------------------------------------------------------

  list(geo = geo, name = name, fips = fips, ..., region = region,
       programs = tibble::tribble(
         ~program,           ~n_children,
         'public pre-K',     prek,
         'Head Start',       head_start,
         'Early Head Start', early_head_start))
}



load_spec <- function(geo) {

  #----------------------------------------------------------------------------
  # Sources states/<GEO>.R into global env, returns SPEC_<GEO> plus
  # nonparental_rate, hs_in_schools_share read live from out/ --
  # machine-derived, so a hand copy would only drift.
  #
  # Params:
  #   - geo (chr): e.g. 'KY', 'NYC'
  #
  # Returns: (list) spec; stops if spec file or either out/ file missing
  #----------------------------------------------------------------------------

  f <- file.path(STATE_DATA_ROOT, 'states', paste0(geo, '.R'))
  if (!file.exists(f)) stop('no spec for ', geo, ' at ', f)
  source(f, local = FALSE)
  spec <- get(paste0('SPEC_', geo))

  nsch <- nsch_rate_of(geo)
  spec$nonparental_rate    <- nsch$rate
  # Unlike NSCH rate, NYC has its own row here.
  spec$hs_in_schools_share <- out_row('head_start_school_overlap.csv', geo,
                                      '03_head_start_school_overlap.R')$share[[1]]
  spec
}



write_geo <- function(res, geo) {

  #----------------------------------------------------------------------------
  # Writes SIMULATOR_DEMAND_DIR/<GEO>.csv -- file simulator reads, only copy.
  #
  # Params:
  #   - res (tibble): central build; geo (chr)
  #
  # Returns: (chr) path written
  #----------------------------------------------------------------------------

  rows <- dplyr::select(res, ecec_type, annual_hours)
  dir.create(SIMULATOR_DEMAND_DIR, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(SIMULATOR_DEMAND_DIR, paste0(geo, '.csv'))
  readr::write_csv(rows, f)
  cat('  wrote                 ', f, '\n')
  f
}



check_geo <- function(res, geo) {

  #----------------------------------------------------------------------------
  # Invariants every build must hold; cheap, run on every --check build.
  #
  # Params:
  #   - res (tibble); geo (chr)
  #
  # Returns: (num) failures
  #----------------------------------------------------------------------------

  d <- attr(res, 'diagnostics')
  say <- function(ok, msg) {
    cat('   ', if (ok) '[ok]  ' else '[FAIL]', msg, '\n'); if (!ok) 1 else 0
  }
  informal <- d$residual / d$nonparental_total

  sum(say(all(res$children > 0), 'every care type positive'),
      say(nrow(res) == length(ECEC_TYPES), paste(length(ECEC_TYPES), 'rows')),
      say(abs(sum(res$children) - d$nonparental_total) < 1,
          'children sum to the non-parental total'),
      say(informal > 0.2 && informal < 0.7, 'informal share between 20% and 70%'),
      say(all(res$annual_hours > 0), 'hours positive'))
}



main <- function(args = commandArgs(trailingOnly = TRUE)) {

  #----------------------------------------------------------------------------
  # Entry point -- builds requested geographies, writes outputs, prints
  # summary; prints usage, returns NULL w/out --all or --geo.
  #
  # Params:
  #   - args (chr): --geo KY[,NJ] | --all; --check (check_geo(), exit 1 on any
  #     failure)
  #
  # Returns: (list) central build per geography, invisibly
  #----------------------------------------------------------------------------

  geos <- if ('--all' %in% args) {
    GEOGRAPHIES
  } else if ('--geo' %in% args) {
    strsplit(cli_value('--geo', args = args), ',')[[1]]
  } else {
    cat('usage: Rscript state_data/code/00_build_demand_targets.R ',
        '[--geo KY[,NJ]] [--all] [--check]\n', sep = '')
    cat('geographies: ', paste(GEOGRAPHIES, collapse = ', '), '\n')
    return(invisible(NULL))
  }

  run_checks <- '--check' %in% args
  results  <- list()
  failures <- 0

  for (g in geos) {
    spec <- load_spec(g)
    res  <- build_geo(spec)
    write_geo(res, g)
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
  show_df(summ)

  if (run_checks) {
    cat('\n', if (failures == 0) 'ALL CHECKS PASS' else
        paste(failures, 'CHECK(S) FAILED'), '\n')
    if (failures > 0) quit(status = 1)
  }
  invisible(results)
}


if (sys.nframe() == 0) main()
