#------------------------------------------------------------------------------
# 00_build_demand_targets.R -- state-level demand inputs for the simulator:
# config/state/demand/<GEO>.csv, six rows, annual hours by care type. Method
# memo (docs/SUMMARY_method.pdf, kept local) defines each number.
#
# METHOD, per geography. Step numbers name quantities across state_data/
# (README, SOURCES.csv) -- fixed, though p0 runs them in
# order 1, 4, 2-3-5, 6.
#   1. children in regular care = population 0-4 x NSCH rate x universe ratio
#   4. Unpaid Center-Based = program counts in centers, ages 0-4, less Head
#      Start children pre-K count already holds (program_children()); pre-K
#      = larger of CCD, all-settings count (NIEER; NYC DOE)
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

# SIMULATOR'S HOURS GRID -- PT_CARE_HOURS, FT_CARE_HOURS, WEEKS_PER_YEAR from
# src/shared_functions/constants.R, no copy here. Simulator places a child at
# PT_CARE_HOURS (10) or FT_CARE_HOURS (40) hrs/week, nothing between, so
# build states target on same grid: part-time children x 10 + full-time
# children x 40, x 50 weeks. Demand file carries hours alone -- simulator
# matches hours, may reach them w/ other child counts or 10/40 mix.



grid_annual_hours <- function(pt) {

  #----------------------------------------------------------------------------
  # Annual hours per child on simulator's grid, given part-time share --
  # build_geo() step 6, 05_compare_nsece_shares.R's NSECE allocation.
  #
  # Params:
  #   - pt (num): part-time share(s), named by care type or not
  #
  # Returns: (num) annual hours per child, names kept
  #----------------------------------------------------------------------------

  (pt * PT_CARE_HOURS + (1 - pt) * FT_CARE_HOURS) * WEEKS_PER_YEAR
}




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

# HOME-BASED, FAMILY CHILD CARE OPTIONS -- not center care. Home-based option
# = weekly home visits, child stays in parent's care; family child care option
# = child in a provider's home. Neither belongs in Unpaid Center-Based --
# removed, so p0 mix places FCC children w/ other five types, home-visited
# children stay inside NSCH total wherever their care sits. Center-based,
# locally designed options kept. Funded slots by option, 2018-19 snapshots:
# HS 11,043 home + 1,277 FCC of 675,734; EHS 57,221 home + 5,722 FCC of
# 166,693 less 5,988 pregnant women; AIAN 1,493 home + 16 FCC, split by AIAN
# age mix as above. National only -- PIR state option counts on request.
# Childcare-partner CENTER slots (App. C A.10, 48,006, all programs) kept --
# Head Start services carry no fee, so Unpaid Center-Based.
AIAN_EHS_FRAC      <- AIAN_EHS_SLOTS / AIAN_CHILD_SLOTS
HS_HOME_FCC_SHARE  <- (11043 + 1277 + (1493 + 16) * (1 - AIAN_EHS_FRAC)) /
                      (675734 + AIAN_CHILD_SLOTS - AIAN_EHS_SLOTS)          # 0.0194
EHS_HOME_FCC_SHARE <- (57221 + 5722 + (1493 + 16) * AIAN_EHS_FRAC) /
                      (EHS_FUNDED - EHS_FUNDED_PREGNANT + AIAN_EHS_SLOTS)   # 0.380

# FIVE-YEAR-OLDS. Population = children 0-4; pre-K grade, Head Start 3-4 slots
# hold children who turn 5 during program year (NIEER folds 5s into 4s; CCD
# counts a grade, not an age). Share of program children 5+ at mid program
# year: NHES-ECPP 2019 (raw/local/, git-ignored), age at 2018-12-31
# (AGE2018), children not yet in kindergarten, center program now
# (CPNNOWX = 1), ages 3+, weight FEWT, by Census region (CENREG):
#   pre-K       program in a public elementary / secondary school (CPPLACEX = 2)
#   Head Start  program is Head Start / EHS (CPHEADST = 1)
# Records (5+ of all): pre-K NE 13/78, S 56/155, MW 34/123, W 26/127; Head
# Start NE 3/36, S 18/85, MW 5/52, W 7/74 -- every cell >= 30, so regional
# (NSECE rule). JK1 SE, 80 replicates: pre-K 4.2-4.9 pts, HS 3.4-5.2 pts.
# National: pre-K 0.2440, HS 0.1074. Region tracks kindergarten cutoffs
# (South: Aug/Sep 1). EHS holds no 5s (EHS snapshot: 5-year-old row blank,
# 109 four-year-olds of 224,436 children). PIR's own 1.1% Head Start
# 5-year-olds dates age at school-entry cutoff, start of year -- not
# population's date. check_constants_match_derivation.R recomputes from NHES.
PROGRAM_AGE5_SHARE <- list(
  prek       = c(northeast = 0.14905344091596437, south = 0.36088869070119439,
                 midwest   = 0.24161835448151603, west  = 0.16785291825795023),
  head_start = c(northeast = 0.059379191819765968, south = 0.18779471483253429,
                 midwest   = 0.0703325789396268,   west  = 0.067367851811474644))

# SITE FILE COVER. 03_'s own-school share = ratio inside 2026 site file, open
# sites only. Open HS slots / NIEER 2018-19 slots runs 0.41-1.22 in 50
# states; DC 0.05 (DC Public Schools grant 'Not Reported'). Below this cover
# share unmeasurable -- spec's hs_in_prek (NIEER Table 4) stands in w/ a CCD
# pre-K figure too. tests/check_program_counts_match_sources.R enforces.
SITE_FILE_MIN_COVER <- 0.25

# ALL-SETTINGS PRE-K (2026-10-06). CCD counts pre-K in public schools only,
# missing community-based public pre-K. All-settings count: NIEER Table 2
# state pre-K, ages 3-4, every setting -- NOT Table 4's pre-K + special
# education (2026-10-06: many special-education children get services at
# home or in regular child care, not a center seat). Spec's
# prek_all_settings overrides: NYC own DOE count (NIEER publishes states
# only), FL school-year VPK (Table 2 adds summer VPK). program_children()
# takes larger of CCD, all-settings count, then Head Start overlap matching
# count it took. CA, MI pre-K figure already NIEER's. Table 4 still sets
# NIEER overlap share (its Head Start increment).
NIEER_T2_FILE   <- 'US_NIEER__state_prek_enrollment_age_3_4__2018-19.csv'
NIEER_T4_FILE   <- 'US_NIEER__prek_and_head_start_enrollment_age_3_4__2018-19.csv'
PREK_FROM_NIEER <- c('CA', 'MI')



nieer_prek_inputs <- function(spec) {

  #----------------------------------------------------------------------------
  # All-settings pre-K count and NIEER Head Start-in-pre-K share (header,
  # ALL-SETTINGS PRE-K). Count: spec's prek_all_settings, else Table 2 state
  # pre-K. NIEER share = (HS slots - Table 4 HS increment) / HS slots,
  # clamped to [0, 1], only where Table 4 marks an unduplicated count
  # (dagger); NA elsewhere. Increment nets HS children in pre-K OR special
  # education, so share slightly overstates overlap w/ Table 2's count.
  # Sub-state geography (NYC) takes parent's share -- approximation, no city
  # overlap count.
  #
  # Params:
  #   - spec (list): geo_spec() result
  #
  # Returns: (list) all_settings (num, children ages 3-4), nieer_hs_share (num
  #   or NA)
  #----------------------------------------------------------------------------

  g  <- parent_geo(spec$geo)
  t4 <- read_chr(raw_path(NIEER_T4_FILE))
  r  <- t4[t4$state == g, ]
  if (nrow(r) != 1) stop(spec$geo, ': no NIEER Table 4 row for ', g, ' in ', NIEER_T4_FILE)
  prek <- num(r$prek_sped_age3) + num(r$prek_sped_age4)
  increment <- num(r$prek_sped_hs_age3) + num(r$prek_sped_hs_age4) - prek

  hs_parent <- if (g == spec$geo) spec else get_spec(g)
  hs <- hs_parent$programs$n_children[hs_parent$programs$program == 'Head Start']
  share <- if (num(r$hs_unduplicated) == 1) min(max((hs - increment) / hs, 0), 1) else NA_real_

  t2 <- read_chr(raw_path(NIEER_T2_FILE))
  r2 <- t2[t2$state == g, ]
  if (nrow(r2) != 1) stop(spec$geo, ': no NIEER Table 2 row for ', g, ' in ', NIEER_T2_FILE)
  state_prek <- num(r2$prek_age3) + num(r2$prek_age4)

  list(all_settings = spec$prek_all_settings %||% if (g == spec$geo) state_prek else
         stop(spec$geo, ': sub-state geography needs prek_all_settings in its spec'),
       nieer_hs_share = share)
}



get_spec <- function(geo) {

  #----------------------------------------------------------------------------
  # SPEC_<GEO> as states/<GEO>.R writes it -- no live out/ reads.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (list) geo_spec() result
  #----------------------------------------------------------------------------

  f <- file.path(STATE_DATA_ROOT, 'states', paste0(geo, '.R'))
  if (!file.exists(f)) stop('no spec for ', geo, ' at ', f)
  source(f, local = FALSE)
  get(paste0('SPEC_', geo))
}



program_children <- function(spec) {

  #----------------------------------------------------------------------------
  # Step 4 total -- program children in centers, ages 0-4, each counted once.
  #
  #   a. slots -> enrolled: EHS loses pregnant-women share, HS, EHS scale by
  #      enrolled per slot
  #   b. center care only: HS, EHS lose home-based, FCC option shares
  #   c. ages 0-4: pre-K, HS lose regional 5+ share (PROGRAM_AGE5_SHARE)
  #   0. pre-K count = larger of spec's (CCD) figure, prek_all_settings
  #      (header, ALL-SETTINGS PRE-K); CA, MI keep their NIEER figure
  #   d. each child once: deduct HS children pre-K count already holds. Share
  #      of HS slots = ONE measure, matching pre-K count taken:
  #        - all-settings count, NIEER reports overlap (nieer_hs_share): NIEER
  #          share -- same source, same universe; NYC takes NY's
  #        - in own school (hs_in_schools_share), CCD count, or all-settings
  #          count where NIEER reports none: HS a school district runs in
  #          one of its own pre-K schools -- class sits in school's CCD
  #          membership AND federal count. 03_head_start_school_overlap.R
  #          measures it from national files (ACF service locations x CCD
  #          pre-K by school x CCD district names x EDGE geocodes), every
  #          state same way.
  #        - in state pre-K (hs_in_prek, spec), replaces own-school share:
  #          NIEER-reported HS children also in state pre-K -- only where
  #          spec's pre-K figure IS NIEER's state pre-K, every setting (MI;
  #          same source, same universe), or where site file covers under
  #          SITE_FILE_MIN_COVER of NIEER's slots, so own-school share
  #          unmeasurable (DC).
  #
  # NOT DEDUCTED w/ a CCD pre-K figure: HS a district runs outside its own
  # pre-K schools, or in a DIFFERENT district's school, or a grantee renting a
  # school classroom -- none enters that school's membership. Nor NIEER's
  # HS-in-state-pre-K count (Table 4): it covers state pre-K in every
  # setting, CCD public schools only. W/ all-settings count, no NIEER
  # overlap: own-school share is a lower bound on overlap.
  #
  # Params:
  #   - spec (list): from load_spec()
  #
  # Returns: (list) by program (named num: public pre-K, Head Start, Early
  #   Head Start) at each step --
  #     slots       spec's counts as sources print them
  #     enrolled    step a
  #     not_centre  removed, step b; centre = enrolled less not_centre
  #     age5        removed, step c; children = centre less age5 (centers,
  #                 ages 0-4, before step d)
  #     netted      removed, step d (Head Start only nonzero); net =
  #                 children less netted
  #   gross (sum of children), total (sum of net -- Unpaid Center-Based);
  #   prek_source ('ccd' | 'all_settings' | 'nieer'); share (step d),
  #   share_school, share_prek, share_nieer; factors (list): conversion
  #   factors applied -- ehs_pregnant_share, enrolled_per_slot, home_fcc_share,
  #   age5_share (by program, spec's region)
  #----------------------------------------------------------------------------

  slots <- setNames(spec$programs$n_children, spec$programs$program)
  hs_slots <- slots[['Head Start']]

  # 0. pre-K count
  all_settings <- spec$prek_all_settings %||% 0
  prek_source <- if (parent_geo(spec$geo) %in% PREK_FROM_NIEER) {
    'nieer'
  } else if (all_settings > slots[['public pre-K']]) {
    'all_settings'
  } else {
    'ccd'
  }
  if (prek_source == 'all_settings') slots['public pre-K'] <- all_settings

  # a. enrolled children
  e <- slots
  e['Head Start']       <- hs_slots * HS_ENROLLED_PER_SLOT
  e['Early Head Start'] <- slots[['Early Head Start']] *
                           (1 - EHS_PREGNANT_SHARE) * EHS_ENROLLED_PER_SLOT

  # b. center-based options only
  not_centre <- c('public pre-K' = 0,
                  'Head Start' = e[['Head Start']] * HS_HOME_FCC_SHARE,
                  'Early Head Start' = e[['Early Head Start']] * EHS_HOME_FCC_SHARE)
  p <- e - not_centre[names(e)]
  centre <- p

  # c. ages 0-4
  a5 <- c('public pre-K' = PROGRAM_AGE5_SHARE$prek[[spec$region]],
          'Head Start'   = PROGRAM_AGE5_SHARE$head_start[[spec$region]],
          'Early Head Start' = 0)
  age5 <- p * a5[names(p)]
  p <- p - age5
  gross <- sum(p)

  # d. HS children pre-K count already holds
  share_school <- spec$hs_in_schools_share
  if (is.null(share_school) || !is.finite(share_school) || share_school < 0 ||
      share_school > 1) {
    stop(spec$geo, ': spec lacks a valid hs_in_schools_share -- run ',
         '03_head_start_school_overlap.R -- load_spec() reads it from ',
         'out/head_start_school_overlap.csv')
  }
  share_prek <- (spec$hs_in_prek %||% 0) / hs_slots
  if (!is.finite(share_prek) || share_prek < 0 || share_prek > 1) {
    stop(spec$geo, ': hs_in_prek must lie between 0 and the Head Start slots')
  }
  share_nieer <- spec$nieer_hs_share %||% NA_real_
  share <- if (share_prek > 0) {
    share_prek
  } else if (prek_source == 'all_settings' && !is.na(share_nieer)) {
    share_nieer
  } else {
    share_school
  }
  netted <- c('public pre-K' = 0, 'Head Start' = unname(p['Head Start']) * share,
              'Early Head Start' = 0)[names(p)]

  list(slots = slots, enrolled = e, not_centre = not_centre, centre = centre,
       age5 = age5, children = p, netted = netted, net = p - netted,
       gross = gross, total = gross - netted[['Head Start']],
       prek_source = prek_source,
       share = share, share_school = share_school, share_prek = share_prek,
       share_nieer = share_nieer,
       factors = list(ehs_pregnant_share = EHS_PREGNANT_SHARE,
                      enrolled_per_slot  = c('public pre-K' = 1,
                                             'Head Start' = HS_ENROLLED_PER_SLOT,
                                             'Early Head Start' = EHS_ENROLLED_PER_SLOT),
                      home_fcc_share     = c('public pre-K' = 0,
                                             'Head Start' = HS_HOME_FCC_SHARE,
                                             'Early Head Start' = EHS_HOME_FCC_SHARE),
                      age5_share         = a5))
}



build_geo <- function(spec) {

  #----------------------------------------------------------------------------
  # Runs one geography's spec, printing each step. Returned list = one result
  # per geography -- write_geo() (simulator file), check_geo(), comparison
  # scripts (05_, 07_; build_of()) all read it, none recompute.
  #
  # Params:
  #   - spec (list): from load_spec()
  #
  # Returns: (list)
  #   geo, name, region (chr)
  #   care (tibble): ecec_type, children, part_time (children), annual_hours
  #     -- ECEC_TYPES order
  #   participation (list), step 1: children_0_4 (PEP), nsch_rate (geography's
  #     rate; NYC = NY's), nsch_children (10+ hrs/week), universe_ratio (NSECE_UNIVERSE_RATIO),
  #     nonparental_total (rounded, any regular care)
  #   programs (list), step 4: program_children() -- counts by program at
  #     each step, total, overlap shares, conversion factors
  #   shares (list), steps 2-6: rest_mix (p0, five types), part_time (by
  #     type), formal, informal (both of nonparental_total), centre (paid
  #     center over paid center + Paid Home-Based)
  #   hours (list), step 6: pt_weekly, ft_weekly, weeks (constants.R),
  #     annual_per_child (by type)
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
  # 'pre-K 28,465, HS 11,781' -- one number at a time, so no common-width pad
  by_prog <- function(x, keep = names(x)) {
    lab <- c('public pre-K' = 'pre-K', 'Head Start' = 'HS', 'Early Head Start' = 'EHS')
    paste(lab[keep], vapply(round(x[keep]), big_n, character(1)), collapse = ', ')
  }
  cat('  programs enrolled     ', big_n(round(sum(pc$enrolled))),
      ' (', by_prog(pc$enrolled), ')\n', sep = '')
  cat('  less not in a center  ', big_n(round(sum(pc$not_centre))),
      ' (home-based, FCC options: ',
      by_prog(pc$not_centre, c('Head Start', 'Early Head Start')), ')\n', sep = '')
  cat('  less age 5+           ', big_n(round(sum(pc$age5))),
      ' (', by_prog(pc$age5, c('public pre-K', 'Head Start')), '; ',
      spec$region, ' shares)\n', sep = '')
  cat('  less HS in pre-K      ', big_n(round(pc$netted[['Head Start']])),
      sprintf(' (%.1f%% of HS: own school %.1f%%, in state pre-K %.1f%%)\n',
              100 * pc$share, 100 * pc$share_school, 100 * pc$share_prek), sep = '')
  cat('  program children      ', big_n(round(program)), '\n', sep = '')

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
  grid_hours <- grid_annual_hours(pt)
  care <- tibble::tibble(ecec_type    = ECEC_TYPES,
                         children     = unname(children),
                         part_time    = unname(children * pt[ECEC_TYPES]),
                         annual_hours = unname(children) *
                                          unname(grid_hours[ECEC_TYPES]))

  cat(sprintf('  part-time share       %s\n',
              paste(sprintf('%s %.0f%%', substr(ECEC_TYPES, 1, 6),
                            100 * pt[ECEC_TYPES]), collapse = '  ')))

  list(geo = spec$geo, name = spec$name, region = spec$region,
       care = care,
       participation = list(children_0_4      = n_children,
                            nsch_rate         = spec$nonparental_rate,
                            nsch_children     = nsch_total,
                            universe_ratio    = NSECE_UNIVERSE_RATIO,
                            nonparental_total = np_total),
       programs = pc,
       shares = list(rest_mix  = sh$rest_mix,
                     part_time = pt[ECEC_TYPES],
                     formal    = formal / np_total,    # realised
                     informal  = informal / np_total,
                     centre    = split),
       hours = list(pt_weekly        = PT_CARE_HOURS,
                    ft_weekly        = FT_CARE_HOURS,
                    weeks            = WEEKS_PER_YEAR,
                    annual_per_child = grid_hours[ECEC_TYPES]))
}



build_of <- function(geo) {

  #----------------------------------------------------------------------------
  # Geography's build_geo() result, step print-out muted -- what comparison
  # scripts read in place of re-deriving build quantities.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (list) build_geo() result
  #----------------------------------------------------------------------------

  utils::capture.output(res <- build_geo(load_spec(geo)))
  res
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
  # Geography's NSCH rate from out/nsch_participation_rate_base.csv
  # (01_nsch_participation_rate.R, BASE_WAVE: 2016-19 pool). Sub-state geography (NYC) has
  # no NSCH records -- takes parent state's rate as is (2026-10-06; ACS-based
  # adjustment moved NYC 0.577 -> 0.589; smoke policies' budget effect moved
  # <= 0.4%, demand subsidy <= 1.2%, employment effect <= 0.08 pp).
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (num) rate
  #----------------------------------------------------------------------------

  out_row('nsch_participation_rate_base.csv', parent_geo(geo),
          '01_nsch_participation_rate.R')$rate[[1]]
}



geo_spec <- function(geo, name, fips, region, prek, head_start,
                     early_head_start, hs_in_prek = 0, ...) {

  #----------------------------------------------------------------------------
  # SPEC_<GEO> for states/<GEO>.R: hand-researched inputs only. load_spec()
  # adds NSCH rate, SE, Head Start in-own-school share from out/.
  #
  # Params:
  #   - geo, name (chr); fips (num): state FIPS
  #   - region (chr): lower-case Census region -- 05_compare_nsece_shares.R,
  #     PROGRAM_AGE5_SHARE key on it
  #   - prek (num): public pre-K children (CCD; NIEER state pre-K where CCD
  #     has no count)
  #   - head_start (num): Head Start funded slots, ages 3-4 (NIEER)
  #   - early_head_start (num): Early Head Start funded slots (NIEER)
  #   - hs_in_prek (num): Head Start slots NIEER reports also in state pre-K
  #     (Appendix B HS less Table 4 HS increment) -- ONLY where `prek` is
  #     NIEER's state pre-K, every setting, or site file covers under
  #     SITE_FILE_MIN_COVER of NIEER's HS slots; else 0
  #   - ...: fields placed between fips, region (NYC: counties, county_fips)
  #
  # Returns: (list) geo, name, fips, ..., region, hs_in_prek, programs
  #          (tibble: program, n_children)
  #----------------------------------------------------------------------------

  list(geo = geo, name = name, fips = fips, ..., region = region,
       hs_in_prek = hs_in_prek,
       programs = tibble::tribble(
         ~program,           ~n_children,
         'public pre-K',     prek,
         'Head Start',       head_start,
         'Early Head Start', early_head_start))
}



load_spec <- function(geo) {

  #----------------------------------------------------------------------------
  # Sources states/<GEO>.R into global env, returns SPEC_<GEO> plus
  # nonparental_rate (parent state's for NYC), hs_in_schools_share read live from
  # out/, prek_all_settings, nieer_hs_share (nieer_prek_inputs()) --
  # machine-derived, so a hand copy would only drift.
  #
  # Params:
  #   - geo (chr): e.g. 'KY', 'NYC'
  #
  # Returns: (list) spec; stops if spec file or either out/ file missing
  #----------------------------------------------------------------------------

  spec <- get_spec(geo)

  spec$nonparental_rate    <- nsch_rate_of(geo)
  nieer <- nieer_prek_inputs(spec)
  spec$prek_all_settings   <- nieer$all_settings
  spec$nieer_hs_share      <- nieer$nieer_hs_share
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
  #   - res (list): build_geo() result; geo (chr)
  #
  # Returns: (chr) path written
  #----------------------------------------------------------------------------

  rows <- dplyr::select(res$care, ecec_type, annual_hours)
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
  #   - res (list): build_geo() result; geo (chr)
  #
  # Returns: (num) failures
  #----------------------------------------------------------------------------

  care <- res$care
  np   <- res$participation$nonparental_total
  say <- function(ok, msg) {
    cat('   ', if (ok) '[ok]  ' else '[FAIL]', msg, '\n'); if (!ok) 1 else 0
  }
  informal <- res$shares$informal

  sum(say(all(care$children > 0), 'every care type positive'),
      say(nrow(care) == length(ECEC_TYPES), paste(length(ECEC_TYPES), 'rows')),
      say(abs(sum(care$children) - np) < 1,
          'children sum to the non-parental total'),
      say(informal > 0.2 && informal < 0.7, 'informal share between 20% and 70%'),
      say(all(care$annual_hours > 0), 'hours positive'))
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
  # Returns: (list) build_geo() result per geography, invisibly
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
    r <- results[[g]]
    k <- setNames(r$care$children, r$care$ecec_type)
    np <- r$participation$nonparental_total
    tibble::tibble(geo = g, children_0_4 = r$participation$children_0_4,
                   non_parental = np,
                   centre_based = round(k[['Center-Based']] + k[['Unpaid Center-Based']]),
                   licensed_home = round(k[['Paid Home-Based']]),
                   informal = round(np - sum(k[FORMAL_TYPES])),
                   centre_share = round(r$shares$centre, 3))
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
