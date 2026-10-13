#------------------------------------------------------------------------------
# 00_build_demand_targets.R -- state-level demand inputs for the simulator:
# config/state/demand/<GEO>.csv, six rows, annual hours by care type. Method
# memos (docs/archive/<date>_summary.pdf) define each number.
#
# METHOD, per geography. Step numbers name quantities across state_data/
# (README, memos) -- fixed, though p0 runs them in
# order 1, 4, 2-3-5, 6.
#   1. children in regular care = population 0-4 x NSCH rate x universe ratio
#   4. Unpaid Center-Based = program counts in centers, ages 0-4, less Head
#      Start children pre-K count already holds (program_children()); pre-K
#      = larger of CCD, all-settings count (NIEER; NYC DOE)
#   2, 3, 5. rest (step 1 less step 4) split over other five types by
#      simulator p0 summed over geography's own ACS households. Formal share,
#      informal mix, center-home split all results.
#   6. annual hours = children x simulator's hours grid x 50, split part-time /
#      full-time by p0's part-time share per type. Unpaid Center-Based too --
#      program-by-program hours (NHES weekly, PIR schedule, operating weeks)
#      sensitivity only, 14_program_hours_sensitivity.R (header says why)
# Regional NSECE shares: comparison only, 05_compare_nsece_shares.R.
#
# states/<GEO>.R = hand-researched DATA (geo_spec()). NSCH rate read live
# from out/ (01_ writes it) -- nothing
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
# children at 10+ hrs/week (what NSCH asks). 3,427 children. One national
# ratio for every geography (2026-10-12; scaled by geography's p0 part-time
# share 2026-10-07 to 10-12 -- moved ratio +-2.5%, made targets depend on
# forest, income-rank scheme). Regular care per resolve_regular_care()
# (2026-10-12: zero-hour regular providers count w/ imputed hours, REGCARE
# -1 / -2 regular when TOC 1-5) -- ratio 1.0981 -> 1.0739, 2,814 children
# before. Imputed hours drawn from cell's weighted donor hours (keyed,
# 2026-10-12; cell weighted median before) -- more imputed rows under 10
# hrs, universe B 3,239 -> 3,157 children, ratio 1.0739 -> 1.0979. TOC 7 /
# 8 never regular (NSECE regular care = TOC 1-5, 2026-10-12; TOC 8 counted
# Other before) -- universe A 3,462 -> 3,427, B 3,157 -> 3,124 children,
# ratio 1.09786 -> 1.09806. Label draws averaged over NSECE_MI_DRAWS = 50
# (nsece_mi_draws.R, 2026-10-12; one draw before, seed moved ratio
# 1.094-1.105) -- ratio 1.09806 -> 1.09949; 5 base seed pairs 1.0990-1.0996.
NSECE_UNIVERSE_RATIO <- 1.0994893847297402

# Short share: NSECE children under 10 total hrs/week over children part-time
# in simulator's sense (primary hours < 20). Every under-10 child part-time.
# 02_derive_nsece_constants.R, quantity short_share_of_part_time. Not used by
# build since national ratio -- 05_compare_nsece_shares.R reads it.
NSECE_SHORT_SHARE_OF_PT <- 0.34957429599815637

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

# FORMAL_TYPES: shared_setup.R (also 02)

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

# FIVE-YEAR-OLDS (2026-10-07). Population = children 0-4; pre-K grade, Head
# Start 3-4 slots hold children who turn 5 during program year (NIEER folds 5s
# into 4s; CCD counts a grade, not an age). One national share 5+ per program
# at 2018-12-31: NHES-ECPP 2019 (raw/US/), age AGE2018, not
# yet in kindergarten, center program now (CPNNOWX = 1), ages 3+, FEWT; pre-K
# CPPLACEX = 2 (483 records), HS CPHEADST = 1 (247). Same share on CCD, NIEER
# counts -- larger-of rule unaffected by order. Documented source rule
# overrides: NYC DOE enrols by calendar year (2019 kindergarten guide: born in
# 2014 -> pre-K 2018-19 all 4 on Dec 31); HS age follows public-school date
# (45 CFR 1302.12(b)) -> NYC 0 both programs. NY: national share on rest of
# state only -- NY less NYC = rest-of-state build (check_nyc_rest_of_state.R).
# EHS holds no 5s (EHS snapshot: 5-year-old row blank).
# check_constants_match_derivation.R recomputes NHES values.
AGE5_NHES_NATIONAL <- c(prek = 0.24395365244042602, head_start = 0.10740813090244962)
AGE5_CALENDAR_YEAR <- 'NYC'

# ALL-SETTINGS PRE-K (2026-10-06). CCD counts pre-K in public schools only,
# missing community-based public pre-K. All-settings count: NIEER Table 2
# state pre-K, ages 3-4, every setting -- NOT Table 4's pre-K + special
# education (2026-10-06: many special-education children get services at
# home or in regular child care, not a center seat). Spec's
# prek_all_settings overrides: NYC own DOE count (NIEER publishes states
# only). program_children()
# takes larger of CCD, all-settings count. CA, MI pre-K figure already
# NIEER's. Table 4 sets NIEER overlap shares (its Head Start increment), pooled
# into one share (pooled_overlap_share()).
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



program_age5_share <- function(spec) {

  #----------------------------------------------------------------------------
  # Pre-K, Head Start share aged 5+ (header, FIVE-YEAR-OLDS): national NHES
  # share; 0 in AGE5_CALENDAR_YEAR; parent state of such a geography applies
  # national share to rest of state only. spec$age5_share overrides (tests:
  # rest of state).
  #
  # Params:
  #   - spec (list): from load_spec()
  #
  # Returns: (named num) prek, head_start
  #----------------------------------------------------------------------------

  if (!is.null(spec$age5_share)) return(spec$age5_share)
  if (spec$geo %in% AGE5_CALENDAR_YEAR) return(c(prek = 0, head_start = 0))

  # Pre-K count program_children() takes (larger of CCD, all settings); HS slots
  counts <- function(s) {
    n <- setNames(s$programs$n_children, s$programs$program)
    c(prek = max(n[['public pre-K']], s$prek_all_settings %||% 0), head_start = n[['Head Start']])
  }
  share <- AGE5_NHES_NATIONAL
  for (sg in AGE5_CALENDAR_YEAR) {
    if (parent_geo(sg) == spec$geo && sg != spec$geo) {
      share <- share * (1 - counts(get_spec(sg)) / counts(spec))
    }
  }
  share
}




overlap_branch <- function(spec) {

  #----------------------------------------------------------------------------
  # Pre-K count taken (step 0) for one geography, and whether its NIEER HS
  # overlap share enters pooled_overlap_share() -- see program_children().
  #
  # Params:
  #   - spec (list): from load_spec()
  #
  # Returns: (list) prek_source ('ccd' | 'all_settings' | 'nieer'), branch
  #   ('nieer' = in pool | 'fallback'), share_nieer (num or NA)
  #----------------------------------------------------------------------------

  n <- setNames(spec$programs$n_children, spec$programs$program)
  prek_source <- if (parent_geo(spec$geo) %in% PREK_FROM_NIEER) {
    'nieer'
  } else if ((spec$prek_all_settings %||% 0) > n[['public pre-K']]) {
    'all_settings'
  } else {
    'ccd'
  }
  share_nieer <- spec$nieer_hs_share %||% NA_real_
  branch <- if (prek_source == 'all_settings' && !is.na(share_nieer)) 'nieer' else 'fallback'
  list(prek_source = prek_source, branch = branch, share_nieer = share_nieer)
}



# Cache -- pooled_overlap_share() loads every state spec once per session.
POOLED_OVERLAP <- new.env()

pooled_overlap_share <- function() {

  #----------------------------------------------------------------------------
  # HS overlap share, every geography: HS-slot-weighted mean NIEER share over
  # states + DC on NIEER branch (overlap_branch(): all-settings count taken,
  # Table 4 unduplicated). Sub-state geographies out -- NYC takes NY's share,
  # would count NY twice.
  #
  # Returns: (num)
  #----------------------------------------------------------------------------

  if (!is.null(POOLED_OVERLAP$rate)) return(POOLED_OVERLAP$rate)
  x <- vapply(names(STATE_POSTAL_TO_FIPS), function(g) {
    s <- load_spec(g)
    ob <- overlap_branch(s)
    if (ob$branch != 'nieer') return(c(0, 0))
    hs <- s$programs$n_children[s$programs$program == 'Head Start']
    c(hs, hs * ob$share_nieer)
  }, numeric(2))
  if (sum(x[1, ]) <= 0) stop('pooled_overlap_share(): no geography on NIEER branch')
  POOLED_OVERLAP$rate <- sum(x[2, ]) / sum(x[1, ])
  POOLED_OVERLAP$rate
}



program_children <- function(spec) {

  #----------------------------------------------------------------------------
  # Step 4 total -- program children in centers, ages 0-4, each counted once.
  #
  #   a. slots -> enrolled: EHS loses pregnant-women share, HS, EHS scale by
  #      enrolled per slot
  #   b. center care only: HS, EHS lose home-based, FCC option shares
  #   c. ages 0-4: pre-K, HS lose 5+ share (program_age5_share())
  #   0. pre-K count = larger of spec's (CCD) figure, prek_all_settings
  #      (header, ALL-SETTINGS PRE-K); CA, MI keep their NIEER figure
  #   d. each child once: deduct HS children pre-K count already holds. Share
  #      of HS slots = pooled_overlap_share(), one share every geography.
  #      Assumes overlap everywhere resembles NIEER-reporting states'.
  #      2026-10-08: replaced site matching (03_head_start_school_overlap.R),
  #      own NIEER shares, DC/MI direct counts -- each moved Unpaid
  #      Center-Based hours <= 4.7%, total hours <= 0.05%.
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
  #   prek_source ('ccd' | 'all_settings' | 'nieer'); branch ('nieer' |
  #   'fallback'); share (step d), share_nieer; factors
  #   (list): conversion
  #   factors applied -- ehs_pregnant_share, enrolled_per_slot, home_fcc_share,
  #   age5_share (by program)
  #----------------------------------------------------------------------------

  slots <- setNames(spec$programs$n_children, spec$programs$program)
  hs_slots <- slots[['Head Start']]

  # 0. pre-K count
  ob <- overlap_branch(spec)
  prek_source <- ob$prek_source
  if (prek_source == 'all_settings') slots['public pre-K'] <- spec$prek_all_settings

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
  s5 <- program_age5_share(spec)
  a5 <- c('public pre-K' = s5[['prek']], 'Head Start' = s5[['head_start']],
          'Early Head Start' = 0)
  age5 <- p * a5[names(p)]
  p <- p - age5
  gross <- sum(p)

  # d. HS children pre-K count already holds
  share <- pooled_overlap_share()
  netted <- c('public pre-K' = 0, 'Head Start' = unname(p['Head Start']) * share,
              'Early Head Start' = 0)[names(p)]

  list(slots = slots, enrolled = e, not_centre = not_centre, centre = centre,
       age5 = age5, children = p, netted = netted, net = p - netted,
       gross = gross, total = gross - netted[['Head Start']],
       prek_source = prek_source, branch = ob$branch,
       share = share, share_nieer = ob$share_nieer,
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
  #     rate; NYC = NY's), nsch_children (10+ hrs/week), universe_ratio (spec's, NSECE_UNIVERSE_RATIO),
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
  np_total   <- round(nsch_total * spec$universe_ratio)
  cat('  children 0-4          ', big_n(n_children), '\n')
  cat('  x non-parental rate   ', sprintf('%.1f%%', 100 * spec$nonparental_rate),
      ' -> ', big_n(round(nsch_total)), ' at 10+ hrs/week\n', sep = '')
  cat(sprintf('  x universe ratio      %.4f -> %s in any regular care\n',
              spec$universe_ratio, big_n(np_total)))

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
      ' (', by_prog(pc$age5, c('public pre-K', 'Head Start')), sprintf('; shares %.1f%%, %.1f%%',
      100 * pc$factors$age5_share[['public pre-K']], 100 * pc$factors$age5_share[['Head Start']]),
      ')\n', sep = '')
  cat('  less HS in pre-K      ', big_n(round(pc$netted[['Head Start']])),
      sprintf(' (%.1f%% of HS, %s)\n', 100 * pc$share, pc$branch), sep = '')
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
                            universe_ratio    = spec$universe_ratio,
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

# Two-count experiment targets (ECEC_STATE_CALIBRATION=counts,
# adjust_alpha_for_state_counts()): children, part_time by group. Own dir --
# hours file, default reader untouched.
SIMULATOR_COUNTS_DIR <- file.path('config', 'state', 'demand_counts')



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
                     early_head_start, ...) {

  #----------------------------------------------------------------------------
  # SPEC_<GEO> for states/<GEO>.R: hand-researched inputs only. load_spec()
  # adds NSCH rate, universe ratio, NIEER pre-K inputs.
  #
  # Params:
  #   - geo, name (chr); fips (num): state FIPS
  #   - region (chr): lower-case Census region -- 05_compare_nsece_shares.R
  #   - prek (num): public pre-K children (CCD; NIEER state pre-K where CCD
  #     has no count)
  #   - head_start (num): Head Start funded slots, ages 3-4 (NIEER)
  #   - early_head_start (num): Early Head Start funded slots (NIEER)
  #   - ...: fields placed between fips, region (NYC: counties, county_fips)
  #
  # Returns: (list) geo, name, fips, ..., region, programs
  #          (tibble: program, n_children)
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
  # nonparental_rate (parent state's for NYC), universe_ratio,
  # prek_all_settings, nieer_hs_share (nieer_prek_inputs()) -- machine-derived,
  # so a hand copy would only drift.
  #
  # Params:
  #   - geo (chr): e.g. 'KY', 'NYC'
  #
  # Returns: (list) spec; stops if spec file or either out/ file missing
  #----------------------------------------------------------------------------

  spec <- get_spec(geo)

  spec$nonparental_rate    <- nsch_rate_of(geo)
  spec$universe_ratio      <- NSECE_UNIVERSE_RATIO
  nieer <- nieer_prek_inputs(spec)
  spec$prek_all_settings   <- nieer$all_settings
  spec$nieer_hs_share      <- nieer$nieer_hs_share
  spec
}



write_geo <- function(res, geo) {

  #----------------------------------------------------------------------------
  # Writes SIMULATOR_DEMAND_DIR/<GEO>.csv -- file simulator reads, only copy
  # -- and SIMULATOR_COUNTS_DIR/<GEO>.csv (two-count experiment targets).
  #
  # Params:
  #   - res (list): build_geo() result; geo (chr)
  #
  # Returns: (chr) hours path written
  #----------------------------------------------------------------------------

  rows <- dplyr::select(res$care, ecec_type, annual_hours)
  dir.create(SIMULATOR_DEMAND_DIR, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(SIMULATOR_DEMAND_DIR, paste0(geo, '.csv'))
  readr::write_csv(rows, f)
  cat('  wrote                 ', f, '\n')

  counts <- dplyr::select(res$care, ecec_type, children, part_time)
  dir.create(SIMULATOR_COUNTS_DIR, showWarnings = FALSE, recursive = TRUE)
  fc <- file.path(SIMULATOR_COUNTS_DIR, paste0(geo, '.csv'))
  readr::write_csv(counts, fc)
  cat('  wrote                 ', fc, '\n')
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
