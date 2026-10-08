#------------------------------------------------------------------------------
# 04_compare_licensing_registers.R -- build's center and home children against
# licensing registers, same population both sides.
#
# VALIDATION ONLY -- nothing feeds config/state/demand/<GEO>.csv. Build reaches
# center and home care via NSCH rate x shares x program counts; registers count
# licensed providers directly.
#
# SAME POPULATION. Children 0-4, not in kindergarten, every child enrolled in
# a licensed center or listed home, point in time.
#   ages     register capacity covers 0 to school age (holds 5-year-olds not
#            in kindergarten); build covers 0-4. Register centers x
#            AGE_0_4_SHARE_CENTRE (NSECE center enrollment 0-4 over 0-5 not in
#            K); register homes x age_0_4_share_home() (listed homes' 3-5
#            share x center 5-year-old share of 3-5).
#   counting register counts every enrolled child; build counts each child in
#            its primary type. Build side x NSECE any-over-primary ratio
#            (02_, quantity any_over_primary): children 0-4 w/ any
#            arrangement in setting over children w/ it as primary.
#   centers  build side: paid center (Center-Based) + Head Start net of pre-K
#            overlap + EHS (centers) + pre-K outside schools (all-settings
#            count less CCD count, x pre-K age coefficient; 0 where build takes
#            CCD). Registers hold private centers, Head Start, EHS, pre-K
#            contracted to community centers -- NOT public-school pre-K
#            (license-exempt in all five), so CCD pre-K out of both sides.
#   homes    build Paid Home-Based = paid, no prior relationship, licensed or
#            not; register = licensed / registered homes, every child. Build
#            side x LISTED_HOME_SHARE (t, cluster/nsece_listed_home_share.py):
#            NSECE listed-home children / paid nonrelative home children.
#   left     build centers hold license-exempt centers registers omit (except
#            VA, which lists religious-exempt) -- NSECE center file carries no
#            license status. Build side can only be too high from it.
#
# REGISTERS. Capacity -> children w/ 31307ce's conversions, no calibration
# factor.
#   KY  CCAoA republication of CHFS licensed + certified registers, 2018-09.
#       Measured enrollment per provider (2019 workforce study, Table 20).
#   NJ  DCF licensed centers, frozen 2019-05. Own fill rate (MRS 2021, Fig 7).
#       Homes: no provider-level register; DCF registered family child care
#       provider count, 2019-12-31 (KIDS COUNT), x 5-child legal max, MRS
#       2021 under-school-age slot share + fill.
#   NY  OCFS register (centers outside NYC, all homes) + NYC DOHMH Article 47
#       group child care, both 2026-09. Rebased to 2019 per program type
#       against OCFS modality series; national NSECE fill rates.
#   NYC Same two registers, five boroughs.
#   VA  VDSS, every approval route, 2026-09. Licensed + voluntary registered
#       homes rebased to 2019 per type against ELPG program counts; centers
#       rebased to 2020-06 against VDSS locality matrix. National NSECE fill
#       rates.
#
# Restored 2026-10-08 from 11ac301^ (dropped 2026-10-05) for memo section 4.2;
# ages and counting basis matched same day.
#
# Usage:
#   Rscript state_data/code/04_compare_licensing_registers.R
#     prints table, writes out/licensing_register_comparison.csv
#------------------------------------------------------------------------------

# dplyr/readr/tibble, build; main() guarded
source('state_data/code/00_build_demand_targets.R')
# NSECE_CENTRE_ENROLLED -- center enrollment ages 0-4, ACF Exhibit 1
source('state_data/code/shared_nsece_ages.R')



#==== CONVERSIONS -- capacity to children ======================================

# NSECE 2019 center enrollment by age, national, weighted -- ACF, Measuring
# Supply Capacity at Center-based CCEE (2023), Exhibit 1 (counts rounded to 3
# significant figures by ACF): ages 0-4 as NSECE_CENTRE_ENROLLED, 5 not in K
# 366,000.
NSECE_CENTRE_ENROLLED_0_5NK <- c(NSECE_CENTRE_ENROLLED, age_5_not_k = 366000)

# Register centers 0 to school age -> 0-4. 0.948.
AGE_0_4_SHARE_CENTRE <- sum(NSECE_CENTRE_ENROLLED) / sum(NSECE_CENTRE_ENROLLED_0_5NK)

# 5-year-olds not in K among center children 3-5 -- applied to listed homes'
# 3-5 band (NSECE home file splits under 3 / 3 to 5 only). 0.075.
AGE_5_SHARE_OF_3_5 <- NSECE_CENTRE_ENROLLED_0_5NK[['age_5_not_k']] /
  sum(NSECE_CENTRE_ENROLLED_0_5NK[c('age_3', 'age_4', 'age_5_not_k')])

# National fill rates, NSECE 2019 provider surveys. Only where state publishes
# no enrollment figure of its own.
#   center 0.9029 -- ACF, Measuring Supply Capacity at Center-based CCEE (2023),
#                    Exhibit 1, five under-5 bands summed
#   home   0.7954 -- NSECE 2019 home-based provider microdata, LISTED stratum
#                    (source 2, n = 3,584) -- sampled from state lists, which
#                    is what a register holds. Fill alone.
#   0.7546        -- same stratum, under-school-age share of enrollment
#                    (n = 3,102); applied only where register gives one
#                    all-ages home capacity (measure_nsece_fill.R, commit
#                    704affc).
REG_FILL_CENTRE   <- 0.9029
REG_FILL_HOME     <- 0.7954
REG_HOME_U5_SHARE <- 0.7546

# KY. 2019 Workforce Study, Table 20, enrolled per provider under school age
# (infant + toddler + preschool): directors 5 + 12 + 25 = 42, FCC 3 + 3 + 3 =
# 9. Directors n = 448, FCC n = 48.
KY_ENROLLED_PER_PROVIDER <- c(CENTRE = 42, HOME = 9)

# KY. Licensed home cap = 12 children; register carries no type column, so
# capacity <= 12 marks a home.
KY_HOME_CAPACITY_MAX <- 12

# KY. Under-school-age share of mixed-age lump capacity, measured inside
# register: mean capacity of band against same band w/out school age (31307ce,
# states/KY.R).
KY_U5_SHARE <- c('INFANT TO SCHOOL_AGE'        = 0.789,
                 'TWO_TO_SCHOOL TO SCHOOL_AGE' = 0.487,
                 'TODDLER TO SCHOOL_AGE'       = 0.827)

# NJ. Market rate survey 2021, Figure 7, 2020 column, slot-weighted under
# school age.
NJ_FILL_CENTRE <- 0.6733

# NJ. Age spans. Escape keeps source ASCII -- register writes '2 1/2' w/
# U+00BD (vulgar fraction one half).
NJ_AGES_SCHOOL_ONLY <- '6 to 13'
NJ_AGES_MIXED       <- c('0 to 13', '2\u00bd to 13', '2 to 13')
NJ_U5_SHARE_MIXED   <- 0.820

# NJ homes. Registered family child care providers, NJ DCF Office of
# Licensing via ACNJ / KIDS COUNT (indicator 2129), statewide, 2019-12-31.
# Registered home holds at most 5 children at a time (KIDS COUNT indicator
# definition).
# MRS 2021, 2020 column (pre-pandemic, nearest 2019), among responding FCC
# providers: slots by age (p. 18) infant 1,013, toddler 1,126, preschool
# 1,032, school-age 815; under-school-age enrolled FT + PT (Table 17) 243 +
# 160 + 269 + 43 + 297 + 384 = 1,396. Both ratios within same respondents.
NJ_FCC_MAX_CHILDREN <- 5
NJ_FCC_U5_SHARE     <- (1013 + 1126 + 1032) / (1013 + 1126 + 1032 + 815)  # 0.7955
NJ_FILL_HOME        <- 1396 / (1013 + 1126 + 1032)                        # 0.4402



county_names_of <- function(geo) {

  #----------------------------------------------------------------------------
  # County names of a sub-state geography -- registers carry names, not
  # codes. SUBSTATE_GEOGRAPHIES FIPS (constants.R) -> PEP CTYNAME, 'County'
  # dropped as registers write them.
  #
  # Params:
  #   - geo (chr): SUBSTATE_GEOGRAPHIES name, e.g. 'NYC'
  #
  # Returns: (chr vec)
  #----------------------------------------------------------------------------

  g   <- SUBSTATE_GEOGRAPHIES[[geo]]
  pep <- read_chr(raw_path('US_PEP__population_children_age_0_4_by_county__2019.csv'))
  hit <- num(pep$STATE) == g$statefip & num(pep$COUNTY) %in% g$countyfip
  if (sum(hit) != length(g$countyfip)) {
    stop(geo, ': PEP holds ', sum(hit), ' of ', length(g$countyfip), ' county FIPS')
  }
  sub(' County$', '', pep$CTYNAME[hit])
}



# NY. NYC's counties from its FIPS (constants.R) -- one definition of NYC.
NYC_COUNTIES <- county_names_of('NYC')

# DOHMH writes boroughs in caps.
NYC_BOROUGH_TO_COUNTY <- c('BRONX' = 'Bronx', 'BROOKLYN' = 'Kings',
                           'MANHATTAN' = 'New York', 'QUEENS' = 'Queens',
                           'STATEN ISLAND' = 'Richmond')
if (!setequal(NYC_BOROUGH_TO_COUNTY, NYC_COUNTIES)) {
  stop('DOHMH borough map names counties other than NYC\'s: ',
       paste(setdiff(NYC_BOROUGH_TO_COUNTY, NYC_COUNTIES), collapse = ', '))
}

# VA. Facility type -> setting, every approval route VDSS publishes.
VA_TYPE_SETTING <- c(
  'Child Day Center'                                       = 'CENTRE',
  'Religious Exempt Child Day Center'                      = 'CENTRE',
  'Short Term Child Day Center'                            = 'CENTRE',
  'Local Government Approved CDC'                          = 'CENTRE',
  'Certified Pre-School'                                   = 'CENTRE',
  'Family Day Home'                                        = 'HOME',
  'Local Ordinance Approved FDH'                           = 'HOME',
  'Unlicensed/Unregistered FDH'                            = 'HOME',
  'System Approved FDH'                                    = 'HOME',
  'Voluntary Registration Registration Duration: Two Year' = 'HOME')
VA_SCHOOL_AGE_MONTHS     <- 60   # span starting here or later -- school age only
VA_UNDER_SIX_MONTHS      <- 72   # span reaching past here -- mixed
VA_U5_SHARE_CENTRE_MIXED <- 0.60
VA_U5_SHARE_HOME_MIXED   <- 1.00 # home capped at 12 mixed ages; span says little

# VA home types w/ own 2019 + 2026 program counts (ELPG, Virginia Child Care
# Program Trends 2019-2026, Tableau export) -> per-type 2019 / 2026 rebase.
# 2026 counts match register (licensed 1,344 vs 1,343 rows, voluntary 183 vs
# 187). Local ordinance + system homes carry no register capacity -- add 0.
VA_HOME_SERIES <- c(
  'Family Day Home'                                        = 'Licensed Family Day Homes',
  'Voluntary Registration Registration Duration: Two Year' = 'Voluntary Registered Homes')



#==== REGISTERS -- one function per geography: setting (CENTRE | HOME), cap_u5,
#     children -- all 0 to school age.

national_fill <- function(setting, home = REG_FILL_HOME) {

  #----------------------------------------------------------------------------
  # National fill rate per row, for registers w/out own enrollment (NY, VA).
  #
  # Params:
  #   - setting (chr vec): CENTRE | HOME
  #   - home (num): HOME rate (VA folds in under-school-age share)
  #
  # Returns: (num vec)
  #----------------------------------------------------------------------------

  dplyr::if_else(setting == 'CENTRE', REG_FILL_CENTRE, home)
}



register_ky <- function() {

  #----------------------------------------------------------------------------
  # KY licensed + certified registers, 2018-09. School-age-only rows dropped;
  # rows w/ no age band kept at full capacity. Children = providers x measured
  # enrolled per provider -- no fill rate.
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  lic <- read_chr(raw_path('KY_CCAOA__licensed_providers_by_county__2018-09.csv')) %>%
    dplyr::filter(is.na(Age_Range_of_Service) | Age_Range_of_Service != 'SCHOOL_AGE') %>%
    dplyr::transmute(
      setting = dplyr::if_else(num(Capacity) <= KY_HOME_CAPACITY_MAX,
                               'HOME', 'CENTRE', missing = 'CENTRE'),
      cap_u5  = num(Capacity) *
                  dplyr::coalesce(unname(KY_U5_SHARE[Age_Range_of_Service]), 1))
  cert <- read_chr(raw_path('KY_CCAOA__certified_home_providers_by_county__2018-09.csv')) %>%
    dplyr::transmute(setting = 'HOME', cap_u5 = num(Capacity))

  dplyr::bind_rows(lic, cert) %>%
    dplyr::mutate(children = unname(KY_ENROLLED_PER_PROVIDER[setting]))
}



register_nj <- function() {

  #----------------------------------------------------------------------------
  # NJ DCF licensed centers, 2019-05. School-age-only dropped, mixed spans
  # scaled to under-school-age share, NJ's own fill rate. Homes: one row,
  # registered provider count 2019 x legal max x MRS under-school-age slot
  # share x MRS fill.
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  centres <- read_chr(raw_path('NJ_DCF__licensed_child_care_centers_by_county__2019-05.csv')) %>%
    dplyr::filter(is.na(ages) | ages != NJ_AGES_SCHOOL_ONLY) %>%
    dplyr::transmute(
      setting  = 'CENTRE',
      cap_u5   = num(capacity) * dplyr::if_else(ages %in% NJ_AGES_MIXED,
                                                NJ_U5_SHARE_MIXED, 1, missing = 1),
      children = cap_u5 * NJ_FILL_CENTRE)

  fcc <- readxl::read_excel(raw_path(
           'NJ_KIDSCOUNT__registered_family_child_care_providers_by_county__1996-2025.xlsx'))
  n <- num(fcc$Data[fcc$LocationType == 'State' & fcc$TimeFrame == '2019'])
  if (length(n) != 1 || !is.finite(n)) stop('NJ: no statewide 2019 FCC provider count')

  dplyr::bind_rows(centres, tibble::tibble(
    setting  = 'HOME',
    cap_u5   = n * NJ_FCC_MAX_CHILDREN * NJ_FCC_U5_SHARE,
    children = cap_u5 * NJ_FILL_HOME))
}



ny_drift <- function() {

  #----------------------------------------------------------------------------
  # 2019 / 2025 capacity factor per program type, rest of state, NYC apart,
  # from OCFS modality series (registers 2026-09 exports; 2025 series' nearest
  # year). DCC series covers DOHMH-permitted NYC centers too, so NYC's DCC
  # factor rebases DOHMH GCC; SDCC takes DCC's.
  #
  # Returns: (tibble) area (ROS | NYC), program_type, factor
  #----------------------------------------------------------------------------

  f <- read_chr(raw_path(
         'NYS_OCFS__provider_counts_and_capacity_by_modality__2015-2025.csv')) %>%
    dplyr::filter(measure == 'capacity', region %in% c('ROS', 'NYC'),
                  modality %in% c('DCC', 'GFDC', 'FDC'), year %in% c('2019', '2025')) %>%
    dplyr::mutate(value = num(value)) %>%
    dplyr::group_by(area = region, modality) %>%
    dplyr::summarise(factor = value[year == '2019'] / value[year == '2025'],
                     .groups = 'drop')
  if (nrow(f) != 6) stop('NY drift: expected 6 area x modality cells, found ', nrow(f))

  dcc <- dplyr::filter(f, modality == 'DCC')
  dplyr::bind_rows(f,
                   dplyr::mutate(dcc, modality = 'SDCC'),
                   dplyr::mutate(dcc, modality = 'GCC')) %>%
    dplyr::rename(program_type = modality)
}



register_ny <- function(counties = NULL) {

  #----------------------------------------------------------------------------
  # NY OCFS register (DCC/SDCC outside NYC, GFDC/FDC everywhere) + NYC DOHMH
  # group child care (GCC) and school-based child care (SBCC); SACC never
  # enters. Center under school age =
  # infant + toddler + preschool capacity; home = total less school age, so
  # homes take fill rate alone.
  #
  # Params:
  #   - counties (chr vec|NULL): NULL for whole state
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  # GCC + SBCC (school-based child care: private and religious-school
  # preschools, ages 2-5 -- paid centers in build's terms); both take DCC
  # rebase factor
  gcc <- read_chr(raw_path('NYC_DOHMH__licensed_providers_by_borough__2026-09.csv')) %>%
    dplyr::filter(facility_type %in% c('GCC', 'SBCC')) %>%
    dplyr::transmute(county = unname(NYC_BOROUGH_TO_COUNTY[borough]),
                     program_type = 'GCC', gcc_capacity = capacity) %>%
    dplyr::filter(!is.na(county))

  d <- read_chr(raw_path('NYS_OCFS__licensed_providers_by_county__2026-09.csv')) %>%
    dplyr::bind_rows(gcc) %>%
    dplyr::filter(program_type %in% c('DCC', 'SDCC', 'GFDC', 'FDC', 'GCC'))
  if (!is.null(counties)) d <- dplyr::filter(d, county %in% counties)

  d <- d %>%
    dplyr::mutate(area = dplyr::if_else(county %in% NYC_COUNTIES, 'NYC', 'ROS')) %>%
    dplyr::left_join(ny_drift(), by = c('area', 'program_type'))
  if (anyNA(d$factor)) stop('NY drift: a register row has no rebase factor')

  g <- function(x) dplyr::coalesce(num(x), 0) * d$factor
  centre_u5 <- g(d$infant_capacity) + g(d$toddler_capacity) + g(d$preschool_capacity)
  home_u5   <- pmax(g(d$total_capacity) - g(d$school_age_capacity), 0)

  tibble::tibble(
    setting = dplyr::if_else(d$program_type %in% c('GFDC', 'FDC'), 'HOME', 'CENTRE'),
    cap_u5  = dplyr::case_when(d$program_type == 'GCC'             ~ g(d$gcc_capacity),
                               d$program_type %in% c('DCC', 'SDCC') ~ centre_u5,
                               TRUE                                 ~ home_u5)) %>%
    dplyr::mutate(children = cap_u5 * national_fill(setting))
}



va_months <- function(x) {

  #----------------------------------------------------------------------------
  # VDSS age string -> months: 'Birth' = 0, '6 years 11 months' = 83. Blank ->
  # NA, other unparseable -> 0; either way row keeps full capacity.
  #
  # Params:
  #   - x (chr vec)
  #
  # Returns: (num vec) months
  #----------------------------------------------------------------------------

  y <- num(sub('.*?([0-9]+) *year.*',  '\\1', x))
  m <- num(sub('.*?([0-9]+) *month.*', '\\1', x))
  y[is.na(y) | !grepl('year',  x)] <- 0
  m[is.na(m) | !grepl('month', x)] <- 0
  out <- y * 12 + m
  out[grepl('^[[:space:]]*Birth', x)] <- 0
  out[is.na(x) | x == ''] <- NA_real_
  out
}



register_va <- function() {

  #----------------------------------------------------------------------------
  # VA VDSS register, 2026-09, every approval route. Spans parsed at ' - '.
  # School-age-only dropped, mixed spans scaled. Home types in VA_HOME_SERIES
  # rebased to 2019 by own program-count ratio; rest by one statewide factor
  # to 2020-06 (VDSS matrix CC slots / register capacity). National fill
  # rates. Homes give one all-ages capacity -- under-school-age share x fill.
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  d <- read_chr(raw_path('VA_VDSS__licensed_providers_by_locality__2026-09.csv'))
  unknown <- setdiff(unique(d$facility_type), names(VA_TYPE_SETTING))
  if (length(unknown)) {
    stop('VA: facility type(s) not mapped: ', paste(unknown, collapse = ', '),
         '. Map each to CENTRE or HOME.')
  }

  lo <- va_months(sub(' *-.*$', '', d$ages))
  hi <- va_months(sub('^.*- *', '', d$ages))
  setting     <- unname(VA_TYPE_SETTING[d$facility_type])
  school_only <- !is.na(lo) & lo >= VA_SCHOOL_AGE_MONTHS
  mixed       <- !is.na(hi) & hi >= VA_UNDER_SIX_MONTHS & !school_only
  share <- dplyr::case_when(!mixed              ~ 1,
                            setting == 'CENTRE' ~ VA_U5_SHARE_CENTRE_MIXED,
                            TRUE                ~ VA_U5_SHARE_HOME_MIXED)

  m20 <- read_chr(raw_path('VA_VDSS20__locality_capacity_and_program_slots__2020-06.csv'))
  cc  <- grep('^CC Slots', names(m20), value = TRUE)
  if (length(cc) != 1) stop('VA: expected one "CC Slots" column, found ', length(cc))
  f <- sum(num(m20[[cc]]), na.rm = TRUE) / sum(num(d$capacity), na.rm = TRUE)

  elpg <- read_chr(raw_path('VA_ELPG__regulated_child_care_programs_by_type__2019-2026.csv'))
  type_f <- vapply(VA_HOME_SERIES, function(s) {
    v <- function(y) num(gsub(',', '', elpg$`Measure Values`[elpg$`Measure Names` == s &
                                                              elpg$Year == y]))
    r <- v('2019') / v('2026')
    if (length(r) != 1 || !is.finite(r)) stop('VA: no 2019 / 2026 ELPG count for ', s)
    r
  }, numeric(1))
  f <- dplyr::coalesce(unname(type_f[d$facility_type]), f)

  tibble::tibble(setting = setting, school_only = school_only,
                 cap_u5  = num(d$capacity) * share * f) %>%
    dplyr::filter(!school_only) %>%
    dplyr::transmute(setting, cap_u5,
                     children = cap_u5 * national_fill(setting,
                                                       REG_FILL_HOME * REG_HOME_U5_SHARE))
}



REGISTERS <- list(KY  = register_ky,
                  NJ  = register_nj,
                  NY  = function() register_ny(NULL),
                  NYC = function() register_ny(NYC_COUNTIES),
                  VA  = register_va)



#==== SAME-POPULATION FACTORS -- NSECE, national ==============================

# Listed-home share t and listed homes' 3-5 share of under-school-age
# children, cluster/nsece_listed_home_share.py. Regions too thin in
# unlisted-paid stratum (Northeast n = 26) to use.
LISTED_HOME <- local({
  d <- read_chr(file.path(STATE_DATA_ROOT, 'out', 'nsece_listed_home_share.csv'))
  d <- d[d$region == 'US', ]
  x <- c(t = num(d$t), share_3to5 = num(d$listed_3to5_share))
  if (length(x) != 2 || any(!is.finite(x))) {
    stop('out/nsece_listed_home_share.csv lacks US t or listed_3to5_share -- rerun ',
         'cluster/nsece_listed_home_share.py')
  }
  x
})
LISTED_HOME_SHARE <- LISTED_HOME[['t']]

# Licensed share by state rule: NSECE listed-home children in homes at or over
# geography's 2019 licensing threshold (count basis: unrelated children) /
# paid nonrelative home children -- national listed homes, state rule. NA:
# no mandatory home licensing (NJ) -- register holds voluntary sign-ups only,
# no home comparison.
home_share_of <- function(geo) {
  r <- read_chr(raw_path('US_STATELAW__fcc_home_licensing_threshold_register_states__2019.csv'))
  r <- r[r$geo == geo, ]
  if (nrow(r) != 1) stop(geo, ': no 2019 licensing threshold row')
  if (r$basis == 'none') return(NA_real_)
  s <- read_chr(file.path(STATE_DATA_ROOT, 'out', 'nsece_home_threshold_share.csv'))
  v <- num(s$share[s$basis == r$basis & num(s$threshold) == num(r$threshold)])
  if (length(v) != 1 || !is.finite(v)) {
    stop(geo, ': no NSECE share for ', r$basis, ' >= ', r$threshold,
         ' -- add it to THRESHOLDS in cluster/nsece_listed_home_share.py')
  }
  v
}

# Register homes 0 to school age -> 0-4: only 3-5 band holds 5-year-olds
AGE_0_4_SHARE_HOME <- 1 - LISTED_HOME[['share_3to5']] * AGE_5_SHARE_OF_3_5

# Children w/ any arrangement in setting over children w/ it as primary
ANY_OVER_PRIMARY <- local({
  k <- read_chr(file.path(STATE_DATA_ROOT, 'out', 'nsece_constants.csv'))
  k <- k[k$quantity == 'any_over_primary', ]
  x <- setNames(num(k$value), k$key)
  if (!all(c('centre', 'Paid Home-Based') %in% names(x))) {
    stop('out/nsece_constants.csv lacks any_over_primary rows -- rerun 02_')
  }
  x
})



programs_outside_paid_centres <- function(spec, pc) {

  #----------------------------------------------------------------------------
  # Center children 0-4 a register holds that build puts outside Center-Based:
  # Head Start net of pre-K overlap, EHS (centers), pre-K outside schools
  # (all-settings count less CCD count, x pre-K age coefficient; 0 where build
  # takes CCD).
  #
  # Params:
  #   - spec (list): load_spec() result
  #   - pc (list): program_children() result
  #
  # Returns: (named num) hs_ehs, prek_out
  #----------------------------------------------------------------------------

  ccd <- spec$programs$n_children[spec$programs$program == 'public pre-K']
  c(hs_ehs   = pc$net[['Head Start']] + pc$children[['Early Head Start']],
    prek_out = if (pc$prek_source == 'ccd') 0 else
      max(pc$slots[['public pre-K']] - ccd, 0) * (1 - pc$factors$age5_share[['public pre-K']]))
}



register_paid_care <- function(geo) {

  #----------------------------------------------------------------------------
  # Register children 0-4 in build terms -- inverse of compare_geo()'s
  # mapping, program counts only (no build mix), so no loop w/ build:
  #   Center-Based    = register centre 0-4 / any-over-primary - HS, EHS,
  #                     pre-K outside schools
  #   Paid Home-Based = register home 0-4 / (any-over-primary x licensed
  #                     share); NA w/out mandatory home register (NJ)
  #
  # Params:
  #   - geo (chr): name in REGISTERS
  #
  # Returns: (tibble) one row: geo, centre, home
  #----------------------------------------------------------------------------

  reg <- REGISTERS[[geo]]()
  rs  <- function(s) sum(reg$children[reg$setting == s], na.rm = TRUE)
  spec <- load_spec(geo)
  spec$register_paid <- NULL
  p  <- programs_outside_paid_centres(spec, program_children(spec))
  h_share <- home_share_of(geo)
  reg_h <- if (is.na(h_share) || !any(reg$setting == 'HOME')) NA_real_ else
    rs('HOME') * AGE_0_4_SHARE_HOME
  tibble::tibble(
    geo    = geo,
    centre = rs('CENTRE') * AGE_0_4_SHARE_CENTRE / ANY_OVER_PRIMARY[['centre']] -
               p[['hs_ehs']] - p[['prek_out']],
    home   = reg_h / (ANY_OVER_PRIMARY[['Paid Home-Based']] * h_share))
}



compare_geo <- function(geo) {

  #----------------------------------------------------------------------------
  # Register vs build children 0-4, centers and homes, one geography (header,
  # SAME POPULATION).
  #
  # Params:
  #   - geo (chr): name in REGISTERS
  #
  # Returns: (tibble) one row
  #----------------------------------------------------------------------------

  reg <- REGISTERS[[geo]]()
  rs  <- function(s, col) sum(reg[[col]][reg$setting == s], na.rm = TRUE)

  spec  <- load_spec(geo)
  utils::capture.output(res <- build_geo(spec))
  kids  <- setNames(res$care$children, res$care$ecec_type)
  # build_geo()'s own program components -- no second program_children() call
  p <- programs_outside_paid_centres(spec, res$programs)
  hs_ehs <- p[['hs_ehs']]; prek_out <- p[['prek_out']]

  centre <- (kids[['Center-Based']] + hs_ehs + prek_out) * ANY_OVER_PRIMARY[['centre']]
  h_share <- home_share_of(geo)
  home   <- kids[['Paid Home-Based']] * ANY_OVER_PRIMARY[['Paid Home-Based']] * h_share
  reg_c  <- rs('CENTRE', 'children') * AGE_0_4_SHARE_CENTRE
  reg_h  <- if (is.na(h_share) || !any(reg$setting == 'HOME')) NA_real_ else
    rs('HOME', 'children') * AGE_0_4_SHARE_HOME

  tibble::tibble(
    geo                         = geo,
    register_centres            = sum(reg$setting == 'CENTRE'),
    register_centre_capacity    = rs('CENTRE', 'cap_u5'),
    register_centre_children    = reg_c,
    build_paid_centre           = kids[['Center-Based']],
    build_hs_ehs                = hs_ehs,
    build_prek_outside_schools  = prek_out,
    build_centre                = centre,
    gap_centre                  = centre / reg_c - 1,
    register_home_children      = reg_h,
    build_paid_home             = kids[['Paid Home-Based']],
    home_licensed_share         = h_share,
    build_listed_home           = home,
    gap_home                    = home / reg_h - 1)
}



main_register_check <- function() {

  #----------------------------------------------------------------------------
  # Every geography in REGISTERS: prints table, writes
  # out/licensing_register_comparison.csv.
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  # Register paid care in build terms -- 00_ reads it (register_paid_of()).
  # Rebuild demand targets after this writes, then rerun this script: gaps
  # then check the round trip (0 where the register sets the type).
  rp <- dplyr::bind_rows(lapply(names(REGISTERS), register_paid_care))
  fp <- file.path(STATE_DATA_ROOT, 'out', REGISTER_PAID_FILE)
  readr::write_csv(rp, fp)
  cat('wrote', fp, '\n')
  show_df(dplyr::mutate(rp, dplyr::across(c(centre, home), round)))

  cat('\nRegister check: build vs licensing registers, children 0-4, every enrolled child\n',
      '(point in time; gap = build / register - 1)\n\n', sep = '')
  out <- dplyr::bind_rows(lapply(names(REGISTERS), compare_geo))

  # Thousands; formatted one value at a time.
  k   <- function(x) format(round(x / 1000, 1), nsmall = 1, width = 7)
  pct <- function(x) ifelse(is.na(x), '     --', sprintf('%+6.0f%%', 100 * x))
  fmt <- '%-4s %8s %8s %7s | %7s %7s %7s\n'
  cat(sprintf(fmt, 'geo', 'reg ctr', 'build', 'gap', 'reg hom', 'bld hom', 'gap'))
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    cat(sprintf(fmt, r$geo, k(r$register_centre_children), k(r$build_centre),
                pct(r$gap_centre),
                if (is.na(r$register_home_children)) '     --' else k(r$register_home_children),
                if (is.na(r$home_licensed_share)) "     --" else k(r$build_listed_home), pct(r$gap_home)))
  }
  cat(sprintf(paste0('\nregister ages 0-4: centers x %.4f, homes x %.4f; build any/primary: ',
                     'centers %.4f, homes %.4f; listed-home share %.4f\n(thousands of children)\n'),
              AGE_0_4_SHARE_CENTRE, AGE_0_4_SHARE_HOME, ANY_OVER_PRIMARY[['centre']],
              ANY_OVER_PRIMARY[['Paid Home-Based']], LISTED_HOME_SHARE))

  f <- file.path(STATE_DATA_ROOT, 'out', 'licensing_register_comparison.csv')
  readr::write_csv(out, f)
  cat('wrote', f, '\n')
  invisible(out)
}


if (sys.nframe() == 0) main_register_check()
