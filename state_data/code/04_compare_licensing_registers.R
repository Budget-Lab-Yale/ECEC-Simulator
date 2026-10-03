#------------------------------------------------------------------------------
# 04_compare_licensing_registers.R -- build's center-based counts against
# licensing registers.
#
# VALIDATION ONLY -- nothing feeds config/state/demand/<GEO>.csv. Build reaches
# center care via
# NSCH rate x shares x program counts; registers count licensed providers
# directly. Gap between two independent routes measures how far off build is.
#
# REGISTERS. 2026-09-18 build (commit 31307ce) scaled five geographies' own
# registers to NSECE formal share, making them non-independent of NSECE. Here
# they stand alone: capacity -> children w/ 31307ce's conversions, NO
# calibration factor.
#
#   KY  CCAoA republication of CHFS licensed + certified registers, 2018-09.
#       Measured enrollment per provider (2019 workforce study, Table 20).
#   NJ  DCF licensed centers, frozen 2019-05. Own fill rate (MRS 2021, Fig 7).
#       Homes: no provider-level register; DCF registered family child care
#       provider count, 2019-12-31 (KIDS COUNT), x 5-child legal max, MRS
#       2021 under-5 slot share + fill.
#   NY  OCFS register (centers outside NYC, all homes) + NYC DOHMH Article 47
#       group child care, both 2026-09. Rebased to 2019 per program type
#       against OCFS modality series; national NSECE fill rates.
#   NYC Same two registers, five boroughs.
#   VA  VDSS, every approval route, 2026-09. Licensed + voluntary registered
#       homes rebased to 2019 per type against ELPG program counts; centers
#       rebased to 2020-06 against VDSS locality matrix (ELPG center count
#       defines centers unlike register). National NSECE fill rates.
#
# REGISTER CENTRES hold private paid centers, most Head Start and EHS, state
# pre-K seats contracted to community centers -- NOT public-school pre-K
# (license-exempt in all five). Build side therefore Center-Based + Head Start
# + EHS (enrolled, less in-school share).
#
# Units: children 0-4, point in time. Register = providers x enrolled per
# provider, or under-5 capacity x fill rate. Build = model children,
# real-world population (PEP-based), before hours grid.
#
# Usage:
#   Rscript state_data/code/04_compare_licensing_registers.R
#     prints table, writes out/licensing_register_comparison.csv
#------------------------------------------------------------------------------

# dplyr/readr/tibble, build; main() guarded
source('state_data/code/00_build_demand_targets.R')



#==== CONVERSIONS -- capacity to children. All from 31307ce, sources unchanged.

# National fill rates, NSECE 2019 provider surveys. Only where state publishes
# no enrollment figure of its own.
#   center 0.9029 -- ACF, Measuring Supply Capacity at Center-based CCEE (2023),
#                    Exhibit 1, five under-5 bands summed
#   home   0.7954 -- NSECE 2019 home-based provider microdata, LISTED stratum
#                    (source 2, n = 3,584) -- sampled from state lists, which
#                    is what a register holds. Fill alone.
#   0.7546        -- same stratum, not-school-age share of enrollment
#                    (n = 3,102); applied only where register gives one
#                    all-ages home capacity. 31307ce held published 0.7567;
#                    derivation (measure_nsece_fill.R, commit 704affc) gives
#                    0.7546. Pooled strata (0.5159) = wrong population --
#                    unlisted stratum is FFN care, about half school age.
REG_FILL_CENTRE   <- 0.9029
REG_FILL_HOME     <- 0.7954
REG_HOME_U5_SHARE <- 0.7546

# KY. 2019 Workforce Study, Table 20, under-5 enrolled per provider (infant +
# toddler + preschool): directors 5 + 12 + 25 = 42, FCC 3 + 3 + 3 = 9.
# Directors n = 448, FCC n = 48.
KY_ENROLLED_PER_PROVIDER <- c(CENTRE = 42, HOME = 9)

# KY. Licensed home cap = 12 children; register carries no type column, so
# capacity <= 12 marks a home.
KY_HOME_CAPACITY_MAX <- 12

# KY. Under-5 share of mixed-age lump capacity, measured inside register: mean
# capacity of band against same band w/out school age (31307ce, states/KY.R).
KY_U5_SHARE <- c('INFANT TO SCHOOL_AGE'        = 0.789,
                 'TWO_TO_SCHOOL TO SCHOOL_AGE' = 0.487,
                 'TODDLER TO SCHOOL_AGE'       = 0.827)

# NJ. Market rate survey 2021, Figure 7, 2020 column, slot-weighted under-5.
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
# 1,032, school-age 815; under-5 enrolled FT + PT (Table 17) 243 + 160 +
# 269 + 43 + 297 + 384 = 1,396. Both ratios within same respondents -- no
# respondent count needed.
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

# VA home types with own 2019 + 2026 program counts (ELPG, Virginia Child Care
# Program Trends 2019-2026, Tableau export) -> per-type 2019 / 2026 rebase.
# 2026 counts match register (licensed 1,344 vs 1,343 rows, voluntary 183 vs
# 187). Local ordinance + system homes carry no register capacity -- add 0.
VA_HOME_SERIES <- c(
  'Family Day Home'                                        = 'Licensed Family Day Homes',
  'Voluntary Registration Registration Duration: Two Year' = 'Voluntary Registered Homes')



#==== REGISTERS -- one function per geography: setting (CENTRE | HOME), cap_u5,
#     children.

national_fill <- function(setting, home = REG_FILL_HOME) {

  #----------------------------------------------------------------------------
  # National fill rate per row, for registers w/out own enrollment (NY, VA).
  #
  # Params:
  #   - setting (chr vec): CENTRE | HOME
  #   - home (num): HOME rate (VA folds in under-5 share)
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
  # scaled to under-5 share, NJ's own fill rate. Homes: one row, registered
  # provider count 2019 x legal max x MRS under-5 slot share x MRS fill.
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
  # group child care (GCC); SACC never enters. Center under-5 = infant +
  # toddler + preschool capacity; home under-5 = total less school age, so
  # homes take fill rate alone, no under-5 share.
  #
  # Params:
  #   - counties (chr vec|NULL): NULL for whole state
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  gcc <- read_chr(raw_path('NYC_DOHMH__licensed_providers_by_borough__2026-09.csv')) %>%
    dplyr::filter(facility_type == 'GCC') %>%
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
  # rates. Homes give one all-ages capacity -- under-5 share x fill.
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



#==== COMPARISON ==============================================================

# Build Paid Home-Based = paid, no prior relationship, licensed or not;
# register = licensed / registered only. LISTED_HOME_SHARE (t) maps build to
# register universe: NSECE listed-home children / Paid Home-Based-like
# children, national (cluster/nsece_listed_home_share.py). Regions too thin
# in unlisted-paid stratum (Northeast n = 26) to use.
LISTED_HOME_SHARE <- local({
  f <- file.path(STATE_DATA_ROOT, 'out', 'nsece_listed_home_share.csv')
  t <- num(read_chr(f)$t[read_chr(f)$region == 'US'])
  if (length(t) != 1 || !is.finite(t) || t <= 0) stop('no national t in ', f)
  t
})

compare_geo <- function(geo) {

  #----------------------------------------------------------------------------
  # Register vs build children, centers and homes, one geography. Build
  # homes on register universe: Paid Home-Based x LISTED_HOME_SHARE.
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
  kids  <- setNames(res$children, res$ecec_type)
  # build_geo()'s own program components -- no second program_children() call
  d     <- attr(res, 'diagnostics')
  hs_ehs <- unname(d$program_children['Head Start']) * (1 - d$hs_in_schools_share) +
            unname(d$program_children['Early Head Start'])

  centre <- kids[['Center-Based']] + hs_ehs
  reg_c  <- rs('CENTRE', 'children')
  reg_h  <- if (any(reg$setting == 'HOME')) rs('HOME', 'children') else NA_real_
  home   <- kids[['Paid Home-Based']] * LISTED_HOME_SHARE

  tibble::tibble(
    geo                         = geo,
    register_centres            = sum(reg$setting == 'CENTRE'),
    register_centre_u5_capacity = rs('CENTRE', 'cap_u5'),
    register_centre_children    = reg_c,
    build_centre                = centre,
    gap_centre                  = centre / reg_c - 1,
    register_home_children      = reg_h,
    build_paid_home             = kids[['Paid Home-Based']],
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

  cat('Register check: build centre children vs licensing registers\n',
      '(children 0-4, point in time; gap = build / register - 1)\n\n', sep = '')
  out <- dplyr::bind_rows(lapply(names(REGISTERS), compare_geo))

  # Thousands; formatted one value at a time.
  k   <- function(x) format(round(x / 1000, 1), nsmall = 1, width = 7)
  pct <- function(x) ifelse(is.na(x), '     --', sprintf('%+6.0f%%', 100 * x))
  fmt <- '%-4s %8s %8s %8s %7s | %7s %7s %7s\n'
  cat(sprintf(fmt, 'geo', 'reg cap', 'reg kids', 'build', 'gap', 'reg hom',
              'bld hom', 'gap'))
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    cat(sprintf(fmt, r$geo, k(r$register_centre_u5_capacity), k(r$register_centre_children),
                k(r$build_centre), pct(r$gap_centre),
                if (is.na(r$register_home_children)) '     --' else k(r$register_home_children),
                k(r$build_listed_home), pct(r$gap_home)))
  }
  cat('\nbuild = Center-Based + Head Start + EHS; homes = Paid Home-Based x ',
      sprintf('%.4f', LISTED_HOME_SHARE), ' (NSECE listed-home share)\n',
      '\n(thousands of children)\n', sep = '')

  f <- file.path(STATE_DATA_ROOT, 'out', 'licensing_register_comparison.csv')
  readr::write_csv(out, f)
  cat('wrote', f, '\n')
  invisible(out)
}


if (sys.nframe() == 0) main_register_check()
