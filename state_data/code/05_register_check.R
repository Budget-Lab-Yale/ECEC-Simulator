#------------------------------------------------------------------------------
# 05_register_check.R -- build's center-based counts against licensing registers.
#
# VALIDATION ONLY. Nothing here feeds out/<GEO>.csv. Build reaches center care
# through NSCH rate x shares x program counts; registers count licensed
# providers directly. Two independent routes to same children -- gap between
# them measures how far off build is.
#
# REGISTERS. The 2026-09-18 build (commit 31307ce) read five geographies' own
# licensing registers, then scaled them to NSECE formal share. That scaling
# made its outputs non-independent of NSECE. Here registers stand alone: capacity
# -> children w/ same conversions 31307ce used, NO calibration factor.
#
#   KY  CCAoA republication of CHFS licensed + certified registers, 2018-09.
#       Measured enrollment per provider (2019 workforce study, Table 20).
#   NJ  DCF licensed centers, frozen 2019-05. Own fill rate (MRS 2021, Fig 7).
#       No family child care register exists -- homes not compared.
#   NY  OCFS register (centers outside NYC, all homes) + NYC DOHMH Article 47
#       group child care, both 2026-09. Rebased to 2019 per program type
#       against OCFS modality series; national NSECE fill rates.
#   NYC Same two registers, five boroughs.
#   VA  VDSS, every approval route, 2026-09. Rebased to 2020-06 against VDSS
#       locality matrix; national NSECE fill rates.
#
# WHAT A REGISTER CENTRE HOLDS. Licensed centers: private paid centers, most
# Head Start and EHS, state pre-K seats contracted to community centers.
# NOT public-school pre-K -- license-exempt in all five. So the build side is
# Center-Based + Head Start + EHS (enrolled, less in-school share).
#
# Units: children 0-4, point in time. Register side = providers x enrolled per
# provider, or under-5 capacity x fill rate. Build side = model children,
# real-world population (PEP-based), before hours grid.
#
# Usage:
#   Rscript state_data/code/05_register_check.R    # table + out/register_check.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble)
})

source('state_data/code/00_main.R')     # main() guarded; loads build only



#=============================================================================
# CONVERSIONS -- capacity to children. All from 31307ce, sources unchanged.
#=============================================================================

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

# NJ. Age spans. '½' keeps source ASCII -- register writes '2 1/2' w/ U+00BD.
NJ_AGES_SCHOOL_ONLY <- '6 to 13'
NJ_AGES_MIXED       <- c('0 to 13', '2½ to 13', '2 to 13')
NJ_U5_SHARE_MIXED   <- 0.820

county_names_of <- function(geo) {

  #----------------------------------------------------------------------------
  # County names of a sub-state geography, from its county FIPS in
  # SUBSTATE_GEOGRAPHIES (constants.R) via PEP's CTYNAME -- registers carry
  # names, not codes. 'County' suffix dropped, as registers write them.
  #
  # Params:
  #   - geo (chr): a SUBSTATE_GEOGRAPHIES name, e.g. 'NYC'
  #
  # Returns: (chr vec) county names
  #----------------------------------------------------------------------------

  g    <- SUBSTATE_GEOGRAPHIES[[geo]]
  pep  <- read_chr(raw_path('US_PEP__population_children_age_0_4_by_county__2019.csv'))
  hit  <- num(pep$STATE) == g$statefip & num(pep$COUNTY) %in% g$countyfip
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



#=============================================================================
# REGISTERS -- one function per geography, each returns setting, cap_u5,
# children. setting is CENTRE or HOME.
#=============================================================================

register_ky <- function() {

  #----------------------------------------------------------------------------
  # Kentucky: licensed + certified registers, 2018-09. School-age-only rows
  # dropped; rows w/ no age band kept at full capacity. Children = providers x
  # measured enrolled per provider -- no fill rate.
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  lic  <- read_chr(raw_path('KY_CCAOA__licensed_providers_by_county__2018-09.csv'))
  cert <- read_chr(raw_path('KY_CCAOA__certified_home_providers_by_county__2018-09.csv'))

  l <- lic %>%
    dplyr::filter(is.na(Age_Range_of_Service) | Age_Range_of_Service != 'SCHOOL_AGE') %>%
    dplyr::transmute(
      setting = dplyr::if_else(num(Capacity) <= KY_HOME_CAPACITY_MAX,
                               'HOME', 'CENTRE', missing = 'CENTRE'),
      cap_u5  = num(Capacity) *
                  dplyr::coalesce(unname(KY_U5_SHARE[Age_Range_of_Service]), 1))
  c_ <- dplyr::transmute(cert, setting = 'HOME', cap_u5 = num(Capacity))

  dplyr::bind_rows(l, c_) %>%
    dplyr::mutate(children = unname(KY_ENROLLED_PER_PROVIDER[setting]))
}



register_nj <- function() {

  #----------------------------------------------------------------------------
  # New Jersey: DCF licensed centers, 2019-05. School-age-only dropped, mixed
  # spans scaled to under-5 share, NJ's own fill rate. Centers only.
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  read_chr(raw_path('NJ_DCF__licensed_child_care_centers_by_county__2019-05.csv')) %>%
    dplyr::filter(is.na(ages) | ages != NJ_AGES_SCHOOL_ONLY) %>%
    dplyr::transmute(
      setting  = 'CENTRE',
      cap_u5   = num(capacity) * dplyr::if_else(ages %in% NJ_AGES_MIXED,
                                                NJ_U5_SHARE_MIXED, 1, missing = 1),
      children = cap_u5 * NJ_FILL_CENTRE)
}



ny_drift <- function() {

  #----------------------------------------------------------------------------
  # 2019 / 2025 capacity per program type, rest of state and NYC apart, from
  # OCFS's own modality series. Registers are 2026-09 exports; 2025 is the
  # series' nearest year. Computed from file, not typed -- moves if refreshed.
  #
  # DCC series covers NYC centers too (DOHMH-permitted), so NYC's DCC factor
  # rebases DOHMH GCC.
  #
  # Returns: (tibble) area (ROS|NYC), program_type, factor
  #----------------------------------------------------------------------------

  s <- read_chr(raw_path(
         'NYS_OCFS__provider_counts_and_capacity_by_modality__2015-2025.csv')) %>%
    dplyr::filter(measure == 'capacity', region %in% c('ROS', 'NYC'),
                  modality %in% c('DCC', 'GFDC', 'FDC'), year %in% c('2019', '2025')) %>%
    dplyr::mutate(value = num(value))

  f <- s %>%
    dplyr::group_by(area = region, modality) %>%
    dplyr::summarise(factor = value[year == '2019'] / value[year == '2025'],
                     .groups = 'drop')
  if (nrow(f) != 6) stop('NY drift: expected 6 area x modality cells, found ', nrow(f))

  # SDCC (small day care center) and GCC take DCC's factor.
  dplyr::bind_rows(f,
                   dplyr::mutate(dplyr::filter(f, modality == 'DCC'), modality = 'SDCC'),
                   dplyr::mutate(dplyr::filter(f, modality == 'DCC'), modality = 'GCC')) %>%
    dplyr::rename(program_type = modality)
}



register_ny <- function(counties = NULL) {

  #----------------------------------------------------------------------------
  # New York: OCFS register (DCC/SDCC outside NYC, GFDC/FDC everywhere) + NYC
  # DOHMH group child care. SACC never enters. Center under-5 = infant +
  # toddler + preschool capacity; home under-5 = total less school age, so
  # homes take fill rate alone, no under-5 share.
  #
  # Params:
  #   - counties (chr vec|NULL): NULL for whole state
  #
  # Returns: (tibble) setting, cap_u5, children
  #----------------------------------------------------------------------------

  ocfs  <- read_chr(raw_path('NYS_OCFS__licensed_providers_by_county__2026-09.csv'))
  dohmh <- read_chr(raw_path('NYC_DOHMH__licensed_providers_by_borough__2026-09.csv'))

  gcc <- dohmh %>%
    dplyr::filter(facility_type == 'GCC') %>%
    dplyr::transmute(county = unname(NYC_BOROUGH_TO_COUNTY[borough]),
                     program_type = 'GCC', gcc_capacity = capacity) %>%
    dplyr::filter(!is.na(county))

  d <- dplyr::bind_rows(ocfs, gcc) %>%
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
    dplyr::mutate(children = cap_u5 * dplyr::if_else(setting == 'CENTRE',
                                                     REG_FILL_CENTRE, REG_FILL_HOME))
}



va_months <- function(x) {

  #----------------------------------------------------------------------------
  # VDSS age string -> months. 'Birth' = 0; '2 years - 6 years 11 months'
  # parses both ends. Unparseable -> NA, row keeps full capacity.
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
  # Virginia: VDSS register, 2026-09, every approval route. School-age-only
  # dropped, mixed-span centers scaled, rebased to 2020-06 by one statewide
  # factor (VDSS matrix CC slots / register capacity), national fill rates.
  # Homes give one all-ages capacity -- take under-5 share x fill.
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

  tibble::tibble(setting = setting, school_only = school_only,
                 cap_u5  = num(d$capacity) * share * f) %>%
    dplyr::filter(!school_only) %>%
    dplyr::transmute(setting, cap_u5,
                     children = cap_u5 * dplyr::if_else(setting == 'CENTRE',
                                                        REG_FILL_CENTRE,
                                                        REG_FILL_HOME * REG_HOME_U5_SHARE))
}



REGISTERS <- list(KY  = register_ky,
                  NJ  = register_nj,
                  NY  = function() register_ny(NULL),
                  NYC = function() register_ny(NYC_COUNTIES),
                  VA  = register_va)



#=============================================================================
# COMPARISON
#=============================================================================

compare_geo <- function(geo) {

  #----------------------------------------------------------------------------
  # One geography: register children against build children, centers and
  # homes.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (tibble) one row
  #----------------------------------------------------------------------------

  reg <- REGISTERS[[geo]]()
  rs  <- function(s, col) sum(reg[[col]][reg$setting == s], na.rm = TRUE)

  spec  <- load_spec(geo)
  quiet <- utils::capture.output(res <- build_geo(spec))
  kids  <- setNames(res$children, res$ecec_type)
  pc    <- program_children(spec)
  hs_ehs <- unname(pc$children['Head Start']) * (1 - pc$share) +
            unname(pc$children['Early Head Start'])

  centre <- kids[['Center-Based']] + hs_ehs
  reg_c  <- rs('CENTRE', 'children')
  reg_h  <- if (any(reg$setting == 'HOME')) rs('HOME', 'children') else NA_real_

  tibble::tibble(
    geo                         = geo,
    register_centres            = sum(reg$setting == 'CENTRE'),
    register_centre_u5_capacity = rs('CENTRE', 'cap_u5'),
    register_centre_children    = reg_c,
    build_centre                = centre,
    gap_centre                  = centre / reg_c - 1,
    register_home_children      = reg_h,
    build_paid_home             = kids[['Paid Home-Based']],
    gap_home                    = kids[['Paid Home-Based']] / reg_h - 1)
}



main_register_check <- function() {

  #----------------------------------------------------------------------------
  # Runs every geography w/ a register, prints table, writes
  # out/register_check.csv.
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  cat('Register check: build centre children vs licensing registers\n')
  cat('(children 0-4, point in time; gap = build / register - 1)\n\n')
  out <- dplyr::bind_rows(lapply(names(REGISTERS), compare_geo))

  k   <- function(x) format(round(x / 1000, 1), nsmall = 1, width = 7)
  pct <- function(x) ifelse(is.na(x), '     --', sprintf('%+6.0f%%', 100 * x))
  cat(sprintf('%-4s %8s %8s %8s %7s | %7s %7s %7s\n',
              'geo', 'reg cap', 'reg kids', 'build', 'gap', 'reg hom', 'bld hom', 'gap'))
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    cat(sprintf('%-4s %8s %8s %8s %7s | %7s %7s %7s\n',
                r$geo, k(r$register_centre_u5_capacity), k(r$register_centre_children),
                k(r$build_centre), pct(r$gap_centre),
                if (is.na(r$register_home_children)) '     --' else k(r$register_home_children),
                k(r$build_paid_home), pct(r$gap_home)))
  }
  cat('\nbuild = Center-Based + Head Start + EHS; homes = all Paid Home-Based\n')
  cat('\n(thousands of children)\n')

  f <- file.path(STATE_DATA_ROOT, 'out', 'register_check.csv')
  readr::write_csv(out, f)
  cat('wrote', f, '\n')
  invisible(out)
}


if (sys.nframe() == 0) main_register_check()
