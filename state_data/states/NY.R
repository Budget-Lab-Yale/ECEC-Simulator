#------------------------------------------------------------------------------
# NY.R -- New York State, all 62 counties, city included.
#
# Region northeast. Build reads here: program counts, NSCH rate w/ standard
# error, region, share of Head Start a district runs in own pre-K school.
# Every other constant national or regional -- see 00_main.R.
#
# NYC.R builds the five boroughs on their own, from the constants here. The two
# outputs OVERLAP by design: each is a deliverable in its own right.
#------------------------------------------------------------------------------


# The five boroughs -- county names as Census PEP writes them, FIPS in same
# order. Only definition; NYC.R, 03_head_start_in_schools.R, checks read these.
NYC_BOROUGHS    <- c('Bronx', 'Kings', 'New York', 'Queens', 'Richmond')
NYC_COUNTY_FIPS <- c('36005', '36047', '36061', '36081', '36085')


# Public-school pre-K, CCD 2018-19: 54,451 children across all 62 counties,
# counted school by school.
NY_PREK_CCD  <- 54451
NYC_PREK_CCD <- 26717   # CCD, five boroughs

# Head Start ages 3-4, NIEER 2018-19. GROSS of pre-K overlap.
NY_HEAD_START  <- 36863

NY_EARLY_HEAD_START  <- 11433

# NYC SHARES. City's OPEN funded slots over New York's, ACF Head Start
# service-location file (2026-09-23 export -- same file 03_ reads). Head Start
# types 1, 3, 5; Early Head Start types 2, 4, 6. Applied to NIEER state counts;
# assumes city's 2026 share held in 2018-19.
# tests/check_program_counts_match_sources.R recomputes both from the file.
NYC_HS_SLOTS  <- 7064;  NY_HS_SLOTS  <- 20518
NYC_EHS_SLOTS <- 4426;  NY_EHS_SLOTS <- 8590
NYC_HEAD_START       <- round(NY_HEAD_START * NYC_HS_SLOTS / NY_HS_SLOTS)              # 12,691
NYC_EARLY_HEAD_START <- round(NY_EARLY_HEAD_START * NYC_EHS_SLOTS / NY_EHS_SLOTS)  # 5,891




SPEC_NY <- list(
  geo    = 'NY',
  name   = 'New York State',
  fips   = 36,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NY_PREK_CCD,
    'Head Start',          NY_HEAD_START,
    'Early Head Start',    NY_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 42 of 20,518 open Head Start slots
  # (0.2%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.0020469831367579687,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 234 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.6117,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0433
)
