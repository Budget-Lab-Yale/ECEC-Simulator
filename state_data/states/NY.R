#------------------------------------------------------------------------------
# NY.R -- New York State, all 62 counties, city included.
#
# Region northeast. The build reads three things here: the programme counts,
# the NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# NYC.R builds the five boroughs on their own, from the constants here. The two
# outputs OVERLAP by design: each is a deliverable in its own right.
#------------------------------------------------------------------------------


NYC_BOROUGHS <- c('Bronx', 'Kings', 'New York', 'Queens', 'Richmond')


# Public-school pre-K, CCD 2018-19: 54,451 children across all 62 counties,
# counted school by school.
NY_PREK_CCD  <- 54451
NYC_PREK_CCD <- 26717   # CCD, five boroughs

# Head Start ages 3-4, NIEER 2018-19. GROSS of pre-K overlap.
NY_HEAD_START  <- 36863
# City share taken from the Head Start service-location file's own funded slots,
# 7,194 of 20,285 statewide.
NYC_HEAD_START <- round(NY_HEAD_START * 7194 / 20285)   # 13,073

NY_EARLY_HEAD_START  <- 11433
NYC_EARLY_HEAD_START <- 5938




SPEC_NY <- list(
  geo    = 'NY',
  name   = 'New York State',
  fips   = 36,
  region = 'northeast',

  programmes = tibble::tribble(
    ~programme,          ~n_children,
    'public pre-K',        NY_PREK_CCD,
    'Head Start',          NY_HEAD_START,
    'Early Head Start',    NY_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 151,805; w/ Head Start, unduplicated,
  # 177,532. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 126,302 (profile p.136) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 151805, with_hs = 177532),
  nieer_state_prek = 126302,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 42 of 20,518 open Head Start slots
  # (0.2%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.0020469831367579687,

  # NSCH 2019 k6q20, children 0-4, child-weighted. n = 112 records.
  # Single wave; 01_nsch_rate.R prints the pools. New York's 2020 rate holds up better than the
  # other three (62.1%) but the pool still fails on the other two states.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5766,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM, PSU HHID,
  # weight FWC, state read as a domain. Census documents that design. Kish's
  # effective n reads 5.885 on the same 112 records against 5.891 here -- the
  # two agree in New York, and nowhere else. Carried through to an interval on
  # every output row; nothing else in the build reads it.
  nonparental_rate_se = 0.0589
)
