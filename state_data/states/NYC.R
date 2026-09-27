#------------------------------------------------------------------------------
# NYC.R -- New York City, the five boroughs.
#
# Not a state. A sub-state geography that gets its own build because it is
# asked about on its own. Region northeast.
#
# THE CITY TAKES THE STATE'S NON-PARENTAL RATE. NSCH publishes no sub-state
# geography, so there is no city rate to measure. What distinguishes the city
# is its population and its program counts.
#
# THE NY BUILD INCLUDES THIS ONE. Same rate, same Northeast constants, so the
# build is linear in population and program counts: NY rows minus NYC rows
# give the rest of the state.
#------------------------------------------------------------------------------

source(file.path(STATE_DATA_ROOT, 'states', 'NY.R'))




SPEC_NYC <- list(
  geo      = 'NYC',
  name     = 'New York City',
  fips     = 36,
  counties = NYC_BOROUGHS,
  county_fips = NYC_COUNTY_FIPS,
  region   = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NYC_PREK_CCD,
    'Head Start',          NYC_HEAD_START,
    'Early Head Start',    NYC_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 7,064 open Head Start slots
  # (0.0%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # New York's rate, NSCH 2018+2019 pooled. See header. Derived by 01_nsch_rate.R.
  nonparental_rate = 0.6117,

  # New York's standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. NSCH holds no city
  # identifier, so the city borrows the state's error as it borrows the rate.
  nonparental_rate_se = 0.0433
)
