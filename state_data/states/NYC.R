#------------------------------------------------------------------------------
# NYC.R -- New York City, the five boroughs.
#
# Not a state. A sub-state geography that gets its own build because it is
# asked about on its own. Region northeast.
#
# THE CITY TAKES THE STATE'S NON-PARENTAL RATE. NSCH publishes no sub-state
# geography, so there is no city rate to measure. load_spec() (00_main.R)
# reads NY's row of out/nsch_rate_base.csv for geo == 'NYC' -- not hand-copied
# here. What distinguishes the city is its population and its program counts.
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
    'Early Head Start',    NYC_EARLY_HEAD_START)
)
