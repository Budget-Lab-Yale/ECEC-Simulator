#------------------------------------------------------------------------------
# NYC.R -- New York City, the five boroughs.
#
# Not a state. A sub-state geography that gets its own build because it is
# asked about on its own. Region northeast.
#
# THE CITY TAKES THE STATE'S NON-PARENTAL RATE. NSCH publishes no sub-state
# geography, so there is no city rate to measure. load_spec() (00_main.R)
# reads the parent state's row of out/nsch_rate_base.csv -- parent_geo() in
# src/shared_functions/constants.R, NYC -> NY -- not hand-copied here. What distinguishes the city is its population and its program counts.
#
# THE NY BUILD INCLUDES THIS ONE. Same rate, so step 1 and program counts
# subtract exactly -- but p0 mix a ratio per geography, so NY rows minus NYC
# rows only approximate a rest-of-state build: within 0.6% of hours per care
# type (memo check registers the figure).
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
