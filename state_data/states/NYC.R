#------------------------------------------------------------------------------
# NYC.R -- New York City, the five boroughs. Sub-state geography, own build
# because asked about on its own.
#
# CITY'S RATE = NY'S x p0 FACTOR. NSCH publishes no sub-state geography;
# nsch_rate_of() (00_build_demand_targets.R) scales NY's
# out/nsch_participation_rate_2019.csv row (parent_geo(),
# src/shared_functions/constants.R) by p0's non-parental share in NYC / in NY. City also differs by population, program counts, own p0
# mix; wage ratio from ACS childcare workers in five boroughs
# (acs_childcare_wage_by_education_2019.py).
#
# NY BUILD INCLUDES THIS ONE. NY minus NYC = rest of state by definition for
# step 1, program counts -- but p0 mix a ratio per geography, so difference
# only approximates a rest-of-state build: within 0.6% of hours per care type.
#------------------------------------------------------------------------------

# NYC_* constants, borough lists: NY.R.
source(file.path(STATE_DATA_ROOT, 'states', 'NY.R'))

SPEC_NYC <- geo_spec('NYC', 'New York City', fips = 36,
  counties = NYC_BOROUGHS, county_fips = NYC_COUNTY_FIPS,
  region = 'northeast',
  prek             = NYC_PREK_CCD,
  head_start       = NYC_HEAD_START,
  early_head_start = NYC_EARLY_HEAD_START)
