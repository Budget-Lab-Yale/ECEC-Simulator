#------------------------------------------------------------------------------
# NYC.R -- New York City, the five boroughs. Sub-state geography, own build
# because asked about on its own.
#
# CITY'S RATE = NY'S. NSCH publishes no sub-state geography; nsch_rate_of()
# (00_build_demand_targets.R) takes NY's out/nsch_participation_rate_base.csv
# row (parent_geo(), src/shared_functions/constants.R) as is. City differs by
# population, program counts, own p0
# mix; wage ratio from OES New York MSA 35620 childcare occupations
# (resources/supply/oes_childcare_wage_ratios_2019.R).
#
# NY BUILD INCLUDES THIS ONE. NY minus NYC = rest of state by definition for
# population, program counts only -- city's rate, p0 mix both ratios per
# geography, so difference only approximates a rest-of-state built same way:
# within 0.8% of hours per care type, Unpaid Home-Based 2.6% (2,864 NY children,
# 0.01% of all hours) (tests/check_nyc_rest_of_state.R).
#------------------------------------------------------------------------------

# NYC_* constants, borough lists: NY.R.
source(file.path(STATE_DATA_ROOT, 'states', 'NY.R'))

SPEC_NYC <- geo_spec('NYC', 'New York City', fips = 36,
  counties = NYC_BOROUGHS, county_fips = NYC_COUNTY_FIPS,
  prek_all_settings = NYC_PREK_ALL_SETTINGS,
  region = 'northeast',
  prek             = NYC_PREK_CCD,
  head_start       = NYC_HEAD_START,
  early_head_start = NYC_EARLY_HEAD_START)
