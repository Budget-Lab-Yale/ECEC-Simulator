#------------------------------------------------------------------------------
# NJ.R -- New Jersey
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#------------------------------------------------------------------------------

SPEC_NJ <- geo_spec('NJ', 'New Jersey', fips = 34, region = 'northeast',
  prek             = 41305,   # CCD 2018-19, public-school pre-K
  head_start       = 11940,   # NIEER 2018-19, ages 3-4
  early_head_start = 3578)    # NIEER State(s) of Head Start, 2018-19
