#------------------------------------------------------------------------------
# VA.R -- Virginia
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#------------------------------------------------------------------------------

SPEC_VA <- geo_spec('VA', 'Virginia', fips = 51, region = 'south',
  prek             = 33790,   # CCD 2018-19, public-school pre-K
  head_start       = 11579,   # NIEER 2018-19, ages 3-4
  early_head_start = 2507)    # published; cross-checks against ACF FY2019
