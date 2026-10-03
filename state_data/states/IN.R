#------------------------------------------------------------------------------
# IN.R -- Indiana
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_IN <- geo_spec('IN', 'Indiana', fips = 18, region = 'midwest',
  prek             = 19971,   # CCD 2018-19, public-school pre-K
  head_start       = 11493,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2480)    # NIEER State(s) of Head Start (2023), 2018-19 bar
