#------------------------------------------------------------------------------
# VT.R -- Vermont
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_VT <- geo_spec('VT', 'Vermont', fips = 50, region = 'northeast',
  prek             = 8594,   # CCD 2018-19, public-school pre-K
  head_start       = 965,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 482)    # NIEER State(s) of Head Start (2023), 2018-19 bar
