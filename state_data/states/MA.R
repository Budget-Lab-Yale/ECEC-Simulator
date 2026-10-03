#------------------------------------------------------------------------------
# MA.R -- Massachusetts
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_MA <- geo_spec('MA', 'Massachusetts', fips = 25, region = 'northeast',
  prek             = 30268,   # CCD 2018-19, public-school pre-K
  head_start       = 9580,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2115)    # NIEER State(s) of Head Start (2023), 2018-19 bar
