#------------------------------------------------------------------------------
# WV.R -- West Virginia
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_WV <- geo_spec('WV', 'West Virginia', fips = 54, region = 'south',
  prek             = 15479,   # CCD 2018-19, public-school pre-K
  head_start       = 6781,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1068)    # NIEER State(s) of Head Start (2023), 2018-19 bar
