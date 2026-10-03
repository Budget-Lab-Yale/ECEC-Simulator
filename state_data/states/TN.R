#------------------------------------------------------------------------------
# TN.R -- Tennessee
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_TN <- geo_spec('TN', 'Tennessee', fips = 47, region = 'south',
  prek             = 32276,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 6632 + 7807 + 87 + 79.
  head_start       = 14605,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2277)    # NIEER State(s) of Head Start (2023), 2018-19 bar
