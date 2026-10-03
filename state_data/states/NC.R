#------------------------------------------------------------------------------
# NC.R -- North Carolina
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_NC <- geo_spec('NC', 'North Carolina', fips = 37, region = 'south',
  prek             = 19052,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 6985 + 9024 + 76 + 68 + 171 + 152.
  head_start       = 16476,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 5155)    # NIEER State(s) of Head Start (2023), 2018-19 bar
