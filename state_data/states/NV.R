#------------------------------------------------------------------------------
# NV.R -- Nevada
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_NV <- geo_spec('NV', 'Nevada', fips = 32, region = 'west',
  prek             = 11598,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 1097 + 987 + 164 + 198.
  head_start       = 2446,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 937)     # NIEER State(s) of Head Start (2023), 2018-19 bar
