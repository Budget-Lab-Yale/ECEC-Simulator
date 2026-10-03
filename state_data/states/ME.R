#------------------------------------------------------------------------------
# ME.R -- Maine
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_ME <- geo_spec('ME', 'Maine', fips = 23, region = 'northeast',
  prek             = 5857,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 949 + 1267 + 28 + 32.
  head_start       = 2276,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 804)    # NIEER State(s) of Head Start (2023), 2018-19 bar
