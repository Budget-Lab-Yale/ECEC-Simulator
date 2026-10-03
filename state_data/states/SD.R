#------------------------------------------------------------------------------
# SD.R -- South Dakota
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_SD <- geo_spec('SD', 'South Dakota', fips = 46, region = 'midwest',
  prek             = 3423,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 892 + 1295 + 750 + 677.
  head_start       = 3614,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 965)    # NIEER State(s) of Head Start (2023), 2018-19 bar
