#------------------------------------------------------------------------------
# UT.R -- Utah
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_UT <- geo_spec('UT', 'Utah', fips = 49, region = 'west',
  prek             = 16447,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 1921 + 2455 + 115 + 127 + 149 + 142.
  head_start       = 4909,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1248)    # NIEER State(s) of Head Start (2023), 2018-19 bar
