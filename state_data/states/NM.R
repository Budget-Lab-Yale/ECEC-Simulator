#------------------------------------------------------------------------------
# NM.R -- New Mexico
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_NM <- geo_spec('NM', 'New Mexico', fips = 35, region = 'west',
  prek             = 10174,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
  # slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.
  head_start       = 7427,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2080)    # NIEER State(s) of Head Start (2023), 2018-19 bar
