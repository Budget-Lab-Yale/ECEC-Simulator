#------------------------------------------------------------------------------
# WA.R -- Washington
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_WA <- geo_spec('WA', 'Washington', fips = 53, region = 'west',
  prek             = 32092,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
  # slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.
  head_start       = 10857,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3832)    # NIEER State(s) of Head Start (2023), 2018-19 bar
