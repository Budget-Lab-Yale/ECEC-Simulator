#------------------------------------------------------------------------------
# CA.R -- California
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_CA <- geo_spec('CA', 'California', fips = 6, region = 'west',

  # Public pre-K. CCD 2018-19 has NO California rows -- CA reported no pre-K
  # membership to CCD that year. NIEER State of Preschool 2019, CA overview:
  # CA State Preschool Program 140,920 + Transitional Kindergarten 104,694.
  # CA, MI only geographies w/out a CCD count. Excludes preschool special ed (52,307), which
  # CCD counts elsewhere -- so CA reads low against CCD states. CSPP incl.
  # contracted non-school centers.
  #
  # TK LEFT OUT. TK = first year of two-year kindergarten for children born
  # Sep 2 - Dec 2; district may admit later birthdays since 2015 (NIEER CA
  # background, printed p.58) -- nearly every TK child 5 by mid-year, outside
  # population 0-4.
  # program_children() still takes West 5+ share off CSPP.

  prek             = 140920,   # NIEER 2018-19, CSPP only (no CCD rows)

  # Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
  # slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.
  head_start       = 70631,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 24052)    # NIEER State(s) of Head Start (2023), 2018-19 bar
