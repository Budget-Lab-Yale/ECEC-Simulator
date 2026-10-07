#------------------------------------------------------------------------------
# FL.R -- Florida
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

# VPK school-year program 2018-19, final (Nov 2019): 165,046 children. NIEER
# Table 2's 173,633 also counts 300-hour summer VPK (~8,600) -- not regular
# care through year, so dropped. FL EDR, VPK Estimating Conference (13 Jan
# 2020), p. 1; PDF in raw/FL/.
FL_VPK_SCHOOL_YEAR <- 165046

SPEC_FL <- geo_spec('FL', 'Florida', fips = 12,
  prek_all_settings = FL_VPK_SCHOOL_YEAR,
  region = 'south',
  prek             = 64164,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 13847 + 17212 + 1046 + 445.
  head_start       = 32550,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 9573)    # NIEER State(s) of Head Start (2023), 2018-19 bar
