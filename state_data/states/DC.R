#------------------------------------------------------------------------------
# DC.R -- District of Columbia
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_DC <- geo_spec('DC', 'District of Columbia', fips = 11, region = 'south',
  prek             = 12815,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 1032 + 1118.
  head_start       = 2150,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1577,    # NIEER State(s) of Head Start (2023), 2018-19 bar

  # HS children also in DC pre-K. Site file can't measure DC's own-school
  # share: 114 open HS slots vs 2,150 (DC Public Schools grant 'Not Reported'
  # in 2026 export) -- under SITE_FILE_MIN_COVER, so NIEER's figure stands in.
  # NIEER State of Preschool 2019, Table 4 (p.28): DC pre-K + special ed same
  # w/ and w/out HS (3s 6,360, 4s 7,237) -- all 2,150 counted in pre-K.
  hs_in_prek       = 2150)
