#------------------------------------------------------------------------------
# MI.R -- Michigan
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_MI <- geo_spec('MI', 'Michigan', fips = 26, region = 'midwest',

  # Public pre-K. CCD 2018-19 carries NO Michigan pre-K count -- 1,064 of
  # 1,219 school rows flagged "Not reported", reported rows sum to 0. NIEER
  # State of Preschool 2019, MI profile: Great Start Readiness Program enrolled
  # 37,140 in 2018-19. As California: NIEER state pre-K where CCD has no count.
  # GSRP incl. community-based sites, so MI reads high against CCD states.
  prek             = 37140,   # NIEER 2018-19, GSRP (no CCD count)

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 11431 + 10924 + 225 + 132 + 416 + 304.
  head_start       = 23432,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 7178,    # NIEER State(s) of Head Start (2023), 2018-19 bar

  # HS children also in GSRP. prek above = GSRP in every setting, so each HS
  # child NIEER reports in GSRP counted twice. NIEER State of Preschool 2019,
  # Table 4 (p.28): HS adds 15,986 - 3,888 = 12,098 three-year-olds and
  # 43,962 - 38,468 = 5,494 four-year-olds = 17,592; Appendix B HS 23,432 less
  # 17,592 = 5,840.
  hs_in_prek       = 5840)
