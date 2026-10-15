#------------------------------------------------------------------------------
# states.R -- hand-researched program counts, one SPEC_<GEO> per geography
# (geo_spec(), 00_build_demand_targets.R). Data, not logic. Sourced once by
# get_spec(). NY before NYC -- NYC reuses the NY constants.
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# AK -- Alaska
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_AK <- geo_spec('AK', 'Alaska', fips = 2, region = 'west',
  prek             = 3544,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 471 + 565 + 670 + 703.
  head_start       = 2409,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 726)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# AL -- Alabama
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_AL <- geo_spec('AL', 'Alabama', fips = 1, region = 'south',
  prek             = 17464,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 5555 + 6048 + 93 + 89.
  head_start       = 11785,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2682)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# AR -- Arkansas
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_AR <- geo_spec('AR', 'Arkansas', fips = 5, region = 'south',
  prek             = 16761,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 3544 + 2769 + 162 + 86.
  head_start       = 6561,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3284)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# AZ -- Arizona
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_AZ <- geo_spec('AZ', 'Arizona', fips = 4, region = 'west',
  prek             = 19371,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
  # slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.
  head_start       = 13891,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3339)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# CA -- California
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


#------------------------------------------------------------------------------
# CO -- Colorado
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_CO <- geo_spec('CO', 'Colorado', fips = 8, region = 'west',
  prek             = 33716,   # CCD 2018-19, public-school pre-K
  head_start       = 8152,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2015)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# CT -- Connecticut
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_CT <- geo_spec('CT', 'Connecticut', fips = 9, region = 'northeast',
  prek             = 14304,   # CCD 2018-19, public-school pre-K
  head_start       = 4206,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1292)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# DC -- District of Columbia
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
  early_head_start = 1577)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# DE -- Delaware
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_DE <- geo_spec('DE', 'Delaware', fips = 10, region = 'south',
  prek             = 2081,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 635 + 849.
  head_start       = 1484,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 483)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# FL -- Florida
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_FL <- geo_spec('FL', 'Florida', fips = 12,
  region = 'south',
  prek             = 64164,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 13847 + 17212 + 1046 + 445.
  head_start       = 32550,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 9573)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# GA -- Georgia
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_GA <- geo_spec('GA', 'Georgia', fips = 13, region = 'south',
  prek             = 49315,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 11312 + 8275 + 172 + 83.
  head_start       = 19842,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 4501)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# HI -- Hawaii
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_HI <- geo_spec('HI', 'Hawaii', fips = 15, region = 'west',
  prek             = 1580,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 993 + 1291.
  head_start       = 2284,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 657)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# IA -- Iowa
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_IA <- geo_spec('IA', 'Iowa', fips = 19, region = 'midwest',
  prek             = 23060,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 2577 + 2994.
  head_start       = 5571,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1702)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# ID -- Idaho
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_ID <- geo_spec('ID', 'Idaho', fips = 16, region = 'west',
  prek             = 3488,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 780 + 1514 + 83 + 121 + 328 + 245.
  head_start       = 3071,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 765)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# IL -- Illinois
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_IL <- geo_spec('IL', 'Illinois', fips = 17, region = 'midwest',
  prek             = 81044,   # CCD 2018-19, public-school pre-K
  head_start       = 26175,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 8889)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# IN -- Indiana
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_IN <- geo_spec('IN', 'Indiana', fips = 18, region = 'midwest',
  prek             = 19971,   # CCD 2018-19, public-school pre-K
  head_start       = 11493,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2480)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# KS -- Kansas
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_KS <- geo_spec('KS', 'Kansas', fips = 20, region = 'midwest',
  prek             = 19508,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 2138 + 2939 + 26 + 36.
  head_start       = 5139,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2030)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# KY -- Kentucky
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#------------------------------------------------------------------------------

SPEC_KY <- geo_spec('KY', 'Kentucky', fips = 21, region = 'south',

  # Public-school pre-K, CCD 2018-19, school-level membership joined to county:
  # 28,465 children. NIEER reports the Kentucky Preschool Program at 21,351 for
  # the same year -- CCD is 33% higher because its `Pre-Kindergarten` grade spans
  # children KPP does not fund, incl. district-funded places and preschool
  # special education. CCD used because it is a direct count.
  prek             = 28465,

  # Head Start ages 3-4. NIEER State of Preschool 2019, KY profile (2018-19).
  # GROSS of state pre-K overlap -- NIEER nets dual enrolment only out of its pie
  # charts, not this row. State-funded Head Start in Kentucky: 0.
  head_start       = 12307,

  # Early Head Start, birth to three. NIEER, The State(s) of Head Start and Early
  # Head Start (2023), KY profile p.126, 2018-19 bar.
  #
  # Cross-checks EXACTLY against ACF Head Start Program Facts FY2019: 12,163 +
  # 3,004 = 15,167, the published state total. Kentucky has no AIAN component.
  early_head_start = 3004)


#------------------------------------------------------------------------------
# LA -- Louisiana
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_LA <- geo_spec('LA', 'Louisiana', fips = 22, region = 'south',
  prek             = 27412,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 10642 + 7065.
  head_start       = 17707,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3112)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# MA -- Massachusetts
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_MA <- geo_spec('MA', 'Massachusetts', fips = 25, region = 'northeast',
  prek             = 30268,   # CCD 2018-19, public-school pre-K
  head_start       = 9580,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2115)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# MD -- Maryland
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_MD <- geo_spec('MD', 'Maryland', fips = 24, region = 'south',
  prek             = 30950,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 4124 + 3504.
  head_start       = 7628,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1855)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# ME -- Maine
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


#------------------------------------------------------------------------------
# MI -- Michigan
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
  early_head_start = 7178)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# MN -- Minnesota
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_MN <- geo_spec('MN', 'Minnesota', fips = 27, region = 'midwest',
  prek             = 24458,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
  # slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.
  head_start       = 9348,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2446)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# MO -- Missouri
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_MO <- geo_spec('MO', 'Missouri', fips = 29, region = 'midwest',
  prek             = 32426,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 5863 + 5033.
  head_start       = 10896,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3767)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# MS -- Mississippi
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_MS <- geo_spec('MS', 'Mississippi', fips = 28, region = 'south',
  prek             = 6404,    # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 9398 + 10055 + 141 + 67.
  head_start       = 19661,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3147)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# MT -- Montana
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_MT <- geo_spec('MT', 'Montana', fips = 30, region = 'west',
  prek             = 979,    # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 1070 + 1344 + 678 + 771.
  head_start       = 3863,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 978)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# NC -- North Carolina
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_NC <- geo_spec('NC', 'North Carolina', fips = 37, region = 'south',
  prek             = 19052,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 6985 + 9024 + 76 + 68 + 171 + 152.
  head_start       = 16476,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 5155)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# ND -- North Dakota
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_ND <- geo_spec('ND', 'North Dakota', fips = 38, region = 'midwest',
  prek             = 2804,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 664 + 856 + 341 + 441.
  head_start       = 2302,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 595)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# NE -- Nebraska
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_NE <- geo_spec('NE', 'Nebraska', fips = 31, region = 'midwest',
  prek             = 18483,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 1475 + 1788 + 74 + 145 + 19 + 18.
  head_start       = 3519,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1779)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# NH -- New Hampshire
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_NH <- geo_spec('NH', 'New Hampshire', fips = 33, region = 'northeast',
  prek             = 4221,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 572 + 606.
  head_start       = 1178,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 385)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# NJ -- New Jersey
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#------------------------------------------------------------------------------

SPEC_NJ <- geo_spec('NJ', 'New Jersey', fips = 34, region = 'northeast',
  prek             = 41305,   # CCD 2018-19, public-school pre-K
  head_start       = 11940,   # NIEER 2018-19, ages 3-4
  early_head_start = 3578)    # NIEER State(s) of Head Start, 2018-19


#------------------------------------------------------------------------------
# NM -- New Mexico
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


#------------------------------------------------------------------------------
# NV -- Nevada
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


#------------------------------------------------------------------------------
# NY -- New York State, all 62 counties, city included.
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# NYC.R builds the five boroughs on their own, from the constants here. The two
# outputs OVERLAP by design: each is a deliverable in its own right.
#------------------------------------------------------------------------------

# The five boroughs -- county names as Census PEP writes them; 5-digit FIPS in
# same order from SUBSTATE_GEOGRAPHIES (constants.R), the one definition of
# NYC. NYC.R, checks read these.
NYC_BOROUGHS    <- c('Bronx', 'Kings', 'New York', 'Queens', 'Richmond')
NYC_COUNTY_FIPS <- with(SUBSTATE_GEOGRAPHIES$NYC,
                        sprintf('%02d%03d', statefip, countyfip))


# Public-school pre-K, CCD 2018-19: 54,451 children across all 62 counties,
# counted school by school.
NY_PREK_CCD  <- 54451
NYC_PREK_CCD <- 26717   # CCD, five boroughs

# NYC pre-K, every setting (district schools, Pre-K Centers, NYC Early
# Education Centers, charters), registered October 2018: full-day UPK 66,854
# (30,313 in DOE Pre-K Centers / district schools) + half-day 2,044 + 3-K
# 5,091. NYC Council Finance Division, DOE Fiscal 2020 Preliminary Budget
# report (March 2019), pp. 30-31, Tables 21-22; PDF in raw/NYC/. Plays role
# NIEER Table 4 plays for a state (NIEER publishes states only).
NYC_PREK_ALL_SETTINGS <- 66854 + 2044 + 5091   # 73,989

# Head Start ages 3-4, NIEER 2018-19. GROSS of pre-K overlap.
NY_HEAD_START  <- 36863

NY_EARLY_HEAD_START  <- 11433

# NYC SHARES. City's OPEN funded slots over New York's, ACF Head Start
# service-location file (2026-09-23 export -- check_program_counts_match_sources.R
# reads it too). Head Start
# types 1, 3, 5; Early Head Start types 2, 4, 6. Applied to NIEER state counts;
# assumes city's 2026 share held in 2018-19.
# tests/check_program_counts_match_sources.R recomputes both from the file.
NYC_HS_SLOTS  <- 7064;  NY_HS_SLOTS  <- 20518
NYC_EHS_SLOTS <- 4426;  NY_EHS_SLOTS <- 8590
NYC_HEAD_START       <- round(NY_HEAD_START * NYC_HS_SLOTS / NY_HS_SLOTS)              # 12,691
NYC_EARLY_HEAD_START <- round(NY_EARLY_HEAD_START * NYC_EHS_SLOTS / NY_EHS_SLOTS)  # 5,891

SPEC_NY <- geo_spec('NY', 'New York State', fips = 36, region = 'northeast',
  prek             = NY_PREK_CCD,
  head_start       = NY_HEAD_START,
  early_head_start = NY_EARLY_HEAD_START)


#------------------------------------------------------------------------------
# NYC -- New York City, the five boroughs. Sub-state geography, own build
# because asked about on its own.
#
# CITY'S RATE = NY'S. NSCH publishes no sub-state geography; nsch_rate_of()
# (00_build_demand_targets.R) takes NY's out/nsch_participation_rate_base.csv
# row (parent_geo(), src/shared_functions/constants.R) as is. City differs by
# population, program counts, own p0
# mix; wage ratio from OES New York MSA 35620 childcare occupations
# (resources/supply/oes_childcare_wage_ratios_2019.R).
#
# NY BUILD INCLUDES THIS ONE. NY minus NYC = rest of state by definition for
# population, program counts only -- city's rate, p0 mix both ratios per
# geography, so difference only approximates a rest-of-state built same way:
# within 0.8% of hours per care type, Unpaid Home-Based 2.6% (2,864 NY children,
# 0.01% of all hours) (tests/check_nyc_rest_of_state.R).
#------------------------------------------------------------------------------


SPEC_NYC <- geo_spec('NYC', 'New York City', fips = 36,
  counties = NYC_BOROUGHS, county_fips = NYC_COUNTY_FIPS,
  prek_all_settings = NYC_PREK_ALL_SETTINGS,
  region = 'northeast',
  prek             = NYC_PREK_CCD,
  head_start       = NYC_HEAD_START,
  early_head_start = NYC_EARLY_HEAD_START)


#------------------------------------------------------------------------------
# OH -- Ohio
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_OH <- geo_spec('OH', 'Ohio', fips = 39, region = 'midwest',
  prek             = 39438,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 13287 + 14664.
  head_start       = 27951,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 6053)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# OK -- Oklahoma
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_OK <- geo_spec('OK', 'Oklahoma', fips = 40, region = 'south',
  prek             = 41901,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 6988 + 4282 + 1284 + 1153.
  head_start       = 13707,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3677)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# OR -- Oregon
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_OR <- geo_spec('OR', 'Oregon', fips = 41, region = 'west',
  prek             = 216,    # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 1810 + 2536 + 159 + 159 + 1368 + 905.
  head_start       = 6937,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2241)   # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# PA -- Pennsylvania
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_PA <- geo_spec('PA', 'Pennsylvania', fips = 42, region = 'northeast',
  prek             = 8655,    # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 10523 + 13060 + 121 + 71.
  head_start       = 23775,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 6615)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# RI -- Rhode Island
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_RI <- geo_spec('RI', 'Rhode Island', fips = 44, region = 'northeast',
  prek             = 2897,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 891 + 986.
  head_start       = 1877,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 675)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# SC -- South Carolina
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_SC <- geo_spec('SC', 'South Carolina', fips = 45, region = 'south',
  prek             = 27435,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 5926 + 3616 + 43 + 37 + 51 + 37.
  head_start       = 9710,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2372)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# SD -- South Dakota
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


#------------------------------------------------------------------------------
# TN -- Tennessee
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_TN <- geo_spec('TN', 'Tennessee', fips = 47, region = 'south',
  prek             = 32276,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 6632 + 7807 + 87 + 79.
  head_start       = 14605,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2277)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# TX -- Texas
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_TX <- geo_spec('TX', 'Texas', fips = 48, region = 'south',
  prek             = 264460,   # CCD 2018-19, public-school pre-K
  head_start       = 61935,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 12027)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# UT -- Utah
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


#------------------------------------------------------------------------------
# VA -- Virginia
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#------------------------------------------------------------------------------

SPEC_VA <- geo_spec('VA', 'Virginia', fips = 51, region = 'south',
  prek             = 33790,   # CCD 2018-19, public-school pre-K
  head_start       = 11579,   # NIEER 2018-19, ages 3-4
  early_head_start = 2507)    # published; cross-checks against ACF FY2019


#------------------------------------------------------------------------------
# VT -- Vermont
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_VT <- geo_spec('VT', 'Vermont', fips = 50, region = 'northeast',
  prek             = 8594,   # CCD 2018-19, public-school pre-K
  head_start       = 965,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 482)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# WA -- Washington
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


#------------------------------------------------------------------------------
# WI -- Wisconsin
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_WI <- geo_spec('WI', 'Wisconsin', fips = 55, region = 'midwest',
  prek             = 56096,   # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
  # slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.
  head_start       = 11015,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 3289)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# WV -- West Virginia
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

SPEC_WV <- geo_spec('WV', 'West Virginia', fips = 54, region = 'south',
  prek             = 15479,   # CCD 2018-19, public-school pre-K
  head_start       = 6781,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 1068)    # NIEER State(s) of Head Start (2023), 2018-19 bar


#------------------------------------------------------------------------------
# WY -- Wyoming
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SPEC_WY <- geo_spec('WY', 'Wyoming', fips = 56, region = 'west',
  prek             = 705,    # CCD 2018-19, public-school pre-K

  # Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
  # AIAN, Migrant & Seasonal where present: 512 + 701 + 110 + 89.
  head_start       = 1412,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 473)    # NIEER State(s) of Head Start (2023), 2018-19 bar
