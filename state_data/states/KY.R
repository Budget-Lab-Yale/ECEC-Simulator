#------------------------------------------------------------------------------
# KY.R -- Kentucky
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
