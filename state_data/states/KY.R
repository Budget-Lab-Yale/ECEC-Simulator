#------------------------------------------------------------------------------
# KY.R -- Kentucky
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#------------------------------------------------------------------------------


# Public-school pre-K, CCD 2018-19, school-level membership joined to county:
# 28,465 children. NIEER reports the Kentucky Preschool Program at 21,351 for
# the same year -- CCD is 33% higher because its `Pre-Kindergarten` grade spans
# children KPP does not fund, incl. district-funded places and preschool
# special education. CCD used because it is a direct count.
KY_PREK_CCD <- 28465

# Head Start ages 3-4. NIEER State of Preschool 2019, KY profile (2018-19).
# GROSS of state pre-K overlap -- NIEER nets dual enrolment only out of its pie
# charts, not this row. State-funded Head Start in Kentucky: 0.
KY_HEAD_START <- 12307

# Early Head Start, birth to three. NIEER, The State(s) of Head Start and Early
# Head Start (2023), KY profile p.126, 2018-19 bar.
#
# Cross-checks EXACTLY against ACF Head Start Program Facts FY2019: 12,163 +
# 3,004 = 15,167, the published state total. Kentucky has no AIAN component.
KY_EARLY_HEAD_START <- 3004




SPEC_KY <- list(
  geo    = 'KY',
  name   = 'Kentucky',
  fips   = 21,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        KY_PREK_CCD,
    'Head Start',          KY_HEAD_START,
    'Early Head Start',    KY_EARLY_HEAD_START)
)
