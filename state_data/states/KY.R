#------------------------------------------------------------------------------
# KY.R -- Kentucky
#
# Region south. The build reads three things here: the programme counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
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

  programmes = tibble::tribble(
    ~programme,          ~n_children,
    'public pre-K',        KY_PREK_CCD,
    'Head Start',          KY_HEAD_START,
    'Early Head Start',    KY_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 21,351; w/ Head Start, unduplicated,
  # 32,875. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 21,351 (profile p.94) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 21351, with_hs = 32875),
  nieer_state_prek = 21351,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 599 of 8,846 open Head Start slots
  # (6.8%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.06771422111688899,

  # NSCH 2019 k6q20, children 0-4, child-weighted. n = 147 records.
  # SINGLE WAVE, deliberately. Pooling 2018+2019+2020 would centre exactly on
  # 2019 and cut the error ~40%, but NSCH 2020 was fielded through the pandemic
  # and Kentucky's rate collapses to 45.8% against 57-59% either side. Pooling
  # 2018+2019 alone centres on mid-2018 while every other input is pinned to
  # 2019. 01_nsch_rate.R prints both pools beside the single wave.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5940,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM, PSU HHID,
  # weight FWC, state read as a domain. Census documents that design. Kish's
  # effective n reads 4.88 on the same 147 records -- it prices the unequal
  # weighting, never the stratification, so it is the wrong estimator and not
  # a conservative one. Carried through to an interval on every output row;
  # nothing else in the build reads it.
  nonparental_rate_se = 0.0500
)
