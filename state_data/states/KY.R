#------------------------------------------------------------------------------
# KY.R -- Kentucky
#
# Region south. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
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
    'Early Head Start',    KY_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 599 of 8,846 open Head Start slots
  # (6.8%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.06771422111688899,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 287 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5834,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0372
)
