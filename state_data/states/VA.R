#------------------------------------------------------------------------------
# VA.R -- Virginia
#
# Region south. The build reads three things here: the program counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#------------------------------------------------------------------------------


VA_PREK_CCD         <- 33790   # CCD 2018-19, public-school pre-K
VA_HEAD_START       <- 11579   # NIEER 2018-19, ages 3-4
VA_EARLY_HEAD_START <- 2507    # published; cross-checks against ACF FY2019




SPEC_VA <- list(
  geo    = 'VA',
  name   = 'Virginia',
  fips   = 51,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        VA_PREK_CCD,
    'Head Start',          VA_HEAD_START,
    'Early Head Start',    VA_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 25,247; w/ Head Start, unduplicated,
  # 36,826. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 17,657 (profile p.170) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 25247, with_hs = 36826),
  nieer_state_prek = 17657,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 1,567 of 7,283 open Head Start slots
  # (21.5%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.21515858849375258,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 232 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5699,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0406
)
