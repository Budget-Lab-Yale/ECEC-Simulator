#------------------------------------------------------------------------------
# IN.R -- Indiana
#
# Region midwest. The build reads three things here: the programme counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

IN_PREK             <- 19971    # CCD 2018-19, public-school pre-K
IN_HEAD_START       <- 11493    # NIEER State of Preschool 2019, ages 3-4
IN_EARLY_HEAD_START <- 2480     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_IN <- list(
  geo    = 'IN',
  name   = 'Indiana',
  fips   = 18,
  region = 'midwest',

  programmes = tibble::tribble(
    ~programme,          ~n_children,
    'public pre-K',        IN_PREK,
    'Head Start',          IN_HEAD_START,
    'Early Head Start',    IN_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 9,310; w/ Head Start, unduplicated,
  # 20,803. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 0 (profile p.84) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 9310, with_hs = 20803),
  nieer_state_prek = 0,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 151 of 7,325 open Head Start slots
  # (2.1%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.020614334470989763,

  # NSCH 2019 k6q20, children 0-4, child-weighted. n = 126 records.
  # Single wave; 01_nsch_rate.R prints the pools.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5003,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM, PSU HHID,
  # weight FWC, state read as a domain. Carried through to an interval on
  # every output row; nothing else reads it.
  nonparental_rate_se = 0.0539
)
