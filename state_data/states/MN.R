#------------------------------------------------------------------------------
# MN.R -- Minnesota
#
# Region midwest. The build reads three things here: the programme counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

MN_PREK             <- 24458    # CCD 2018-19, public-school pre-K
MN_HEAD_START       <- 9348     # NIEER State of Preschool 2019, ages 3-4
MN_EARLY_HEAD_START <- 2446     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start counts: NIEER state figures. Whether they include the AIAN
# (tribal) programmes in this state is not checked.



SPEC_MN <- list(
  geo    = 'MN',
  name   = 'Minnesota',
  fips   = 27,
  region = 'midwest',

  programmes = tibble::tribble(
    ~programme,          ~n_children,
    'public pre-K',        MN_PREK,
    'Head Start',          MN_HEAD_START,
    'Early Head Start',    MN_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 16,656; w/ Head Start, unduplicated,
  # 26,003. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 8,237 (profile p.111) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 16656, with_hs = 26003),
  nieer_state_prek = 8237,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 6,452 open Head Start slots
  # (0.0%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2019 k6q20, children 0-4, child-weighted. n = 143 records.
  # Single wave; 01_nsch_rate.R prints the pools.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5606,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM, PSU HHID,
  # weight FWC, state read as a domain. Carried through to an interval on
  # every output row; nothing else reads it.
  nonparental_rate_se = 0.0524
)
