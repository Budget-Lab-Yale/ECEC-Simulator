#------------------------------------------------------------------------------
# CA.R -- California
#
# Region west. The build reads three things here: the programme counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

# Public pre-K. CCD 2018-19 has NO California rows -- CA reported no pre-K
# membership to CCD that year. NIEER State of Preschool 2019, CA overview:
# CA State Preschool Program 140,920 + Transitional Kindergarten 104,694.
# ONLY geography not on CCD. Excludes preschool special ed (52,307), which
# CCD counts elsewhere -- so CA reads low against CCD states. CSPP incl.
# contracted non-school centres.
CA_PREK             <- 245614   # NIEER 2018-19, CSPP + TK (no CCD rows)
CA_HEAD_START       <- 70631    # NIEER State of Preschool 2019, ages 3-4
CA_EARLY_HEAD_START <- 24052    # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start counts: NIEER state figures. Whether they include the AIAN
# (tribal) programmes in this state is not checked.



SPEC_CA <- list(
  geo    = 'CA',
  name   = 'California',
  fips   = 6,
  region = 'west',

  programmes = tibble::tribble(
    ~programme,          ~n_children,
    'public pre-K',        CA_PREK,
    'Head Start',          CA_HEAD_START,
    'Early Head Start',    CA_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 274,126; w/ Head Start, unduplicated,
  # 344,758. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 245,614 (profile p.59) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 274126, with_hs = 344758),
  nieer_state_prek = 245614,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 41,519 open Head Start slots
  # (0.0%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2019 k6q20, children 0-4, child-weighted. n = 137 records.
  # Single wave; 01_nsch_rate.R prints the pools.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.4243,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM, PSU HHID,
  # weight FWC, state read as a domain. Carried through to an interval on
  # every output row; nothing else reads it.
  nonparental_rate_se = 0.0533
)
