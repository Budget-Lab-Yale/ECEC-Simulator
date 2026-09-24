#------------------------------------------------------------------------------
# CT.R -- Connecticut
#
# Region northeast. The build reads three things here: the program counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

CT_PREK             <- 14304    # CCD 2018-19, public-school pre-K
CT_HEAD_START       <- 4206     # NIEER State of Preschool 2019, ages 3-4
CT_EARLY_HEAD_START <- 1292     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_CT <- list(
  geo    = 'CT',
  name   = 'Connecticut',
  fips   = 9,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        CT_PREK,
    'Head Start',          CT_HEAD_START,
    'Early Head Start',    CT_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 18,885; w/ Head Start, unduplicated,
  # 22,248. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 15,008 (profile p.65) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 18885, with_hs = 22248),
  nieer_state_prek = 15008,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 102 of 3,402 open Head Start slots
  # (3.0%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.029982363315696647,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 222 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5523,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0448
)
