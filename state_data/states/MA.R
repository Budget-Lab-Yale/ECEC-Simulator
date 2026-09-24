#------------------------------------------------------------------------------
# MA.R -- Massachusetts
#
# Region northeast. The build reads three things here: the program counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

MA_PREK             <- 30268    # CCD 2018-19, public-school pre-K
MA_HEAD_START       <- 9580     # NIEER State of Preschool 2019, ages 3-4
MA_EARLY_HEAD_START <- 2115     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_MA <- list(
  geo    = 'MA',
  name   = 'Massachusetts',
  fips   = 25,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MA_PREK,
    'Head Start',          MA_HEAD_START,
    'Early Head Start',    MA_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 33,557; w/ Head Start, unduplicated,
  # 40,303. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 37,246 (profile p.105) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 33557, with_hs = 40303),
  nieer_state_prek = 37246,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 6,201 open Head Start slots
  # (0.0%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 261 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.6560,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0386
)
