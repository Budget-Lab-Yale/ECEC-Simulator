#------------------------------------------------------------------------------
# WI.R -- Wisconsin
#
# Region midwest. The build reads three things here: the program counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

WI_PREK             <- 56096    # CCD 2018-19, public-school pre-K
WI_HEAD_START       <- 11015    # NIEER State of Preschool 2019, ages 3-4
WI_EARLY_HEAD_START <- 3289     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start counts: NIEER state figures. Whether they include the AIAN
# (tribal) programs in this state is not checked.



SPEC_WI <- list(
  geo    = 'WI',
  name   = 'Wisconsin',
  fips   = 55,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        WI_PREK,
    'Head Start',          WI_HEAD_START,
    'Early Head Start',    WI_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 51,793; w/ Head Start, unduplicated,
  # 62,807. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 51,618 (profile p.176) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 51793, with_hs = 62807),
  nieer_state_prek = 51618,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 955 of 6,420 open Head Start slots
  # (14.9%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.14875389408099687,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 240 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5840,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0430
)
