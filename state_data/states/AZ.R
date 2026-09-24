#------------------------------------------------------------------------------
# AZ.R -- Arizona
#
# Region west. The build reads three things here: the program counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

AZ_PREK             <- 19371    # CCD 2018-19, public-school pre-K
AZ_HEAD_START       <- 13891    # NIEER State of Preschool 2019, ages 3-4
AZ_EARLY_HEAD_START <- 3339     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start counts: NIEER state figures. Whether they include the AIAN
# (tribal) programs in this state is not checked.



SPEC_AZ <- list(
  geo    = 'AZ',
  name   = 'Arizona',
  fips   = 4,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        AZ_PREK,
    'Head Start',          AZ_HEAD_START,
    'Early Head Start',    AZ_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 13,475; w/ Head Start, unduplicated,
  # 27,367. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 5,410 (profile p.55) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 13475, with_hs = 27367),
  nieer_state_prek = 5410,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 9,237 open Head Start slots
  # (0.0%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 257 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.4793,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0400
)
