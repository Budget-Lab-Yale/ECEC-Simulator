#------------------------------------------------------------------------------
# TX.R -- Texas
#
# Region south. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

TX_PREK             <- 264460   # CCD 2018-19, public-school pre-K
TX_HEAD_START       <- 61935    # NIEER State of Preschool 2019, ages 3-4
TX_EARLY_HEAD_START <- 12027    # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_TX <- list(
  geo    = 'TX',
  name   = 'Texas',
  fips   = 48,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        TX_PREK,
    'Head Start',          TX_HEAD_START,
    'Early Head Start',    TX_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 3,659 of 38,360 open Head Start slots
  # (9.5%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.095385818561001048,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 253 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5184,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0417
)
