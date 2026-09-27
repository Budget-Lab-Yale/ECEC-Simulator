#------------------------------------------------------------------------------
# VT.R -- Vermont
#
# Region northeast. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

VT_PREK             <- 8594     # CCD 2018-19, public-school pre-K
VT_HEAD_START       <- 965      # NIEER State of Preschool 2019, ages 3-4
VT_EARLY_HEAD_START <- 482      # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_VT <- list(
  geo    = 'VT',
  name   = 'Vermont',
  fips   = 50,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        VT_PREK,
    'Head Start',          VT_HEAD_START,
    'Early Head Start',    VT_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 521 open Head Start slots
  # (0.0%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 241 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.6585,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0422
)
