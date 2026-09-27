#------------------------------------------------------------------------------
# IN.R -- Indiana
#
# Region midwest. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
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

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        IN_PREK,
    'Head Start',          IN_HEAD_START,
    'Early Head Start',    IN_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 151 of 7,325 open Head Start slots
  # (2.1%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.020614334470989763,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 258 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5196,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0384
)
