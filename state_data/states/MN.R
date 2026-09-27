#------------------------------------------------------------------------------
# MN.R -- Minnesota
#
# Region midwest. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

MN_PREK             <- 24458    # CCD 2018-19, public-school pre-K
MN_HEAD_START       <- 9348     # NIEER State of Preschool 2019, ages 3-4
MN_EARLY_HEAD_START <- 2446     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
# slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.



SPEC_MN <- list(
  geo    = 'MN',
  name   = 'Minnesota',
  fips   = 27,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MN_PREK,
    'Head Start',          MN_HEAD_START,
    'Early Head Start',    MN_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 6,452 open Head Start slots
  # (0.0%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 289 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5705,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0368
)
