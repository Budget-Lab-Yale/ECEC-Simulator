#------------------------------------------------------------------------------
# WA.R -- Washington
#
# Region west. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

WA_PREK             <- 32092    # CCD 2018-19, public-school pre-K
WA_HEAD_START       <- 10857    # NIEER State of Preschool 2019, ages 3-4
WA_EARLY_HEAD_START <- 3832     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
# slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.



SPEC_WA <- list(
  geo    = 'WA',
  name   = 'Washington',
  fips   = 53,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        WA_PREK,
    'Head Start',          WA_HEAD_START,
    'Early Head Start',    WA_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 612 of 7,091 open Head Start slots
  # (8.6%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.086306585813002393,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 248 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.4300,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0403
)
