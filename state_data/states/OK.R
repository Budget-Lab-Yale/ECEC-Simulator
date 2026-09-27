#------------------------------------------------------------------------------
# OK.R -- Oklahoma
#
# Region south. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

OK_PREK             <- 41901    # CCD 2018-19, public-school pre-K
OK_HEAD_START       <- 13707    # NIEER State of Preschool 2019, ages 3-4
OK_EARLY_HEAD_START <- 3677     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 6988 + 4282 + 1284 + 1153.



SPEC_OK <- list(
  geo    = 'OK',
  name   = 'Oklahoma',
  fips   = 40,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        OK_PREK,
    'Head Start',          OK_HEAD_START,
    'Early Head Start',    OK_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 16 of 7,385 open Head Start slots
  # (0.2%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.00216655382532159,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 283 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.4561,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0376
)

