#------------------------------------------------------------------------------
# AR.R -- Arkansas
#
# Region south. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

AR_PREK             <- 16761    # CCD 2018-19, public-school pre-K
AR_HEAD_START       <- 6561     # NIEER State of Preschool 2019, ages 3-4
AR_EARLY_HEAD_START <- 3284     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 3544 + 2769 + 162 + 86.



SPEC_AR <- list(
  geo    = 'AR',
  name   = 'Arkansas',
  fips   = 5,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        AR_PREK,
    'Head Start',          AR_HEAD_START,
    'Early Head Start',    AR_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 76 of 2,932 open Head Start slots
  # (2.6%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.025920873124147339,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 318 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5669,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0360
)

