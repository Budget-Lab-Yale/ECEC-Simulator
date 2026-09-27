#------------------------------------------------------------------------------
# KS.R -- Kansas
#
# Region midwest. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

KS_PREK             <- 19508    # CCD 2018-19, public-school pre-K
KS_HEAD_START       <- 5139     # NIEER State of Preschool 2019, ages 3-4
KS_EARLY_HEAD_START <- 2030     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 2138 + 2939 + 26 + 36.



SPEC_KS <- list(
  geo    = 'KS',
  name   = 'Kansas',
  fips   = 20,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        KS_PREK,
    'Head Start',          KS_HEAD_START,
    'Early Head Start',    KS_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 337 of 3,720 open Head Start slots
  # (9.1%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.090591397849462366,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 270 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5266,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0398
)

