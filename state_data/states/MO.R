#------------------------------------------------------------------------------
# MO.R -- Missouri
#
# Region midwest. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

MO_PREK             <- 32426    # CCD 2018-19, public-school pre-K
MO_HEAD_START       <- 10896    # NIEER State of Preschool 2019, ages 3-4
MO_EARLY_HEAD_START <- 3767     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 5863 + 5033.



SPEC_MO <- list(
  geo    = 'MO',
  name   = 'Missouri',
  fips   = 29,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MO_PREK,
    'Head Start',          MO_HEAD_START,
    'Early Head Start',    MO_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 238 of 7,842 open Head Start slots
  # (3.0%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.03034940066309615,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 280 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.6083,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0384
)

