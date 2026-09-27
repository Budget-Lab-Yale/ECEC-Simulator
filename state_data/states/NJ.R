#------------------------------------------------------------------------------
# NJ.R -- New Jersey
#
# Region northeast. Build reads here: program counts, NSCH rate w/ standard
# error, region, share of Head Start a district runs in own pre-K school.
# Every other constant national or regional -- see 00_main.R.
#------------------------------------------------------------------------------


NJ_PREK_CCD         <- 41305   # CCD 2018-19, public-school pre-K
NJ_HEAD_START       <- 11940   # NIEER 2018-19, ages 3-4
NJ_EARLY_HEAD_START <- 3578    # NIEER State(s) of Head Start, 2018-19




SPEC_NJ <- list(
  geo    = 'NJ',
  name   = 'New Jersey',
  fips   = 34,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NJ_PREK_CCD,
    'Head Start',          NJ_HEAD_START,
    'Early Head Start',    NJ_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 772 of 6,788 open Head Start slots
  # (11.4%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.11373011196228638,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 208 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5919,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0434
)
