#------------------------------------------------------------------------------
# MI.R -- Michigan
#
# Region midwest. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

# Public pre-K. CCD 2018-19 carries NO Michigan pre-K count -- 1,064 of
# 1,219 school rows flagged "Not reported", reported rows sum to 0. NIEER
# State of Preschool 2019, MI profile: Great Start Readiness Program enrolled
# 37,140 in 2018-19. As California: NIEER state pre-K where CCD has no count.
# GSRP incl. community-based sites, so MI reads high against CCD states.
MI_PREK             <- 37140    # NIEER 2018-19, GSRP (no CCD count)
MI_HEAD_START       <- 23432    # NIEER State of Preschool 2019, ages 3-4
MI_EARLY_HEAD_START <- 7178     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 11431 + 10924 + 225 + 132 + 416 + 304.



SPEC_MI <- list(
  geo    = 'MI',
  name   = 'Michigan',
  fips   = 26,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MI_PREK,
    'Head Start',          MI_HEAD_START,
    'Early Head Start',    MI_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 15,414 open Head Start slots
  # (0.0%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 233 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5179,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0414
)

