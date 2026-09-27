#------------------------------------------------------------------------------
# CA.R -- California
#
# Region west. Build reads here: program counts, NSCH rate w/ standard error,
# region, share of Head Start a district runs in own pre-K school. Every
# other constant national or regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

# Public pre-K. CCD 2018-19 has NO California rows -- CA reported no pre-K
# membership to CCD that year. NIEER State of Preschool 2019, CA overview:
# CA State Preschool Program 140,920 + Transitional Kindergarten 104,694.
# CA, MI only geographies w/out a CCD count. Excludes preschool special ed (52,307), which
# CCD counts elsewhere -- so CA reads low against CCD states. CSPP incl.
# contracted non-school centres.
CA_PREK             <- 245614   # NIEER 2018-19, CSPP + TK (no CCD rows)
CA_HEAD_START       <- 70631    # NIEER State of Preschool 2019, ages 3-4
CA_EARLY_HEAD_START <- 24052    # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
# slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.



SPEC_CA <- list(
  geo    = 'CA',
  name   = 'California',
  fips   = 6,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        CA_PREK,
    'Head Start',          CA_HEAD_START,
    'Early Head Start',    CA_EARLY_HEAD_START),

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 0 of 41,519 open Head Start slots
  # (0.0%), from 03_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0,

  # NSCH 2018+2019 pooled k6q20, children 0-4, child-weighted. n = 243 records.
  # Year joins stratum -- each wave keeps own design. 2020 excluded: fielded
  # through pandemic.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.4559,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM x year,
  # PSU HHID, weight FWC, state read as a domain. Carried through to an
  # interval on every output row; nothing else reads it.
  nonparental_rate_se = 0.0413
)
