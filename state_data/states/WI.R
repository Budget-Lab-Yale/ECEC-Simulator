#------------------------------------------------------------------------------
# WI.R -- Wisconsin
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

WI_PREK             <- 56096    # CCD 2018-19, public-school pre-K
WI_HEAD_START       <- 11015    # NIEER State of Preschool 2019, ages 3-4
WI_EARLY_HEAD_START <- 3289     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
# slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.



SPEC_WI <- list(
  geo    = 'WI',
  name   = 'Wisconsin',
  fips   = 55,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        WI_PREK,
    'Head Start',          WI_HEAD_START,
    'Early Head Start',    WI_EARLY_HEAD_START)
)
