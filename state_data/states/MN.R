#------------------------------------------------------------------------------
# MN.R -- Minnesota
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
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
    'Early Head Start',    MN_EARLY_HEAD_START)
)
