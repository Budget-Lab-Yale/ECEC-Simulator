#------------------------------------------------------------------------------
# CT.R -- Connecticut
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

CT_PREK             <- 14304    # CCD 2018-19, public-school pre-K
CT_HEAD_START       <- 4206     # NIEER State of Preschool 2019, ages 3-4
CT_EARLY_HEAD_START <- 1292     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_CT <- list(
  geo    = 'CT',
  name   = 'Connecticut',
  fips   = 9,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        CT_PREK,
    'Head Start',          CT_HEAD_START,
    'Early Head Start',    CT_EARLY_HEAD_START)
)
