#------------------------------------------------------------------------------
# TX.R -- Texas
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

TX_PREK             <- 264460   # CCD 2018-19, public-school pre-K
TX_HEAD_START       <- 61935    # NIEER State of Preschool 2019, ages 3-4
TX_EARLY_HEAD_START <- 12027    # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_TX <- list(
  geo    = 'TX',
  name   = 'Texas',
  fips   = 48,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        TX_PREK,
    'Head Start',          TX_HEAD_START,
    'Early Head Start',    TX_EARLY_HEAD_START)
)
