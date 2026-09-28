#------------------------------------------------------------------------------
# VT.R -- Vermont
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

VT_PREK             <- 8594     # CCD 2018-19, public-school pre-K
VT_HEAD_START       <- 965      # NIEER State of Preschool 2019, ages 3-4
VT_EARLY_HEAD_START <- 482      # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_VT <- list(
  geo    = 'VT',
  name   = 'Vermont',
  fips   = 50,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        VT_PREK,
    'Head Start',          VT_HEAD_START,
    'Early Head Start',    VT_EARLY_HEAD_START)
)
