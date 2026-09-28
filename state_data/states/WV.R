#------------------------------------------------------------------------------
# WV.R -- West Virginia
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

WV_PREK             <- 15479    # CCD 2018-19, public-school pre-K
WV_HEAD_START       <- 6781     # NIEER State of Preschool 2019, ages 3-4
WV_EARLY_HEAD_START <- 1068     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_WV <- list(
  geo    = 'WV',
  name   = 'West Virginia',
  fips   = 54,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        WV_PREK,
    'Head Start',          WV_HEAD_START,
    'Early Head Start',    WV_EARLY_HEAD_START)
)
