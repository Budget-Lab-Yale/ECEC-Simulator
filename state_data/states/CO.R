#------------------------------------------------------------------------------
# CO.R -- Colorado
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

CO_PREK             <- 33716    # CCD 2018-19, public-school pre-K
CO_HEAD_START       <- 8152     # NIEER State of Preschool 2019, ages 3-4
CO_EARLY_HEAD_START <- 2015     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_CO <- list(
  geo    = 'CO',
  name   = 'Colorado',
  fips   = 8,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        CO_PREK,
    'Head Start',          CO_HEAD_START,
    'Early Head Start',    CO_EARLY_HEAD_START)
)
