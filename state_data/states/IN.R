#------------------------------------------------------------------------------
# IN.R -- Indiana
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

IN_PREK             <- 19971    # CCD 2018-19, public-school pre-K
IN_HEAD_START       <- 11493    # NIEER State of Preschool 2019, ages 3-4
IN_EARLY_HEAD_START <- 2480     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_IN <- list(
  geo    = 'IN',
  name   = 'Indiana',
  fips   = 18,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        IN_PREK,
    'Head Start',          IN_HEAD_START,
    'Early Head Start',    IN_EARLY_HEAD_START)
)
