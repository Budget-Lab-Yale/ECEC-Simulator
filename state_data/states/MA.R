#------------------------------------------------------------------------------
# MA.R -- Massachusetts
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

MA_PREK             <- 30268    # CCD 2018-19, public-school pre-K
MA_HEAD_START       <- 9580     # NIEER State of Preschool 2019, ages 3-4
MA_EARLY_HEAD_START <- 2115     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_MA <- list(
  geo    = 'MA',
  name   = 'Massachusetts',
  fips   = 25,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MA_PREK,
    'Head Start',          MA_HEAD_START,
    'Early Head Start',    MA_EARLY_HEAD_START)
)
