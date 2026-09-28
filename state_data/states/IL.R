#------------------------------------------------------------------------------
# IL.R -- Illinois
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

IL_PREK             <- 81044    # CCD 2018-19, public-school pre-K
IL_HEAD_START       <- 26175    # NIEER State of Preschool 2019, ages 3-4
IL_EARLY_HEAD_START <- 8889     # NIEER State(s) of Head Start (2023), 2018-19 bar



SPEC_IL <- list(
  geo    = 'IL',
  name   = 'Illinois',
  fips   = 17,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        IL_PREK,
    'Head Start',          IL_HEAD_START,
    'Early Head Start',    IL_EARLY_HEAD_START)
)
