#------------------------------------------------------------------------------
# PA.R -- Pennsylvania
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

PA_PREK             <- 8655     # CCD 2018-19, public-school pre-K
PA_HEAD_START       <- 23775    # NIEER State of Preschool 2019, ages 3-4
PA_EARLY_HEAD_START <- 6615     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 10523 + 13060 + 121 + 71.



SPEC_PA <- list(
  geo    = 'PA',
  name   = 'Pennsylvania',
  fips   = 42,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        PA_PREK,
    'Head Start',          PA_HEAD_START,
    'Early Head Start',    PA_EARLY_HEAD_START)
)

