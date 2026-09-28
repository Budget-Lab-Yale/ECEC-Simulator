#------------------------------------------------------------------------------
# AL.R -- Alabama
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

AL_PREK             <- 17464    # CCD 2018-19, public-school pre-K
AL_HEAD_START       <- 11785    # NIEER State of Preschool 2019, ages 3-4
AL_EARLY_HEAD_START <- 2682     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 5555 + 6048 + 93 + 89.



SPEC_AL <- list(
  geo    = 'AL',
  name   = 'Alabama',
  fips   = 1,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        AL_PREK,
    'Head Start',          AL_HEAD_START,
    'Early Head Start',    AL_EARLY_HEAD_START)
)

