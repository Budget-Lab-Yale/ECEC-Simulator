#------------------------------------------------------------------------------
# OK.R -- Oklahoma
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

OK_PREK             <- 41901    # CCD 2018-19, public-school pre-K
OK_HEAD_START       <- 13707    # NIEER State of Preschool 2019, ages 3-4
OK_EARLY_HEAD_START <- 3677     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 6988 + 4282 + 1284 + 1153.



SPEC_OK <- list(
  geo    = 'OK',
  name   = 'Oklahoma',
  fips   = 40,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        OK_PREK,
    'Head Start',          OK_HEAD_START,
    'Early Head Start',    OK_EARLY_HEAD_START)
)

