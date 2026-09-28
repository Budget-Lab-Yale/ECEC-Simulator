#------------------------------------------------------------------------------
# UT.R -- Utah
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

UT_PREK             <- 16447    # CCD 2018-19, public-school pre-K
UT_HEAD_START       <- 4909     # NIEER State of Preschool 2019, ages 3-4
UT_EARLY_HEAD_START <- 1248     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 1921 + 2455 + 115 + 127 + 149 + 142.



SPEC_UT <- list(
  geo    = 'UT',
  name   = 'Utah',
  fips   = 49,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        UT_PREK,
    'Head Start',          UT_HEAD_START,
    'Early Head Start',    UT_EARLY_HEAD_START)
)

