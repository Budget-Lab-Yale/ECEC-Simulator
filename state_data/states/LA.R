#------------------------------------------------------------------------------
# LA.R -- Louisiana
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

LA_PREK             <- 27412    # CCD 2018-19, public-school pre-K
LA_HEAD_START       <- 17707    # NIEER State of Preschool 2019, ages 3-4
LA_EARLY_HEAD_START <- 3112     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 10642 + 7065.



SPEC_LA <- list(
  geo    = 'LA',
  name   = 'Louisiana',
  fips   = 22,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        LA_PREK,
    'Head Start',          LA_HEAD_START,
    'Early Head Start',    LA_EARLY_HEAD_START)
)

