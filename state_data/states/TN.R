#------------------------------------------------------------------------------
# TN.R -- Tennessee
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

TN_PREK             <- 32276    # CCD 2018-19, public-school pre-K
TN_HEAD_START       <- 14605    # NIEER State of Preschool 2019, ages 3-4
TN_EARLY_HEAD_START <- 2277     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 6632 + 7807 + 87 + 79.



SPEC_TN <- list(
  geo    = 'TN',
  name   = 'Tennessee',
  fips   = 47,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        TN_PREK,
    'Head Start',          TN_HEAD_START,
    'Early Head Start',    TN_EARLY_HEAD_START)
)

