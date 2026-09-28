#------------------------------------------------------------------------------
# MD.R -- Maryland
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

MD_PREK             <- 30950    # CCD 2018-19, public-school pre-K
MD_HEAD_START       <- 7628     # NIEER State of Preschool 2019, ages 3-4
MD_EARLY_HEAD_START <- 1855     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 4124 + 3504.



SPEC_MD <- list(
  geo    = 'MD',
  name   = 'Maryland',
  fips   = 24,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MD_PREK,
    'Head Start',          MD_HEAD_START,
    'Early Head Start',    MD_EARLY_HEAD_START)
)

