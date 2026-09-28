#------------------------------------------------------------------------------
# DE.R -- Delaware
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

DE_PREK             <- 2081     # CCD 2018-19, public-school pre-K
DE_HEAD_START       <- 1484     # NIEER State of Preschool 2019, ages 3-4
DE_EARLY_HEAD_START <- 483      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 635 + 849.



SPEC_DE <- list(
  geo    = 'DE',
  name   = 'Delaware',
  fips   = 10,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        DE_PREK,
    'Head Start',          DE_HEAD_START,
    'Early Head Start',    DE_EARLY_HEAD_START)
)

