#------------------------------------------------------------------------------
# DC.R -- District of Columbia
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

DC_PREK             <- 12815    # CCD 2018-19, public-school pre-K
DC_HEAD_START       <- 2150     # NIEER State of Preschool 2019, ages 3-4
DC_EARLY_HEAD_START <- 1577     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 1032 + 1118.



SPEC_DC <- list(
  geo    = 'DC',
  name   = 'District of Columbia',
  fips   = 11,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        DC_PREK,
    'Head Start',          DC_HEAD_START,
    'Early Head Start',    DC_EARLY_HEAD_START)
)

