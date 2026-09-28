#------------------------------------------------------------------------------
# OH.R -- Ohio
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

OH_PREK             <- 39438    # CCD 2018-19, public-school pre-K
OH_HEAD_START       <- 27951    # NIEER State of Preschool 2019, ages 3-4
OH_EARLY_HEAD_START <- 6053     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 13287 + 14664.



SPEC_OH <- list(
  geo    = 'OH',
  name   = 'Ohio',
  fips   = 39,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        OH_PREK,
    'Head Start',          OH_HEAD_START,
    'Early Head Start',    OH_EARLY_HEAD_START)
)

