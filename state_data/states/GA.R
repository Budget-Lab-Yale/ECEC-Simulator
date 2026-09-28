#------------------------------------------------------------------------------
# GA.R -- Georgia
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

GA_PREK             <- 49315    # CCD 2018-19, public-school pre-K
GA_HEAD_START       <- 19842    # NIEER State of Preschool 2019, ages 3-4
GA_EARLY_HEAD_START <- 4501     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 11312 + 8275 + 172 + 83.



SPEC_GA <- list(
  geo    = 'GA',
  name   = 'Georgia',
  fips   = 13,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        GA_PREK,
    'Head Start',          GA_HEAD_START,
    'Early Head Start',    GA_EARLY_HEAD_START)
)

