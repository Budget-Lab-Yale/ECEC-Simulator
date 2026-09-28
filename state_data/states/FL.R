#------------------------------------------------------------------------------
# FL.R -- Florida
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

FL_PREK             <- 64164    # CCD 2018-19, public-school pre-K
FL_HEAD_START       <- 32550    # NIEER State of Preschool 2019, ages 3-4
FL_EARLY_HEAD_START <- 9573     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 13847 + 17212 + 1046 + 445.



SPEC_FL <- list(
  geo    = 'FL',
  name   = 'Florida',
  fips   = 12,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        FL_PREK,
    'Head Start',          FL_HEAD_START,
    'Early Head Start',    FL_EARLY_HEAD_START)
)

