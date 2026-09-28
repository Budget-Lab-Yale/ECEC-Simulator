#------------------------------------------------------------------------------
# AR.R -- Arkansas
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

AR_PREK             <- 16761    # CCD 2018-19, public-school pre-K
AR_HEAD_START       <- 6561     # NIEER State of Preschool 2019, ages 3-4
AR_EARLY_HEAD_START <- 3284     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 3544 + 2769 + 162 + 86.



SPEC_AR <- list(
  geo    = 'AR',
  name   = 'Arkansas',
  fips   = 5,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        AR_PREK,
    'Head Start',          AR_HEAD_START,
    'Early Head Start',    AR_EARLY_HEAD_START)
)

