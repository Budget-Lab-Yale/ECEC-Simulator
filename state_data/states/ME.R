#------------------------------------------------------------------------------
# ME.R -- Maine
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

ME_PREK             <- 5857     # CCD 2018-19, public-school pre-K
ME_HEAD_START       <- 2276     # NIEER State of Preschool 2019, ages 3-4
ME_EARLY_HEAD_START <- 804      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 949 + 1267 + 28 + 32.



SPEC_ME <- list(
  geo    = 'ME',
  name   = 'Maine',
  fips   = 23,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        ME_PREK,
    'Head Start',          ME_HEAD_START,
    'Early Head Start',    ME_EARLY_HEAD_START)
)

