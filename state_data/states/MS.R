#------------------------------------------------------------------------------
# MS.R -- Mississippi
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

MS_PREK             <- 6404     # CCD 2018-19, public-school pre-K
MS_HEAD_START       <- 19661    # NIEER State of Preschool 2019, ages 3-4
MS_EARLY_HEAD_START <- 3147     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 9398 + 10055 + 141 + 67.



SPEC_MS <- list(
  geo    = 'MS',
  name   = 'Mississippi',
  fips   = 28,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MS_PREK,
    'Head Start',          MS_HEAD_START,
    'Early Head Start',    MS_EARLY_HEAD_START)
)

