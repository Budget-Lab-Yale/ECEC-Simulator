#------------------------------------------------------------------------------
# IA.R -- Iowa
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

IA_PREK             <- 23060    # CCD 2018-19, public-school pre-K
IA_HEAD_START       <- 5571     # NIEER State of Preschool 2019, ages 3-4
IA_EARLY_HEAD_START <- 1702     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 2577 + 2994.



SPEC_IA <- list(
  geo    = 'IA',
  name   = 'Iowa',
  fips   = 19,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        IA_PREK,
    'Head Start',          IA_HEAD_START,
    'Early Head Start',    IA_EARLY_HEAD_START)
)

