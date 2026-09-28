#------------------------------------------------------------------------------
# ND.R -- North Dakota
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

ND_PREK             <- 2804     # CCD 2018-19, public-school pre-K
ND_HEAD_START       <- 2302     # NIEER State of Preschool 2019, ages 3-4
ND_EARLY_HEAD_START <- 595      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 664 + 856 + 341 + 441.



SPEC_ND <- list(
  geo    = 'ND',
  name   = 'North Dakota',
  fips   = 38,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        ND_PREK,
    'Head Start',          ND_HEAD_START,
    'Early Head Start',    ND_EARLY_HEAD_START)
)

