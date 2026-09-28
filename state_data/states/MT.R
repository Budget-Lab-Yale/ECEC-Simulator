#------------------------------------------------------------------------------
# MT.R -- Montana
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

MT_PREK             <- 979      # CCD 2018-19, public-school pre-K
MT_HEAD_START       <- 3863     # NIEER State of Preschool 2019, ages 3-4
MT_EARLY_HEAD_START <- 978      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 1070 + 1344 + 678 + 771.



SPEC_MT <- list(
  geo    = 'MT',
  name   = 'Montana',
  fips   = 30,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MT_PREK,
    'Head Start',          MT_HEAD_START,
    'Early Head Start',    MT_EARLY_HEAD_START)
)

