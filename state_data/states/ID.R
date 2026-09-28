#------------------------------------------------------------------------------
# ID.R -- Idaho
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

ID_PREK             <- 3488     # CCD 2018-19, public-school pre-K
ID_HEAD_START       <- 3071     # NIEER State of Preschool 2019, ages 3-4
ID_EARLY_HEAD_START <- 765      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 780 + 1514 + 83 + 121 + 328 + 245.



SPEC_ID <- list(
  geo    = 'ID',
  name   = 'Idaho',
  fips   = 16,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        ID_PREK,
    'Head Start',          ID_HEAD_START,
    'Early Head Start',    ID_EARLY_HEAD_START)
)

