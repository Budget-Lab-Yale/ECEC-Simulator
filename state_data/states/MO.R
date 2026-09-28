#------------------------------------------------------------------------------
# MO.R -- Missouri
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

MO_PREK             <- 32426    # CCD 2018-19, public-school pre-K
MO_HEAD_START       <- 10896    # NIEER State of Preschool 2019, ages 3-4
MO_EARLY_HEAD_START <- 3767     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 5863 + 5033.



SPEC_MO <- list(
  geo    = 'MO',
  name   = 'Missouri',
  fips   = 29,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MO_PREK,
    'Head Start',          MO_HEAD_START,
    'Early Head Start',    MO_EARLY_HEAD_START)
)

