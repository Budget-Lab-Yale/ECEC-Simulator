#------------------------------------------------------------------------------
# AK.R -- Alaska
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

AK_PREK             <- 3544     # CCD 2018-19, public-school pre-K
AK_HEAD_START       <- 2409     # NIEER State of Preschool 2019, ages 3-4
AK_EARLY_HEAD_START <- 726      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 471 + 565 + 670 + 703.



SPEC_AK <- list(
  geo    = 'AK',
  name   = 'Alaska',
  fips   = 2,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        AK_PREK,
    'Head Start',          AK_HEAD_START,
    'Early Head Start',    AK_EARLY_HEAD_START)
)

