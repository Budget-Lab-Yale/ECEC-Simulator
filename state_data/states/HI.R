#------------------------------------------------------------------------------
# HI.R -- Hawaii
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

HI_PREK             <- 1580     # CCD 2018-19, public-school pre-K
HI_HEAD_START       <- 2284     # NIEER State of Preschool 2019, ages 3-4
HI_EARLY_HEAD_START <- 657      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 993 + 1291.



SPEC_HI <- list(
  geo    = 'HI',
  name   = 'Hawaii',
  fips   = 15,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        HI_PREK,
    'Head Start',          HI_HEAD_START,
    'Early Head Start',    HI_EARLY_HEAD_START)
)

