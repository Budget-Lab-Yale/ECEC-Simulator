#------------------------------------------------------------------------------
# NV.R -- Nevada
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

NV_PREK             <- 11598    # CCD 2018-19, public-school pre-K
NV_HEAD_START       <- 2446     # NIEER State of Preschool 2019, ages 3-4
NV_EARLY_HEAD_START <- 937      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 1097 + 987 + 164 + 198.



SPEC_NV <- list(
  geo    = 'NV',
  name   = 'Nevada',
  fips   = 32,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NV_PREK,
    'Head Start',          NV_HEAD_START,
    'Early Head Start',    NV_EARLY_HEAD_START)
)

