#------------------------------------------------------------------------------
# OR.R -- Oregon
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

OR_PREK             <- 216      # CCD 2018-19, public-school pre-K
OR_HEAD_START       <- 6937     # NIEER State of Preschool 2019, ages 3-4
OR_EARLY_HEAD_START <- 2241     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 1810 + 2536 + 159 + 159 + 1368 + 905.



SPEC_OR <- list(
  geo    = 'OR',
  name   = 'Oregon',
  fips   = 41,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        OR_PREK,
    'Head Start',          OR_HEAD_START,
    'Early Head Start',    OR_EARLY_HEAD_START)
)

