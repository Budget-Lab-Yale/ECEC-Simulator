#------------------------------------------------------------------------------
# NH.R -- New Hampshire
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

NH_PREK             <- 4221     # CCD 2018-19, public-school pre-K
NH_HEAD_START       <- 1178     # NIEER State of Preschool 2019, ages 3-4
NH_EARLY_HEAD_START <- 385      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 572 + 606.



SPEC_NH <- list(
  geo    = 'NH',
  name   = 'New Hampshire',
  fips   = 33,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NH_PREK,
    'Head Start',          NH_HEAD_START,
    'Early Head Start',    NH_EARLY_HEAD_START)
)

