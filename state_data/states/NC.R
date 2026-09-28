#------------------------------------------------------------------------------
# NC.R -- North Carolina
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

NC_PREK             <- 19052    # CCD 2018-19, public-school pre-K
NC_HEAD_START       <- 16476    # NIEER State of Preschool 2019, ages 3-4
NC_EARLY_HEAD_START <- 5155     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 6985 + 9024 + 76 + 68 + 171 + 152.



SPEC_NC <- list(
  geo    = 'NC',
  name   = 'North Carolina',
  fips   = 37,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NC_PREK,
    'Head Start',          NC_HEAD_START,
    'Early Head Start',    NC_EARLY_HEAD_START)
)

