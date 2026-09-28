#------------------------------------------------------------------------------
# RI.R -- Rhode Island
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

RI_PREK             <- 2897     # CCD 2018-19, public-school pre-K
RI_HEAD_START       <- 1877     # NIEER State of Preschool 2019, ages 3-4
RI_EARLY_HEAD_START <- 675      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 891 + 986.



SPEC_RI <- list(
  geo    = 'RI',
  name   = 'Rhode Island',
  fips   = 44,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        RI_PREK,
    'Head Start',          RI_HEAD_START,
    'Early Head Start',    RI_EARLY_HEAD_START)
)

