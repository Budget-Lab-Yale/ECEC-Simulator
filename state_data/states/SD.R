#------------------------------------------------------------------------------
# SD.R -- South Dakota
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SD_PREK             <- 3423     # CCD 2018-19, public-school pre-K
SD_HEAD_START       <- 3614     # NIEER State of Preschool 2019, ages 3-4
SD_EARLY_HEAD_START <- 965      # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 892 + 1295 + 750 + 677.



SPEC_SD <- list(
  geo    = 'SD',
  name   = 'South Dakota',
  fips   = 46,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        SD_PREK,
    'Head Start',          SD_HEAD_START,
    'Early Head Start',    SD_EARLY_HEAD_START)
)

