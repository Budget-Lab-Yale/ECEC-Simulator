#------------------------------------------------------------------------------
# SC.R -- South Carolina
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

SC_PREK             <- 27435    # CCD 2018-19, public-school pre-K
SC_HEAD_START       <- 9710     # NIEER State of Preschool 2019, ages 3-4
SC_EARLY_HEAD_START <- 2372     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 5926 + 3616 + 43 + 37 + 51 + 37.



SPEC_SC <- list(
  geo    = 'SC',
  name   = 'South Carolina',
  fips   = 45,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        SC_PREK,
    'Head Start',          SC_HEAD_START,
    'Early Head Start',    SC_EARLY_HEAD_START)
)

