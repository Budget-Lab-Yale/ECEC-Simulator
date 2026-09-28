#------------------------------------------------------------------------------
# NM.R -- New Mexico
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

NM_PREK             <- 10174    # CCD 2018-19, public-school pre-K
NM_HEAD_START       <- 7427     # NIEER State of Preschool 2019, ages 3-4
NM_EARLY_HEAD_START <- 2080     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
# slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.



SPEC_NM <- list(
  geo    = 'NM',
  name   = 'New Mexico',
  fips   = 35,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NM_PREK,
    'Head Start',          NM_HEAD_START,
    'Early Head Start',    NM_EARLY_HEAD_START)
)
