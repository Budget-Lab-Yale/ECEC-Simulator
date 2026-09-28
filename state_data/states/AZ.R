#------------------------------------------------------------------------------
# AZ.R -- Arizona
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

AZ_PREK             <- 19371    # CCD 2018-19, public-school pre-K
AZ_HEAD_START       <- 13891    # NIEER State of Preschool 2019, ages 3-4
AZ_EARLY_HEAD_START <- 3339     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
# slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.



SPEC_AZ <- list(
  geo    = 'AZ',
  name   = 'Arizona',
  fips   = 4,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        AZ_PREK,
    'Head Start',          AZ_HEAD_START,
    'Early Head Start',    AZ_EARLY_HEAD_START)
)
