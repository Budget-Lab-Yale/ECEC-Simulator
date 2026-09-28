#------------------------------------------------------------------------------
# MI.R -- Michigan
#
# Region midwest. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Added Sep 2026 w/ 52-geography build (50 states, DC, NYC).
#------------------------------------------------------------------------------

# Public pre-K. CCD 2018-19 carries NO Michigan pre-K count -- 1,064 of
# 1,219 school rows flagged "Not reported", reported rows sum to 0. NIEER
# State of Preschool 2019, MI profile: Great Start Readiness Program enrolled
# 37,140 in 2018-19. As California: NIEER state pre-K where CCD has no count.
# GSRP incl. community-based sites, so MI reads high against CCD states.
MI_PREK             <- 37140    # NIEER 2018-19, GSRP (no CCD count)
MI_HEAD_START       <- 23432    # NIEER State of Preschool 2019, ages 3-4
MI_EARLY_HEAD_START <- 7178     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row, funded slots ages 3 and 4 -- federal,
# AIAN, Migrant & Seasonal where present: 11431 + 10924 + 225 + 132 + 416 + 304.



SPEC_MI <- list(
  geo    = 'MI',
  name   = 'Michigan',
  fips   = 26,
  region = 'midwest',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        MI_PREK,
    'Head Start',          MI_HEAD_START,
    'Early Head Start',    MI_EARLY_HEAD_START)
)

