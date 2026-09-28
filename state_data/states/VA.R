#------------------------------------------------------------------------------
# VA.R -- Virginia
#
# Region south. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#------------------------------------------------------------------------------


VA_PREK_CCD         <- 33790   # CCD 2018-19, public-school pre-K
VA_HEAD_START       <- 11579   # NIEER 2018-19, ages 3-4
VA_EARLY_HEAD_START <- 2507    # published; cross-checks against ACF FY2019




SPEC_VA <- list(
  geo    = 'VA',
  name   = 'Virginia',
  fips   = 51,
  region = 'south',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        VA_PREK_CCD,
    'Head Start',          VA_HEAD_START,
    'Early Head Start',    VA_EARLY_HEAD_START)
)
