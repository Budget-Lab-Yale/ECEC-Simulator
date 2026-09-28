#------------------------------------------------------------------------------
# NJ.R -- New Jersey
#
# Region northeast. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#------------------------------------------------------------------------------


NJ_PREK_CCD         <- 41305   # CCD 2018-19, public-school pre-K
NJ_HEAD_START       <- 11940   # NIEER 2018-19, ages 3-4
NJ_EARLY_HEAD_START <- 3578    # NIEER State(s) of Head Start, 2018-19




SPEC_NJ <- list(
  geo    = 'NJ',
  name   = 'New Jersey',
  fips   = 34,
  region = 'northeast',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        NJ_PREK_CCD,
    'Head Start',          NJ_HEAD_START,
    'Early Head Start',    NJ_EARLY_HEAD_START)
)
