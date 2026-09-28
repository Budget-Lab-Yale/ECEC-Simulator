#------------------------------------------------------------------------------
# CA.R -- California
#
# Region west. Build reads here: program counts and Census region, each
# hand-researched. NSCH rate w/ standard error, and Head Start's
# in-own-school share, are read live from out/ by load_spec() -- not
# hand-copied here. Every other constant national or regional -- see
# 00_main.R.
#
# Full section in the Sep 2026 Flashpoint States memo(s).
#------------------------------------------------------------------------------

# Public pre-K. CCD 2018-19 has NO California rows -- CA reported no pre-K
# membership to CCD that year. NIEER State of Preschool 2019, CA overview:
# CA State Preschool Program 140,920 + Transitional Kindergarten 104,694.
# CA, MI only geographies w/out a CCD count. Excludes preschool special ed (52,307), which
# CCD counts elsewhere -- so CA reads low against CCD states. CSPP incl.
# contracted non-school centres.
CA_PREK             <- 245614   # NIEER 2018-19, CSPP + TK (no CCD rows)
CA_HEAD_START       <- 70631    # NIEER State of Preschool 2019, ages 3-4
CA_EARLY_HEAD_START <- 24052    # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start: NIEER Appendix B row -- federal, AIAN, Migrant & Seasonal funded
# slots, ages 3-4. tests/check_program_counts_match_sources.R checks the sum.



SPEC_CA <- list(
  geo    = 'CA',
  name   = 'California',
  fips   = 6,
  region = 'west',

  programs = tibble::tribble(
    ~program,          ~n_children,
    'public pre-K',        CA_PREK,
    'Head Start',          CA_HEAD_START,
    'Early Head Start',    CA_EARLY_HEAD_START)
)
