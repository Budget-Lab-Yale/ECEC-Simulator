#------------------------------------------------------------------------------
# NM.R -- New Mexico
#
# Region west. The build reads three things here: the programme counts, the
# NSCH rate w/ its standard error, and the region. Every other constant is
# NSECE and regional -- see 00_main.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

NM_PREK             <- 10174    # CCD 2018-19, public-school pre-K
NM_HEAD_START       <- 7427     # NIEER State of Preschool 2019, ages 3-4
NM_EARLY_HEAD_START <- 2080     # NIEER State(s) of Head Start (2023), 2018-19 bar

# Head Start counts: NIEER state figures. Whether they include the AIAN
# (tribal) programmes in this state is not checked.



SPEC_NM <- list(
  geo    = 'NM',
  name   = 'New Mexico',
  fips   = 35,
  region = 'west',

  programmes = tibble::tribble(
    ~programme,          ~n_children,
    'public pre-K',        NM_PREK,
    'Head Start',          NM_HEAD_START,
    'Early Head Start',    NM_EARLY_HEAD_START),

  # HEAD START / STATE PRE-K OVERLAP. NIEER State of Preschool 2019, Table 4
  # (p.28), 3s + 4s: pre-K + special ed 13,350; w/ Head Start, unduplicated,
  # 20,777. Difference = Head Start net of children NIEER finds in state pre-K.
  # State pre-K total 11,251 (profile p.134) -- scales overlap to CCD coverage.
  nieer_undup_34   = c(without_hs = 13350, with_hs = 20777),
  nieer_state_prek = 11251,

  # HEAD START A DISTRICT RUNS IN ITS OWN PRE-K SCHOOL -- deducted in step 3,
  # since CCD counts those children too. 408 of 3,373 open Head Start slots
  # (12.1%), from 04_head_start_in_schools.R; `--check` compares back to it.
  hs_in_schools_share = 0.12096056922620813,

  # NSCH 2019 k6q20, children 0-4, child-weighted. n = 129 records.
  # Single wave; 01_nsch_rate.R prints the pools.
  # Derived by 01_nsch_rate.R; `--check` compares this constant back to it.
  nonparental_rate = 0.5573,

  # Standard error, FROM THE SURVEY DESIGN: strata FIPSST x STRATUM, PSU HHID,
  # weight FWC, state read as a domain. Carried through to an interval on
  # every output row; nothing else reads it.
  nonparental_rate_se = 0.0584
)
