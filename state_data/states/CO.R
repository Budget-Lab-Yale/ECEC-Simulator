#------------------------------------------------------------------------------
# CO.R -- Colorado
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Flashpoint, Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_CO <- geo_spec('CO', 'Colorado', fips = 8, region = 'west',
  prek             = 33716,   # CCD 2018-19, public-school pre-K
  head_start       = 8152,    # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 2015)    # NIEER State(s) of Head Start (2023), 2018-19 bar
