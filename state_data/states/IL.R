#------------------------------------------------------------------------------
# IL.R -- Illinois
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# Full section in the Sep 2026 Expansion States memo(s).
#------------------------------------------------------------------------------

SPEC_IL <- geo_spec('IL', 'Illinois', fips = 17, region = 'midwest',
  prek             = 81044,   # CCD 2018-19, public-school pre-K
  head_start       = 26175,   # NIEER State of Preschool 2019, ages 3-4
  early_head_start = 8889)    # NIEER State(s) of Head Start (2023), 2018-19 bar
