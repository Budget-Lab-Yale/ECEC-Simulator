#------------------------------------------------------------------------------
# NY.R -- New York State, all 62 counties, city included.
#
# Fields, what load_spec() adds: geo_spec() in 00_build_demand_targets.R.
#
# NYC.R builds the five boroughs on their own, from the constants here. The two
# outputs OVERLAP by design: each is a deliverable in its own right.
#------------------------------------------------------------------------------

# The five boroughs -- county names as Census PEP writes them; 5-digit FIPS in
# same order from SUBSTATE_GEOGRAPHIES (constants.R), the one definition of
# NYC. NYC.R, 03_head_start_school_overlap.R, checks read these.
NYC_BOROUGHS    <- c('Bronx', 'Kings', 'New York', 'Queens', 'Richmond')
NYC_COUNTY_FIPS <- with(SUBSTATE_GEOGRAPHIES$NYC,
                        sprintf('%02d%03d', statefip, countyfip))


# Public-school pre-K, CCD 2018-19: 54,451 children across all 62 counties,
# counted school by school.
NY_PREK_CCD  <- 54451
NYC_PREK_CCD <- 26717   # CCD, five boroughs

# Head Start ages 3-4, NIEER 2018-19. GROSS of pre-K overlap.
NY_HEAD_START  <- 36863

NY_EARLY_HEAD_START  <- 11433

# NYC SHARES. City's OPEN funded slots over New York's, ACF Head Start
# service-location file (2026-09-23 export -- same file 03_ reads). Head Start
# types 1, 3, 5; Early Head Start types 2, 4, 6. Applied to NIEER state counts;
# assumes city's 2026 share held in 2018-19.
# tests/check_program_counts_match_sources.R recomputes both from the file.
NYC_HS_SLOTS  <- 7064;  NY_HS_SLOTS  <- 20518
NYC_EHS_SLOTS <- 4426;  NY_EHS_SLOTS <- 8590
NYC_HEAD_START       <- round(NY_HEAD_START * NYC_HS_SLOTS / NY_HS_SLOTS)              # 12,691
NYC_EARLY_HEAD_START <- round(NY_EARLY_HEAD_START * NYC_EHS_SLOTS / NY_EHS_SLOTS)  # 5,891

SPEC_NY <- geo_spec('NY', 'New York State', fips = 36, region = 'northeast',
  prek             = NY_PREK_CCD,
  head_start       = NY_HEAD_START,
  early_head_start = NY_EARLY_HEAD_START)
