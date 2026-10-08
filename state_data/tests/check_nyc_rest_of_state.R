#-----------------------------------------------------------------------------
# Does NY minus NYC agree with a separate rest-of-state build?
#
# NY, NYC each built on own households -- NY includes city, so the two runs
# overlap. Rest of state built here same way as NYC: NY's NSCH rate, PEP
# counties outside five boroughs, NY
# program counts less NYC's,
# p0 rows NY less NYC. p0 mix a ratio per geography -- difference only
# approximates a rest-of-state build. Memo quotes max gap per care type.
#
#   Rscript state_data/tests/check_nyc_rest_of_state.R
#
# Exit status is 1 when any care type differs by MAX_GAP or more.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({library(dplyr)})

# Relative, per care type. 2026-10-07 build (national 5+ share, NYC 0): 0.88%,
# Unpaid Center-Based -- children match exactly, gap = part-time share (NY, NYC, rest
# each own p0 ratio).
MAX_GAP <- 0.01

source('state_data/code/00_build_demand_targets.R')

ny  <- load_spec('NY')
nyc <- load_spec('NYC')

# p0 rows for 'NYR' = NY rows less NYC rows, same keys
p0_rows_geo <- p0_rows
p0_rows <- function(geo, types = NULL, hint = '') {
  if (geo != 'NYR') return(p0_rows_geo(geo, types, hint))
  a <- p0_rows_geo('NY', types)
  b <- p0_rows_geo('NYC', types)
  key <- function(d) paste(d$ecec_type, d$hours_choice)
  a$children <- as.character(
    num(a$children) - dplyr::coalesce(num(b$children)[match(key(a), key(b))], 0))
  a
}

pep <- read_chr(raw_path('US_PEP__population_children_age_0_4_by_county__2019.csv'))
ny_counties <- paste0(pep$STATE, pep$COUNTY)[num(pep$STATE) == ny$fips]

rest <- ny
rest$geo         <- 'NYR'
rest$name        <- 'New York outside NYC'
rest$county_fips <- setdiff(ny_counties, nyc$county_fips)
rest$programs$n_children <- ny$programs$n_children - nyc$programs$n_children
rest$nonparental_rate <- ny$nonparental_rate
# All-settings pre-K: NY's NIEER count less NYC's DOE count; NIEER overlap
# share NY's, as NYC's (copied w/ ny)
rest$prek_all_settings <- ny$prek_all_settings - nyc$prek_all_settings
# Five-year-olds: national share outside NYC -- NY's applies it to rest only
rest$age5_share <- AGE5_NHES_NATIONAL
# Register paid care: NY's register less NYC's (both registers state law + DOHMH)
if (!is.null(ny$register_paid) && !is.null(nyc$register_paid))
  rest$register_paid <- list(centre = ny$register_paid$centre - nyc$register_paid$centre,
                             home   = ny$register_paid$home - nyc$register_paid$home)

r_ny   <- build_geo(ny)
r_nyc  <- build_geo(nyc)
r_rest <- build_geo(rest)

d <- tibble::tibble(ecec_type     = r_ny$care$ecec_type,
                    ny_less_nyc   = r_ny$care$annual_hours - r_nyc$care$annual_hours,
                    rest_of_state = r_rest$care$annual_hours) %>%
  mutate(gap = ny_less_nyc / rest_of_state - 1)

cat('\n== NY minus NYC vs rest-of-state build (annual hours)\n')
print(as.data.frame(d), row.names = FALSE)
worst <- max(abs(d$gap))
cat(sprintf('\nmax |gap| %.2f%% (limit %.0f%%)\n', 100 * worst, 100 * MAX_GAP))
if (worst >= MAX_GAP) {
  cat('FAIL: NY minus NYC drifts from a rest-of-state build\n')
  quit(status = 1)
}
cat('PASS\n')
