#------------------------------------------------------------------------------
# acquire_tract_urban_children_2010.R -- 2010 Census tract urban / rural
# population, children under 5, 50 states + DC (Census API, 2010 Decennial
# SF1; key CENSUS_API_KEY from .Renviron). Feeds state child shares by NSECE community urban density
# (urban_density.R codes: 1 urban share >= 0.85, 2 0.30-0.85, 3 < 0.30).
#
# Variables: P002001 total, P002002 urban, P002005 rural; P012003 male under
#   5, P012027 female under 5.
#
# Usage: Rscript resources/census_population/acquire_tract_urban_children_2010.R
#   writes resources/census_population/US_Census__tract_urban_rural_children_under5__2010.csv
#   (+ .provenance.txt)
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(jsonlite); library(dplyr)})
source('src/shared_functions/constants.R')   # STATE_POSTAL_TO_FIPS

URL <- 'https://api.census.gov/data/2010/dec/sf1?get=P002001,P002002,P002005,P012003,P012027&for=tract:*&in=state:%02d'
OUT <- file.path('resources', 'census_population', 'US_Census__tract_urban_rural_children_under5__2010.csv')

KEY <- Sys.getenv('CENSUS_API_KEY')
if (!nzchar(KEY)) stop('CENSUS_API_KEY unset -- add to .Renviron (key registry: ~/.config/api-keys/REGISTRY.md)')
rows <- lapply(STATE_POSTAL_TO_FIPS, function(f) {
  m <- fromJSON(paste0(sprintf(URL, f), '&key=', KEY))
  d <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(d) <- m[1, ]
  Sys.sleep(0.2)
  d
})
d <- bind_rows(rows) %>%
  transmute(geoid = paste0(state, county, tract), state, county, tract,
            pop_total = as.numeric(P002001), pop_urban = as.numeric(P002002),
            pop_rural = as.numeric(P002005),
            children_under5 = as.numeric(P012003) + as.numeric(P012027))
if (anyNA(d$pop_total) || length(unique(d$state)) != length(STATE_POSTAL_TO_FIPS) ||
    any(abs(d$pop_urban + d$pop_rural - d$pop_total) > 0)) stop('Census tract pull incomplete or inconsistent')
write.csv(d, OUT, row.names = FALSE)
writeLines(c(paste('source:', sub('%02d', '<state fips>', URL)),
             paste('retrieved:', format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')),
             paste('tracts:', nrow(d), ' children under 5:', sum(d$children_under5)),
             paste('md5:', unname(tools::md5sum(OUT)))),
           paste0(OUT, '.provenance.txt'))
cat('wrote', OUT, nrow(d), 'tracts\n')
