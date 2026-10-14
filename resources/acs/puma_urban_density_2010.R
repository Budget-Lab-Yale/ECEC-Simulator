#------------------------------------------------------------------------------
# puma_urban_density_2010.R -- writes puma_urban_density_2010.csv, share of
# 2010 children 0-4 at each community urban density level (1 high,
# 2 moderate, 3 rural) per 2010 PUMA (statefip, puma). No longer read (2026-10-14):
# 1b no longer averages care-forest predictions over PUMA shares
# (care forest has no density feature).
#
# MEASURE. NSECE density (HH9_COMM_URBAN_DENSITY) = 2010 Census urban share
# of tracts in 2-mile community around household's tract, weights inversely
# proportional to distance (2019 household user guide, sec. 15); cuts 0.85 /
# 0.30. Same measure computed for every 2010 tract (tract_urban_share(),
# urban_density.R), cut, then tracts pooled to PUMA, weighted by children
# 0-4 (aggregate_puma_density()). ACS PUMS has no tract -- PUMA smallest
# geography; 2010 PUMAs nest 2010 tracts exactly.
#
# DEVIATIONS from NSECE (guide gives no geometry, no anchor distance):
#   - overlap w/ 2-mile circle approximated by equal-area disk per tract
#     (Gazetteer land area), not tract polygons
#   - distance floor 2/3 x anchor's equal-area radius (anchor's own distance
#     0 otherwise)
#   - 2010 tracts, not NSECE's sampling-frame tracts (ACS 2013 boundaries --
#     same as 2010)
#
# SOURCES (state_data/raw/US/):
#   - US_Census__tract_urban_rural_population_children_0_4__2010.csv --
#     extract of 2010 SF1 Urban/Rural Update (www2.census.gov/census_2010/
#     04-Summary_File_1/Urban_Rural_Update/<State>/<st>2010.ur1.zip), tract
#     summary level 140: P2 total / urban / rural (segment 02), P12 male +
#     female under 5 (segment 04). Built by `extract` mode below -- state zips
#     ~11 GB, not kept.
#   - US_Census__tract_population_centroids__2010.txt (www2.census.gov/geo/
#     docs/reference/cenpop2010/tract/CenPop2010_Mean_TR.txt)
#   - US_Census__gazetteer_tracts__2010.zip (www2.census.gov/geo/docs/
#     maps-data/data/gazetteer/Gaz_tracts_national.zip) -- land area
#   - US_Census__tract_to_puma_relationship__2010.txt (www2.census.gov/geo/
#     docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt)
#
# Usage, from repo root:
#   Rscript resources/acs/puma_urban_density_2010.R
#   Rscript resources/acs/puma_urban_density_2010.R extract <dir of *.ur1.zip>
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(data.table)
})

source('src/shared_functions/urban_density.R')

RAW        <- file.path('state_data', 'raw', 'US')
TRACT_POP  <- file.path(RAW, 'US_Census__tract_urban_rural_population_children_0_4__2010.csv')
CENTROIDS  <- file.path(RAW, 'US_Census__tract_population_centroids__2010.txt')
GAZETTEER  <- file.path(RAW, 'US_Census__gazetteer_tracts__2010.zip')
TRACT_PUMA <- file.path(RAW, 'US_Census__tract_to_puma_relationship__2010.txt')
OUT        <- file.path('resources', 'acs', 'puma_urban_density_2010.csv')

# 50 states + DC (ACS PUMS universe); Puerto Rico (72) out
STATES_51 <- setdiff(c(1:56), c(3, 7, 14, 43, 52))



extract_sf1_ur1_tracts <- function(zip_path) {

  #----------------------------------------------------------------------------
  # One state's SF1 Urban/Rural Update zip -> tract counts. Geo header
  # (fixed width): SUMLEV 9-11, GEOCOMP 12-13, LOGRECNO 19-25, STATE 28-29,
  # COUNTY 30-32, TRACT 55-60; tracts = SUMLEV 140, GEOCOMP 00. Segment 02
  # field 6-10 = P2 cells 1-5 (total, urban, in UA, in UC, rural); segment
  # 04 field 150 = P12 cell 1 (total), 152 = cell 3 (male < 5), 176 = cell 27
  # (female < 5). Fails if urban + rural != total or P12 total != P2 total.
  #
  # Params:
  #   - zip_path (chr): path to <st>2010.ur1.zip
  #
  # Returns: (tibble) statefip, countyfip (int), tractce (chr), pop, urban,
  #   rural, children_0_4 (num) -- one row per tract
  #----------------------------------------------------------------------------

  st <- sub('2010\\.ur1\\.zip$', '', basename(zip_path))
  tmp <- file.path(tempdir(), st)
  dir.create(tmp, showWarnings = FALSE)
  files <- paste0(st, c('geo2010.ur1', '000022010.ur1', '000042010.ur1'))
  utils::unzip(zip_path, files = files, exdir = tmp)
  g <- readLines(file.path(tmp, files[1]), encoding = 'latin1')
  geo <- tibble(sumlev = substr(g, 9, 11), geocomp = substr(g, 12, 13),
                logrecno = as.integer(substr(g, 19, 25)),
                statefip = as.integer(substr(g, 28, 29)),
                countyfip = as.integer(substr(g, 30, 32)),
                tractce = substr(g, 55, 60)) %>%
    filter(sumlev == '140', geocomp == '00') %>%
    select(-sumlev, -geocomp)
  s2 <- fread(file.path(tmp, files[2]), header = FALSE, select = c(5, 6, 7, 10),
              col.names = c('logrecno', 'pop', 'urban', 'rural'))
  s4 <- fread(file.path(tmp, files[3]), header = FALSE, select = c(5, 150, 152, 176),
              col.names = c('logrecno', 'p12_total', 'male_u5', 'female_u5'))
  unlink(tmp, recursive = TRUE)
  out <- geo %>%
    inner_join(tibble(s2), by = 'logrecno') %>%
    inner_join(tibble(s4), by = 'logrecno')
  if (nrow(out) != nrow(geo)) stop('extract_sf1_ur1_tracts: ', st, ' tract rows lost in segment join.')
  if (any(out$urban + out$rural != out$pop)) stop('extract_sf1_ur1_tracts: ', st, ' urban + rural != total.')
  if (any(out$p12_total != out$pop)) stop('extract_sf1_ur1_tracts: ', st, ' P12 total != P2 total -- segment layout off.')
  out %>%
    transmute(statefip, countyfip, tractce, pop = as.numeric(pop), urban = as.numeric(urban),
              rural = as.numeric(rural), children_0_4 = as.numeric(male_u5 + female_u5)) %>%
    arrange(countyfip, tractce)
}



read_tract_inputs <- function() {

  #----------------------------------------------------------------------------
  # Join tract counts, population centroids, land area, PUMA. Fails on any
  # tract missing from a file (all 2010 tract vintage).
  #
  # Params: none (reads RAW files)
  #
  # Returns: (tibble) geoid, statefip, puma, lat, lon, r_mi, pop, urban,
  #   children -- one row per 2010 tract, 50 states + DC
  #----------------------------------------------------------------------------

  cnt <- read_csv(TRACT_POP, col_types = cols(tractce = 'c', .default = 'd')) %>%
    mutate(geoid = sprintf('%02d%03d%s', as.integer(statefip), as.integer(countyfip), tractce))
  cen <- read_csv(CENTROIDS, col_types = cols(.default = 'c')) %>%
    transmute(geoid = paste0(STATEFP, COUNTYFP, TRACTCE), lat = as.numeric(LATITUDE),
              lon = as.numeric(LONGITUDE), cen_pop = as.numeric(POPULATION))
  gz <- read.delim(unz(GAZETTEER, 'Gaz_tracts_national.txt'), colClasses = 'character',
                   strip.white = TRUE) %>%
    transmute(geoid = GEOID, aland_sqmi = as.numeric(ALAND_SQMI))
  tp <- read_csv(TRACT_PUMA, col_types = cols(.default = 'c')) %>%
    transmute(geoid = paste0(STATEFP, COUNTYFP, TRACTCE), puma = as.integer(PUMA5CE))

  x <- cnt %>%
    filter(statefip %in% STATES_51) %>%
    left_join(cen, by = 'geoid') %>%
    left_join(gz, by = 'geoid') %>%
    left_join(tp, by = 'geoid')
  miss <- x %>% filter(is.na(lat) | is.na(aland_sqmi) | is.na(puma))
  if (nrow(miss) > 0) {
    stop(nrow(miss), ' SF1 tracts w/out centroid, Gazetteer area, or PUMA: ',
         paste(head(miss$geoid, 10), collapse = ', '), '. Check 2010 tract vintage of raw files.')
  }
  extra <- setdiff(cen$geoid[as.integer(substr(cen$geoid, 1, 2)) %in% STATES_51], x$geoid)
  if (length(extra) > 0) stop(length(extra), ' centroid tracts absent from SF1 extract: ',
                              paste(head(extra, 10), collapse = ', '), '.')
  if (any(x$cen_pop != x$pop)) stop('Centroid file population != SF1 P2 total for some tracts.')
  x %>%
    transmute(geoid, statefip = as.integer(statefip), puma, lat, lon,
              r_mi = sqrt(aland_sqmi / pi), pop, urban, children = children_0_4)
}



main <- function(args = commandArgs(TRUE)) {

  #----------------------------------------------------------------------------
  # `extract <dir>`: SF1 UR1 state zips -> TRACT_POP. Default: build OUT,
  # print summary.
  #
  # Params:
  #   - args (chr): command-line args
  #
  # Returns: nothing (writes TRACT_POP or OUT)
  #----------------------------------------------------------------------------

  if (length(args) >= 1 && args[1] == 'extract') {
    zips <- list.files(args[2], '2010\\.ur1\\.zip$', full.names = TRUE)
    cat('SF1 UR1 zips:', length(zips), '\n')
    out <- bind_rows(lapply(zips, function(z) { cat('  ', basename(z), '\n'); extract_sf1_ur1_tracts(z) })) %>%
      filter(statefip %in% STATES_51) %>%
      arrange(statefip, countyfip, tractce)
    if (!setequal(unique(out$statefip), STATES_51)) stop('extract: states missing from zips.')
    cat('Tracts:', nrow(out), ' pop:', sum(out$pop), ' urban share:', round(sum(out$urban) / sum(out$pop), 4),
        ' children 0-4:', sum(out$children_0_4), '\n')
    write_csv(out, TRACT_POP)
    cat('Wrote', TRACT_POP, '\n')
    return(invisible())
  }

  tr <- read_tract_inputs()
  sh <- tract_urban_share(tr)
  td <- tr %>% left_join(sh, by = 'geoid') %>%
    mutate(density = density_from_urban_share(urban_share))
  cat('Tracts:', nrow(td), ' w/out density (no population in community):', sum(is.na(td$density)),
      ' children in those:', sum(td$children[is.na(td$density)]), '\n')
  cat('Community size (tracts incl. anchor): median', median(td$n_scatter), ' max', max(td$n_scatter), '\n')
  cat('Children 0-4 by tract density (2010, pct):\n')
  print(round(100 * tapply(td$children, td$density, sum) / sum(td$children), 2))

  out <- aggregate_puma_density(td)
  cat('PUMAs:', nrow(out), ' all one level:', sum(pmax(out$share_1, out$share_2, out$share_3) == 1), '\n')
  write_csv(out %>% mutate(across(starts_with('share_'), ~ round(.x, 6))), OUT)
  cat('Wrote', OUT, '\n')
}


if (sys.nframe() == 0) main()
