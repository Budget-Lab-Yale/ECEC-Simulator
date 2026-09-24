#------------------------------------------------------------------------------
# 04_head_start_in_schools.R -- Head Start places a school district runs in
# one of its own pre-K schools.
#
# Step 3 adds CCD pre-K + Head Start + Early Head Start. A Head Start class a
# district runs in its own school can sit in that school's CCD pre-K
# membership AND in the federal Head Start count -- counted twice. This
# measures that overlap from national files only, so every state is treated
# alike:
#
#   ACF Head Start service locations (every open site, funded slots, grantee
#   and operator names) x CCD pre-K membership by school x CCD school
#   directory (district name) x EDGE school geocodes.
#
# A site's slots count as overlap when ALL THREE hold:
#   1. site at a school reporting pre-K -- normalized street address + ZIP
#      match, OR geocodes within MATCH_METRES. Neither needs exact strings.
#   2. operator (or grantee) is a school district by name -- LEA_PATTERN.
#   3. operator is THAT school's district -- shares a distinctive place-name
#      word w/ the CCD district name, generic words dropped. Tolerant of
#      renames between CCD 2018-19 and the 2026 site file ("Prince William
#      County School District" vs "Prince William County Public Schools").
#
# Test 3 is the one that binds: an address match alone only bounds the
# overlap -- a grantee renting a school classroom runs a separate program
# whose children never enter the school's membership. Test 2 stops a county
# community action agency ("Pike County Head Start") matching a county
# district on place name alone.
#
# The site file is a 2026 export; its share is applied to the 2018-19 NIEER
# Head Start count, assuming the share held.
#
# Writes out/head_start_in_schools.csv (one row per geography) and
# out/head_start_in_schools_review.csv (every site that passes test 2, and the
# decision on it).
#
# Usage:
#   Rscript state_data/04_head_start_in_schools.R
#------------------------------------------------------------------------------

source('state_data/00_main.R')     # main() guarded; GEOGRAPHIES, load_spec()

HSSL_FILE     <- 'US_HSSL__head_start_funded_slots_by_service_location__2026-09-23.csv'
MATCH_METRES  <- 150   # campus scale; 100-400 m is a plateau, see memo
HS_TYPES      <- c('1', '3', '5')  # Head Start, AIAN HS, MSHS -- as NIEER's count

LEA_PATTERN <- paste0(
  'school district|public schools|\\bschools\\b|board of education|\\bisd\\b|',
  '\\busd\\b|unified|school board|department of education|school system|',
  'independent school|school corporation|\\bsd\\b|\\bcsd\\b')

# Generic words -- dropped before comparing operator and district names.
GENERIC <- c('THE', 'OF', 'AND', 'PUBLIC', 'SCHOOL', 'SCHOOLS', 'DISTRICT',
             'DISTRICTS', 'BOARD', 'EDUCATION', 'COUNTY', 'CITY', 'TOWN',
             'TOWNSHIP', 'INDEPENDENT', 'UNIFIED', 'CONSOLIDATED', 'COMMUNITY',
             'AREA', 'REGIONAL', 'CENTRAL', 'UNION', 'ISD', 'USD', 'SD', 'CSD',
             'HEAD', 'START', 'PROGRAM', 'PROGRAMS', 'EARLY', 'CHILDHOOD',
             'CHILD', 'DEVELOPMENT', 'CORPORATION', 'SYSTEM', 'OFFICE',
             'DEPARTMENT', 'NO', 'INC', 'DBA', 'PRESCHOOL', 'PRE', 'K')

NYC_HSSL_COUNTIES <- c('Bronx County', 'Kings County', 'New York County',
                       'Queens County', 'Richmond County')
NYC_FIPS          <- c('36005', '36047', '36061', '36081', '36085')

SUFFIX <- paste0('\\b(STREET|ST|AVENUE|AVE|ROAD|RD|DRIVE|DR|LANE|LN|BOULEVARD|',
                 'BLVD|COURT|CT|PLACE|PL|HIGHWAY|HWY|PARKWAY|PKWY|CIRCLE|CIR|',
                 'TERRACE|TER|WAY|SUITE|STE|UNIT|APT)\\b')



ORDINALS <- c(FIRST = '1', SECOND = '2', THIRD = '3', FOURTH = '4',
              FIFTH = '5', SIXTH = '6', SEVENTH = '7', EIGHTH = '8',
              NINTH = '9', TENTH = '10', ELEVENTH = '11', TWELFTH = '12',
              THIRTEENTH = '13', FOURTEENTH = '14', FIFTEENTH = '15',
              SIXTEENTH = '16', SEVENTEENTH = '17', EIGHTEENTH = '18',
              NINETEENTH = '19', TWENTIETH = '20')

address_key <- function(street, zip) {

  #----------------------------------------------------------------------------
  # ZIP5 + house number + first distinctive street word.
  #
  # TWO FILES, TWO SPELLINGS OF THE SAME ADDRESS. Normalizes what differs
  # between the ACF site file and CCD: street suffixes (Ave / Avenue),
  # compass words (E / East), ordinals written out ("359 Thirteenth Ave" in
  # CCD is "359 13th Ave" in the site file), and route words (ST RT / STATE
  # ROUTE / HIGHWAY / US HWY). The number stays -- 31208 and 31230
  # Constitution Highway are different buildings, and only the geocode
  # match can join them.
  #
  # Params:
  #   - street (chr vec), zip (chr vec)
  #
  # Returns: (chr vec) key, NA where no house number or ZIP
  #----------------------------------------------------------------------------

  s <- toupper(ifelse(is.na(street), '', street))
  s <- gsub('[^A-Z0-9 ]', ' ', s)
  s <- gsub('\\b([0-9]+)(ST|ND|RD|TH)\\b', '\\1', s)          # 13TH -> 13
  for (w in names(ORDINALS)) {                                 # THIRTEENTH -> 13
    s <- gsub(paste0('\\b', w, '\\b'), ORDINALS[[w]], s)
  }
  s <- gsub('\\b(US|STATE|ST)\\s+(RT|RTE|ROUTE|HWY|HIGHWAY)\\b', 'ROUTE', s)
  s <- gsub('\\b(RT|RTE|HWY)\\b', 'ROUTE', s)
  s <- gsub(SUFFIX, ' ', s)
  s <- gsub('\\b(N|S|E|W|NE|NW|SE|SW|NORTH|SOUTH|EAST|WEST)\\b', ' ', s)
  s <- gsub(' +', ' ', trimws(s))
  nm <- sub('^([0-9]+).*$', '\\1', s)
  nm[!grepl('^[0-9]+$', nm)] <- NA
  wd <- sub('^([A-Z0-9]+).*$', '\\1', sub('^[0-9]+ *', '', s))
  z5 <- substr(gsub('[^0-9]', '', ifelse(is.na(zip), '', zip)), 1, 5)
  key <- paste(z5, nm, wd)
  key[is.na(nm) | wd == '' | nchar(z5) != 5] <- NA
  key
}



name_words <- function(x) {

  #----------------------------------------------------------------------------
  # Distinctive words of an organisation name -- upper case, punctuation and
  # GENERIC words dropped, district codes like R2J / #2 / 50-J dropped.
  #
  # Params:
  #   - x (chr): one name
  #
  # Returns: (chr vec) words
  #----------------------------------------------------------------------------

  w <- strsplit(gsub(' +', ' ', trimws(gsub('[^A-Z ]', ' ', toupper(x)))), ' ')[[1]]
  w <- w[nchar(w) >= 3 & !(w %in% GENERIC)]
  unique(w)
}



same_district <- function(operator, lea_name) {

  #----------------------------------------------------------------------------
  # Test 3. TRUE when operator and district share a distinctive word and one
  # name's distinctive words are a subset of the other's -- tolerant of
  # generic-word changes, not of a different place.
  #
  # Params:
  #   - operator (chr), lea_name (chr)
  #
  # Returns: (lgl)
  #----------------------------------------------------------------------------

  a <- name_words(operator); b <- name_words(lea_name)
  length(a) > 0 && length(b) > 0 && (all(a %in% b) || all(b %in% a))
}



metres <- function(lat1, lon1, lat2, lon2) {
  r <- 6371000; p <- pi / 180
  h <- sin((lat2 - lat1) * p / 2)^2 +
       cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * r * asin(pmin(1, sqrt(h)))
}



load_schools <- function() {

  #----------------------------------------------------------------------------
  # Every school reporting pre-K in 2018-19, w/ address, geocode, district.
  #
  # Returns: (tibble) NCESSCH, ST, pk, SCH_NAME, LEA_NAME, key, lat, lon, CNTY
  #----------------------------------------------------------------------------

  pk <- read_chr(raw_path('US_CCD__public_school_prekindergarten_children_by_school__2018-19.csv')) %>%
    dplyr::mutate(pk = num(STUDENT_COUNT)) %>%
    dplyr::filter(!is.na(pk), pk > 0) %>%
    dplyr::select(NCESSCH, ST, pk)
  dir <- read_chr(raw_path('US_CCD__public_school_directory__2018-19.csv')) %>%
    dplyr::select(NCESSCH, SCH_NAME, LEA_NAME, LSTREET1, LZIP)
  geo <- read_chr(raw_path('US_EDGE__public_school_geocodes__2018-19.csv')) %>%
    dplyr::select(NCESSCH, STREET, ZIP, LAT, LON, CNTY)

  s <- pk %>% dplyr::left_join(dir, by = 'NCESSCH') %>%
    dplyr::left_join(geo, by = 'NCESSCH')
  # Two address sources, either may match: the CCD location address and the
  # EDGE geocoded street.
  s$key1 <- address_key(s$LSTREET1, s$LZIP)
  s$key2 <- address_key(s$STREET, s$ZIP)
  s$lat  <- num(s$LAT); s$lon <- num(s$LON)
  s
}



load_sites <- function() {

  #----------------------------------------------------------------------------
  # Open Head Start sites (HS_TYPES), w/ slots, operator, grantee, key.
  #
  # Returns: (tibble)
  #----------------------------------------------------------------------------

  h <- read_chr(raw_path(HSSL_FILE))
  h <- h[h$status == 'Open' & h$program_type %in% HS_TYPES, ]
  h$slots    <- num(h$funded_slots)
  h$lat      <- num(h$latitude); h$lon <- num(h$longitude)
  h$key      <- address_key(h$address_line_one, h$zip)
  # recipient_name is a slug ('murray_public_schools_ky') -- restore spaces.
  h$grantee  <- sub(' [a-z]{2}$', '', gsub('_', ' ', h$recipient_name))
  h$operator <- ifelse(is.na(h$program_admin_name) | h$program_admin_name == '',
                       h$grantee, h$program_admin_name)
  h$lea_named <- grepl(LEA_PATTERN, h$operator, ignore.case = TRUE) |
                 grepl(LEA_PATTERN, h$grantee,  ignore.case = TRUE)
  h
}



match_geo <- function(geo, sites, schools) {

  #----------------------------------------------------------------------------
  # All three tests for one geography.
  #
  # Params:
  #   - geo (chr), sites (tibble), schools (tibble)
  #
  # Returns: (list) summary (1-row tibble), review (tibble)
  #----------------------------------------------------------------------------

  spec <- load_spec(geo)
  st   <- STATE_POSTAL_OF[[as.character(spec$fips)]]
  h <- sites[sites$state == st, ]
  s <- schools[schools$ST == st, ]
  if (!is.null(spec$counties)) {
    h <- h[h$county %in% NYC_HSSL_COUNTIES, ]
    s <- s[s$CNTY %in% NYC_FIPS, ]
  }

  review <- NULL
  counted <- logical(nrow(h))
  for (i in which(h$lea_named)) {
    # test 1 -- address key on either school address, or geocode distance
    at <- which((!is.na(h$key[i]) & (s$key1 %in% h$key[i] | s$key2 %in% h$key[i])))
    d  <- if (is.finite(h$lat[i]) && is.finite(h$lon[i])) {
      metres(h$lat[i], h$lon[i], s$lat, s$lon)
    } else {
      rep(NA_real_, nrow(s))
    }
    near <- which(!is.na(d) & d <= MATCH_METRES)
    cand <- union(at, near)
    # test 3 -- operator or grantee is the candidate school's district
    own <- cand[vapply(cand, function(j)
      same_district(h$operator[i], s$LEA_NAME[j]) ||
      same_district(h$grantee[i],  s$LEA_NAME[j]), logical(1))]
    counted[i] <- length(own) > 0
    j <- if (length(own)) own[1] else if (length(cand)) cand[1] else NA
    review <- rbind(review, data.frame(
      geo = geo, site = h$service_location_name[i], slots = h$slots[i],
      operator = h$operator[i],
      school = if (is.na(j)) '' else s$SCH_NAME[j],
      school_district = if (is.na(j)) '' else s$LEA_NAME[j],
      metres = if (is.na(j)) NA_real_ else round(d[j]),
      address_match = !is.na(j) && j %in% at,
      decision = if (counted[i]) 'counted: own district school'
                 else if (length(cand)) 'not counted: school of another district'
                 else 'not counted: not at a pre-K school',
      stringsAsFactors = FALSE))
  }

  tot <- sum(h$slots, na.rm = TRUE)
  cnt <- sum(h$slots[counted], na.rm = TRUE)
  list(summary = tibble::tibble(
         geo = geo, sites = nrow(h), hs_slots = tot,
         lea_named_slots = sum(h$slots[h$lea_named], na.rm = TRUE),
         in_own_school_slots = cnt, share = if (tot > 0) cnt / tot else 0),
       review = review)
}



# FIPS -> postal, from the simulator's own table.
source('src/shared_functions/constants.R')
STATE_POSTAL_OF <- setNames(names(STATE_POSTAL_TO_FIPS),
                            as.character(STATE_POSTAL_TO_FIPS))

if (sys.nframe() == 0) {
  args    <- commandArgs(trailingOnly = TRUE)
  schools <- load_schools()
  sites   <- load_sites()
  cat('pre-K schools ', nrow(schools), ', open Head Start sites ', nrow(sites),
      '\n', sep = '')

  res     <- lapply(GEOGRAPHIES, match_geo, sites = sites, schools = schools)
  summary <- dplyr::bind_rows(lapply(res, `[[`, 'summary'))
  review  <- dplyr::bind_rows(lapply(res, `[[`, 'review'))

  readr::write_csv(summary, file.path(STATE_DATA_ROOT, 'out', 'head_start_in_schools.csv'))
  readr::write_csv(review,  file.path(STATE_DATA_ROOT, 'out', 'head_start_in_schools_review.csv'))
  cat('\nHead Start slots a district runs in its own pre-K school\n\n')
  print(as.data.frame(summary %>% dplyr::mutate(share = round(100 * share, 1))),
        row.names = FALSE)

  ## --check: does states/<GEO>.R carry what this script derives? -------------
  if ('--check' %in% args) {
    cat('\nchecking hs_in_schools_share in states/<GEO>.R\n')
    fails <- 0
    for (g in GEOGRAPHIES) {
      want <- summary$share[summary$geo == g]
      got  <- load_spec(g)$hs_in_schools_share
      ok   <- is.numeric(got) && abs(got - want) < 1e-12
      cat(sprintf('   %-4s %s  %.4f\n', g, if (ok) '[ok]  ' else '[FAIL]', want))
      if (!ok) cat(sprintf('        file %s, derived %.6f\n',
                           format(got %||% NA), want))
      fails <- fails + !ok
    }
    cat('\n', if (fails == 0) 'SHARES MATCH THE MATCH' else
        paste(fails, 'SHARE(S) STALE'), '\n')
    if (fails > 0) quit(status = 1)
  }
}
