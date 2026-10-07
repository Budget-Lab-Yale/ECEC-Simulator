#------------------------------------------------------------------------------
# 03_head_start_school_overlap.R -- Head Start places a school district runs in
# one of its own pre-K schools.
#
# Step 4 adds CCD pre-K + Head Start + EHS. A Head Start class a district runs
# in its own school sits in CCD pre-K membership AND federal Head Start count
# -- counted twice. Measured from national files only, every state alike: ACF
# Head Start service locations (open sites, funded slots, grantee, operator)
# x CCD pre-K by school x CCD directory (district) x EDGE geocodes.
#
# Site's slots count as overlap when ALL THREE hold:
#   1. at a pre-K school -- normalized address + ZIP match, OR geocodes within
#      MATCH_METRES.
#   2. operator (or grantee) a school district by name -- LEA_PATTERN. Stops a
#      county community action agency ("Pike County Head Start") matching a
#      county district on place name alone.
#   3. operator THAT school's district -- shared distinctive name word,
#      generic words dropped; tolerant of renames between CCD 2018-19 and 2026
#      site file ("Prince William County School District" vs "... Public
#      Schools"). Binding test: grantee renting a school classroom runs a
#      separate program, children never in school's membership.
#
# Site file a 2026 export; share applied to 2018-19 Head Start children in
# centers, ages 0-4 (NIEER slots x HS_ENROLLED_PER_SLOT, less home-based, FCC,
# 5+ shares -- program_children(), 00_build_demand_targets.R), assuming share
# held. program_children() deducts this share, except where spec sets
# hs_in_prek (MI, DC) -- NIEER share replaces it there.
#
# GAP: site file lists only OPEN sites in 2026. A district grantee closed or
# 'Not Reported' by then drops out -- DC: DC Public Schools 250 slots 'Not
# Reported', 114 open slots in all vs NIEER 2,150, so DC share reads 0 while
# NIEER Table 4 puts all 2,150 in DC pre-K.
#
# Writes out/head_start_school_overlap.csv (one row per geography; load_spec()
# reads hs_in_schools_share live from it).
#
# Usage:
#   Rscript state_data/code/03_head_start_school_overlap.R
#------------------------------------------------------------------------------

# main() guarded; GEOGRAPHIES, load_spec()
source('state_data/code/00_build_demand_targets.R')

HSSL_FILE     <- 'US_HSSL__head_start_funded_slots_by_service_location__2026-09-23.csv'
MATCH_METRES  <- 150   # campus scale; 100-400 m is a plateau, see memo
HS_TYPES      <- c('1', '3', '5')  # Head Start, AIAN HS, MSHS -- as NIEER's count

# FIPS -> postal, from simulator's own table (constants.R, sourced by
# 00_build_demand_targets.R).
STATE_POSTAL_OF <- setNames(names(STATE_POSTAL_TO_FIPS),
                            as.character(STATE_POSTAL_TO_FIPS))

LEA_PATTERN <- paste0(
  'school district|public schools|\\bschools\\b|board of education|\\bisd\\b|',
  '\\busd\\b|unified|school board|department of education|school system|',
  'independent school|school corporation|\\bsd\\b|\\bcsd\\b')

# Generic words -- dropped before comparing operator, district names.
GENERIC <- c('THE', 'OF', 'AND', 'PUBLIC', 'SCHOOL', 'SCHOOLS', 'DISTRICT',
             'DISTRICTS', 'BOARD', 'EDUCATION', 'COUNTY', 'CITY', 'TOWN',
             'TOWNSHIP', 'INDEPENDENT', 'UNIFIED', 'CONSOLIDATED', 'COMMUNITY',
             'AREA', 'REGIONAL', 'CENTRAL', 'UNION', 'ISD', 'USD', 'SD', 'CSD',
             'HEAD', 'START', 'PROGRAM', 'PROGRAMS', 'EARLY', 'CHILDHOOD',
             'CHILD', 'DEVELOPMENT', 'CORPORATION', 'SYSTEM', 'OFFICE',
             'DEPARTMENT', 'NO', 'INC', 'DBA', 'PRESCHOOL', 'PRE', 'K')

# Street suffixes, unit words -- dropped from address keys.
SUFFIX <- paste0('\\b(STREET|ST|AVENUE|AVE|ROAD|RD|DRIVE|DR|LANE|LN|BOULEVARD|',
                 'BLVD|COURT|CT|PLACE|PL|HIGHWAY|HWY|PARKWAY|PKWY|CIRCLE|CIR|',
                 'TERRACE|TER|WAY|SUITE|STE|UNIT|APT)\\b')

# Ordinals written out -> digits.
ORDINALS <- setNames(as.character(1:20), c(
  'FIRST', 'SECOND', 'THIRD', 'FOURTH', 'FIFTH', 'SIXTH', 'SEVENTH', 'EIGHTH',
  'NINTH', 'TENTH', 'ELEVENTH', 'TWELFTH', 'THIRTEENTH', 'FOURTEENTH',
  'FIFTEENTH', 'SIXTEENTH', 'SEVENTEENTH', 'EIGHTEENTH', 'NINETEENTH',
  'TWENTIETH'))



address_key <- function(street, zip) {

  #----------------------------------------------------------------------------
  # ZIP5 + house number + first distinctive street word. Normalizes how ACF
  # site file, CCD spell same address differently: suffixes (Ave / Avenue),
  # compass words (E / East), ordinals ("359 Thirteenth Ave" in CCD is "359
  # 13th Ave" in site file), route words (ST RT / STATE ROUTE / HIGHWAY / US
  # HWY). Number stays -- 31208, 31230 Constitution Highway are different
  # buildings, only geocode match can join them.
  #
  # Params:
  #   - street (chr vec), zip (chr vec)
  #
  # Returns: (chr vec) NA where no house number, street word, or ZIP5
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
  # Distinctive words of one organisation name, upper case -- punctuation,
  # GENERIC words, words under 3 letters, district codes (R2J / #2 / 50-J)
  # dropped.
  #
  # Params:
  #   - x (chr): one name
  #
  # Returns: (chr vec)
  #----------------------------------------------------------------------------

  w <- strsplit(gsub(' +', ' ', trimws(gsub('[^A-Z ]', ' ', toupper(x)))), ' ')[[1]]
  unique(w[nchar(w) >= 3 & !(w %in% GENERIC)])
}



same_district <- function(operator, lea_name) {

  #----------------------------------------------------------------------------
  # Test 3: one name's distinctive words a non-empty subset of other's --
  # tolerant of generic-word changes, not of a different place.
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

  #----------------------------------------------------------------------------
  # Haversine distance, earth radius 6,371,000 m. Vectorised.
  #
  # Params:
  #   - lat1, lon1, lat2, lon2 (num): decimal degrees
  #
  # Returns: (num) metres
  #----------------------------------------------------------------------------

  r <- 6371000; p <- pi / 180
  h <- sin((lat2 - lat1) * p / 2)^2 +
       cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * r * asin(pmin(1, sqrt(h)))
}



load_schools <- function() {

  #----------------------------------------------------------------------------
  # Every school reporting pre-K in 2018-19, w/ district, geocode, two
  # address keys -- CCD location address (key1), EDGE geocoded street (key2);
  # either may match.
  #
  # Returns: (tibble) NCESSCH, ST, pk, LEA_NAME, LSTREET1, LZIP,
  #   STREET, ZIP, LAT, LON, CNTY, key1, key2, lat, lon
  #----------------------------------------------------------------------------

  pk <- read_chr(raw_path('US_CCD__public_school_prekindergarten_children_by_school__2018-19.csv')) %>%
    dplyr::mutate(pk = num(STUDENT_COUNT)) %>%
    dplyr::filter(!is.na(pk), pk > 0) %>%
    dplyr::select(NCESSCH, ST, pk)
  dir <- read_chr(raw_path('US_CCD__public_school_directory__2018-19.csv')) %>%
    dplyr::select(NCESSCH, LEA_NAME, LSTREET1, LZIP)
  geo <- read_chr(raw_path('US_EDGE__public_school_geocodes__2018-19.csv')) %>%
    dplyr::select(NCESSCH, STREET, ZIP, LAT, LON, CNTY)

  pk %>%
    dplyr::left_join(dir, by = 'NCESSCH') %>%
    dplyr::left_join(geo, by = 'NCESSCH') %>%
    dplyr::mutate(key1 = address_key(LSTREET1, LZIP),
                  key2 = address_key(STREET, ZIP),
                  lat  = num(LAT),
                  lon  = num(LON))
}



load_sites <- function() {

  #----------------------------------------------------------------------------
  # Open Head Start sites of HS_TYPES. Operator = program admin, else grantee.
  #
  # Returns: (tibble) site file + slots, lat, lon, key, grantee, operator,
  #   lea_named (test 2)
  #----------------------------------------------------------------------------

  read_chr(raw_path(HSSL_FILE)) %>%
    dplyr::filter(status == 'Open', program_type %in% HS_TYPES) %>%
    dplyr::mutate(
      slots   = num(funded_slots),
      lat     = num(latitude),
      lon     = num(longitude),
      key     = address_key(address_line_one, zip),
      # recipient_name a slug ('murray_public_schools_ky') -- restore spaces.
      grantee  = sub(' [a-z]{2}$', '', gsub('_', ' ', recipient_name)),
      operator = ifelse(is.na(program_admin_name) | program_admin_name == '',
                        grantee, program_admin_name),
      lea_named = grepl(LEA_PATTERN, operator, ignore.case = TRUE) |
                  grepl(LEA_PATTERN, grantee,  ignore.case = TRUE))
}



match_site <- function(site, s) {

  #----------------------------------------------------------------------------
  # Tests 1, 3 for one site passing test 2.
  #
  # Params:
  #   - site (tibble): one load_sites() row
  #   - s (tibble): geography's load_schools() rows
  #
  # Returns: (lgl) TRUE when site counts as overlap
  #----------------------------------------------------------------------------

  # test 1 -- address key on either school address, or geocode distance
  at <- which(!is.na(site$key) & (s$key1 %in% site$key | s$key2 %in% site$key))
  d  <- if (is.finite(site$lat) && is.finite(site$lon)) {
    metres(site$lat, site$lon, s$lat, s$lon)
  } else {
    rep(NA_real_, nrow(s))
  }
  near <- which(!is.na(d) & d <= MATCH_METRES)
  cand <- union(at, near)

  # test 3 -- operator or grantee is candidate school's district
  own <- cand[vapply(cand, function(j)
    same_district(site$operator, s$LEA_NAME[j]) ||
    same_district(site$grantee,  s$LEA_NAME[j]), logical(1))]
  length(own) > 0
}



match_geo <- function(geo, sites, schools) {

  #----------------------------------------------------------------------------
  # All three tests for one geography.
  #
  # Params:
  #   - geo (chr), sites (tibble), schools (tibble)
  #
  # Returns: (tibble) one summary row
  #----------------------------------------------------------------------------

  spec <- load_spec(geo)
  st   <- STATE_POSTAL_OF[[as.character(spec$fips)]]
  h <- dplyr::filter(sites,   state == st)
  s <- dplyr::filter(schools, ST == st)
  if (!is.null(spec$counties)) {
    # Sub-state geography: site file names counties '<name> County', CCD by FIPS.
    h <- dplyr::filter(h, county %in% paste0(spec$counties, ' County'))
    s <- dplyr::filter(s, CNTY %in% spec$county_fips)
  }

  idx <- which(h$lea_named)
  counted <- logical(nrow(h))
  counted[idx] <- vapply(idx, function(i) match_site(h[i, ], s), logical(1))

  tot <- sum(h$slots, na.rm = TRUE)
  cnt <- sum(h$slots[counted], na.rm = TRUE)
  tibble::tibble(
    geo = geo, sites = nrow(h), hs_slots = tot,
    lea_named_slots = sum(h$slots[h$lea_named], na.rm = TRUE),
    in_own_school_slots = cnt, share = if (tot > 0) cnt / tot else 0)
}



if (sys.nframe() == 0) {
  schools <- load_schools()
  sites   <- load_sites()
  cat('pre-K schools ', nrow(schools), ', open Head Start sites ', nrow(sites),
      '\n', sep = '')

  summary <- dplyr::bind_rows(lapply(GEOGRAPHIES, match_geo, sites = sites,
                                      schools = schools))

  readr::write_csv(summary, file.path(STATE_DATA_ROOT, 'out',
                                      'head_start_school_overlap.csv'))
  cat('\nHead Start slots a district runs in its own pre-K school\n\n')
  show_df(summary %>% dplyr::mutate(share = round(100 * share, 1)))
}
