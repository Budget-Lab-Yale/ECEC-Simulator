#------------------------------------------------------------------------------
# bls_laus_epop.R -- state employment-population ratios, BLS LAUS, 2006-2024.
#
# Writes resources/epop/US_BLS__laus_state_epop__2006-2024.csv: one row per
# state (and US) x year, the annual mean of the monthly seasonally adjusted
# employment-population ratio, ages 16+.
#
#   States: LASST{FIPS}0000000000007 (Local Area Unemployment Statistics)
#   US:     LNS12300000 (CPS) -- the national total LAUS state estimates are
#           controlled to
#
# WHY LAUS. state_epop_projections.R needs each state's change in position
# relative to the nation. LAUS is a model-based estimate that smooths CPS
# sampling noise, so a single year's ratio is reliable without pooling. It
# carries no sex or age detail, so the state adjustment is one number per
# state, applied to every employment group.
#
# Needs BLS_API_KEY in ~/.Renviron (see ~/.config/api-keys/REGISTRY.md). v2 API:
# 50 series and 20 years per request.
#
# Usage:
#   Rscript resources/epop/bls_laus_epop.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(httr); library(jsonlite)
})

source('src/shared_functions/constants.R')

OUT        <- file.path('resources', 'epop', 'US_BLS__laus_state_epop__2006-2024.csv')
YEARS      <- c(2006, 2024)
US_SERIES  <- 'LNS12300000'
API        <- 'https://api.bls.gov/publicAPI/v2/timeseries/data/'



laus_series <- function(fips) {

  #----------------------------------------------------------------------------
  # LAUS series ID for one state's seasonally adjusted epop ratio.
  #
  # Params:
  #   - fips (int): state FIPS
  #
  # Returns: (chr) series ID
  #----------------------------------------------------------------------------

  sprintf('LASST%02d0000000000007', as.integer(fips))
}



fetch_bls <- function(series, key) {

  #----------------------------------------------------------------------------
  # One BLS v2 request: monthly values for up to 50 series over YEARS.
  #
  # Params:
  #   - series (chr vec): series IDs, at most 50
  #   - key (chr): BLS API key
  #
  # Returns: (tibble) series_id, year, period, value
  #----------------------------------------------------------------------------

  body <- list(seriesid = series, startyear = as.character(YEARS[1]),
               endyear = as.character(YEARS[2]), registrationkey = key)
  r <- POST(API, body = toJSON(body, auto_unbox = TRUE),
            content_type_json(), timeout(120))
  stop_for_status(r)
  j <- fromJSON(content(r, as = 'text', encoding = 'UTF-8'), simplifyVector = FALSE)
  if (!identical(j$status, 'REQUEST_SUCCEEDED')) {
    stop('BLS API: ', j$status, ' -- ', paste(unlist(j$message), collapse = ' | '))
  }
  bind_rows(lapply(j$Results$series, function(s) {
    bind_rows(lapply(s$data, function(d) tibble(
      series_id = s$seriesID, year = as.integer(d$year),
      period = d$period, value = as.numeric(d$value))))
  }))
}



main <- function() {

  #----------------------------------------------------------------------------
  # Fetches every state and the US, averages months to years, writes OUT.
  #
  # Returns: (tibble) written rows, invisibly
  #----------------------------------------------------------------------------

  key <- Sys.getenv('BLS_API_KEY')
  if (key == '') stop('BLS_API_KEY is not set -- add it to ~/.Renviron.')

  ids <- c(setNames(vapply(STATE_POSTAL_TO_FIPS, laus_series, ''),
                    names(STATE_POSTAL_TO_FIPS)),
           US = US_SERIES)
  chunks <- split(unname(ids), ceiling(seq_along(ids) / 50))
  raw <- bind_rows(lapply(chunks, fetch_bls, key = key))

  missing <- setdiff(ids, unique(raw$series_id))
  if (length(missing)) stop('BLS returned no data for: ', paste(missing, collapse = ', '))

  out <- raw %>%
    filter(grepl('^M(0[1-9]|1[0-2])$', period)) %>%
    group_by(series_id, year) %>%
    summarise(n_months = n(), epop = mean(value), .groups = 'drop') %>%
    mutate(geo = names(ids)[match(series_id, ids)]) %>%
    select(geo, series_id, year, n_months, epop) %>%
    arrange(geo, year)

  short <- out %>% filter(n_months != 12)
  if (nrow(short)) stop('incomplete years: ', paste(short$geo, short$year, collapse = ', '))

  write_csv(out, OUT)
  cat('wrote ', OUT, ': ', n_distinct(out$geo), ' geographies, ',
      paste(range(out$year), collapse = '-'), '\n', sep = '')
  invisible(out)
}



if (sys.nframe() == 0) main()
