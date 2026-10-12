#------------------------------------------------------------------------------
# income_rank.R
#
# Income-rank scheme for care-choice forest (1b) + price QRF / wedge cache
# (1a, compute_price_wedge_cache.R). One scheme for training (NSECE) and
# prediction (ACS). Set by env var ECEC_INCOME_RANK (config.R ->
# income_rank_scheme):
#   - state (default): NSECE within Census region, ACS within state (NYC in NYS)
#   - region: NSECE, ACS both within Census region
#   - national: NSECE, ACS both ranked nationally
#   - national_rpp: income / price level (RPP / 100), ranked nationally.
#     ACS: state RPP (BEA all items 2019; NYC = NY state). NSECE: region RPP =
#     ACS under-5 child-weighted mean of state RPPs in region
#
# Called from: 1a (price QRF), 1b (care-choice forest), compute_price_wedge_cache.R
#------------------------------------------------------------------------------



INCOME_RANK_SCHEMES <- c('state', 'region', 'national', 'national_rpp')

INCOME_RANK_RPP_PATH <- file.path('resources', 'supply',
                                  'US_BEA__regional_price_parities_all_items__2019.csv')



get_income_rank_scheme <- function() {

  #----------------------------------------------------------------------------
  # Active income-rank scheme -- global income_rank_scheme (config.R), else
  # env var ECEC_INCOME_RANK, else 'state'.
  #
  # Returns: (chr) one of INCOME_RANK_SCHEMES
  #----------------------------------------------------------------------------

  s <- if (exists('income_rank_scheme', envir = globalenv())) {
    get('income_rank_scheme', envir = globalenv())
  } else {
    Sys.getenv('ECEC_INCOME_RANK', 'state')
  }
  if (is.null(s) || s == '') s <- 'state'
  if (!s %in% INCOME_RANK_SCHEMES) {
    stop('Income-rank scheme "', s, '" unknown. Set ECEC_INCOME_RANK to one of: ',
         paste(INCOME_RANK_SCHEMES, collapse = ', '), '.')
  }
  s
}



get_state_rpp <- function() {

  #----------------------------------------------------------------------------
  # State RPP (all items 2019, US = 100) keyed by statefip.
  #
  # Returns: (num vec) named by statefip (chr)
  #----------------------------------------------------------------------------

  if (!file.exists(INCOME_RANK_RPP_PATH)) {
    stop('RPP file missing: ', INCOME_RANK_RPP_PATH, '. Restore with ',
         '`git show f893f6f~1:', INCOME_RANK_RPP_PATH, '`.')
  }
  rpp <- utils::read.csv(INCOME_RANK_RPP_PATH, stringsAsFactors = FALSE)
  rpp <- rpp[rpp$bea_geography == 'state', ]
  fips <- STATE_POSTAL_TO_FIPS[rpp$geo]
  if (anyNA(fips) || length(fips) != 51) {
    stop('RPP file: expected 50 states + DC, got ', sum(!is.na(fips)), '.')
  }
  setNames(rpp$rpp_all_items_2019, as.character(fips))
}



get_region_rpp <- function() {

  #----------------------------------------------------------------------------
  # Region RPP = ACS under-5 child-weighted (PERWT, 2019, households GQ 1-2)
  # mean of state RPPs in each Census region. Reads 4 cols of raw ACS --
  # NSECE (1a) runs before ACS processing. Memoised in global env.
  #
  # Returns: (num vec) named by region '1'-'4'
  #----------------------------------------------------------------------------

  if (exists('.income_rank_region_rpp', envir = globalenv())) {
    return(get('.income_rank_region_rpp', envir = globalenv()))
  }
  acs_raw_path <- file.path(estimation_info$paths$ACS, 'ipums_usa.csv')
  kids <- data.table::fread(acs_raw_path,
                            select = c('YEAR', 'GQ', 'STATEFIP', 'REGION', 'AGE', 'PERWT'))
  kids <- kids[kids$YEAR == 2019 & kids$GQ %in% c(1, 2) & kids$AGE < 5, ]
  state_rpp <- get_state_rpp()
  kids$rpp <- state_rpp[as.character(kids$STATEFIP)]
  if (anyNA(kids$rpp)) stop('Region RPP: ACS children w/ statefip lacking RPP.')
  kids$region <- kids$REGION %/% 10L
  out <- sapply(split(kids, kids$region), function(d) sum(d$rpp * d$PERWT) / sum(d$PERWT))
  cat('  Income rank: region RPP (child-weighted) ',
      paste(names(out), round(out, 2), sep = '=', collapse = ', '), '\n')
  assign('.income_rank_region_rpp', out, envir = globalenv())
  out
}



income_rank_inputs <- function(income, region, statefip, source) {

  #----------------------------------------------------------------------------
  # Income to rank + group to rank within, per active scheme.
  #
  # Params:
  #   - income (num vec): income (HHINCOME, NSECE income, baseline AGI)
  #   - region (vec): Census region 1-4, same length
  #   - statefip (vec): state FIPS (ACS); NULL / ignored for NSECE
  #   - source (chr): 'nsece' or 'acs'
  #
  # Returns: (list) income (num vec, RPP-deflated under national_rpp), group
  #   (int vec -- region, statefip, or 0 = one national group)
  #----------------------------------------------------------------------------

  scheme <- get_income_rank_scheme()
  n <- length(income)
  if (scheme == 'state') {
    group <- if (source == 'nsece') region else statefip
    return(list(income = income, group = group))
  }
  if (scheme == 'region') return(list(income = income, group = region))
  if (scheme == 'national') return(list(income = income, group = rep(0L, n)))

  # national_rpp
  rpp <- if (source == 'nsece') {
    get_region_rpp()[as.character(region)]
  } else {
    get_state_rpp()[as.character(statefip)]
  }
  if (anyNA(rpp[!is.na(income)])) {
    stop('income_rank_inputs: ', sum(is.na(rpp) & !is.na(income)), ' ', source,
         ' records lack RPP (region / statefip NA).')
  }
  list(income = income / (unname(rpp) / 100), group = rep(0L, n))
}
