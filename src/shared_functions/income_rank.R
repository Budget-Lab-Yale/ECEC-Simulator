#------------------------------------------------------------------------------
# income_rank.R
#
# Income-rank scheme for care-choice forest (1b), price wedge income-quintile
# grids (center_low, other_paid: 1a builds on NSECE ranks,
# price_wedge_income_pctiles() ranks ACS -- compute_price_wedge_cache.R),
# 1a price-by-income diagnostic. One scheme for training (NSECE) and
# prediction (ACS). Set by env var ECEC_INCOME_RANK (config.R ->
# income_rank_scheme). Both rank NSECE, ACS nationally (one group):
#   - national_rpp (default): income / price level (RPP / 100).
#     ACS: state RPP (BEA all items 2019; NYC = NY state). NSECE: region RPP =
#     ACS under-5 child-weighted mean of state RPPs in region
#   - national: nominal income (sensitivity)
# Within-state / within-region schemes ('state', 'region') removed 2026-10-12
# -- ranking test found them indistinguishable from national_rpp.
#
# Sub-state geography (SUBSTATE_GEOGRAPHIES, constants.R) ranked twice:
# national uses + parent-state run rank it at parent-state RPP (above); its
# own run gets substate_rank_inputs() -- national_rpp: geo's own RPP row (NYC
# metro, msa_35620) vs national cutpoints; national: versions identical,
# second skipped.
#
# Called from: 1a (price wedge quintile grids, price-by-income diagnostic),
# 1b (care-choice forest), compute_price_wedge_cache.R
# (price_wedge_income_pctiles()), compute_substate_alpha.R
#------------------------------------------------------------------------------



INCOME_RANK_SCHEMES <- c('national_rpp', 'national')

INCOME_RANK_RPP_PATH <- file.path('resources', 'supply',
                                  'US_BEA__regional_price_parities_all_items__2019.csv')



get_income_rank_scheme <- function() {

  #----------------------------------------------------------------------------
  # Active income-rank scheme -- global income_rank_scheme (config.R), else
  # env var ECEC_INCOME_RANK, else 'national_rpp'.
  #
  # Returns: (chr) one of INCOME_RANK_SCHEMES
  #----------------------------------------------------------------------------

  s <- if (exists('income_rank_scheme', envir = globalenv())) {
    get('income_rank_scheme', envir = globalenv())
  } else {
    Sys.getenv('ECEC_INCOME_RANK', 'national_rpp')
  }
  if (is.null(s) || s == '') s <- 'national_rpp'
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
  # default_paths global (config.R) -- estimation_info local to 1a/1b, not
  # visible here
  acs_raw_path <- file.path(default_paths$roots$input, default_paths$dependencies$ACS,
                            'ipums_usa.csv')
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
  #   (int vec, 0 = one national group)
  #----------------------------------------------------------------------------

  scheme <- get_income_rank_scheme()
  n <- length(income)
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



get_substate_rpp <- function(geo) {

  #----------------------------------------------------------------------------
  # Sub-state geography's own RPP (all items 2019, US = 100) -- RPP file row
  # w/ geo code, non-state BEA geography (NYC = msa_35620, 114.899).
  #
  # Params:
  #   - geo (chr): sub-state code, e.g. 'NYC'
  #
  # Returns: (dbl) RPP
  #----------------------------------------------------------------------------

  if (!file.exists(INCOME_RANK_RPP_PATH)) {
    stop('RPP file missing: ', INCOME_RANK_RPP_PATH, '.')
  }
  rpp <- utils::read.csv(INCOME_RANK_RPP_PATH, stringsAsFactors = FALSE)
  row <- rpp[rpp$geo == geo & rpp$bea_geography != 'state', ]
  if (nrow(row) != 1 || !is.finite(row$rpp_all_items_2019)) {
    stop('RPP file ', INCOME_RANK_RPP_PATH, ' has ', nrow(row), ' non-state rows for ',
         geo, ', need 1. Add the BEA metro row for ', geo, ' (geo = ', geo, ').')
  }
  row$rpp_all_items_2019
}



substate_rank_inputs <- function(income, region, statefip, in_geo, geo) {

  #----------------------------------------------------------------------------
  # ACS rank inputs for sub-state geography's own version, per active scheme.
  # Cutpoints built from (cut_income, group), every record evaluated at
  # (eval_income, group). Only geo's records differ from income_rank_inputs():
  #   - national_rpp: one national group, cutpoints from default inputs (geo
  #     at parent-state RPP); geo's records evaluated at income / geo RPP
  #   - national: geo ranks same in both versions -- NULL, logged
  # Callers keep geo's records only -- rest of geo's state ranks off w/out geo.
  #
  # Params:
  #   - income (num vec): income (HHINCOME, baseline AGI)
  #   - region (vec): Census region 1-4, same length
  #   - statefip (vec): state FIPS, same length
  #   - in_geo (lgl vec): record in geo, same length, no NA
  #   - geo (chr): sub-state code, e.g. 'NYC'
  #
  # Returns: (list) cut_income (num vec), eval_income (num vec), group (int
  #   vec, 0); NULL when scheme leaves versions identical
  #----------------------------------------------------------------------------

  if (length(in_geo) != length(income) || anyNA(in_geo)) {
    stop('substate_rank_inputs: in_geo needs one non-NA flag per income (',
         length(in_geo), ' flags, ', length(income), ' incomes).')
  }
  scheme <- get_income_rank_scheme()
  if (scheme == 'national') {
    cat('  Sub-state ', geo, ' ranks: scheme ', scheme, ' ranks ', geo,
        ' same as its state -- own version skipped\n', sep = '')
    return(NULL)
  }

  # national_rpp
  base <- income_rank_inputs(income, region, statefip, 'acs')
  geo_rpp <- get_substate_rpp(geo)
  eval_income <- base$income
  eval_income[in_geo] <- income[in_geo] / (geo_rpp / 100)
  cat('  Sub-state ', geo, ' ranks: income / ', geo, ' RPP ', geo_rpp,
      ' vs national cutpoints (', sum(in_geo), ' records)\n', sep = '')
  list(cut_income = base$income, eval_income = eval_income, group = base$group)
}
