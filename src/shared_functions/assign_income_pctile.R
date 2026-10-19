#------------------------------------------------------------------------------
# assign_income_pctile.R
#
# Income percentile (1-100) of each record within its own group, from
# per-group cutpoints. Current callers pass one national group (0, from
# income_rank_inputs(), income_rank.R); function still takes several groups.
#
# Called from: 1b (care-choice forest, NSECE + ACS), compute_price_wedge_cache.R
# (price_wedge_income_pctiles())
#------------------------------------------------------------------------------



assign_income_pctile <- function(income, group, cutpoints) {

  #----------------------------------------------------------------------------
  # Percentile bin of each income against its group's cutpoints. Bin = 1 +
  # cutpoints strictly below income -- same bins as cut(right = TRUE) when
  # cutpoints unique; also takes tied cutpoints (small state, one heavy record
  # spans several percentiles), where cut() stops. NA income or NA group -> NA.
  #
  # Params:
  #   - income (num vec): income of each record
  #   - group (vec): group of each record (current callers: 0, national), same
  #     length
  #   - cutpoints (list): named by group, each a sorted num vec of 99
  #     cutpoints (weighted quantiles 0.01-0.99)
  #
  # Returns: (int vec) percentile bin 1-100, NA where income or group NA
  #----------------------------------------------------------------------------

  if (length(income) != length(group)) {
    stop('assign_income_pctile: income has ', length(income), ' values, group ',
         length(group), '. Pass one group per income.')
  }

  # Every non-NA group needs cutpoints -- else its records silently get NA
  key <- as.character(group)
  no_cuts <- setdiff(unique(key[!is.na(key)]), names(cutpoints))
  if (length(no_cuts) > 0) {
    stop('assign_income_pctile: no income cutpoints for group(s) ',
         paste(head(no_cuts, 10), collapse = ', '),
         '. Check that every group has records with positive income.')
  }

  pctile <- rep(NA_integer_, length(income))
  for (g in names(cutpoints)) {
    i <- which(key == g)
    pctile[i] <- findInterval(income[i], cutpoints[[g]], left.open = TRUE) + 1L
  }
  pctile
}
