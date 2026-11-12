#------------------------------------------------------------------------------
# assign_income_pctile.R
#
# Income percentile from cutpoints, ties at mid-rank. Used by care forest
# (NSECE training, ACS prediction), price forest (NSECE training), price
# wedge cache (ACS).
#------------------------------------------------------------------------------


assign_income_pctile <- function(income, income_pctiles) {

  #----------------------------------------------------------------------------
  # Income percentile (1-100) from 99 cutpoints, w/ ties at mid-rank.
  # Cutpoints come from income plus U(0,1) jitter, so a heaped income value
  # (e.g. NSECE $25,000) spans several cutpoints; cut() on unjittered income
  # put every tie in lowest percentile of its span (Harris's input audit: 19%
  # of 2024 NSECE children under 5 in ties spanning 2+ percentiles). Tie gets
  # middle of span instead, both surveys (Anna, 2026-11-12). Untied incomes
  # keep their percentile.
  #
  # Params:
  #   - income (num vec): Income (<= lowest cutpoint -> percentile 1)
  #   - income_pctiles (num vec): 99 cutpoints (weighted quantiles of income
  #       plus U(0,1) jitter, probs 0.01-0.99)
  #
  # Returns: (int vec) Percentile 1-100, NA where income NA
  #----------------------------------------------------------------------------

  lo <- 1L + findInterval(income, income_pctiles, left.open = TRUE)
  hi <- pmin(100L, 1L + findInterval(income + 1, income_pctiles, left.open = TRUE))

  # Midpoint, halves rounded down: single income at a cutpoint has own
  # jittered cutpoint inside its span (hi = lo + 1) -> keeps lo, as cut()
  as.integer((lo + hi) %/% 2L)
}
