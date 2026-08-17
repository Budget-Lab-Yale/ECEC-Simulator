#------------------------------------------------------------------------------
# child_ubi_age_dep.R - Tax Policy
#
# Age-dependent universal refundable credit: $12,000 (2026) per child aged
# 0-2 and $7,000 (2026) per child aged 3-4, indexed to chained CPI after
# 2026. No income phase-out, no caps - pure "child UBI" style transfer.
#
# For 3+ child pseudofamilies, the two child slots represent a pair of the
# family's m children; scaling by n_children_original / n_children makes the
# aggregate credit exact (matches the uniform child_ubi_* convention, which
# reduces to amount x n_children_original when both ages get the same rate).
#------------------------------------------------------------------------------


do_tax_policy <- function(parent_units_df, n_children, year) {

  #----------------------------------------------------------------------------
  # Age-dependent refundable credit: $12,000/child aged 0-2, $7,000/child
  # aged 3-4 (2026 dollars, chained-CPI-indexed).
  #
  # Fully refundable - taxes can go negative.
  #----------------------------------------------------------------------------

  credit_2026_age_0_2 <- 12000
  credit_2026_age_3_4 <- 7000

  # Index the nominal credits with chained CPI, using 2026 as base year.
  # Inflation factor in 2026 is exactly 1.
  extract_single_positive <- function(x, default_value) {
    if (is.null(x)) return(default_value)
    x_unique <- unique(x)
    x_unique <- x_unique[is.finite(x_unique) & x_unique > 0]
    if (length(x_unique) != 1) return(default_value)
    x_unique[1]
  }

  wage_growth_factor_2019 <- extract_single_positive(parent_units_df[['wage_growth_factor_2019']], 1.0)
  wage_growth_factor_2026 <- extract_single_positive(parent_units_df[['wage_growth_factor_2026']], wage_growth_factor_2019)
  indexing_factor <- wage_growth_factor_2019 / wage_growth_factor_2026
  if (!is.finite(indexing_factor) || indexing_factor <= 0) indexing_factor <- 1.0

  credit_age_0_2 <- round(credit_2026_age_0_2 * indexing_factor)
  credit_age_3_4 <- round(credit_2026_age_3_4 * indexing_factor)

  # Per-slot credit by child age (slots .1 and .2 hold the unit's children)
  slot_credit <- rep(0, nrow(parent_units_df))
  for (cidx in 1:n_children) {
    age_col <- paste0('child_age.', cidx)
    if (!age_col %in% names(parent_units_df)) next
    child_age <- parent_units_df[[age_col]]
    slot_credit <- slot_credit + dplyr::case_when(
      is.na(child_age)  ~ 0,
      child_age <= 2    ~ credit_age_0_2,
      TRUE              ~ credit_age_3_4
    )
  }

  # Pseudofamily scaling: preserves aggregate per-child credits for 3+ child
  # families (reduces to 1 for families with <= 2 children under 5)
  total_credit <- slot_credit * parent_units_df$n_children_original / n_children

  # Apply credit uniformly across employment choices
  for (emp in c('none', 'pt', 'ft')) {
    tax_col <- paste0('total_tax.', emp)
    parent_units_df[[tax_col]] <- parent_units_df[[tax_col]] - total_credit
  }

  return(parent_units_df)
}
