#------------------------------------------------------------------------------
# child_ubi_10k.R - Tax Policy
#
# Simple $10,000/child universal refundable credit.
# No income phase-out, no caps - pure "child UBI" style transfer.
#------------------------------------------------------------------------------


do_tax_policy <- function(parent_units_df, n_children, year) {

  #----------------------------------------------------------------------------
  # $10,000 per child refundable credit.
  #
  # Uses n_children_original (actual child count) for credit calculation,
  # not n_children (effective, capped at 2 for choice set).
  #
  # Fully refundable - taxes can go negative.
  #----------------------------------------------------------------------------

  credit_per_child_2026 <- 10000

  # Index the nominal credit with economy-wide hourly wage growth
  # (gdp_wages / agg_hours_index), using 2026 as base year.
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

  credit_per_child <- round(credit_per_child_2026 * indexing_factor)

  # Use actual number of children for credit

  actual_n_children <- parent_units_df$n_children_original

  # Apply credit uniformly across employment choices
  for (emp in c('none', 'pt', 'ft')) {
    tax_col <- paste0('total_tax.', emp)
    parent_units_df[[tax_col]] <- parent_units_df[[tax_col]] - credit_per_child * actual_n_children
  }

  return(parent_units_df)
}
