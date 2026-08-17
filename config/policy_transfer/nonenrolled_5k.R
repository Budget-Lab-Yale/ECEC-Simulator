#------------------------------------------------------------------------------
# nonenrolled_5k.R - Transfer Policy
#
# $5,000 (2026) cash transfer per child under 5 NOT enrolled in formal
# center- or home-based care, indexed to nominal wage growth after 2026.
#
# "Enrolled" = the child's chosen arrangement is one of the four market
# sectors (Unpaid/Low-Priced/High-Priced Center-Based, Paid Home-Based).
# Children in Other Paid, informal unpaid care, or Parent Only receive the
# transfer. No work requirement: the transfer is unconditional on employment.
#
# For 3+ child pseudofamilies, the two child slots represent a pair of the
# family's m children; scaling by n_children_original / n_children makes
# aggregate transfers exact (each child appears in m-1 of the C(m,2)
# pseudofamilies; family weights are divided by C(m,2) in the split).
#------------------------------------------------------------------------------



do_transfer_policy <- function(parent_units_df, catalog, n_children) {

  #----------------------------------------------------------------------------
  # $5,000 per non-enrolled child under 5, wage-growth-indexed (2026 base).
  #
  # Params:
  #   - parent_units_df (tibble): Parent unit data with wage_growth_factor_* and
  #       n_children_original columns
  #   - catalog (tibble): Choice catalog from get_choice_catalog()
  #   - n_children (int): Number of children (1 or 2)
  #
  # Returns:
  #   matrix (n_units x n_choices) of transfer amounts
  #----------------------------------------------------------------------------

  transfer_per_child_2026 <- 5000

  # Index the nominal transfer with economy-wide hourly wage growth
  # (gdp_wages / agg_hours_index), using 2026 as base year
  # (same series that updates baseline care worker wages)
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

  transfer_per_child <- round(transfer_per_child_2026 * indexing_factor)

  # Enrollment by choice: a child slot is enrolled iff its arrangement is a
  # market sector (non-NA market_sector_id; Parent Only / Other Paid /
  # informal unpaid arrangements have NA)
  n_nonenrolled_by_choice <- as.numeric(is.na(catalog$child1_market_sector_id))
  if (n_children == 2) {
    n_nonenrolled_by_choice <- n_nonenrolled_by_choice +
      as.numeric(is.na(catalog$child2_market_sector_id))
  }

  # Pseudofamily scaling: preserves aggregate per-child transfers for 3+
  # child families (reduces to 1 for families with <= 2 children under 5)
  pseudo_scale <- parent_units_df$n_children_original / n_children

  # Transfer matrix: per-unit scale x per-choice non-enrolled count
  tcrossprod(pseudo_scale * transfer_per_child, n_nonenrolled_by_choice)
}
