#------------------------------------------------------------------------------
# murray_kaine.R
#
# Demand-side policy implementing the Child Care for Working Families Act
# ("Murray-Kaine"), the flagship progressive-Democrat childcare proposal.
#
# Key policy parameters:
#   - Universal eligibility: NO income cap (unlike BBBA/CCDBG)
#   - Free care threshold: 75% of State Median Income (SMI)
#   - Graduated copay structure:
#     * 75-100% SMI: 2% of income
#     * 100-125% SMI: 4% of income
#     * 125-150% SMI: 7% of income
#     * Above 150% SMI: 7% of income (capped maximum)
#   - Work requirement: All parents must be employed (primary caregiver working,
#     and in two-parent families, secondary parent must also be working)
#
# Simplifications for this model:
#   - Uses National Median Income (NMI) as proxy for SMI since model lacks
#     state-level geographic variation
#   - Universal pre-K component not explicitly modeled (captured implicitly
#     through sector composition)
#   - "True cost of quality" provider payments handled separately via
#     supply-side policy if desired
#
# Reference: S.1354 / H.R.2976 (118th Congress)
#            Child Care for Working Families Act of 2023
#            Lead sponsors: Sen. Murray (D-WA), Sen. Kaine (D-VA)
#------------------------------------------------------------------------------



do_demand_policy <- function(parent_units_df, catalog, P, n_children,
                             agi_matrix, taxes_matrix, gross_ecec_cost_matrix,
                             child1_cost_matrix = NULL, child2_cost_matrix = NULL) {

  #----------------------------------------------------------------------------
  # Computes demand-side subsidies under Murray-Kaine policy.
  #
  # The policy provides:
  #   - Free care for families at or below 75% of median income
  #   - Graduated copay structure for families above 75%:
  #     * 75-100% SMI: 2% of income
  #     * 100-125% SMI: 4% of income
  #     * 125-150% SMI: 7% of income
  #     * Above 150% SMI: 7% of income
  #   - NO upper income eligibility limit (universal affordability guarantee)
  #   - Work requirement: all parents must be employed (pt or ft)
  #
  # Parameters:
  #   - parent_units_df (tibble): Parent unit data with median_income column
  #   - catalog (tibble): Choice catalog from get_choice_catalog()
  #   - P (dbl[4]): Price vector for 4 market sectors
  #   - n_children (int): Number of children (1 or 2)
  #   - agi_matrix (matrix): n_units x n_choices AGI values
  #   - taxes_matrix (matrix): n_units x n_choices tax liability values
  #   - gross_ecec_cost_matrix (matrix): n_units x n_choices care costs
  #   - child1_cost_matrix (matrix): n_units x n_choices child 1 care costs
  #   - child2_cost_matrix (matrix): n_units x n_choices child 2 care costs
  #
  # Returns:
  #   matrix (n_units x n_choices) of subsidy amounts
  #----------------------------------------------------------------------------

  # Policy parameters from Murray-Kaine bill (S.1354, 118th Congress)
  # Graduated copay structure:
  FREE_CARE_THRESHOLD <- 0.75  # <= 75% SMI = free care (Updated from 85%)
  TIER_1_THRESHOLD    <- 1.00  # 75-100% SMI = 2% copay
  TIER_2_THRESHOLD    <- 1.25  # 100-125% SMI = 4% copay
  TIER_3_THRESHOLD    <- 1.50  # 125-150% SMI = 7% copay
  # Above 150% SMI = 7% copay (capped maximum)

  COPAY_RATE_TIER_1   <- 0.02  # 2% for 75-100% SMI
  COPAY_RATE_TIER_2   <- 0.04  # 4% for 100-125% SMI
  COPAY_RATE_TIER_3   <- 0.07  # 7% for 125-150% SMI
  COPAY_RATE_MAX      <- 0.07  # 7% for >150% SMI

  # Median income (SMI by family size, stored in parent_units_df)
  median_income <- parent_units_df$median_income

  n_units <- nrow(parent_units_df)
  n_choices <- nrow(catalog)

  # Secondary parent work requirement (for two-parent families)
  # TRUE if single parent (hours_secondary is NA) or secondary works (hours > 0)
  secondary_works <- is.na(parent_units_df$hours_secondary) |
                     parent_units_df$hours_secondary > 0

  # Paid care sectors eligible for subsidy (sectors 2, 3, 4)
  is_paid_care <- function(sector_id) {
    !is.na(sector_id) & sector_id %in% c(2, 3, 4)
  }

  employment_choices <- catalog$employment_choice

  # Pre-compute copay cap by employment type (AGI varies by employment choice)
  precomputed <- list()
  for (emp in c('none', 'pt', 'ft')) {
    choice_indices <- which(employment_choices == emp)
    if (length(choice_indices) == 0) next

    agi <- agi_matrix[, choice_indices[1]]
    income_ratio <- agi / median_income

    # Murray-Kaine graduated copay structure (NO upper eligibility limit)
    copay_rate <- ifelse(
      income_ratio <= FREE_CARE_THRESHOLD, 0,
      ifelse(income_ratio <= TIER_1_THRESHOLD, COPAY_RATE_TIER_1,
      ifelse(income_ratio <= TIER_2_THRESHOLD, COPAY_RATE_TIER_2,
      ifelse(income_ratio <= TIER_3_THRESHOLD, COPAY_RATE_TIER_3,
      COPAY_RATE_MAX))))

    # Work requirement: all parents must be employed
    is_eligible <- emp != 'none' & secondary_works

    precomputed[[emp]] <- list(
      agi = agi,
      is_eligible = is_eligible,
      max_family_contribution = copay_rate * pmax(0, agi)
    )
  }

  subsidy_matrix <- matrix(0, nrow = n_units, ncol = n_choices)

  for (k in 1:n_choices) {
    choice <- catalog[k, ]
    emp <- choice$employment_choice

    is_eligible <- precomputed[[emp]]$is_eligible
    max_family_contribution <- precomputed[[emp]]$max_family_contribution

    c1_is_paid <- is_paid_care(choice$child1_market_sector_id)

    # Use if/else (not ifelse) because c1_is_paid is scalar — ifelse would
    # truncate the vector result to length 1, using only row 1's cost
    if (n_children == 1) {
      total_subsidizable_cost <- if (c1_is_paid) child1_cost_matrix[, k] else rep(0, n_units)
    } else {
      c2_is_paid <- is_paid_care(choice$child2_market_sector_id)
      c1_subsid_cost <- if (c1_is_paid) child1_cost_matrix[, k] else rep(0, n_units)
      c2_subsid_cost <- if (c2_is_paid) child2_cost_matrix[, k] else rep(0, n_units)
      total_subsidizable_cost <- c1_subsid_cost + c2_subsid_cost
    }

    # Subsidy = cost - min(cost, copay cap)
    # For eligible working families only
    subsidy_matrix[, k] <- ifelse(
      is_eligible,
      pmax(0, total_subsidizable_cost - pmin(total_subsidizable_cost, max_family_contribution)),
      0
    )
  }

  return(subsidy_matrix)
}
