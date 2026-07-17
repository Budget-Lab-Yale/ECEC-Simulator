#------------------------------------------------------------------------------
# smooth_phaseout.R
#
# Demand-side policy: cliff-free variant of the Murray-Kaine universal
# subsidy (see universal.R). Free care at or below 75% of median income;
# above the threshold, the family copay is a constant rate applied to income
# ABOVE the threshold rather than tiered rates applied to total income:
#
#   copay(income) = COPAY_PHASEOUT_RATE * max(0, income - 0.75 * median_income)
#
# Properties (vs universal.R):
#   - Copay is exactly $0 at the free-care threshold, so out-of-pocket cost
#     is CONTINUOUS in income (universal.R jumps at 75%/100%/125% of median
#     because each tier rate applies to total income)
#   - The subsidy phases out smoothly to zero on its own once the copay
#     exceeds the cost of care -- no upper eligibility limit or cliff
#   - Same work requirement and paid-sector coverage as universal.R, so
#     differences isolate the copay schedule
#
# COPAY_PHASEOUT_RATE is calibrated so the policy's FY2026-2035 net budget
# effect matches the universal policy's ten-year score under the same data
# vintages (full ACS sample, n=1 draws, phase_in_years=5, 2026:2035):
#   universal:              -$818.5B  (run 202607162316)
#   smooth_phaseout 0.1157: -$819.5B  (run 202607170035; 0.12% from target)
# Verified full-sample bracket puts the exact crossing at X ~= 0.116.
# Calibrated 2026-07-17 via secant search at 1% sample (--baseline-interface
# iterations) with full-sample verification.
#------------------------------------------------------------------------------



do_demand_policy <- function(parent_units_df, catalog, P, n_children,
                             agi_matrix, taxes_matrix, gross_ecec_cost_matrix,
                             child1_cost_matrix = NULL, child2_cost_matrix = NULL) {

  #----------------------------------------------------------------------------
  # Computes demand-side subsidies under the smooth-phaseout policy.
  #
  # The policy provides:
  #   - Free care for families at or below 75% of median income
  #   - Above the threshold, copay = COPAY_PHASEOUT_RATE * (income - threshold),
  #     a continuous schedule with no cliffs and no upper eligibility limit
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

  # Free-care threshold shared with universal.R (Murray-Kaine)
  FREE_CARE_THRESHOLD <- 0.75   # <= 75% of median income = free care

  # Copay rate on income ABOVE the threshold. Calibrated so the ten-year
  # fiscal cost matches the universal policy's score.
  COPAY_PHASEOUT_RATE <- 0.1157

  # Median income (by family size, stored in parent_units_df)
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

    # Smooth phaseout: copay applies only to income above the free-care
    # threshold, so the schedule is continuous at the threshold (copay = 0)
    income_above_threshold <- pmax(0, agi - FREE_CARE_THRESHOLD * median_income)

    # Work requirement: all parents must be employed
    is_eligible <- emp != 'none' & secondary_works

    precomputed[[emp]] <- list(
      agi = agi,
      is_eligible = is_eligible,
      max_family_contribution = COPAY_PHASEOUT_RATE * income_above_threshold
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
