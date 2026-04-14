#------------------------------------------------------------------------------
# bbba.R
#
# Demand-side policy implementing the Build Back Better Act (BBBA) childcare
# and pre-K title as proposed in 2021.
#
# Key policy parameters:
#   - Eligibility cap: 250% of State Median Income (SMI)
#   - Free care threshold: 75% SMI
#   - Graduated copay schedule:
#       75-100% SMI:  2% of income
#       100-125% SMI: 4% of income
#       125-150% SMI: 7% of income
#       150-250% SMI: 7% of income (capped)
#   - Above 250% SMI: Not eligible
#   - Work requirement: All parents must be employed (primary caregiver working,
#     and in two-parent families, secondary parent must also be working)
#
# Simplifications for this model:
#   - Uses National Median Income (NMI) as proxy for SMI since model lacks
#     state-level geographic variation
#   - Universal pre-K component with wage parity not explicitly modeled
#   - Mixed-delivery system abstracted via existing market sectors
#
# Reference: H.R.5376 (117th Congress), Title II - Committee on Education
#            and Labor, Subtitle A - Universal Preschool, Subtitle B - Child
#            Care for Working Families
#------------------------------------------------------------------------------



do_demand_policy <- function(parent_units_df, catalog, P, n_children,
                             agi_matrix, taxes_matrix, gross_ecec_cost_matrix,
                             child1_cost_matrix = NULL, child2_cost_matrix = NULL) {

  #----------------------------------------------------------------------------
  # Computes demand-side subsidies under BBBA childcare policy.
  #
  # The policy provides graduated subsidies:
  #   - NO subsidy for families above 250% of median income
  #   - Free care for families at or below 75% of median income
  #   - 2% copay cap for 75-100% of median income
  #   - 4% copay cap for 100-125% of median income
  #   - 7% copay cap for 125-250% of median income
  #   - Work requirement: all parents must be employed (pt or ft)
  #
  # Parameters:
  #   - parent_units_df (tibble): Parent unit data with median_income column
  #   - catalog (tibble): Choice catalog from get_choice_catalog()
  #   - P (dbl[4]): Price vector for 4 ECEC market sectors
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

  # Policy parameters from BBBA text
  ELIGIBILITY_CAP        <- 2.50   # 250% median income = not eligible
  FREE_CARE_THRESHOLD    <- 0.75   # 75% median income = free care
  TIER_2_THRESHOLD       <- 1.00   # 100% median income
  TIER_3_THRESHOLD       <- 1.25   # 125% median income
  TIER_4_THRESHOLD       <- 1.50   # 150% median income
  COPAY_RATE_TIER_1      <- 0.02   # 2% copay for 75-100%
  COPAY_RATE_TIER_2      <- 0.04   # 4% copay for 100-125%
  COPAY_RATE_TIER_3      <- 0.07   # 7% copay for 125-150%
  COPAY_RATE_TIER_4      <- 0.07   # 7% copay for 150-250% (same rate, capped)

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

  # Calculate copay rate based on income ratio with BBBA graduated tiers
  calculate_copay_rate <- function(income_ratio) {
    case_when(
      # Above 250%: NOT ELIGIBLE (copay = 100% effectively means no subsidy)
      income_ratio > ELIGIBILITY_CAP ~ 1.0,

      # Below 75%: Free care
      income_ratio <= FREE_CARE_THRESHOLD ~ 0,

      # 75-100%: 2% copay cap
      income_ratio <= TIER_2_THRESHOLD ~ COPAY_RATE_TIER_1,

      # 100-125%: 4% copay cap
      income_ratio <= TIER_3_THRESHOLD ~ COPAY_RATE_TIER_2,

      # 125-150%: 7% copay cap
      income_ratio <= TIER_4_THRESHOLD ~ COPAY_RATE_TIER_3,

      # 150-250%: 7% copay cap (continues at same rate)
      TRUE ~ COPAY_RATE_TIER_4
    )
  }

  #--------------------------
  # Pre-compute eligibility by employment type
  #--------------------------

  employment_choices <- catalog$employment_choice

  precomputed <- list()
  for (emp in c('none', 'pt', 'ft')) {
    choice_indices <- which(employment_choices == emp)
    if (length(choice_indices) == 0) next

    agi <- agi_matrix[, choice_indices[1]]
    income_ratio <- agi / median_income
    copay_rate <- calculate_copay_rate(income_ratio)
    is_eligible <- copay_rate < 1.0
    max_family_contribution <- ifelse(is_eligible, copay_rate * pmax(0, agi), Inf)

    precomputed[[emp]] <- list(
      agi = agi,
      copay_rate = copay_rate,
      is_eligible = is_eligible & emp != 'none' & secondary_works,  # Work requirement (all parents)
      max_family_contribution = max_family_contribution
    )
  }

  #--------------------------
  # Calculate subsidy for each choice
  #--------------------------

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
