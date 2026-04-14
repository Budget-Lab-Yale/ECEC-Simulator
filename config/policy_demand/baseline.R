#------------------------------------------------------------------------------
# baseline.R
#
# Baseline policy function that implements no demand-side subsidies.
#
# Returns a zero subsidy matrix for all choices.
#------------------------------------------------------------------------------



do_demand_policy <- function(parent_units_df, catalog, P, n_children,
                             agi_matrix, taxes_matrix, gross_ecec_cost_matrix,
                             child1_cost_matrix = NULL, child2_cost_matrix = NULL) {

  #----------------------------------------------------------------------------
  # Computes demand-side subsidies for each choice.
  #
  # This is the baseline/reference policy that applies no subsidies.
  #
  # Parameters:
  #   - parent_units_df (tibble): Parent unit data
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
  #   matrix (n_units x n_choices) of subsidy amounts (all zeros for baseline)
  #----------------------------------------------------------------------------

  n_units <- nrow(parent_units_df)
  n_choices <- nrow(catalog)

  # Baseline: no subsidies
  subsidy_matrix <- matrix(0, nrow = n_units, ncol = n_choices)

  return(subsidy_matrix)
}
