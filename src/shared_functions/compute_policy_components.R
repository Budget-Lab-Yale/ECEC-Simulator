compute_policy_components <- function(parent_units_df, P, n_children, demand_params,
                                       policy_demand, policy_cdctc,
                                       cpi_growth_factor = 1.0) {

  #----------------------------------------------------------------------------
  # Runs the full policy pipeline: base matrices -> demand subsidy -> CDCTC ->
  # final components.
  #
  # Params:
  #   - parent_units_df (df): Parent units (must have price_wedge.* columns)
  #   - P (num vec): Sector prices
  #   - n_children (int): 1 or 2
  #   - demand_params (list): Demand parameters (must have other_paid_base_price)
  #   - policy_demand (fn): Demand policy function
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI adjustment factor
  #
  # Returns: (list) base, subsidy_matrix, cdctc_matrix, components
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)

  base <- compute_base_matrices(
    parent_units_df = parent_units_df,
    catalog = catalog,
    P = P,
    n_children = n_children,
    price_wedge.center_low = parent_units_df$price_wedge.center_low,
    price_wedge.center_high = parent_units_df$price_wedge.center_high,
    price_wedge.home = parent_units_df$price_wedge.home,
    price_wedge.other_paid = parent_units_df$price_wedge.other_paid,
    other_paid_base_price = demand_params$other_paid_base_price,
    cpi_growth_factor = cpi_growth_factor
  )

  subsidy_matrix <- policy_demand(
    parent_units_df = parent_units_df,
    catalog = catalog,
    P = P,
    n_children = n_children,
    agi_matrix = base$agi_matrix,
    taxes_matrix = base$taxes_matrix,
    gross_ecec_cost_matrix = base$gross_ecec_cost_matrix,
    child1_cost_matrix = base$child1_cost_matrix,
    child2_cost_matrix = base$child2_cost_matrix
  )

  cdctc_matrix <- policy_cdctc(
    parent_units_df = parent_units_df,
    catalog = catalog,
    P = P,
    n_children = n_children,
    agi_matrix = base$agi_matrix,
    taxes_matrix = base$taxes_matrix,
    iit_matrix = base$iit_matrix,
    gross_ecec_cost_matrix = base$gross_ecec_cost_matrix,
    subsidy_matrix = subsidy_matrix,
    child1_cost_matrix = base$child1_cost_matrix,
    child2_cost_matrix = base$child2_cost_matrix,
    agi1_matrix = base$agi1_matrix,
    agi2_matrix = base$agi2_matrix,
    iit1_matrix = base$iit1_matrix,
    iit2_matrix = base$iit2_matrix,
    earnings1_matrix = base$earnings1_matrix,
    earnings2_matrix = base$earnings2_matrix
  )

  # -- Assertions: policy outputs have correct shape ----
  # Policy functions receive and return matrices. If a policy accidentally returns
  # a vector (length n_units) instead of a matrix (n_units x n_choices), R will
  # silently recycle it. If it returns a scalar, same problem but worse.
  n_units <- nrow(parent_units_df)
  n_choices <- nrow(catalog)
  stopifnot(identical(dim(subsidy_matrix), c(n_units, n_choices)))
  stopifnot(identical(dim(cdctc_matrix), c(n_units, n_choices)))

  # If a policy is active (non-zero subsidies exist), they should vary across
  # households (catches the ifelse() scalar-broadcast bug)
  if (n_units > 1 && any(subsidy_matrix > 0)) {
    nonzero_col <- which(colSums(subsidy_matrix) > 0)[1]
    if (!is.na(nonzero_col)) {
      stopifnot(sd(subsidy_matrix[subsidy_matrix[, nonzero_col] > 0, nonzero_col]) > 0 ||
                sum(subsidy_matrix[, nonzero_col] > 0) <= 1)
    }
  }

  components <- build_final_components(
    agi_matrix = base$agi_matrix,
    taxes_matrix = base$taxes_matrix,
    gross_ecec_cost_matrix = base$gross_ecec_cost_matrix,
    subsidy_matrix = subsidy_matrix,
    cdctc_matrix = cdctc_matrix
  )

  list(
    base = base,
    subsidy_matrix = subsidy_matrix,
    cdctc_matrix = cdctc_matrix,
    components = components
  )
}
