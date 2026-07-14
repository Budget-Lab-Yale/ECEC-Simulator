compute_policy_components <- function(parent_units_df, P, n_children, demand_params,
                                       policy_demand, policy_cdctc,
                                       cpi_growth_factor = 1.0,
                                       offered1 = FALSE, offered2 = FALSE,
                                       base = NULL, build_components = TRUE,
                                       check_subsidy_variation = TRUE) {

  #----------------------------------------------------------------------------
  # Runs the full policy pipeline: base matrices -> demand subsidy -> CDCTC ->
  # final components.
  #
  # Params:
  #   - parent_units_df (df): Parent units (must have price_wedge.* columns)
  #   - P (num vec): Sector prices
  #   - n_children (int): 1 or 2
  #   - demand_params (list): Demand parameters (must have other_paid_base_price)
  #   - policy_demand (fn): Demand policy function (accepts offered1/offered2)
  #   - policy_cdctc (fn): CDCTC policy function
  #   - cpi_growth_factor (dbl): CPI adjustment factor
  #   - offered1 (logical): Treat child 1 as holding a program offer in every
  #       rationed sector (ignored by unrationed policies)
  #   - offered2 (logical): Same for child 2
  #   - base (list or NULL): Precomputed base matrices from a previous call at
  #       the same P (base matrices are offer-independent, so they can be
  #       reused across offer variants)
  #   - build_components (logical): If FALSE, skip building the wide-format
  #       components tibble (hot paths that only need the matrices)
  #
  # Returns: (list) base, subsidy_matrix, cdctc_matrix, components (NULL if
  #   build_components is FALSE)
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)

  if (is.null(base)) {
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
  }

  subsidy_matrix <- policy_demand(
    parent_units_df = parent_units_df,
    catalog = catalog,
    P = P,
    n_children = n_children,
    agi_matrix = base$agi_matrix,
    taxes_matrix = base$taxes_matrix,
    gross_ecec_cost_matrix = base$gross_ecec_cost_matrix,
    child1_cost_matrix = base$child1_cost_matrix,
    child2_cost_matrix = base$child2_cost_matrix,
    offered1 = offered1,
    offered2 = offered2
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
  # households (catches the ifelse() scalar-broadcast bug). Skipped for
  # row-chunked callers (e.g. extract_offer_conditional_values): a small chunk
  # can legitimately have a first-nonzero column whose positive values are
  # identical (sd == 0), a false positive. The dim() assertions above still
  # run, and the same policy functions are variation-checked on the full-data
  # equilibrium path.
  if (check_subsidy_variation && n_units > 1 && any(subsidy_matrix > 0)) {
    nonzero_col <- which(colSums(subsidy_matrix) > 0)[1]
    if (!is.na(nonzero_col)) {
      stopifnot(sd(subsidy_matrix[subsidy_matrix[, nonzero_col] > 0, nonzero_col]) > 0 ||
                sum(subsidy_matrix[, nonzero_col] > 0) <= 1)
    }
  }

  components <- if (build_components) {
    build_final_components(
      agi_matrix = base$agi_matrix,
      taxes_matrix = base$taxes_matrix,
      gross_ecec_cost_matrix = base$gross_ecec_cost_matrix,
      subsidy_matrix = subsidy_matrix,
      cdctc_matrix = cdctc_matrix
    )
  } else {
    NULL
  }

  list(
    base = base,
    subsidy_matrix = subsidy_matrix,
    cdctc_matrix = cdctc_matrix,
    components = components
  )
}
