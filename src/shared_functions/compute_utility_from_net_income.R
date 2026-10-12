compute_utility_from_net_income <- function(NI_policy, parent_units_df, beta, rho,
                                            n_children, pu_name,
                                            employment_shifts = NULL) {

  #----------------------------------------------------------------------------
  # Computes the deterministic utility matrix V = beta * u(NI) from a net
  # income matrix, deflating to real 2019 dollars and optionally applying
  # employment-targeting shifts to working choices.
  #
  # beta/rho are calibrated on 2019-dollar net income; without deflation,
  # nominal income growth would mechanically change behavior over time.
  #
  # Params:
  #   - NI_policy (matrix): n_units x n_choices net income (nominal dollars)
  #   - parent_units_df (df): Parent units dataframe; may carry a constant
  #       cpi_factor_2019 column (defaults to 1.0 if absent)
  #   - beta (dbl): Utility scale parameter
  #   - rho (dbl): CRRA risk aversion parameter
  #   - n_children (int): Number of children (1 or 2), for the working mask
  #   - pu_name (chr): Parent unit type name (for error messages)
  #   - employment_shifts (num vec): Named vector of shifts by pc_group added
  #       to working-choice utilities (or NULL for no targeting)
  #
  # Returns: (matrix) n_units x n_choices utility matrix
  #----------------------------------------------------------------------------

  # Deflate to real 2019 dollars using the year-constant CPI factor
  cpi_factor_2019 <- parent_units_df[['cpi_factor_2019']]
  if (is.null(cpi_factor_2019)) {
    cpi_factor_2019 <- 1.0
  } else {
    cpi_factor_2019 <- unique(cpi_factor_2019)
    if (length(cpi_factor_2019) != 1 || !is.finite(cpi_factor_2019) || cpi_factor_2019 <= 0) {
      stop('compute_utility_from_net_income: invalid cpi_factor_2019 ',
           '(must be a single positive finite value) for ', pu_name, '.')
    }
  }

  NI_real_2019 <- NI_policy / cpi_factor_2019
  V_matrix <- compute_V_crra(NI_real_2019, beta, rho)

  if (any(!is.finite(V_matrix))) {
    stop('compute_utility_from_net_income: non-finite V_matrix from CRRA utility (', pu_name, ').')
  }

  # Apply employment shifts if provided (for employment targeting)
  # Adds group-specific delta to working choices for each demographic group.
  # NOTE: any alternative utility variant (e.g. offer states for rationed
  # policies) must also pass through this function so the shifts are never
  # silently dropped from a variant.
  if (!is.null(employment_shifts) && any(employment_shifts != 0)) {
    working_mask <- get_working_choice_mask(n_children)
    pc_group <- parent_units_df$pc_group

    for (g in names(employment_shifts)) {
      if (employment_shifts[g] != 0) {
        group_rows <- which(pc_group == g)
        if (length(group_rows) > 0) {
          V_matrix[group_rows, working_mask] <- V_matrix[group_rows, working_mask] + employment_shifts[g]
        }
      }
    }
  }

  return(V_matrix)
}
