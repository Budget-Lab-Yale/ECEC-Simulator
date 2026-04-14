#------------------------------------------------------------------------------
# add_simulation_variables.R
#
# Adds simulation context variables to parent_units dataframes.
# Variables like median_income are computed once per year and added as columns
# so policy functions can access them without changing function signatures.
#
# Called from: 3b (run_simulation_year), 3c (run_scenario)
#------------------------------------------------------------------------------



add_simulation_variables <- function(parent_units_list, median_income_lookup,
                                     cpi_factor_2019 = 1.0,
                                     cpi_chain_factor_2019 = 1.0,
                                     cpi_chain_factor_2026 = 1.0) {

  #----------------------------------------------------------------------------
  # Adds simulation context variables to all parent unit dataframes.
  #
  # This allows policy functions to access year-specific parameters (like
  # median income) without changing function signatures. Variables are added
  # as columns that are constant across all rows within a year.
  #
  # Params:
  #   - parent_units_list (list): Named list of parent unit dataframes
  #   - median_income_lookup (named vector): Mapping of family size (1-5) to Median AGI
  #   - cpi_factor_2019 (dbl): CPI growth factor from 2019 to the simulation year.
  #       Used to deflate nominal values into 2019 dollars for utility.
  #   - cpi_chain_factor_2019 (dbl): Chained CPI growth factor from 2019 to sim year.
  #       Used only for ARPA CDCTC inflation indexing.
  #   - cpi_chain_factor_2026 (dbl): Chained CPI growth factor from 2019 to 2026.
  #       Used as the base-year denominator for ARPA CDCTC and child UBI indexing.
  #
  # Returns: list with same structure, dataframes augmented with context columns
  #----------------------------------------------------------------------------

  # Add variables to each parent unit dataframe
  for (pu_name in PARENT_UNIT_NAMES) {
    if (!is.null(parent_units_list[[pu_name]]) &&
        nrow(parent_units_list[[pu_name]]) > 0) {

      df <- parent_units_list[[pu_name]]

      # Calculate family size for median income lookup
      # Adults: 1 + (1 if secondary parent exists)
      # Children: Actual number of children (n_children_original)
      n_adults <- 1 + as.integer(!is.na(df$hours_secondary))
      n_children <- df$n_children_original
      family_size <- pmin(n_adults + n_children, 5)

      # Lookup median income by family size
      # median_income_lookup is a named vector (names are "1", "2", "3", "4", "5")
      medians <- median_income_lookup[as.character(family_size)]

      # Fallback for any missing lookups (shouldn't happen if lookup is complete)
      if (any(is.na(medians))) {
        # Use size 3 as fallback if something goes wrong
        medians[is.na(medians)] <- median_income_lookup['3']
      }

      parent_units_list[[pu_name]] <- df %>%
        mutate(
          median_income = !!medians,
          cpi_factor_2019 = !!cpi_factor_2019,
          cpi_chain_factor_2019 = !!cpi_chain_factor_2019,
          cpi_chain_factor_2026 = !!cpi_chain_factor_2026
        )
    }
  }

  return(parent_units_list)
}
