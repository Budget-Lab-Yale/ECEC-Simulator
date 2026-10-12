write_employment_targeting_diagnostics <- function(baseline_result, parent_units_list,
                                                    demand_params, supply_params,
                                                    baseline_info, target_emp_rates, year) {

  #----------------------------------------------------------------------------
  # Writes employment targeting diagnostics (target vs achieved rates and
  # solved shifts by group) to models/equilibrium/employment_targeting_<year>.csv
  # for the baseline scenario. No-op unless the baseline converged with
  # employment targeting enabled.
  #
  # Params:
  #   - baseline_result (list): Baseline run_scenario result (prices, shifts)
  #   - parent_units_list (list): Named list of parent unit dataframes by type
  #   - demand_params (list): Demand model parameters by parent unit type
  #   - supply_params (list): Supply model parameters (for cpi_factor)
  #   - baseline_info (list): Baseline scenario configuration (policies, paths)
  #   - target_emp_rates (num vec or NULL): Target rates by group
  #   - year (int): Simulation year
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  if (!isTRUE(baseline_result$converged) ||
      is.null(baseline_result$employment_shifts) ||
      is.null(target_emp_rates)) {
    return(invisible(NULL))
  }

  do_demand_policy <- load_demand_policy(baseline_info$equilibrium$policy_demand)$do_demand_policy
  do_cdctc_policy  <- load_cdctc_policy(baseline_info$policy_cdctc %||% 'baseline')

  # Create output directory if needed
  eq_output_dir <- file.path(baseline_info$paths$output, 'models', 'equilibrium')
  dir.create(eq_output_dir, showWarnings = FALSE, recursive = TRUE)

  # Compute achieved employment rates at final solution
  achieved_rates <- setNames(rep(NA_real_, length(EMPLOYMENT_TARGETING_GROUPS)), EMPLOYMENT_TARGETING_GROUPS)

  for (g in EMPLOYMENT_TARGETING_GROUPS) {
    achieved_rates[g] <- compute_group_employment_rate(
      P = baseline_result$prices,
      group = g,
      parent_units_list = parent_units_list,
      demand_params = demand_params,
      employment_shifts = baseline_result$employment_shifts,
      policy_demand = do_demand_policy,
      policy_cdctc = do_cdctc_policy,
      cpi_growth_factor = supply_params$cpi_factor
    )
  }

  # Build diagnostics dataframe
  diag_df <- tibble(
    year = year,
    group = EMPLOYMENT_TARGETING_GROUPS,
    target_rate = target_emp_rates[EMPLOYMENT_TARGETING_GROUPS],
    achieved_rate = achieved_rates[EMPLOYMENT_TARGETING_GROUPS],
    delta_shift = baseline_result$employment_shifts[EMPLOYMENT_TARGETING_GROUPS],
    rate_gap = achieved_rates[EMPLOYMENT_TARGETING_GROUPS] - target_emp_rates[EMPLOYMENT_TARGETING_GROUPS]
  )

  # Write CSV
  output_file <- file.path(eq_output_dir, paste0('employment_targeting_', year, '.csv'))
  fwrite(diag_df, output_file, na = 'NA')

  # Print summary
  cat('  Employment targeting diagnostics:\n')
  for (g in EMPLOYMENT_TARGETING_GROUPS) {
    cat(sprintf('    %s: target=%.4f, achieved=%.4f, delta=%.4f\n',
                g, target_emp_rates[g], achieved_rates[g], baseline_result$employment_shifts[g]))
  }

  invisible(NULL)
}
