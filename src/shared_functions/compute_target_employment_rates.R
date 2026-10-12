compute_target_employment_rates <- function(parent_units_list, macro_projections,
                                            demand_base_year, year,
                                            baseline_rates_2019 = NULL,
                                            epop_path = NULL) {

  #----------------------------------------------------------------------------
  # Computes group-specific employment rate targets for a simulation year:
  #   target_g(year) = emp_rate_g(base year) * overall_emp_growth(year vs base)
  #
  # RF probabilities (p_employment.pt, p_employment.ft) are from the 2019
  # calibration and don't change by simulation year, so baseline rates can be
  # computed from any year's parent_units data (and cached by the caller).
  #
  # Params:
  #   - parent_units_list (list): Named list of parent unit dataframes by type
  #   - macro_projections (df): Macro projections for employment growth
  #   - demand_base_year (int): Base year of the demand calibration (2019)
  #   - year (int): Simulation year
  #   - baseline_rates_2019 (num vec or NULL): Precomputed baseline rates by
  #       group (NULL = compute from parent_units_list)
  #   - epop_path (chr or NULL): Optional state epop projections override for
  #       the growth factor (NULL = national CBO file)
  #
  # Returns: (list) with elements:
  #   - targets (num vec): Target employment rates by group, capped below 1
  #   - baseline_rates_2019 (num vec): Baseline rates (for caller-side caching)
  #----------------------------------------------------------------------------

  if (is.null(baseline_rates_2019)) {
    baseline_rates_2019 <- compute_baseline_employment_rates(parent_units_list)
  }

  growth_factor <- get_employment_rate_growth_factor(
    macro_projections = macro_projections,
    base_year = demand_base_year,
    target_year = year,
    epop_path = epop_path
  )

  targets <- pmin(baseline_rates_2019 * growth_factor, 0.999999)

  cat('  Employment targeting:\n')
  for (g in EMPLOYMENT_TARGETING_GROUPS) {
    cat(sprintf('    %s: target=%.4f\n', g, targets[g]))
  }

  list(
    targets = targets,
    baseline_rates_2019 = baseline_rates_2019
  )
}
