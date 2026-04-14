get_hourly_wage_growth_factor <- function(macro_projections, base_year, target_year) {

  #----------------------------------------------------------------------------
  # Calculates hourly wage growth factor from base_year to target_year
  #
  # Uses gdp_wages / agg_hours_index to capture actual hourly earnings growth.
  #
  # Params:
  #   - macro_projections (df): Macro projections table (must have gdp_wages and agg_hours_index)
  #   - base_year (int): Base year (e.g., 2019 for NSECE)
  #   - target_year (int): Target year to project to
  #
  # Returns: (dbl) Hourly wage growth factor
  #----------------------------------------------------------------------------

  if (target_year <= base_year) {
    return(1.0)
  }

  factors <- macro_projections %>%
    filter(year %in% c(base_year, target_year)) %>%
    select(year, gdp_wages, agg_hours_index) %>%
    mutate(
      hourly_wage = gdp_wages / agg_hours_index,
      growth = hourly_wage / lag(hourly_wage)
    ) %>%
    tail(1)

  return(factors$growth)
}
