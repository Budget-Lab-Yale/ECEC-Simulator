get_nsece_dollar_conversion <- function(nsece_year, base_year, macro_projections_path) {

  #----------------------------------------------------------------------------
  # Returns the factors that convert NSECE survey-year dollars into the
  # model's base-year (ACS year) dollars, using the same indices the model
  # uses to grow supply-side values forward in simulation: care worker wages
  # move with the hourly wage index (gdp_wages / agg_hours_index) and the
  # non-labor part of provider prices (delta_0) and the Other Paid price move
  # with CPI-U. Dividing a survey-year value by its factor gives base-year
  # dollars. Both factors are 1 when the survey year equals the base year.
  #
  # Params:
  #   - nsece_year (int): NSECE survey year (e.g. 2024)
  #   - base_year (int): Model base year, the paired ACS year (e.g. 2023)
  #   - macro_projections_path (chr): Macro-Projections folder holding
  #       historical.csv and projections.csv
  #
  # Returns: (list) wage_factor (dbl), cpi_factor (dbl), nsece_year (int),
  #   base_year (int)
  #----------------------------------------------------------------------------

  if (nsece_year == base_year) {
    return(list(wage_factor = 1, cpi_factor = 1, nsece_year = nsece_year, base_year = base_year))
  }
  if (nsece_year < base_year) {
    stop('get_nsece_dollar_conversion: NSECE year ', nsece_year, ' precedes base year ', base_year,
         '. Only conversion from a later survey year back to the base year is supported.')
  }

  macro_projections <- bind_rows(
    read_csv(file.path(macro_projections_path, 'historical.csv'),  show_col_types = FALSE),
    read_csv(file.path(macro_projections_path, 'projections.csv'), show_col_types = FALSE)
  ) %>%
    distinct(year, .keep_all = TRUE)

  list(
    wage_factor = get_hourly_wage_growth_factor(macro_projections, base_year, nsece_year),
    cpi_factor  = get_cpi_growth_factor(macro_projections, base_year, nsece_year),
    nsece_year  = nsece_year,
    base_year   = base_year
  )
}
