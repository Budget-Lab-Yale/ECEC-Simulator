get_macro_ratio <- function(macro_projections, base_year, target_year, col, label) {

  #----------------------------------------------------------------------------
  # Computes the ratio of a macro projection column between two years.
  # Shared helper for CPI and chained-CPI growth factors.
  #
  # Params:
  #   - macro_projections (df): Macro projection table
  #   - base_year (int): Base year
  #   - target_year (int): Target year
  #   - col (chr): Column name to compute ratio for
  #   - label (chr): Label for error messages
  #
  # Returns: (dbl) Ratio of target_year / base_year values
  #----------------------------------------------------------------------------

  if (target_year <= base_year) return(1.0)

  if (!(col %in% names(macro_projections))) {
    stop(label, ': missing ', col, ' column in macro_projections.')
  }

  vals <- macro_projections %>%
    filter(year %in% c(base_year, target_year)) %>%
    select(year, value = all_of(col))

  if (!all(c(base_year, target_year) %in% vals$year)) {
    stop(label, ': missing data for base_year=', base_year,
         ' or target_year=', target_year, '.')
  }

  base_value <- vals %>% filter(year == base_year) %>% pull(value)
  target_value <- vals %>% filter(year == target_year) %>% pull(value)

  if (length(base_value) != 1 || length(target_value) != 1 ||
      !is.finite(base_value) || !is.finite(target_value) ||
      base_value <= 0 || target_value <= 0) {
    stop(label, ': invalid ', col, ' values for base_year=', base_year,
         ', target_year=', target_year, '.')
  }

  target_value / base_value
}



get_cpi_growth_factor <- function(macro_projections, base_year, target_year) {

  #----------------------------------------------------------------------------
  # Calculates CPI-U growth factor from base_year to target_year.
  #
  # Params:
  #   - macro_projections (df): Macro projection table
  #   - base_year (int): Base year
  #   - target_year (int): Target year
  #
  # Returns: (dbl) CPI-U growth factor
  #----------------------------------------------------------------------------

  get_macro_ratio(macro_projections, base_year, target_year, 'cpiu', 'get_cpi_growth_factor')
}
