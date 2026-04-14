get_employment_rate_growth_factor <- function(macro_projections, base_year, target_year,
                                             column = NULL) {

  #----------------------------------------------------------------------------
  # Calculates employment rate growth factors by sex x age bin from base_year
  # to target_year.
  #
  # Uses CBO epop projections aggregated to broad age bins using population
  # weights from macro projections.
  #
  # Params:
  #   - macro_projections (df): Macro projections table (for population weights)
  #   - base_year (int): Base year (e.g., 2019 for NSECE)
  #   - target_year (int): Target year to project to
  #   - column (chr): Unused, for backwards compatibility
  #
  # Returns:
  #   Named numeric vector with 8 growth factors:
  #   c(F_u25, M_u25, F_25_35, M_25_35, F_35_45, M_35_45, F_45plus, M_45plus)
  #----------------------------------------------------------------------------

  # Define group names to match EMPLOYMENT_TARGETING_GROUPS
  group_names <- c('F_u25', 'M_u25', 'F_25_35', 'M_25_35',
                   'F_35_45', 'M_35_45', 'F_45plus', 'M_45plus')

  # Return 1.0 for all groups if target year <= base year
  if (target_year <= base_year) {
    return(setNames(rep(1.0, 8), group_names))
  }

  # Load epop projections
  epop_path <- 'resources/epop/epop_projections.csv'
  if (!file.exists(epop_path)) {
    warning('get_employment_rate_growth_factor: epop projections not found at ', epop_path,
            '. Returning 1.0 for all groups.')
    return(setNames(rep(1.0, 8), group_names))
  }

  epop <- read_csv(epop_path, show_col_types = FALSE)

  # Define mapping from broad bins to fine epop bins
  bin_map <- list(
    u25 = c('16_17', '18_19', '20_24'),
    '25_35' = c('25_29', '30_34'),
    '35_45' = c('35_39', '40_44'),
    '45plus' = c('45_49', '50_54', '55_59')
  )

  # Get population weights by age bin from macro_projections
  pop_row <- macro_projections %>% filter(year == !!base_year)
  bin_ages <- list(
    '16_17' = 16:17, '18_19' = 18:19, '20_24' = 20:24,
    '25_29' = 25:29, '30_34' = 30:34, '35_39' = 35:39,
    '40_44' = 40:44, '45_49' = 45:49, '50_54' = 50:54, '55_59' = 55:59
  )
  pop_weights <- if (nrow(pop_row) == 0) {
    warning(paste0('get_employment_rate_growth_factor: no population data for year ', base_year))
    setNames(rep(0, length(bin_ages)), names(bin_ages))
  } else {
    sapply(names(bin_ages), function(bin) {
      total <- 0
      for (age in bin_ages[[bin]]) {
        for (prefix in c('married_', 'unmarried_')) {
          col <- paste0(prefix, age)
          if (col %in% names(pop_row)) {
            val <- pop_row[[col]]
            if (!is.na(val)) total <- total + val
          }
        }
      }
      total
    })
  }

  # Compute growth factors for each sex x age bin group
  growth_factors <- numeric(8)
  names(growth_factors) <- group_names

  for (sex in c('F', 'M')) {
    sex_prefix <- if_else(sex == 'F', 'epop_f_', 'epop_m_')

    for (bin_name in names(bin_map)) {
      fine_bins <- bin_map[[bin_name]]
      cols <- paste0(sex_prefix, fine_bins)

      # Filter to columns that exist in the epop data
      cols_exist <- cols[cols %in% names(epop)]
      if (length(cols_exist) == 0) {
        warning(paste0('No epop columns found for ', sex, '_', bin_name))
        next
      }

      # Get epop values for base and target years
      base_row <- epop %>% filter(year == base_year)
      target_row <- epop %>% filter(year == target_year)

      if (nrow(base_row) == 0 || nrow(target_row) == 0) {
        warning(paste0('Missing epop data for base_year=', base_year,
                       ' or target_year=', target_year))
        next
      }

      base_vals <- as.numeric(base_row[, cols_exist])
      target_vals <- as.numeric(target_row[, cols_exist])

      # Get population weights for these bins (corresponding to fine_bins that exist)
      fine_bins_exist <- fine_bins[cols %in% names(epop)]
      weights <- pop_weights[fine_bins_exist]

      # Replace NA values with 0 in weights
      weights[is.na(weights)] <- 0

      # Handle case where all weights are 0 or NA
      if (sum(weights, na.rm = TRUE) <= 0) {
        # Use equal weights if no population data
        weights <- rep(1, length(weights))
      }

      # Compute weighted average epop for base and target years
      # Handle NA values in epop data (e.g., missing cells for older age bins)
      valid_idx <- !is.na(base_vals) & !is.na(target_vals)
      if (sum(valid_idx) == 0) {
        warning(paste0('All epop values are NA for ', sex, '_', bin_name))
        next
      }

      base_epop <- weighted.mean(base_vals[valid_idx], weights[valid_idx], na.rm = TRUE)
      target_epop <- weighted.mean(target_vals[valid_idx], weights[valid_idx], na.rm = TRUE)

      # Compute growth factor
      group_name <- paste0(sex, '_', bin_name)
      if (base_epop > 0) {
        growth_factors[group_name] <- target_epop / base_epop
      } else {
        growth_factors[group_name] <- 1.0
      }
    }
  }

  # Fill any remaining NA values with 1.0
  growth_factors[is.na(growth_factors)] <- 1.0

  return(growth_factors)
}
