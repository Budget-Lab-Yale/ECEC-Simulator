# Home staffing audit; capacity-matched configurations, unresolved cases explicit.
suppressPackageStartupMessages({library(dplyr); library(readr)})

review_home_staffing_inputs <- function() {

  # Returns: (df) 52 geography records; no inference of adult hours from headcounts.
  rules <- read_csv('resources/supply/licensing_ratios_reviewed_2019.csv',
                    show_col_types = FALSE) %>% filter(setting == 'home')
  configurations <- read_csv('resources/licensing/licensing_2019_home_configurations.csv',
                             show_col_types = FALSE)
  sources <- read_csv('resources/licensing/licensing_2019_sources.csv', show_col_types = FALSE)
  stopifnot(nrow(rules) == 52, !anyDuplicated(rules$state),
            !anyDuplicated(configurations[c('state', 'configuration')]),
            !anyDuplicated(sources$source_id))
  verified <- configurations %>%
    filter(status == 'historical_rule_configuration_verified',
           is.finite(max_preschool_children), is.finite(required_adults), required_adults > 0)
  if (any(!verified$source_id %in% sources$source_id)) stop('Unregistered home configuration source.')
  matches <- rules %>% select(state, capacity = children_per_staff) %>%
    inner_join(verified, by = 'state', relationship = 'many-to-many') %>%
    filter(capacity == max_preschool_children)
  matched <- matches %>% group_by(state) %>% summarise(
    minimum_documented_adults = min(required_adults),
    maximum_documented_adults = max(required_adults),
    matching_configurations = paste(configuration, collapse = ';'),
    configuration_sources = paste(unique(source_id), collapse = ';'),
    configuration_conditions = paste(conditions, collapse = ' | '), .groups = 'drop')
  result <- rules %>% transmute(geo = state, selected_preschool_capacity = children_per_staff,
    capacity_measure = measure, capacity_source = source, capacity_locator = locator,
    capacity_conditions = notes) %>% left_join(matched, by = c('geo' = 'state')) %>%
    mutate(adult_count_status = if_else(is.na(minimum_documented_adults),
      'unresolved_configuration_mapping', 'verified_conditional_configurations'),
      age_standardization_status = 'unresolved_common_age_mix',
      adult_hours_status = 'unresolved_hours_per_helper',
      occupancy_status = 'unresolved_own_resident_and_vacant_places',
      provider_mix_status = 'unresolved_category_weights',
      ready_for_production = FALSE)
  stopifnot(nrow(result) == 52, !anyDuplicated(result$geo))
  write_csv(result, 'state_data/out/home_staffing_input_review.csv')
  cat('Home input audit:', sum(!is.na(result$minimum_documented_adults)),
      'of 52 geographies have capacity-matched adult configurations.\n')
  cat('Common age mix, helper hours, occupancy, category weights remain unresolved.\n')
  invisible(result)
}

if (sys.nframe() == 0) review_home_staffing_inputs()
