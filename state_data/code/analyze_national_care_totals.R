# State targets vs price-adjusted national calibration; NYC excluded from sums.
suppressPackageStartupMessages({library(dplyr); library(readr)})
source('src/shared_functions/constants.R')
source('state_data/code/00_build_demand_targets.R')
geos <- names(STATE_POSTAL_TO_FIPS)
p0 <- read_csv('resources/acs/US_ACS__p0_children_by_care_type_and_hours__2019.csv',
               show_col_types = FALSE) %>% filter(geo %in% geos, ecec_type %in% names(P0_TYPE_MAP))
stopifnot(setequal(unique(p0$geo), geos), all(p0$hours_choice %in% c('pt', 'ft')))
national <- p0 %>% mutate(ecec_type = unname(P0_TYPE_MAP[ecec_type]),
                          hours = children * ifelse(hours_choice == 'pt', 500, 2000)) %>%
  group_by(ecec_type) %>% summarise(national_calibration_hours = sum(hours), .groups = 'drop')
states <- bind_rows(lapply(geos, function(geo) {
  x <- read_csv(file.path('config', 'state', 'demand', paste0(geo, '.csv')), show_col_types = FALSE)
  stopifnot(nrow(x) == 6, setequal(x$ecec_type, ECEC_TYPES))
  x
})) %>% group_by(ecec_type) %>% summarise(state_sum_hours = sum(annual_hours), .groups = 'drop')
report <- inner_join(states, national, by = 'ecec_type') %>%
  mutate(gap_hours = state_sum_hours - national_calibration_hours,
         gap_pct = 100 * gap_hours / national_calibration_hours,
         reference = 'National price-adjusted calibration 202610191203; before local re-anchoring')
stopifnot(nrow(report) == 6, all(is.finite(report$gap_pct)))
write_csv(report, 'state_data/out/national_care_totals_comparison.csv')
print(as.data.frame(report %>% select(-reference)), row.names = FALSE)
cat('Weighted annual child-hours; 50 states + DC; NYC excluded. Reference uses RPP-adjusted national calibration.\n')
