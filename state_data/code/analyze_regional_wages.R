#------------------------------------------------------------------------------
# analyze_regional_wages.R -- model 2019 state wages vs NSECE regional centre
# staff wages, by Census region x education. Diagnostic only, no supply writes.
#
# Model: config/state/supply/<ST>.yaml wages (NSECE national x OEWS state
# ratio), 50 states + DC, averaged to region two ways:
#   - children: PEP 2019 children 0-4 weights
#   - staff: ACS 2015-2019 centre care-worker hours by state of work, same
#     education group (analyze_observed_staffing_acs.py) -- closest to NSECE
#     worker weights
# NSECE: nsece_wage_region.R cells w/ design SE. Gap = model / NSECE - 1; z =
# (model - NSECE) / SE (NSECE sampling only; OEWS, ACS taken as exact).
#
# Usage: Rscript state_data/code/analyze_regional_wages.R
#   writes state_data/out/regional_wage_comparison.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(yaml); library(tidyr)})
# pop_0_4_by_state(), NDCP_LOCAL
source('resources/supply/state_supply_params.R')

NSECE_WAGES <- 'resources/supply/US_NSECE__wage_region__2019.csv'
ACS_HOURS <- 'state_data/out/acs_childcare_staff_hours_by_state_2019.csv'
OUT_CSV <- 'state_data/out/regional_wage_comparison.csv'
# Census region (NSECE _REGION: 1 Northeast, 2 Midwest, 3 South, 4 West)
REGION <- local({e <- new.env(); sys.source(NDCP_LOCAL, envir = e); e$REGION_OF})
region_of <- function(g) as.integer(unname(REGION[g]))



analyze_regional_wages <- function() {

  #----------------------------------------------------------------------------
  # Region x education comparison of model wages w/ NSECE.
  #
  # Returns: (tibble) region, educ, nsece, se, model_children, model_staff,
  #   gaps, z -- written to OUT_CSV
  #----------------------------------------------------------------------------

  states <- names(STATE_POSTAL_TO_FIPS)
  wages <- bind_rows(lapply(states, function(g) {
    y <- read_yaml(file.path('config', 'state', 'supply', paste0(g, '.yaml')))
    tibble(state = g, no_ba = y$wages$no_ba, ba = y$wages$ba)
  })) %>% pivot_longer(c(no_ba, ba), names_to = 'educ', values_to = 'wage')

  pop <- pop_0_4_by_state()
  staff <- read_csv(ACS_HOURS, col_types = cols(geo = col_character())) %>%
    filter(geo != 'NYC', sector == 'centre') %>%
    mutate(state = names(STATE_POSTAL_TO_FIPS)[match(geo, as.character(STATE_POSTAL_TO_FIPS))]) %>%
    select(state, educ = educ_group, staff_hours = hours_wt, wage_wt, wage_mean)

  d <- wages %>% inner_join(pop, by = 'state') %>% inner_join(staff, by = c('state', 'educ')) %>%
    mutate(region = region_of(state))
  stopifnot(nrow(d) == 2 * length(states))

  model <- bind_rows(d %>% group_by(region, educ),
                     d %>% mutate(region = 0L) %>% group_by(region, educ)) %>%
    summarise(model_children = weighted.mean(wage, pop),
              model_staff = weighted.mean(wage, staff_hours),
              acs = weighted.mean(wage_mean, wage_wt), .groups = 'drop')

  ns <- read_csv(NSECE_WAGES, show_col_types = FALSE) %>% select(region, educ, nsece = est, se, n)
  out <- ns %>% inner_join(model, by = c('region', 'educ')) %>%
    mutate(gap_children = model_children / nsece - 1, gap_staff = model_staff / nsece - 1,
           z_children = (model_children - nsece) / se, z_staff = (model_staff - nsece) / se) %>%
    group_by(educ) %>%
    # Region / national, each source -- ACS level differs (self-reported
    # earnings, 2015-2019), pattern comparable
    mutate(nsece_rel = nsece / nsece[region == 0], model_rel = model_staff / model_staff[region == 0],
           acs_rel = acs / acs[region == 0], se_rel = se / nsece[region == 0]) %>%
    ungroup() %>%
    arrange(educ, region)
  stopifnot(nrow(out) == 10)
  write_csv(out, OUT_CSV)
  print(as.data.frame(out %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
  invisible(out)
}



if (sys.nframe() == 0) analyze_regional_wages()
