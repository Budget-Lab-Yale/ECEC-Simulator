#------------------------------------------------------------------------------
# 15_state_age_ratios.R -- writes config/state/aging/state_age_ratios.csv.
#
# WHY. Simulator ages 2019 ACS weights w/ national population growth by age
# and marital status. State run: state's own growth by age differs. Ratio
# = state growth / national growth by single year of age, 2019 -> year.
# 3b age_demographics() multiplies national factor by ratio when
# ECEC_STATE_AGING=1 (off default).
#
# INPUT. Census Population Estimates, civilian state population by single year
# of age (0-85+), both sexes -- two vintages chained so each ratio of years
# comes from one vintage:
#   vintage 2020 (2010-2020): 2019 -> 2020
#   vintage 2024 (2020-2024): 2020 -> 2024
#   state_data/raw/US/US_Census__state_population_by_age_sex__vintage{2020,2024}.csv
# Growth(state, age, t) = P_t / P_2019 chained as above. Years after 2024 take
# 2024 ratio (no projection). NYC takes NY ratio (parent state).
#
# Usage: Rscript state_data/code/15_state_age_ratios.R [--check]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})

source('src/shared_functions/constants.R')   # STATE_POSTAL_TO_FIPS

RAW   <- file.path('state_data', 'raw', 'US')
V2020 <- file.path(RAW, 'US_Census__state_population_by_age_sex__vintage2020.csv')
V2024 <- file.path(RAW, 'US_Census__state_population_by_age_sex__vintage2024.csv')
OUT   <- file.path('config', 'state', 'aging', 'state_age_ratios.csv')
CHECK <- '--check' %in% commandArgs(TRUE)

for (p in c(V2020, V2024)) {
  if (!file.exists(p)) {
    cat('MISSING INPUT: ', p, '\n', sep = '')
    quit(status = 1)
  }
}



read_pop <- function(path, years) {

  #----------------------------------------------------------------------------
  # Both-sex population by state (FIPS 0 = US) and single year of age.
  #
  # Params:
  #   - path (chr): Census SC-EST csv
  #   - years (int): estimate years to keep
  #
  # Returns: (tibble) fips, age, year, pop
  #----------------------------------------------------------------------------

  read_csv(path, show_col_types = FALSE) %>%
    filter(SEX == 0, AGE <= 85) %>%
    select(fips = STATE, age = AGE, matches('^POPEST\\d{4}_CIV$')) %>%
    pivot_longer(starts_with('POPEST'), names_to = 'year', values_to = 'pop') %>%
    mutate(year = as.integer(sub('^POPEST(\\d{4})_CIV$', '\\1', year))) %>%
    filter(year %in% years)
}



growth <- function(d, from, to) {

  #----------------------------------------------------------------------------
  # Population growth from -> to by fips and age.
  #
  # Params:
  #   - d (tibble): read_pop() output
  #   - from, to (int): years
  #
  # Returns: (tibble) fips, age, g
  #----------------------------------------------------------------------------

  d %>%
    filter(year %in% c(from, to)) %>%
    mutate(year = if_else(year == from, 'a', 'b')) %>%
    pivot_wider(names_from = year, values_from = pop) %>%
    transmute(fips, age, g = b / a)
}



p20 <- read_pop(V2020, c(2019, 2020))
p24 <- read_pop(V2024, 2020:2024)
g19_20 <- growth(p20, 2019, 2020)

# Chained growth 2019 -> t, t = 2020..2024
chain <- bind_rows(lapply(2020:2024, function(t) {
  g_t <- if (t == 2020) g19_20 %>% mutate(g = g) else {
    growth(p24, 2020, t) %>% inner_join(g19_20, by = c('fips', 'age'), suffix = c('', '_0')) %>%
      transmute(fips, age, g = g * g_0)
  }
  mutate(g_t, year = t)
}))

natl <- chain %>% filter(fips == 0) %>% select(age, year, g_us = g)

ratios <- chain %>%
  filter(fips != 0) %>%
  inner_join(natl, by = c('age', 'year')) %>%
  mutate(state = names(STATE_POSTAL_TO_FIPS)[match(fips, STATE_POSTAL_TO_FIPS)],
         ratio = g / g_us) %>%
  filter(!is.na(state)) %>%
  select(state, age, year, ratio) %>%
  arrange(state, year, age)

# NYC takes NY ratio
ratios <- bind_rows(ratios, ratios %>% filter(state == 'NY') %>% mutate(state = 'NYC'))

stopifnot(!anyNA(ratios$ratio), all(is.finite(ratios$ratio)),
          length(unique(ratios$state)) == 52)

cat('state_age_ratios: ', nrow(ratios), ' rows, ', length(unique(ratios$state)),
    ' geographies, ages 0-85, years 2020-2024\n', sep = '')
cat('ratio range, children 0-4 in 2024: ',
    paste(round(range(ratios$ratio[ratios$age <= 4 & ratios$year == 2024]), 3), collapse = ' to '),
    '\n', sep = '')
if (!CHECK) {
  dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
  write_csv(ratios, OUT)
  cat('wrote ', OUT, '\n', sep = '')
}
