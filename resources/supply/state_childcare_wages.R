#------------------------------------------------------------------------------
# state_childcare_wages.R -- state wage ratios for config/state/supply/<ST>.yaml.
#
# The supply block's `wages` is w[no_ba], w[ba] -- an hourly wage RATE by
# education, derived nationally from NSECE by 1a_nsece_processing.R. This emits
# the state-to-national ratio for the same quantity, so a state supply yaml can
# scale the national wages without re-deriving anything else.
#
# WHY A RATIO. The national w comes from NSECE and the state variation comes
# from ACS. Taking ACS as a ratio keeps NSECE's level and definition -- which
# the rest of the supply block is built on -- and imports only the thing ACS
# measures better, which is how far a state sits from the national wage.
#
# WHY ACS, NOT QCEW. QCEW's annual_avg_wkly_wage is weekly EARNINGS, so it
# mixes the wage with hours worked; childcare part-time incidence varies by
# state, and a high-part-time state would read as low-wage at an identical
# hourly rate. QCEW also counts everyone on a child day care payroll rather
# than the teaching labour L_req describes, gives no education split, and
# largely excludes the self-employed -- who are most of Paid Home-Based.
#
# SOURCE. IPUMS ACS extract usa:33, summarised by acs_ccwage_2019.py: hourly
# EARNINGS (wage + business income, so self-employed providers count), weeks
# from continuous WKSWORK1 where asked, 80 replicate weights for SEs.
#
# SAMPLE. Ratio from 2018, 2019 records of 2015-2019 ACS 5-year (sample
# acs5_2018_2019) -- base year + year before only, no 2015-2017 records.
# ~2x records of 2019 1-year. Income in 2019 dollars (IPUMS standardises
# multi-year income to final year). 2019 1-year ratio kept alongside as check
# (ratio_acs1, se_ratio_acs1), not used.
#
# Usage:
#   Rscript resources/supply/state_childcare_wages.R
#   Rscript resources/supply/state_childcare_wages.R --check
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr)
})

DIR <- file.path('resources', 'supply')
ACS <- file.path(DIR,
                 'US_ACS__childcare_hourly_wage_by_state_and_education__2019.csv')
SAMPLE       <- 'acs5_2018_2019'
SAMPLE_CHECK <- 'acs1_2019'
OUT <- file.path(DIR, 'state_childcare_wage_ratios.csv')

# State x education cell under this many ACS workers not trusted -- ratio set
# to 1, state keeps national wage for that education group. In 2018 1-year,
# Vermont had 13 no-BA, 18 BA records.
MIN_N <- 50


# Postal -> FIPS, from the simulator's own table.
source('src/shared_functions/constants.R')
POSTAL_TO_FIPS <- STATE_POSTAL_TO_FIPS

# Every state + DC -- demand build covers all 51. No Puerto Rico records in these ACS data.
STATES <- names(POSTAL_TO_FIPS)



main <- function() {

  #----------------------------------------------------------------------------
  # Entry point -- state-to-national childcare hourly-earnings ratio by
  # education from SAMPLE; cells under MIN_N workers take ratio 1. Joins
  # SAMPLE_CHECK ratio as check, writes OUT, prints table.
  #
  # Params: none -- --check on CLI skips write, exits 1 if any state lacks
  #   ACS cell; exits 1 if ACS input missing or national rows != 2
  #
  # Returns: (tibble) one row per state x education group, invisibly
  #----------------------------------------------------------------------------

  check_only <- '--check' %in% commandArgs(TRUE)

  if (!file.exists(ACS)) {
    cat('MISSING INPUT: ', ACS, '\n', sep = '')
    quit(status = 1)
  }

  all_rows <- read_csv(ACS, show_col_types = FALSE) %>%
    filter(educ_group != 'all', sector == 'all', occ_group == 'all')
  d <- all_rows %>% filter(sample == SAMPLE)
  chk <- all_rows %>% filter(sample == SAMPLE_CHECK, state_fips != 0) %>%
    select(state_fips, educ_group, n_acs1 = n, ratio_acs1 = ratio_to_national,
           se_ratio_acs1 = se_ratio)

  nat <- d %>% filter(state_fips == 0) %>%
    select(educ_group, nat_hourly = mean_hourly)
  if (nrow(nat) != 2) {
    cat('the ACS file must carry both education groups nationally (state_fips 0)\n')
    quit(status = 1)
  }

  fips <- POSTAL_TO_FIPS[STATES]

  out <- d %>%
    filter(state_fips %in% fips) %>%
    left_join(nat, by = 'educ_group') %>%
    mutate(state = names(POSTAL_TO_FIPS)[match(state_fips, POSTAL_TO_FIPS)],
           thin  = n < MIN_N,
           ratio_raw = mean_hourly / nat_hourly,
           wage_ratio = if_else(thin, 1, ratio_raw)) %>%
    left_join(chk, by = c('state_fips', 'educ_group')) %>%
    select(state, state_fips, educ_group, n, mean_hourly, nat_hourly,
           ratio_raw, se_ratio, thin, wage_ratio,
           n_acs1, ratio_acs1, se_ratio_acs1) %>%
    arrange(state, educ_group)

  missing <- setdiff(STATES, unique(out$state))
  if (length(missing)) {
    cat('NO ACS CELL FOR: ', paste(missing, collapse = ', '), '\n', sep = '')
    if (check_only) quit(status = 1)
  }

  if (!check_only) write_csv(out, OUT)

  cat('== childcare hourly earnings, ACS 2018-2019 records, state against national\n')
  cat('   national: ')
  cat(paste(sprintf('%s $%.2f', nat$educ_group, nat$nat_hourly),
            collapse = '   '), '\n\n')
  print(as.data.frame(out %>%
    mutate(across(c(mean_hourly, nat_hourly), ~round(.x, 2)),
           across(c(ratio_raw, se_ratio, wage_ratio, ratio_acs1), ~round(.x, 4))) %>%
    select(state, educ_group, n, mean_hourly, ratio_raw, se_ratio, thin,
           wage_ratio, n_acs1, ratio_acs1)),
    row.names = FALSE)

  th <- out %>% filter(thin)
  cat('\n', nrow(th), ' cell(s) under ', MIN_N,
      ' workers take the national wage: ', sep = '')
  cat(if (nrow(th)) paste(unique(paste0(th$state, '/', th$educ_group)),
                          collapse = ', ') else 'none', '\n', sep = '')
  if (!check_only) cat('wrote ', OUT, '\n', sep = '')
  invisible(out)
}



if (sys.nframe() == 0) main()
