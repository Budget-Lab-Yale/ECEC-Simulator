#------------------------------------------------------------------------------
# analyze_observed_staffing.R -- observed vs legal staffing by state.
# Diagnostic only, no supply writes.
#
# Observed: ACS 2015-2019 care-worker hours (analyze_observed_staffing_acs.py,
# cluster) / state 2019 care-hour targets (config/state/demand). Index = state
# staff hours per child-hour / PEP children-0-4-weighted mean over 50 states
# + DC, as licensing_indices(). Two centre denominators: all centre hours
# (paid + unpaid) and paid centre hours only -- public pre-K teachers sit in
# elementary schools (IND 7860), outside ACS population, so states w/ large
# pre-K look understaffed on first.
# Legal: licensing_indices() centre index; home index before licensed-share
# blend (home_share = 1).
# NSECE: observed regional centre staff per child (classroom table,
# supply_vs_nsece_cluster_2019.txt) at NSECE centre age weights, home
# children per FTE staff by region -- region benchmark for ACS index.
#
# Usage: Rscript state_data/code/analyze_observed_staffing.R
#   reads state_data/out/acs_childcare_staff_hours_by_state_2019.csv
#   writes state_data/out/observed_staffing_by_state.csv,
#   state_data/out/observed_staffing_summary.txt
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
source('resources/supply/state_supply_params.R')

ACS_HOURS <- 'state_data/out/acs_childcare_staff_hours_by_state_2019.csv'
NSECE_TXT <- 'state_data/out/supply_vs_nsece_cluster_2019.txt'
OUT_CSV <- 'state_data/out/observed_staffing_by_state.csv'
OUT_TXT <- 'state_data/out/observed_staffing_summary.txt'
# Census region (NSECE _REGION: 1 Northeast, 2 Midwest, 3 South, 4 West)
REGION <- local({e <- new.env(); sys.source(NDCP_LOCAL, envir = e); e$REGION_OF})
# Fewer ACS records -> flagged as noisy
MIN_RECORDS <- 100



read_nsece_regional <- function(path = NSECE_TXT) {

  #----------------------------------------------------------------------------
  # NSECE observed regional staffing from cluster supply comparison log.
  # Centre: staff per child = sum_a w_a / observed children per staff (region
  # x age of youngest child), w = NSECE_CENTRE_ENROLLED. Home: 1 / children
  # per FTE staff, Paid Home-Based row.
  #
  # Params:
  #   - path (chr): supply_vs_nsece_cluster_2019.txt
  #
  # Returns: (tibble) region, centre_s, home_s -- staff per child
  #----------------------------------------------------------------------------

  require_input(path, ' -- run state_data/tests/test_supply_vs_nsece_cluster.R on cluster')
  txt <- readLines(path)
  i <- grep('^ region +age +n +obs_cps', txt)
  cls <- read.table(text = txt[(i + 1):(i + 20)],
                    col.names = c('region', 'age', 'n', 'obs_cps', 'legal_cps', 'ratio'))
  w <- NSECE_CENTRE_ENROLLED[NSECE_AGES_0_4]
  centre <- cls %>% group_by(region) %>%
    dplyr::summarise(centre_s = sum(w[age] / obs_cps) / sum(w[age]), .groups = 'drop')
  j <- grep('^ +Paid Home-Based', txt)
  home <- tibble(line = txt[j]) %>%
    mutate(cps = as.numeric(sapply(strsplit(trimws(line), ' +'), `[`, 8)),
           region = as.integer(sapply(strsplit(trimws(line), ' +'), function(x) tail(x, 1)))) %>%
    filter(region > 0) %>% transmute(region, home_s = 1 / cps)
  stopifnot(nrow(centre) == 4, nrow(home) == 4)
  inner_join(centre, home, by = 'region')
}



slope <- function(d, y, x, fe = NULL) {

  #----------------------------------------------------------------------------
  # Log-log slope of y on x across states, PEP children-0-4 weights.
  #
  # Params:
  #   - d (df): one row per state, w/ pop
  #   - y, x (chr): column names
  #   - fe (chr or NULL): fixed-effect column (within-region slope)
  #
  # Returns: (chr) 'slope (se)'
  #----------------------------------------------------------------------------

  f <- as.formula(paste0('log(', y, ') ~ log(', x, ')', if (!is.null(fe)) paste0(' + factor(', fe, ')')))
  m <- summary(lm(f, data = d, weights = pop))$coef
  sprintf('%.3f (se %.3f)', m[2, 1], m[2, 2])
}



analyze_observed_staffing <- function() {

  #----------------------------------------------------------------------------
  # Builds observed state staffing indices, compares w/ legal indices and
  # NSECE regional staffing.
  #
  # Returns: (tibble) one row per state, written to OUT_CSV
  #----------------------------------------------------------------------------

  require_input(ACS_HOURS, ' -- run state_data/code/analyze_observed_staffing_acs.py on cluster')
  # geo = state FIPS or NYC (NYC rows also inside NY)
  acs <- read_csv(ACS_HOURS, col_types = cols(geo = col_character())) %>%
    mutate(state = ifelse(geo == 'NYC', 'NYC',
                          names(STATE_POSTAL_TO_FIPS)[match(geo, as.character(STATE_POSTAL_TO_FIPS))]))
  stopifnot(!anyNA(acs$state))
  staff <- acs %>% group_by(state, sector) %>%
    dplyr::summarise(hours = sum(hours_wt), n = sum(n), .groups = 'drop') %>%
    pivot_wider(names_from = sector, values_from = c(hours, n), values_fill = 0)

  states <- names(STATE_POSTAL_TO_FIPS)
  geos <- c(states, 'NYC')
  targets <- bind_rows(lapply(geos, function(g) {
    read_csv(file.path(DEMAND, paste0(g, '.csv')), show_col_types = FALSE) %>%
      mutate(state = g)
  })) %>% pivot_wider(names_from = ecec_type, values_from = annual_hours)

  county <- read_csv(POP_0_4, col_types = cols(.default = col_character()))
  pop <- pop_0_4_by_state() %>%
    bind_rows(tibble(state = 'NYC', pop = sum(as.numeric(county$TOT_POP[
      as.integer(county$STATE) == 36 & as.integer(county$COUNTY) %in% c(5, 47, 61, 81, 85)]))))

  legal <- licensing_indices(home_share = 1) %>% filter(state %in% geos) %>%
    transmute(state, legal_centre = centre, legal_home = home)
  current <- licensing_indices() %>% filter(state %in% geos) %>%
    transmute(state, current_home = home)

  # Staff hours per child-hour, then index to PEP-weighted mean over 50
  # states + DC (NYC indexed, not in mean)
  d <- staff %>% inner_join(targets, by = 'state') %>% inner_join(pop, by = 'state') %>%
    inner_join(legal, by = 'state') %>% inner_join(current, by = 'state') %>%
    mutate(region = ifelse(state == 'NYC', 1, unname(REGION[state])),
           w_nat = ifelse(state == 'NYC', 0, pop))
  norm <- function(x) x / weighted.mean(x, d$w_nat)
  d <- d %>%
    mutate(centre_all_s = hours_centre / (`Center-Based` + `Unpaid Center-Based`),
           centre_paid_s = hours_centre / `Center-Based`,
           home_s = hours_home / `Paid Home-Based`,
           obs_centre_all = norm(centre_all_s),
           obs_centre_paid = norm(centre_paid_s),
           obs_home = norm(home_s),
           low_n = n_centre < MIN_RECORDS | n_home < MIN_RECORDS)
  stopifnot(nrow(d) == 52, !anyNA(d$region), all(d$pop > 0))
  ds <- d %>% filter(state != 'NYC')

  # Region means, ACS vs NSECE vs legal (all as index to national mean)
  ns <- read_nsece_regional()
  nat <- ds %>% group_by(region) %>% dplyr::summarise(pop = sum(pop), .groups = 'drop') %>%
    inner_join(ns, by = 'region')
  ns <- ns %>% mutate(nsece_centre = centre_s / weighted.mean(nat$centre_s, nat$pop),
                      nsece_home = home_s / weighted.mean(nat$home_s, nat$pop))
  reg <- ds %>% group_by(region) %>%
    dplyr::summarise(across(c(obs_centre_all, obs_centre_paid, legal_centre, obs_home, legal_home),
                            ~ weighted.mean(.x, pop)), .groups = 'drop') %>%
    inner_join(ns %>% select(region, nsece_centre, nsece_home), by = 'region')

  out <- d %>% transmute(state, region, pop, n_centre, n_home, low_n,
                         centre_all_s, centre_paid_s, home_s,
                         obs_centre_all, obs_centre_paid, legal_centre,
                         obs_home, legal_home, current_home)
  write_csv(out, OUT_CSV)

  r3 <- function(x) round(x, 3)
  log_lines <- c(
    paste('script: state_data/code/analyze_observed_staffing.R; run at', format(Sys.time())),
    paste('inputs:', ACS_HOURS, '|', NSECE_TXT, '|', DEMAND, '|', RATIOS),
    '',
    '== national level: ACS staff hours per child-hour (50 states + DC, totals)',
    sprintf('  centre, all centre hours %.3f | paid centre hours %.3f | home %.3f',
            sum(ds$hours_centre) / sum(ds$`Center-Based` + ds$`Unpaid Center-Based`),
            sum(ds$hours_centre) / sum(ds$`Center-Based`),
            sum(ds$hours_home) / sum(ds$`Paid Home-Based`)),
    sprintf('  ACS records: centre %d, home %d; geos under %d records in a sector: %d',
            sum(ds$n_centre), sum(ds$n_home), MIN_RECORDS, sum(ds$low_n)),
    '',
    '== log slope, observed index on legal index across states (PEP weights)',
    paste('  centre (all hours)  pooled', slope(ds, 'obs_centre_all', 'legal_centre'),
          '| within region', slope(ds, 'obs_centre_all', 'legal_centre', 'region')),
    paste('  centre (paid hours) pooled', slope(ds, 'obs_centre_paid', 'legal_centre'),
          '| within region', slope(ds, 'obs_centre_paid', 'legal_centre', 'region')),
    paste('  home                pooled', slope(ds, 'obs_home', 'legal_home'),
          '| within region', slope(ds, 'obs_home', 'legal_home', 'region')),
    '',
    '== region means (index, 1 = national), ACS vs NSECE observed vs legal',
    capture.output(print(as.data.frame(reg %>% mutate(across(-region, r3))), row.names = FALSE)),
    '',
    '== geographies (NYC also inside NY), sorted by ACS centre index (all hours)',
    capture.output(print(as.data.frame(out %>% arrange(obs_centre_all) %>%
      select(state, region, n_centre, n_home, obs_centre_all, obs_centre_paid, legal_centre,
             obs_home, legal_home) %>% mutate(across(where(is.double), r3))), row.names = FALSE)))
  writeLines(log_lines, OUT_TXT)
  cat(log_lines, sep = '\n')
  invisible(out)
}



if (sys.nframe() == 0) analyze_observed_staffing()
