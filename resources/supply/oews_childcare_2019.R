#------------------------------------------------------------------------------
# oews_childcare_2019.R -- childcare employment, wages by state from OEWS May 2019.
#
# Supply-side wages, employment from BLS occupational codes. Every SOC 2018
# occupation for care of children under school age -- four:
#
#   39-9011  Childcare Workers                                     -> no_ba
#   25-2011  Preschool Teachers, Except Special Education          -> ba
#   25-2051  Special Education Teachers, Preschool                 -> ba
#   11-9031  Education and Childcare Administrators, Preschool and
#            Daycare                                                -> ba
#
# NOT INCLUDED, each checked in this file: kindergarten, elementary, other
# special-education teachers (school-age children); 25-9045 Teaching
# Assistants, Except Postsecondary (one code preschool through grade 12,
# mostly K-12); recreation workers, personal care aides, child and family
# social workers, personal-service supervisors (not childcare).
#
# OCCUPATION -> EDUCATION GROUP. OEWS has no education detail; simulator
# prices two labour types. Childcare workers -> no_ba; preschool teachers,
# special-education preschool teachers, administrators -> ba. A mapping by
# occupation, not a measured degree split.
#
# ANNUAL-ONLY PAY. BLS publishes 25-2051 pay as annual only (ANNUAL flag, no
# H_MEAN). Hourly = A_MEAN / 2,080 -- OEWS's own hourly-annual rule. Sets the
# occupation's weight inside ba only; 2,080 cancels in state / national.
#
# WAGE RATIO = state hourly mean / national hourly mean, per group; ba
# employment-weighted over its three occupations. Ratio, not level -- keeps
# NSECE's national wage definition, which rest of parameter set built on.
# National = all 50 states + DC from this file, employment-weighted -- the
# state file carries no national row. A suppressed state cell ('*', '**',
# '#') takes ratio 1, national wage kept.
#
# EMPLOYMENT. Workers per state and group (TOT_EMP). Feeds
#   - state labour-supply FALLBACK: national NSECE L_0 x state share of
#     national OEWS employment (state_supply_params.R)
#   - VALIDITY CHECK: national NSECE L_0 (hours) against national OEWS
#     employment x 2,080 hours (OEWS's own annual basis). OEWS covers wage
#     and salary jobs only -- no self-employed, so most home-based providers
#     are absent; the gap measures that.
#
# Input: state_data/raw/US_OEWS__occupational_employment_and_wages_by_state__2019-05.zip
#        (BLS oesm19st.zip, state_M2019_dl.xlsx)
# Output: resources/supply/US_OEWS__childcare_employment_and_wages_by_state__2019.csv
#         resources/supply/state_childcare_wage_ratios_oews.csv
#
# Usage (cluster):
#   Rscript resources/supply/oews_childcare_2019.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(readxl); library(yaml)
})

ZIP      <- file.path('state_data', 'raw',
                      'US_OEWS__occupational_employment_and_wages_by_state__2019-05.zip')
XLSX     <- 'oesm19st/state_M2019_dl.xlsx'
NATIONAL <- file.path('resources', 'supply', 'US_NSECE__national_supply_parameters__2019.yaml')
OUT      <- file.path('resources', 'supply', 'US_OEWS__childcare_employment_and_wages_by_state__2019.csv')
RATIOS   <- file.path('resources', 'supply', 'state_childcare_wage_ratios_oews.csv')

OCC_GROUP <- c('39-9011' = 'no_ba', '25-2011' = 'ba', '25-2051' = 'ba', '11-9031' = 'ba')
OEWS_ANNUAL_HOURS <- 2080

source('src/shared_functions/constants.R')



read_oews <- function() {

  #----------------------------------------------------------------------------
  # State rows for OCC_GROUP occupations, numeric where published. Annual-only
  # occupation: h_mean = A_MEAN / 2,080, wage_basis says so.
  #
  # Returns: (tibble) state, occ_code, occ_title, educ_group, tot_emp,
  #          emp_prse, h_mean, wage_basis, mean_prse; NA where suppressed
  #----------------------------------------------------------------------------

  if (!file.exists(ZIP)) stop('missing ', ZIP)
  td <- tempfile(); dir.create(td)
  utils::unzip(ZIP, files = XLSX, exdir = td)
  d <- read_excel(file.path(td, XLSX), col_types = 'text')
  names(d) <- toupper(names(d))

  fips_to_postal <- setNames(names(STATE_POSTAL_TO_FIPS), STATE_POSTAL_TO_FIPS)
  n <- function(x) suppressWarnings(as.numeric(gsub(',', '', x)))

  out <- d %>%
    filter(OCC_CODE %in% names(OCC_GROUP)) %>%
    transmute(state      = unname(fips_to_postal[as.character(as.integer(AREA))]),
              occ_code   = OCC_CODE, occ_title = OCC_TITLE,
              educ_group = unname(OCC_GROUP[OCC_CODE]),
              tot_emp    = n(TOT_EMP), emp_prse = n(EMP_PRSE),
              annual     = toupper(ANNUAL) %in% 'TRUE',
              h_mean     = dplyr::if_else(annual, n(A_MEAN) / OEWS_ANNUAL_HOURS, n(H_MEAN)),
              wage_basis = dplyr::if_else(annual, 'annual / 2080', 'hourly'),
              mean_prse  = n(MEAN_PRSE)) %>%
    select(-annual) %>%
    filter(!is.na(state))

  miss <- setdiff(names(STATE_POSTAL_TO_FIPS), out$state)
  if (length(miss)) stop('OEWS file lacks states: ', paste(miss, collapse = ', '))

  # State x occupation cells w/ no row at all -- BLS publishes no estimate.
  # Said, not dropped silently: group wage then rests on its other occupations.
  absent <- tidyr::expand_grid(state = names(STATE_POSTAL_TO_FIPS),
                               occ_code = names(OCC_GROUP)) %>%
    anti_join(out, by = c('state', 'occ_code'))
  if (nrow(absent)) {
    cat('no OEWS row (not published): ',
        paste(absent$state, absent$occ_code, collapse = ', '), '\n', sep = '')
  }
  out
}



main_oews <- function() {

  d <- read_oews()
  cat('OEWS May 2019: ', nrow(d), ' state x occupation rows\n', sep = '')
  supp <- d %>% filter(is.na(tot_emp) | is.na(h_mean))
  if (nrow(supp)) {
    cat('suppressed cells:\n'); print(as.data.frame(select(supp, state, occ_code, tot_emp, h_mean)))
  }

  # National = 50 states + DC, employment-weighted
  natl <- d %>%
    filter(!is.na(tot_emp), !is.na(h_mean)) %>%
    group_by(occ_code) %>%
    # h_mean before tot_emp -- summarise() would otherwise read the summed
    # tot_emp (README known trap)
    summarise(h_mean = sum(h_mean * tot_emp) / sum(tot_emp), tot_emp = sum(tot_emp),
              .groups = 'drop') %>%
    mutate(state = 'US', educ_group = unname(OCC_GROUP[occ_code]))
  write_csv(bind_rows(d, natl) %>% arrange(state, occ_code), OUT)
  cat('wrote', OUT, '\n')

  # Group wages: employment-weighted over a group's published occupations
  grp <- function(x) x %>%
    filter(!is.na(tot_emp), !is.na(h_mean)) %>%
    group_by(state, educ_group) %>%
    summarise(workers = sum(tot_emp), wage = sum(h_mean * tot_emp) / sum(tot_emp),
              .groups = 'drop')
  g  <- grp(d)
  gn <- grp(natl) %>% select(educ_group, natl_wage = wage, natl_workers = workers)

  ratios <- tidyr::expand_grid(state = names(STATE_POSTAL_TO_FIPS),
                               educ_group = c('no_ba', 'ba')) %>%
    left_join(g, by = c('state', 'educ_group')) %>%
    left_join(gn, by = 'educ_group') %>%
    mutate(wage_ratio = dplyr::coalesce(wage / natl_wage, 1),
           source     = if_else(is.na(wage), 'suppressed -> 1', 'oews'),
           emp_share  = workers / natl_workers)
  write_csv(ratios, RATIOS)
  cat('wrote', RATIOS, '\n')

  ## validity check -- NSECE national labour vs OEWS national employment ----
  ns <- read_yaml(NATIONAL)
  cat('\n== national NSECE labour supply vs OEWS employment x ', OEWS_ANNUAL_HOURS,
      ' hours\n', sep = '')
  for (eg in c('no_ba', 'ba')) {
    nsece_h <- ns$labor_supply[[eg]]
    oews_w  <- gn$natl_workers[gn$educ_group == eg]
    cat(sprintf('  %-6s NSECE %.3gbn hours = %s FTE workers | OEWS %s workers | NSECE / OEWS %.2f\n',
                eg, nsece_h / 1e9, format(round(nsece_h / OEWS_ANNUAL_HOURS), big.mark = ','),
                format(round(oews_w), big.mark = ','), nsece_h / OEWS_ANNUAL_HOURS / oews_w))
  }
  tot_ns <- sum(unlist(ns$labor_supply)) / OEWS_ANNUAL_HOURS
  cat(sprintf('  total  NSECE %s FTE | OEWS %s | ratio %.2f\n',
              format(round(tot_ns), big.mark = ','),
              format(round(sum(gn$natl_workers)), big.mark = ','),
              tot_ns / sum(gn$natl_workers)))
  cat(sprintf('  NSECE wages $%.2f no_ba, $%.2f ba | OEWS hourly mean $%.2f, $%.2f\n',
              ns$wages$no_ba, ns$wages$ba,
              gn$natl_wage[gn$educ_group == 'no_ba'], gn$natl_wage[gn$educ_group == 'ba']))

  cat('\nwage ratio range: no_ba ',
      paste(round(range(ratios$wage_ratio[ratios$educ_group == 'no_ba']), 3), collapse = ' to '),
      ', ba ', paste(round(range(ratios$wage_ratio[ratios$educ_group == 'ba']), 3), collapse = ' to '),
      '\n', sep = '')
  invisible(ratios)
}


if (sys.nframe() == 0) main_oews()
