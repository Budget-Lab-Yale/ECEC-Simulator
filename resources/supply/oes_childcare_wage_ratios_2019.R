#------------------------------------------------------------------------------
# oes_childcare_wage_ratios_2019.R -- writes state_childcare_wage_ratios_oes.csv,
# state / national childcare wage ratio per education group, read by
# state_supply_params.R (w_0 = national NSECE wage x ratio).
#
# SOURCE. BLS OES May 2019 (state, national, MSA files). Employer survey --
# larger sample, SOC-coded occupations vs ACS self-report.
#
# GROUPS. OES has no education. Each group mixes occupation wages at
# NATIONAL ACS weights w(o, g) = share of group g's childcare workers in
# occupation o (WEIGHTS, acs_childcare_occupation_weights_2019.py):
#
#   ratio(g, s) = sum_o w(o, g) h(o, s) / sum_o w(o, g) h(o, US)
#
# h = OES hourly median. Occupations (SOC 2018):
#   39-9011 Childcare Workers
#   25-2011 Preschool Teachers, Except Special Education
#   11-9031 Education and Childcare Administrators, Preschool and Daycare
#   25-2051 Special Education Teachers, Preschool -- OES publishes no hourly
#   median (annual-only, school-year pay) anywhere; its weight folds into
#   25-2011 (FOLD).
# Weights fixed nationally -- state ratio moves w/ state wages only, not w/
# small state education samples. Occupation w/ suppressed state wage dropped
# from BOTH sums for that state (same occupation set top and bottom), rest
# of weights renormalize -- listed in column `occupations`. Group w/ no
# occupation left takes state's all-occupations (00-0000) median ratio
# (`fallback` TRUE).
#
# NYC. 2019 MSA file has no metropolitan divisions -- NYC takes New
# York-Newark-Jersey City MSA (35620), incl. NJ, PA suburbs.
#
# Usage, from repo root:
#   Rscript resources/supply/oes_childcare_wage_ratios_2019.R [<oes_dir>]
#   <oes_dir>: unzipped oesm19st, oesm19nat, oesm19ma (www.bls.gov/oes/
#   special.requests/) -- rewrites EXTRACT first. Without it, ratios from
#   EXTRACT as is.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr); library(dplyr)
})

# STATE_POSTAL_TO_FIPS
source('src/shared_functions/constants.R')

EXTRACT <- file.path('state_data', 'raw', 'US',
                     'US_OES__childcare_occupation_wages__2019.csv')
WEIGHTS <- file.path('resources', 'supply',
                     'childcare_occupation_weights_by_education_acs_2019.csv')
OUT     <- file.path('resources', 'supply', 'state_childcare_wage_ratios_oes.csv')

OCC     <- c('39-9011', '25-2011', '11-9031')
FOLD    <- c('25-2051' = '25-2011')   # no OES hourly median -> weight joins 25-2011
ALL_OCC <- '00-0000'
NYC_MSA <- '35620'



write_extract <- function(oes_dir) {

  #----------------------------------------------------------------------------
  # Needed rows from OES xlsx files -> EXTRACT: OCC + ALL_OCC, 50 states + DC,
  # US, NYC (MSA 35620). Raw strings kept ('*', '**' = suppressed).
  #
  # Params:
  #   - oes_dir (chr): dir holding state_M2019_dl.xlsx, national_M2019_dl.xlsx,
  #     MSA_M2019_dl.xlsx (any depth)
  #
  # Returns: nothing
  #----------------------------------------------------------------------------

  find1 <- function(name) {
    f <- list.files(oes_dir, pattern = name, recursive = TRUE, full.names = TRUE)
    if (length(f) != 1) stop('need one ', name, ' under ', oes_dir, ' -- unzip oesm19*.zip there')
    f
  }
  occ  <- c(OCC, ALL_OCC)
  keep <- c('area', 'area_title', 'occ_code', 'occ_title', 'tot_emp', 'h_median', 'a_median')
  rd <- function(f) {
    readxl::read_excel(f, col_types = 'text') %>%
      filter(occ_code %in% occ) %>%
      select(all_of(keep))
  }

  st <- rd(find1('^state_M2019_dl[.]xlsx$')) %>%
    mutate(geo = names(STATE_POSTAL_TO_FIPS)[match(as.integer(area), STATE_POSTAL_TO_FIPS)]) %>%
    filter(!is.na(geo))
  us  <- rd(find1('^national_M2019_dl[.]xlsx$')) %>% mutate(area = 'US', geo = 'US')
  nyc <- rd(find1('^MSA_M2019_dl[.]xlsx$')) %>% filter(area == NYC_MSA) %>% mutate(geo = 'NYC')

  d <- bind_rows(us, st, nyc) %>% relocate(geo)
  if (n_distinct(d$geo) != length(STATE_POSTAL_TO_FIPS) + 2) stop('extract missing geographies')
  write_csv(d, EXTRACT)
  cat('wrote ', EXTRACT, ' (', nrow(d), ' rows)\n', sep = '')
}



read_weights <- function() {

  #----------------------------------------------------------------------------
  # National ACS weights w(o, g), pooled 2015-19 sample, FOLD applied --
  # named num vec per group over OCC, sums to 1.
  #
  # Returns: (list) no_ba, ba (named num vec, names = SOC)
  #----------------------------------------------------------------------------

  if (!file.exists(WEIGHTS)) stop('no ', WEIGHTS, ' -- run acs_childcare_occupation_weights_2019.py')
  w <- read_csv(WEIGHTS, col_types = cols(soc = col_character(), .default = col_guess())) %>%
    mutate(soc = ifelse(soc %in% names(FOLD), FOLD[soc], soc))
  if (!all(w$soc %in% OCC)) stop(WEIGHTS, ': SOC outside OCC: ', setdiff(w$soc, OCC))
  out <- lapply(c(no_ba = 'no_ba', ba = 'ba'), function(g) {
    x <- w %>% filter(educ_group == g) %>% group_by(soc) %>%
      summarise(weight = sum(weight), .groups = 'drop')
    v <- setNames(x$weight, x$soc)[OCC]
    v[is.na(v)] <- 0
    v / sum(v)
  })
  out
}



group_ratio <- function(d, us, w) {

  #----------------------------------------------------------------------------
  # sum_o w h(o, s) / sum_o w h(o, US) over occupations w/ a numeric hourly
  # median in state (same set top and bottom).
  #
  # Params:
  #   - d (df): EXTRACT rows of one geography
  #   - us (named num vec): national hourly medians by SOC
  #   - w (named num vec): group weights by SOC
  #
  # Returns: (list) ratio (num, NA if no occupation left), occupations (chr)
  #----------------------------------------------------------------------------

  h <- setNames(suppressWarnings(as.numeric(d$h_median)), d$occ_code)[names(w)]
  ok <- is.finite(h) & w > 0
  list(ratio = if (any(ok)) sum(w[ok] * h[ok]) / sum(w[ok] * us[names(w)][ok]) else NA_real_,
       occupations = paste(sort(names(w)[ok]), collapse = ';'))
}



main <- function() {

  #----------------------------------------------------------------------------
  # Writes OUT: state, educ_group, occupations, fallback, wage_ratio. Prints
  # weights, fallbacks, dropped occupations.
  #
  # Returns: (tibble) OUT rows, invisibly
  #----------------------------------------------------------------------------

  args <- commandArgs(TRUE)
  if (length(args)) write_extract(args[1])
  if (!file.exists(EXTRACT)) stop('no ', EXTRACT, ' -- pass the unzipped OES dir')
  d <- read_csv(EXTRACT, col_types = cols(.default = col_character()))
  W <- read_weights()

  num <- function(x) suppressWarnings(as.numeric(x))
  all_med <- function(g) num(d$h_median[d$geo == g & d$occ_code == ALL_OCC])
  us <- setNames(num(d$h_median[d$geo == 'US']), d$occ_code[d$geo == 'US'])[OCC]
  if (anyNA(us)) stop('national hourly median missing for ', names(us)[is.na(us)])

  out <- list()
  for (g in setdiff(unique(d$geo), 'US')) {
    for (grp in names(W)) {
      gr <- group_ratio(d[d$geo == g, ], us, W[[grp]])
      fb <- is.na(gr$ratio)
      ratio <- if (fb) all_med(g) / all_med('US') else gr$ratio
      if (!is.finite(ratio)) stop('no wage ratio for ', g, '/', grp)
      out[[length(out) + 1]] <- tibble::tibble(
        state = g, educ_group = grp, occupations = gr$occupations,
        fallback = fb, wage_ratio = round(ratio, 4))
    }
  }
  r <- bind_rows(out) %>% arrange(state, desc(educ_group))
  write_csv(r, OUT)

  full <- vapply(W, function(w) paste(sort(names(w)[w > 0]), collapse = ';'), '')
  part <- r %>% filter(!fallback, occupations != full[educ_group])
  for (grp in names(W)) {
    cat('weights ', grp, ': ', paste(sprintf('%s %.3f', names(W[[grp]]), W[[grp]]), collapse = ', '),
        '\n', sep = '')
  }
  cat('fallback to all-occupations ratio: ',
      if (any(r$fallback)) paste(r$state[r$fallback], r$educ_group[r$fallback], collapse = ', ') else 'none',
      '\n', sep = '')
  cat('occupation dropped (suppressed): ',
      if (nrow(part)) paste0(part$state, '/', part$educ_group, ' (', part$occupations, ')', collapse = ', ') else 'none',
      '\n', sep = '')
  cat('wrote ', OUT, ' (', nrow(r), ' rows)\n', sep = '')
  invisible(r)
}



if (sys.nframe() == 0) main()
