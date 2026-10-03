#------------------------------------------------------------------------------
# 06_compare_industry_counts.R -- demand build against 2019 industry counts.
#
# VALIDATION ONLY -- writes nothing simulator reads. Two administrative counts,
# every state, no capacity-to-enrollment conversion:
#   centers  QCEW private employment, NAICS 624410 child day care services
#            (BLS, annual average). Build side: child-hours in licensed-type
#            centers = Center-Based hours + Unpaid Center-Based hours x
#            (Head Start + Early Head Start) / program children -- public-school
#            pre-K staff sit in elementary schools (NAICS 6111), not 624410.
#   homes    Nonemployer establishments, NAICS 6244 (Census; self-employed,
#            mostly home providers, licensed or not). Build side: Paid
#            Home-Based children.
#
# AGREEMENT INDEX = geography's share of national count / its share of national
# build quantity. 1 = build places care in proportion to industry. National =
# 50 states + DC (NYC inside NY). Level not tested -- both industry counts
# hold school-age care, centers part-time staff; index cancels any factor
# common to every state.
#
# Usage: Rscript state_data/code/06_compare_industry_counts.R
#   prints table, writes out/industry_count_comparison.csv
#------------------------------------------------------------------------------

# build_geo(), load_spec(), raw_path(), read_chr(), num(); main() guarded
source('state_data/code/00_build_demand_targets.R')

QCEW_FILE <- 'US_QCEW__child_day_care_private_employment_by_state__2019.csv'
NES_FILE  <- 'US_NES__child_day_care_nonemployers_by_state__2019.csv'



industry_counts <- function() {

  #----------------------------------------------------------------------------
  # QCEW employment, nonemployer establishments per geography (states, DC,
  # NYC = sum of its counties).
  #
  # Returns: (tibble) geo, qcew_employment, nonemployers, nonemployer_receipts
  #----------------------------------------------------------------------------

  fips_geo <- setNames(names(STATE_POSTAL_TO_FIPS), as.integer(STATE_POSTAL_TO_FIPS))
  nyc <- SUBSTATE_GEOGRAPHIES$NYC

  q <- read_chr(raw_path(QCEW_FILE))
  q_st <- q[q$agglvl_code == '58', ]
  q_st$geo <- unname(fips_geo[as.character(num(substr(q_st$area_fips, 1, 2)))])
  q_nyc <- q[q$agglvl_code == '78' &
               num(substr(q$area_fips, 1, 2)) == nyc$statefip &
               num(substr(q$area_fips, 3, 5)) %in% nyc$countyfip, ]
  if (nrow(q_nyc) != length(nyc$countyfip)) stop('QCEW: NYC counties missing')
  qc <- dplyr::bind_rows(
    tibble::tibble(geo = q_st$geo, qcew_employment = num(q_st$annual_avg_emplvl)),
    tibble::tibble(geo = 'NYC', qcew_employment = sum(num(q_nyc$annual_avg_emplvl)))) %>%
    dplyr::filter(!is.na(geo))

  n <- read_chr(raw_path(NES_FILE))
  n_st  <- n[n$level == 'state', ]
  n_nyc <- n[n$level == 'county' & num(n$state) == nyc$statefip &
               num(n$county) %in% nyc$countyfip, ]
  if (nrow(n_nyc) != length(nyc$countyfip)) stop('NES: NYC counties missing')
  nc <- dplyr::bind_rows(
    tibble::tibble(geo = unname(fips_geo[as.character(num(n_st$state))]),
                   nonemployers = num(n_st$nonemployer_establishments),
                   nonemployer_receipts = 1000 * num(n_st$receipts_thousands)),
    tibble::tibble(geo = 'NYC', nonemployers = sum(num(n_nyc$nonemployer_establishments)),
                   nonemployer_receipts = 1000 * sum(num(n_nyc$receipts_thousands))))

  out <- dplyr::inner_join(qc, nc, by = 'geo')
  miss <- setdiff(GEOGRAPHIES, out$geo)
  if (length(miss)) stop('industry counts missing for ', paste(miss, collapse = ', '))
  out
}



build_quantities <- function(geo) {

  #----------------------------------------------------------------------------
  # Build's center child-hours (licensed-type centers) and Paid Home-Based,
  # one geography.
  #
  # Params:
  #   - geo (chr)
  #
  # Returns: (tibble) one row: geo, centre_hours, paid_home_children,
  #          paid_home_hours
  #----------------------------------------------------------------------------

  utils::capture.output(res <- build_geo(load_spec(geo)))
  d  <- attr(res, 'diagnostics')
  h  <- setNames(res$annual_hours, res$ecec_type)
  k  <- setNames(res$children, res$ecec_type)
  pc <- d$program_children
  hs <- unname(pc['Head Start']) * (1 - d$hs_in_schools_share) +
        unname(pc['Early Head Start'])
  tibble::tibble(geo = geo,
                 centre_hours = h[['Center-Based']] +
                   h[['Unpaid Center-Based']] * hs / d$program_net,
                 paid_home_children = k[['Paid Home-Based']],
                 paid_home_hours = h[['Paid Home-Based']])
}



main_industry_check <- function() {

  #----------------------------------------------------------------------------
  # Every geography: agreement indices, printed and written to
  # out/industry_count_comparison.csv.
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  ind <- industry_counts()
  b   <- dplyr::bind_rows(lapply(GEOGRAPHIES, build_quantities))
  d   <- dplyr::inner_join(b, ind, by = 'geo')

  us <- d[d$geo %in% names(STATE_POSTAL_TO_FIPS), ]
  tot <- function(x) sum(us[[x]])
  d <- d %>%
    dplyr::mutate(
      centre_index = (qcew_employment / tot('qcew_employment')) /
                     (centre_hours / tot('centre_hours')),
      home_index   = (nonemployers / tot('nonemployers')) /
                     (paid_home_children / tot('paid_home_children')),
      child_hours_per_employee   = centre_hours / qcew_employment,
      paid_home_children_per_nes = paid_home_children / nonemployers,
      receipts_per_home_hour     = nonemployer_receipts / paid_home_hours)

  cat('Build against 2019 industry counts (index 1 = proportional)\n\n')
  show_df(d %>%
            dplyr::filter(geo %in% MEMO_GEOGRAPHIES) %>%
            dplyr::transmute(geo,
                             centre_hours_m = round(centre_hours / 1e6, 1),
                             qcew_emp       = qcew_employment,
                             centre_index   = round(centre_index, 2),
                             paid_home_k    = round(paid_home_children / 1e3, 1),
                             nonemployers,
                             home_index     = round(home_index, 2)))

  s <- d[d$geo %in% names(STATE_POSTAL_TO_FIPS), ]
  q <- function(x) paste(sprintf('%.2f', stats::quantile(x, c(0, .1, .5, .9, 1))), collapse = ' / ')
  cat('\n50 states + DC: min / p10 / median / p90 / max\n')
  cat('  centre index ', q(s$centre_index), '\n')
  cat('  home index   ', q(s$home_index), '\n')
  cat(sprintf('  correlation, log build vs log industry: centres %.3f, homes %.3f\n',
              stats::cor(log(s$centre_hours), log(s$qcew_employment)),
              stats::cor(log(s$paid_home_children), log(s$nonemployers))))
  cat(sprintf('  national: %.0f centre child-hours per QCEW employee; %.2f Paid Home-Based children per nonemployer; $%.2f nonemployer receipts per Paid Home-Based child-hour\n',
              tot('centre_hours') / tot('qcew_employment'),
              tot('paid_home_children') / tot('nonemployers'),
              tot('nonemployer_receipts') / tot('paid_home_hours')))

  f <- file.path(STATE_DATA_ROOT, 'out', 'industry_count_comparison.csv')
  readr::write_csv(d, f)
  cat('wrote', f, '\n')
  invisible(d)
}


if (sys.nframe() == 0) main_industry_check()
