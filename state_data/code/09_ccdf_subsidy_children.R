#------------------------------------------------------------------------------
# 09_ccdf_subsidy_children.R -- CCDF-subsidised children 0-4 by state: setting,
# monthly hours, $0 family copay; zero-copay centre children set beside
# Unpaid Center-Based target, NSECE-implied unpaid-centre children.
#
# DIAGNOSTIC ONLY -- writes nothing simulator or 00_ reads.
#
# Source: CCDF ACF-801 administrative data FY2019 (Oct 2018 - Sep 2019), ICPSR
# 38677 public-use sample:
#   raw/US/US_ICPSR38677__ccdf_acf801_child_records__FY2019.rda    (da38677.0003)
#   raw/US/US_ICPSR38677__ccdf_acf801_family_records__FY2019.rda   (da38677.0002)
#   raw/US/US_ICPSR38677__ccdf_acf801_setting_records__FY2019.rda  (da38677.0004)
#   raw/US/US_ICPSR38677__ccdf_acf801_pooling_factor__FY2019.rda   (da38677.0005)
#   raw/US/US_ICPSR38677__citation__FY2019.html (weights documented there)
# Weight A_FCHWT ('annual adjusted weight', citation page): sampling weight x
# state pooling factor (POOLFCTR = CCDF share of pooled funds) -- children
# CCDF funds, not every child state's pooled programme serves. Sum over 12
# monthly records = average monthly children (1.374M national; U_FCHWT
# 1.863M). Check: A_FCHWT = U_FCHWT x POOLFCTR every state.
#
# Rules:
#   age      months from CH_DOB to report month < 60 (children 0-4)
#   setting  primary = setting record w/ most HOURS in child-month; CARETYPE
#            01/05/06 in-home, 02/07/08 family home, 03/09/10 group home,
#            04/11 centre (licensed + legally operating alike)
#   hours    sum of HOURS over child-month's setting records (hours/month)
#   $0 copay family record COPAY == 0 that month (copay family-level)
#   priced   PAYMENT > 0 on primary setting (provider receives subsidy)
# NYC: no sub-state field -- NY row statewide, NYC row omitted.
#
# Comparison columns (out/state_share_comparison.csv, 06_): unpaid_center_target
# = targets share x children_0_4 (programme-count step), nsece_unpaid_center =
# regional NSECE share x children_0_4.
#
# Usage: Rscript state_data/code/09_ccdf_subsidy_children.R
#   writes out/ccdf_subsidy_children.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages(library(dplyr))

source('state_data/code/shared_setup.R')
source('src/shared_functions/constants.R')   # STATE_POSTAL_TO_FIPS

RAW_US   <- file.path('resources', 'ccdf')
CCDF_FMT <- 'US_ICPSR38677__ccdf_acf801_%s__FY2019.rda'
SHARES   <- file.path(STATE_DATA_ROOT, 'out', 'state_share_comparison.csv')
OUT      <- file.path(STATE_DATA_ROOT, 'out', 'ccdf_subsidy_children.csv')

# CARETYPE two-digit code -> setting
SETTING_OF <- c('01' = 'in_home', '05' = 'in_home', '06' = 'in_home',
                '02' = 'family_home', '07' = 'family_home', '08' = 'family_home',
                '03' = 'group_home', '09' = 'group_home', '10' = 'group_home',
                '04' = 'center', '11' = 'center')



load_ccdf <- function(k) {

  #----------------------------------------------------------------------------
  # Loads one ICPSR 38677 .rda table.
  #
  # Params:
  #   - k (chr): table key, e.g. 'child_records'
  #
  # Returns: (data.frame) table, STATE trimmed to chr
  #----------------------------------------------------------------------------

  f <- file.path(RAW_US, sprintf(CCDF_FMT, k))
  if (!file.exists(f)) stop('MISSING INPUT: ', f)
  e <- new.env()
  d <- e[[load(f, e)]]
  d$STATE <- trimws(as.character(d$STATE))
  d
}



ccdf_child_months <- function() {

  #----------------------------------------------------------------------------
  # One row per child-month, children 0-4, 50 states + DC, w/ primary setting,
  # monthly hours, family copay, provider payment, weight.
  #
  # Returns: (tibble) STATE, FAMID, CHID, month, w, setting, hours, payment,
  #   copay
  #----------------------------------------------------------------------------

  ch <- load_ccdf('child_records')
  fa <- load_ccdf('family_records')
  se <- load_ccdf('setting_records')
  pf <- load_ccdf('pooling_factor')

  # Weight check: adjusted = unadjusted x pooling factor
  pfv <- setNames(as.numeric(pf$POOLFCTR), pf$STATE)
  gap <- max(abs(ch$A_FCHWT - ch$U_FCHWT * pfv[ch$STATE]))
  if (gap > 1e-3) stop('A_FCHWT != U_FCHWT x POOLFCTR (max gap ', gap, ')')

  ym <- function(x) (x %/% 100) * 12 + x %% 100
  kids <- tibble::tibble(STATE = ch$STATE, FAMID = ch$FAMID, CHID = ch$CHID,
                         month = ch$CREPORT, w = ch$A_FCHWT,
                         age_m = ym(ch$CREPORT) - ym(ch$CH_DOB)) %>%
    filter(STATE %in% names(STATE_POSTAL_TO_FIPS))

  sett <- tibble::tibble(STATE = se$STATE, FAMID = se$FAMID, CHID = se$CHID,
                         month = se$REPORT, hours = se$HOURS, payment = se$PAYMENT,
                         setting = unname(SETTING_OF[substr(as.character(se$CARETYPE), 2, 3)])) %>%
    group_by(STATE, FAMID, CHID, month) %>%
    arrange(desc(hours), .by_group = TRUE) %>%
    dplyr::summarise(setting   = first(setting),
                     payment   = first(payment),
                     tot_hours = sum(hours, na.rm = TRUE),
                     n_hours   = sum(!is.na(hours)), .groups = 'drop') %>%
    mutate(tot_hours = ifelse(n_hours == 0, NA_real_, tot_hours))

  fam <- tibble::tibble(STATE = fa$STATE, FAMID = fa$FAMID, month = fa$FREPORT,
                        copay = fa$COPAY)
  if (anyDuplicated(fam[, c('STATE', 'FAMID', 'month')])) stop('family key not unique')

  kids %>%
    left_join(sett, by = c('STATE', 'FAMID', 'CHID', 'month')) %>%
    left_join(fam, by = c('STATE', 'FAMID', 'month')) %>%
    transmute(STATE, FAMID, CHID, month, w, age_m, setting,
              hours = tot_hours, payment, copay)
}



summarise_ccdf <- function(cm, fam_w) {

  #----------------------------------------------------------------------------
  # Weighted children 0-4 by setting, mean hours, zero-copay shares per state.
  #
  # Params:
  #   - cm (tibble): ccdf_child_months(), already filtered to ages 0-4
  #   - fam_w (tibble): STATE, families, families_0copay (all ages, weighted)
  #
  # Returns: (tibble) one row per state + 'US'
  #----------------------------------------------------------------------------

  one <- function(d) {
    tw <- sum(d$w)
    tibble::tibble(
      records_0_4         = nrow(d),
      children_0_4        = tw,
      center              = sum(d$w[d$setting %in% 'center']),
      family_home         = sum(d$w[d$setting %in% 'family_home']),
      group_home          = sum(d$w[d$setting %in% 'group_home']),
      in_home             = sum(d$w[d$setting %in% 'in_home']),
      no_setting          = sum(d$w[is.na(d$setting)]),
      mean_hours_month    = sum((d$w * d$hours)[!is.na(d$hours)]) /
                            sum(d$w[!is.na(d$hours)]),
      center_hours_month  = with(d[d$setting %in% 'center' & !is.na(d$hours), ],
                                 sum(w * hours) / sum(w)),
      zero_copay_0_4      = sum(d$w[d$copay %in% 0]),
      zero_copay_center   = sum(d$w[d$copay %in% 0 & d$setting %in% 'center']),
      zero_copay_center_priced = sum(d$w[d$copay %in% 0 & d$setting %in% 'center' &
                                           (d$payment > 0) %in% TRUE]),
      zero_copay_center_records = sum(d$copay %in% 0 & d$setting %in% 'center'))
  }
  st <- cm %>% group_by(STATE) %>% group_modify(~ one(.x)) %>% ungroup()
  us <- one(cm) %>% mutate(STATE = 'US')
  bind_rows(st, us) %>%
    left_join(bind_rows(fam_w, fam_w %>% dplyr::summarise(STATE = 'US',
                                                          fam_all = sum(fam_all),
                                                          fam_0copay = sum(fam_0copay),
                                                          fam_04 = sum(fam_04),
                                                          fam_04_0copay = sum(fam_04_0copay))),
              by = 'STATE') %>%
    mutate(share_families_0copay     = fam_0copay / fam_all,
           share_families_04_0copay  = fam_04_0copay / fam_04) %>%
    select(-fam_all, -fam_0copay, -fam_04, -fam_04_0copay)
}



main <- function() {

  #----------------------------------------------------------------------------
  # Builds child-month file, summarises, joins demand comparison, writes OUT.
  #
  # Returns: (tibble) OUT rows, invisibly
  #----------------------------------------------------------------------------

  cm_all <- ccdf_child_months()
  cm <- cm_all %>% filter(!is.na(age_m), age_m >= 0, age_m < 60)

  # Families weighted w/ family-record weight; '0-4' = family w/ any child 0-4
  fa <- load_ccdf('family_records') %>% filter(STATE %in% names(STATE_POSTAL_TO_FIPS))
  f04 <- distinct(cm, STATE, FAMID, month) %>% mutate(has04 = TRUE)
  fam_w <- tibble::tibble(STATE = fa$STATE, FAMID = fa$FAMID, month = fa$FREPORT,
                          w = fa$A_FCHWT, copay = fa$COPAY) %>%
    left_join(f04, by = c('STATE', 'FAMID', 'month')) %>%
    group_by(STATE) %>%
    dplyr::summarise(fam_all       = sum(w),
                     fam_0copay    = sum(w[copay %in% 0]),
                     fam_04        = sum(w[has04 %in% TRUE]),
                     fam_04_0copay = sum(w[has04 %in% TRUE & copay %in% 0]),
                     .groups = 'drop')

  s <- summarise_ccdf(cm, fam_w)

  sh <- read.csv(SHARES, colClasses = 'character') %>%
    filter(type == 'Unpaid Center-Based', geo != 'NYC') %>%
    transmute(STATE = geo,
              unpaid_center_target = as.numeric(targets) * as.numeric(children_0_4),
              nsece_unpaid_center  = as.numeric(nsece) * as.numeric(children_0_4),
              pop_0_4              = as.numeric(children_0_4))
  sh <- bind_rows(sh, tibble::tibble(STATE = 'US',
                                     unpaid_center_target = sum(sh$unpaid_center_target),
                                     nsece_unpaid_center  = sum(sh$nsece_unpaid_center),
                                     pop_0_4              = sum(sh$pop_0_4)))

  out <- s %>%
    left_join(sh, by = 'STATE') %>%
    mutate(zc_center_pct_of_target = 100 * zero_copay_center / unpaid_center_target,
           zc_center_pct_of_nsece  = 100 * zero_copay_center / nsece_unpaid_center,
           zc_center_pct_of_pop    = 100 * zero_copay_center / pop_0_4) %>%
    rename(geo = STATE) %>%
    arrange(geo == 'US', geo)

  write.csv(out, OUT, row.names = FALSE)

  cat('== CCDF children 0-4, FY2019 average month (weighted, A_FCHWT)\n\n')
  show_df(out %>% transmute(geo, records_0_4, children_0_4 = round(children_0_4),
                            pct_center = round(100 * center / children_0_4, 1),
                            hrs = round(mean_hours_month),
                            fam0 = round(100 * share_families_0copay, 1),
                            zc_center = round(zero_copay_center),
                            target = round(unpaid_center_target),
                            pct_tgt = round(zc_center_pct_of_target, 1),
                            pct_nsece = round(zc_center_pct_of_nsece, 1)))
  cat('\nwrote ', OUT, '\n', sep = '')
  invisible(out)
}



if (sys.nframe() == 0) main()
