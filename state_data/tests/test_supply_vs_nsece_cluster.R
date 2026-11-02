#------------------------------------------------------------------------------
# test_supply_vs_nsece_cluster.R -- cluster half of 2 November supply
# comparison (methodology memo open items: wages, staffing). Diagnostics only,
# nothing simulator or build reads.
#
# 1. NSECE 2019 supply by Census region, 1a recipe: centre staff wages by
#    education (roles 1-2, worker weight), staff per FTE child by tier,
#    centre + home prices, labour cost, residual. Check: national row
#    reproduces 1a (wages 12.06219 / 19.695, residuals 0.510055 low,
#    4.614567 high, 2.827343 home).
# 2. Observed vs legal centre staffing: classroom children per staff
#    (classroom weight, age of youngest child) by region x age vs legal ratio
#    from test_supply_vs_nsece_local.R. Log-log slopes: pooled, within age
#    (regional variation), within region (age variation).
#
# Usage (repo root, cluster; run local half first):
#   Rscript state_data/tests/test_supply_vs_nsece_cluster.R --nsece <NSECE 2019 dir> \
#     --est <interface>/estimation/data --out <dir>
#   reads state_data/out/legal_centre_children_per_staff_by_region_age_2019.csv
#   writes <out>/nsece_regional_supply_2019.csv, nsece_observed_vs_legal_2019.csv,
#   supply_vs_nsece_cluster_2019.txt
# Exit status 1 on failed check.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(data.table); library(dplyr); library(tidyr)})
options(width = 200)

args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(k) { i <- match(k, args); if (is.na(i)) stop('Missing ', k) else args[i + 1] }
N <- arg_val('--nsece'); EST <- arg_val('--est'); OUT <- arg_val('--out')
LEGAL <- 'state_data/out/legal_centre_children_per_staff_by_region_age_2019.csv'
# 1a national values (src/1_processing/1a_nsece_processing.R output)
NAT_1A <- c(wage_no_ba = 12.06219, wage_ba = 19.695, low = 0.510055, high = 4.614567, home = 2.827343)
TOL <- 1e-5

fails <- 0
check <- function(ok, msg) {
  cat(if (ok) '  [PASS] ' else '  [FAIL] ', msg, '\n', sep = '')
  if (!ok) fails <<- fails + 1
}
log_lines <- character()
say <- function(...) { x <- capture.output(...); log_lines <<- c(log_lines, x); cat(x, sep = '\n') }
na_neg <- function(x) ifelse(x < 0, NA, x)



cb <- fread(file.path(N, '37941-0006-Data.tsv')) %>% tibble()
wf <- fread(file.path(N, '37941-0005-Data.tsv')) %>% tibble()
hb <- fread(file.path(N, '37941-0007-Data.tsv')) %>% tibble()

# Centres, as 1a
ec <- grep('^CB9_ENRL_(INF|1YR|2YR|3YR|4YR|5YR)$', names(cb), value = TRUE)
fc <- grep('^CB9_ENRL_FT_(INF|1YR|2YR|3YR|4YR|5YR)$', names(cb), value = TRUE)
pc <- grep('^CB9_STDRATE_WKLY_(INF|2YR|3YR|4YR)$', names(cb), value = TRUE)
gc <- grep('^CB9_B1_3A_AGERATE_(INF|2YR|3YR|4YR)_R$', names(cb), value = TRUE)
cen <- cb %>%
  select(id = CB9_METH_CASEID, center_weight = CB9_METH_WEIGHT, region = CB9_REGION,
         density = CB9_COMM_URBAN_DENSITY, all_of(c(ec, fc, pc, gc))) %>%
  pivot_longer(all_of(c(ec, fc, pc, gc)), names_to = c('.value', 'age_bin'),
               names_pattern = '^CB9_(.+?)_(INF|1YR|2YR|3YR|4YR|5YR)(?:_R)?$') %>%
  mutate(price_status = case_when(is.na(STDRATE_WKLY) ~ 'NIU', ENRL == 0 ~ 'NIU', B1_3A_AGERATE == -8 ~ 'FREE',
                                  B1_3A_AGERATE == 2 ~ 'NORATE', STDRATE_WKLY %in% c(-1, -2) ~ 'MISS', TRUE ~ 'OK'),
         price = case_when(price_status == 'OK' ~ STDRATE_WKLY / 40, price_status == 'FREE' ~ 0, TRUE ~ NA_real_)) %>%
  group_by(id, region, density, age_bin = case_when(age_bin %in% c('INF', '1YR') ~ '0-1',
                                                    age_bin %in% c('2YR', '3YR') ~ '2-3',
                                                    age_bin %in% c('4YR', '5YR') ~ '4-5')) %>%
  summarise(center_weight = mean(center_weight), enrollment = sum(ENRL),
            share_ft = case_when(all(ENRL_FT < 0) ~ 0, any(ENRL_FT < 0) ~ mean(ENRL_FT[ENRL_FT >= 0]) / 100,
                                 TRUE ~ mean(ENRL_FT) / 100),
            price = if (all(is.na(price))) NA_real_ else mean(price, na.rm = TRUE), .groups = 'drop') %>%
  mutate(enrollment_fte = enrollment * share_ft + 0.5 * enrollment * (1 - share_ft))
thr <- fread(file.path(EST, 'nsece_tier_thresholds_2019.csv')) %>% tibble() %>%
  select(region, density, age_bin, median = threshold)
cen <- cen %>% left_join(thr, by = c('region', 'density', 'age_bin')) %>%
  mutate(ecec_choice = case_when(is.na(price) ~ NA_character_, price == 0 ~ 'Unpaid Center-Based',
                                 price < median ~ 'Low-Priced Center-Based', TRUE ~ 'High-Priced Center-Based'))
cls <- cb %>% transmute(id = CB9_METH_CASEID, classroom_weight = CB9_WEIGHT_CLSM, enroll = na_neg(CB9_F3_ENROLL_TC),
                        teach = na_neg(CB9_F3_TEACH_TC), assist = na_neg(CB9_F3_ASSIST_TC),
                        young = na_neg(CB9_F3_YOUNGMONTHS_R), region = CB9_REGION) %>%
  mutate(age_bin = case_when(floor(young / 12) %in% 0:1 ~ '0-1', floor(young / 12) %in% 2:3 ~ '2-3',
                             floor(young / 12) %in% 4:5 ~ '4-5'))
w <- wf %>% transmute(id = CB9_METH_CASEID, worker_weight = WF9_METH_WEIGHT,
                      wage = ifelse(WF9_WORK_WAGE > 0, WF9_WORK_WAGE, NA), role = ifelse(WF9_WORK_ROLE %in% 1:3, WF9_WORK_ROLE, NA),
                      college = case_when(WF9_CHAR_EDUC %in% 7:8 ~ 1, WF9_CHAR_EDUC %in% c(1, 3:6) ~ 0),
                      ft = case_when(WF9_WORK_FT == 1 ~ 1, WF9_WORK_FT == 2 ~ 0.5)) %>%
  left_join(cb %>% select(id = CB9_METH_CASEID, region = CB9_REGION), by = 'id')

# Homes, as 1a (home enrolment / workers unweighted)
kids <- fread(file.path(EST, 'nsece_children_2019.csv')) %>% tibble()
prov <- fread(file.path(EST, 'nsece_providers_2019.csv')) %>% tibble()
hh   <- fread(file.path(EST, 'nsece_households_2019.csv')) %>% tibble() %>% select(hh_id, region)
hprice <- kids %>% left_join(prov, by = c('hh_id', 'child_id')) %>% left_join(hh, by = 'hh_id') %>%
  filter(ecec_choice == 'Paid Home-Based', age < 5, net_hourly_cost_status == 'OK')
hbw <- hb %>% transmute(w = HB9_METH_WEIGHT, src = HB9_METH_SAMPLE_SOURCE, region = HB9_REGION, enrollment = HB9_ENRL_TOTAL,
                        resp_college = HB9_CAREER_EDUC, n_helpers = HB9_H2_NUMHELP, pct = HB9_STAFF_EDUC_PRCNT_4YR) %>%
  filter(enrollment >= 0, resp_college > 0, n_helpers > -4, pct != -1, pct != -3, src == 2) %>%
  mutate(n_helpers = ifelse(n_helpers == -2, 0, n_helpers), n_ba = as.integer(resp_college >= 6) + n_helpers * pct / 100,
         n_no_ba = 1 + n_helpers - n_ba)



supply <- function(cen_d, cls_d, w_d, hp_d, hb_d) {

  #----------------------------------------------------------------------------
  # 1a supply recipe on one subset (national or region).
  #
  # Params: cen_d, cls_d, w_d, hp_d, hb_d (tibble): centre x age, classroom,
  #   worker, home price, home provider records
  # Returns: (tibble) per care type: price, ba_ratio, no_ba_ratio, labor_cost,
  #   residual, children_per_fte_staff, wage_no_ba, wage_ba
  #----------------------------------------------------------------------------

  wages <- w_d %>% filter(!is.na(college), !is.na(wage), role %in% 1:2) %>% group_by(college) %>%
    summarise(wage = weighted.mean(wage, worker_weight), n = n(), .groups = 'drop')
  wr <- setNames(wages$wage, c('no_ba', 'ba')[wages$college + 1])
  wt <- w_d %>% left_join(cls_d %>% select(id, classroom_weight, age_bin), by = 'id', relationship = 'many-to-many') %>%
    left_join(cen_d %>% select(id, age_bin, ecec_choice), by = c('id', 'age_bin')) %>%
    filter(role %in% 1:2, !is.na(age_bin), !is.na(ecec_choice), !is.na(college), !is.na(ft)) %>%
    group_by(ecec_choice, age_bin) %>%
    summarise(ba = sum(college * ft * worker_weight), no_ba = sum((college == 0) * ft * worker_weight), .groups = 'drop')
  ct <- cen_d %>% filter(!is.na(ecec_choice)) %>% group_by(ecec_choice, age_bin) %>%
    summarise(price = weighted.mean(price, center_weight * enrollment_fte, na.rm = TRUE),
              enrollment_fte = sum(enrollment_fte * center_weight), .groups = 'drop')
  ci <- ct %>% left_join(wt, by = c('ecec_choice', 'age_bin')) %>% group_by(ecec_choice) %>%
    summarise(price = weighted.mean(price, enrollment_fte), across(c(enrollment_fte, ba, no_ba), sum), .groups = 'drop') %>%
    mutate(ba_ratio = ba / enrollment_fte, no_ba_ratio = no_ba / enrollment_fte)
  hp <- with(hp_d, weighted.mean(net_hourly_cost, child_weight))
  hi <- hb_d %>% summarise(ba_ratio = sum(n_ba) / sum(enrollment), no_ba_ratio = sum(n_no_ba) / sum(enrollment))
  bind_rows(ci %>% select(ecec_choice, price, ba_ratio, no_ba_ratio),
            tibble(ecec_choice = 'Paid Home-Based', price = hp, ba_ratio = hi$ba_ratio, no_ba_ratio = hi$no_ba_ratio)) %>%
    mutate(labor_cost = wr['ba'] * ba_ratio + wr['no_ba'] * no_ba_ratio, residual = ifelse(price > 0, price - labor_cost, NA),
           children_per_fte_staff = 1 / (ba_ratio + no_ba_ratio), wage_no_ba = wr['no_ba'], wage_ba = wr['ba'],
           n_workers_no_ba = wages$n[wages$college == 0], n_workers_ba = wages$n[wages$college == 1])
}



# 1. Regional supply
nat <- supply(cen, cls, w, hprice, hbw)
r_of <- function(t) nat$residual[nat$ecec_choice == t]
check(abs(nat$wage_no_ba[1] - NAT_1A[['wage_no_ba']]) < TOL && abs(nat$wage_ba[1] - NAT_1A[['wage_ba']]) < TOL,
      'national wages reproduce 1a')
check(abs(r_of('Low-Priced Center-Based') - NAT_1A[['low']]) < TOL &&
      abs(r_of('High-Priced Center-Based') - NAT_1A[['high']]) < TOL &&
      abs(r_of('Paid Home-Based') - NAT_1A[['home']]) < TOL, 'national residuals reproduce 1a')
reg <- bind_rows(lapply(1:4, function(r) supply(cen %>% filter(region == r), cls %>% filter(region == r),
                                                w %>% filter(region == r), hprice %>% filter(region == r),
                                                hbw %>% filter(region == r)) %>% mutate(region = r)))
sup <- bind_rows(nat %>% mutate(region = 0L), reg)
fwrite(sup, file.path(OUT, 'nsece_regional_supply_2019.csv'))
say(cat('== NSECE 2019 supply by region (0 national, 1 NE, 2 MW, 3 S, 4 W)\n'))
say(print(as.data.frame(sup %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE))

# 2. Observed vs legal centre staffing, classroom level
leg <- fread(LEGAL)
check(nrow(leg) == 20, 'legal ratio table has 4 regions x 5 ages')
cc <- cls %>% filter(enroll > 0, teach + assist > 0, young < 60) %>% mutate(age = paste0('age_', floor(young / 12)))
obs <- cc %>% group_by(region, age) %>%
  summarise(n = n(), obs_cps = sum(classroom_weight * enroll) / sum(classroom_weight * (teach + assist)), .groups = 'drop') %>%
  left_join(leg, by = c('region', 'age')) %>% mutate(obs_over_legal = obs_cps / legal_cps)
check(!anyNA(obs$legal_cps), 'every observed cell has a legal ratio')
fwrite(obs, file.path(OUT, 'nsece_observed_vs_legal_2019.csv'))
say(cat('\n== classrooms: observed children per staff vs legal, by region x age of youngest child\n'))
say(print(as.data.frame(obs %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE))
fits <- list(pooled = lm(log(obs_cps) ~ log(legal_cps), data = obs, weights = n),
             within_age = lm(log(obs_cps) ~ log(legal_cps) + factor(age), data = obs, weights = n),
             within_region = lm(log(obs_cps) ~ log(legal_cps) + factor(region), data = obs, weights = n))
for (k in names(fits)) {
  say(cat(sprintf('slope log observed on log legal, %-13s %.3f (se %.3f)\n', k,
                  coef(fits[[k]])[2], summary(fits[[k]])$coef[2, 2])))
}
cc2 <- cc %>% left_join(leg, by = c('region', 'age')) %>% mutate(cps = enroll / (teach + assist))
say(cat('share of classrooms (classroom weight) w/ more children per staff than regional legal ratio:',
        round(with(cc2, sum(classroom_weight * (cps > legal_cps)) / sum(classroom_weight)), 3), '\n'))

writeLines(c(paste('script: state_data/tests/test_supply_vs_nsece_cluster.R; run at', format(Sys.time())),
             paste('nsece:', N), paste('interface data:', EST), log_lines),
           file.path(OUT, 'supply_vs_nsece_cluster_2019.txt'))
cat('\n', fails, ' failure(s)\n', sep = '')
if (fails > 0) quit(status = 1)
