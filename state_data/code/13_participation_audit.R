#------------------------------------------------------------------------------
# 13_participation_audit.R -- where NSECE non-parental participation goes
# between published OPRE statistic, model labels, forest, ACS prediction.
#
# DIAGNOSTIC ONLY -- prints tables, writes CSVs to --out (not repo). Three
# parts:
#   A  OPRE chartbook (May 2023, 2019 NSECE): 48.5% under 3, 68.1% ages 3-5
#      in regular care (any TOC 1-5 arrangement, >= 5 hrs usual). Reproduced
#      on chartbook population (under 60 months on 2018-09-01, under 72 months
#      at interview, age group by age at interview), then one restriction at
#      a time to model population + rules (children 0-4 current age, REGCARE
#      = 1, calendar hours > 0, model label).
#   B  fixed cohort -- children 0-4 (current age, weight > 0): any provider,
#      qualifying care, model label (EXCLUDED = label NA, leaves denominator;
#      RELABELED = qualifying care but Parent Only label), forest training
#      frame, forest prediction. Forest refit per 1b settings (forests not
#      saved).
#   C  NSECE vs ACS within cells (age, n_members band, income quintile, PC
#      employment, n_parents): NSECE observed, forest on NSECE, forest on
#      ACS; composition / within-cell split; feature distributions.
#   D  PC employment band, NSECE (new rule, old rule) vs ACS.
#   E  (w/ --est-old) 2026-10-12 rule changes: label transitions by cause,
#      Parent Only share, pt / ft split, imputed hours vs donors, 1a vs 02
#      child by child, parent sex resolution.
#
# 1b helpers (prepare_data_for_rf() etc.) evaluated from 1b source, not
# copied -- frames identical to 1b's by construction.
#
# Every share child-weighted (HHC9_METH_WEIGHT, ACS child weight); record
# counts unweighted. NSECE labels: label draw 0 only (draw0()) -- 1a, 02
# average NSECE_MI_DRAWS draws; audit stays child for child, forest refit
# on draw 0 (1b trains on every draw).
#
# DATA. NSECE public-use household file (licensed, cluster) + estimation
# output of a full run (data/nsece_*, data/acs_*). Submit w/ sbatch.
#
# Usage (repo root):
#   Rscript state_data/code/13_participation_audit.R --nsece <dir>
#     --est <run>/estimation --out <dir> [--est-old <run>/estimation]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(stringr)
  library(tibble); library(readr); library(purrr); library(ranger)
})

source('state_data/code/shared_setup.R')
source('src/shared_functions/constants.R')
source('src/shared_functions/income_rank.R')
source('src/shared_functions/assign_income_pctile.R')
# Regular-care rule (A step 3c), NSECE caregiver / employment (1b frame)
source('src/shared_functions/impute_unknown_payment.R')
source('src/shared_functions/compute_price_wedge_cache.R')
source('src/shared_functions/resolve_regular_care.R')
source('src/shared_functions/nsece_parent_attributes.R')

# 1b calls wtd.quantile() unqualified (Hmisc attached by main.R)
wtd.quantile <- Hmisc::wtd.quantile

PROV <- '(?:[1-9]|1[0-4])'
OPRE <- c(under3 = 0.485, age3to5 = 0.681)
PO   <- 'Parent Only_none'



load_1b_helpers <- function(wanted) {

  #----------------------------------------------------------------------------
  # Evaluates named local helpers of run_acs_processing() (1b) in global env
  # -- same code 1b runs, no copy.
  #
  # Params:
  #   - wanted (chr): helper names
  #
  # Returns: (NULL) invisibly; stops if any helper missing
  #----------------------------------------------------------------------------

  ex <- parse('src/1_processing/1b_acs_processing.R', keep.source = FALSE)
  top <- Filter(function(e) is.call(e) && identical(e[[1]], as.name('<-')) &&
                  identical(e[[2]], as.name('run_acs_processing')), as.list(ex))
  if (length(top) != 1) stop('run_acs_processing not found in 1b source')
  for (st in as.list(top[[1]][[3]][[3]])[-1]) {
    if (is.call(st) && identical(st[[1]], as.name('<-')) && is.name(st[[2]]) &&
        as.character(st[[2]]) %in% wanted) eval(st, globalenv())
  }
  miss <- wanted[!vapply(wanted, exists, logical(1), envir = globalenv())]
  if (length(miss)) stop('1b helpers missing: ', paste(miss, collapse = ', '))
  invisible(NULL)
}



read_raw <- function(root) {

  #----------------------------------------------------------------------------
  # NSECE household file, columns parts A, B need.
  #
  # Params:
  #   - root (chr): NSECE root holding 2019/
  #
  # Returns: (list) kids (one row per child), arr (one row per child x
  #   provider w/ HH9_CHILDREN == 1)
  #----------------------------------------------------------------------------

  f <- file.path(root, '2019', '37941-0009-Data.tsv')
  hdr <- names(fread(f, nrows = 0))
  cols <- function(p) unlist(lapply(p, grep, x = hdr, value = TRUE))
  xy <- paste0('^', c('HH9_CHILDREN', 'HH9_REGCARE', 'HH9_TYPEOFCARE_AGG',
                      'HH9_WEEKLY_PAYMENT_TC', 'HH9_CARELOC'), '_[1-9]_', PROV, '$')
  d <- fread(f, select = unique(c(
    'HH9_METH_CASEID', 'HH9_REGION', 'HH9_SUMMERQUEX',
    cols(c('^HHC9_AGE_[1-9]$', '^HHC9_AGE_AT_SEP1_18_[1-9]$', '^HHC9_METH_WEIGHT_[1-9]$',
           '^HHC9_NPC_USEREGPROVIDER_[1-9]$', '^HH9_MISSING_STATUS_CC_[1-9]$',
           paste0('^HH9_LASTWEEK_', PROV, '$'), paste0('^HH9_PRIORREL_', PROV, '$'),
           xy, '^HH9_HRS_WEEK_PROV[0-9_]+$', '^HH9_HOURS_WEEK_PROV[0-9_]+$')))))
  for (stem in c('HH9_HRS_WEEK_PROV', 'HH9_HOURS_WEEK_PROV')) {
    p   <- paste0('^', stem, '([0-9]+)_([0-9]+)$')
    old <- grep(p, names(d), value = TRUE)
    setnames(d, old, str_replace(old, p, 'HH9_HRS_WEEK_PROV_\\2_\\1'))
  }

  kid_cols <- grep('^HHC9_(AGE|AGE_AT_SEP1_18|METH_WEIGHT|NPC_USEREGPROVIDER)_[1-9]$|^HH9_MISSING_STATUS_CC_[1-9]$',
                   names(d), value = TRUE)
  kids <- as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID, summer = HH9_SUMMERQUEX, all_of(kid_cols)) %>%
    pivot_longer(c(-hh_id, -summer), names_to = 'v', values_to = 'value') %>%
    mutate(child_id = as.integer(str_sub(v, -1)),
           name = case_when(str_detect(v, 'SEP1')    ~ 'age_m_sep1',
                            str_detect(v, 'WEIGHT')  ~ 'w',
                            str_detect(v, 'USEREG')  ~ 'opre_reg',
                            str_detect(v, 'MISSING') ~ 'cal_status',
                            TRUE                     ~ 'age_m')) %>%
    select(-v) %>%
    pivot_wider() %>%
    filter(!is.na(age_m), age_m >= 0)

  prov_cols <- grep(paste0('^HH9_(LASTWEEK|PRIORREL)_', PROV, '$'), names(d), value = TRUE)
  prov <- as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID, all_of(prov_cols)) %>%
    pivot_longer(-hh_id, names_to = c('name', 'provider_id'),
                 names_pattern = '(HH9_[A-Z]+)_([0-9]+)$') %>%
    mutate(provider_id = as.integer(provider_id)) %>%
    pivot_wider()

  arr <- as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID,
           all_of(unlist(lapply(c(xy, paste0('^HH9_HRS_WEEK_PROV_[1-9]_', PROV, '$')),
                                grep, x = names(d), value = TRUE)))) %>%
    pivot_longer(-hh_id, names_to = c('name', 'child_id', 'provider_id'),
                 names_pattern = '(HH9_[A-Z0-9_]+)_([1-9])_([0-9]+)$') %>%
    mutate(child_id = as.integer(child_id), provider_id = as.integer(provider_id)) %>%
    pivot_wider() %>%
    filter(HH9_CHILDREN == 1) %>%
    left_join(prov, by = c('hh_id', 'provider_id')) %>%
    transmute(hh_id, child_id, provider_id, regcare = HH9_REGCARE,
              toc = HH9_TYPEOFCARE_AGG, hours = HH9_HRS_WEEK_PROV,
              lastweek = HH9_LASTWEEK, cost = HH9_WEEKLY_PAYMENT_TC)
  list(kids = kids, arr = arr)
}



wsh <- function(flag, w) sum(w[flag %in% TRUE]) / sum(w)



pct <- function(x) round(100 * x, 1)



# Label draw 0 only (= single draw before multiple imputation, 1a / 02
# nsece_mi_draws.R) -- audit child for child, one label per child; 1a
# providers table holds draw 0 too. Drops draw, draw_weight (full weight
# again); no draw column -> table as is
draw0 <- function(t) {
  if (!'draw' %in% names(t)) return(t)
  t %>% filter(draw == 0) %>% select(-any_of(c('draw', 'draw_weight')))
}



part_a <- function(raw, enr) {

  #----------------------------------------------------------------------------
  # Chartbook reproduction, then one restriction at a time to model.
  #
  # Params:
  #   - raw (list): read_raw()
  #   - enr (df): data/nsece_enrollment_2019.csv
  #
  # Returns: (df) step x age group: raw n, weighted children (M), share
  #----------------------------------------------------------------------------

  k <- raw$kids %>% filter(!is.na(w), w > 0)
  a <- raw$arr %>% left_join(select(k, hh_id, child_id, summer, cal_status),
                             by = c('hh_id', 'child_id'))
  any_by <- function(rows) {
    r <- distinct(rows, hh_id, child_id) %>% mutate(flag = TRUE)
    left_join(k, r, by = c('hh_id', 'child_id'))$flag %in% TRUE
  }
  k <- k %>% mutate(
    toc15     = any_by(filter(a, toc %in% 1:5)),
    reg1      = any_by(filter(a, regcare == 1, toc != 6)),
    reg1_toc15 = any_by(filter(a, regcare == 1, toc %in% 1:5)),
    reg1_h    = any_by(filter(a, regcare == 1, toc != 6, hours > 0)),
    # 2026-10-12 rule (resolve_regular_care()): TOC 1-5 w/ REGCARE 1, -1,
    # -2 (TOC 7 / 8 never); zero-hour regular rows take imputed hours ->
    # every regular row counts
    reg_new   = any_by(filter(a, regular_care_flag(regcare, toc) == 1, toc != 6)),
    any_prov  = any_by(a)) %>%
    left_join(enr %>% select(hh_id, child_id, ecec_choice), by = c('hh_id', 'child_id')) %>%
    mutate(label_np = !is.na(ecec_choice) & !ecec_choice %in% c('Parent Only', 'K-8'),
           labelled = !is.na(ecec_choice))

  # Born after 2018-09-01 -> age at Sep 1 reserve code / NA, still in
  # chartbook population (under 60 months then)
  cat('  HHC9_AGE_AT_SEP1_18 codes < 0 / NA, children under 72 months (raw):\n')
  print(table(sep1 = ifelse(is.na(k$age_m_sep1), 'NA', ifelse(k$age_m_sep1 < 0, k$age_m_sep1, '>=0'))[k$age_m < 72],
              useNA = 'ifany'))
  cb  <- k %>% filter(is.na(age_m_sep1) | age_m_sep1 < 60, age_m < 72) %>%
    mutate(grp = if_else(age_m < 36, 'under 3', '3-5'))
  m04 <- k %>% filter(age_m < 60) %>%
    mutate(grp = if_else(age_m < 36, 'under 3', '3-4'))

  step <- function(df, label, flag, keep = rep(TRUE, nrow(df))) {
    df <- df[keep, ]
    f <- flag[keep]
    bind_rows(df %>% mutate(grp = 'all'), df) %>%
      mutate(f = c(f, f)) %>%
      group_by(grp) %>%
      summarise(n_raw = n(), wt_m = sum(w) / 1e6,
                share = pct(sum(w * f) / sum(w)), .groups = 'drop') %>%
      mutate(step = label, .before = 1)
  }
  res <- bind_rows(
    step(cb, '0 chartbook pop; HHC9_NPC_USEREGPROVIDER = 1 (OPRE variable)', cb$opre_reg == 1),
    step(cb, '0b chartbook pop; any arrangement TOC 1-5', cb$toc15),
    step(m04, '1 children 0-4 current age; TOC 1-5', m04$toc15),
    step(m04, '2 + rule REGCARE = 1 (any TOC but K-8)', m04$reg1),
    step(m04, '2b REGCARE = 1 and TOC 1-5', m04$reg1_toc15),
    step(m04, '3 + calendar hours > 0 (old model counting rule)', m04$reg1_h),
    step(m04, '3c new rule: TOC 1-5 w/ REGCARE 1 / -1 / -2, zero hours imputed', m04$reg_new),
    step(m04, '4 model label non-parental, denominator all children', m04$label_np),
    step(m04, '5 model label non-parental, excluded (label NA) out', m04$label_np, m04$labelled)
  )

  # Why REGCARE = 1 arrangements carry 0 hours -- children 0-4, arrangement
  # rows, raw; children who lose qualifying care at step 3, weighted
  r1 <- a %>% semi_join(filter(k, age_m < 60), by = c('hh_id', 'child_id')) %>%
    filter(regcare == 1, toc != 6) %>%
    mutate(h0 = hours <= 0,
           why = case_when(!h0                    ~ 'hours > 0',
                           summer == 1            ~ 'h0: summer questionnaire',
                           lastweek == 0          ~ 'h0: regular, not last week (C3-C4)',
                           cal_status != 2        ~ 'h0: calendar missing / partial',
                           TRUE                   ~ 'h0: last-week provider, 0 calendar hrs'))
  why_rows <- count(r1, why, name = 'rows_raw')
  lost <- m04 %>% filter(reg1, !reg1_h)
  lost_why <- r1 %>% semi_join(lost, by = c('hh_id', 'child_id')) %>%
    group_by(hh_id, child_id) %>%
    summarise(why = paste(sort(unique(why)), collapse = ' + '), .groups = 'drop') %>%
    left_join(select(lost, hh_id, child_id, w), by = c('hh_id', 'child_id')) %>%
    group_by(why) %>%
    summarise(children_raw = n(), wt_m = sum(w) / 1e6, .groups = 'drop') %>%
    mutate(share_of_all_0_4 = pct(wt_m * 1e6 / sum(m04$w)))

  # REGCARE code mix on CHILDREN == 1 rows, children 0-4 (raw rows)
  reg_codes <- a %>% semi_join(filter(k, age_m < 60), by = c('hh_id', 'child_id')) %>%
    count(regcare, name = 'rows_raw')

  # 1a vs 02: children 0-4 whose every arrangement has REGCARE < 0 -- 1a
  # label NA (excluded), 02 Parent Only
  neg_only <- a %>% group_by(hh_id, child_id) %>%
    summarise(all_neg = all(regcare < 0), .groups = 'drop') %>%
    filter(all_neg) %>%
    inner_join(m04, by = c('hh_id', 'child_id'))
  neg <- tibble(children_raw = nrow(neg_only), wt_m = sum(neg_only$w) / 1e6,
                label_na_raw = sum(is.na(neg_only$ecec_choice)))

  cat('\n== A1. PUBLISHED STATISTIC -> MODEL, one restriction at a time (weighted %)\n')
  show_df(res)
  cat('   OPRE Table 1b: under 3 = 48.5, 3-5 = 68.1 (children not yet 5 on 2018-09-01)\n')
  cat('\n== A2. REGCARE = 1 arrangements of children 0-4 by hours reason (raw rows)\n')
  show_df(why_rows)
  cat('\n== A3. Children 0-4 who lose all qualifying care at step 3, by reason\n')
  show_df(lost_why)
  cat('\n== A4. REGCARE codes, child x provider rows w/ HH9_CHILDREN = 1, children 0-4 (raw)\n')
  show_df(reg_codes)
  cat('\n== A5. Children 0-4 w/ only REGCARE < 0 arrangements (1a: label NA; 02: Parent Only)\n')
  show_df(neg)
  list(steps = res, why_rows = why_rows, lost_why = lost_why, kids = k)
}



rank_tables <- function(est) {

  #----------------------------------------------------------------------------
  # Estimation tables + income-rank inputs, as 1b ecec block (national_rpp
  # default; region RPP from processed ACS children, not raw pull).
  #
  # Params:
  #   - est (chr): estimation dir
  #
  # Returns: (list) nsece (tables), acs (tables), nsece_hh / acs_hh (rank
  #   income + rank_group)
  #----------------------------------------------------------------------------

  rd <- function(n) as_tibble(fread(file.path(est, 'data', paste0(n, '_2019.csv')),
                                    integer64 = 'double'))
  ns <- setNames(lapply(c('children', 'parent_units', 'households', 'household_members',
                          'enrollment'), function(t) rd(paste0('nsece_', t))),
                 c('children', 'parent_units', 'households', 'household_members', 'enrollment'))
  ns$enrollment <- draw0(ns$enrollment)
  ac <- list(children = rd('acs_children'), households = rd('acs_households'),
             enrollment = rd('acs_enrollment'),
             # Written file has 3 rows per unit (PC hours set to none / pt /
             # ft counterfactual) -- observed row carries actual hours, as
             # 1b ecec block saw them (before expansion)
             parent_units = rd('acs_parent_units') %>%
               filter(employment_choice == employment_observed) %>%
               distinct(hh_id, parent_unit_id, .keep_all = TRUE))

  if (get_income_rank_scheme() == 'national_rpp') {
    srpp <- get_state_rpp()
    kk <- ac$children %>% filter(age < 5) %>%
      left_join(select(ac$households, hh_id, statefip, region), by = 'hh_id') %>%
      mutate(rpp = srpp[as.character(statefip)])
    rr <- sapply(split(kk, kk$region), function(x) sum(x$rpp * x$child_weight) / sum(x$child_weight))
    assign('.income_rank_region_rpp', rr, envir = globalenv())
    cat('  region RPP (ACS children 0-4, child-weighted):',
        paste(names(rr), round(rr, 2), sep = '=', collapse = ', '), '\n')
  }
  rank_hh <- function(h, source) {
    ri <- income_rank_inputs(h$income, h$region, if (source == 'acs') h$statefip else NULL, source)
    h %>% mutate(income = ri$income, rank_group = ri$group)
  }
  list(nsece = ns, acs = ac, nsece_hh = rank_hh(ns$households, 'nsece'),
       acs_hh = rank_hh(ac$households, 'acs'))
}



FEATURES <- list(
  one_child = list(
    `0` = c('age', 'male', 'income_pctile', 'n_members', 'region'),
    `1` = c('age', 'male', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.male1', 'employment_choice_pc'),
    `2` = c('age', 'male', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.age2', 'employment_choice_pc')),
  two_child = list(
    `0` = c('age_1', 'age_2', 'male_1', 'male_2', 'income_pctile', 'n_members', 'region', 'is_3plus'),
    `1` = c('age_1', 'age_2', 'male_1', 'male_2', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.male1', 'employment_choice_pc', 'is_3plus'),
    `2` = c('age_1', 'age_2', 'male_1', 'male_2', 'income_pctile', 'n_members', 'region', 'p.age1', 'p.age2', 'employment_choice_pc', 'is_3plus')))



fit_forests <- function(frame) {

  #----------------------------------------------------------------------------
  # Forests per 1b settings (feature_sets, weighted 50k resample, 500 trees,
  # min.node.size 10, mtry = all features, probability).
  #
  # Params:
  #   - frame (list): prepare_data_for_rf() NSECE output
  #
  # Returns: (list) ranger models, nested as frame
  #----------------------------------------------------------------------------

  imap(frame, function(fd, fam) imap(fd, function(df, pc) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    feats <- FEATURES[[fam]][[pc]]
    tr <- df %>% select(model_weight, ecec_class, all_of(feats)) %>%
      sample_n(size = 50000, weight = model_weight, replace = nrow(df) < 50000) %>%
      mutate(ecec_class = factor(ecec_class))
    ranger(ecec_class ~ ., data = tr[, c('ecec_class', feats)], num.trees = 500,
           min.node.size = 10, mtry = length(feats), probability = TRUE,
           verbose = FALSE, num.threads = 8)
  }))
}



child_rows <- function(frame, models, src) {

  #----------------------------------------------------------------------------
  # One row per child in a frame: features, forest P(Parent Only) (two-child
  # joint -> child marginal), observed class (NSECE).
  #
  # Params:
  #   - frame (list): prepare_data_for_rf() output
  #   - models (list): fit_forests()
  #   - src (chr): 'NSECE' / 'ACS'
  #
  # Returns: (tibble)
  #----------------------------------------------------------------------------

  emp_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  out <- list()
  for (pc in c('0', '1', '2')) {
    d1 <- frame$one_child[[pc]]
    if (!is.null(d1) && nrow(d1) > 0) {
      pr <- predict(models$one_child[[pc]], data = d1, num.threads = 8)$predictions
      p_po <- if (PO %in% colnames(pr)) pr[, PO] else 0
      out[[length(out) + 1]] <- tibble(
        src = src, fam = 'one', n_par = as.integer(pc), hh_id = d1$hh_id,
        parent_unit_id = as.character(d1$parent_unit_id), child_id = d1$child_id,
        w = d1$child_weight, age = d1$age, male = d1$male,
        income_pctile = d1$income_pctile, n_members = d1$n_members,
        region = as.character(d1$region),
        p.age1 = if ('p.age1' %in% names(d1)) d1$p.age1 else NA_real_,
        p.age2 = if ('p.age2' %in% names(d1)) d1$p.age2 else NA_real_,
        p.male1 = if ('p.male1' %in% names(d1)) d1$p.male1 else NA_real_,
        emp = if (pc == '0') 'no parent' else emp_chr(d1$employment_choice_pc),
        obs_class = if ('ecec_class' %in% names(d1)) d1$ecec_class else NA_character_,
        pred_po = p_po)
    }
    d2 <- frame$two_child[[pc]]
    if (!is.null(d2) && nrow(d2) > 0) {
      pr <- predict(models$two_child[[pc]], data = d2, num.threads = 8)$predictions
      first  <- sub('__.*$', '', colnames(pr))
      second <- sub('^.*__', '', colnames(pr))
      for (j in 1:2) {
        cols <- if (j == 1) first == PO else second == PO
        out[[length(out) + 1]] <- tibble(
          src = src, fam = 'two', n_par = as.integer(pc), hh_id = d2$hh_id,
          parent_unit_id = as.character(d2$parent_unit_id),
          child_id = d2[[paste0('child_id_', j)]], w = d2[[paste0('child_weight_', j)]],
          age = d2[[paste0('age_', j)]], male = d2[[paste0('male_', j)]],
          income_pctile = d2$income_pctile, n_members = d2$n_members,
          region = as.character(d2$region),
          p.age1 = d2$p.age1, p.age2 = d2$p.age2, p.male1 = d2$p.male1,
          emp = if (pc == '0') 'no parent' else emp_chr(d2$employment_choice_pc),
          obs_class = if (paste0('ecec_class_', j) %in% names(d2)) d2[[paste0('ecec_class_', j)]] else NA_character_,
          pred_po = rowSums(pr[, cols, drop = FALSE]))
      }
    }
  }
  bind_rows(out) %>%
    mutate(obs_po = if_else(is.na(obs_class), NA_real_, as.numeric(obs_class == PO)),
           nmem_band = cut(n_members, c(0, 3, 4, 5, Inf), labels = c('<=3', '4', '5', '6+')),
           inc_q = pmin(5L, (as.integer(income_pctile) - 1L) %/% 20L + 1L),
           age_band = if_else(age <= 2, '0-2', '3-4'))
}



part_b <- function(ak, rt, models, tr_rows, pf_rows) {

  #----------------------------------------------------------------------------
  # Fixed cohort: NSECE children 0-4 (current age, weight > 0) through each
  # pipeline stage.
  #
  # Params:
  #   - ak (df): part_a()$kids (raw-file children + flags + label)
  #   - rt (list): rank_tables()
  #   - models (list): fit_forests() on training frame
  #   - tr_rows, pf_rows (df): child_rows() of training frame, of
  #     label-filled prediction frame
  #
  # Returns: (list) stages, frame exclusion reasons
  #----------------------------------------------------------------------------

  co <- ak %>% filter(age_m < 60) %>%
    left_join(rt$nsece$children %>% select(hh_id, child_id, parent_unit_id, male),
              by = c('hh_id', 'child_id')) %>%
    left_join(tr_rows %>% transmute(hh_id, child_id, in_train = TRUE, pred_po_tr = pred_po),
              by = c('hh_id', 'child_id')) %>%
    left_join(pf_rows %>% transmute(hh_id, child_id, in_pf = TRUE, pred_po_pf = pred_po),
              by = c('hh_id', 'child_id')) %>%
    mutate(in_train = in_train %in% TRUE, in_pf = in_pf %in% TRUE,
           label = case_when(is.na(ecec_choice)          ~ 'EXCLUDED (label NA)',
                             ecec_choice == 'Parent Only' ~ 'Parent Only',
                             TRUE                         ~ 'non-parental'))
  W <- sum(co$w)
  st <- function(label, f, wexp = NULL) {
    tibble(stage = label, children_raw = sum(f), wt_m = sum(co$w[f]) / 1e6,
           pct_of_cohort = pct(sum(co$w[f]) / W),
           expected_np_pct_of_cohort = if (is.null(wexp)) NA_real_ else pct(wexp / W))
  }
  tr <- co$in_train
  stages <- bind_rows(
    st('0 cohort: children 0-4, weight > 0', rep(TRUE, nrow(co))),
    st('1 any provider row (HH9_CHILDREN = 1)', co$any_prov),
    st('2 qualifying care: any TOC 1-5 arrangement (OPRE regular)', co$toc15),
    st('3 old counting arrangement (REGCARE 1, hours > 0)', co$reg1_h),
    st('3c new counting arrangement (resolve_regular_care())', co$reg_new),
    st('4a label: non-parental', co$label == 'non-parental'),
    st('4b label: Parent Only', co$label == 'Parent Only'),
    st('4c EXCLUDED: label NA', co$label == 'EXCLUDED (label NA)'),
    st('4d RELABELED: TOC 1-5 care, label Parent Only', co$toc15 & co$label == 'Parent Only'),
    st('5 in forest training frame', tr),
    st('5a   of which label non-parental', tr & co$label == 'non-parental'),
    st('6 forest prediction, training children (expected non-parental)', tr,
       sum(co$w[tr] * (1 - co$pred_po_tr[tr]))),
    st('6b forest prediction, label-filled frame (expected non-parental)', co$in_pf,
       sum(co$w[co$in_pf] * (1 - co$pred_po_pf[co$in_pf]))))

  # Shares within each stage's own denominator
  dens <- tibble(
    measure = c('non-parental | labelled (4a / (4a+4b))',
                'non-parental | training frame (observed)',
                'non-parental | training frame (forest, in-sample)',
                'non-parental | label-filled frame (forest)',
                'TOC 1-5 care | cohort', 'TOC 1-5 care | training frame'),
    pct = c(pct(sum(co$w[co$label == 'non-parental']) /
                  sum(co$w[co$label != 'EXCLUDED (label NA)'])),
            pct(sum(co$w[tr & co$label == 'non-parental']) / sum(co$w[tr])),
            pct(sum(co$w[tr] * (1 - co$pred_po_tr[tr])) / sum(co$w[tr])),
            pct(sum(co$w[co$in_pf] * (1 - co$pred_po_pf[co$in_pf])) / sum(co$w[co$in_pf])),
            pct(wsh(co$toc15, co$w)), pct(sum(co$w[tr & co$toc15]) / sum(co$w[tr]))))

  # Frame exclusion reasons (children 0-4 not in training frame)
  pu <- rt$nsece$parent_units %>%
    mutate(pc = nsece_primary_caregiver(hhm_id2, male1, male2),
           emp = case_when(pc == 1L ~ emp1, pc == 2L ~ emp2)) %>%
    select(hh_id, parent_unit_id, hhm_id1, hhm_id2, male1, male2, pc, emp)
  why <- co %>% filter(!in_train) %>%
    left_join(pu, by = c('hh_id', 'parent_unit_id')) %>%
    mutate(reason = case_when(
      label == 'EXCLUDED (label NA)'             ~ 'own label NA',
      is.na(male)                                ~ 'child sex NA',
      in_pf                                      ~ 'two-child family, sibling label NA',
      !is.na(parent_unit_id) & is.na(male1)      ~ 'parent 1 sex unresolved (male1 NA)',
      !is.na(parent_unit_id) & is.na(pc)         ~ 'caregiver unresolved (mother unknown)',
      !is.na(parent_unit_id) & is.na(emp)        ~ 'caregiver employment NA',
      TRUE                                       ~ 'other (censored 3rd+ child / parent age NA / other)')) %>%
    group_by(reason, label) %>%
    summarise(children_raw = n(), wt_m = sum(w) / 1e6, .groups = 'drop') %>%
    mutate(pct_of_cohort = pct(wt_m * 1e6 / W))

  cat('\n== B1. FIXED COHORT, NSECE children 0-4 (raw n, weighted M, % of cohort weight)\n')
  show_df(stages)
  cat('\n== B2. Shares on each stage\'s own denominator (weighted %)\n')
  show_df(dens)
  cat('\n== B3. Cohort children NOT in training frame, by reason x label\n')
  show_df(why)
  list(stages = stages, dens = dens, why = why, cohort = co)
}



decompose <- function(n, a, by, value = 'pred_po') {

  #----------------------------------------------------------------------------
  # ACS - NSECE gap in mean `value` over cells `by`: composition (ACS cell
  # weights - NSECE cell weights) x NSECE cell mean; within (ACS cell weights
  # x cell mean gap). Cells w/out NSECE rows reported, left out of split.
  #
  # Params:
  #   - n, a (df): child_rows() NSECE, ACS
  #   - by (chr): cell columns
  #   - value (chr): NSECE column for NSECE cell mean (pred_po / obs_po)
  #
  # Returns: (tibble) one row
  #----------------------------------------------------------------------------

  cn <- n %>% group_by(across(all_of(by))) %>%
    summarise(wn = sum(w), pn = sum(w * .data[[value]]) / sum(w), .groups = 'drop')
  ca <- a %>% group_by(across(all_of(by))) %>%
    summarise(wa = sum(w), pa = sum(w * pred_po) / sum(w), .groups = 'drop')
  m <- full_join(cn, ca, by = by)
  un <- sum(m$wa[is.na(m$wn)], na.rm = TRUE) / sum(m$wa, na.rm = TRUE)
  m <- m %>% filter(!is.na(wn), !is.na(wa)) %>%
    mutate(sn = wn / sum(wn), sa = wa / sum(wa))
  tibble(cells = paste(by, collapse = ' x '), n_cells = nrow(m),
         acs_wt_in_cells_w_no_nsece = pct(un),
         nsece = pct(sum(m$sn * m$pn)), acs = pct(sum(m$sa * m$pa)),
         gap = pct(sum(m$sa * m$pa) - sum(m$sn * m$pn)),
         composition = pct(sum((m$sa - m$sn) * m$pn)),
         within = pct(sum(m$sa * (m$pa - m$pn))))
}



part_c <- function(nr, ar, saved_po) {

  #----------------------------------------------------------------------------
  # NSECE vs ACS Parent Only within cells, composition / within split,
  # feature distributions.
  #
  # Params:
  #   - nr, ar (df): child_rows() NSECE training frame, ACS frame
  #   - saved_po (df): hh_id, parent_unit_id, child_id, saved_po (ACS p0 at
  #     actual PC employment, run's acs_enrollment_2019.csv)
  #
  # Returns: (list) tables
  #----------------------------------------------------------------------------

  ar <- ar %>% left_join(saved_po, by = c('hh_id', 'parent_unit_id', 'child_id'))
  nr <- nr %>% mutate(saved_po = NA_real_)
  lvl <- function(df, g) df %>% group_by(across(all_of(g))) %>%
    summarise(obs = sum(w * obs_po) / sum(w), pred = sum(w * pred_po) / sum(w),
              saved = sum(w * saved_po) / sum(w), n_raw = n(), wt = sum(w),
              .groups = 'drop')
  head_tab <- bind_rows(
    lvl(mutate(nr, g = 'all'), 'g') %>% mutate(src = 'NSECE'),
    lvl(mutate(ar, g = 'all'), 'g') %>% mutate(src = 'ACS'),
    lvl(mutate(nr, g = paste0(n_par, ' parents')), 'g') %>% mutate(src = 'NSECE'),
    lvl(mutate(ar, g = paste0(n_par, ' parents')), 'g') %>% mutate(src = 'ACS')) %>%
    transmute(src, group = g, n_raw, wt_m = wt / 1e6, observed_po = pct(obs),
              forest_po_refit = pct(pred), forest_po_saved_run = pct(saved))

  by_dim <- map_dfr(c('age', 'nmem_band', 'inc_q', 'emp', 'n_par'), function(v) {
    f <- function(df, s) df %>% mutate(level = as.character(.data[[v]])) %>%
      group_by(n_par_grp = if (v == 'n_par') 'all' else as.character(n_par), level) %>%
      summarise(obs = sum(w * obs_po) / sum(w),
                pred = sum(w * pred_po) / sum(w), wt = sum(w), .groups = 'drop') %>%
      group_by(n_par_grp) %>% mutate(share = wt / sum(wt)) %>% ungroup() %>%
      mutate(src = s)
    full_join(f(nr, 'n') %>% select(n_par_grp, level, sh_n = share, obs_n = obs, pred_n = pred),
              f(ar, 'a') %>% select(n_par_grp, level, sh_a = share, pred_a = pred),
              by = c('n_par_grp', 'level')) %>%
      mutate(dim = v, .before = 1) %>%
      mutate(across(c(sh_n, obs_n, pred_n, sh_a, pred_a), pct))
  })

  cells <- list(c('n_par'), c('emp'), c('n_par', 'emp'), c('age_band', 'n_par', 'emp'),
                c('age_band', 'n_par', 'emp', 'inc_q'),
                c('age_band', 'n_par', 'emp', 'inc_q', 'nmem_band'),
                c('age', 'n_par', 'emp', 'inc_q', 'nmem_band'))
  dec <- bind_rows(map_dfr(cells, ~ decompose(nr, ar, .x, 'pred_po') %>% mutate(nsece_value = 'forest')),
                   map_dfr(cells, ~ decompose(nr, ar, .x, 'obs_po') %>% mutate(nsece_value = 'observed')))
  dec_by_par <- map_dfr(c(1, 2), function(k) {
    map_dfr(list(c('emp'), c('age_band', 'emp', 'inc_q', 'nmem_band')), ~
              decompose(filter(nr, n_par == k), filter(ar, n_par == k), .x) %>%
              mutate(n_par = k, .before = 1))
  })

  # Feature distributions, weighted, by n_par
  wm <- function(x, wt) { k <- !is.na(x); sum(x[k] * wt[k]) / sum(wt[k]) }
  feat <- bind_rows(nr, ar) %>%
    group_by(src, n_par) %>%
    summarise(wt_m = sum(w) / 1e6,
              mean_p.age1 = wm(p.age1, w), mean_p.age2 = wm(p.age2, w),
              pct_p.male1 = pct(wm(p.male1, w)),
              pct_emp_none = pct(wm(emp == 'none', w)),
              pct_emp_pt = pct(wm(emp == 'pt', w)),
              pct_emp_ft = pct(wm(emp == 'ft', w)),
              mean_n_members = wm(n_members, w),
              mean_income_pctile = wm(income_pctile, w),
              mean_age = wm(age, w), .groups = 'drop')

  cat('\n== C1. Parent Only share, forest frames (weighted %): observed, forest refit, forest saved run\n')
  show_df(head_tab)
  cat('\n== C2. One dimension at a time: weight share (sh_), NSECE observed (obs_n),\n',
      '       forest on NSECE (pred_n), forest on ACS (pred_a), within n_par group\n')
  show_df(by_dim)
  cat('\n== C3. ACS - NSECE gap split: composition vs within-cell (points)\n')
  show_df(dec)
  cat('\n== C4. Same split within one- and two-parent families (forest on both)\n')
  show_df(dec_by_par)
  cat('\n== C5. Feature distributions by source x n_par (weighted)\n')
  show_df(feat)
  list(head = head_tab, by_dim = by_dim, dec = dec, dec_by_par = dec_by_par, feat = feat)
}



part_d <- function(ak, rt) {

  #----------------------------------------------------------------------------
  # PC employment input, NSECE vs ACS. NSECE new: caregiver as ACS, band
  # per nsece_person_employment() (worked last week -> reference-week WST
  # hours; else last job within 12 months -> usual hours there; NA -> out).
  # NSECE old (reference): parent 1 hours = last-week work + school +
  # training + commuting (HH9_WST_HRS_TOT_PHI/PLO), NA -> 'none'. ACS =
  # mother's (else parent 1's) UHRSWORK, usual hours worked past 12 months.
  #
  # Params:
  #   - ak (df): part_a()$kids
  #   - rt (list): rank_tables()
  #
  # Returns: (tibble) source x n_par x hours band, raw n, weighted %
  #----------------------------------------------------------------------------

  band <- function(h) case_when(is.na(h) ~ 'NA -> none', h == 0 ~ '0 -> none',
                                h < 35 ~ '1-34 -> pt', TRUE ~ '35+ -> ft')
  # New feature (2026-10-12): caregiver chosen as ACS, band from 1a emp1 /
  # emp2 (nsece_person_employment()); NA -> out of frame
  nband <- function(e) case_when(is.na(e) ~ 'NA -> out', e == 'none' ~ '0 -> none',
                                 e == 'pt' ~ '1-34 -> pt', TRUE ~ '35+ -> ft')
  ns0 <- ak %>% filter(age_m < 60) %>%
    inner_join(rt$nsece$children %>% select(hh_id, child_id, parent_unit_id),
               by = c('hh_id', 'child_id')) %>%
    inner_join(rt$nsece$parent_units %>%
                 select(hh_id, parent_unit_id, n_parents, hours1, hhm_id2, male1, male2, emp1, emp2),
               by = c('hh_id', 'parent_unit_id')) %>%
    mutate(pc = nsece_primary_caregiver(hhm_id2, male1, male2),
           emp = case_when(pc == 1L ~ emp1, pc == 2L ~ emp2))
  ns <- ns0 %>%
    transmute(src = if_else(summer == 1, 'NSECE summer questionnaire', 'NSECE calendar'),
              n_par = n_parents, b = nband(emp), w)
  ns <- bind_rows(ns, mutate(ns, src = 'NSECE all'),
                  ns0 %>% transmute(src = 'NSECE old (parent 1 WST hours)', n_par = n_parents,
                                    b = band(hours1), w))
  ac <- rt$acs$children %>% filter(age < 5, !is.na(parent_unit_id)) %>%
    inner_join(rt$acs$parent_units %>%
                 mutate(pc = get_primary_caregiver(hhm_id2, male1, male2),
                        h = if_else(pc == 1L, hours1, hours2)) %>%
                 select(hh_id, parent_unit_id, n_parents, h),
               by = c('hh_id', 'parent_unit_id')) %>%
    transmute(src = 'ACS', n_par = n_parents, b = band(h), w = child_weight)
  res <- bind_rows(ns, ac) %>%
    group_by(src, n_par, b) %>%
    summarise(children_raw = n(), wt = sum(w), .groups = 'drop') %>%
    group_by(src, n_par) %>%
    mutate(pct = pct(wt / sum(wt))) %>%
    ungroup() %>%
    select(-wt)
  cat('\n== D1. PC hours band, children 0-4 w/ a parent unit (raw n, weighted % within source x n_par)\n')
  show_df(res)
  res
}



part_e <- function(raw, ak, est, est_old, nsece_root) {

  #----------------------------------------------------------------------------
  # 2026-10-12 rule changes, new run (est) vs old run (est_old): label
  # transitions by cause, Parent Only share, pt / ft split, imputed hours vs
  # donors, 1a vs 02 child by child, parent sex resolution.
  #
  # Params:
  #   - raw (list): read_raw()
  #   - ak (df): part_a()$kids
  #   - est, est_old (chr): estimation dirs, new / old code
  #   - nsece_root (chr): NSECE root (02 classification)
  #
  # Returns: (list) tables
  #----------------------------------------------------------------------------

  rd <- function(e, n) as_tibble(fread(file.path(e, 'data', paste0(n, '_2019.csv')),
                                       integer64 = 'double'))
  grp <- function(x) case_when(is.na(x) ~ 'NA', x == 'Parent Only' ~ 'Parent Only',
                               x == 'K-8' ~ 'K-8', TRUE ~ 'non-parental')
  # Elementwise equality, NA == NA counts same
  same <- function(x, y) coalesce(as.character(x) == as.character(y), is.na(x) & is.na(y))
  co <- ak %>% filter(age_m < 60) %>% select(hh_id, child_id, w)
  W <- sum(co$w)
  enr_new <- draw0(rd(est, 'nsece_enrollment'))
  enr_old <- draw0(rd(est_old, 'nsece_enrollment'))
  prov <- rd(est, 'nsece_providers')
  cnt <- function(p) !is.na(p$regular) & p$regular == 1 & !is.na(p$hours) & p$hours > 0
  flags <- prov %>%
    mutate(counts = cnt(prov), lab = !is.na(ecec_choice) | !is.na(unknown_kind)) %>%
    group_by(hh_id, child_id) %>%
    summarise(
      old_rule = any(regcare %in% 1 & !is.na(hours_reported) & hours_reported > 0 & lab),
      imp_row  = any(counts & lab & regcare %in% 1 & hours_imputed %in% TRUE),
      type_row = any(counts & lab & regular_by_type %in% TRUE),
      .groups = 'drop')
  tr <- co %>%
    left_join(enr_old %>% transmute(hh_id, child_id, old = ecec_choice, old_h = ecec_hours_choice),
              by = c('hh_id', 'child_id')) %>%
    left_join(enr_new %>% transmute(hh_id, child_id, new = ecec_choice, new_h = ecec_hours_choice),
              by = c('hh_id', 'child_id')) %>%
    left_join(flags, by = c('hh_id', 'child_id')) %>%
    mutate(across(c(old_rule, imp_row, type_row), ~ .x %in% TRUE),
           og = grp(old), ng = grp(new),
           cause = case_when(
             # Row by row -- %in% tested membership in whole column
             og == ng & same(old, new) & same(old_h, new_h) ~ 'unchanged',
             !old_rule & imp_row                            ~ 'fix 1: zero-hour regular provider',
             !old_rule & type_row                           ~ 'fix 2: undetermined regularity (TOC 1-5)',
             og == 'NA' & ng == 'Parent Only'               ~ 'fix 2: undetermined, not TOC 1-5 -> Parent Only',
             TRUE                                           ~ 'other (draw / tier / hours sum)'))
  trans <- tr %>%
    group_by(cause, old = og, new = ng) %>%
    summarise(children_raw = n(), pct_of_cohort = pct(sum(w) / W), .groups = 'drop')

  # Parent Only share, pt / ft among non-parental, old vs new
  shares <- map_dfr(c('old', 'new'), function(v) {
    lab <- tr[[v]]; hb <- tr[[paste0(v, '_h')]]
    l <- !is.na(lab)
    np <- l & !lab %in% c('Parent Only', 'K-8')
    tibble(run = v,
           labelled_raw = sum(l), label_na_raw = sum(!l),
           parent_only_pct_of_labelled = pct(sum(tr$w[l & lab == 'Parent Only']) / sum(tr$w[l])),
           non_parental_pct_of_labelled = pct(sum(tr$w[np]) / sum(tr$w[l])),
           pt_pct_of_non_parental = pct(sum(tr$w[np & hb == 'pt']) / sum(tr$w[np])),
           ft_pct_of_non_parental = pct(sum(tr$w[np & hb == 'ft']) / sum(tr$w[np])))
  })

  # Imputed hours vs donors: donor pt share by cell, imputed rows' weighted
  # pt share, median (drawn hours -- should track donor pt share)
  rc <- raw$arr %>%
    left_join(raw$kids %>% transmute(hh_id, child_id, age = floor(age_m / 12), child_weight = w),
              by = c('hh_id', 'child_id')) %>%
    rename(type_raw = toc) %>%
    resolve_regular_care()
  imp <- rc$arr %>%
    filter(hours_imputed, type_raw != 6) %>%
    semi_join(co, by = c('hh_id', 'child_id')) %>%
    mutate(toc_group = regular_care_toc_group(type_raw), age_cell = impute_pay_age_cell(age)) %>%
    group_by(toc_group, age_cell) %>%
    summarise(imputed_rows = n(), imputed_median = weighted_median(hours, child_weight),
              imputed_pt_pct = pct(sum(child_weight * (hours < PT_FT_CUT_HOURS)) / sum(child_weight)),
              .groups = 'drop')
  cells <- rc$cells %>%
    transmute(toc_group, age_cell, donors = n, median_hours, donor_pt_pct = pct(pt_share)) %>%
    full_join(imp, by = c('toc_group', 'age_cell'))

  # 1a vs 02, child by child (children 0-4 w/out K-8 arrangement -- 02 drops
  # K-8 rows by design)
  e02 <- new.env()
  sys.source('state_data/code/02_derive_nsece_constants.R', envir = e02)
  cc <- e02$classify_children(e02$read_household(nsece_root))
  # Labels only read below -- 02's child_weight / M never enters a share
  cc[c('kids', 'prim', 'arr', 'dropped')] <- lapply(cc[c('kids', 'prim', 'arr', 'dropped')], draw0)
  k8 <- cc$arr %>% filter(ecec_choice %in% 'K-8') %>% distinct(hh_id, child_id)
  band <- function(h) case_when(is.na(h) ~ NA_character_, h == 0 ~ 'none', h < PT_FT_CUT_HOURS ~ 'pt', TRUE ~ 'ft')
  l02 <- cc$kids %>% select(hh_id, child_id) %>%
    left_join(cc$prim %>% select(hh_id, child_id, l02 = ecec_choice, h02 = hours),
              by = c('hh_id', 'child_id')) %>%
    mutate(l02 = coalesce(l02, 'Parent Only'), b02 = if_else(l02 == 'Parent Only', 'none', band(h02))) %>%
    bind_rows(cc$dropped %>% mutate(l02 = NA_character_, b02 = NA_character_))
  agree <- co %>%
    anti_join(k8, by = c('hh_id', 'child_id')) %>%
    left_join(enr_new %>% transmute(hh_id, child_id,
                                    l1a = if_else(grepl('Priced Center', ecec_choice), 'Center-Based', ecec_choice),
                                    b1a = ecec_hours_choice), by = c('hh_id', 'child_id')) %>%
    left_join(l02, by = c('hh_id', 'child_id')) %>%
    mutate(status = case_when(
      is.na(l1a) & is.na(l02)                     ~ 'both NA',
      l1a %in% l02 & b1a %in% b02                 ~ 'agree',
      l1a %in% l02                                ~ 'label agrees, band differs',
      TRUE                                        ~ 'label differs'))
  agree_tab <- agree %>% group_by(status) %>%
    summarise(children_raw = n(), pct = pct(sum(w) / sum(agree$w)), .groups = 'drop')
  disagree_ex <- agree %>% filter(!status %in% c('agree', 'both NA')) %>% head(20)

  # Parent sex: children 0-4 w/ parent unit, male1 NA old vs new
  ch <- rd(est, 'nsece_children') %>% semi_join(co, by = c('hh_id', 'child_id')) %>%
    filter(!is.na(parent_unit_id)) %>% select(hh_id, child_id, parent_unit_id) %>%
    left_join(co, by = c('hh_id', 'child_id'))
  pu_new <- rd(est, 'nsece_parent_units') %>% mutate(parent_unit_id = as.character(parent_unit_id))
  pu_old <- rd(est_old, 'nsece_parent_units') %>% mutate(parent_unit_id = as.character(parent_unit_id))
  sx <- ch %>% mutate(parent_unit_id = as.character(parent_unit_id)) %>%
    left_join(pu_old %>% select(hh_id, parent_unit_id, male1_old = male1), by = c('hh_id', 'parent_unit_id')) %>%
    left_join(pu_new %>% select(hh_id, parent_unit_id, n_parents, male1, male2, hhm_id2),
              by = c('hh_id', 'parent_unit_id')) %>%
    mutate(pc = nsece_primary_caregiver(hhm_id2, male1, male2)) %>%
    group_by(n_parents, male1_old_na = is.na(male1_old), male1_new_na = is.na(male1),
             male1_changed = !is.na(male1_old) & !is.na(male1) & male1_old != male1,
             caregiver_na = is.na(pc)) %>%
    summarise(children_raw = n(), pct_of_cohort = pct(sum(w) / W), .groups = 'drop')

  cat('\n== E1. Label transitions old -> new run, children 0-4, by cause (raw n, % of cohort weight)\n')
  show_df(trans)
  cat('\n== E2. Parent Only / non-parental / pt-ft split, old vs new (weighted %)\n')
  show_df(shares)
  cat('\n== E3. Imputed hours (weighted donor draw) vs donor distribution, TOC group x age cell\n')
  show_df(cells)
  cat('\n== E4. 1a vs 02, children 0-4 w/out K-8 arrangement (K-8 children: ', nrow(semi_join(co, k8, by = c('hh_id', 'child_id'))), ')\n', sep = '')
  show_df(agree_tab)
  if (nrow(disagree_ex) > 0) show_df(disagree_ex)
  cat('\n== E5. Parent sex, children 0-4 w/ parent unit: male1 NA old vs new, caregiver NA\n')
  show_df(sx)
  list(trans = trans, shares = shares, cells = cells, agree = agree_tab, sex = sx)
}



main <- function() {

  nsece_root <- cli_value('--nsece', Sys.getenv('NSECE_ROOT'))
  est <- cli_value('--est')
  est_old <- cli_value('--est-old')
  out <- cli_value('--out')
  if (is.na(est) || is.na(out)) stop('usage: --nsece <dir> --est <estimation dir> --out <dir>')
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  set.seed(20261013)

  load_1b_helpers(c('get_primary_caregiver', 'derive_employment_choice',
                    'compute_income_pctiles', 'prepare_data_for_rf'))

  cat('reading NSECE household file\n')
  raw <- read_raw(nsece_root)
  rt <- rank_tables(est)

  A <- part_a(raw, rt$nsece$enrollment)

  # Frames, as 1b ecec block
  nsece_cuts <- compute_income_pctiles(rt$nsece$children, rt$nsece_hh, 'rank_group')
  acs_cuts   <- compute_income_pctiles(rt$acs$children, rt$acs_hh, 'rank_group')
  mk_nsece <- function(enr) prepare_data_for_rf(
    children = rt$nsece$children, parent_units = rt$nsece$parent_units,
    households = rt$nsece_hh, household_members = rt$nsece$household_members,
    enrollment = enr, income_pctiles = nsece_cuts, income_group_col = 'rank_group',
    source_label = 'NSECE', is_nsece = TRUE)
  tr_frame <- mk_nsece(rt$nsece$enrollment)
  # Label-filled: NA label -> placeholder, so frame keeps every child w/
  # features (prediction only; never trained on)
  pf_frame <- mk_nsece(rt$nsece$enrollment %>%
    mutate(fill = is.na(ecec_choice) | is.na(ecec_hours_choice),
           ecec_choice = if_else(fill, 'Missing', ecec_choice),
           ecec_hours_choice = if_else(fill, 'none', ecec_hours_choice)) %>%
    select(-fill))
  acs_frame <- prepare_data_for_rf(
    children = rt$acs$children, parent_units = rt$acs$parent_units,
    households = rt$acs_hh, household_members = NULL, enrollment = NULL,
    income_pctiles = acs_cuts, income_group_col = 'rank_group',
    source_label = 'ACS', is_nsece = FALSE)

  cat('fitting forests (1b settings)\n')
  models <- fit_forests(tr_frame)
  tr_rows  <- child_rows(tr_frame, models, 'NSECE')
  pf_rows  <- child_rows(pf_frame, models, 'NSECE')
  acs_rows <- child_rows(acs_frame, models, 'ACS')

  B <- part_b(A$kids, rt, models, tr_rows, pf_rows)

  # Run's saved ACS p0 at actual PC employment (as nsece_acs_comp.csv)
  emp <- acs_rows %>% distinct(hh_id, parent_unit_id, child_id, emp)
  saved_po <- rt$acs$enrollment %>%
    filter(ecec_choice == 'Parent Only', ecec_hours_choice == 'none') %>%
    mutate(parent_unit_id = as.character(parent_unit_id)) %>%
    inner_join(emp, by = c('hh_id', 'parent_unit_id', 'child_id')) %>%
    transmute(hh_id, parent_unit_id, child_id,
              saved_po = case_when(emp == 'pt' ~ p_enrollment.pt,
                                   emp == 'ft' ~ p_enrollment.ft,
                                   TRUE        ~ p_enrollment.none))
  C <- part_c(tr_rows, acs_rows, saved_po)
  D <- part_d(A$kids, rt)
  E <- if (!is.na(est_old)) part_e(raw, A$kids, est, est_old, nsece_root) else NULL

  # Run-level check: saved ACS p0, every child in acs_enrollment (3+ extras
  # incl.), PC employment from parent units -- as nsece_acs_comp.csv
  pu_emp <- rt$acs$parent_units %>%
    mutate(pc = get_primary_caregiver(hhm_id2, male1, male2),
           e = as.character(derive_employment_choice(if_else(pc == 1L, hours1, hours2))),
           parent_unit_id = as.character(parent_unit_id)) %>%
    select(hh_id, parent_unit_id, e)
  all_po <- rt$acs$enrollment %>%
    filter(ecec_choice == 'Parent Only', ecec_hours_choice == 'none') %>%
    mutate(parent_unit_id = as.character(parent_unit_id)) %>%
    left_join(pu_emp, by = c('hh_id', 'parent_unit_id')) %>%
    mutate(p = case_when(e %in% 'pt' ~ p_enrollment.pt, e %in% 'ft' ~ p_enrollment.ft,
                         TRUE ~ p_enrollment.none))
  cat('\n== C0. Saved run, ACS Parent Only, every predicted child (weighted %): ',
      pct(sum(all_po$p * all_po$child_weight) / sum(all_po$child_weight)),
      '  (children raw ', nrow(all_po), ')\n', sep = '')

  w <- function(x, f) write_csv(x, file.path(out, f))
  w(A$steps, 'a_steps.csv'); w(A$why_rows, 'a_hours_reason_rows.csv'); w(A$lost_why, 'a_lost_children.csv')
  w(B$stages, 'b_stages.csv'); w(B$dens, 'b_shares.csv'); w(B$why, 'b_frame_exclusions.csv')
  w(C$head, 'c_headline.csv'); w(C$by_dim, 'c_by_dim.csv'); w(C$dec, 'c_decomposition.csv')
  w(C$dec_by_par, 'c_decomposition_by_par.csv'); w(C$feat, 'c_features.csv')
  w(D, 'd_pc_hours.csv')
  if (!is.null(E)) {
    w(E$trans, 'e_transitions.csv'); w(E$shares, 'e_shares.csv'); w(E$cells, 'e_imputed_hours.csv')
    w(E$agree, 'e_1a_vs_02.csv'); w(E$sex, 'e_parent_sex.csv')
  }
  saveRDS(list(tr = tr_rows, pf = pf_rows, acs = acs_rows), file.path(out, 'child_rows.rds'))
  cat('\nwrote ', out, '\n', sep = '')
  invisible(NULL)
}



if (sys.nframe() == 0) main()
