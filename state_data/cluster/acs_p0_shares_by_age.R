#------------------------------------------------------------------------------
# acs_p0_shares_by_age.R -- acs_p0_shares.R summary split by child age.
#
# VALIDATION INPUT ONLY -- demand build reads acs_p0_shares.R's summary, not
# this one. Same p0, same child weights, same geographies; one more key: child
# age (0-4) from calibration_<type>_2019.csv (child_age.1, child_age.2). Lets
# a validation subset build to a benchmark's ages (e.g. 0-2, no pre-K, no
# five-year-olds).
#
# Alignment: calibration rows joined to alpha row_ids by every shared id
# column; stops unless joined child weights equal alpha's child weights.
# Summed over age, output must equal acs_p0_shares.R's -- checked at end when
# that file is passed.
#
# Sub-state geography (NYC): own p0 when alpha_<type>_2019_NYC.rds exists,
# else national, logged -- same rule as acs_p0_shares.R.
#
# KINDERGARTEN (--kindergarten <flags.csv>, acs_kindergarten_flags_2019.py):
# extra key `kindergarten` (1 = child 0-4 already in kindergarten, joined on
# hh_id = SERIAL, child_id = PERNUM). NHES-ECPP leaves these children out, so
# 07_compare_nhes.R drops them. Sum over key unchanged.
#
# Usage, from repo root (cluster):
#   Rscript state_data/cluster/acs_p0_shares_by_age.R <interface_dir> <out.csv> [<p0_summary.csv>]
#     [--kindergarten <flags.csv>]
#------------------------------------------------------------------------------

source('state_data/cluster/acs_p0_shares.R')   # helpers; main guarded



child_ages <- function(iface, pu_type, ad) {

  #----------------------------------------------------------------------------
  # Child ages, ids aligned to alpha rows, one column per child slot.
  #
  # Params: iface (chr), pu_type (chr), ad (list): alpha data
  # Returns: (list) ages, ids -- n_units x n_children matrices
  #----------------------------------------------------------------------------

  cal <- fread(file.path(iface, 'estimation', 'data', paste0('calibration_', pu_type, '_2019.csv')))
  keys <- intersect(names(ad$row_ids), c('hh_id', 'parent_unit_id', 'pseudofamily_id'))
  if (!length(keys)) stop(pu_type, ': alpha row_ids share no id column w/ calibration file')
  cat(pu_type, ': joining on ', paste(keys, collapse = ', '), '\n', sep = '')
  rid <- as.data.table(ad$row_ids)[, ..keys][, .row := .I]
  cal <- unique(cal, by = keys)
  j <- merge(rid, cal, by = keys, all.x = TRUE, sort = FALSE)[order(.row)]
  if (nrow(j) != nrow(rid)) stop(pu_type, ': join changed row count')

  n_children <- if (pu_type == 'c1') 1 else 2
  ages <- sapply(seq_len(n_children), function(c) j[[paste0('child_age.', c)]])
  ids  <- sapply(seq_len(n_children), function(c) j[[paste0('child_id.', c)]])
  for (c in seq_len(n_children)) {
    a <- ad$child_weights[[paste0('child_weight.', c)]]
    b <- j[[paste0('child_weight.', c)]]
    bad <- !(is.na(a) & is.na(b)) & (is.na(a) | is.na(b) | abs(a - b) > 1e-8)
    if (any(bad)) stop(pu_type, ' child ', c, ': ', sum(bad), ' rows w/ child weight mismatch -- misaligned')
  }
  list(ages = matrix(ages, ncol = n_children), ids = matrix(ids, ncol = n_children))
}



geo_children_by_age <- function(ad, n_children, geo_hh, ages, kg) {

  #----------------------------------------------------------------------------
  # geo_children() w/ child age, kindergarten flag as extra keys.
  #
  # Params: as geo_children(); ages (matrix) from child_ages();
  #   kg (logical matrix, same shape): child in kindergarten
  # Returns: (tibble) geo, key, age, kindergarten, children
  #----------------------------------------------------------------------------

  catalog <- get_choice_catalog(n_children)
  out <- list()
  for (c in seq_len(n_children)) {
    w <- ad$child_weights[[paste0('child_weight.', c)]]
    w[is.na(w)] <- 0
    PM <- (ad$p0 %*% choice_indicator(catalog, c)) * w
    for (a in sort(unique(ages[, c]))) {
      for (k in c(FALSE, TRUE)) {
        at_a <- !is.na(ages[, c]) & ages[, c] == a & kg[, c] == k
        if (!any(at_a)) next
        for (g in names(geo_hh)) {
          in_g <- at_a & ad$row_ids$hh_id %in% geo_hh[[g]]
          out[[length(out) + 1]] <- tibble(geo = g, key = colnames(PM), age = a,
                                           kindergarten = as.integer(k),
                                           children = colSums(PM[in_g, , drop = FALSE]))
        }
      }
    }
  }
  bind_rows(out)
}



main_by_age <- function(args = commandArgs(trailingOnly = TRUE)) {

  kg_f <- NULL
  if ('--kindergarten' %in% args) {
    i <- match('--kindergarten', args)
    kg_f <- args[i + 1]
    args <- args[-c(i, i + 1)]
  }
  if (!length(args) %in% 2:3) {
    stop('usage: Rscript state_data/cluster/acs_p0_shares_by_age.R <interface_dir> <out.csv> ',
         '[<p0_summary.csv>] [--kindergarten <flags.csv>]')
  }
  iface <- args[1]; out_f <- args[2]
  kg_ids <- if (is.null(kg_f)) character(0) else {
    k <- fread(kg_f)
    paste(k$SERIAL, k$PERNUM)
  }
  hh <- fread(file.path(iface, 'estimation', 'data', 'acs_households_2019.csv')) %>% tibble()
  geos <- c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))
  geo_hh <- list()
  for (g in geos) {
    fips <- if (g %in% names(SUBSTATE_GEOGRAPHIES)) SUBSTATE_GEOGRAPHIES[[g]]$statefip
            else STATE_POSTAL_TO_FIPS[[g]]
    geo_hh[[g]] <- select_geo_hh_ids(hh, g, fips)
  }

  # Sub-state geography w/ own p0 (NYC income ranks) -- as acs_p0_shares.R
  sub_ad <- substate_alpha_data(iface, geo_hh)
  nat_geo_hh <- geo_hh[setdiff(names(geo_hh), names(sub_ad))]

  res <- list()
  for (pu_type in c('c1', 'c2plus')) {
    ad <- readRDS(file.path(iface, 'estimation', paste0('alpha_', pu_type, '_2019.rds')))
    ad <- if (!is.null(ad[[pu_type]])) ad[[pu_type]] else ad
    n_children <- if (pu_type == 'c1') 1 else 2
    ca <- child_ages(iface, pu_type, ad)
    ages <- ca$ages
    kg <- matrix(paste(ad$row_ids$hh_id, ca$ids) %in% kg_ids, ncol = ncol(ages))
    cat(pu_type, ' ages: ', paste(names(table(ages)), table(ages), sep = '=', collapse = ' '),
        '; kindergarten slots: ', sum(kg), '\n', sep = '')
    res[[pu_type]] <- geo_children_by_age(ad, n_children, nat_geo_hh, ages, kg)
    for (g in names(sub_ad)) {
      sad <- sub_ad[[g]][[pu_type]]
      sca <- child_ages(iface, pu_type, sad)
      skg <- matrix(paste(sad$row_ids$hh_id, sca$ids) %in% kg_ids, ncol = ncol(sca$ages))
      res[[paste(pu_type, g)]] <- geo_children_by_age(sad, n_children, geo_hh[g], sca$ages, skg)
    }
  }
  if (!is.null(kg_f) && !any(bind_rows(res)$kindergarten == 1)) {
    stop('--kindergarten given but no child matched ', kg_f, ' -- check hh_id = SERIAL, child_id = PERNUM')
  }

  out <- bind_rows(res) %>%
    separate(key, c('ecec_type', 'hours_choice'), sep = '[|]') %>%
    group_by(geo, ecec_type, hours_choice, age, kindergarten) %>%
    summarise(children = sum(children), .groups = 'drop') %>%
    arrange(geo, ecec_type, hours_choice, age, kindergarten)
  write_csv(out, out_f)
  cat('wrote', out_f, nrow(out), 'rows\n')

  if (length(args) == 3) {
    ref <- read_csv(args[3], show_col_types = FALSE)
    s <- out %>% group_by(geo, ecec_type, hours_choice) %>%
      summarise(children = sum(children), .groups = 'drop') %>%
      inner_join(ref, by = c('geo', 'ecec_type', 'hours_choice'), suffix = c('', '_ref'))
    gap <- max(abs(s$children - s$children_ref) / pmax(s$children_ref, 1))
    cat(sprintf('summed over age vs %s: %d cells, max relative gap %.2e\n', args[3], nrow(s), gap))
    if (gap > 1e-6) stop('by-age summary does not sum to the p0 summary')
  }
  invisible(out)
}


if (sys.nframe() == 0) main_by_age()
