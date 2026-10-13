#------------------------------------------------------------------------------
# acs_p0_shares.R -- simulator's base-year care mix, summed per geography.
#
# Share model for state_data/code/00_build_demand_targets.R: each
# geography's care mix from simulator's own base-year choice probabilities
# (p0) over that geography's ACS households. p0 = calibration's p_observed, stored in
# estimation/alpha_<type>_2019.rds w/ child weights, row_ids (hh_id).
#
# Per geography g, care type t, hours h (pt = 10 hrs/week, ft = 40):
#
#   children[g, t, h] = sum_i 1{i in g} sum_c w_ic sum_j p0_ij 1{type_c(j) = t,
#                                                              hours_c(j) = h}
#
# Weighted children 0-4 (real-world population, ACS child weights), both
# children of a 2-child choice counted. 'Parent Only' kept -- the build drops
# it, but the total says how many children p0 holds against PEP.
#
# Geographies: 50 states + DC by STATEFIP, NYC by STATEFIP + COUNTYFIP
# (select_geo_hh_ids()). An interface w/out countyfip writes no NYC rows and
# says so.
#
# Sub-state geography ranked twice (compute_substate_alpha.R): NYC rows read
# NYC's own p0 (estimation/alpha_<type>_2019_NYC.rds, NYC income ranks) when
# both types exist; else national p0 (NYC ranked inside NY), logged. NY rows
# always national p0.
#
# Runs on the cluster (interfaces live there). Writes the summary only.
#
# Usage, from repo root:
#   Rscript state_data/cluster/acs_p0_shares.R <interface_dir> <out.csv>
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(data.table); library(magrittr); library(stringr)
})

source('src/shared_functions/constants.R')
source('src/shared_functions/get_choice_catalog.R')
source('src/shared_functions/select_geo_hh_ids.R')
source('src/shared_functions/compute_substate_alpha.R')   # substate_alpha_path()



substate_alpha_data <- function(iface, geo_hh) {

  #----------------------------------------------------------------------------
  # Own alpha data (p0 on own income ranks) of each sub-state geography in
  # geo_hh, when interface holds both types; else geo left on national p0,
  # logged.
  #
  # Params: iface (chr): interface dir, geo_hh (named list): geo -> hh_id
  # Returns: (list) geo -> list(c1 = alpha data, c2plus = alpha data)
  #----------------------------------------------------------------------------

  out <- list()
  for (g in intersect(names(SUBSTATE_GEOGRAPHIES), names(geo_hh))) {
    paths <- vapply(c('c1', 'c2plus'), function(t) {
      substate_alpha_path(file.path(iface, 'estimation'), t, 2019, g)
    }, character(1))
    if (all(file.exists(paths))) {
      out[[g]] <- lapply(paths, readRDS)
      cat(g, ': own p0 (', paste(basename(paths), collapse = ', '), ', scheme ',
          if (is.null(out[[g]]$c1$income_rank_scheme)) '?' else out[[g]]$c1$income_rank_scheme,
          ')\n', sep = '')
    } else {
      cat(g, ': no ', paste(basename(paths[!file.exists(paths)]), collapse = ', '),
          ' -- national p0, ', g, ' ranked inside ', SUBSTATE_GEOGRAPHIES[[g]]$parent, '\n', sep = '')
    }
  }
  out
}



choice_indicator <- function(catalog, child) {

  #----------------------------------------------------------------------------
  # Choice x (type, hours) 0/1 matrix for child 1 or 2 of a catalog.
  #
  # Params: catalog (tibble): get_choice_catalog() output, child (int)
  # Returns: (matrix) n_choices x K, colnames 'type|hours'
  #----------------------------------------------------------------------------

  key <- paste(catalog[[paste0('child', child, '_ecec_type')]],
               catalog[[paste0('child', child, '_hours_choice')]], sep = '|')
  keys <- sort(unique(key))
  M <- matrix(0, nrow = length(key), ncol = length(keys),
              dimnames = list(NULL, keys))
  M[cbind(seq_along(key), match(key, keys))] <- 1
  M
}



geo_children <- function(ad, n_children, geo_hh) {

  #----------------------------------------------------------------------------
  # Weighted children by (type, hours) for every geography, one pu type. NA
  # child weight counts 0.
  #
  # Params: ad (list): alpha data (p0, child_weights, row_ids), n_children
  #   (int), geo_hh (named list): geo -> hh_id vector
  # Returns: (tibble) geo, key, children
  #----------------------------------------------------------------------------

  if (is.null(ad$p0) || is.null(ad$child_weights)) {
    stop('alpha data lacks p0/child_weights -- interface predates state support')
  }
  catalog <- get_choice_catalog(n_children)
  if (ncol(ad$p0) != nrow(catalog)) {
    stop('p0 has ', ncol(ad$p0), ' choices, catalog ', nrow(catalog))
  }

  out <- list()
  for (c in seq_len(n_children)) {
    w <- ad$child_weights[[paste0('child_weight.', c)]]
    w[is.na(w)] <- 0
    PM <- (ad$p0 %*% choice_indicator(catalog, c)) * w   # units x K
    for (g in names(geo_hh)) {
      in_g <- ad$row_ids$hh_id %in% geo_hh[[g]]
      out[[length(out) + 1]] <- tibble(geo = g, key = colnames(PM),
                                       children = colSums(PM[in_g, , drop = FALSE]))
    }
  }
  bind_rows(out)
}



main_p0_shares <- function(args = commandArgs(trailingOnly = TRUE)) {

  #----------------------------------------------------------------------------
  # Entry point: p0 children by geography, care type, hours, to CSV. Geography
  # select_geo_hh_ids() rejects is skipped, said so.
  #
  # Params: args (chr): c(interface_dir, out.csv)
  # Returns: (tibble, invisible) geo, ecec_type, hours_choice, children
  #----------------------------------------------------------------------------

  if (length(args) != 2) {
    stop('usage: Rscript state_data/cluster/acs_p0_shares.R <interface_dir> <out.csv>')
  }
  iface <- args[1]; out_f <- args[2]

  hh_f <- file.path(iface, 'estimation', 'data', 'acs_households_2019.csv')
  if (!file.exists(hh_f)) stop('no households table at ', hh_f)
  hh <- fread(hh_f) %>% tibble()
  cat('households:', nrow(hh), '| countyfip:',
      if ('countyfip' %in% names(hh) && any(!is.na(hh$countyfip))) 'yes' else 'NO', '\n')

  geos <- c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))
  geo_hh <- list()
  for (g in geos) {
    fips <- if (g %in% names(SUBSTATE_GEOGRAPHIES)) SUBSTATE_GEOGRAPHIES[[g]]$statefip
            else STATE_POSTAL_TO_FIPS[[g]]
    ids <- tryCatch(select_geo_hh_ids(hh, g, fips),
                    error = function(e) { cat('  skip', g, '--', conditionMessage(e), '\n'); NULL })
    if (!is.null(ids)) geo_hh[[g]] <- ids
  }

  sub_ad <- substate_alpha_data(iface, geo_hh)
  nat_geo_hh <- geo_hh[setdiff(names(geo_hh), names(sub_ad))]

  res <- list()
  for (pu_type in c('c1', 'c2plus')) {
    f <- file.path(iface, 'estimation', paste0('alpha_', pu_type, '_2019.rds'))
    ad <- readRDS(f)
    ad <- if (!is.null(ad[[pu_type]])) ad[[pu_type]] else ad
    n_children <- if (pu_type == 'c1') 1 else 2
    cat(pu_type, ': ', nrow(ad$p0), ' units x ', ncol(ad$p0), ' choices\n', sep = '')
    res[[pu_type]] <- geo_children(ad, n_children, nat_geo_hh) %>% mutate(pu_type = pu_type)
    for (g in names(sub_ad)) {
      res[[paste(pu_type, g)]] <- geo_children(sub_ad[[g]][[pu_type]], n_children, geo_hh[g]) %>%
        mutate(pu_type = pu_type)
    }
  }

  out <- bind_rows(res) %>%
    separate(key, c('ecec_type', 'hours_choice'), sep = '[|]') %>%
    group_by(geo, ecec_type, hours_choice) %>%
    summarise(children = sum(children), .groups = 'drop') %>%
    arrange(geo, ecec_type, hours_choice)

  tot <- out %>% group_by(geo) %>% summarise(children = sum(children), .groups = 'drop')
  cat('\nweighted children 0-4 held by p0, first geographies:\n')
  print(as.data.frame(head(tot, 8)), row.names = FALSE)

  write_csv(out, out_f)
  cat('wrote', out_f, '\n')
  invisible(out)
}


if (sys.nframe() == 0) main_p0_shares()
