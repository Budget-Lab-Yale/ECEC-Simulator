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
# Common-rank NY/NYC controls written to <out.csv>.nyc_partition.csv:
# NY, NYC_parent, NYR from national p0; NYC from production p0. Diagnostic
# only -- NYC_parent never substitutes for own-rank production NYC.
# Runs on cluster (interfaces live there); aggregate summaries only.
# Needs ECEC_URBAN_DENSITY=1 (state setup) and an interface built under it --
# stops otherwise.
#
# Usage, from repo root:
#   ECEC_URBAN_DENSITY=1 Rscript state_data/cluster/acs_p0_shares.R <interface_dir> <out.csv>
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(data.table); library(magrittr); library(stringr)
})

source('src/shared_functions/constants.R')
source('src/shared_functions/get_choice_catalog.R')
source('src/shared_functions/select_geo_hh_ids.R')
source('src/shared_functions/compute_substate_alpha.R')   # substate_alpha_path()
source('src/shared_functions/urban_density.R')            # ECEC_URBAN_DENSITY switch



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
      require_urban_density_on('state_data/cluster/acs_p0_shares.R')
      out[[g]] <- lapply(paths, readRDS)
      for (t in names(out[[g]])) {
        check_urban_density_built(out[[g]][[t]]$urban_density, paste0('Sub-state alpha ', paths[[t]]), '2a')
      }
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



geo_hh_ids <- function(hh, skip_errors = TRUE) {

  #----------------------------------------------------------------------------
  # Household ids of every simulator geography (50 states + DC, sub-state
  # geographies), select_geo_hh_ids(). Shared w/ acs_p0_shares_by_age.R.
  #
  # Params: hh (tibble): acs_households_2019.csv, skip_errors (lgl): geography
  #   select_geo_hh_ids() rejects is skipped, said so (else error stops run)
  # Returns: (named list) geo -> hh_id vector
  #----------------------------------------------------------------------------

  geo_hh <- list()
  for (g in c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES))) {
    fips <- if (g %in% names(SUBSTATE_GEOGRAPHIES)) SUBSTATE_GEOGRAPHIES[[g]]$statefip
            else STATE_POSTAL_TO_FIPS[[g]]
    ids <- if (skip_errors) {
      tryCatch(select_geo_hh_ids(hh, g, fips),
               error = function(e) { cat('  skip', g, '--', conditionMessage(e), '\n'); NULL })
    } else {
      select_geo_hh_ids(hh, g, fips)
    }
    if (!is.null(ids)) geo_hh[[g]] <- ids
  }
  geo_hh
}



read_alpha_data <- function(iface, pu_type) {

  #----------------------------------------------------------------------------
  # National alpha data (p0, child_weights, row_ids) of one parent unit type.
  #
  # Params: iface (chr): interface dir, pu_type (chr): 'c1' | 'c2plus'
  # Returns: (list) alpha data
  #----------------------------------------------------------------------------

  require_urban_density_on('state_data/cluster/acs_p0_shares.R')
  alpha_f <- file.path(iface, 'estimation', paste0('alpha_', pu_type, '_2019.rds'))
  ad <- readRDS(alpha_f)
  ad <- if (!is.null(ad[[pu_type]])) ad[[pu_type]] else ad
  check_urban_density_built(ad$urban_density, paste0('Alpha matrix ', alpha_f), '2a')
  ad
}



nyc_partition_hh_ids <- function(geo_hh) {

  # Common-rank controls only; production NYC keeps own p0.
  # Returns: (list or NULL) NY, NYC_parent, NYR disjoint partition.
  if (!all(c('NY', 'NYC') %in% names(geo_hh))) return(NULL)
  if (anyDuplicated(geo_hh$NY) || anyDuplicated(geo_hh$NYC) ||
      !all(geo_hh$NYC %in% geo_hh$NY)) {
    stop('NYC household ids must be a unique subset of NY for partition controls.')
  }
  list(NY = geo_hh$NY, NYC_parent = geo_hh$NYC,
       NYR = setdiff(geo_hh$NY, geo_hh$NYC))
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

  geo_hh <- geo_hh_ids(hh)

  sub_ad <- substate_alpha_data(iface, geo_hh)
  nat_geo_hh <- geo_hh[setdiff(names(geo_hh), names(sub_ad))]
  partition_hh <- nyc_partition_hh_ids(geo_hh)
  export_hh <- c(nat_geo_hh, partition_hh[setdiff(names(partition_hh), names(nat_geo_hh))])

  res <- list()
  partition_res <- list()
  for (pu_type in c('c1', 'c2plus')) {
    ad <- read_alpha_data(iface, pu_type)
    n_children <- if (pu_type == 'c1') 1 else 2
    cat(pu_type, ': ', nrow(ad$p0), ' units x ', ncol(ad$p0), ' choices\n', sep = '')
    national <- geo_children(ad, n_children, export_hh)
    res[[pu_type]] <- national %>% filter(geo %in% names(nat_geo_hh)) %>% mutate(pu_type = pu_type)
    partition_res[[pu_type]] <- national %>% filter(geo %in% names(partition_hh))
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
  if (!is.null(partition_hh)) {
    partition <- bind_rows(partition_res) %>%
      separate(key, c('ecec_type', 'hours_choice'), sep = '[|]') %>%
      group_by(geo, ecec_type, hours_choice) %>%
      summarise(children = sum(children), .groups = 'drop') %>%
      bind_rows(out %>% filter(geo == 'NYC')) %>%
      arrange(geo, ecec_type, hours_choice)
    path <- paste0(out_f, '.nyc_partition.csv')
    write_csv(partition, path)
    writeLines(c(paste('interface:', iface),
      'NY, NYC_parent, NYR: national alpha p0; NYC: production p0 (own when available)',
      'NY = NYC_parent + NYR; partition controls never replace production NYC p0'),
      paste0(path, '.provenance.txt'))
    cat('wrote', path, '\n')
  }
  invisible(out)
}


if (sys.nframe() == 0) main_p0_shares()
