#------------------------------------------------------------------------------
# 07_compare_nhes.R -- demand build against NHES-ECPP 2019, same population,
# same care-type rules, by Census region.
#
# VALIDATION ONLY -- writes nothing simulator reads. NHES-ECPP 2019 (NCES) is
# independent of NSECE, which trained the care-choice model. Public file has
# Census region only, so build geographies sum to regions (NYC inside NY).
#
# SAME POPULATION. Children 0-4 (AGE2018: age at 2018-12-31; ECPP excludes
# kindergarten), one primary arrangement each, simulator's rule
# (src/1_processing/1a_nsece_processing.R):
#   arrangement types
#     center      CPNNOWX = 1 -- center, preschool, pre-K not in a private home
#     home-based  non-relative (NCNOW = 1), other home (NCPLACE = 2), provider
#                 not already known (NCALKNE = 2) -- no prior relationship
#     other       relative care (RCNOW = 1); non-relative in child's home or
#                 both homes, or already known
#   paid        household's own net payment > 0 (RCCOST / NCCOST / CPCOST)
#   regular     arrangement hours >= MIN_HOURS a week (RCHRS / NCHRS / CPHRS)
#   primary     paid center > unpaid center > paid home > other paid >
#               unpaid home > other unpaid; hours break ties
#   part-time   primary arrangement under 20 hours a week
# NHES gives one provider per type in detail (RC/NC/CP); further arrangements
# of a type (RCTLHR, NCTLHR, CPTLHR) carry no payment or place, left out.
#
# BUILD SIDE, two versions per region:
#   targets  demand targets (NSCH participation, program counts, p0 mix) --
#            what simulator calibrates to; Parent Only = population - in care
#   model    p0 alone (national model over ACS families), Parent Only incl.
#
# SE: 80 jackknife replicates (FEWT1-80), variance = sum of squared replicate
# deviations.
#
# BY AGE (--by age): cells 0-2 and 3-4 in place of regions. Build side =
# model only (p0 by child age, cluster/acs_p0_shares_by_age.R); demand targets
# carry no age. Ages 0-2 hold no pre-K, almost no Head Start, no five-year-olds.
#
# Usage: Rscript state_data/code/07_compare_nhes.R [--min-hours 5] [--by region|age]
#   NHES file: state_data/raw/local/US_NHES__ecpp_public_use_file__2019.csv
#   (git-ignored; cluster copy in state_data/raw/)
#   prints table, writes out/nhes_comparison.csv
#------------------------------------------------------------------------------

# build_geo(), load_spec(), p0_rows(), GEOGRAPHIES; main() guarded
source('state_data/code/00_build_demand_targets.R')

NHES_FILE <- file.path(STATE_DATA_ROOT, 'raw', 'local',
                       'US_NHES__ecpp_public_use_file__2019.csv')
P0_BY_AGE_FILE <- 'US_ACS__p0_children_by_care_type_hours_age__2019.csv'
NHES_REGION <- c('1' = 'northeast', '2' = 'south', '3' = 'midwest', '4' = 'west')
PRIORITY <- c('Center-Based', 'Unpaid Center-Based', 'Paid Home-Based', 'Other Paid',
              'Unpaid Home-Based', 'Other Unpaid')
TYPES <- c(PRIORITY, 'Parent Only')
PT_CUT <- 20
N_REP  <- 80



nhes_children <- function(min_hours) {

  #----------------------------------------------------------------------------
  # NHES children 0-4 w/ primary care type, part-time flag, region, weights.
  #
  # Params:
  #   - min_hours (num): arrangement counts at or above this many hours a week
  #
  # Returns: (tibble) region, age_group, type, part_time, FEWT, FEWT1..FEWT80
  #----------------------------------------------------------------------------

  if (!file.exists(NHES_FILE)) stop('no ', NHES_FILE, ' -- copy it from the cluster')
  wcols <- c('FEWT', paste0('FEWT', seq_len(N_REP)))
  d <- read_chr(NHES_FILE)
  d <- d[num(d$AGE2018) <= 4, ]
  v <- function(x) num(d[[x]])
  pos <- function(x) !is.na(x) & x > 0

  hrs <- function(now, h) ifelse(v(now) == 1 & !is.na(v(h)) & v(h) >= min_hours, v(h), 0)
  rc_h <- hrs('RCNOW', 'RCHRS')
  nc_h <- hrs('NCNOW', 'NCHRS')
  cp_h <- hrs('CPNNOWX', 'CPHRS')

  rc_type <- ifelse(pos(v('RCCOST')), 'Other Paid', 'Other Unpaid')
  nc_home <- v('NCPLACE') == 2 & v('NCALKNE') == 2
  nc_paid <- pos(v('NCCOST'))
  nc_type <- ifelse(nc_home, ifelse(nc_paid, 'Paid Home-Based', 'Unpaid Home-Based'),
                    ifelse(nc_paid, 'Other Paid', 'Other Unpaid'))
  cp_type <- ifelse(pos(v('CPCOST')), 'Center-Based', 'Unpaid Center-Based')

  arr <- list(list(type = rc_type, h = rc_h), list(type = nc_type, h = nc_h),
              list(type = cp_type, h = cp_h))
  n <- nrow(d)
  best_type <- rep('Parent Only', n)
  best_rank <- rep(Inf, n)
  best_h    <- rep(0, n)
  # Hours summed within a type (relative and non-relative can both be Other)
  for (t in PRIORITY) {
    h_t <- Reduce(`+`, lapply(arr, function(a) ifelse(a$type == t, a$h, 0)))
    r_t <- match(t, PRIORITY)
    better <- h_t > 0 & (r_t < best_rank | (r_t == best_rank & h_t > best_h))
    best_type[better] <- t; best_rank[better] <- r_t; best_h[better] <- h_t[better]
  }

  out <- tibble::tibble(region = unname(NHES_REGION[d$CENREG]),
                        age_group = ifelse(num(d$AGE2018) <= 2, '0-2', '3-4'),
                        type = best_type,
                        part_time = best_type != 'Parent Only' & best_h < PT_CUT)
  for (w in wcols) out[[w]] <- num(d[[w]])
  out
}



nhes_shares <- function(x, cell = 'region') {

  #----------------------------------------------------------------------------
  # Share of children 0-4 by type, and part-time share by type, per region and
  # US, w/ jackknife SE.
  #
  # Params:
  #   - x (tibble): nhes_children() output; cell (chr): 'region' or 'age_group'
  #
  # Returns: (tibble) region (the cell), type, measure (share | part_time), nhes, nhes_se,
  #          n (unweighted children)
  #----------------------------------------------------------------------------

  wcols <- c('FEWT', paste0('FEWT', seq_len(N_REP)))
  x$region <- x[[cell]]
  x <- dplyr::bind_rows(x, dplyr::mutate(x, region = 'US'))
  rows <- list()
  for (g in unique(x$region)) {
    xg <- x[x$region == g, ]
    for (t in TYPES) {
      in_t <- xg$type == t
      share <- vapply(wcols, function(w) sum(xg[[w]][in_t]) / sum(xg[[w]]), 0)
      pt    <- vapply(wcols, function(w) sum(xg[[w]][in_t & xg$part_time]) /
                                         sum(xg[[w]][in_t]), 0)
      se <- function(r) sqrt(sum((r[-1] - r[1])^2))
      rows[[length(rows) + 1]] <- tibble::tibble(
        region = g, type = t, measure = c('share', 'part_time'),
        nhes = c(share[1], pt[1]), nhes_se = c(se(share), se(pt)), n = sum(in_t))
    }
  }
  dplyr::bind_rows(rows) %>% dplyr::filter(!(type == 'Parent Only' & measure == 'part_time'))
}



build_shares <- function() {

  #----------------------------------------------------------------------------
  # Build's shares by region: demand targets and p0 model, states + DC only.
  #
  # Returns: (tibble) region, type, measure, targets, model
  #----------------------------------------------------------------------------

  geos <- names(STATE_POSTAL_TO_FIPS)
  rows <- lapply(geos, function(g) {
    spec <- load_spec(g)
    utils::capture.output(res <- build_geo(spec))
    d  <- attr(res, 'diagnostics')
    tg <- tibble::tibble(type = c(res$ecec_type, 'Parent Only'),
                         children = c(res$children, d$children_0_4 - sum(res$children)),
                         part_time = c(res$part_time, 0))
    p0 <- p0_rows(g) %>%
      dplyr::mutate(type = dplyr::recode(ecec_type,
                                         'Low-Priced Center-Based' = 'Center-Based',
                                         'High-Priced Center-Based' = 'Center-Based'),
                    children = num(children)) %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(model = sum(children),
                       model_pt = sum(children[hours_choice == 'pt']), .groups = 'drop')
    dplyr::left_join(tg, p0, by = 'type') %>% dplyr::mutate(region = spec$region)
  })
  b <- dplyr::bind_rows(rows)
  b <- dplyr::bind_rows(b, dplyr::mutate(b, region = 'US')) %>%
    dplyr::group_by(region, type) %>%
    dplyr::summarise(children = sum(children), part_time = sum(part_time),
                     model = sum(model), model_pt = sum(model_pt), .groups = 'drop') %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(targets_share = children / sum(children),
                  model_share = model / sum(model)) %>%
    dplyr::ungroup()
  dplyr::bind_rows(
    dplyr::transmute(b, region, type, measure = 'share',
                     targets = targets_share, model = model_share),
    dplyr::transmute(b, region, type, measure = 'part_time',
                     targets = part_time / children, model = model_pt / model) %>%
      dplyr::filter(type != 'Parent Only'))
}



build_shares_by_age <- function() {

  #----------------------------------------------------------------------------
  # Model (p0) shares by child age group, states + DC.
  #
  # Returns: (tibble) region (age group or US), type, measure, targets (NA),
  #          model
  #----------------------------------------------------------------------------

  f <- raw_path(P0_BY_AGE_FILE)
  if (!file.exists(f)) stop('no ', f, ' -- run state_data/cluster/acs_p0_shares_by_age.R')
  b <- read_chr(f) %>%
    dplyr::filter(geo %in% names(STATE_POSTAL_TO_FIPS)) %>%
    dplyr::mutate(type = dplyr::recode(ecec_type,
                                       'Low-Priced Center-Based' = 'Center-Based',
                                       'High-Priced Center-Based' = 'Center-Based'),
                  region = ifelse(num(age) <= 2, '0-2', '3-4'),
                  children = num(children))
  b <- dplyr::bind_rows(b, dplyr::mutate(b, region = 'US')) %>%
    dplyr::group_by(region, type) %>%
    dplyr::summarise(model = sum(children), model_pt = sum(children[hours_choice == 'pt']),
                     .groups = 'drop') %>%
    dplyr::group_by(region) %>% dplyr::mutate(model_share = model / sum(model)) %>%
    dplyr::ungroup()
  dplyr::bind_rows(
    dplyr::transmute(b, region, type, measure = 'share', targets = NA_real_, model = model_share),
    dplyr::transmute(b, region, type, measure = 'part_time', targets = NA_real_,
                     model = model_pt / model) %>% dplyr::filter(type != 'Parent Only'))
}



main_nhes_check <- function(args = commandArgs(trailingOnly = TRUE)) {

  #----------------------------------------------------------------------------
  # NHES vs build, every region and US: prints, writes out/nhes_comparison.csv.
  #
  # Params:
  #   - args (chr): --min-hours h (default 5); --by region|age (default region)
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  mh <- if ('--min-hours' %in% args) num(cli_value('--min-hours', args = args)) else 5
  by <- if ('--by' %in% args) cli_value('--by', args = args) else 'region'
  if (!by %in% c('region', 'age')) stop('--by takes region or age')
  cat('NHES-ECPP 2019 vs build, children 0-4, arrangements >=', mh, 'hours a week, by', by, '\n')
  nh <- nhes_shares(nhes_children(mh), if (by == 'age') 'age_group' else 'region')
  bs <- if (by == 'age') build_shares_by_age() else build_shares()
  d <- dplyr::left_join(nh, bs, by = c('region', 'type', 'measure')) %>%
    dplyr::mutate(z_targets = (targets - nhes) / nhes_se,
                  z_model   = (model - nhes) / nhes_se,
                  min_hours = mh, cell = by)

  p <- function(x) sprintf('%5.1f', 100 * x)
  for (m in c('share', 'part_time')) {
    cat('\n==', if (m == 'share') 'share of children 0-4 (%)' else 'part-time share within type (%)', '\n')
    show_df(d %>% dplyr::filter(measure == m) %>%
              dplyr::transmute(region, type, n, nhes = p(nhes), se = p(nhes_se),
                               targets = p(targets), model = p(model),
                               z_targets = round(z_targets, 1), z_model = round(z_model, 1)))
  }

  f <- file.path(STATE_DATA_ROOT, 'out',
                 if (by == 'age') 'nhes_comparison_by_age.csv' else 'nhes_comparison.csv')
  readr::write_csv(d, f)
  cat('wrote', f, '\n')
  invisible(d)
}


if (sys.nframe() == 0) main_nhes_check()
