#------------------------------------------------------------------------------
# 07_compare_nhes.R -- demand build against NHES-ECPP 2019, same population,
# same care-type rules, by Census region.
#
# VALIDATION ONLY -- writes nothing simulator reads. NHES-ECPP 2019 (NCES) is
# independent of NSECE, which trained the care-choice model. Public file has
# Census region only, so build geographies sum to regions (NYC inside NY).
#
# SAME POPULATION. Children 0-4 (AGE2018: age at 2018-12-31), not yet in
# kindergarten -- NHES-ECPP universe (NHES:2019 Data File User's Manual, ch.
# 2); model side drops ACS children 0-4 in kindergarten (p0 by-age file,
# kindergarten key). One primary arrangement each, simulator's rule
# (src/1_processing/1a_nsece_processing.R):
#   arrangement types
#     center      CPNNOWX = 1 -- center, preschool, pre-K not in a private home
#     home-based  non-relative (NCNOW = 1), other home (NCPLACE = 2), provider
#                 not already known (NCALKNE = 2) -- no prior relationship
#     other       relative care (RCNOW = 1) -- prior relationship by kinship, so
#                 NHES asks "already known" of non-relatives only; non-relative
#                 in child's home or both homes, or already known
#   paid        household's payment > 0 (RCCOST / NCCOST / CPCOST)
#   regular     arrangement hours >= MIN_HOURS a week (RCHRS / NCHRS / CPHRS)
#   further     other regular arrangements of same kind (RCOTHC / NCOTHC /
#               CPOTHC = 1) carry total hours only (RCTLHR / NCTLHR / CPTLHR),
#               no payment or place -- take detailed arrangement's type; regular
#               when their total >= MIN_HOURS
#   primary     hours summed within type, then rank alone: paid center >
#               unpaid center > paid home > other paid > unpaid home > other
#               unpaid (as 1a -- no ties left for hours to break)
#   part-time   primary type under 20 hours a week
#
# BUILD SIDE, two versions per region:
#   targets  demand targets (NSCH participation, program counts, p0 mix) --
#            what simulator calibrates to; Parent Only = population - in care;
#            no age, so kindergarten children stay in
#   model    p0 alone (national model over ACS families), Parent Only incl.,
#            kindergarten children out (P0_BY_AGE_FILE)
#
# ACS AGE MIX (--by region, US rows). NHES age (AGE2018, 2018-12-31) and
# model's ACS children differ in single-year age mix. nhes_acs_mix = NHES
# records reweighted once to ACS single-year shares of model's children
# (P0_BY_AGE_FILE, states + DC, kindergarten out of denominator) -- care
# shares, part-time shares from same weights. Both single-year
# mixes, plus Census PEP 2019 single-year mix (PEP_SINGLE_AGE_FILE), written
# to out/nhes_age_mix.csv.
#
# BY AGE (--by age): cells 0-2 and 3-4 in place of regions. Build side =
# model only (p0 by child age, cluster/acs_p0_shares_by_age.R); demand targets
# carry no age. Ages 0-2 hold no pre-K, almost no Head Start, no five-year-olds.
#
# Usage: Rscript state_data/code/07_compare_nhes.R [--by region|age]
#   NHES file: state_data/raw/US/US_NHES__ecpp_public_use_file__2019.csv
#   (git-ignored; cluster copy in state_data/raw/)
#   prints table, writes out/nhes_comparison.csv (by age:
#   nhes_comparison_by_age.csv) and out/nhes_age_mix.csv
#------------------------------------------------------------------------------

# build_of(), read_chr(), raw_path(), GEOGRAPHIES; main() guarded
source('state_data/code/00_build_demand_targets.R')

NHES_FILE <- file.path(STATE_DATA_ROOT, 'raw', 'US',
                       'US_NHES__ecpp_public_use_file__2019.csv')
P0_BY_AGE_FILE <- 'US_ACS__p0_children_by_care_type_hours_age__2019.csv'
# Census PEP vintage 2019, resident population, 1 July 2019, ages 0-4 -- same
# file child age reweight in src/1_processing/1b_acs_processing.R reads
PEP_SINGLE_AGE_FILE <- file.path('resources', 'acs',
                                 'US_PEP__national_population_by_single_year_of_age_0_4__2019.csv')
NHES_REGION <- c('1' = 'northeast', '2' = 'south', '3' = 'midwest', '4' = 'west')
PRIORITY <- c('Center-Based', 'Unpaid Center-Based', 'Paid Home-Based', 'Other Paid',
              'Unpaid Home-Based', 'Other Unpaid')
TYPES <- c(PRIORITY, 'Parent Only')
MIN_HOURS <- 5    # NSECE regular care: 5+ hours a week
PT_CUT    <- 20



nhes_children <- function() {

  #----------------------------------------------------------------------------
  # NHES children 0-4 w/ primary care type, part-time flag, region, weight.
  #
  # Returns: (tibble) region, age, age_group, type, part_time, FEWT
  #----------------------------------------------------------------------------

  if (!file.exists(NHES_FILE)) stop('no ', NHES_FILE, ' -- copy it from the cluster')
  d <- read_chr(NHES_FILE)
  d <- d[num(d$AGE2018) <= 4, ]
  v <- function(x) num(d[[x]])
  pos <- function(x) !is.na(x) & x > 0
  regular <- function(flag, h) ifelse(v(flag) == 1 & !is.na(v(h)) & v(h) >= MIN_HOURS, v(h), 0)

  rc_type <- ifelse(pos(v('RCCOST')), 'Other Paid', 'Other Unpaid')
  nc_home <- v('NCPLACE') == 2 & v('NCALKNE') == 2
  nc_paid <- pos(v('NCCOST'))
  nc_type <- ifelse(nc_home, ifelse(nc_paid, 'Paid Home-Based', 'Unpaid Home-Based'),
                    ifelse(nc_paid, 'Other Paid', 'Other Unpaid'))
  cp_type <- ifelse(pos(v('CPCOST')), 'Center-Based', 'Unpaid Center-Based')

  # Detailed arrangement, then further arrangements of same kind (same type)
  arr <- list(list(type = rc_type, h = regular('RCNOW', 'RCHRS')),
              list(type = rc_type, h = regular('RCOTHC', 'RCTLHR')),
              list(type = nc_type, h = regular('NCNOW', 'NCHRS')),
              list(type = nc_type, h = regular('NCOTHC', 'NCTLHR')),
              list(type = cp_type, h = regular('CPNNOWX', 'CPHRS')),
              list(type = cp_type, h = regular('CPOTHC', 'CPTLHR')))
  n <- nrow(d)
  best_type <- rep('Parent Only', n)
  best_rank <- rep(Inf, n)
  best_h    <- rep(0, n)
  # Hours summed within a type (relative and non-relative can both be Other)
  for (t in PRIORITY) {
    h_t <- Reduce(`+`, lapply(arr, function(a) ifelse(!is.na(a$type) & a$type == t, a$h, 0)))
    r_t <- match(t, PRIORITY)
    better <- h_t > 0 & r_t < best_rank
    best_type[better] <- t; best_rank[better] <- r_t; best_h[better] <- h_t[better]
  }

  tibble::tibble(region = unname(NHES_REGION[d$CENREG]),
                 age = num(d$AGE2018),
                 age_group = ifelse(num(d$AGE2018) <= 2, '0-2', '3-4'),
                 type = best_type,
                 part_time = best_type != 'Parent Only' & best_h < PT_CUT,
                 FEWT = num(d$FEWT))
}



nhes_shares <- function(x, cell = 'region') {

  #----------------------------------------------------------------------------
  # Share of children 0-4 by type, and part-time share by type, per region and
  # US.
  #
  # Params:
  #   - x (tibble): nhes_children() output; cell (chr): 'region' or 'age_group'
  #
  # Returns: (tibble) region (the cell), type, measure (share | part_time), nhes,
  #          n (unweighted children)
  #----------------------------------------------------------------------------

  x$region <- x[[cell]]
  x <- dplyr::bind_rows(x, dplyr::mutate(x, region = 'US'))
  x %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(total = sum(FEWT)) %>%
    dplyr::group_by(region, type) %>%
    dplyr::summarise(share = sum(FEWT) / dplyr::first(total),
                     part_time = sum(FEWT[part_time]) / sum(FEWT),
                     n = dplyr::n(), .groups = 'drop') %>%
    # Type w/ no NHES children in a cell: share 0, part-time undefined
    tidyr::complete(region, type = TYPES, fill = list(share = 0, n = 0L)) %>%
    tidyr::pivot_longer(c(share, part_time), names_to = 'measure', values_to = 'nhes') %>%
    dplyr::filter(!(type == 'Parent Only' & measure == 'part_time')) %>%
    dplyr::select(region, type, measure, nhes, n)
}



p0_by_age <- function() {

  #----------------------------------------------------------------------------
  # P0_BY_AGE_FILE rows for states + DC, children in kindergarten dropped.
  #
  # Returns: (tibble) geo, ecec_type, hours_choice, age, children (num)
  #----------------------------------------------------------------------------

  f <- raw_path(P0_BY_AGE_FILE)
  if (!file.exists(f)) stop('no ', f, ' -- run state_data/cluster/acs_p0_shares_by_age.R')
  b <- read_chr(f) %>% dplyr::filter(geo %in% names(STATE_POSTAL_TO_FIPS))
  if (!'kindergarten' %in% names(b)) {
    stop(f, ' has no kindergarten key -- rerun acs_p0_shares_by_age.R w/ --kindergarten')
  }
  b %>%
    dplyr::filter(num(kindergarten) == 0) %>%
    dplyr::mutate(age = num(age), children = num(children))
}



acs_age_mix <- function() {

  #----------------------------------------------------------------------------
  # Single-year age shares of model's ACS children 0-4, states + DC,
  # kindergarten out (child-weighted, every care type incl. Parent Only).
  #
  # Returns: (named num) share by age '0'..'4'
  #----------------------------------------------------------------------------

  b <- p0_by_age()
  w <- tapply(b$children, b$age, sum)
  w[as.character(0:4)] / sum(w)
}



nhes_acs_mix <- function(x, mix) {

  #----------------------------------------------------------------------------
  # US share of children 0-4 by type, part-time share within type: NHES
  # records reweighted once to ACS single-year mix (FEWT x ACS share / NHES
  # share at child's age), both measures from same weights.
  #
  # Params:
  #   - x (tibble): nhes_children() output
  #   - mix (named num): acs_age_mix()
  #
  # Returns: (tibble) region ('US'), type, measure (share | part_time),
  #   nhes_acs_mix
  #----------------------------------------------------------------------------

  own <- tapply(x$FEWT, x$age, sum)
  own <- own / sum(own)
  a <- as.character(x$age)
  w <- x$FEWT * mix[a] / own[a]
  dplyr::bind_rows(lapply(TYPES, function(t) {
    i <- x$type == t
    out <- tibble::tibble(region = 'US', type = t, measure = 'share',
                          nhes_acs_mix = sum(w[i]) / sum(w))
    if (t == 'Parent Only') return(out)
    dplyr::bind_rows(out, tibble::tibble(region = 'US', type = t, measure = 'part_time',
                                         nhes_acs_mix = sum(w[i & x$part_time]) / sum(w[i])))
  }))
}



build_shares <- function() {

  #----------------------------------------------------------------------------
  # Build's shares by region: demand targets and p0 model, states + DC only.
  # Model side from P0_BY_AGE_FILE, kindergarten out -- same children as NHES.
  # Demand targets carry no age, so kindergarten stays in them.
  #
  # Returns: (tibble) region, type, measure, targets, model
  #----------------------------------------------------------------------------

  geos <- names(STATE_POSTAL_TO_FIPS)
  p0k <- p0_by_age() %>%
    dplyr::mutate(type = dplyr::recode(ecec_type,
                                       'Low-Priced Center-Based' = 'Center-Based',
                                       'High-Priced Center-Based' = 'Center-Based'))
  rows <- lapply(geos, function(g) {
    res  <- build_of(g)
    care <- res$care
    tg <- tibble::tibble(type = c(care$ecec_type, 'Parent Only'),
                         children = c(care$children,
                                      res$participation$children_0_4 - sum(care$children)),
                         part_time = c(care$part_time, 0))
    p0 <- p0k %>%
      dplyr::filter(geo == g) %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(model = sum(children),
                       model_pt = sum(children[hours_choice == 'pt']), .groups = 'drop')
    dplyr::left_join(tg, p0, by = 'type') %>% dplyr::mutate(region = res$region)
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
  # Model (p0) shares by child age group, states + DC, kindergarten out.
  #
  # Returns: (tibble) region (age group or US), type, measure, targets (NA),
  #          model
  #----------------------------------------------------------------------------

  b <- p0_by_age() %>%
    dplyr::mutate(type = dplyr::recode(ecec_type,
                                       'Low-Priced Center-Based' = 'Center-Based',
                                       'High-Priced Center-Based' = 'Center-Based'),
                  region = ifelse(age <= 2, '0-2', '3-4'))
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
  # NHES vs build, every region and US: prints, writes out/nhes_comparison.csv
  # (file names: header), out/nhes_age_mix.csv.
  #
  # Params:
  #   - args (chr): --by region|age (default region)
  #
  # Returns: (tibble) invisibly
  #----------------------------------------------------------------------------

  by <- if ('--by' %in% args) cli_value('--by', args = args) else 'region'
  if (!by %in% c('region', 'age')) stop('--by takes region or age')
  cat('NHES-ECPP 2019 vs build, children 0-4 not in kindergarten, arrangements >=',
      MIN_HOURS, 'hours a week, by', by, '\n')
  kids <- nhes_children()
  nh <- nhes_shares(kids, if (by == 'age') 'age_group' else 'region')
  bs <- if (by == 'age') build_shares_by_age() else build_shares()
  mix <- acs_age_mix()
  if (by == 'region') {
    nh <- dplyr::left_join(nh, nhes_acs_mix(kids, mix),
                           by = c('region', 'type', 'measure'))
  }
  nhes_mix <- tapply(kids$FEWT, kids$age, sum)
  pep <- readr::read_csv(PEP_SINGLE_AGE_FILE, show_col_types = FALSE)
  pep <- setNames(pep$TOT_POP, pep$AGE)[as.character(0:4)]
  ages <- tibble::tibble(age    = 0:4,
                         nhes   = as.numeric(nhes_mix[as.character(0:4)] / sum(nhes_mix)),
                         acs    = as.numeric(mix),
                         census = as.numeric(pep / sum(pep)))
  d <- dplyr::left_join(nh, bs, by = c('region', 'type', 'measure')) %>%
    dplyr::mutate(cell = by)

  p <- function(x) sprintf('%5.1f', 100 * x)
  for (m in c('share', 'part_time')) {
    cat('\n==', if (m == 'share') 'share of children 0-4 (%)' else 'part-time share within type (%)', '\n')
    show_df(d %>% dplyr::filter(measure == m) %>%
              dplyr::transmute(region, type, n, nhes = p(nhes),
                               targets = p(targets), model = p(model)))
  }

  cat('\n== single-year age mix of children 0-4 (%)\n')
  show_df(dplyr::transmute(ages, age, nhes = p(nhes), acs = p(acs), census = p(census)))
  if (by == 'region') {
    for (m in c('share', 'part_time')) {
      cat('\n== US', if (m == 'share') 'share of children 0-4' else 'part-time share within type',
          '(%), NHES records reweighted to ACS age mix\n')
      show_df(d %>% dplyr::filter(region == 'US', measure == m) %>%
                dplyr::transmute(type, nhes_acs_mix = p(nhes_acs_mix), model = p(model)))
    }
  }

  f <- file.path(STATE_DATA_ROOT, 'out',
                 if (by == 'age') 'nhes_comparison_by_age.csv' else 'nhes_comparison.csv')
  readr::write_csv(d, f)
  fa <- file.path(STATE_DATA_ROOT, 'out', 'nhes_age_mix.csv')
  readr::write_csv(ages, fa)
  cat('wrote', f, 'and', fa, '\n')
  invisible(d)
}


if (sys.nframe() == 0) main_nhes_check()
