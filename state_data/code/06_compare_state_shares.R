#------------------------------------------------------------------------------
# 06_compare_state_shares.R -- care mix of every geography (50 states, DC, NYC)
# against its region's NHES-ECPP 2019 and NSECE 2019 shares.
#
# VALIDATION ONLY -- writes nothing simulator reads. NHES, NSECE public files
# carry Census region, no state, so a geography's benchmark = its region.
#
# SAME POPULATION, each column: share of ALL children 0-4 (Parent Only in),
# six build care types + Parent Only, primary arrangement, regular care on a
# five-hour screen.
#   targets  demand targets (step 4) over PEP population 0-4 -- inputs carry
#            no kindergarten flag, so kindergarten children stay in (1.2% of
#            ACS children 0-4)
#   model    simulator p0 before geographic calibration (step 1), geography's
#            own ACS families, kindergarten children out
#   NHES     region's NHES records, kindergarten out, reweighted once to
#            region's model single-year age mix (07_ does same for US)
#   NSECE    region's universe-A shares, kindergarten children (any K-8
#            arrangement) out -- 02_, quantity share_of_children_region
#
# TOTAL ERROR = dissimilarity index: half the summed absolute share gaps over
# seven categories -- share of children who would have to change category to
# match benchmark. 0 = same mix; 1 = no overlap.
#
# Usage: Rscript state_data/code/06_compare_state_shares.R
#   prints summary, writes out/state_share_comparison.csv (geo x type) and
#   out/state_share_errors.csv (geo)
#------------------------------------------------------------------------------

# nhes_children(), TYPES, build_of(), raw_path(), read_chr(), GEOGRAPHIES;
# main() guarded
source('state_data/code/07_compare_nhes.R')



region_of <- function(geo) get_spec(parent_geo(geo))$region



model_shares <- function() {

  #----------------------------------------------------------------------------
  # p0 shares of all children 0-4 per geography, kindergarten out; age mix
  # by region for NHES reweight.
  #
  # Returns: (list) shares (tibble geo, type, model), age_mix (tibble region,
  #   age, share)
  #----------------------------------------------------------------------------

  b <- read_chr(raw_path(P0_BY_AGE_FILE)) %>%
    dplyr::filter(kindergarten == '0') %>%
    dplyr::mutate(children = num(children), age = num(age),
                  type = dplyr::recode(ecec_type,
                                       'Low-Priced Center-Based'  = 'Center-Based',
                                       'High-Priced Center-Based' = 'Center-Based'))
  missing <- setdiff(GEOGRAPHIES, unique(b$geo))
  if (length(missing)) stop('p0 by-age file lacks ', paste(missing, collapse = ', '))
  shares <- b %>%
    dplyr::filter(geo %in% GEOGRAPHIES) %>%
    dplyr::group_by(geo, type) %>%
    dplyr::summarise(children = sum(children), .groups = 'drop') %>%
    dplyr::group_by(geo) %>%
    dplyr::mutate(model = children / sum(children)) %>%
    dplyr::ungroup() %>%
    dplyr::select(geo, type, model)
  # Region age mix -- states + DC only (NYC inside NY)
  st <- names(STATE_POSTAL_TO_FIPS)
  rg <- setNames(vapply(st, region_of, ''), st)
  age_mix <- b %>%
    dplyr::filter(geo %in% st) %>%
    dplyr::mutate(region = rg[geo]) %>%
    dplyr::group_by(region, age) %>%
    dplyr::summarise(children = sum(children), .groups = 'drop') %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(share = children / sum(children)) %>%
    dplyr::ungroup()
  list(shares = shares, age_mix = age_mix)
}



nhes_region_shares <- function(age_mix) {

  #----------------------------------------------------------------------------
  # NHES share of children 0-4 by type, each region, records reweighted once
  # to region's model age mix (FEWT x model share / NHES share at age).
  #
  # Params:
  #   - age_mix (tibble): region, age, share
  #
  # Returns: (tibble) region, type, nhes, n
  #----------------------------------------------------------------------------

  x <- nhes_children()
  dplyr::bind_rows(lapply(split(x, x$region), function(r) {
    own <- tapply(r$FEWT, r$age, sum); own <- own / sum(own)
    m <- age_mix[age_mix$region == r$region[1], ]
    target <- setNames(m$share, m$age)
    a <- as.character(r$age)
    w <- r$FEWT * target[a] / own[a]
    tibble::tibble(region = r$region[1], type = TYPES,
                   nhes = vapply(TYPES, function(t) sum(w[r$type == t]) / sum(w), 0),
                   n = vapply(TYPES, function(t) sum(r$type == t), 0L))
  }))
}



nsece_region_shares <- function() {

  #----------------------------------------------------------------------------
  # NSECE universe-A share of children 0-4 by type, each region (02_).
  #
  # Returns: (tibble) region, type, nsece
  #----------------------------------------------------------------------------

  k <- read_chr(file.path(STATE_DATA_ROOT, 'out', 'nsece_constants.csv'))
  k <- k[k$quantity == 'share_of_children_region', ]
  if (!nrow(k)) stop('nsece_constants.csv lacks share_of_children_region -- rerun 02_')
  tibble::tibble(region = sub(':.*$', '', k$key), type = sub('^[^:]*:', '', k$key),
                 nsece = num(k$value)) %>%
    dplyr::filter(region != 'US')
}



target_shares <- function() {

  #----------------------------------------------------------------------------
  # Demand-target share of children 0-4 by type per geography (build_of()).
  #
  # Returns: (tibble) geo, type, targets, children_0_4
  #----------------------------------------------------------------------------

  dplyr::bind_rows(lapply(GEOGRAPHIES, function(g) {
    r <- build_of(g)
    pop <- r$participation$children_0_4
    tibble::tibble(geo = g, type = c(r$care$ecec_type, 'Parent Only'),
                   targets = c(r$care$children, pop - sum(r$care$children)) / pop,
                   children_0_4 = pop)
  }))
}



main_state_shares <- function() {

  #----------------------------------------------------------------------------
  # Every geography vs region's NHES, NSECE: writes both out/ files, prints
  # summary.
  #
  # Returns: (tibble) errors, invisibly
  #----------------------------------------------------------------------------

  m  <- model_shares()
  d <- target_shares() %>%
    dplyr::left_join(m$shares, by = c('geo', 'type')) %>%
    dplyr::mutate(region = vapply(geo, region_of, '')) %>%
    dplyr::left_join(nhes_region_shares(m$age_mix), by = c('region', 'type')) %>%
    dplyr::left_join(nsece_region_shares(), by = c('region', 'type')) %>%
    dplyr::mutate(model = dplyr::coalesce(model, 0))
  if (anyNA(d[, c('targets', 'nhes', 'nsece')])) stop('a share is missing -- check type names')

  dis <- function(a, b) sum(abs(a - b)) / 2
  e <- d %>%
    dplyr::group_by(geo, region) %>%
    dplyr::summarise(children_0_4  = dplyr::first(children_0_4),
                     targets_nhes  = dis(targets, nhes),
                     targets_nsece = dis(targets, nsece),
                     model_nhes    = dis(model, nhes),
                     model_nsece   = dis(model, nsece),
                     nhes_nsece    = dis(nhes, nsece),
                     .groups = 'drop')

  pct <- function(x) sprintf('%5.1f', 100 * x)
  cat('Total error (dissimilarity index, %) vs region benchmark, 52 geographies\n\n')
  st <- e %>% dplyr::filter(geo != 'NYC')   # NYC inside NY for weighted means
  wm <- function(x) sum(x * st$children_0_4) / sum(st$children_0_4)
  cat(sprintf('%-28s %8s %8s %8s %8s\n', '', 'mean', 'median', 'max', 'wtd'))
  for (v in c('targets_nhes', 'targets_nsece', 'model_nhes', 'model_nsece', 'nhes_nsece')) {
    cat(sprintf('%-28s %8s %8s %8s %8s  (%s)\n', v, pct(mean(e[[v]])), pct(stats::median(e[[v]])),
                pct(max(e[[v]])), pct(wm(st[[v]])), e$geo[which.max(e[[v]])]))
  }

  cat('\nSigned gap, targets - NHES, share of children 0-4 (pp), by type\n')
  g <- d %>% dplyr::mutate(gap = 100 * (targets - nhes)) %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(mean = mean(gap), min = min(gap), max = max(gap), .groups = 'drop')
  show_df(dplyr::mutate(g, dplyr::across(where(is.numeric), ~round(.x, 1))))

  readr::write_csv(d, file.path(STATE_DATA_ROOT, 'out', 'state_share_comparison.csv'))
  readr::write_csv(e, file.path(STATE_DATA_ROOT, 'out', 'state_share_errors.csv'))
  cat('\nwrote out/state_share_comparison.csv, out/state_share_errors.csv\n')
  invisible(e)
}


if (sys.nframe() == 0) main_state_shares()
