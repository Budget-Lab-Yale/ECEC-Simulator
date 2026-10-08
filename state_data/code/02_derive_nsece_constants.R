#------------------------------------------------------------------------------
# 02_derive_nsece_constants.R -- derives every NSECE constant
# 00_build_demand_targets.R holds.
#
# NSECE 2019 household file, classified as src/1_processing/1a_nsece_processing.R
# does, by Census region and nationally: formal share, center-home split,
# informal mix, weekly hours, each w/ unweighted record count. Full precision
# to out/nsece_constants.csv.
#
# TWO UNIVERSES, build uses A:
#   A  every child 0-4 w/ a regular non-parental arrangement -- NSECE's own
#      five-hour screener. Reproduces NSECE's published figures exactly
#      (validates code). SIMULATOR'S OWN UNIVERSE: 1a_nsece_processing.R keeps
#      every child w/ positive hours, adds no threshold of its own.
#   B  same children whose TOTAL weekly hours across regular non-parental
#      arrangements reach 10 -- NSCH k6q20 threshold. Kept since NSCH can only
#      measure B; `children_all_over_10plus` carries step 1 from B up to A.
# Restriction on child's total since NSCH asks about all non-parental care.
# School (K-8) excluded from both.
#
# DATA. NSECE public-use, ICPSR 37941, licensed, not in repo -- on cluster
# beside simulator's inputs. Reads 2019/37941-0009-Data.tsv (117 MB, several
# thousand columns) under --nsece <dir> or NSECE_ROOT. Submit w/ sbatch.
#
# Usage (repo root):
#   Rscript state_data/code/02_derive_nsece_constants.R --nsece <dir>
#     [--out <file>]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(stringr)
  library(tibble); library(readr)
})

# REGIONS (HH9_REGION codes), UNIVERSE_A / _B labels, cli_value(), show_df().
# Not 00_build_demand_targets.R -- too heavy for a cluster job on licensed data.
source('state_data/code/shared_setup.R')

# Center price split, $/hr -- 1a_nsece_processing.R's boundary.
PRICE_BOUNDARY <- 4.75

# Part-time below this many hrs/wk. src/shared_functions/constants.R sets
# pt = 10, ft = 40; 1a_nsece_processing.R cuts between them at `hours < 20`.
PT_FT_CUT <- 20

# Primary-arrangement ranking, as simulator: paid before unpaid, center before
# home. Hours summed within type first, so each type one row -- rank alone
# picks, desc(hours) in arrange() never breaks a tie.
CATEGORY_PRIORITY <- c('K-8', 'High-Priced Center-Based',
                       'Low-Priced Center-Based', 'Unpaid Center-Based',
                       'Paid Home-Based', 'Other Paid', 'Unpaid Home-Based',
                       'Other Unpaid')

PAID_CENTRE <- c('High-Priced Center-Based', 'Low-Priced Center-Based')
FORMAL      <- c(PAID_CENTRE, 'Unpaid Center-Based', 'Paid Home-Based')
INFORMAL    <- c('Other Unpaid', 'Other Paid', 'Unpaid Home-Based')

# NSECE's published regional formal shares -- universe A must reproduce them.
PUBLISHED_FORMAL <- c(northeast = 0.5653, midwest = 0.5292,
                      south = 0.5316, west = 0.4835)

# Column patterns. Child x provider columns are HH9_<stem>_<child 1-9>_<provider
# 1-14>; hours normalised to HOURS_RE's spelling after read.
PROV        <- '(?:[1-9]|1[0-4])'
AGE_RE      <- '^HHC9_AGE_[1-9]$'
WEIGHT_RE   <- '^HHC9_METH_WEIGHT_[1-9]$'
PRIORREL_RE <- paste0('^HH9_PRIORREL_', PROV, '$')
CHILD_PROV_RE <- paste0('^', c('HH9_CHILDREN', 'HH9_REGCARE', 'HH9_TYPEOFCARE_AGG',
                               'HH9_CARELOC', 'HH9_WEEKLY_PAYMENT_TC'),
                        '_[1-9]_', PROV, '$')
HOURS_RE    <- paste0('^HH9_HRS_WEEK_PROV_[1-9]_', PROV, '$')



read_household <- function(root) {

  #----------------------------------------------------------------------------
  # Reads only columns this measurement needs -- keeps 117 MB file small
  # enough to hold twice.
  #
  # Params:
  #   - root (chr): NSECE root holding 2019/
  #
  # Returns: (data.table)
  #----------------------------------------------------------------------------

  f <- file.path(root, '2019', '37941-0009-Data.tsv')
  if (!file.exists(f)) stop('missing NSECE household file: ', f)

  hdr  <- names(fread(f, nrows = 0))
  cols <- function(p) unlist(lapply(p, grep, x = hdr, value = TRUE))

  # Hours arrive as PROV<provider>_<child> in some releases,
  # PROV_<child>_<provider> in others -- take both, normalise.
  d <- fread(f, select = unique(c('HH9_METH_CASEID', 'HH9_REGION',
    cols(c(AGE_RE, WEIGHT_RE, CHILD_PROV_RE, PRIORREL_RE,
           '^HH9_HRS_WEEK_PROV[0-9_]+$', '^HH9_HOURS_WEEK_PROV[0-9_]+$')))))
  for (stem in c('HH9_HRS_WEEK_PROV', 'HH9_HOURS_WEEK_PROV')) {
    p   <- paste0('^', stem, '([0-9]+)_([0-9]+)$')
    old <- grep(p, names(d), value = TRUE)
    setnames(d, old, str_replace(old, p, 'HH9_HRS_WEEK_PROV_\\2_\\1'))
  }
  d
}



build_children <- function(d) {

  #----------------------------------------------------------------------------
  # One row per child aged 0-4 w/ positive weight.
  #
  # Params:
  #   - d (data.table) from read_household()
  #
  # Returns: (tibble) hh_id, child_id, age, child_weight, region
  #----------------------------------------------------------------------------

  as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID, region = HH9_REGION,
           all_of(grep(AGE_RE, names(d), value = TRUE)),
           all_of(grep(WEIGHT_RE, names(d), value = TRUE))) %>%
    pivot_longer(c(-hh_id, -region), names_to = 'variable', values_to = 'value') %>%
    mutate(name     = if_else(str_detect(variable, 'WEIGHT'),
                              'child_weight', 'age_months'),
           child_id = as.integer(str_sub(variable, start = -1))) %>%
    select(-variable) %>%
    pivot_wider() %>%
    filter(!is.na(age_months), age_months >= 0) %>%
    mutate(age = floor(age_months / 12)) %>%
    filter(age <= 4, !is.na(child_weight), child_weight > 0) %>%
    mutate(region = REGIONS[as.character(region)]) %>%
    select(hh_id, child_id, age, child_weight, region)
}



build_arrangements <- function(d) {

  #----------------------------------------------------------------------------
  # One row per regular child x provider arrangement w/ positive hours, in
  # simulator's care types. Classification deliberately identical to
  # 1a_nsece_processing.R -- a different rule would measure a different thing,
  # universe difference would not be readable.
  #
  # Params:
  #   - d (data.table) from read_household()
  #
  # Returns: (tibble) hh_id, child_id, provider_id, ecec_choice, hours
  #----------------------------------------------------------------------------

  prior <- as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID,
           all_of(grep(PRIORREL_RE, names(d), value = TRUE))) %>%
    pivot_longer(-hh_id, names_to = 'provider_id',
                 names_pattern = 'HH9_PRIORREL_([0-9]+)$',
                 values_to = 'priorrel') %>%
    mutate(provider_id = as.integer(provider_id))

  as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID,
           all_of(unlist(lapply(c(CHILD_PROV_RE, HOURS_RE), grep, x = names(d),
                                value = TRUE)))) %>%
    pivot_longer(-hh_id, names_to = c('name', 'child_id', 'provider_id'),
                 names_pattern = '(HH9_[A-Z0-9_]+)_([1-9])_([0-9]+)$',
                 values_to = 'value') %>%
    mutate(child_id = as.integer(child_id),
           provider_id = as.integer(provider_id)) %>%
    pivot_wider() %>%
    filter(HH9_CHILDREN == 1) %>%
    left_join(prior, by = c('hh_id', 'provider_id')) %>%
    transmute(hh_id, child_id, provider_id,
              regular  = HH9_REGCARE,
              type_raw = HH9_TYPEOFCARE_AGG,
              careloc  = HH9_CARELOC,
              priorrel,
              hours    = HH9_HRS_WEEK_PROV,
              cost     = HH9_WEEKLY_PAYMENT_TC) %>%
    mutate(
      hours = if_else(!is.na(hours) & hours >= 0, hours, NA_real_),
      cost  = if_else(!is.na(cost)  & cost  >= 0, cost,  NA_real_),
      is_paid      = !is.na(cost) & cost > 0,
      hourly       = if_else(!is.na(hours) & hours > 0, cost / hours, NA_real_),
      is_k8        = type_raw == 6,
      is_center    = type_raw == 4,
      is_home      = type_raw %in% c(1, 2, 3),
      in_child_home = !is.na(careloc) & careloc == 1,
      has_prior_rel = !is.na(priorrel) & priorrel == 1,
      is_home_based = is_home & !in_child_home & !has_prior_rel,
      is_other_kind = (!is_center & !is_k8) &
                        (!is_home | in_child_home | has_prior_rel),
      is_low  = is_center & is_paid & !is.na(hourly) & hourly <  PRICE_BOUNDARY,
      is_high = is_center & is_paid & !is.na(hourly) & hourly >= PRICE_BOUNDARY,
      ecec_choice = case_when(
        is_k8                              ~ 'K-8',
        is_center & !is_paid               ~ 'Unpaid Center-Based',
        is_center & is_paid & is_low       ~ 'Low-Priced Center-Based',
        is_center & is_paid & is_high      ~ 'High-Priced Center-Based',
        is_center & is_paid                ~ NA_character_,
        is_home_based & !is_paid           ~ 'Unpaid Home-Based',
        is_home_based &  is_paid           ~ 'Paid Home-Based',
        is_other_kind & !is_paid           ~ 'Other Unpaid',
        is_other_kind &  is_paid           ~ 'Other Paid',
        TRUE                               ~ NA_character_)) %>%
    filter(!is.na(regular), regular == 1, !is.na(ecec_choice),
           !is.na(hours), hours > 0) %>%
    select(hh_id, child_id, provider_id, ecec_choice, hours)
}



primary_arrangement <- function(arr) {

  #----------------------------------------------------------------------------
  # One row per child: primary care type, its hours, child's TOTAL
  # non-parental hours. K-8 dropped first -- NSCH's question names centers,
  # preschool, Head Start, family child care, nannies, babysitters, and
  # relatives, not school; counting school would put every school-age child
  # over ten-hour line for wrong reason.
  #
  # Params:
  #   - arr (tibble) from build_arrangements()
  #
  # Returns: (tibble) hh_id, child_id, ecec_choice, hours, total_hours
  #----------------------------------------------------------------------------

  arr %>%
    filter(ecec_choice != 'K-8') %>%
    group_by(hh_id, child_id, ecec_choice) %>%
    summarise(hours = sum(hours), .groups = 'drop') %>%
    group_by(hh_id, child_id) %>%
    mutate(total_hours = sum(hours)) %>%
    arrange(match(ecec_choice, CATEGORY_PRIORITY), desc(hours),
            .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
}



summarise_universe <- function(kids, label, all_children = NULL) {

  #----------------------------------------------------------------------------
  # NSECE constants on universe `kids`.
  #
  # Params:
  #   - kids (tibble): primary-arrangement rows w/ child_weight, region
  #   - label (chr): universe label
  #   - all_children (tibble|NULL): every child 0-4, in care or not --
  #       price-band shares' denominator, as 00_build_demand_targets.R
  #       states them
  #
  # Returns: (list of tibbles) formal, mix, hours, bands (NULL w/out
  #   all_children), split, mix_r, hours_r, unpaid, ptft, ptft_nat
  #----------------------------------------------------------------------------

  w <- function(x, wt) sum(x * wt) / sum(wt)
  # Paid centers pooled into one Center-Based row -- no price-band blend.
  pooled <- kids %>%
    mutate(ecec_choice = if_else(ecec_choice %in% PAID_CENTRE,
                                 'Center-Based', ecec_choice))
  in_region <- filter(kids, !is.na(region))

  formal <- in_region %>%
    group_by(region) %>%
    summarise(universe = label, children = n(),
              formal_share = w(ecec_choice %in% FORMAL, child_weight),
              .groups = 'drop')

  mix <- kids %>%
    filter(ecec_choice %in% INFORMAL) %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n(), wt = sum(child_weight),
              .groups = 'drop') %>%
    mutate(share = wt / sum(wt)) %>%
    select(universe, ecec_choice, children, share)

  hrs <- kids %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n(),
              weekly = w(hours, child_weight), .groups = 'drop') %>%
    mutate(annual = weekly * 50)

  # Center price bands. Only their RATIO enters blended Center-Based hours (the
  # denominator cancels), but 00_build_demand_targets.R, memo print the shares
  # themselves.
  bands <- NULL
  if (!is.null(all_children)) {
    denom <- sum(all_children$child_weight)
    bands <- kids %>%
      filter(ecec_choice %in% PAID_CENTRE) %>%
      group_by(ecec_choice) %>%
      summarise(universe = label, children = n(),
                share = sum(child_weight) / denom, .groups = 'drop')
  }

  # Step 4 cross-check only -- step 4 uses program counts. Weighted Unpaid
  # Center-Based over weighted formal, by region.
  unpaid <- in_region %>%
    filter(ecec_choice %in% FORMAL) %>%
    group_by(region) %>%
    summarise(universe = label, children = n(),
              n_unpaid = sum(ecec_choice == 'Unpaid Center-Based'),
              unpaid_share = w(ecec_choice == 'Unpaid Center-Based', child_weight),
              .groups = 'drop')

  # Step 5: paid centers over paid centers + Paid Home-Based, by region.
  # Unpaid Center-Based excluded -- own share, step 4.
  split <- in_region %>%
    filter(ecec_choice %in% c(PAID_CENTRE, 'Paid Home-Based')) %>%
    group_by(region) %>%
    summarise(universe = label,
              n_centre = sum(ecec_choice %in% PAID_CENTRE),
              n_home   = sum(ecec_choice == 'Paid Home-Based'),
              centre_share = w(ecec_choice %in% PAID_CENTRE, child_weight),
              .groups = 'drop')

  # Part-time share on primary arrangement's hours -- simulator's own cut, so
  # build, simulator agree child for child.
  ptft_nat <- pooled %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n(),
              pt_share = w(hours < PT_FT_CUT, child_weight), .groups = 'drop')

  ptft <- pooled %>%
    filter(!is.na(region)) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n(),
              n_pt = sum(hours < PT_FT_CUT),
              pt_share = w(hours < PT_FT_CUT, child_weight), .groups = 'drop')

  mix_r <- in_region %>%
    filter(ecec_choice %in% INFORMAL) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n(), wt = sum(child_weight),
              .groups = 'drop') %>%
    group_by(region) %>%
    mutate(share = wt / sum(wt)) %>%
    ungroup() %>%
    select(universe, region, ecec_choice, children, share)

  hrs_r <- pooled %>%
    filter(!is.na(region)) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n(),
              weekly = w(hours, child_weight), .groups = 'drop')

  list(formal = formal, mix = mix, hours = hrs, bands = bands, split = split,
       mix_r = mix_r, hours_r = hrs_r, unpaid = unpaid, ptft = ptft,
       ptft_nat = ptft_nat)
}



pooled_national <- function(kids, label) {

  #----------------------------------------------------------------------------
  # Pooled national formal share, center-home split, unpaid center share.
  # Reference rows only -- build reads regional cells.
  #
  # Params:
  #   - kids (tibble): one universe's primary-arrangement rows
  #   - label (chr): universe label
  #
  # Returns: (tibble) universe, quantity, key, value, n -- three rows
  #----------------------------------------------------------------------------

  paid_formal <- filter(kids, ecec_choice %in% c(PAID_CENTRE, 'Paid Home-Based'))
  formal_kids <- filter(kids, ecec_choice %in% FORMAL)
  tibble(
    universe = label,
    quantity = c('formal_share', 'centre_home_split', 'unpaid_centre_share'),
    key = 'national',
    value = c(sum(kids$child_weight[kids$ecec_choice %in% FORMAL]) /
                sum(kids$child_weight),
              sum(paid_formal$child_weight[paid_formal$ecec_choice %in% PAID_CENTRE]) /
                sum(paid_formal$child_weight),
              sum(formal_kids$child_weight[formal_kids$ecec_choice ==
                     'Unpaid Center-Based']) / sum(formal_kids$child_weight)),
    n = c(nrow(kids), nrow(paid_formal), nrow(formal_kids)))
}



as_long <- function(t, quantity, key, value, n = children) {

  #----------------------------------------------------------------------------
  # One summarise_universe() table as CSV rows.
  #
  # Params:
  #   - t (tibble), quantity (chr)
  #   - key, value, n: column expressions of t
  #
  # Returns: (tibble) universe, quantity, key, value, n
  #----------------------------------------------------------------------------

  transmute(t, universe, quantity = !!quantity, key = {{ key }},
            value = {{ value }}, n = {{ n }})
}



any_over_primary <- function(arr, kids, prim) {

  #----------------------------------------------------------------------------
  # Children 0-4 w/ ANY regular arrangement in a setting over children w/ it as
  # PRIMARY (CATEGORY_PRIORITY), weighted -- 04_compare_licensing_registers.R
  # puts build's primary-type counts on a register's every-enrolled-child
  # basis. Settings: centre (three center types), Paid Home-Based.
  #
  # Params:
  #   - arr (tibble): build_arrangements() rows
  #   - kids (tibble): build_children() rows (child_weight)
  #   - prim (tibble): primary_arrangement() rows joined to kids
  #
  # Returns: (tibble) universe, quantity, key, value, n
  #----------------------------------------------------------------------------

  sets <- list(centre = c(PAID_CENTRE, 'Unpaid Center-Based'),
               'Paid Home-Based' = 'Paid Home-Based')
  bind_rows(lapply(names(sets), function(k) {
    any_ids <- arr %>% filter(ecec_choice %in% sets[[k]]) %>% distinct(hh_id, child_id) %>%
      inner_join(kids, by = c('hh_id', 'child_id'))
    p <- prim %>% filter(ecec_choice %in% sets[[k]])
    tibble(universe = UNIVERSE_A, quantity = 'any_over_primary', key = k,
           value = sum(any_ids$child_weight) / sum(p$child_weight), n = nrow(any_ids))
  }))
}



share_of_children_region <- function(arr, kids, prim) {

  #----------------------------------------------------------------------------
  # Share of ALL children 0-4 by primary type, Parent Only included, by region
  # + national -- 06_compare_state_shares.R benchmark. Kindergarten children
  # (any K-8 arrangement) out of numerator and denominator, as NHES. Paid
  # center price bands pooled as build's Center-Based.
  #
  # Params:
  #   - arr (tibble): build_arrangements() rows
  #   - kids (tibble): build_children() rows (child_weight, region)
  #   - prim (tibble): primary_arrangement() rows joined to kids
  #
  # Returns: (tibble) universe, quantity, key ('<region>:<type>'), value, n
  #----------------------------------------------------------------------------

  k_ids <- arr %>% filter(ecec_choice == 'K-8') %>% distinct(hh_id, child_id)
  all_k <- kids %>% anti_join(k_ids, by = c('hh_id', 'child_id'))
  typ <- prim %>%
    anti_join(k_ids, by = c('hh_id', 'child_id')) %>%
    transmute(hh_id, child_id,
              type = if_else(ecec_choice %in% PAID_CENTRE, 'Center-Based', ecec_choice))
  d <- all_k %>%
    left_join(typ, by = c('hh_id', 'child_id')) %>%
    mutate(type = coalesce(type, 'Parent Only'))
  d <- bind_rows(d, mutate(d, region = 'US')) %>% filter(!is.na(region))
  d %>%
    group_by(region) %>%
    mutate(tot = sum(child_weight)) %>%
    group_by(region, type) %>%
    summarise(value = sum(child_weight) / first(tot), n = n(), .groups = 'drop') %>%
    transmute(universe = UNIVERSE_A, quantity = 'share_of_children_region',
              key = paste(region, type, sep = ':'), value, n)
}



short_share_by <- function(prim, by, quantity) {

  #----------------------------------------------------------------------------
  # Short share of part-time (children under 10 total hrs/week over children
  # w/ primary hours < PT_FT_CUT), weighted, one row per group of `by`.
  #
  # Params:
  #   - prim (tibble): universe A primary-arrangement rows
  #   - by (chr): grouping column, 'region' | 'ecec_choice'
  #   - quantity (chr): quantity label
  #
  # Returns: (tibble) universe, quantity, key, value, n
  #----------------------------------------------------------------------------

  prim %>%
    filter(hours < PT_FT_CUT, !is.na(.data[[by]])) %>%
    group_by(key = .data[[by]]) %>%
    summarise(value = sum(child_weight[total_hours < 10]) / sum(child_weight),
              n = n(), .groups = 'drop') %>%
    transmute(universe = UNIVERSE_A, quantity = quantity, key, value, n)
}



show_table <- function(title, x, digits) {

  #----------------------------------------------------------------------------
  # Prints '== title', then x w/ numeric columns rounded.
  #
  # Params:
  #   - title (chr), x (tibble), digits (int)
  #
  # Returns: (NULL) invisibly
  #----------------------------------------------------------------------------

  cat('\n== ', title, '\n', sep = '')
  show_df(mutate(x, across(where(is.numeric), ~round(.x, digits))))
}



main <- function() {

  #----------------------------------------------------------------------------
  # Builds both universes, prints every constant 00_build_demand_targets.R
  # carries, writes long CSV of quantities by universe.
  #
  # Params: none -- CLI --nsece <dir> (else env NSECE_ROOT, else stop),
  #   --out <file> (default nsece_constants.csv)
  #
  # Returns: (tibble) written quantities, invisibly
  #----------------------------------------------------------------------------

  root <- cli_value('--nsece', Sys.getenv('NSECE_ROOT'))
  if (!nzchar(root)) stop('pass --nsece <dir> or set NSECE_ROOT')
  out <- cli_value('--out', 'nsece_constants.csv')

  cat('reading ', root, '\n', sep = '')
  d <- read_household(root)
  cat('  households ', nrow(d), '\n', sep = '')
  kids <- build_children(d)
  cat('  children aged 0-4 ', nrow(kids), '\n', sep = '')
  arr <- build_arrangements(d)
  cat('  child x provider arrangements ', nrow(arr), '\n', sep = '')
  prim <- primary_arrangement(arr) %>% inner_join(kids, by = c('hh_id', 'child_id'))
  cat('  children 0-4 in non-parental care ', nrow(prim), '\n', sep = '')

  keep   <- prim$total_hours >= 10
  b_kids <- prim[keep, ]
  a <- summarise_universe(prim,   UNIVERSE_A, kids)
  b <- summarise_universe(b_kids, UNIVERSE_B, kids)

  # Targets are in HOURS, so report share of hours below ten-hour line too.
  hA <- sum(prim$child_weight * prim$hours)
  hB <- sum(prim$child_weight[keep] * prim$hours[keep])
  cat('\n== children reaching the ten-hour line\n',
      sprintf('  universe A %d children, universe B %d (%.1f%% kept)\n',
              nrow(prim), sum(keep), 100 * mean(keep)),
      sprintf('  weighted primary-arrangement hours: A %.0f, B %.0f (%.1f%% kept)\n',
              hA, hB, 100 * hB / hA),
      sprintf('  sub-ten-hour group: %.1f%% of children, %.1f%% of HOURS\n',
              100 * (1 - mean(keep)), 100 * (1 - hB / hA)), sep = '')

  # A's table beside B's, joined on `by`; a_cols / b_cols pick + rename.
  ab <- function(part, by, a_cols, b_cols) {
    left_join(select(a[[part]], all_of(c(by, a_cols))),
              select(b[[part]], all_of(c(by, b_cols))), by = by)
  }

  show_table('REGIONAL FORMAL SHARE (step 2)',
             ab('formal', 'region', c(n_A = 'children', A = 'formal_share'),
                c(n_B = 'children', B = 'formal_share')) %>%
               mutate(published = PUBLISHED_FORMAL[region],
                      `A vs published` = A - published, `B - A` = B - A),
             4)
  show_table('INFORMAL MIX -- divides the residual',
             ab('mix', 'ecec_choice', c(A = 'share'), c(B = 'share')), 4)
  show_table('MEAN WEEKLY HOURS IN THE PRIMARY ARRANGEMENT',
             ab('hours', 'ecec_choice', c(n_A = 'children', weekly_A = 'weekly'),
                c(n_B = 'children', weekly_B = 'weekly')) %>%
               mutate(annual_A = weekly_A * 50, annual_B = weekly_B * 50,
                      pct = 100 * (weekly_B / weekly_A - 1)),
             2)

  bands <- ab('bands', 'ecec_choice', c(n_A = 'children', A = 'share'),
              c(n_B = 'children', B = 'share'))
  show_table(paste0('CENTRE PRICE-BAND SHARES -- weights for the blended centre hours\n',
                    '   (shares of ALL children 0-4; NSECE publishes 0.07008 high, 0.06556 low)'),
             bands, 5)
  # Price-band-share-weighted annual hours of the two paid-center bands.
  blend <- function(col) {
    u <- (if (col == 'B') b else a)$hours
    h <- setNames(u$weekly * 50, u$ecec_choice)
    s <- setNames(bands[[col]], bands$ecec_choice)
    (s[['High-Priced Center-Based']] * h[['High-Priced Center-Based']] +
     s[['Low-Priced Center-Based']]  * h[['Low-Priced Center-Based']]) /
      (s[['High-Priced Center-Based']] + s[['Low-Priced Center-Based']])
  }
  cat(sprintf('   blended Center-Based annual hours: A %.1f   B %.1f  (%+.1f%%)\n',
              blend('A'), blend('B'), 100 * (blend('B') / blend('A') - 1)))

  show_table('CENTRE-HOME SPLIT OF PAID FORMAL CARE, by region (step 5, universe A)',
             a$split, 4)
  show_table(paste0('PART-TIME SHARE (hours < ', PT_FT_CUT, '), by region and type'),
             ab('ptft', c('region', 'ecec_choice'),
                c(n_A = 'children', pt_A = 'n_pt', A = 'pt_share'),
                c(n_B = 'children', B = 'pt_share')),
             4)
  show_table('UNPAID CENTER-BASED SHARE OF FORMAL CARE, by region (step 4)',
             ab('unpaid', 'region',
                c(n_A = 'children', unpaid_A = 'n_unpaid', A = 'unpaid_share'),
                c(n_B = 'children', unpaid_B = 'n_unpaid', B = 'unpaid_share')),
             4)
  show_table('INFORMAL MIX, by region (universe A)', a$mix_r, 4)
  show_table('WEEKLY HOURS, by region (universe A)', a$hours_r, 2)

  # One quantity's rows, universe A then B.
  both <- function(part, quantity, key, value, n = children) {
    bind_rows(as_long(a[[part]], quantity, {{ key }}, {{ value }}, {{ n }}),
              as_long(b[[part]], quantity, {{ key }}, {{ value }}, {{ n }}))
  }

  res <- bind_rows(
    pooled_national(prim,   UNIVERSE_A),
    pooled_national(b_kids, UNIVERSE_B),
    # UNIVERSE RATIO. NSCH k6q20 counts children at 10+ hrs/week; simulator's
    # baseline holds every regular arrangement on NSECE's five-hour screener.
    # Step 1 scales NSCH control total by this to target simulator's population.
    tibble(universe = UNIVERSE_A, quantity = 'children_all_over_10plus',
           key = 'national',
           value = sum(prim$child_weight) / sum(b_kids$child_weight),
           n = nrow(prim)),
    # SHORT SHARE OF PART-TIME. Children under 10 total hrs/week over
    # children part-time in simulator's sense (primary hours < PT_FT_CUT).
    # Every under-10 child part-time -- primary hours <= total hours. Step 1
    # applies it to simulator's predicted part-time children per geography.
    tibble(universe = UNIVERSE_A, quantity = 'short_share_of_part_time',
           key = 'national',
           value = sum(prim$child_weight[!keep]) /
                   sum(prim$child_weight[prim$hours < PT_FT_CUT]),
           n = sum(prim$hours < PT_FT_CUT)),
    # Same share by region, by primary care type -- 05_compare_nsece_shares.R
    # tests whether either split moves step 1; build keeps national.
    short_share_by(prim, 'region', 'short_share_of_part_time_region'),
    short_share_by(prim, 'ecec_choice', 'short_share_of_part_time_type'),
    any_over_primary(arr, kids, prim),
    share_of_children_region(arr, kids, prim),
    both('ptft_nat', 'part_time_share', ecec_choice, pt_share),
    both('ptft', 'part_time_share_region', paste(region, ecec_choice, sep = ':'),
         pt_share),
    # Regional mix, hours: A both, then B both.
    lapply(list(a, b), function(u) bind_rows(
      as_long(u$mix_r,   'informal_mix_region', paste(region, ecec_choice, sep = ':'), share),
      as_long(u$hours_r, 'weekly_hours_region', paste(region, ecec_choice, sep = ':'), weekly))),
    both('split',  'centre_home_split',   region, centre_share, n_centre + n_home),
    both('unpaid', 'unpaid_centre_share', region, unpaid_share),
    both('bands',  'price_band_share',    ecec_choice, share),
    both('formal', 'formal_share',        region, formal_share),
    both('mix',    'informal_mix',        ecec_choice, share),
    both('hours',  'weekly_hours',        ecec_choice, weekly))

  write_csv(res, out)
  cat('\nwrote ', out, '\n', sep = '')
  invisible(res)
}



if (sys.nframe() == 0) main()
