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
#      five-hour screener. Reproduced NSECE's published regional formal
#      shares exactly while unknown cost counted unpaid (validated code);
#      since 2026-10-12 children whose label unknown payment changes take
#      imputed paid status (impute_unknown_payment(), as 1a) -- midwest +2.1
#      points, other regions within 0.8 points; paid status from HH9_PAID
#      where known, payment -2 (no Section J) unknown, not unpaid (same
#      day). Since 2026-10-12 regular
#      care per resolve_regular_care() (as 1a): regular provider w/ zero /
#      missing reference-week hours counts w/ imputed hours (drawn from
#      cell's weighted donor hours, keyed); REGCARE -1 / -2
#      regular when TOC 1-5. Universe 2,814 -> 3,462 children; added
#      children mostly informal, so formal shares now 4-10 points under
#      published (published shares rest on reference-week care). TOC 7 /
#      8 never regular (same day): NSECE's regular care = TOC 1-5 only --
#      universe 3,462 -> 3,427, in-care share of weighted children 0-4
#      55.55% -> 54.98%, NSECE's own TOC 1-5 rate.
#      SIMULATOR'S OWN UNIVERSE: 1a_nsece_processing.R counts same
#      arrangements, adds no threshold of its own.
#   B  same children whose TOTAL weekly hours across regular non-parental
#      arrangements reach 10 -- NSCH k6q20 threshold. Kept since NSCH can only
#      measure B; `children_all_over_10plus` carries step 1 from B up to A.
# Restriction on child's total since NSCH asks about all non-parental care.
# School (K-8) excluded from both.
#
# LABEL DRAWS (2026-10-12, as 1a): paid status, hours draws run
# NSECE_MI_DRAWS times (nsece_mi_draws.R), every child one row per draw w/
# child_weight / M -- every constant = pooled average over draws. Record
# counts (n) = mean records per draw, may be fractional. One draw before:
# seed choice moved universe ratio 1.094-1.105 over 20 seed pairs.
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

# Unknown-payment imputation -- same rule, same draws as 1a
# (assign_primary_care(), impute_unknown_payment()); with_rng_preserved()
source('src/shared_functions/impute_unknown_payment.R')
# Urban-density switch (ECEC_URBAN_DENSITY, require_urban_density_on())
source('src/shared_functions/urban_density.R')
# Regular care, weekly hours -- same rule as 1a (REGCARE -1 / -2 by TOC,
# zero-hour regular rows drawn hours, keyed hash from price wedge file) and
# provider care type, classify_nsece_provider() -- same rules as 1a
source('src/shared_functions/compute_price_wedge_cache.R')
source('src/shared_functions/resolve_regular_care.R')
# Label draws, seeds per draw -- same M, seeds as 1a
source('src/shared_functions/nsece_mi_draws.R')

# Part-time below PT_FT_CUT_HOURS hrs/wk -- simulator's cut (constants.R, as 1a)
source('src/shared_functions/constants.R')

# Primary-arrangement ranking, as simulator: paid before unpaid, center before
# home. Hours summed within type first, so each type one row -- rank alone
# picks, desc(hours) in arrange() never breaks a tie. Paid centres one type --
# build has one Center-Based row, price tiers (1a) don't enter its constants.
CATEGORY_PRIORITY <- ECEC_PRIMARY_PRIORITY_ONE_CENTRE   # constants.R

PAID_CENTRE <- 'Center-Based'
FORMAL      <- FORMAL_TYPES   # shared_setup.R
INFORMAL    <- c('Other Unpaid', 'Other Paid', 'Unpaid Home-Based')

# NSECE's published regional formal shares -- universe A printed beside them
# (exact match w/ unknown cost unpaid, reference-week hours only;
# unknown-payment rule, 2026-10-12 regular-care rule move it).
PUBLISHED_FORMAL <- c(northeast = 0.5653, midwest = 0.5292,
                      south = 0.5316, west = 0.4835)

# Column patterns. Child x provider columns are HH9_<stem>_<child 1-9>_<provider
# 1-14>; hours normalised to HOURS_RE's spelling after read.
PROV        <- '(?:[1-9]|1[0-4])'
AGE_RE      <- '^HHC9_AGE_[1-9]$'
WEIGHT_RE   <- '^HHC9_METH_WEIGHT_[1-9]$'
PRIORREL_RE <- paste0('^HH9_PRIORREL_', PROV, '$')
CHILD_PROV_RE <- paste0('^', c('HH9_CHILDREN', 'HH9_REGCARE', 'HH9_TYPEOFCARE_AGG',
                               'HH9_CARELOC', 'HH9_WEEKLY_PAYMENT_TC', 'HH9_PAID'),
                        '_[1-9]_', PROV, '$')
HOURS_RE    <- paste0('^HH9_HRS_WEEK_PROV_[1-9]_', PROV, '$')

# Record count of child x draw rows: sum of record shares (rec = 1 / M) =
# mean records per draw, rounded to 6 decimals -- drops float noise of
# summed 1 / M (3427.000000000001)
n_rec <- function(rec) round(sum(rec), 6)



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
  d <- fread(f, select = unique(c('HH9_METH_CASEID', 'HH9_REGION', 'HH9_COMM_URBAN_DENSITY',
    cols(c(AGE_RE, WEIGHT_RE, CHILD_PROV_RE, PRIORREL_RE,
           '^HH9_HRS_WEEK_PROV[0-9_]+$', '^HH9_HOURS_WEEK_PROV[0-9_]+$')))))
  for (stem in c('HH9_HRS_WEEK_PROV', 'HH9_HOURS_WEEK_PROV')) {
    p   <- paste0('^', stem, '([0-9]+)_([0-9]+)$')
    old <- grep(p, names(d), value = TRUE)
    setnames(d, old, str_replace(old, p, 'HH9_HRS_WEEK_PROV_\\2_\\1'))
  }
  d
}



build_children_all <- function(d) {

  #----------------------------------------------------------------------------
  # One row per child of every age (age in months >= 0), any weight -- same
  # children as 1a. Unknown-payment imputation cells, draw universe span
  # every age (as 1a), so draws match 1a child for child.
  #
  # Params:
  #   - d (data.table) from read_household()
  #
  # Returns: (tibble) hh_id, child_id, age, child_weight, region, density
  #   (HH9_COMM_URBAN_DENSITY 1-3 -- tier donor cells, as 1a)
  #----------------------------------------------------------------------------

  as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID, region = HH9_REGION, density = HH9_COMM_URBAN_DENSITY,
           all_of(grep(AGE_RE, names(d), value = TRUE)),
           all_of(grep(WEIGHT_RE, names(d), value = TRUE))) %>%
    pivot_longer(c(-hh_id, -region, -density), names_to = 'variable', values_to = 'value') %>%
    mutate(name     = if_else(str_detect(variable, 'WEIGHT'),
                              'child_weight', 'age_months'),
           child_id = as.integer(str_sub(variable, start = -1))) %>%
    select(-variable) %>%
    pivot_wider() %>%
    filter(!is.na(age_months), age_months >= 0) %>%
    mutate(age = floor(age_months / 12)) %>%
    mutate(region = REGIONS[as.character(region)]) %>%
    select(hh_id, child_id, age, child_weight, region, density)
}



build_arrangements <- function(d, kids_all, draws = NSECE_MI_DRAWS) {

  #----------------------------------------------------------------------------
  # One row per regular child x provider arrangement w/ positive hours, in
  # simulator's care types, per label draw. Classification deliberately
  # identical to 1a_nsece_processing.R -- a different rule would measure a
  # different thing, universe difference would not be readable. Regularity,
  # hours from resolve_regular_care() (as 1a): REGCARE -1 / -2 regular when
  # TOC 1-5; TOC 7 / 8 never regular (NSECE's regular care TOC 1-5 only);
  # regular row w/ zero / missing reference-week hours takes imputed hours,
  # draw m seed nsece_mi_seed(REGULAR_HOURS_SEED, m) (as 1a).
  #
  # Params:
  #   - d (data.table) from read_household()
  #   - kids_all (tibble): build_children_all() rows (age, child_weight,
  #       region -- hours imputation cells, donor weights)
  #   - draws (int): label draws (M)
  #
  # Returns: (tibble) hh_id, child_id, provider_id, ecec_choice,
  #   unknown_kind, kind (imputation cell, every row), paid (lgl; NA =
  #   unknown payment), paid_q (lgl: HH9_PAID 1 / 0, NA unknown), hours,
  #   hours_imputed (lgl), regular_by_type (lgl), draw (int, 0 .. M - 1) --
  #   same rows every draw, hours of imputed rows differ
  #----------------------------------------------------------------------------

  prior <- as_tibble(d) %>%
    select(hh_id = HH9_METH_CASEID,
           all_of(grep(PRIORREL_RE, names(d), value = TRUE))) %>%
    pivot_longer(-hh_id, names_to = 'provider_id',
                 names_pattern = 'HH9_PRIORREL_([0-9]+)$',
                 values_to = 'priorrel') %>%
    mutate(provider_id = as.integer(provider_id))

  base <- as_tibble(d) %>%
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
              regcare  = HH9_REGCARE,
              type_raw = HH9_TYPEOFCARE_AGG,
              careloc  = HH9_CARELOC,
              priorrel,
              hours    = HH9_HRS_WEEK_PROV,
              cost     = HH9_WEEKLY_PAYMENT_TC,
              # Questionnaire paid status, as 1a paid_hh -> paid_q
              paid_q   = case_when(HH9_PAID %in% 1 ~ TRUE, HH9_PAID %in% 0 ~ FALSE)) %>%
    # region: hours-draw donor cells (region x TOC group x age), as 1a
    left_join(select(kids_all, hh_id, child_id, age, child_weight, region),
              by = c('hh_id', 'child_id'))

  bind_rows(lapply(seq_len(draws) - 1L, function(m) classify_arrangements(
    select(resolve_regular_care(base, seed = nsece_mi_seed(REGULAR_HOURS_SEED, m))$arr, -region)) %>%
      mutate(draw = m)))
}



classify_arrangements <- function(rc) {

  #----------------------------------------------------------------------------
  # Care type per arrangement, one draw -- 1a's classification.
  #
  # Params:
  #   - rc (tibble): resolve_regular_care()$arr rows
  #
  # Returns: (tibble) build_arrangements() columns, w/out draw
  #----------------------------------------------------------------------------

  rc %>%
    mutate(
      # Cost codes as 1a status handling: -1 DK/refused, -2 no Section J
      # (MISS) -> unknown payment, NA; other negative codes, blank (NIU) ->
      # 0. 2026-10-12; every missing cost unpaid before, -2 unpaid until
      # later same day
      cost  = case_when(!is.na(cost) & cost >= 0  ~ cost,
                        !is.na(cost) & cost >= -2 ~ NA_real_,
                        TRUE                      ~ 0),
      pay_unknown  = is.na(cost),
      is_paid      = !is.na(cost) & cost > 0,
      hourly       = if_else(!is.na(hours) & hours > 0, cost / hours, NA_real_),
      is_k8        = type_raw == 6,
      is_center    = type_raw == 4,
      is_home      = type_raw %in% c(1, 2, 3),
      in_child_home = !is.na(careloc) & careloc == 1,
      has_prior_rel = !is.na(priorrel) & priorrel == 1) %>%
    # Home-based / other, unknown_kind (candidate labels,
    # primary_arrangement()), label -- 1a's rule (resolve_regular_care.R)
    classify_nsece_provider(paid_centre_label = PAID_CENTRE) %>%
    mutate(
      kind = pay_kind(is_k8, is_center, is_home_based, is_other_kind),
      paid = if_else(pay_unknown, NA, is_paid)) %>%
    filter(regular == 1,
           !is.na(ecec_choice) | !is.na(unknown_kind),
           !is.na(hours), hours > 0) %>%
    select(hh_id, child_id, provider_id, ecec_choice, unknown_kind, kind,
           paid, paid_q, hours, hours_imputed, regular_by_type)
}



add_imputed_payment <- function(arr, kids_all) {

  #----------------------------------------------------------------------------
  # Draws paid status for unknown-payment rows, impute_unknown_payment() --
  # same function, seeds, universe as 1a: draw m seed
  # nsece_mi_seed(IMPUTE_PAY_SEED, m). Tiers unused here (one paid-centre
  # type); tier uniforms still drawn, so paid draws match 1a's.
  #
  # Params:
  #   - arr (tibble): build_arrangements() rows (regular, hours > 0 only),
  #       w/ draw
  #   - kids_all (tibble): build_children_all() rows
  #
  # Returns: (list) arr (+ imputed_choice: drawn label, NA on known rows),
  #   imp (impute_unknown_payment() result, draw 0 -- cells same every
  #   draw), n_paid (int: rows drawn paid, per draw)
  #----------------------------------------------------------------------------

  kid_cols <- select(kids_all, hh_id, child_id, age, child_weight, region, density)
  per_draw <- lapply(split(arr, arr$draw), function(a) {
    m <- a$draw[1]
    imp <- a %>%
      left_join(kid_cols, by = c('hh_id', 'child_id')) %>%
      mutate(tier = NA_character_, counts = TRUE) %>%
      impute_unknown_payment(seed = nsece_mi_seed(IMPUTE_PAY_SEED, m))
    a <- a %>%
      left_join(select(imp$draws, hh_id, child_id, provider_id, imp_paid),
                by = c('hh_id', 'child_id', 'provider_id')) %>%
      mutate(imputed_choice = imputed_pay_label(unknown_kind, imp_paid, NA_character_,
                                                paid_centre = PAID_CENTRE)) %>%
      select(-imp_paid)
    list(arr = a, imp = imp)
  })
  list(arr = bind_rows(lapply(per_draw, `[[`, 'arr')), imp = per_draw[[1]]$imp,
       n_paid = vapply(per_draw, function(p) sum(p$imp$draws$imp_paid), numeric(1)))
}



classify_children <- function(d, draws = NSECE_MI_DRAWS) {

  #----------------------------------------------------------------------------
  # Children 0-4, arrangements, primary arrangement (unknown payment, hours
  # drawn) -- shared by main(), nsece_shares_by_age.R,
  # 13_participation_audit.R. Multiple imputation (nsece_mi_draws.R, as
  # 1a): every child `draws` rows, one per label draw, child_weight / M,
  # rec = 1 / M (record share) -- weighted results = pooled average over
  # draws, n_rec(rec) = mean record count per draw.
  #
  # Params:
  #   - d (data.table) from read_household()
  #   - draws (int): label draws (M); draw 0 = single draw before
  #
  # Returns: (list) kids (retained child x draw rows, children 0-4), prim
  #   (their primary arrangements, joined to kids), arr (arrangements w/
  #   imputed_choice, every draw), dropped (hh_id, child_id, draw still
  #   NA), n_kids, wt_kids (all children 0-4 before drop, once each), imp
  #   (impute_unknown_payment() result, draw 0), n_paid (rows drawn paid,
  #   per draw), draws
  #----------------------------------------------------------------------------

  kids_all <- build_children_all(d)
  kids <- filter(kids_all, age <= 4, !is.na(child_weight), child_weight > 0)
  ai <- add_imputed_payment(build_arrangements(d, kids_all, draws), kids_all)
  kids_d <- kids %>%
    cross_join(tibble(draw = seq_len(draws) - 1L)) %>%
    mutate(child_weight = child_weight / draws, rec = 1 / draws)
  prim <- primary_arrangement(ai$arr) %>%
    inner_join(kids_d, by = c('hh_id', 'child_id', 'draw'))
  kept <- drop_unresolved(kids_d, prim)
  list(kids = kept$kids, prim = kept$prim, arr = ai$arr, dropped = kept$dropped,
       n_kids = nrow(kids), wt_kids = sum(kids$child_weight), imp = ai$imp,
       n_paid = ai$n_paid, draws = draws)
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
  # Unknown payment (unknown_kind set), as 1a assign_primary_care(): unknown
  # rows take drawn labels (imputed_choice, add_imputed_payment()) -- child
  # whose type x hours band (PT_FT_CUT_HOURS) payment can't change gets same label
  # from any draw. No draw -> ecec_choice NA, drop_unresolved() takes it out
  # of numerators, denominators (2026-10-12; unknown cost unpaid before,
  # enumerated + drawn where ambiguous same day).
  #
  # One row per child x label draw (draw column; absent -> draw 0).
  #
  # Returns: (tibble) draw, hh_id, child_id, ecec_choice (NA = ambiguous, no
  #   draw), hours, total_hours, imputed (lgl)
  #----------------------------------------------------------------------------

  if (!'draw' %in% names(arr)) arr$draw <- 0L
  arr <- filter(arr, is.na(ecec_choice) | ecec_choice != 'K-8')
  has_unknown <- arr %>%
    group_by(draw, hh_id, child_id) %>%
    mutate(unk = any(!is.na(unknown_kind))) %>%
    ungroup() %>%
    pull(unk)

  known <- arr[!has_unknown, ] %>%
    group_by(draw, hh_id, child_id, ecec_choice) %>%
    summarise(hours = sum(hours), .groups = 'drop') %>%
    group_by(draw, hh_id, child_id) %>%
    mutate(total_hours = sum(hours)) %>%
    arrange(match(ecec_choice, CATEGORY_PRIORITY), desc(hours),
            .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(imputed = FALSE)

  unknown <- arr[has_unknown, ] %>%
    group_by(draw, hh_id, child_id) %>%
    group_modify(~ {
      p <- .x
      unk <- which(!is.na(p$unknown_kind))
      imp <- if ('imputed_choice' %in% names(p)) p$imputed_choice[unk] else NA_character_
      if (any(is.na(imp))) {
        return(tibble(ecec_choice = NA_character_, hours = NA_real_,
                      total_hours = sum(p$hours), imputed = FALSE))
      }
      # Unknown rows take drawn labels, as 1a
      labels <- p$ecec_choice
      labels[unk] <- imp
      h <- tapply(p$hours, labels, sum)
      top <- order(match(names(h), CATEGORY_PRIORITY), -h)[1]
      tibble(ecec_choice = names(h)[top], hours = h[[top]],
             total_hours = sum(p$hours), imputed = TRUE)
    }) %>%
    ungroup()

  bind_rows(known, unknown) %>% arrange(hh_id, child_id, draw)
}



drop_unresolved <- function(kids, prim) {

  #----------------------------------------------------------------------------
  # Takes children whose label unknown payment changes (primary_arrangement()
  # ecec_choice NA) out of both child table, primary-arrangement table -- out
  # of every numerator, denominator (Parent Only share incl.), as 1a.
  #
  # Within label draw: child x draw row out when that draw leaves label NA.
  #
  # Params:
  #   - kids (tibble): child x draw rows (classify_children())
  #   - prim (tibble): primary_arrangement() rows
  #
  # Returns: (list) kids, prim (retained child x draw rows), dropped (hh_id,
  #   child_id, draw)
  #----------------------------------------------------------------------------

  dropped <- prim %>% filter(is.na(ecec_choice)) %>% distinct(hh_id, child_id, draw)
  list(kids    = anti_join(kids, dropped, by = c('hh_id', 'child_id', 'draw')),
       prim    = filter(prim, !is.na(ecec_choice)),
       dropped = dropped)
}



summarise_universe <- function(kids, label) {

  #----------------------------------------------------------------------------
  # NSECE constants on universe `kids`. Child x draw rows, child_weight / M
  # -> shares pooled over draws; record counts n_rec(rec) = mean records per
  # draw (may be fractional).
  #
  # Params:
  #   - kids (tibble): primary-arrangement rows w/ child_weight, region, rec
  #   - label (chr): universe label
  #
  # Returns: (list of tibbles) formal, mix, hours, split, mix_r, hours_r,
  #   unpaid, ptft, ptft_nat
  #----------------------------------------------------------------------------

  w <- function(x, wt) sum(x * wt) / sum(wt)
  in_region <- filter(kids, !is.na(region))

  formal <- in_region %>%
    group_by(region) %>%
    summarise(universe = label, children = n_rec(rec),
              formal_share = w(ecec_choice %in% FORMAL, child_weight),
              .groups = 'drop')

  mix <- kids %>%
    filter(ecec_choice %in% INFORMAL) %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n_rec(rec), wt = sum(child_weight),
              .groups = 'drop') %>%
    mutate(share = wt / sum(wt)) %>%
    select(universe, ecec_choice, children, share)

  hrs <- kids %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n_rec(rec),
              weekly = w(hours, child_weight), .groups = 'drop') %>%
    mutate(annual = weekly * 50)

  # Step 4 cross-check only -- step 4 uses program counts. Weighted Unpaid
  # Center-Based over weighted formal, by region.
  unpaid <- in_region %>%
    filter(ecec_choice %in% FORMAL) %>%
    group_by(region) %>%
    summarise(universe = label, children = n_rec(rec),
              n_unpaid = n_rec(rec * (ecec_choice == 'Unpaid Center-Based')),
              unpaid_share = w(ecec_choice == 'Unpaid Center-Based', child_weight),
              .groups = 'drop')

  # Step 5: paid centers over paid centers + Paid Home-Based, by region.
  # Unpaid Center-Based excluded -- own share, step 4.
  split <- in_region %>%
    filter(ecec_choice %in% c(PAID_CENTRE, 'Paid Home-Based')) %>%
    group_by(region) %>%
    summarise(universe = label,
              n_centre = n_rec(rec * (ecec_choice %in% PAID_CENTRE)),
              n_home   = n_rec(rec * (ecec_choice == 'Paid Home-Based')),
              centre_share = w(ecec_choice %in% PAID_CENTRE, child_weight),
              .groups = 'drop')

  # Part-time share on primary arrangement's hours -- simulator's own cut, so
  # build, simulator agree child for child.
  ptft_nat <- kids %>%
    group_by(ecec_choice) %>%
    summarise(universe = label, children = n_rec(rec),
              pt_share = w(hours < PT_FT_CUT_HOURS, child_weight), .groups = 'drop')

  ptft <- kids %>%
    filter(!is.na(region)) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n_rec(rec),
              n_pt = n_rec(rec * (hours < PT_FT_CUT_HOURS)),
              pt_share = w(hours < PT_FT_CUT_HOURS, child_weight), .groups = 'drop')

  mix_r <- in_region %>%
    filter(ecec_choice %in% INFORMAL) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n_rec(rec), wt = sum(child_weight),
              .groups = 'drop') %>%
    group_by(region) %>%
    mutate(share = wt / sum(wt)) %>%
    ungroup() %>%
    select(universe, region, ecec_choice, children, share)

  hrs_r <- kids %>%
    filter(!is.na(region)) %>%
    group_by(region, ecec_choice) %>%
    summarise(universe = label, children = n_rec(rec),
              weekly = w(hours, child_weight), .groups = 'drop')

  list(formal = formal, mix = mix, hours = hrs, split = split,
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
    n = c(n_rec(kids$rec), n_rec(paid_formal$rec), n_rec(formal_kids$rec)))
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



share_of_children_region <- function(arr, kids, prim) {

  #----------------------------------------------------------------------------
  # Share of ALL children 0-4 by primary type, Parent Only included, by region
  # + national -- 06_compare_state_shares.R benchmark. Kindergarten children
  # (any K-8 arrangement) out of numerator and denominator, as NHES.
  #
  # Params:
  #   - arr (tibble): build_arrangements() rows
  #   - kids (tibble): classify_children() child x draw rows (child_weight
  #       / M, region, rec)
  #   - prim (tibble): primary_arrangement() rows joined to kids
  #
  # Returns: (tibble) universe, quantity, key ('<region>:<type>'), value, n
  #   (mean records per draw)
  #----------------------------------------------------------------------------

  k_ids <- arr %>% filter(ecec_choice == 'K-8') %>% distinct(hh_id, child_id)
  all_k <- kids %>% anti_join(k_ids, by = c('hh_id', 'child_id'))
  typ <- prim %>%
    anti_join(k_ids, by = c('hh_id', 'child_id')) %>%
    transmute(hh_id, child_id, draw, type = ecec_choice)
  d <- all_k %>%
    left_join(typ, by = c('hh_id', 'child_id', 'draw')) %>%
    mutate(type = coalesce(type, 'Parent Only'))
  d <- bind_rows(d, mutate(d, region = 'US')) %>% filter(!is.na(region))
  d %>%
    group_by(region) %>%
    mutate(tot = sum(child_weight)) %>%
    group_by(region, type) %>%
    summarise(value = sum(child_weight) / first(tot), n = n_rec(rec), .groups = 'drop') %>%
    transmute(universe = UNIVERSE_A, quantity = 'share_of_children_region',
              key = paste(region, type, sep = ':'), value, n)
}



short_share_by <- function(prim, by, quantity) {

  #----------------------------------------------------------------------------
  # Short share of part-time (children under 10 total hrs/week over children
  # w/ primary hours < PT_FT_CUT_HOURS), weighted, one row per group of `by`.
  #
  # Params:
  #   - prim (tibble): universe A primary-arrangement rows
  #   - by (chr): grouping column, 'region' | 'ecec_choice'
  #   - quantity (chr): quantity label
  #
  # Returns: (tibble) universe, quantity, key, value, n
  #----------------------------------------------------------------------------

  prim %>%
    filter(hours < PT_FT_CUT_HOURS, !is.na(.data[[by]])) %>%
    group_by(key = .data[[by]]) %>%
    summarise(value = sum(child_weight[total_hours < 10]) / sum(child_weight),
              n = n_rec(rec), .groups = 'drop') %>%
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

  # Reads NSECE community urban density (tier donor cells, as 1a): state setup
  # runs w/ urban density on (urban_density.R, state_data/README.md)
  require_urban_density_on('state_data/code/02_derive_nsece_constants.R')

  root <- cli_value('--nsece', Sys.getenv('NSECE_ROOT'))
  if (!nzchar(root)) stop('pass --nsece <dir> or set NSECE_ROOT')
  out <- cli_value('--out', 'nsece_constants.csv')

  cat('reading ', root, '\n', sep = '')
  d <- read_household(root)
  cat('  households ', nrow(d), '\n', sep = '')
  cc <- classify_children(d)
  cat('  children aged 0-4 ', cc$n_kids, '; label draws ', cc$draws,
      ' (counts below: mean records per draw)\n', sep = '')
  arr <- cc$arr
  arr0 <- filter(arr, draw == 0)
  cat('  child x provider arrangements ', nrow(arr0), '\n', sep = '')
  cat(sprintf('  of which regular by TOC (REGCARE -1/-2) %d, imputed hours %d (pt < %d hrs: %.1f)\n',
              sum(arr0$regular_by_type), sum(arr0$hours_imputed), PT_FT_CUT_HOURS,
              sum(arr$hours_imputed & arr$hours < PT_FT_CUT_HOURS) / cc$draws))
  show_table('UNKNOWN PAYMENT -- paid share of known-payment arrangements, kind x age cell',
             cc$imp$paid_cells, 4)
  cat(sprintf('  unknown-payment arrangements drawn: %d (paid %.1f)\n',
              nrow(cc$imp$draws), mean(cc$n_paid)))
  cat(sprintf('  children 0-4 w/ imputed label (ambiguous): %.1f (weighted %.0f)\n',
              sum(cc$prim$rec[cc$prim$imputed]), sum(cc$prim$child_weight[cc$prim$imputed])))
  cat(sprintf('  children 0-4 still out (no label): %.1f (weighted %.0f)\n',
              nrow(cc$dropped) / cc$draws, cc$wt_kids - sum(cc$kids$child_weight)))
  kids <- cc$kids
  prim <- cc$prim
  cat('  children 0-4 in non-parental care ', n_rec(prim$rec), '\n', sep = '')

  keep   <- prim$total_hours >= 10
  b_kids <- prim[keep, ]
  a <- summarise_universe(prim,   UNIVERSE_A)
  b <- summarise_universe(b_kids, UNIVERSE_B)

  # Targets are in HOURS, so report share of hours below ten-hour line too.
  hA <- sum(prim$child_weight * prim$hours)
  hB <- sum(prim$child_weight[keep] * prim$hours[keep])
  cat('\n== children reaching the ten-hour line\n',
      sprintf('  universe A %.1f children, universe B %.1f (%.1f%% kept)\n',
              sum(prim$rec), sum(prim$rec[keep]), 100 * mean(keep)),
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

  show_table('CENTRE-HOME SPLIT OF PAID FORMAL CARE, by region (step 5, universe A)',
             a$split, 4)
  show_table(paste0('PART-TIME SHARE (hours < ', PT_FT_CUT_HOURS, '), by region and type'),
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
           n = n_rec(prim$rec)),
    # SHORT SHARE OF PART-TIME. Children under 10 total hrs/week over
    # children part-time in simulator's sense (primary hours < PT_FT_CUT_HOURS).
    # Every under-10 child part-time -- primary hours <= total hours. Step 1
    # applies it to simulator's predicted part-time children per geography.
    tibble(universe = UNIVERSE_A, quantity = 'short_share_of_part_time',
           key = 'national',
           value = sum(prim$child_weight[!keep]) /
                   sum(prim$child_weight[prim$hours < PT_FT_CUT_HOURS]),
           n = n_rec(prim$rec[prim$hours < PT_FT_CUT_HOURS])),
    # Same share by region, by primary care type -- 05_compare_nsece_shares.R
    # tests whether either split moves step 1; build keeps national.
    short_share_by(prim, 'region', 'short_share_of_part_time_region'),
    short_share_by(prim, 'ecec_choice', 'short_share_of_part_time_type'),
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
    both('formal', 'formal_share',        region, formal_share),
    both('mix',    'informal_mix',        ecec_choice, share),
    both('hours',  'weekly_hours',        ecec_choice, weekly))

  write_csv(res, out)
  cat('\nwrote ', out, '\n', sep = '')
  invisible(res)
}



if (sys.nframe() == 0) main()
