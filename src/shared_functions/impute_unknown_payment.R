#------------------------------------------------------------------------------
# impute_unknown_payment.R
#
# Paid status (+ paid-centre tier) drawn for NSECE arrangements w/ unknown
# payment amount (HH9_WEEKLY_PAYMENT_TC -1 DK / refused, -2 no Section J).
# Every counting unknown row takes its draw -- child whose label payment
# can't change gets same label from any draw. Replaces complete case
# (2026-10-12): drop depended on outcome (Parent Only child never
# ambiguous), raised Parent Only share 1.67 points among children 0-4.
# Questionnaire paid status (HH9_PAID 1 / 0, paid_q) known -> used in
# place of paid draw (2026-10-12): 295 of 382 drawn DK rows among children
# 0-4 answered "pay provider" (J1), 195 of them drawn unpaid before. Known
# amounts conflict w/ HH9_PAID on 6 of 4,456 regular rows, children 0-4.
#
# Called from: 1a_nsece_processing.R (labels -> assign_primary_care()),
# state_data/code/02_derive_nsece_constants.R (constants, by-age diagnostic).
# One function, one seed, one draw order -> both label same children alike.
# Both call it NSECE_MI_DRAWS times, draw m seed nsece_mi_seed(IMPUTE_PAY_SEED,
# m) -- labels averaged over draws (nsece_mi_draws.R, 2026-10-12; one draw
# before, seed choice moved high-price share of paid centres 2.5 points).
#------------------------------------------------------------------------------

# Dedicated seed -- own stream, global RNG state restored after
IMPUTE_PAY_SEED <- 20261012L

# Min raw donor rows for tier cell finer than age (region x density x age,
# density x age; switch on only -- off = age cells only); thinner cell falls to
# next level
TIER_CELL_MIN_N <- 30L



impute_pay_age_cell <- function(age) {

  #----------------------------------------------------------------------------
  # Child age (years) -> imputation age cell, same bins as 1a centre price
  # bins (older children pooled w/ 4-5).
  #
  # Params:
  #   - age (num vec): child age in years
  #
  # Returns: (chr vec) '0-1', '2-3', '4-5'; NA where age NA
  #----------------------------------------------------------------------------

  ifelse(is.na(age), NA_character_,
         ifelse(age <= 1, '0-1', ifelse(age <= 3, '2-3', '4-5')))
}



pay_kind <- function(is_k8, is_center, is_home_based, is_other_kind) {

  #----------------------------------------------------------------------------
  # Arrangement kind for imputation cells -- same rule as 1a / 02
  # unknown_kind, applied to every row (known payment too). NA flags act
  # FALSE, as case_when().
  #
  # Params:
  #   - is_k8, is_center, is_home_based, is_other_kind (lgl vec)
  #
  # Returns: (chr vec) 'center' | 'home' | 'other'; NA for K-8 / unclassified
  #----------------------------------------------------------------------------

  case_when(
    is_k8 %in% TRUE         ~ NA_character_,
    is_center %in% TRUE     ~ 'center',
    is_home_based %in% TRUE ~ 'home',
    is_other_kind %in% TRUE ~ 'other'
  )
}



imputed_pay_label <- function(kind, imp_paid, imp_tier, paid_centre = NULL) {

  #----------------------------------------------------------------------------
  # Drawn paid status (+ tier) -> care label.
  #
  # Params:
  #   - kind (chr vec): 'center' | 'home' | 'other' | NA
  #   - imp_paid (lgl vec): drawn paid status; NA = no draw
  #   - imp_tier (chr vec): 'High' | 'Low' | NA
  #   - paid_centre (chr): single paid-centre label (02 'Center-Based');
  #       NULL -> tier labels (1a)
  #
  # Returns: (chr vec) label; NA where no draw (or paid centre w/out tier)
  #----------------------------------------------------------------------------

  centre_paid <- if (is.null(paid_centre)) {
    case_when(imp_tier %in% 'High' ~ 'High-Priced Center-Based',
              imp_tier %in% 'Low'  ~ 'Low-Priced Center-Based')
  } else {
    rep(paid_centre, length(kind))
  }
  case_when(
    is.na(imp_paid) | is.na(kind)  ~ NA_character_,
    kind == 'center' &  imp_paid   ~ centre_paid,
    kind == 'center' & !imp_paid   ~ 'Unpaid Center-Based',
    kind == 'home'   &  imp_paid   ~ 'Paid Home-Based',
    kind == 'home'   & !imp_paid   ~ 'Unpaid Home-Based',
    kind == 'other'  &  imp_paid   ~ 'Other Paid',
    kind == 'other'  & !imp_paid   ~ 'Other Unpaid'
  )
}



impute_unknown_payment <- function(arr, seed = IMPUTE_PAY_SEED) {

  #----------------------------------------------------------------------------
  # Draws paid status for every counting unknown-payment arrangement from
  # weighted paid share among known-payment arrangements of same Census
  # kind x age cell, else kind over all ages (child weights). Urban density
  # on (`density` column in arr -- state runs, 2026-10-14): region x kind x
  # age cell first (>= TIER_CELL_MIN_N raw donor rows). Centre drawn paid ->
  # tier drawn from weighted High / Low split of known tiered paid centres:
  # on region x urban density x age cell, < TIER_CELL_MIN_N raw donors ->
  # density x age, then age; off (default, = main) age only. Draws made for whole
  # universe, sorted (hh_id, child_id, provider_id), two uniforms per row
  # (paid, tier) -- draw for a row doesn't depend on which children end up
  # ambiguous, nor on caller's tier labels. Caller applies draws to every
  # counting unknown row. Known paid centre w/ tier_unknown (1a: imputed
  # hours, no own hourly price) -> tier alone drawn, same tier cells, own
  # stream (seed + 1) -- main draws unchanged (2026-10-12).
  #
  # Params:
  #   - arr (df): child x provider rows -- hh_id, child_id, provider_id,
  #       kind (chr: 'center' | 'home' | 'other' | NA), paid (lgl; NA =
  #       unknown payment), tier (chr: 'High' | 'Low' | NA; known paid
  #       centres w/ price tier), counts (lgl: regular == 1 & hours > 0),
  #       age (num, years), region (any), child_weight (num); optional
  #       density (HH9_COMM_URBAN_DENSITY 1-3), tier_unknown (lgl: known
  #       paid centre, tier to draw; absent = FALSE), paid_q (lgl:
  #       questionnaire paid status, NA unknown -- set -> imp_paid; uniforms
  #       still drawn, other rows' draws unchanged)
  #   - seed (int): dedicated seed
  #
  # Returns: (list)
  #   - draws (tibble): hh_id, child_id, provider_id, kind, age_cell,
  #       paid_share, u_paid, imp_paid (lgl), u_tier, high_share, tier_level
  #       (cell level used), imp_tier (chr 'High' | 'Low'; NA unless centre
  #       drawn paid w/ tier donors), tier_only (lgl: tier_unknown row --
  #       imp_paid TRUE, paid_share / u_paid NA)
  #   - paid_cells (tibble): kind, age_cell, paid_share, n (raw donor rows),
  #       wt (weighted donor children)
  #   - paid_cells_region (tibble): region, kind, age_cell, paid_share, n, wt
  #   - tier_cells (tibble): level, region, density, age_cell, high_share,
  #       n, wt -- every level, thin cells incl.
  #----------------------------------------------------------------------------

  arr <- arr %>%
    mutate(age_cell = impute_pay_age_cell(age),
           wt = if_else(!is.na(child_weight) & child_weight > 0, child_weight, 0))

  # Donors: counting rows, known kind + payment, positive weight
  paid_cells <- arr %>%
    filter(counts %in% TRUE, !is.na(kind), !is.na(paid), !is.na(age_cell), wt > 0) %>%
    group_by(kind, age_cell) %>%
    summarise(paid_share = sum(wt * paid) / sum(wt), n = n(), wt = sum(wt),
              .groups = 'drop')
  # Finer donor cells (2026-10-14, switch on only): Census region x kind x age
  # cell (used w/ >= TIER_CELL_MIN_N raw donor rows), and kind over all ages
  # (fallback when kind x age cell empty). Fallback order region x kind x age
  # -> kind x age -> kind. NSECE has no state id -- region finest geography
  paid_donors <- arr %>%
    filter(counts %in% TRUE, !is.na(kind), !is.na(paid), !is.na(age_cell), wt > 0)
  paid_cells_region <- paid_donors %>%
    filter(!is.na(region)) %>%
    group_by(region, kind, age_cell) %>%
    summarise(paid_share = sum(wt * paid) / sum(wt), n = n(), wt = sum(wt),
              .groups = 'drop')
  paid_cells_kind <- paid_donors %>%
    group_by(kind) %>%
    summarise(paid_share = sum(wt * paid) / sum(wt), n = n(), wt = sum(wt),
              .groups = 'drop')
  # Urban density on = `density` column in arr (1a passes it only w/ switch
  # on). Off = main: no region cells anywhere -- paid status kind x age ->
  # kind, tier age only
  use_density <- 'density' %in% names(arr)
  if (!use_density) paid_cells_region <- paid_cells_region[0, ]

  # Tier donors: known paid centres w/ tier (caller's labels -- 1a tier
  # line). Urban density on (density column): cells region x density x age ->
  # density x age -> age. Density off (no column): age cells only. Finer
  # cell used only w/ >= TIER_CELL_MIN_N raw donor rows, age cell always
  if (!use_density) arr$density <- NA_integer_
  tier_donors <- arr %>%
    filter(counts %in% TRUE, kind %in% 'center', paid %in% TRUE,
           !is.na(tier), !is.na(age_cell), wt > 0)
  tier_share <- function(d, keys, level) {
    d %>%
      group_by(across(all_of(keys))) %>%
      summarise(high_share = sum(wt * (tier == 'High')) / sum(wt), n = n(),
                wt = sum(wt), .groups = 'drop') %>%
      mutate(level = level)
  }
  tier_rda <- tier_share(filter(tier_donors, !is.na(region), !is.na(density)),
                         c('region', 'density', 'age_cell'), 'region x density x age')
  tier_da  <- tier_share(filter(tier_donors, !is.na(density)),
                         c('density', 'age_cell'), 'density x age')
  tier_a   <- tier_share(tier_donors, 'age_cell', 'age')
  tier_cells <- if (use_density) bind_rows(tier_rda, tier_da, tier_a) else tier_a

  # Draw universe: counting rows, known kind, unknown payment
  universe <- arr %>%
    filter(counts %in% TRUE, !is.na(kind), is.na(paid), !is.na(age_cell)) %>%
    arrange(hh_id, child_id, provider_id)
  n <- nrow(universe)
  u <- with_rng_preserved({
    set.seed(seed)
    matrix(runif(2 * n), ncol = 2, byrow = TRUE)
  })

  # Tier share per row: finest cell w/ >= TIER_CELL_MIN_N donors, age always
  ok <- function(t) filter(t, n >= TIER_CELL_MIN_N)
  add_tier_share <- function(d) {
    if (!use_density) {
      # Off = main: age cells only, no region
      return(d %>%
        left_join(select(tier_a, age_cell, hs_a = high_share), by = 'age_cell') %>%
        mutate(tier_level = if_else(!is.na(hs_a), 'age', NA_character_),
               high_share = hs_a))
    }
    d %>%
      left_join(select(ok(tier_rda), region, density, age_cell, hs_rda = high_share),
                by = c('region', 'density', 'age_cell')) %>%
      left_join(select(ok(tier_da), density, age_cell, hs_da = high_share),
                by = c('density', 'age_cell')) %>%
      left_join(select(tier_a, age_cell, hs_a = high_share), by = 'age_cell') %>%
      mutate(tier_level = case_when(!is.na(hs_rda) ~ 'region x density x age',
                                    !is.na(hs_da)  ~ 'density x age',
                                    !is.na(hs_a)   ~ 'age'),
             high_share = coalesce(hs_rda, hs_da, hs_a))
  }

  if (!'paid_q' %in% names(universe)) universe$paid_q <- NA
  draws <- universe %>%
    select(hh_id, child_id, provider_id, kind, region, density, age_cell, paid_q) %>%
    mutate(u_paid = u[, 1], u_tier = u[, 2]) %>%
    left_join(select(filter(paid_cells_region, n >= TIER_CELL_MIN_N), region, kind, age_cell,
                     ps_region = paid_share), by = c('region', 'kind', 'age_cell')) %>%
    left_join(select(paid_cells, kind, age_cell, ps_ka = paid_share), by = c('kind', 'age_cell')) %>%
    left_join(select(paid_cells_kind, kind, ps_k = paid_share), by = 'kind') %>%
    mutate(paid_share = coalesce(ps_region, ps_ka, ps_k)) %>%
    add_tier_share() %>%
    # Questionnaire answer wins; draw only where HH9_PAID unknown
    mutate(imp_paid = coalesce(as.logical(paid_q), u_paid < paid_share),
           imp_tier = case_when(kind != 'center' | !imp_paid | is.na(high_share) ~ NA_character_,
                                u_tier < high_share                              ~ 'High',
                                TRUE                                             ~ 'Low')) %>%
    select(hh_id, child_id, provider_id, kind, age_cell, paid_share, u_paid,
           imp_paid, u_tier, high_share, tier_level, imp_tier) %>%
    mutate(tier_only = FALSE)

  # Tier-only universe: counting known paid centres flagged tier_unknown
  if (!'tier_unknown' %in% names(arr)) arr$tier_unknown <- FALSE
  tier_only <- arr %>%
    filter(counts %in% TRUE, kind %in% 'center', paid %in% TRUE,
           tier_unknown %in% TRUE, !is.na(age_cell)) %>%
    arrange(hh_id, child_id, provider_id)
  u2 <- with_rng_preserved({
    set.seed(seed + 1L)
    runif(nrow(tier_only))
  })
  tier_draws <- tier_only %>%
    select(hh_id, child_id, provider_id, kind, region, density, age_cell) %>%
    mutate(u_tier = u2, paid_share = NA_real_, u_paid = NA_real_, imp_paid = TRUE) %>%
    add_tier_share() %>%
    mutate(imp_tier = case_when(is.na(high_share)   ~ NA_character_,
                                u_tier < high_share ~ 'High',
                                TRUE                ~ 'Low'),
           tier_only = TRUE) %>%
    select(names(draws))

  list(draws = bind_rows(draws, tier_draws), paid_cells = paid_cells,
       paid_cells_region = paid_cells_region, tier_cells = tier_cells)
}
