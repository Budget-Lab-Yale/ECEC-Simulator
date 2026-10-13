#------------------------------------------------------------------------------
# nsece_mi_draws.R
#
# Multiple imputation of NSECE labels (2026-10-12). Three random draws per
# uncertain record -- paid status of unknown-payment rows, paid-centre tier
# (impute_unknown_payment()), hours of zero-hour regular rows
# (resolve_regular_care()). One draw moved every non-program demand target
# up to ~1%, high-price share of paid centres 2.5 points across seed
# pairs. Draw stage now runs NSECE_MI_DRAWS times; draw m uses base seed +
# m x NSECE_MI_SEED_STEP (both seeds), m = 0 = single draw before. Copies
# stacked, each row weight share 1 / M (draw_weight) -- weighted results =
# pooled average over draws.
#
# Called from: 1a_nsece_processing.R (enrollment, one row per child x
# draw -- certain children too, one rule for every reader),
# state_data/code/02_derive_nsece_constants.R (constants on stacked rows,
# weight / M).
#------------------------------------------------------------------------------

# Draws per uncertain record (M). 1a: every child M enrollment rows. 50 --
# over 5 base seed pairs, M = 10 left high-price share of paid centres in a
# 0.28-point range, universe ratio 1.0979-1.1004; M = 50: 0.21 points,
# 1.0990-1.0996. 1a ~11 min (~3.5 at M = 10), 02 ~2 min; record counts
# k / 50 have two decimals at most
NSECE_MI_DRAWS <- 50L

# Seed step between draws -- > 1, so seed + 1 (tier-only stream,
# impute_unknown_payment()) never meets another draw's seed
NSECE_MI_SEED_STEP <- 7919L



nsece_mi_seed <- function(base, m) {

  #----------------------------------------------------------------------------
  # Seed of draw m from base seed. m = 0 -> base (single draw before MI).
  #
  # Params:
  #   - base (int): base seed (IMPUTE_PAY_SEED, REGULAR_HOURS_SEED)
  #   - m (int): draw index, 0 .. NSECE_MI_DRAWS - 1
  #
  # Returns: (int) seed
  #----------------------------------------------------------------------------

  as.integer(base + m * NSECE_MI_SEED_STEP)
}



stack_mi_draws <- function(tables, keys = c('hh_id', 'child_id')) {

  #----------------------------------------------------------------------------
  # Stacks per-draw tables (list element i = draw i - 1). Each table one row
  # per key, same keys every draw -- stops otherwise. Adds draw (int, 0 ..
  # M - 1), draw_weight (1 / M): weight x draw_weight summed over a key's
  # rows = weight, so totals exact.
  #
  # Params:
  #   - tables (list of df): one table per draw, draw 0 first
  #   - keys (chr): columns identifying a record
  #
  # Returns: (tibble) rows of every table, sorted keys x draw, + draw,
  #   draw_weight
  #----------------------------------------------------------------------------

  M <- length(tables)
  if (M < 1) stop('stack_mi_draws: no draws')
  k0 <- tables[[1]] %>% distinct(across(all_of(keys)))
  if (nrow(k0) != nrow(tables[[1]])) stop('stack_mi_draws: draw 0 has duplicate keys')
  out <- bind_rows(lapply(seq_len(M), function(i) mutate(tables[[i]], draw = i - 1L)))
  per <- out %>% count(across(all_of(c(keys, 'draw'))))
  if (any(per$n != 1) || nrow(per) != M * nrow(k0) ||
      nrow(semi_join(per, k0, by = keys)) != nrow(per)) {
    stop('stack_mi_draws: every draw must hold the same records, one row each')
  }
  out %>%
    mutate(draw_weight = 1 / M) %>%
    arrange(across(all_of(c(keys, 'draw'))))
}
