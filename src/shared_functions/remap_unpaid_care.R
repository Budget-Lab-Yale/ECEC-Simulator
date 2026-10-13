#------------------------------------------------------------------------------
# remap_unpaid_care.R
#
# Opt-in experiment (env var ECEC_REMAP_UNPAID=1, read in config.R): Unpaid
# Center-Based children supplied by low-price paid-center sector. Choice side
# untouched -- same choice set, family price 0 (P[1] = 0), same alpha.
# Supply side: their hours join low-price sector demand in equilibrium
# (labor, wages, price); provider gets equilibrium low-price supplier price
# per hour, paid publicly -> fiscal line total_unpaid_remap_payment.
# Off (default): every function here identity / no-op.
#------------------------------------------------------------------------------



unpaid_remap_on <- function() {

  #----------------------------------------------------------------------------
  # Remap switch -- global remap_unpaid_care (config.R), else env var
  # ECEC_REMAP_UNPAID, else off.
  #
  # Returns: (logical) TRUE when experiment on
  #----------------------------------------------------------------------------

  s <- if (exists('remap_unpaid_care', envir = globalenv())) {
    get('remap_unpaid_care', envir = globalenv())
  } else {
    Sys.getenv('ECEC_REMAP_UNPAID', '0')
  }
  isTRUE(s) || identical(as.character(s), '1')
}



remap_unpaid_demand <- function(Qd) {

  #----------------------------------------------------------------------------
  # Moves sector-1 (Unpaid Center-Based) hours into sector 2 (Low-Priced
  # Center-Based) for supply side. Identity when switch off.
  #
  # Params:
  #   - Qd (num vec): hours demanded by market sector (length 4)
  #
  # Returns: (num vec) Qd, sector 1 folded into sector 2 when on
  #----------------------------------------------------------------------------

  if (!unpaid_remap_on()) return(Qd)
  Qd[2] <- Qd[2] + Qd[1]
  Qd[1] <- 0
  Qd
}



unpaid_remap_fiscal <- function(parent_units, price) {

  #----------------------------------------------------------------------------
  # Public payment for remapped care: Unpaid Center-Based hours of discrete
  # choices x low-price supplier price. Weight = parent_unit_weight, same as
  # supply_subsidy line in aggregate_fiscal_cost() (3d).
  #
  # Params:
  #   - parent_units (list): named list of parent unit dfs (discrete choices;
  #       ecec_type.c, ecec_hours.c, per_weight1, per_weight2)
  #   - price (dbl): low-price supplier price per hour (P_supply[2])
  #
  # Returns: (list) hours (weighted annual hours remapped), payment (dollars)
  #----------------------------------------------------------------------------

  hours <- 0
  for (pu_df in parent_units) {
    if (is.null(pu_df) || nrow(pu_df) == 0) next
    w <- (pu_df$per_weight1 + coalesce(pu_df$per_weight2, pu_df$per_weight1)) / 2
    for (c in 1:2) {
      type_col <- paste0('ecec_type.', c)
      if (!type_col %in% names(pu_df)) next
      is_unpaid <- coalesce(pu_df[[type_col]] == 'Unpaid Center-Based', FALSE)
      h <- unname(HOURS_ANNUAL[pu_df[[paste0('ecec_hours.', c)]]])
      hours <- hours + sum((w * h)[is_unpaid])
    }
  }
  list(hours = hours, payment = hours * price)
}
