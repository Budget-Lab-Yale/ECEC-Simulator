#------------------------------------------------------------------------------
# ny_2k_ramp.R
#
# Quantity-limited (rationed) demand policy with a SLOT SCHEDULE: the same
# NY-2K-style program as ny_2k_style.R, but slots ramp in linearly over three
# years (1/3 in 2026, 2/3 in 2027, full from 2028) via rationing$slots as a
# function(year). Slot-schedule policies should leave the runscript
# phase_in_years column blank — the ramp lives here.
#
# See ny_2k_style.R for the full rationed-policy contract documentation.
#------------------------------------------------------------------------------


# Sectors covered by the program (constant across the ramp). Kept as a
# separate constant because rationing$slots is a function of year here and
# do_demand_policy() does not receive the year.
PROGRAM_SECTORS <- c(2, 3)

# Full-scale program slots by market sector
FULL_SLOTS <- c(0, 100000, 50000, 0)

rationing <- list(
  # Linear ramp: 1/3 of slots in 2026, 2/3 in 2027, full from 2028 onward
  slots = function(year) {
    ramp <- pmin(1, pmax(0, (year - 2025) / 3))
    ramp * FULL_SLOTS
  },

  # School-day coverage: hours up to this cap are fully paid by the program
  covered_hours_per_week = 30
)



is_child_program_eligible <- function(parent_units_df, child_idx) {

  #----------------------------------------------------------------------------
  # Program lottery eligibility: 2-year-olds (universal, no income test).
  #
  # Params:
  #   - parent_units_df (df): Parent units with child_age.{idx} columns
  #   - child_idx (int): Child slot (1 or 2)
  #
  # Returns: (logical vec) TRUE where the child can enter the program lottery
  #----------------------------------------------------------------------------

  age <- parent_units_df[[paste0('child_age.', child_idx)]]
  !is.na(age) & age == 2
}



do_demand_policy <- function(parent_units_df, catalog, P, n_children,
                             agi_matrix, taxes_matrix, gross_ecec_cost_matrix,
                             child1_cost_matrix = NULL, child2_cost_matrix = NULL,
                             offered1 = FALSE, offered2 = FALSE) {

  #----------------------------------------------------------------------------
  # Computes offer-conditional program subsidies (see ny_2k_style.R). With
  # offered1/2, the eligible child's cost in program sectors is covered up to
  # the school-hours share.
  #
  # Params: see ny_2k_style.R (identical signature)
  #
  # Returns:
  #   matrix (n_units x n_choices) of subsidy amounts
  #----------------------------------------------------------------------------

  n_units <- nrow(parent_units_df)
  n_choices <- nrow(catalog)
  subsidy_matrix <- matrix(0, nrow = n_units, ncol = n_choices)

  # No-offer state: no program subsidy
  if (!isTRUE(offered1) && !isTRUE(offered2)) {
    return(subsidy_matrix)
  }

  # Covered share by hours choice: full coverage up to school hours
  covered_share <- c(
    pt = min(1, rationing$covered_hours_per_week / PT_CARE_HOURS),
    ft = min(1, rationing$covered_hours_per_week / FT_CARE_HOURS)
  )

  add_child_subsidy <- function(subsidy_matrix, child_idx, offered, cost_matrix) {
    if (!isTRUE(offered) || is.null(cost_matrix)) return(subsidy_matrix)

    # Per-child eligibility must match is_child_program_eligible()
    elig <- is_child_program_eligible(parent_units_df, child_idx)
    if (!any(elig)) return(subsidy_matrix)

    sector_col <- catalog[[paste0('child', child_idx, '_market_sector_id')]]
    hours_col <- catalog[[paste0('child', child_idx, '_hours_choice')]]

    program_cols <- which(!is.na(sector_col) &
                          sector_col %in% PROGRAM_SECTORS &
                          hours_col %in% c('pt', 'ft'))
    for (k in program_cols) {
      share <- covered_share[[hours_col[k]]]
      subsidy_matrix[elig, k] <- subsidy_matrix[elig, k] + share * cost_matrix[elig, k]
    }

    subsidy_matrix
  }

  subsidy_matrix <- add_child_subsidy(subsidy_matrix, 1, offered1, child1_cost_matrix)
  if (n_children == 2) {
    subsidy_matrix <- add_child_subsidy(subsidy_matrix, 2, offered2, child2_cost_matrix)
  }

  return(subsidy_matrix)
}
