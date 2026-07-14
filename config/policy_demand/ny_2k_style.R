#------------------------------------------------------------------------------
# ny_2k_style.R
#
# Example QUANTITY-LIMITED (rationed) demand policy, modeled on New York's 2K
# program: a fixed number of program slots for 2-year-olds in center-based
# care, allocated by lottery. Families that win a slot pay nothing for school
# hours; care beyond school hours is out of pocket, so full-time users face a
# partial effective subsidy.
#
# Rationed-policy contract (see load_demand_policy):
#   - rationing$slots: length-4 slot counts by market sector (or function(year)
#     for a slot schedule). Sector 1 (Unpaid Center-Based) must be 0.
#   - is_child_program_eligible(): which units' child can enter the lottery.
#     Must match the per-child eligibility applied inside do_demand_policy().
#   - do_demand_policy() accepts offered1/offered2: treat child 1/2 as holding
#     an offer in EVERY rationed sector. The framework assembles offer states
#     column-wise, so the subsidy for a given choice column only matters when
#     that column's sector is actually offered.
#
# Take-up accounting: a slot is consumed when an offered eligible child
# enrolls (pt or ft) in the offered sector; slots are counted in children,
# not hours.
#------------------------------------------------------------------------------


# Program slots by market sector:
#   [1] Unpaid Center-Based (must be 0), [2] Low-Priced Center-Based,
#   [3] High-Priced Center-Based, [4] Paid Home-Based
rationing <- list(
  slots = c(0, 100000, 50000, 0),

  # School-day coverage: hours up to this cap are fully paid by the program.
  # PT care (10 hrs/wk) is fully covered; FT care (40 hrs/wk) gets a 75%
  # effective subsidy (30/40)
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
  # Computes offer-conditional program subsidies. With no offers, the subsidy
  # is zero everywhere (the no-offer state of the lottery). With offered1/2,
  # the eligible child's cost in rationed sectors is covered up to the
  # school-hours share.
  #
  # Params:
  #   - parent_units_df (df): Parent unit data with child_age.* columns
  #   - catalog (tibble): Choice catalog from get_choice_catalog()
  #   - P (dbl[4]): Price vector for 4 ECEC market sectors
  #   - n_children (int): Number of children (1 or 2)
  #   - agi_matrix (matrix): n_units x n_choices AGI values (unused)
  #   - taxes_matrix (matrix): n_units x n_choices tax values (unused)
  #   - gross_ecec_cost_matrix (matrix): n_units x n_choices care costs (unused)
  #   - child1_cost_matrix (matrix): n_units x n_choices child 1 care costs
  #   - child2_cost_matrix (matrix): n_units x n_choices child 2 care costs
  #   - offered1 (logical): Treat child 1 as holding an offer in every
  #       rationed sector
  #   - offered2 (logical): Same for child 2
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

  rationed_sectors <- which(rationing$slots > 0)

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
                          sector_col %in% rationed_sectors &
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
