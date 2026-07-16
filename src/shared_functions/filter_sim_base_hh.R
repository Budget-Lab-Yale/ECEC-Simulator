filter_sim_base_hh <- function(sim_base_hh, keep_hh_ids, weight_inflation = 1) {

  #----------------------------------------------------------------------------
  # Restricts all simulation base tables to a set of households, optionally
  # inflating survey weights (used by simulation sub-sampling to preserve
  # population aggregates; state filtering passes 1 since ACS weights are
  # valid subnationally).
  #
  # Factored from the inline sub-sampling block in initialize_simulation() so
  # the national sub-sample path and the state-filter path share one cascade.
  #
  # Params:
  #   - sim_base_hh (list): Simulation base tables (households, parent_units,
  #       children, tax_units, spm tables, ...)
  #   - keep_hh_ids (vec): Household ids to keep
  #   - weight_inflation (dbl): Factor applied to all survey weights (1 = none)
  #
  # Returns: (list) sim_base_hh with all tables filtered (and weights inflated)
  #----------------------------------------------------------------------------

  sim_base_hh$households <- sim_base_hh$households %>%
    filter(hh_id %in% keep_hh_ids) %>%
    mutate(hh_weight = hh_weight * weight_inflation)

  for (tbl_name in c('household_members', 'parent_units', 'children',
                      'tax_units', 'enrollment', 'enrollment_joint')) {
    if (!is.null(sim_base_hh[[tbl_name]]) && 'hh_id' %in% names(sim_base_hh[[tbl_name]])) {
      sim_base_hh[[tbl_name]] <- sim_base_hh[[tbl_name]] %>%
        filter(hh_id %in% keep_hh_ids)
    }
  }

  # Inflate all survey weights to maintain population-level aggregates
  # (hh_weight already inflated above; these are the per-person/child weights
  # used by the equilibrium solver and output aggregation)
  if (weight_inflation != 1) {
    sim_base_hh$household_members <- sim_base_hh$household_members %>%
      mutate(per_weight = per_weight * weight_inflation)

    sim_base_hh$parent_units <- sim_base_hh$parent_units %>%
      mutate(
        per_weight1 = per_weight1 * weight_inflation,
        per_weight2 = per_weight2 * weight_inflation
      )

    sim_base_hh$children <- sim_base_hh$children %>%
      mutate(child_weight = child_weight * weight_inflation)

    sim_base_hh$tax_units <- sim_base_hh$tax_units %>%
      mutate(tax_unit_weight = tax_unit_weight * weight_inflation)
  }

  # SPM tables: filter by hh_id via pu_spm_xwalk
  if (!is.null(sim_base_hh$pu_spm_xwalk)) {
    sim_base_hh$pu_spm_xwalk <- sim_base_hh$pu_spm_xwalk %>%
      filter(hh_id %in% keep_hh_ids)

    if (!is.null(sim_base_hh$spm_units)) {
      kept_spm_ids <- unique(sim_base_hh$pu_spm_xwalk$spm_unit_id)
      sim_base_hh$spm_units <- sim_base_hh$spm_units %>%
        filter(spm_unit_id %in% kept_spm_ids) %>%
        mutate(
          spm_weight = spm_weight * weight_inflation,
          n_members_weighted = n_members_weighted * weight_inflation,
          n_children_weighted = n_children_weighted * weight_inflation
        )
    }

    if (!is.null(sim_base_hh$spm_parent_earnings)) {
      sim_base_hh$spm_parent_earnings <- sim_base_hh$spm_parent_earnings %>%
        filter(hh_id %in% keep_hh_ids)
    }
  }

  sim_base_hh
}
