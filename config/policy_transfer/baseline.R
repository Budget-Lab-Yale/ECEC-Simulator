#------------------------------------------------------------------------------
# baseline.R - Transfer Policy
#
# No choice-dependent transfers under current law: returns a zero matrix.
#
# Transfer policies provide cash transfers that may depend on the family's
# care choice (unlike policy_tax, which can only vary by employment status).
# Transfers enter net income directly: they are NOT netted against childcare
# costs, so ECEC burden statistics and the SPM childcare-expense channel are
# unaffected; fiscal accounting reports them as their own budget component.
#------------------------------------------------------------------------------



do_transfer_policy <- function(parent_units_df, catalog, n_children) {

  #----------------------------------------------------------------------------
  # Baseline: no transfers.
  #
  # Params:
  #   - parent_units_df (tibble): Parent unit data
  #   - catalog (tibble): Choice catalog from get_choice_catalog()
  #   - n_children (int): Number of children (1 or 2)
  #
  # Returns:
  #   matrix (n_units x n_choices) of transfer amounts (all zero)
  #----------------------------------------------------------------------------

  matrix(0, nrow = nrow(parent_units_df), ncol = nrow(catalog))
}
