#------------------------------------------------------------------------------
# baseline.R - Tax Policy
#
# Identity tax policy - no modification to taxes.
# This is the default policy when no tax policy is specified.
#------------------------------------------------------------------------------


do_tax_policy <- function(parent_units_df, n_children, year) {

  #----------------------------------------------------------------------------
  # Baseline tax policy: no modification.
  #
  # This function returns the parent units unchanged, preserving the tax
  # liabilities calculated from donor pool matching.
  #
  # Params:
  #   - parent_units_df (tibble): Parent unit data with:
  #       - total_tax.{none,pt,ft}: Total tax liability by employment choice
  #       - agi.{none,pt,ft}: AGI by employment choice
  #       - income_tax.{none,pt,ft}: Income tax only
  #       - liab_iit1.{emp}, liab_iit2.{emp}: Income tax per tax unit
  #       - earnings1.{emp}, earnings2.{emp}: Earnings per parent
  #       - n_children, child_age.1, child_age.2: Child info
  #       - married1, filing_status1, filing_status2: Marital/filing status
  #       - age1, age2: Parent ages
  #       - region: Geographic region
  #   - n_children (int): Effective number of children (1 or 2)
  #   - year (int): Simulation year
  #
  # Returns:
  #   parent_units_df unchanged
  #----------------------------------------------------------------------------

  return(parent_units_df)
}
