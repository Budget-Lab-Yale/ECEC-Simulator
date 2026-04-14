#------------------------------------------------------------------------------
# baseline.R
#
# Baseline CDCTC (Child and Dependent Care Tax Credit) policy - current law.
#
# Implements the CDCTC as amended by the One Big Beautiful Bill Act (OBBBA, 2025):
#   - Credit rate: 50% at low income, two-tier phase-out to 20%
#     Tier 1: 50% -> 35%
#       Single/HoH: AGI > $15K, -1pp per $2K
#       MFJ: AGI > $30K, -1pp per $2K
#     Tier 2: 35% -> 20%
#       Single/HoH: AGI > $75K, -1pp per $2K
#       MFJ: AGI > $150K, -1pp per $4K
#   - Expense caps: $3k per child, max 2 children ($6k)
#   - Earned income limit: lesser of two spouses' earnings (for MFJ)
#   - Non-refundable (limited to income tax liability)
#   - Discrete rounding for phase-out calculation
#
# Tax-unit-level calculation:
#   - For married couples (same tax_unit_id): joint AGI, min(earnings)
#   - For cohabiting couples (different tax_unit_ids): separate calculation per TU
#   - Children are assigned to their claiming parent's tax unit
#------------------------------------------------------------------------------



do_cdctc_policy <- function(parent_units_df, catalog, P, n_children,
                            agi_matrix, taxes_matrix, iit_matrix,
                            gross_ecec_cost_matrix, subsidy_matrix,
                            # New tax-unit-level parameters
                            child1_cost_matrix = NULL,
                            child2_cost_matrix = NULL,
                            agi1_matrix = NULL,
                            agi2_matrix = NULL,
                            iit1_matrix = NULL,
                            iit2_matrix = NULL,
                            earnings1_matrix = NULL,
                            earnings2_matrix = NULL) {

  #----------------------------------------------------------------------------
  # Computes CDCTC credit for each choice under current law (OBBBA 2025).
  #
  # For married couples (same tax_unit_id), calculates CDCTC using:
  #   - Joint AGI (agi_matrix, which is agi1 + agi2)
  #   - min(earnings1, earnings2) as earned income limit
  #   - Combined income tax liability for non-refundable cap
  #   - MFJ phase-out thresholds
  #
  # For cohabiting couples (different tax_unit_ids), calculates separately:
  #   - Each tax unit uses its own AGI
  #   - Children's expenses assigned to claiming parent's tax unit
  #   - Each filer uses own earnings as EI limit (not MFJ so no spouse rule)
  #   - Each tax unit's credit capped by its own income tax liability
  #   - Single/HoH phase-out thresholds
  #
  # Parameters:
  #   - parent_units_df (tibble): Parent unit data with:
  #       - min_parent_earnings.* cols (for married couples)
  #       - tax_unit_id1, tax_unit_id2 (tax unit identifiers)
  #       - filing_status1, filing_status2 (1=Single/HoH, 2=MFJ)
  #       - child_tax_unit.1, child_tax_unit.2 (which TU claims each child)
  #   - catalog (tibble): Choice catalog from get_choice_catalog()
  #   - P (dbl[4]): Price vector for 4 market sectors
  #   - n_children (int): Number of children (1 or 2)
  #   - agi_matrix (matrix): n_units x n_choices combined AGI
  #   - taxes_matrix (matrix): n_units x n_choices total tax
  #   - iit_matrix (matrix): n_units x n_choices combined income tax
  #   - gross_ecec_cost_matrix (matrix): n_units x n_choices total care costs
  #   - subsidy_matrix (matrix): n_units x n_choices demand policy subsidies
  #   - child1_cost_matrix (matrix): n_units x n_choices child1 care costs
  #   - child2_cost_matrix (matrix): n_units x n_choices child2 care costs
  #   - agi1_matrix, agi2_matrix (matrix): separate AGI per tax unit
  #   - iit1_matrix, iit2_matrix (matrix): income tax per tax unit
  #   - earnings1_matrix, earnings2_matrix (matrix): earnings per parent
  #
  # Returns:
  #   matrix (n_units x n_choices) of CDCTC credit amounts
  #----------------------------------------------------------------------------

  #---------------------------
  # OBBBA CDCTC Parameters (2026+)
  #---------------------------

  exp_limit_per_child <- 3000    # Max qualified expenses per child (unchanged)
  n_dep_limit         <- 2       # Max qualifying children (unchanged)
  rate_base           <- 0.20    # Base credit rate (never phases out)
  rate_add1           <- 0.15    # Tier 1 additional rate (50% -> 35%)
  rate_add2           <- 0.15    # Tier 2 additional rate (35% -> 20%)

  # Tier 1 phase-out: 50% -> 35%
  po_thresh_add1_single <- 15000   # Single/HoH: phase-out starts at $15K
  po_thresh_add1_mfj    <- 30000   # MFJ: phase-out starts at $30K
  po_rate_add1          <- 0.000005  # -1pp per $2K
  discrete_step_add1    <- 2000    # $2K rounding step (both filing statuses)

  # Tier 2 phase-out: 35% -> 20%
  po_thresh_add2_single <- 75000   # Single/HoH: phase-out starts at $75K
  po_thresh_add2_mfj    <- 150000  # MFJ: phase-out starts at $150K
  po_rate_add2_single   <- 0.000005   # Single/HoH: -1pp per $2K
  po_rate_add2_mfj      <- 0.0000025  # MFJ: -1pp per $4K
  discrete_step_add2_single <- 2000   # Single/HoH: $2K rounding step
  discrete_step_add2_mfj    <- 4000   # MFJ: $4K rounding step

  refundable          <- FALSE   # Non-refundable (limited to tax liability)
  takeup              <- 1.0     # Take-up rate (1.0 = 100%)


  #---------------------------
  # Setup
  #---------------------------

  n_units <- nrow(parent_units_df)
  n_choices <- nrow(catalog)
  employment_choices <- catalog$employment_choice

  # Validate required tax-unit-level data is present
  if (is.null(agi1_matrix) || is.null(child1_cost_matrix)) {
    stop('CDCTC calculation requires tax-unit-level data (agi1_matrix, child1_cost_matrix)')
  }
  if (nrow(child1_cost_matrix) != n_units) {
    stop(sprintf('CDCTC dimension mismatch: n_units=%d but child1_cost_matrix has %d rows',
                 n_units, nrow(child1_cost_matrix)))
  }

  #---------------------------
  # Identify married vs cohabiting
  #---------------------------

  # Married = same tax_unit_id (or single parent with no tax_unit_id2)
  # Cohabiting = different tax_unit_ids
  if ('tax_unit_id2' %in% names(parent_units_df)) {
    is_cohabiting <- !is.na(parent_units_df$tax_unit_id2) &
                     (parent_units_df$tax_unit_id1 != parent_units_df$tax_unit_id2)
  } else {
    is_cohabiting <- rep(FALSE, n_units)
  }

  #---------------------------
  # Attribute expenses to tax units
  #---------------------------

  # For each child, determine which tax unit claims them
  # child_tax_unit.1, child_tax_unit.2 indicate which TU each child belongs to
  # Default to NA (which means TU1) if column doesn't exist
  child1_tu <- if ('child_tax_unit.1' %in% names(parent_units_df)) {
    parent_units_df$child_tax_unit.1
  } else {
    rep(NA, n_units)
  }
  child2_tu <- if ('child_tax_unit.2' %in% names(parent_units_df)) {
    parent_units_df$child_tax_unit.2
  } else {
    rep(NA, n_units)
  }

  tu1_id <- if ('tax_unit_id1' %in% names(parent_units_df)) {
    parent_units_df$tax_unit_id1
  } else {
    rep(NA, n_units)  # All children default to TU1 when no tax unit info
  }
  tu2_id <- if ('tax_unit_id2' %in% names(parent_units_df)) {
    parent_units_df$tax_unit_id2
  } else {
    rep(NA, n_units)
  }

  # Child belongs to TU1 if their tax_unit_id matches tu1_id
  # Use replace_na to handle NA comparisons (default to TU1)
  child1_in_tu1 <- replace_na(child1_tu == tu1_id, TRUE)
  child1_in_tu2 <- replace_na(!is.na(tu2_id) & (child1_tu == tu2_id), FALSE)

  child2_in_tu1 <- if (n_children == 2) {
    replace_na(child2_tu == tu1_id, TRUE)
  } else {
    rep(FALSE, n_units)
  }
  child2_in_tu2 <- if (n_children == 2) {
    replace_na(!is.na(tu2_id) & (child2_tu == tu2_id), FALSE)
  } else {
    rep(FALSE, n_units)
  }

  # Net per-child costs after subsidy (proportionally allocated)
  # Subsidy is total; allocate proportionally based on child's share of total cost
  total_cost <- gross_ecec_cost_matrix
  child1_share <- ifelse(total_cost > 0, child1_cost_matrix / total_cost, 0.5)
  child2_share <- ifelse(total_cost > 0, child2_cost_matrix / total_cost, 0.5)

  child1_net <- child1_cost_matrix - subsidy_matrix * child1_share
  child2_net <- child2_cost_matrix - subsidy_matrix * child2_share

  # Floor at 0 - use pmax with matrix to preserve dimensions
  child1_net <- pmax(child1_net, 0)
  child2_net <- pmax(child2_net, 0)

  # Ensure matrix structure is preserved (pmax can drop dimensions)
  if (!is.matrix(child1_net)) {
    dim(child1_net) <- c(n_units, n_choices)
  }
  if (!is.matrix(child2_net)) {
    dim(child2_net) <- c(n_units, n_choices)
  }

  #---------------------------
  # Tax Unit 1 CDCTC calculation
  #---------------------------

  # Expenses assigned to TU1
  tu1_expenses <- sweep(child1_net, 1, as.numeric(child1_in_tu1), `*`)
  if (n_children == 2) {
    tu1_expenses <- tu1_expenses + sweep(child2_net, 1, as.numeric(child2_in_tu1), `*`)
  }

  # Number of children in TU1
  tu1_n_children <- as.numeric(child1_in_tu1) + if (n_children == 2) as.numeric(child2_in_tu1) else 0

  # Earned income limit for TU1
  # For MFJ (filing_status1 == 2): min(earnings1, earnings2)
  # For Single/HoH: earnings1
  is_mfj1 <- (parent_units_df$filing_status1 == 2)
  tu1_ei_limit <- matrix(NA_real_, nrow = n_units, ncol = n_choices)

  for (emp in c('none', 'pt', 'ft')) {
    choice_mask <- employment_choices == emp
    e1 <- pmax(0, parent_units_df[[paste0('earnings1.', emp)]])
    e2_raw <- parent_units_df[[paste0('earnings2.', emp)]]
    e2 <- pmax(0, ifelse(is.na(e2_raw), Inf, e2_raw))

    # MFJ uses min of both, others use own earnings
    ei <- ifelse(is_mfj1, pmin(e1, e2), e1)
    tu1_ei_limit[, choice_mask] <- ei
  }

  # AGI for TU1: for married couples use combined, for cohabiting use agi1
  tu1_agi <- ifelse(
    matrix(rep(is_cohabiting, n_choices), nrow = n_units, ncol = n_choices),
    agi1_matrix,
    agi_matrix
  )

  # Income tax for non-refundable cap
  tu1_iit <- ifelse(
    matrix(rep(is_cohabiting, n_choices), nrow = n_units, ncol = n_choices),
    iit1_matrix,
    iit_matrix
  )

  # Filing-status-dependent threshold matrices for TU1
  # MFJ filers get MFJ thresholds; single/HoH get single thresholds
  tu1_thresh_add1 <- matrix(
    rep(ifelse(is_mfj1, po_thresh_add1_mfj, po_thresh_add1_single), n_choices),
    nrow = n_units, ncol = n_choices
  )
  tu1_thresh_add2 <- matrix(
    rep(ifelse(is_mfj1, po_thresh_add2_mfj, po_thresh_add2_single), n_choices),
    nrow = n_units, ncol = n_choices
  )
  tu1_step_add2 <- matrix(
    rep(ifelse(is_mfj1, discrete_step_add2_mfj, discrete_step_add2_single), n_choices),
    nrow = n_units, ncol = n_choices
  )
  tu1_porate_add2 <- matrix(
    rep(ifelse(is_mfj1, po_rate_add2_mfj, po_rate_add2_single), n_choices),
    nrow = n_units, ncol = n_choices
  )

  # Calculate TU1 CDCTC
  tu1_cdctc <- calculate_cdctc_obbba_for_tu(
    expenses = tu1_expenses,
    n_children = tu1_n_children,
    agi = tu1_agi,
    ei_limit = tu1_ei_limit,
    iit = tu1_iit,
    exp_limit_per_child = exp_limit_per_child,
    n_dep_limit = n_dep_limit,
    rate_base = rate_base,
    rate_add1 = rate_add1,
    thresh_add1 = tu1_thresh_add1,
    po_rate_add1 = po_rate_add1,
    discrete_step_add1 = discrete_step_add1,
    rate_add2 = rate_add2,
    thresh_add2 = tu1_thresh_add2,
    po_rate_add2 = tu1_porate_add2,
    discrete_step_add2 = tu1_step_add2,
    refundable = refundable
  )

  #---------------------------
  # Tax Unit 2 CDCTC calculation (cohabiting only)
  #---------------------------

  # Expenses assigned to TU2
  tu2_expenses <- sweep(child1_net, 1, as.numeric(child1_in_tu2), `*`)
  if (n_children == 2) {
    tu2_expenses <- tu2_expenses + sweep(child2_net, 1, as.numeric(child2_in_tu2), `*`)
  }

  # Number of children in TU2
  tu2_n_children <- as.numeric(child1_in_tu2) + if (n_children == 2) as.numeric(child2_in_tu2) else 0

  # Earned income limit for TU2 (cohabiting parent files separately, not MFJ)
  # So they just use their own earnings
  tu2_ei_limit <- earnings2_matrix
  tu2_ei_limit <- pmax(0, replace(tu2_ei_limit, is.na(tu2_ei_limit), 0))

  # TU2 always files Single/HoH (cohabiting = separate tax units)
  tu2_thresh_add1 <- matrix(po_thresh_add1_single, nrow = n_units, ncol = n_choices)
  tu2_thresh_add2 <- matrix(po_thresh_add2_single, nrow = n_units, ncol = n_choices)
  tu2_step_add2 <- matrix(discrete_step_add2_single, nrow = n_units, ncol = n_choices)
  tu2_porate_add2 <- matrix(po_rate_add2_single, nrow = n_units, ncol = n_choices)

  # Calculate TU2 CDCTC (only for cohabiting couples with children in TU2)
  tu2_cdctc <- calculate_cdctc_obbba_for_tu(
    expenses = tu2_expenses,
    n_children = tu2_n_children,
    agi = agi2_matrix,
    ei_limit = tu2_ei_limit,
    iit = iit2_matrix,
    exp_limit_per_child = exp_limit_per_child,
    n_dep_limit = n_dep_limit,
    rate_base = rate_base,
    rate_add1 = rate_add1,
    thresh_add1 = tu2_thresh_add1,
    po_rate_add1 = po_rate_add1,
    discrete_step_add1 = discrete_step_add1,
    rate_add2 = rate_add2,
    thresh_add2 = tu2_thresh_add2,
    po_rate_add2 = tu2_porate_add2,
    discrete_step_add2 = tu2_step_add2,
    refundable = refundable
  )

  # Zero out TU2 for non-cohabiting units
  tu2_cdctc <- sweep(tu2_cdctc, 1, as.numeric(is_cohabiting), `*`)

  #---------------------------
  # Combine and return
  #---------------------------

  cdctc_matrix <- tu1_cdctc + tu2_cdctc

  # Apply take-up rate
  cdctc_matrix <- cdctc_matrix * takeup

  # Ensure return is a proper matrix
  if (!is.matrix(cdctc_matrix)) {
    cdctc_matrix <- matrix(cdctc_matrix, nrow = n_units, ncol = n_choices)
  }

  return(cdctc_matrix)
}



calculate_cdctc_obbba_for_tu <- function(expenses, n_children, agi, ei_limit, iit,
                                          exp_limit_per_child, n_dep_limit,
                                          rate_base,
                                          rate_add1, thresh_add1, po_rate_add1, discrete_step_add1,
                                          rate_add2, thresh_add2, po_rate_add2, discrete_step_add2,
                                          refundable) {
  #----------------------------------------------------------------------------
  # Calculate OBBBA CDCTC for a single tax unit using two-tier phase-out.
  #
  # The OBBBA CDCTC has three rate components:
  #   rate_base  (20%): never phases out
  #   rate_add1  (15%): top tier (50% -> 35%), phases out first
  #   rate_add2  (15%): middle tier (35% -> 20%), phases out second
  #
  # Parameters:
  #   - expenses (matrix): n_units x n_choices care expenses for this TU
  #   - n_children (numeric): number of children in this TU (per unit)
  #   - agi (matrix): n_units x n_choices AGI for this TU
  #   - ei_limit (matrix): n_units x n_choices earned income limit
  #   - iit (matrix): n_units x n_choices income tax liability
  #   - exp_limit_per_child (dbl): expense cap per qualifying child
  #   - n_dep_limit (int): max qualifying children
  #   - rate_base (dbl): base credit rate (scalar)
  #   - rate_add1 (dbl): tier 1 additional rate (scalar)
  #   - thresh_add1 (matrix): tier 1 phase-out threshold (filing-status-dependent)
  #   - po_rate_add1 (dbl): tier 1 rate reduction per dollar (scalar)
  #   - discrete_step_add1 (dbl): tier 1 rounding step (scalar)
  #   - rate_add2 (dbl): tier 2 additional rate (scalar)
  #   - thresh_add2 (matrix): tier 2 phase-out threshold (filing-status-dependent)
  #   - po_rate_add2 (matrix): tier 2 rate reduction per dollar (filing-status-dependent)
  #   - discrete_step_add2 (matrix): tier 2 rounding step (filing-status-dependent)
  #   - refundable (logical): whether credit is refundable
  #
  # Returns:
  #   matrix (n_units x n_choices) of CDCTC credit amounts
  #----------------------------------------------------------------------------

  n_units <- nrow(expenses)
  n_choices <- ncol(expenses)

  # Helper to preserve matrix dims after pmin/pmax
  ensure_matrix <- function(x) {
    if (!is.matrix(x)) dim(x) <- c(n_units, n_choices)
    x
  }

  # Cap 1: Per-child expense limit (vectorized over units)
  effective_children <- pmin(n_children, n_dep_limit)
  expense_cap <- matrix(rep(effective_children * exp_limit_per_child, n_choices),
                        nrow = n_units, ncol = n_choices)

  # Cap 2: Earned income limit
  qual_exp <- ensure_matrix(pmin(expenses, expense_cap))
  qual_exp <- ensure_matrix(pmin(qual_exp, ei_limit))
  qual_exp <- ensure_matrix(pmax(qual_exp, 0))

  # Handle NA AGI (TU2 doesn't exist for single parents)
  agi_safe <- replace(agi, is.na(agi), 0)

  # Tier 1 phase-out: 50% -> 35%
  excess_add1 <- ensure_matrix(pmax(agi_safe - thresh_add1, 0))
  excess_add1_rounded <- ceiling(excess_add1 / discrete_step_add1) * discrete_step_add1
  rate_add1_phased <- ensure_matrix(pmax(rate_add1 - excess_add1_rounded * po_rate_add1, 0))

  # Tier 2 phase-out: 35% -> 20%
  excess_add2 <- ensure_matrix(pmax(agi_safe - thresh_add2, 0))
  excess_add2_rounded <- ceiling(excess_add2 / discrete_step_add2) * discrete_step_add2
  rate_add2_phased <- ensure_matrix(pmax(rate_add2 - excess_add2_rounded * po_rate_add2, 0))

  # Combined credit rate
  credit_rate <- rate_base + rate_add1_phased + rate_add2_phased

  # Calculate credit
  cdctc <- qual_exp * credit_rate
  if (!is.matrix(cdctc)) dim(cdctc) <- c(n_units, n_choices)

  # Apply non-refundability: cap by income tax liability
  if (!refundable) {
    iit_safe <- ensure_matrix(pmax(replace(iit, is.na(iit), 0), 0))
    cdctc <- ensure_matrix(pmin(cdctc, iit_safe))
  }

  # Zero out for units with no children in this TU
  no_children_mask <- (n_children == 0)
  cdctc <- sweep(cdctc, 1, as.numeric(!no_children_mask), `*`)

  return(cdctc)
}


