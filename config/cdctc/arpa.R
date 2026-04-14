#------------------------------------------------------------------------------
# arpa.R
#
# ARPA CDCTC (Child and Dependent Care Tax Credit) - 2021 expansion parameters.
#
# The American Rescue Plan Act of 2021 temporarily expanded the CDCTC:
#   - Credit rate: 50% for AGI <= $125k, phasing to 20% by $440k
#   - Expense caps: $8k per child, max 2 children ($16k)
#   - Earned income limit: lesser of two spouses' earnings (for MFJ)
#   - FULLY REFUNDABLE (not limited by tax liability)
#   - Discrete $2,000 rounding for phase-out calculation
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
  # Computes CDCTC credit for each choice under ARPA 2021 expansion.
  #
  # For married couples (same tax_unit_id), calculates CDCTC using:
  #   - Joint AGI (agi_matrix, which is agi1 + agi2)
  #   - min(earnings1, earnings2) as earned income limit
  #
  # For cohabiting couples (different tax_unit_ids), calculates separately:
  #   - Each tax unit uses its own AGI
  #   - Children's expenses assigned to claiming parent's tax unit
  #   - Each filer uses own earnings as EI limit (not MFJ so no spouse rule)
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
  # ARPA parameters in 2026 base-year dollars
  #---------------------------

  exp_limit_per_child_2026 <- 8000    # Max qualified expenses per child (vs $3k)
  n_dep_limit         <- 2       # Max qualifying children
  rate_base           <- 0.20    # Base credit rate (phases out above $400k)
  rate_additional     <- 0.30    # Additional rate (phases out above $125k)
  po_thresh_base_2026 <- 400000  # AGI threshold for base rate phase-out
  po_thresh_add_2026  <- 125000  # AGI threshold for additional rate phase-out
  po_rate             <- 0.000005 # Rate reduction per dollar above threshold
  discrete_step_2026  <- 2000    # Rounding step for phase-out
  refundable          <- TRUE    # ARPA made it fully refundable
  takeup              <- 1.0     # Take-up rate (1.0 = 100%)


  #---------------------------
  # Setup and inflation indexing
  #---------------------------

  n_units <- nrow(parent_units_df)
  n_choices <- nrow(catalog)
  employment_choices <- catalog$employment_choice

  # Convert ARPA dollar parameters from 2026 base-year dollars to simulation-year dollars.
  # Uses chained CPI factors dedicated to ARPA:
  # cpi_chain_factor_2019 = chained CPI(sim_year) / chained CPI(2019)
  # cpi_chain_factor_2026 = chained CPI(2026) / chained CPI(2019)
  # Inflation factor from 2026 -> sim year = cpi_chain_factor_2019 / cpi_chain_factor_2026
  extract_single_positive <- function(x, default_value) {
    if (is.null(x)) return(default_value)
    x_unique <- unique(x)
    x_unique <- x_unique[is.finite(x_unique) & x_unique > 0]
    if (length(x_unique) != 1) return(default_value)
    x_unique[1]
  }

  cpi_chain_factor_2019 <- extract_single_positive(parent_units_df[['cpi_chain_factor_2019']], 1.0)
  cpi_chain_factor_2026 <- extract_single_positive(parent_units_df[['cpi_chain_factor_2026']], cpi_chain_factor_2019)
  inflation_factor <- cpi_chain_factor_2019 / cpi_chain_factor_2026
  if (!is.finite(inflation_factor) || inflation_factor <= 0) inflation_factor <- 1.0

  exp_limit_per_child <- round(exp_limit_per_child_2026 * inflation_factor)
  po_thresh_base <- round(po_thresh_base_2026 * inflation_factor)
  po_thresh_add <- round(po_thresh_add_2026 * inflation_factor)
  discrete_step <- max(1, round(discrete_step_2026 * inflation_factor))

  # Validate required tax-unit-level data is present
  if (is.null(agi1_matrix) || is.null(child1_cost_matrix)) {
    stop('CDCTC calculation requires tax-unit-level data (agi1_matrix, child1_cost_matrix)')
  }

  #---------------------------
  # Identify married vs cohabiting
  #---------------------------

  if ('tax_unit_id2' %in% names(parent_units_df)) {
    is_cohabiting <- !is.na(parent_units_df$tax_unit_id2) &
                     (parent_units_df$tax_unit_id1 != parent_units_df$tax_unit_id2)
  } else {
    is_cohabiting <- rep(FALSE, n_units)
  }

  #---------------------------
  # Attribute expenses to tax units
  #---------------------------

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

  # Net per-child costs after subsidy
  total_cost <- gross_ecec_cost_matrix
  child1_share <- ifelse(total_cost > 0, child1_cost_matrix / total_cost, 0.5)
  child2_share <- ifelse(total_cost > 0, child2_cost_matrix / total_cost, 0.5)

  child1_net <- child1_cost_matrix - subsidy_matrix * child1_share
  child2_net <- child2_cost_matrix - subsidy_matrix * child2_share

  # Floor at 0 - preserve matrix dimensions
  child1_net <- pmax(child1_net, 0)
  child2_net <- pmax(child2_net, 0)
  if (!is.matrix(child1_net)) dim(child1_net) <- c(n_units, n_choices)
  if (!is.matrix(child2_net)) dim(child2_net) <- c(n_units, n_choices)

  #---------------------------
  # Tax Unit 1 CDCTC calculation
  #---------------------------

  tu1_expenses <- sweep(child1_net, 1, as.numeric(child1_in_tu1), `*`)
  if (n_children == 2) {
    tu1_expenses <- tu1_expenses + sweep(child2_net, 1, as.numeric(child2_in_tu1), `*`)
  }

  tu1_n_children <- as.numeric(child1_in_tu1) + if (n_children == 2) as.numeric(child2_in_tu1) else 0

  # Earned income limit for TU1
  is_mfj1 <- (parent_units_df$filing_status1 == 2)
  tu1_ei_limit <- matrix(NA_real_, nrow = n_units, ncol = n_choices)

  for (emp in c('none', 'pt', 'ft')) {
    choice_mask <- employment_choices == emp
    e1 <- pmax(0, parent_units_df[[paste0('earnings1.', emp)]])
    e2_raw <- parent_units_df[[paste0('earnings2.', emp)]]
    e2 <- pmax(0, ifelse(is.na(e2_raw), Inf, e2_raw))
    ei <- ifelse(is_mfj1, pmin(e1, e2), e1)
    tu1_ei_limit[, choice_mask] <- ei
  }

  # AGI for TU1
  tu1_agi <- ifelse(
    matrix(rep(is_cohabiting, n_choices), nrow = n_units, ncol = n_choices),
    agi1_matrix,
    agi_matrix
  )

  tu1_iit <- ifelse(
    matrix(rep(is_cohabiting, n_choices), nrow = n_units, ncol = n_choices),
    iit1_matrix,
    iit_matrix
  )

  tu1_cdctc <- calculate_cdctc_arpa_for_tu(
    expenses = tu1_expenses,
    n_children = tu1_n_children,
    agi = tu1_agi,
    ei_limit = tu1_ei_limit,
    iit = tu1_iit,
    exp_limit_per_child = exp_limit_per_child,
    n_dep_limit = n_dep_limit,
    rate_base = rate_base,
    rate_additional = rate_additional,
    po_thresh_base = po_thresh_base,
    po_thresh_add = po_thresh_add,
    po_rate = po_rate,
    discrete_step = discrete_step,
    refundable = refundable
  )

  #---------------------------
  # Tax Unit 2 CDCTC calculation (cohabiting only)
  #---------------------------

  tu2_expenses <- sweep(child1_net, 1, as.numeric(child1_in_tu2), `*`)
  if (n_children == 2) {
    tu2_expenses <- tu2_expenses + sweep(child2_net, 1, as.numeric(child2_in_tu2), `*`)
  }

  tu2_n_children <- as.numeric(child1_in_tu2) + if (n_children == 2) as.numeric(child2_in_tu2) else 0

  tu2_ei_limit <- pmax(0, replace(earnings2_matrix, is.na(earnings2_matrix), 0))

  tu2_cdctc <- calculate_cdctc_arpa_for_tu(
    expenses = tu2_expenses,
    n_children = tu2_n_children,
    agi = agi2_matrix,
    ei_limit = tu2_ei_limit,
    iit = iit2_matrix,
    exp_limit_per_child = exp_limit_per_child,
    n_dep_limit = n_dep_limit,
    rate_base = rate_base,
    rate_additional = rate_additional,
    po_thresh_base = po_thresh_base,
    po_thresh_add = po_thresh_add,
    po_rate = po_rate,
    discrete_step = discrete_step,
    refundable = refundable
  )

  tu2_cdctc <- sweep(tu2_cdctc, 1, as.numeric(is_cohabiting), `*`)

  #---------------------------
  # Combine and return
  #---------------------------

  cdctc_matrix <- tu1_cdctc + tu2_cdctc
  cdctc_matrix <- cdctc_matrix * takeup

  if (!is.matrix(cdctc_matrix)) {
    cdctc_matrix <- matrix(cdctc_matrix, nrow = n_units, ncol = n_choices)
  }

  return(cdctc_matrix)
}



calculate_cdctc_arpa_for_tu <- function(expenses, n_children, agi, ei_limit, iit,
                                         exp_limit_per_child, n_dep_limit,
                                         rate_base, rate_additional,
                                         po_thresh_base, po_thresh_add,
                                         po_rate, discrete_step, refundable) {
  #----------------------------------------------------------------------------
  # Calculate ARPA CDCTC for a single tax unit.
  # Uses two-tier phase-out structure.
  #----------------------------------------------------------------------------

  n_units <- nrow(expenses)
  n_choices <- ncol(expenses)

  # Helper to preserve matrix dims after pmin/pmax
  ensure_matrix <- function(x) {
    if (!is.matrix(x)) dim(x) <- c(n_units, n_choices)
    x
  }

  effective_children <- pmin(n_children, n_dep_limit)
  expense_cap <- matrix(rep(effective_children * exp_limit_per_child, n_choices),
                        nrow = n_units, ncol = n_choices)

  qual_exp <- ensure_matrix(pmin(expenses, expense_cap))
  qual_exp <- ensure_matrix(pmin(qual_exp, ei_limit))
  qual_exp <- ensure_matrix(pmax(qual_exp, 0))

  agi_safe <- replace(agi, is.na(agi), 0)

  # ARPA two-tier phase-out
  excess_agi_add <- ensure_matrix(pmax(agi_safe - po_thresh_add, 0))
  excess_agi_add_rounded <- ceiling(excess_agi_add / discrete_step) * discrete_step
  rate_add_phased <- ensure_matrix(pmax(rate_additional - excess_agi_add_rounded * po_rate, 0))

  excess_agi_base <- ensure_matrix(pmax(agi_safe - po_thresh_base, 0))
  excess_agi_base_rounded <- ceiling(excess_agi_base / discrete_step) * discrete_step
  rate_base_phased <- ensure_matrix(pmax(rate_base - excess_agi_base_rounded * po_rate, 0))

  credit_rate <- rate_base_phased + rate_add_phased

  cdctc <- qual_exp * credit_rate
  if (!is.matrix(cdctc)) dim(cdctc) <- c(n_units, n_choices)

  if (!refundable) {
    iit_safe <- ensure_matrix(pmax(replace(iit, is.na(iit), 0), 0))
    cdctc <- ensure_matrix(pmin(cdctc, iit_safe))
  }

  no_children_mask <- (n_children == 0)
  cdctc <- sweep(cdctc, 1, as.numeric(!no_children_mask), `*`)

  return(cdctc)
}



