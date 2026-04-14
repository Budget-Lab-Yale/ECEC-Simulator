#------------------------------------------------------------------------------
# prepare_donor_pool.R
#
# Prepares tax donor pools from Tax-Simulator output, including AGI
# percentile thresholding and EMTR interpolation helpers.
#
# Functions:
#   - prepare_donor_pool(): Main function for building donor pools
#   - compute_agi_percentile_thresholds(): Computes AGI dollar thresholds
#   - get_agi_pct_bin(): Assigns AGI values to percentile bins
#   - interpolate_emtr_nas(): Interpolates NA values in EMTR schedules
#
# Called from: 2a (run_calibration), 3b (run_simulation_year)
#------------------------------------------------------------------------------



prepare_donor_pool <- function(tax_sim_output_path, year) {

  #----------------------------------------------------------------------------
  # Prepares the donor pool from Tax-Simulator microdata
  #
  # Keeps individual tax records with their actual ETR and full EMTR schedule,
  # rather than aggregating to group means. Uses percentile-based AGI binning
  # which is stable over time (no inflation adjustment needed).
  #
  # Params:
  #   - tax_sim_output_path (chr): Path to Tax-Simulator detail output file
  #   - year (int): Tax year
  #
  # Returns: list with two elements:
  #   - donors: tibble with donor records containing:
  #       - donor_id: unique identifier
  #       - pct_bin, joint, n_dep_u13, has_dep: matching variables
  #       - n_persons: family size (1-5, capped)
  #       - filer: 1 if tax filer, 0 otherwise
  #       - etr: actual effective tax rate
  #       - mtr_wages_p*: full EMTR schedule (31 columns)
  #       - weight: survey weight
  #   - pct_thresholds: named vector mapping percentile breaks to dollar amounts
  #----------------------------------------------------------------------------

  # Read Tax-Simulator output
  tax_data <- tax_sim_output_path %>%
    fread() %>%
    tibble()

  # Filter to non-dependents
  tax_data <- tax_data %>% filter(dep_status == 0)

  # Compute percentile thresholds from the data
  pct_thresholds <- compute_agi_percentile_thresholds(tax_data$agi, tax_data$weight)

  # Prepare donor pool with percentile bins
  donors <- tax_data %>%
    mutate(
      donor_id = row_number(),

      # Recompute: count only deps age <= 12 from dep_age1, dep_age2, dep_age3
      n_dep_u13 = pmin(3,
        (!is.na(dep_age1) & dep_age1 <= 12) +
        (!is.na(dep_age2) & dep_age2 <= 12) +
        (!is.na(dep_age3) & dep_age3 <= 12)
      ),

      # Calculate family size (for median income lookups)
      # 1 (primary) + 1 (if joint) + dependents < 18
      n_persons = 1 + as.integer(filing_status == 2) + pmin(3,
        (!is.na(dep_age1) & dep_age1 < 18) +
        (!is.na(dep_age2) & dep_age2 < 18) +
        (!is.na(dep_age3) & dep_age3 < 18)
      ),

      # Calculate ETR
      etr = liab_iit_net / agi,
      etr = if_else(agi <= 0 | is.na(etr) | is.infinite(etr), 0, etr),
      etr = pmin(pmax(etr, -1), 2),  # Cap ETR between -100% and 200%

      # Assign percentile-based AGI bins
      pct_bin = get_agi_pct_bin(agi, pct_thresholds),

      # Create grouping variables
      joint = as.integer(filing_status == 2),
      has_dep = as.integer(n_dep_u13 > 0)
    ) %>%
    select(
      donor_id, agi, pct_bin, joint, n_dep_u13, has_dep, n_persons, filer, etr, weight,
      all_of(MTR_COLS)
    )

  # Interpolate any NA values in EMTR schedules
  donors <- interpolate_emtr_nas(donors)

  return(list(
    donors = donors,
    pct_thresholds = pct_thresholds
  ))
}



compute_agi_percentile_thresholds <- function(agi, weights) {

  #----------------------------------------------------------------------------
  # Computes AGI dollar thresholds corresponding to percentile breaks
  #
  # Uses weighted quantiles to find the AGI value at each percentile cutpoint.
  # Only considers positive AGI values; negative/zero AGI is handled separately.
  #
  # Params:
  #   - agi (num vec): AGI values
  #   - weights (num vec): Survey weights
  #
  # Returns: (num vec) Named vector mapping percentile breaks to dollar thresholds
  #----------------------------------------------------------------------------

  # Filter to positive AGI only for percentile calculation
  pos_mask <- agi > 0
  agi_pos <- agi[pos_mask]
  weights_pos <- weights[pos_mask]

  # Sort by AGI
  ord <- order(agi_pos)
  agi_sorted <- agi_pos[ord]
  weights_sorted <- weights_pos[ord]

  # Compute cumulative weight distribution
  cum_weights <- cumsum(weights_sorted) / sum(weights_sorted)

  # Find AGI at each percentile break
  thresholds <- sapply(AGI_PERCENTILE_BREAKS / 100, function(p) {
    if (p == 0) return(0)
    if (p >= 1) return(Inf)
    idx <- which(cum_weights >= p)[1]
    if (is.na(idx)) return(Inf)
    agi_sorted[idx]
  })

  names(thresholds) <- AGI_PERCENTILE_BREAKS
  return(thresholds)
}



get_agi_pct_bin <- function(agi, pct_thresholds) {

  #----------------------------------------------------------------------------
  # Assigns AGI values to percentile bins
  #
  # Uses pre-computed dollar thresholds corresponding to percentile breaks.
  # This approach is stable over time since percentile ranks don't need
  # inflation adjustment.
  #
  # Params:
  #   - agi (num vec): Adjusted gross income values
  #   - pct_thresholds (num vec): Named vector from compute_agi_percentile_thresholds()
  #
  # Returns: (num vec) Percentile bin values (0, 2, 4, ..., 100)
  #----------------------------------------------------------------------------

  # Use findInterval to find which bin each AGI falls into
  idx <- findInterval(agi, pct_thresholds, left.open = TRUE)

  # Clamp to valid range

  idx_clamped <- pmax(1L, pmin(idx, length(AGI_PERCENTILE_BREAKS)))

  # Get percentile bin values
  result <- AGI_PERCENTILE_BREAKS[idx_clamped]

  # Handle edge cases: negative AGI gets special bin -1
  result[agi <= 0] <- -1

  return(result)
}



interpolate_emtr_nas <- function(donors) {

  #----------------------------------------------------------------------------
  # Interpolates NA values in EMTR schedules
  #
  # For p0 (zero wages): NA -> 0
  # For other percentiles: NA -> average of neighboring percentiles
  # For p100 (highest): NA -> use p99.99 value
  #
  # Params:
  #   - donors (df): Donor pool with mtr_wages_p* columns
  #
  # Returns: (df) Donor pool with NAs interpolated
  #----------------------------------------------------------------------------

  n_cols <- length(MTR_COLS)
  na_nonzero_wage <- 0

  # Process each EMTR column
  for (i in seq_along(MTR_COLS)) {
    col <- MTR_COLS[i]
    na_mask <- is.na(donors[[col]])

    if (!any(na_mask)) next

    if (i == 1) {
      # p0 (zero wages): set NA to 0
      donors[[col]][na_mask] <- 0
    } else if (i == n_cols) {
      # p100 (last column): use previous column value
      prev_col <- MTR_COLS[i - 1]
      donors[[col]][na_mask] <- donors[[prev_col]][na_mask]
      na_nonzero_wage <- na_nonzero_wage + sum(na_mask)
    } else {
      # Middle columns: average of neighbors
      prev_col <- MTR_COLS[i - 1]
      next_col <- MTR_COLS[i + 1]
      donors[[col]][na_mask] <- (donors[[prev_col]][na_mask] + donors[[next_col]][na_mask]) / 2
      na_nonzero_wage <- na_nonzero_wage + sum(na_mask)
    }
  }

  # Note: na_nonzero_wage interpolations handled silently

  return(donors)
}
