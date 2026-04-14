#------------------------------------------------------------------------------
# compute_price_wedge_cache.R
#
# Computes price heterogeneity (price_wedge) cache for all parent units.
#
# Called from: 2a (run_calibration), 3a (initialize_simulation)
#------------------------------------------------------------------------------



compute_price_wedge_cache <- function(parent_units, children, households, qrf_models) {

  #----------------------------------------------------------------------------
  # Computes price heterogeneity (price_wedge) cache for all parent units.
  #
  # This function draws price_wedge values ONCE at initialization, creating a cache
  # that persists across simulation years. This ensures each household has
  # a stable price_wedge that doesn't change year-to-year.
  #
  # Draws 4 separate price_wedge values:
  #   - price_wedge.center_low: for low-priced center-based care (sector 2)
  #   - price_wedge.center_high: for high-priced center-based care (sector 3)
  #   - price_wedge.home: for paid home-based care (sector 4)
  #   - price_wedge.other_paid: for Other Paid care (non-market sector)
  #
  # Uses baseline AGI for income percentile calculation rather than
  # employment-choice-specific AGI (which would require full tax imputation).
  # Computes percentile thresholds internally to match QRF training (1:100 bins).
  #
  # Params:
  #   - parent_units (tibble): base parent units with baseline_agi1, baseline_agi2
  #   - children (tibble): children data with ages
  #   - households (tibble): households data with region
  #   - qrf_models (list): trained QRF models from train_price_qrf_models()
  #       with center_low_price, center_high_price, home_price, other_paid_price models
  #
  # Returns: tibble with (hh_id, parent_unit_id, price_wedge.center_low, price_wedge.center_high, price_wedge.home, price_wedge.other_paid)
  #----------------------------------------------------------------------------

  # Set seed for reproducible random operations
  set.seed(random_seed_base + 2)


  # Get one row per parent unit (deduplicate from employment choice expansion)
  parent_units_unique <- parent_units %>%
    distinct(hh_id, parent_unit_id, .keep_all = TRUE)

  # Calculate mean child age per parent unit
  child_ages <- children %>%
    filter(age < 5) %>%
    group_by(hh_id, parent_unit_id) %>%
    summarise(child_age = mean(age, na.rm = TRUE), .groups = 'drop')

  # Join parent units with child ages and region
  # Use averaged parent weights (consistent with fiscal_cost.R, distributional_impact.R)
  cache_df <- parent_units_unique %>%
    mutate(weight = (per_weight1 + coalesce(per_weight2, per_weight1)) / 2) %>%
    select(hh_id, parent_unit_id, n_parents, baseline_agi1, baseline_agi2, weight) %>%
    left_join(child_ages, by = c('hh_id', 'parent_unit_id')) %>%
    left_join(households %>% select(hh_id, region), by = 'hh_id') %>%
    mutate(
      # Use sum of baseline AGI for income percentile
      baseline_agi = baseline_agi1 + replace_na(baseline_agi2, 0)
    )

  # Compute income percentile thresholds from the data (99 values for 100 bins)
  # This matches the QRF training which used seq(0.01, 0.99, 0.01)
  income_pctiles <- wtd.quantile(
    x      = cache_df$baseline_agi + runif(nrow(cache_df)),  # small jitter for ties
    weight = cache_df$weight,
    probs  = seq(0.01, 0.99, 0.01)
  )

  cache_df <- cache_df %>%
    mutate(
      income_pctile = cut(baseline_agi, c(-Inf, income_pctiles, Inf), labels = 1:100) %>%
        as.character() %>% as.integer(),
      region_factor = factor(region, levels = qrf_models$center_low_price$region_levels)
    )

  n_total <- nrow(cache_df)

  # Identify complete cases for imputation
  complete_mask <- !is.na(cache_df$income_pctile) &
                   !is.na(cache_df$region_factor) &
                   !is.na(cache_df$n_parents) &
                   !is.na(cache_df$child_age)

  n_complete <- sum(complete_mask)

  # Initialize price_wedge columns with NA (will be filled for complete cases only)
  cache_df$price_wedge.center_low <- NA_real_
  cache_df$price_wedge.center_high <- NA_real_
  cache_df$price_wedge.home <- NA_real_
  cache_df$price_wedge.other_paid <- NA_real_

  if (n_complete > 0) {
    # Prepare prediction data frame for complete cases only
    pred_df <- cache_df[complete_mask, ] %>%
      select(income_pctile, region = region_factor, n_parents, child_age)

    # Draw a SINGLE random percentile for each parent unit (consistent across care types)
    # This ensures that if a family is at the 80th percentile for center prices,
    # they're also at the 80th percentile for home prices (same underlying factors)
    quantile_levels <- seq(0.01, 0.99, 0.01)
    drawn_pctile_idx <- sample(1:99, n_complete, replace = TRUE)

    # Get full quantile grid from each model (n_complete x 99 matrix)
    # Then extract each row's value at its drawn percentile
    q_center_low  <- predict(qrf_models$center_low_price, data = pred_df, type = 'quantiles', quantiles = quantile_levels)$predictions
    q_center_high <- predict(qrf_models$center_high_price, data = pred_df, type = 'quantiles', quantiles = quantile_levels)$predictions
    q_home        <- predict(qrf_models$home_price, data = pred_df, type = 'quantiles', quantiles = quantile_levels)$predictions
    q_other_paid  <- predict(qrf_models$other_paid_price, data = pred_df, type = 'quantiles', quantiles = quantile_levels)$predictions

    # Extract the drawn percentile for each parent unit
    price_wedge.center_low  <- q_center_low[cbind(1:n_complete, drawn_pctile_idx)]
    price_wedge.center_high <- q_center_high[cbind(1:n_complete, drawn_pctile_idx)]
    price_wedge.home        <- q_home[cbind(1:n_complete, drawn_pctile_idx)]
    price_wedge.other_paid  <- q_other_paid[cbind(1:n_complete, drawn_pctile_idx)]

    # Clip to reasonable bounds
    price_wedge.center_low  <- pmax(0.1, pmin(5.0, price_wedge.center_low))
    price_wedge.center_high <- pmax(0.1, pmin(5.0, price_wedge.center_high))
    price_wedge.home        <- pmax(0.1, pmin(5.0, price_wedge.home))
    price_wedge.other_paid  <- pmax(0.1, pmin(5.0, price_wedge.other_paid))

    # Assign to complete cases
    cache_df$price_wedge.center_low[complete_mask]  <- price_wedge.center_low
    cache_df$price_wedge.center_high[complete_mask] <- price_wedge.center_high
    cache_df$price_wedge.home[complete_mask]        <- price_wedge.home
    cache_df$price_wedge.other_paid[complete_mask]  <- price_wedge.other_paid
  }


  # Return only the columns needed for joining
  cache_df %>%
    select(hh_id, parent_unit_id, price_wedge.center_low, price_wedge.center_high, price_wedge.home, price_wedge.other_paid)
}
