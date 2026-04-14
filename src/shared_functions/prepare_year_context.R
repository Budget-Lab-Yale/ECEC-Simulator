prepare_year_context <- function(sim_base_hh, macro_projections, base_supply_params,
                                  base_demand_params, baseline_info, cached_price_wedge,
                                  year, aging_base_year, seed_offset) {

  #----------------------------------------------------------------------------
  # Prepares all year-level data: age microdata, growth factors, tax params,
  # parent units, simulation variables. Shared by 3b and slurm year handlers.
  #
  # Params:
  #   - sim_base_hh (list): Base household data from initialization
  #   - macro_projections (df): Macro projection table
  #   - base_supply_params (list): Base supply-side parameters
  #   - base_demand_params (list): Base demand-side parameters
  #   - baseline_info (list): Baseline scenario info (paths, options, law)
  #   - cached_price_wedge (df): Pre-computed price wedge cache
  #   - year (int): Simulation year
  #   - aging_base_year (int): Base year for aging microdata
  #   - seed_offset (int): Seed offset for random number generation
  #
  # Returns: (list) supply_params, demand_params, parent_units_list,
  #   donor_pool, tax_sim_path, aged_spm, aged_spm_parent_earnings,
  #   median_income_lookup, year_seed
  #----------------------------------------------------------------------------

  get_chained_cpi_growth_factor <- function(macro_projections, base_year, target_year) {

    #--------------------------------------------------------------------------
    # Computes chained CPI-U growth factor between two years.
    # Tries ccpiu_irs first, falls back to ccpiu column.
    #
    # Params:
    #   - macro_projections (df): Macro projection table
    #   - base_year (int): Base year
    #   - target_year (int): Target year
    #
    # Returns: (dbl) Chained CPI growth factor
    #--------------------------------------------------------------------------

    if (target_year <= base_year) return(1.0)

    candidate_cols <- c('ccpiu_irs', 'ccpiu')
    col <- candidate_cols[candidate_cols %in% names(macro_projections)][1]

    if (is.na(col) || is.null(col)) {
      stop('get_chained_cpi_growth_factor: no chained CPI column found. Expected one of: ',
           paste(candidate_cols, collapse = ', '))
    }

    get_macro_ratio(macro_projections, base_year, target_year, col, 'get_chained_cpi_growth_factor')
  }

  # Age data
  aged_hh <- age_microdata_to_year(sim_base_hh, macro_projections, year, aging_base_year)

  aged_spm <- NULL
  aged_spm_parent_earnings <- NULL
  if (!is.null(sim_base_hh$spm_units)) {
    aged_spm <- age_spm_units(
      sim_base_hh$spm_units, macro_projections, year, aging_base_year
    )
    if (!is.null(sim_base_hh$spm_parent_earnings)) {
      aged_spm_parent_earnings <- age_spm_parent_earnings(
        sim_base_hh$spm_parent_earnings, macro_projections, year, aging_base_year
      )
    }
  }

  # Supply params with growth factors
  supply_params <- base_supply_params
  supply_params$nominal_wage_factor <- get_hourly_wage_growth_factor(macro_projections, 2019, year)
  supply_params$cpi_factor <- get_cpi_growth_factor(macro_projections, 2019, year)
  cpi_chain_factor_2019 <- get_chained_cpi_growth_factor(macro_projections, 2019, year)

  # -- Assertions: growth factors are economically plausible ----
  # CPI and wages grow over time. If these are <= 1.0 for years after 2019,
  # either the wrong base year was passed, or macro_projections has bad data.
  # (Deflation is theoretically possible but hasn't happened in the US since
  # the Great Depression — if these fire, it's almost certainly a bug.)
  if (year > 2019) {
    stopifnot(supply_params$cpi_factor > 1.0)
    stopifnot(supply_params$nominal_wage_factor > 1.0)
  }
  # Growth factors should be reasonable magnitude (not 0.5, not 10x)
  if (year <= 2035) {
    stopifnot(supply_params$cpi_factor < 3.0)
    stopifnot(supply_params$nominal_wage_factor < 3.0)
  }
  cpi_chain_factor_2026 <- get_chained_cpi_growth_factor(macro_projections, 2019, 2026)

  demand_params <- base_demand_params

  # Tax parameters
  tax_sim_path <- baseline_info$paths$`Tax-Simulator`
  use_cached_donors <- baseline_info$options$use_cached_donors

  if (use_cached_donors) {
    # -- Inlined: load_donor_pool_cache() ---
    donor_pool_result <- {
      cache_file <- file.path('./cache/donor_pools', paste0('donor_pool_', year, '.rds'))
      if (file.exists(cache_file)) {
        readRDS(cache_file)
      } else {
        NULL
      }
    }
    if (is.null(donor_pool_result)) {
      stop(paste0(
        'FATAL: --use-cached-donors flag set but cache does not exist for year ', year, '.\n',
        '       Expected cache file: ./cache/donor_pools/donor_pool_', year, '.rds\n',
        '       Either run without --use-cached-donors to rebuild, or check cache path.'
      ))
    }
    donor_pool <- donor_pool_result$donors
    pct_thresholds <- donor_pool_result$pct_thresholds
  } else {
    donor_pool_result <- tax_sim_path %>%
      file.path('static/detail', paste0(year, '.csv')) %>%
      prepare_donor_pool(year)
    donor_pool <- donor_pool_result$donors
    pct_thresholds <- donor_pool_result$pct_thresholds
    # -- Inlined: save_donor_pool_cache() ---
    {
      cache_dir <- './cache/donor_pools'
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      cache_file <- file.path(cache_dir, paste0('donor_pool_', year, '.rds'))
      saveRDS(donor_pool_result, cache_file)
    }
  }

  # -- Inlined: load_emtr_thresholds() ---
  wage_thresholds <- {
    threshold_file <- file.path(tax_sim_path, 'static', 'supplemental',
                                 paste0('percentiles_', year, '.csv'))
    thresholds <- read_csv(threshold_file, show_col_types = FALSE) %>%
      filter(year == !!year) %>%
      select(percentile, wages)
    thresholds
  }

  tax_max <- baseline_info$law$tax_max %>%
    filter(year == !!year) %>%
    pull(oasi_tax_max)

  # Prepare parent units
  prepared <- prepare_parent_units(
    parent_units       = aged_hh$parent_units,
    children           = aged_hh$children,
    households         = aged_hh$households,
    donor_pool         = donor_pool,
    pct_thresholds     = pct_thresholds,
    wage_thresholds    = wage_thresholds,
    tax_max            = tax_max,
    cached_price_wedge = cached_price_wedge
  )

  parent_units_list <- prepared[PARENT_UNIT_NAMES]

  # -- Assertions: parent unit data integrity after preparation ----
  # Price wedge columns must exist and vary across households. If the
  # cached_price_wedge join failed silently (e.g., wrong key types),
  # these columns would be missing or constant.
  for (pu_name in PARENT_UNIT_NAMES) {
    pu_df <- parent_units_list[[pu_name]]
    if (!is.null(pu_df) && nrow(pu_df) > 1) {
      stopifnot('price_wedge.center_low' %in% names(pu_df))
      stopifnot('price_wedge.center_high' %in% names(pu_df))
      stopifnot('price_wedge.home' %in% names(pu_df))
      stopifnot(sd(pu_df$price_wedge.center_high, na.rm = FALSE) > 0)
    }
  }

  # Row counts must match between parent_units and demand_params row_ids.
  # If aging created or dropped rows without updating demand_params,
  # the alpha matching in get_demand_prob_matrix would silently fail.
  for (i in seq_along(PARENT_UNIT_NAMES)) {
    pu_name <- PARENT_UNIT_NAMES[i]
    pu_df <- parent_units_list[[pu_name]]
    if (!is.null(pu_df) && nrow(pu_df) > 0 && !is.null(demand_params[[pu_name]]$row_ids)) {
      stopifnot(nrow(pu_df) == nrow(demand_params[[pu_name]]$row_ids))
    }
  }

  # Median income thresholds
  # -- Inlined: compute_median_income_by_family_size() ---
  median_income_lookup <- {
    adjustment_factors <- c('1' = 0.52, '2' = 0.68, '3' = 0.84, '4' = 1.00, '5' = 1.16)

    four_person_filers <- donor_pool %>%
      filter(filer == 1, n_persons == 4)

    mask <- !is.na(four_person_filers$agi) & !is.na(four_person_filers$weight)
    x <- four_person_filers$agi[mask]
    w <- four_person_filers$weight[mask]
    ord <- order(x)
    x <- x[ord]
    w <- w[ord]
    cw <- cumsum(w) / sum(w)
    baseline_median <- x[which(cw >= 0.5)[1]]

    baseline_median * adjustment_factors
  }

  cat('  Median income thresholds (by family size):\n')
  cat(sprintf('    1-person: $%s | 2-person: $%s | 3-person: $%s | 4-person: $%s | 5+-person: $%s\n',
              format(round(median_income_lookup['1']), big.mark = ','),
              format(round(median_income_lookup['2']), big.mark = ','),
              format(round(median_income_lookup['3']), big.mark = ','),
              format(round(median_income_lookup['4']), big.mark = ','),
              format(round(median_income_lookup['5']), big.mark = ',')))

  # Add simulation context variables
  parent_units_list <- add_simulation_variables(
    parent_units_list,
    median_income_lookup,
    cpi_factor_2019 = supply_params$cpi_factor,
    cpi_chain_factor_2019 = cpi_chain_factor_2019,
    cpi_chain_factor_2026 = cpi_chain_factor_2026
  )

  year_seed <- year * 1000 + seed_offset

  list(
    supply_params            = supply_params,
    demand_params            = demand_params,
    parent_units_list        = parent_units_list,
    donor_pool               = donor_pool,
    tax_sim_path             = tax_sim_path,
    aged_spm                 = aged_spm,
    aged_spm_parent_earnings = aged_spm_parent_earnings,
    median_income_lookup     = median_income_lookup,
    year_seed                = year_seed
  )
}
