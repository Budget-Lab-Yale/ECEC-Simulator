#------------------------------------------------------------------------------
# 3a_initialize_simulation.R
#
# Simulation initialization: load base data, price models, compute caches.
# Primary function: initialize_simulation()
#------------------------------------------------------------------------------



initialize_simulation <- function() {

  #----------------------------------------------------------------------------
  # Initialize simulation context with all configuration and base data.
  # Loads scenarios, microdata, supply/demand parameters, price models,
  # and computes caches needed for the simulation loop.
  #
  # Params: none
  #
  # Returns: (list) simulation context containing scenarios, microdata,
  #   supply/demand params, macro projections, summary accumulators,
  #   price models, and cached price wedges
  #----------------------------------------------------------------------------

  # Calculate OASI tax maximums using AWI from macro projections.
  # Indexes to 2025 actual values: 2023: $160,200, 2024: $168,600, 2025: $176,100
  calculate_oasi_tax_max <- function(scenario_info) {

    #--------------------------------------------------------------------------
    # Calculate OASI tax maximums for each simulation year using AWI from
    # macro projections, indexed to 2025 actual values.
    #
    # Params:
    #   - scenario_info (list): scenario configuration with paths and years
    #
    # Returns: (df) tibble with year and oasi_tax_max columns
    #--------------------------------------------------------------------------

    known_values <- tibble(
      year = as.integer(names(OASI_TAX_MAX_KNOWN)),
      oasi_tax_max = unlist(OASI_TAX_MAX_KNOWN)
    )

    macro_projections_local <- bind_rows(
      read_csv(file.path(scenario_info$paths$`Macro-Projections`, 'historical.csv'),  show_col_types = F),
      read_csv(file.path(scenario_info$paths$`Macro-Projections`, 'projections.csv'), show_col_types = F)
    )

    awi_2025 <- macro_projections_local %>%
      filter(year == 2025) %>%
      pull(awi)

    oasi_table <- tibble(year = scenario_info$years) %>%
      left_join(known_values, by = 'year') %>%
      left_join(
        macro_projections_local %>% select(year, awi),
        by = 'year'
      ) %>%
      mutate(
        oasi_tax_max = if_else(
          is.na(oasi_tax_max),
          176100 * (awi / awi_2025),
          oasi_tax_max
        )
      ) %>%
      select(year, oasi_tax_max)

    return(oasi_table)
  }


  all_scenarios <- runscript %>%
    pull(id) %>%
    unique()

  baseline_id <- 'baseline'
  counterfactual_ids <- all_scenarios[all_scenarios != baseline_id]

  cat('Baseline:', baseline_id, '\n')
  cat('Counterfactuals:', paste(counterfactual_ids, collapse = ', '), '\n\n')

  baseline_info <- get_scenario_info(baseline_id)

  counterfactual_infos <- counterfactual_ids %>%
    map(get_scenario_info) %>%
    set_names(counterfactual_ids)


  # Read simulation base year microdata
  {
    table_names <- c('households', 'household_members', 'parent_units', 'children', 'tax_units')

    base_path <- file.path(
      default_paths$roots$output,
      time_stamp,
      'estimation',
      'data'
    )

    sim_base_hh <- setNames(lapply(table_names, function(name) {
      filepath <- file.path(base_path, paste0('acs_', name, '_', acs_base_year, '.csv'))
      if (file.exists(filepath)) return(tibble(fread(filepath)))
      cat('Warning: Simulation base data file not found:', filepath, '\n')
      NULL
    }), table_names)
    sim_base_hh <- compact(sim_base_hh)

    # Read SPM data files (optional - for poverty analysis)
    spm_units_path <- file.path(base_path, paste0('acs_spm_units_', acs_base_year, '.csv'))
    pu_spm_xwalk_path <- file.path(base_path, paste0('acs_pu_spm_xwalk_', acs_base_year, '.csv'))
    spm_parent_earnings_path <- file.path(base_path, paste0('acs_spm_parent_earnings_', acs_base_year, '.csv'))

    if (file.exists(spm_units_path) && file.exists(pu_spm_xwalk_path)) {
      sim_base_hh$spm_units <- tibble(fread(spm_units_path))
      sim_base_hh$pu_spm_xwalk <- tibble(fread(pu_spm_xwalk_path))
      if (file.exists(spm_parent_earnings_path)) {
        sim_base_hh$spm_parent_earnings <- tibble(fread(spm_parent_earnings_path))
      }
      cat('  SPM data loaded for poverty analysis\n')
    }

    # Read enrollment tables (diagnostics/debugging only - skip during SLURM setup
    # to reduce parallel_ctx.rds size)
    if (is.null(slurm_phase) || slurm_phase != 'setup') {
      enrollment_path <- file.path(base_path, paste0('acs_enrollment_', acs_base_year, '.csv'))
      enrollment_joint_path <- file.path(base_path, paste0('acs_enrollment_joint_', acs_base_year, '.csv'))

      if (file.exists(enrollment_path)) {
        sim_base_hh$enrollment <- tibble(fread(enrollment_path))
      }
      if (file.exists(enrollment_joint_path)) {
        sim_base_hh$enrollment_joint <- tibble(fread(enrollment_joint_path))
      }
    }
  }


  # Sub-sample households for simulation if sim_sample < calib_sample
  if (sim_sample < calib_sample) {
    sampling_fraction <- sim_sample / calib_sample
    weight_inflation  <- calib_sample / sim_sample

    cat('Simulation sub-sampling: ', sim_sample, '% of full ACS (',
        round(sampling_fraction * 100, 1), '% of calibration data)\n', sep = '')

    set.seed(random_seed_base + 1000)
    sampled_hh_ids <- sim_base_hh$households %>%
      slice_sample(prop = sampling_fraction) %>%
      pull(hh_id)

    cat('  Households: ', length(sampled_hh_ids), ' of ',
        nrow(sim_base_hh$households), '\n', sep = '')

    # Filter all tables to sampled households and inflate weights
    sim_base_hh$households <- sim_base_hh$households %>%
      filter(hh_id %in% sampled_hh_ids) %>%
      mutate(hh_weight = hh_weight * weight_inflation)

    for (tbl_name in c('household_members', 'parent_units', 'children',
                        'tax_units', 'enrollment', 'enrollment_joint')) {
      if (!is.null(sim_base_hh[[tbl_name]]) && 'hh_id' %in% names(sim_base_hh[[tbl_name]])) {
        sim_base_hh[[tbl_name]] <- sim_base_hh[[tbl_name]] %>%
          filter(hh_id %in% sampled_hh_ids)
      }
    }

    # Inflate all survey weights to maintain population-level aggregates
    # (hh_weight already inflated above; these are the per-person/child weights
    # used by the equilibrium solver and output aggregation)
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

    # SPM tables: filter by hh_id via pu_spm_xwalk
    if (!is.null(sim_base_hh$pu_spm_xwalk)) {
      sim_base_hh$pu_spm_xwalk <- sim_base_hh$pu_spm_xwalk %>%
        filter(hh_id %in% sampled_hh_ids)

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
          filter(hh_id %in% sampled_hh_ids)
      }
    }

    cat('  Parent units after sampling: ', nrow(sim_base_hh$parent_units), '\n', sep = '')
  }


  # Load supply model parameters from estimation output
  {
    supply_params_path <- file.path(output_root, 'estimation', 'supply', 'supply_2019.yaml')

    if (!file.exists(supply_params_path)) {
      stop(paste0('Supply params file not found: ', supply_params_path, '\n',
                  'Run NSECE processing first, or use -C to specify a calibration interface.'))
    }

    supply_yaml_data <- read_yaml(supply_params_path)

    sector_keys <- c('unpaid_center_based', 'low_price_center_based',
                     'high_price_center_based', 'paid_home_based')
    L_req <- do.call(rbind, lapply(sector_keys, function(s) {
      c(supply_yaml_data$labor_requirements[[s]]$no_ba,
        supply_yaml_data$labor_requirements[[s]]$ba)
    }))

    base_supply_params <- list(
      L_req   = L_req,
      w       = c(supply_yaml_data$wages$no_ba, supply_yaml_data$wages$ba),
      L       = c(supply_yaml_data$labor_supply$no_ba, supply_yaml_data$labor_supply$ba),
      e       = c(supply_yaml_data$elasticities$no_ba, supply_yaml_data$elasticities$ba),
      delta_0 = c(
        supply_yaml_data$per_unit_residual$unpaid_center_based,
        supply_yaml_data$per_unit_residual$low_price_center_based,
        supply_yaml_data$per_unit_residual$high_price_center_based,
        supply_yaml_data$per_unit_residual$paid_home_based
      )
    )
  }


  # Load demand model parameters from estimation output
  {
    estimation_dir <- file.path(output_root, 'estimation')
    demand_params_path <- file.path(estimation_dir, 'demand_params_2019.yaml')

    if (!file.exists(demand_params_path)) {
      stop(paste0('Demand params file not found: ', demand_params_path, '\n',
                  'Run calibration first, or use -C to specify a calibration interface.'))
    }

    demand_yaml_data <- read_yaml(demand_params_path)

    # Parse demand params - supports per-type or uniform format
    {
      pu_types <- c('c1', 'c2plus')

      # Check for old 4-type format
      old_4type_found <- intersect(names(demand_yaml_data), c('c1_p1', 'c1_p2', 'c2plus_p1', 'c2plus_p2'))
      if (length(old_4type_found) > 0) {
        stop(paste0(
          'Old 4-type demand format detected in demand_params.yaml.\n',
          '  Found old type name (', old_4type_found[1], ').\n',
          '  The model now uses 2 types: c1, c2plus (n_parents distinction removed).\n',
          '  Please re-run calibration to generate a new demand_params.yaml file.\n',
          '  Run: Rscript src/main.R -r <runscript> with estimation enabled.'
        ))
      }

      has_per_type <- all(sapply(pu_types, function(t) !is.null(demand_yaml_data[[t]])))

      if (has_per_type) {
        base_demand_params <- list()
        for (pu_type in pu_types) {
          type_data <- demand_yaml_data[[pu_type]]
          if (!is.null(type_data$beta) && !is.null(type_data$rho)) {
            base_demand_params[[pu_type]] <- list(beta = type_data$beta, rho = type_data$rho)
          } else if (!is.null(type_data$beta_0)) {
            stop(paste0(
              'Old beta_0/beta_1 format detected in demand_params.yaml for type ', pu_type, '.\n',
              'This format is no longer supported. Please re-run calibration.'
            ))
          } else if (!is.null(type_data$beta)) {
            base_demand_params[[pu_type]] <- list(beta = type_data$beta, rho = 0.1)
          } else {
            stop(paste0('Missing beta/rho for type ', pu_type, ' in demand_params.yaml'))
          }
        }
      } else if (!is.null(demand_yaml_data$beta) && !is.null(demand_yaml_data$rho)) {
        uniform_params <- list(beta = demand_yaml_data$beta, rho = demand_yaml_data$rho)
        base_demand_params <- setNames(rep(list(uniform_params), length(pu_types)), pu_types)
      } else if (!is.null(demand_yaml_data$beta_0)) {
        stop('Old beta_0/beta_1 format no longer supported. Please re-run calibration.')
      } else if (!is.null(demand_yaml_data$beta)) {
        uniform_params <- list(beta = demand_yaml_data$beta, rho = 0.1)
        base_demand_params <- setNames(rep(list(uniform_params), length(pu_types)), pu_types)
      } else {
        old_6type_found <- intersect(names(demand_yaml_data), c('c2_p1', 'c2_p2', 'c3plus_p1', 'c3plus_p2'))
        if (length(old_6type_found) > 0) {
          stop(paste0(
            'Old 6-type demand format detected in demand_params.yaml.\n',
            '  Found old type name (', old_6type_found[1], '). The model now uses 2 types: c1, c2plus.\n',
            '  Please re-run calibration.'
          ))
        }

        stop('Invalid demand_params.yaml format. Expected per-type or uniform beta/rho.')
      }
    }

    calibration_year <- demand_yaml_data$year %||% 2019

    # Load alpha matrices for each parent unit type
    for (pu_type in c('c1', 'c2plus')) {
      alpha_path <- file.path(estimation_dir, paste0('alpha_', pu_type, '_', calibration_year, '.rds'))
      if (file.exists(alpha_path)) {
        alpha_data <- readRDS(alpha_path)
        base_demand_params[[pu_type]]$alpha <- alpha_data$alpha
        base_demand_params[[pu_type]]$row_ids <- alpha_data$row_ids
      }
    }

    # Load other_paid_base_price from price models cache
    price_models_path <- file.path('./cache/price_models', 'price_qrf_models.rds')
    if (file.exists(price_models_path)) {
      price_qrf_models <- readRDS(price_models_path)
      base_demand_params$other_paid_base_price <- price_qrf_models$summary_stats$other_paid_base_price
      if (!is.null(base_demand_params$other_paid_base_price)) {
        cat('  Loaded other_paid_base_price: $', round(base_demand_params$other_paid_base_price, 2), '/hr\n', sep = '')
      }
    }

    base_demand_params$calibration_year <- calibration_year
  }


  macro_projections <- bind_rows(
    read_csv(file.path(baseline_info$paths$`Macro-Projections`, 'historical.csv'),  show_col_types = FALSE),
    read_csv(file.path(baseline_info$paths$`Macro-Projections`, 'projections.csv'), show_col_types = FALSE)
  )

  # Calculate OASI tax max for all scenarios
  baseline_info$law <- list(tax_max = calculate_oasi_tax_max(baseline_info))

  for (scenario_id in counterfactual_ids) {
    counterfactual_infos[[scenario_id]]$law <- list(
      tax_max = calculate_oasi_tax_max(counterfactual_infos[[scenario_id]])
    )
  }

  # Initialize summary accumulators
  summary_accumulators <- {
    empty_accum <- function(fields) setNames(lapply(fields, function(x) tibble()), fields)

    baseline_fields <- c('allocation', 'employment', 'fiscal_cost', 'median_income_thresholds')
    cf_fields <- c('allocation', 'employment', 'fiscal_cost', 'mechanical_fiscal',
                   'child_earnings_overall', 'child_earnings_by_quintile',
                   'child_earnings_by_transition', 'child_earnings_by_age',
                   'fiscal_npv', 'fiscal_npv_by_quintile', 'distributional', 'poverty')

    c(
      setNames(list(empty_accum(baseline_fields)), baseline_id),
      setNames(lapply(counterfactual_ids, function(x) empty_accum(cf_fields)), counterfactual_ids)
    )
  }

  # Load price models and compute price_wedge cache
  price_models <- {
    price_qrf_models <- readRDS(file.path('./cache/price_models', 'price_qrf_models.rds'))
    list(price_qrf_models = price_qrf_models)
  }

  cached_price_wedge <- compute_price_wedge_cache(
    parent_units = sim_base_hh$parent_units,
    children     = sim_base_hh$children,
    households   = sim_base_hh$households,
    qrf_models   = price_models$price_qrf_models
  )

  # Optionally disable price wedge heterogeneity (set all wedges to 1.0)
  if (exists('disable_price_wedge') && isTRUE(disable_price_wedge)) {
    cat('--no-price-wedge flag set: disabling price heterogeneity (all wedges = 1.0)\n')
    cached_price_wedge <- NULL
  }

  # Return simulation context
  list(
    baseline_id              = baseline_id,
    counterfactual_ids       = counterfactual_ids,
    baseline_info            = baseline_info,
    counterfactual_infos     = counterfactual_infos,
    years_to_run             = baseline_info$years,
    baseline_emp_rates_2019  = NULL,
    sim_base_hh              = sim_base_hh,
    base_supply_params       = base_supply_params,
    base_demand_params       = base_demand_params,
    macro_projections        = macro_projections,
    summary_accumulators     = summary_accumulators,
    baseline_results         = list(),
    prev_baseline_prices     = INITIAL_PRICES,
    prev_counterfactual_prices = setNames(
      lapply(counterfactual_ids, function(x) INITIAL_PRICES),
      counterfactual_ids
    ),
    price_models             = price_models,
    cached_price_wedge       = cached_price_wedge
  )
}
