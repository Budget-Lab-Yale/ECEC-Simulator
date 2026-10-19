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

  # Validate baseline interface compatibility before any simulation work
  if (exists('baseline_interface') && !is.null(baseline_interface)) {
    validate_baseline_interface(
      baseline_interface   = baseline_interface,
      years_to_run         = baseline_info$years,
      counterfactual_infos = counterfactual_infos,
      run_states           = if (exists('run_states')) run_states else NULL
    )
  }


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

    # Interface stages (1a, 1b) built under this run's urban-density setting?
    # (urban_density.R; alphas, price forest checked on load below)
    # State runs only: national default run = main, no density markers
    if (exists('run_states') && !is.null(run_states)) check_interface_urban_density(base_path)

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


  # State-level run? (run_states set in main.R from the runscript; see
  # docs/state_level_analysis.md). State runs defer sub-sampling to the
  # per-state contexts below: filter to state first, then subsample.
  is_state_run <- exists('run_states') && !is.null(run_states)
  if (is_state_run) {
    if (!'statefip' %in% names(sim_base_hh$households) ||
        all(is.na(sim_base_hh$households$statefip))) {
      stop('State-level run requested but the households table has no statefip ',
           'values. The interface predates state support; regenerate it ',
           '(re-run ACS processing with STATEFIP in the raw pull).')
    }
  }

  # Price wedge income ranks (price forests, income percentile) on full
  # calibration sample, before sub-sampling -- same ranks as 2a cache, so
  # base-year wedges match alphas. Wedge off -> not needed
  # State runs only (national: main's cache, built after sub-sampling below)
  price_wedge_on <- !(exists('disable_price_wedge') && isTRUE(disable_price_wedge))
  price_income_pctiles <- if (is_state_run && price_wedge_on) {
    price_wedge_income_pctiles(sim_base_hh$parent_units, sim_base_hh$households)
  } else {
    NULL
  }

  # Sub-sample households for simulation if sim_sample < calib_sample
  if (sim_sample < calib_sample && !is_state_run) {
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

    sim_base_hh <- filter_sim_base_hh(sim_base_hh, sampled_hh_ids, weight_inflation)

    cat('  Parent units after sampling: ', nrow(sim_base_hh$parent_units), '\n', sep = '')
  }


  # Parses a supply params yaml (estimation output or a state override, which
  # share the same schema) into the base_supply_params list format
  parse_supply_params_yaml <- function(supply_params_path) {
    supply_yaml_data <- read_yaml(supply_params_path)

    sector_keys <- c('unpaid_center_based', 'low_price_center_based',
                     'high_price_center_based', 'paid_home_based')
    L_req <- do.call(rbind, lapply(sector_keys, function(s) {
      c(supply_yaml_data$labor_requirements[[s]]$no_ba,
        supply_yaml_data$labor_requirements[[s]]$ba)
    }))

    list(
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

  # Load supply model parameters from estimation output
  {
    supply_params_path <- file.path(output_root, 'estimation', 'supply', 'supply_2019.yaml')

    if (!file.exists(supply_params_path)) {
      stop(paste0('Supply params file not found: ', supply_params_path, '\n',
                  'Run NSECE processing first, or use -C to specify a calibration interface.'))
    }

    base_supply_params <- parse_supply_params_yaml(supply_params_path)
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

    # Load alpha matrices for each parent unit type. State runs keep the full
    # rds contents (p0, child_weights) for the state demand contraction.
    alpha_data_by_type <- list()
    for (pu_type in c('c1', 'c2plus')) {
      alpha_path <- file.path(estimation_dir, paste0('alpha_', pu_type, '_', calibration_year, '.rds'))
      if (file.exists(alpha_path)) {
        alpha_data <- readRDS(alpha_path)
        # Interface built under this run's urban-density setting? (urban_density.R)
        if (is_state_run) {
          check_urban_density_built(alpha_data$urban_density, paste0('Alpha matrix ', alpha_path), '2a')
        }
        base_demand_params[[pu_type]]$alpha <- alpha_data$alpha
        base_demand_params[[pu_type]]$row_ids <- alpha_data$row_ids
        if (is_state_run) {
          alpha_data_by_type[[pu_type]] <- alpha_data
        }
      }
    }

    # Load other_paid_base_price from price forest cache
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

  wedge_disabled <- exists('disable_price_wedge') && isTRUE(disable_price_wedge)

  cached_price_wedge <- NULL
  if (!is_state_run) {
    # National path: main's cache + disable handling, unchanged
    cached_price_wedge <- compute_price_wedge_cache(
      parent_units = sim_base_hh$parent_units,
      children     = sim_base_hh$children,
      households   = sim_base_hh$households,
      qrf_models   = price_models$price_qrf_models
    )
    if (wedge_disabled) {
      cat('--no-price-wedge flag set: disabling price heterogeneity (all wedges = 1.0)\n')
      cached_price_wedge <- NULL
    }
  } else {
    if (wedge_disabled) {
      cat('--no-price-wedge flag set: disabling price heterogeneity (all wedges = 1.0)\n')
    }
    # State runs: one cache over all households, each state context takes its
    # rows (below) -- keyed draws, same forests as calibration cache, so
    # base-year wedges match alphas.
    if (!wedge_disabled) {
      cached_price_wedge <- compute_price_wedge_cache_state(
        parent_units     = sim_base_hh$parent_units,
        children         = sim_base_hh$children,
        households       = sim_base_hh$households,
        price_qrf_models = price_qrf_models,
        income_pctiles   = price_income_pctiles
      )
    }
  }

  #-----------------------------
  # Build per-state contexts
  #-----------------------------
  # For state-level runs (docs/state_level_analysis.md), each state gets its
  # own context: households filtered to the state (then sub-sampled), alphas
  # re-anchored to state demand targets when a target CSV exists, supply
  # params from a state yaml when present, a state epop file when present,
  # and per-state warm-start prices / employment-rate caches.

  state_contexts <- NULL
  if (is_state_run) {
    state_contexts <- list()
    state_metadata <- list()

    empty_prices <- setNames(
      lapply(counterfactual_ids, function(x) INITIAL_PRICES),
      counterfactual_ids
    )

    for (st in names(run_states)) {
      fips <- run_states[[st]]
      cat('\nBuilding state context:', st, '(FIPS', fips, ')\n')

      # State by STATEFIP; sub-state (NYC) adds COUNTYFIP
      state_hh_ids <- select_geo_hh_ids(sim_base_hh$households, st, fips)

      if (length(state_hh_ids) == 0) {
        stop('No households found for state ', st, ' (FIPS ', fips, ').')
      }

      st_base_hh <- filter_sim_base_hh(sim_base_hh, state_hh_ids)
      cat('  Households: ', length(state_hh_ids),
          ' | Parent units: ', nrow(st_base_hh$parent_units), '\n', sep = '')

      # Subsample within the state (filter first, then sample)
      if (sim_sample < calib_sample) {
        sampling_fraction <- sim_sample / calib_sample
        set.seed(random_seed_base + 1000 + fips)
        st_sampled_ids <- st_base_hh$households %>%
          slice_sample(prop = sampling_fraction) %>%
          pull(hh_id)
        st_base_hh <- filter_sim_base_hh(st_base_hh, st_sampled_ids,
                                         calib_sample / sim_sample)
        cat('  Sub-sampled to ', length(st_sampled_ids), ' households (',
            sim_sample, '% of ACS)\n', sep = '')
      }

      # Sub-state geography (NYC) on own income ranks: own alpha rds
      # (compute_substate_alpha.R) + own price wedge rows below. NULL (logged)
      # -> national alphas, as every other geography
      st_alpha_data <- alpha_data_by_type
      substate_alpha <- load_substate_alpha(st, estimation_dir, calibration_year)
      if (!is.null(substate_alpha)) st_alpha_data <- substate_alpha

      # State's rows of price wedge cache. Sub-state (NYC) w/ own alphas: own
      # wedge rows -- forest at geo's own income percentile, as
      # compute_substate_alphas() (2a), so base-year wedges match own alphas
      st_price_wedge <- if (is.null(cached_price_wedge)) {
        NULL
      } else if (!is.null(substate_alpha)) {
        compute_price_wedge_cache_state(
          parent_units     = sim_base_hh$parent_units %>% filter(hh_id %in% state_hh_ids),
          children         = sim_base_hh$children %>% filter(hh_id %in% state_hh_ids),
          households       = sim_base_hh$households %>% filter(hh_id %in% state_hh_ids),
          price_qrf_models = price_qrf_models,
          income_pctiles   = price_wedge_income_pctiles(sim_base_hh$parent_units,
                                                        sim_base_hh$households, st)
        )
      } else {
        cached_price_wedge %>% filter(hh_id %in% state_hh_ids)
      }

      # ECEC_STATE_ALPHA_PRICE=state, default (reprice_alpha_to_state_supply.R):
      # alpha rebuilt at state's own 2019 supply prices, before re-anchoring.
      # 'national' (off) -> national-price alpha as calibration wrote it
      state_V_by_type <- NULL
      alpha_supply_override <- file.path('config', 'state', 'supply', paste0(st, '.yaml'))
      if (get_state_alpha_price_mode() == 'state' && file.exists(alpha_supply_override) &&
          file.exists(file.path('config', 'state', 'demand', paste0(st, '.csv')))) {
        if (is.null(st_price_wedge)) {
          stop('ECEC_STATE_ALPHA_PRICE=state needs the price wedge (do not use --no-price-wedge).')
        }
        P_nat <- state_base_supply_prices(base_supply_params)
        P_st  <- state_base_supply_prices(parse_supply_params_yaml(alpha_supply_override))
        cat(sprintf('  State-price alpha (%s): supply price low/high/home %.2f/%.2f/%.2f vs national %.2f/%.2f/%.2f $/hr\n',
                    st, P_st[2], P_st[3], P_st[4], P_nat[2], P_nat[3], P_nat[4]))
        repriced <- reprice_alpha_to_supply(
          alpha_data_by_type    = st_alpha_data,
          state_hh_ids          = state_hh_ids,
          calibration_data_dir  = file.path(estimation_dir, 'data'),
          wedge_rows            = st_price_wedge,
          P_nat                 = P_nat,
          P_state               = P_st,
          other_paid_base_price = base_demand_params$other_paid_base_price,
          calibration_year      = calibration_year,
          check_net_income      = is.null(substate_alpha),
          state_postal          = st
        )
        print(repriced$diagnostics, row.names = FALSE)
        st_alpha_data   <- repriced$alpha_data_by_type
        state_V_by_type <- repriced$V_by_type
      }

      # Demand: re-anchor alphas to state targets when a target CSV exists;
      # otherwise subset the national alphas to the state's rows unchanged
      st_demand_params <- base_demand_params
      demand_targets_path <- file.path('config', 'state', 'demand', paste0(st, '.csv'))
      demand_adjustment <- NULL
      if (file.exists(demand_targets_path)) {
        # Care hours + caregiver employment solved together (base-year
        # employment = forest rate, which yearly targets grow from)
        demand_adjustment <- adjust_alpha_for_state(
          alpha_data_by_type = st_alpha_data,
          state_hh_ids       = state_hh_ids,
          targets_csv_path   = demand_targets_path,
          state_postal       = st,
          caregivers         = caregiver_groups_for_state(
            sim_base_hh$parent_units %>% filter(hh_id %in% state_hh_ids))
        )
        for (pu_type in names(demand_adjustment$alpha_by_type)) {
          st_demand_params[[pu_type]]$alpha <- demand_adjustment$alpha_by_type[[pu_type]]
          st_demand_params[[pu_type]]$row_ids <- demand_adjustment$row_ids_by_type[[pu_type]]
        }
        # Frozen base year: re-anchored alpha at state prices reproduces the
        # demand adjustment's sector hours (base-year labor L_0)
        if (!is.null(state_V_by_type) && !is.null(demand_adjustment$sector_hours)) {
          check_state_base_year(
            alpha_by_type         = demand_adjustment$alpha_by_type,
            V_by_type             = state_V_by_type,
            alpha_data_by_type    = st_alpha_data,
            expected_sector_hours = demand_adjustment$sector_hours,
            state_postal          = st
          )
        }
      } else {
        cat('  No demand target file (', demand_targets_path,
            '): using national alphas, filter only\n', sep = '')
        for (pu_type in names(st_alpha_data)) {
          ad <- st_alpha_data[[pu_type]]
          in_state <- ad$row_ids$hh_id %in% state_hh_ids
          st_demand_params[[pu_type]]$alpha <- ad$alpha[in_state, , drop = FALSE]
          st_demand_params[[pu_type]]$row_ids <- ad$row_ids[in_state, , drop = FALSE]
        }
      }

      # Supply: state yaml override when present, else NSECE-derived params
      supply_override_path <- file.path('config', 'state', 'supply', paste0(st, '.yaml'))
      if (file.exists(supply_override_path)) {
        # Override carries no labor_supply -- base-year labor below needs
        # re-anchored demand
        if (is.null(demand_adjustment)) {
          stop('State ', st, ': supply override ', supply_override_path,
               ' exists but demand target file ', demand_targets_path,
               ' does not. Base-year labor comes from re-anchored demand.',
               ' Build it: Rscript state_data/code/00_build_demand_targets.R,',
               ' or remove the override to run on national parameters.')
        }
        cat('  Supply params: state override (', supply_override_path, ')\n', sep = '')
        st_supply_params <- parse_supply_params_yaml(supply_override_path)
        supply_source <- supply_override_path
      } else {
        st_supply_params <- base_supply_params
        supply_source <- 'nsece'
      }

      # Base-year labor from re-anchored model's sector hours (same
      # colSums(L_req * Qd) solver uses) -- only state L_0 definition; wage
      # curve starts at base-year wage by construction. Contraction pins
      # combined centre hours, not low/high split, so a fixed national split
      # would miss model's own labor. No demand file, no override -> national
      # NSECE params unchanged.
      if (!is.null(demand_adjustment)) {
        st_supply_params$L <- colSums(st_supply_params$L_req * demand_adjustment$sector_hours)
        cat(sprintf('  Base-year labor: model base-year hours -> no_ba %.4g, ba %.4g\n',
                    st_supply_params$L[1], st_supply_params$L[2]))
      } else {
        cat(sprintf('  Base-year labor: national NSECE labor_supply -> no_ba %.4g, ba %.4g\n',
                    st_supply_params$L[1], st_supply_params$L[2]))
      }

      # Employment growth: state epop file when present, else national CBO
      epop_override_path <- file.path('config', 'state', 'employment', paste0(st, '.csv'))
      st_epop_path <- if (file.exists(epop_override_path)) epop_override_path else NULL
      if (!is.null(st_epop_path)) {
        cat('  Employment growth: state epop file (', st_epop_path, ')\n', sep = '')
      }

      state_contexts[[st]] <- list(
        postal                     = st,
        fips                       = fips,
        sim_base_hh                = st_base_hh,
        base_supply_params         = st_supply_params,
        base_demand_params         = st_demand_params,
        cached_price_wedge         = st_price_wedge,
        epop_path                  = st_epop_path,
        prev_baseline_prices       = INITIAL_PRICES,
        prev_counterfactual_prices = empty_prices,
        baseline_emp_rates_2019    = NULL
      )

      state_metadata[[st]] <- list(
        fips             = fips,
        n_households     = nrow(st_base_hh$households),
        demand_adjusted  = !is.null(demand_adjustment),
        demand_deltas    = if (!is.null(demand_adjustment)) as.list(demand_adjustment$deltas) else NULL,
        base_employment_shifts = if (!is.null(demand_adjustment$employment_shifts))
          as.list(demand_adjustment$employment_shifts) else NULL,
        supply_source    = supply_source,
        alpha_price      = if (!is.null(state_V_by_type)) 'state' else 'national',
        base_year_labor  = as.list(setNames(st_supply_params$L, c('no_ba', 'ba'))),
        epop_source      = st_epop_path %||% 'national'
      )
      if (!is.null(substate_alpha)) {
        state_metadata[[st]]$income_ranks <- paste0('own (', get_income_rank_scheme(), ')')
      }
    }

    # Record state-run provenance in run metadata
    metadata_dir <- file.path(output_root, 'metadata')
    dir.create(metadata_dir, showWarnings = FALSE, recursive = TRUE)
    write_yaml(state_metadata, file.path(metadata_dir, 'state_analysis.yaml'))

    # The national tables are no longer needed once contexts are built
    sim_base_hh <- NULL
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
    cached_price_wedge       = cached_price_wedge,
    state_contexts           = state_contexts
  )
}
