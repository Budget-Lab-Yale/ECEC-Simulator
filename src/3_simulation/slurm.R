#------------------------------------------------------------------------------
# slurm.R
#
# SLURM parallel execution functions for distributed simulation.
# Not numbered - alternate execution path for cluster environments.
#
# Extracted from: simulation.R (SLURM functions only)
#------------------------------------------------------------------------------
# SLURM JOB ARRAY + PARALLEL YEAR PROCESSING FUNCTIONS
#
# These functions enable parallel execution of simulation years via SLURM
# job arrays. Each year gets its own SLURM job with dedicated memory.
#
# Three-phase architecture:
#   slurm_setup_simulation()  → processing + calibration + save context to disk
#   slurm_run_year()          → load context, run one year, save result to disk
#   slurm_finalize()          → load all year results, combine, write output
#
# Core worker functions (reused by SLURM and sequential paths):
#   run_year_standalone()     → processes one year, returns results (no mutation)
#   run_counterfactual_standalone() → processes one counterfactual, returns results
#   combine_year_results()    → merges parallel results into accumulators
#------------------------------------------------------------------------------



restore_globals_from_config <- function(run_config) {

  #----------------------------------------------------------------------------
  # Restores global variables from a saved run config. Assigns each named
  # element from run_config into the global environment so downstream
  # functions can access them as expected.
  #
  # Params:
  #   - run_config (list): Named list containing saved global values
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  for (nm in c('time_stamp', 'output_root', 'runscript_id', 'runscript',
               'default_paths', 'random_seed_base', 'seed_offset')) {
    assign(nm, run_config[[nm]], envir = .GlobalEnv)
  }
}



slurm_setup_simulation <- function(scratch_dir) {

  #----------------------------------------------------------------------------
  # SLURM setup phase: initialize simulation context and save to scratch dir.
  #
  # Runs after processing + calibration have completed (normal Sections 1-2).
  # Builds the immutable parallel_ctx and saves it along with run config
  # so that SLURM array workers can load and run individual years.
  #
  # Params:
  #   - scratch_dir (chr): Path to scratch directory for inter-job communication
  #
  # Returns: nothing (side effects only; writes parallel_ctx.rds and run_config.rds to scratch_dir)
  #----------------------------------------------------------------------------

  cat('=== SLURM Setup Phase ===\n')
  cat('Interface:', time_stamp, '\n\n')

  sim_ctx <- initialize_simulation()

  # Build immutable context for year workers
  parallel_ctx <- list(
    baseline_id          = sim_ctx$baseline_id,
    counterfactual_ids   = sim_ctx$counterfactual_ids,
    baseline_info        = sim_ctx$baseline_info,
    counterfactual_infos = sim_ctx$counterfactual_infos,
    years_to_run         = sim_ctx$years_to_run,
    sim_base_hh          = sim_ctx$sim_base_hh,
    base_supply_params   = sim_ctx$base_supply_params,
    base_demand_params   = sim_ctx$base_demand_params,
    macro_projections    = sim_ctx$macro_projections,
    cached_price_wedge   = sim_ctx$cached_price_wedge,
    n_draws_per_record   = n_draws_per_record,
    aging_base_year      = acs_base_year,
    random_seed_base     = random_seed_base,
    seed_offset          = seed_offset,
    sim_sample           = sim_sample,
    fiscal_npv           = if (exists('fiscal_npv')) fiscal_npv else FALSE,
    disable_employment_targeting = exists('disable_employment_targeting') && isTRUE(disable_employment_targeting)
  )

  # Save run config (globals needed by year workers and finalize)
  run_config <- list(
    time_stamp         = time_stamp,
    output_root        = output_root,
    runscript_id       = runscript_id,
    runscript          = runscript,
    default_paths      = default_paths,
    years              = sim_ctx$years_to_run,
    baseline_id        = sim_ctx$baseline_id,
    counterfactual_ids = sim_ctx$counterfactual_ids,
    n_draws_per_record = n_draws_per_record,
    random_seed_base   = random_seed_base,
    seed_offset        = seed_offset,
    fiscal_npv         = if (exists('fiscal_npv')) fiscal_npv else FALSE,
    acs_base_year      = acs_base_year
  )

  # Create scratch directory structure
  dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(scratch_dir, 'year_results'), showWarnings = FALSE)

  # Save to disk
  saveRDS(parallel_ctx, file.path(scratch_dir, 'parallel_ctx.rds'))
  saveRDS(run_config, file.path(scratch_dir, 'run_config.rds'))

  cat('\nSLURM setup complete.\n')
  cat('  Scratch dir:', scratch_dir, '\n')
  cat('  Years:', paste(sim_ctx$years_to_run, collapse = ', '), '\n')
  cat('  N years:', length(sim_ctx$years_to_run), '\n')
  cat('\nNext: submit SLURM array with --slurm-phase year --slurm-year-index $SLURM_ARRAY_TASK_ID\n')
}



slurm_run_year <- function(scratch_dir, year_index) {

  #----------------------------------------------------------------------------
  # SLURM year phase: load context, run one year, save result to scratch dir.
  #
  # Each SLURM array task calls this with its SLURM_ARRAY_TASK_ID as
  # year_index. The function restores all globals needed by run_year_standalone,
  # processes the year, and saves the result to disk.
  #
  # Params:
  #   - scratch_dir (chr): Path to scratch directory
  #   - year_index (int): 1-based index into years vector
  #
  # Returns: nothing (side effects only; writes year result to scratch_dir/year_results/)
  #----------------------------------------------------------------------------

  cat('=== SLURM Year Phase ===\n')
  cat('  Year index:', year_index, '\n')
  cat('  Scratch dir:', scratch_dir, '\n\n')

  # Load run config and restore globals
  run_config <- readRDS(file.path(scratch_dir, 'run_config.rds'))
  restore_globals_from_config(run_config)

  # Load parallel context
  parallel_ctx <- readRDS(file.path(scratch_dir, 'parallel_ctx.rds'))

  # Determine year from index
  year <- run_config$years[year_index]
  if (is.na(year)) {
    stop(paste0('Error: year_index ', year_index, ' is out of range. ',
                'Valid range: 1-', length(run_config$years),
                ' (years: ', paste(run_config$years, collapse = ', '), ')'))
  }

  cat('  Running year:', year, '\n\n')

  # Run the year
  result <- run_year_standalone(year, parallel_ctx, scratch_dir)

  # Save result to scratch dir
  result_path <- file.path(scratch_dir, 'year_results', paste0('year_', year, '.rds'))
  saveRDS(result, result_path)

  cat('\nSLURM year phase complete.\n')
  cat('  Year:', year, '\n')
  cat('  Result saved to:', result_path, '\n')
}



slurm_finalize <- function(scratch_dir) {

  #----------------------------------------------------------------------------
  # SLURM finalize phase: load all year results, combine, and write output.
  #
  # Loads run_config to restore globals, loads all year result files from
  # scratch_dir/year_results/, combines them, and calls finalize_simulation().
  #
  # Params:
  #   - scratch_dir (chr): Path to scratch directory
  #
  # Returns: nothing (side effects only; writes output files)
  #----------------------------------------------------------------------------

  cat('=== SLURM Finalize Phase ===\n')
  cat('  Scratch dir:', scratch_dir, '\n\n')

  # Load run config and restore globals
  run_config <- readRDS(file.path(scratch_dir, 'run_config.rds'))
  restore_globals_from_config(run_config)

  # Load all year results
  year_result_files <- list.files(
    file.path(scratch_dir, 'year_results'),
    pattern = '^year_.*\\.rds$',
    full.names = TRUE
  )

  if (length(year_result_files) == 0) {
    stop('Error: no year result files found in ', file.path(scratch_dir, 'year_results'))
  }

  cat('  Found', length(year_result_files), 'year result files\n')
  cat('  Expected', length(run_config$years), 'years:', paste(run_config$years, collapse = ', '), '\n')

  if (length(year_result_files) != length(run_config$years)) {
    cat('  WARNING: number of result files does not match expected number of years!\n')
  }

  year_results <- lapply(year_result_files, readRDS)

  # Combine results from all workers
  summary_accumulators <- combine_year_results(
    year_results,
    run_config$baseline_id,
    run_config$counterfactual_ids
  )

  # Build sim_ctx for finalize_simulation
  sim_ctx <- list(
    baseline_id          = run_config$baseline_id,
    counterfactual_ids   = run_config$counterfactual_ids,
    years_to_run         = run_config$years,
    summary_accumulators = summary_accumulators
  )

  finalize_simulation(sim_ctx)

  cat('\nSLURM finalize phase complete.\n')
}



run_year_standalone <- function(year, ctx, scratch_dir = NULL) {

  #----------------------------------------------------------------------------
  # Processes a single simulation year as a pure function for parallel
  # execution. Runs baseline and all counterfactual scenarios for one year,
  # returning results without mutating ctx.
  #
  # Params:
  #   - year (int): Simulation year to process
  #   - ctx (list): Immutable parallel context containing baseline/counterfactual
  #       info, household data, supply/demand params, macro projections, and
  #       simulation configuration
  #   - scratch_dir (chr): Path to scratch directory for temporary file offloading
  #       (NULL = keep everything in memory)
  #
  # Returns: (list) With elements: year (int), baseline (list of summary
  #   tibbles), counterfactuals (named list of per-scenario result summaries)
  #----------------------------------------------------------------------------

  cat('\n========================================\n')
  cat('YEAR:', year, '(parallel worker)\n')
  cat('========================================\n')

  baseline_id          <- ctx$baseline_id
  baseline_info        <- ctx$baseline_info
  counterfactual_ids   <- ctx$counterfactual_ids
  counterfactual_infos <- ctx$counterfactual_infos
  run_fiscal_npv       <- isTRUE(ctx$fiscal_npv)

  yc <- prepare_year_context(
    sim_base_hh        = ctx$sim_base_hh,
    macro_projections  = ctx$macro_projections,
    base_supply_params = ctx$base_supply_params,
    base_demand_params = ctx$base_demand_params,
    baseline_info      = baseline_info,
    cached_price_wedge = ctx$cached_price_wedge,
    year               = year,
    aging_base_year    = ctx$aging_base_year,
    seed_offset        = seed_offset
  )
  supply_params     <- yc$supply_params
  demand_params     <- yc$demand_params
  parent_units_list <- yc$parent_units_list
  year_seed         <- yc$year_seed
  donor_pool        <- yc$donor_pool
  tax_sim_path      <- yc$tax_sim_path
  aged_spm                 <- yc$aged_spm
  aged_spm_parent_earnings <- yc$aged_spm_parent_earnings
  median_income_lookup     <- yc$median_income_lookup

  # B4: Free yc after extracting all fields
  rm(yc)

  # B1: Free sim_base_hh after prepare_year_context (~10-12GB)
  # Only pu_spm_xwalk is needed later (for poverty impact)
  pu_spm_xwalk <- ctx$sim_base_hh$pu_spm_xwalk
  ctx$sim_base_hh <- NULL
  gc()

  #---------------------------------
  # Compute employment targeting
  #---------------------------------

  employment_targeting_enabled <- !isTRUE(ctx$disable_employment_targeting)

  target_emp_rates <- NULL
  if (employment_targeting_enabled) {
    baseline_emp_rates_2019 <- compute_baseline_employment_rates(parent_units_list)
    demand_base_year <- as.integer(demand_params$year %||% 2019)

    growth_factor <- get_employment_rate_growth_factor(
      macro_projections = ctx$macro_projections,
      base_year = demand_base_year,
      target_year = year
    )

    target_emp_rates <- pmin(baseline_emp_rates_2019 * growth_factor, 0.999999)

    cat('  Employment targeting:\n')
    for (g in EMPLOYMENT_TARGETING_GROUPS) {
      cat(sprintf('    %s: target=%.4f\n', g, target_emp_rates[g]))
    }
  } else {
    cat('  Employment targeting: DISABLED\n')
  }

  cat('\n  --- Baseline ---\n')

  baseline_result <- run_scenario(
    scenario_info      = baseline_info,
    supply_params      = supply_params,
    demand_params      = demand_params,
    parent_units_list  = parent_units_list,
    year               = year,
    initial_prices     = INITIAL_PRICES,
    n_draws_per_record = ctx$n_draws_per_record,
    year_seed          = year_seed,
    macro_projections  = ctx$macro_projections,
    target_employment_rates = target_emp_rates
  )

  # Write employment targeting diagnostics
  if (baseline_result$converged && employment_targeting_enabled &&
      !is.null(baseline_result$employment_shifts) && !is.null(target_emp_rates)) {
    do_demand_policy <- load_demand_policy(baseline_info$equilibrium$policy_demand)
    do_cdctc_policy <- load_cdctc_policy(baseline_info$policy_cdctc %||% 'baseline')

    eq_output_dir <- file.path(baseline_info$paths$output, 'models', 'equilibrium')
    dir.create(eq_output_dir, showWarnings = FALSE, recursive = TRUE)

    achieved_rates <- setNames(rep(NA_real_, length(EMPLOYMENT_TARGETING_GROUPS)), EMPLOYMENT_TARGETING_GROUPS)
    for (g in EMPLOYMENT_TARGETING_GROUPS) {
      achieved_rates[g] <- compute_group_employment_rate(
        P = baseline_result$prices,
        group = g,
        parent_units_list = parent_units_list,
        demand_params = demand_params,
        employment_shifts = baseline_result$employment_shifts,
        policy_demand = do_demand_policy,
        policy_cdctc = do_cdctc_policy,
        cpi_growth_factor = supply_params$cpi_factor
      )
    }

    diag_df <- tibble(
      year = year,
      group = EMPLOYMENT_TARGETING_GROUPS,
      target_rate = target_emp_rates[EMPLOYMENT_TARGETING_GROUPS],
      achieved_rate = achieved_rates[EMPLOYMENT_TARGETING_GROUPS],
      delta_shift = baseline_result$employment_shifts[EMPLOYMENT_TARGETING_GROUPS],
      rate_gap = achieved_rates[EMPLOYMENT_TARGETING_GROUPS] - target_emp_rates[EMPLOYMENT_TARGETING_GROUPS]
    )

    output_file <- file.path(eq_output_dir, paste0('employment_targeting_', year, '.csv'))
    fwrite(diag_df, output_file, na = 'NA')

    cat('  Employment targeting diagnostics:\n')
    for (g in EMPLOYMENT_TARGETING_GROUPS) {
      cat(sprintf('    %s: target=%.4f, achieved=%.4f, delta=%.4f\n',
                  g, target_emp_rates[g], achieved_rates[g], baseline_result$employment_shifts[g]))
    }
  }

  baseline_summaries <- list(
    converged   = baseline_result$converged,
    prices      = baseline_result$prices,
    allocation  = aggregate_year_allocation(baseline_result, year),
    employment  = aggregate_year_employment(baseline_result, year),
    fiscal_cost = aggregate_year_fiscal_cost(baseline_result, year),
    median_income_thresholds = tibble(
      year = year,
      family_size_1 = median_income_lookup['1'],
      family_size_2 = median_income_lookup['2'],
      family_size_3 = median_income_lookup['3'],
      family_size_4 = median_income_lookup['4'],
      family_size_5_plus = median_income_lookup['5']
    )
  )

  counterfactual_results <- list()

  # B2: Write baseline parent_units to disk before counterfactual (~2-3GB + fragmentation relief)
  baseline_pu_path <- NULL
  if (!is.null(scratch_dir) && length(counterfactual_ids) > 0) {
    baseline_pu_path <- file.path(scratch_dir, 'year_results',
                                   paste0('baseline_pu_', year, '.rds'))
    saveRDS(baseline_result$parent_units, baseline_pu_path)
    baseline_result$parent_units <- NULL

    # B3: Strip unnecessary fields from baseline_result
    baseline_result$optim_result <- NULL
    baseline_result$Qd <- NULL
    baseline_result$L <- NULL
    baseline_result$actual_delta <- NULL
    baseline_result$target_delta <- NULL
    baseline_result$value <- NULL
    baseline_result$max_abs_delta_gap_paid <- NULL
    gc()
  }

  for (scenario_id in counterfactual_ids) {
    gc()
    counterfactual_results[[scenario_id]] <- run_counterfactual_standalone(
      scenario_id              = scenario_id,
      scenario_info            = counterfactual_infos[[scenario_id]],
      year                     = year,
      supply_params            = supply_params,
      demand_params            = demand_params,
      parent_units_list        = parent_units_list,
      baseline_result          = baseline_result,
      baseline_summaries       = baseline_summaries,
      aged_spm                 = aged_spm,
      aged_spm_parent_earnings = aged_spm_parent_earnings,
      tax_sim_path             = tax_sim_path,
      n_draws_per_record       = ctx$n_draws_per_record,
      year_seed                = year_seed,
      years_to_run             = ctx$years_to_run,
      pu_spm_xwalk             = pu_spm_xwalk,
      baseline_pu_path         = baseline_pu_path,
      baseline_info            = baseline_info,
      macro_projections        = ctx$macro_projections,
      target_employment_rates  = target_emp_rates,
      baseline_employment_shifts = baseline_result$employment_shifts,
      run_fiscal_npv           = run_fiscal_npv
    )
  }

  # Clean up temporary baseline parent_units file
  if (!is.null(baseline_pu_path) && file.exists(baseline_pu_path)) {
    file.remove(baseline_pu_path)
  }

  # Return all results for this year
  list(
    year             = year,
    baseline         = baseline_summaries,
    counterfactuals  = counterfactual_results
  )
}



run_counterfactual_standalone <- function(scenario_id, scenario_info, year,
                                          supply_params, demand_params,
                                          parent_units_list, baseline_result,
                                          baseline_summaries,
                                          aged_spm, aged_spm_parent_earnings,
                                          tax_sim_path, n_draws_per_record,
                                          year_seed, years_to_run,
                                          pu_spm_xwalk = NULL,
                                          baseline_pu_path = NULL,
                                          baseline_info, macro_projections,
                                          target_employment_rates = NULL,
                                          baseline_employment_shifts = NULL,
                                          run_fiscal_npv = FALSE) {

  #----------------------------------------------------------------------------
  # Processes a single counterfactual scenario as a pure function for parallel
  # execution. Computes mechanical fiscal effects, runs equilibrium, and
  # aggregates allocation, employment, fiscal, child earnings, distributional,
  # and poverty results.
  #
  # Params:
  #   - scenario_id (chr): Identifier for this counterfactual scenario
  #   - scenario_info (list): Scenario configuration (policy files, phase-in, etc.)
  #   - year (int): Simulation year to process
  #   - supply_params (list): Supply-side parameters for this year
  #   - demand_params (list): Demand-side parameters for this year
  #   - parent_units_list (list): Parent unit data frames by family type
  #   - baseline_result (list): Converged baseline scenario results (parent_units
  #       may be NULL if offloaded to disk via baseline_pu_path)
  #   - baseline_summaries (list): Aggregated baseline summary tibbles
  #   - aged_spm (df): Aged SPM unit data for poverty calculations
  #   - aged_spm_parent_earnings (df): Parent earnings linked to SPM units
  #   - tax_sim_path (chr): Path to tax simulator output
  #   - n_draws_per_record (int): Number of Gumbel draws per household record
  #   - year_seed (int): Random seed for this year
  #   - years_to_run (int vec): Full vector of simulation years
  #   - pu_spm_xwalk (df): Parent unit to SPM unit crosswalk (extracted from sim_base_hh)
  #   - baseline_pu_path (chr): Path to saved baseline parent_units RDS (NULL = use
  #       baseline_result$parent_units directly)
  #   - baseline_info (list): Baseline scenario configuration
  #   - macro_projections (list): Macro projection data (CPI, wages, etc.)
  #   - target_employment_rates (num vec): Target employment rates by group
  #   - baseline_employment_shifts (list): Employment shifts from baseline
  #   - run_fiscal_npv (logical): Whether to compute fiscal NPV
  #
  # Returns: (list) Named list of result summary tibbles including allocation,
  #   employment, fiscal_cost, mechanical_fiscal, child_earnings, distributional,
  #   and poverty results
  #----------------------------------------------------------------------------

  cat('\n  --- Counterfactual:', scenario_id, '---\n')

  # Load baseline parent_units from disk if offloaded (B2 memory optimization)
  baseline_pu_loaded <- FALSE
  if (is.null(baseline_result$parent_units) && !is.null(baseline_pu_path)) {
    baseline_result$parent_units <- readRDS(baseline_pu_path)
    baseline_pu_loaded <- TRUE
  }

  result_summaries <- list(
    converged                    = FALSE,
    allocation                   = tibble(),
    employment                   = tibble(),
    fiscal_cost                  = tibble(),
    mechanical_fiscal            = tibble(),
    child_earnings_overall       = tibble(),
    child_earnings_by_quintile   = tibble(),
    child_earnings_by_transition = tibble(),
    child_earnings_by_age        = tibble(),
    distributional               = tibble(),
    poverty                      = tibble()
  )

  first_year <- min(years_to_run)
  is_first_year <- (year == first_year)

  # Distributional impact runs at first year of full phase-in (or year 1 if no phase-in)
  dist_idx <- which(scenario_info$phase_in >= 1.0)[1]
  distributional_year <- years_to_run[dist_idx]
  is_distributional_year <- (year == distributional_year)

  # Compute MECHANICAL effect first
  if (baseline_result$converged) {
    if (isTRUE(scenario_info$employer_subsidy_config$enabled)) {
      config <- scenario_info$employer_subsidy_config
      if (config$type == 'percentage') {
        # Percentage-based: use baseline wages to compute dollar equivalent
        dollar_equiv <- baseline_result$w * config$rate
        employer_subsidy_rate <- as.vector(supply_params$L_req %*% dollar_equiv)
      } else {
        # Dollar-based: adjust for inflation
        subsidy_base_year <- config$base_year %||% 2019
        subsidy_growth_factor <- get_hourly_wage_growth_factor(macro_projections, subsidy_base_year, year)
        adjusted_subsidy <- config$subsidy * subsidy_growth_factor
        employer_subsidy_rate <- as.vector(supply_params$L_req %*% adjusted_subsidy)
      }
      covered <- config$covered_sectors %||% seq_along(employer_subsidy_rate)
      employer_subsidy_rate[!seq_along(employer_subsidy_rate) %in% covered] <- 0
    } else {
      employer_subsidy_rate <- c(0, 0, 0, 0)
    }

    mechanical_effect <- compute_mechanical_fiscal_effect(
      baseline_result       = baseline_result,
      policy_demand_file    = scenario_info$equilibrium$policy_demand,
      policy_supply_file    = scenario_info$equilibrium$policy_supply,
      year                  = year,
      policy_cdctc_file     = scenario_info$policy_cdctc %||% 'baseline',
      policy_tax_file       = scenario_info$policy_tax %||% 'baseline',
      employer_subsidy_rate = employer_subsidy_rate,
      demand_params         = demand_params,
      cpi_growth_factor     = supply_params$cpi_factor
    )

    result_summaries$mechanical_fiscal <- mechanical_effect
  }

  # Run full equilibrium (use INITIAL_PRICES - no warm-starting in parallel)
  result <- run_scenario(
    scenario_info      = scenario_info,
    supply_params      = supply_params,
    demand_params      = demand_params,
    parent_units_list  = parent_units_list,
    year               = year,
    initial_prices     = if (baseline_result$converged && !is.null(baseline_result$prices)) baseline_result$prices else INITIAL_PRICES,
    n_draws_per_record = n_draws_per_record,
    year_seed          = year_seed,
    macro_projections  = macro_projections,
    target_employment_rates = target_employment_rates,
    baseline_employment_shifts = baseline_employment_shifts
  )

  result_summaries$converged <- result$converged

  # Aggregate scenario results
  scenario_allocation <- aggregate_year_allocation(result, year)
  scenario_employment <- aggregate_year_employment(result, year)
  scenario_fiscal <- aggregate_year_fiscal_cost(result, year)

  result_summaries$allocation <- scenario_allocation
  result_summaries$employment <- scenario_employment
  result_summaries$fiscal_cost <- scenario_fiscal

  # Child earnings impact (first year only, requires --fiscal-npv flag)
  if (run_fiscal_npv && is_first_year && result$converged && baseline_result$converged) {
    child_earnings_micro <- calculate_child_earnings(
      baseline_result   = baseline_result,
      policy_result     = result,
      year              = year,
      tax_data_path     = baseline_info$paths$`Tax-Data`,
      macro_projections = macro_projections
    )

    if (nrow(child_earnings_micro) > 0) {
      child_earnings_agg <- aggregate_child_earnings(child_earnings_micro)
      result_summaries$child_earnings_overall <- child_earnings_agg$overall
      result_summaries$child_earnings_by_quintile <- child_earnings_agg$by_quintile
      result_summaries$child_earnings_by_transition <- child_earnings_agg$by_transition
      result_summaries$child_earnings_by_age <- child_earnings_agg$by_age_group

      # Calculate fiscal NPV while we have the microdata
      # Get fiscal costs by child age for proper NPV compounding
      baseline_fiscal_by_age <- aggregate_year_fiscal_cost_by_age(baseline_result, year)
      policy_fiscal_by_age <- aggregate_year_fiscal_cost_by_age(result, year)

      child_fiscal_npv <- calculate_child_fiscal_npv(
        child_earnings_micro    = child_earnings_micro,
        baseline_fiscal         = baseline_summaries$fiscal_cost,
        policy_fiscal           = scenario_fiscal,
        baseline_fiscal_by_age  = baseline_fiscal_by_age,
        policy_fiscal_by_age    = policy_fiscal_by_age,
        year                    = year,
        tax_data_path           = baseline_info$paths$`Tax-Data`,
        macro_projections       = macro_projections,
        tax_sim_full_mtr_path   = baseline_info$paths$`Tax-Simulator-Full-MTR`
      )
      result_summaries$fiscal_npv <- child_fiscal_npv$summary
      result_summaries$fiscal_npv_by_quintile <- child_fiscal_npv$by_quintile
    }
  }

  # Distributional impact (at full phase-in year)
  if (is_distributional_year && result$converged && baseline_result$converged) {
    distributional_impact <- aggregate_year_distributional_impact(
      baseline_result    = baseline_result,
      policy_result      = result,
      policy_demand_file = scenario_info$equilibrium$policy_demand,
      policy_supply_file = scenario_info$equilibrium$policy_supply,
      policy_cdctc_file  = scenario_info$policy_cdctc %||% 'baseline',
      policy_tax_file    = scenario_info$policy_tax %||% 'baseline',
      year               = year,
      tax_sim_path       = tax_sim_path,
      demand_params      = demand_params,
      cpi_growth_factor  = supply_params$cpi_factor
    )
    # Deflate dollar amounts to year-1 dollars using CPI-U
    if (distributional_year != first_year) {
      cpi_deflator <- get_cpi_growth_factor(macro_projections, first_year, distributional_year)
      distributional_impact <- distributional_impact %>%
        mutate(
          avg_baseline_net_income = avg_baseline_net_income / cpi_deflator,
          avg_mechanical_income_change = avg_mechanical_income_change / cpi_deflator,
          avg_welfare_income_change = avg_welfare_income_change / cpi_deflator,
          avg_total_income_change = avg_total_income_change / cpi_deflator
        )
    }

    result_summaries$distributional <- distributional_impact
  }

  # Poverty impact (first year only)
  if (is_first_year && result$converged && baseline_result$converged) {
    if (!is.null(aged_spm) && !is.null(pu_spm_xwalk)) {
      poverty_impact <- aggregate_year_poverty_impact(
        baseline_result        = baseline_result,
        policy_result          = result,
        policy_demand_file     = scenario_info$equilibrium$policy_demand,
        policy_cdctc_file      = scenario_info$policy_cdctc %||% 'baseline',
        policy_tax_file        = scenario_info$policy_tax %||% 'baseline',
        year                   = year,
        aged_spm               = aged_spm,
        pu_spm_xwalk           = pu_spm_xwalk,
        spm_parent_earnings    = aged_spm_parent_earnings,
        demand_params          = demand_params,
        cpi_growth_factor      = supply_params$cpi_factor
      )

      if (nrow(poverty_impact) > 0) {
        result_summaries$poverty <- poverty_impact
      }
    }
  }

  # Write price deltas
  if (result$converged && baseline_result$converged) {
    write_price_deltas(
      baseline_result = baseline_result,
      result          = result,
      year            = year,
      scenario_info   = scenario_info
    )
  }

  # Free baseline parent_units if we loaded them from disk
  if (baseline_pu_loaded) {
    baseline_result$parent_units <- NULL
  }

  # Free full scenario result to reduce heap fragmentation between counterfactuals
  rm(result)
  gc()

  result_summaries
}



combine_year_results <- function(year_results, baseline_id, counterfactual_ids) {

  #----------------------------------------------------------------------------
  # Combines results from parallel year processing into summary accumulators.
  # Initializes empty accumulator structures for baseline and counterfactual
  # scenarios, then iterates through all year results to bind rows into
  # the appropriate accumulator fields.
  #
  # Params:
  #   - year_results (list): List of per-year result lists, each containing
  #       baseline summaries and counterfactual results
  #   - baseline_id (chr): Scenario ID for the baseline
  #   - counterfactual_ids (chr vec): Scenario IDs for counterfactuals
  #
  # Returns: (list) Named list of summary accumulators keyed by scenario ID,
  #   each containing tibbles of bound results across all years
  #----------------------------------------------------------------------------

  # -- Inlined: initialize_summary_accumulators() ---
  summary_accumulators <- {
    empty_accum <- function(fields) setNames(lapply(fields, function(x) tibble()), fields)

    baseline_fields <- c('allocation', 'employment', 'fiscal_cost', 'median_income_thresholds')
    cf_fields <- c('allocation', 'employment', 'fiscal_cost', 'mechanical_fiscal',
                   'child_earnings_overall', 'child_earnings_by_quintile',
                   'child_earnings_by_transition', 'child_earnings_by_age',
                   'fiscal_npv', 'fiscal_npv_by_quintile', 'distributional', 'poverty')

    accumulators <- list()
    accumulators[[baseline_id]] <- empty_accum(baseline_fields)
    for (scenario_id in counterfactual_ids) {
      accumulators[[scenario_id]] <- empty_accum(cf_fields)
    }

    accumulators
  }

  for (yr_result in year_results) {
    summary_accumulators[[baseline_id]]$allocation <- bind_rows(
      summary_accumulators[[baseline_id]]$allocation,
      yr_result$baseline$allocation
    )
    summary_accumulators[[baseline_id]]$employment <- bind_rows(
      summary_accumulators[[baseline_id]]$employment,
      yr_result$baseline$employment
    )
    summary_accumulators[[baseline_id]]$fiscal_cost <- bind_rows(
      summary_accumulators[[baseline_id]]$fiscal_cost,
      yr_result$baseline$fiscal_cost
    )
    summary_accumulators[[baseline_id]]$median_income_thresholds <- bind_rows(
      summary_accumulators[[baseline_id]]$median_income_thresholds,
      yr_result$baseline$median_income_thresholds
    )

    for (scenario_id in counterfactual_ids) {
      cf_result <- yr_result$counterfactuals[[scenario_id]]
      if (is.null(cf_result)) next

      acc <- summary_accumulators[[scenario_id]]
      for (field in names(cf_result)) {
        if (!is.data.frame(cf_result[[field]])) next
        acc[[field]] <- bind_rows(acc[[field]], cf_result[[field]])
      }
      summary_accumulators[[scenario_id]] <- acc
    }
  }

  summary_accumulators
}
