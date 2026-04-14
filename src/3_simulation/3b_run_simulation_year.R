#------------------------------------------------------------------------------
# 3b_run_simulation_year.R
#
# Year-level simulation orchestration: age data, prepare parent units,
# run scenarios (baseline + counterfactual), accumulate results.
# Primary function: run_simulation_year()
#------------------------------------------------------------------------------



get_gdp_per_capita_growth_factor <- function(macro_projections, base_year, target_year) {

  #----------------------------------------------------------------------------
  # Compute GDP per capita growth factor between base and target year.
  # Used by age_economics and age_spm_units for income aging.
  #
  # Params:
  #   - macro_projections (df): macro projections with year, gdp, population cols
  #   - base_year (int): starting year for growth calculation
  #   - target_year (int): ending year for growth calculation
  #
  # Returns: (dbl) ratio of GDP per capita in target_year vs base_year
  #----------------------------------------------------------------------------

  if (target_year <= base_year) {
    return(1.0)
  }
  pop_factor <- macro_projections %>%
    filter(year %in% c(base_year, target_year)) %>%
    select(year, contains('married_'), contains('unmarried_')) %>%
    pivot_longer(cols = -year) %>%
    group_by(year) %>%
    summarise(pop = sum(value), .groups = 'drop') %>%
    mutate(pop = pop / lag(pop)) %>%
    tail(1) %>%
    pull(pop)
  econ_factors <- macro_projections %>%
    filter(year %in% c(base_year, target_year)) %>%
    select(year, gdp) %>%
    mutate(gdp = gdp / lag(gdp)) %>%
    tail(1)
  return(econ_factors$gdp / pop_factor)
}



run_simulation_year <- function(sim_ctx, year) {

  #----------------------------------------------------------------------------
  # Run all scenarios (baseline + counterfactuals) for a single simulation
  # year. Ages microdata, prepares parent units, solves equilibrium for each
  # scenario, and accumulates year-level results into summary accumulators.
  #
  # Params:
  #   - sim_ctx (list): simulation context from initialize_simulation
  #   - year (int): simulation year to run
  #
  # Returns: (list) updated simulation context with accumulated results
  #----------------------------------------------------------------------------

  # Accumulate summary fields into sim_ctx accumulators
  accumulate <- function(scenario_id, summaries) {

    #--------------------------------------------------------------------------
    # Append summary tibbles to the sim_ctx accumulators for a given scenario.
    # Uses <<- to modify the parent environment sim_ctx in place.
    #
    # Params:
    #   - scenario_id (chr): scenario identifier (e.g., 'baseline')
    #   - summaries (list): named list of summary tibbles to accumulate
    #
    # Returns: nothing (side effects only)
    #--------------------------------------------------------------------------

    for (field in names(summaries)) {
      sim_ctx$summary_accumulators[[scenario_id]][[field]] <<- bind_rows(
        sim_ctx$summary_accumulators[[scenario_id]][[field]],
        summaries[[field]]
      )
    }
  }

  cat('\n========================================\n')
  cat('YEAR:', year, '\n')
  cat('========================================\n')

  baseline_id          <- sim_ctx$baseline_id
  baseline_info        <- sim_ctx$baseline_info
  counterfactual_ids   <- sim_ctx$counterfactual_ids
  counterfactual_infos <- sim_ctx$counterfactual_infos

  yc <- prepare_year_context(
    sim_base_hh        = sim_ctx$sim_base_hh,
    macro_projections  = sim_ctx$macro_projections,
    base_supply_params = sim_ctx$base_supply_params,
    base_demand_params = sim_ctx$base_demand_params,
    baseline_info      = baseline_info,
    cached_price_wedge = sim_ctx$cached_price_wedge,
    year               = year,
    aging_base_year    = acs_base_year,
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

  # Free yc after extracting all fields
  rm(yc)
  gc()

  #---------------------------------
  # Compute employment targeting
  #---------------------------------

  # Check if employment targeting is disabled via CLI flag
  employment_targeting_enabled <- !(exists('disable_employment_targeting') &&
                                     isTRUE(disable_employment_targeting))

  target_emp_rates <- NULL
  if (employment_targeting_enabled) {
    # Targets are group-specific primary-caregiver employment rates:
    #   target_g(year) = emp_rate_g(2019) * overall_emp_growth(year vs 2019)
    # Note: RF probabilities (p_employment.pt, p_employment.ft) are from 2019
    # calibration and don't change by simulation year, so we can compute baseline
    # rates from any year's parent_units data.
    demand_base_year <- as.integer(sim_ctx$base_demand_params$year %||% 2019)

    if (is.null(sim_ctx$baseline_emp_rates_2019)) {
      sim_ctx$baseline_emp_rates_2019 <- compute_baseline_employment_rates(parent_units_list)
    }

    growth_factor <- get_employment_rate_growth_factor(
      macro_projections = sim_ctx$macro_projections,
      base_year = demand_base_year,
      target_year = year
    )

    target_emp_rates <- pmin(sim_ctx$baseline_emp_rates_2019 * growth_factor, 0.999999)

    cat('  Employment targeting:\n')
    for (g in EMPLOYMENT_TARGETING_GROUPS) {
      cat(sprintf('    %s: target=%.4f\n', g, target_emp_rates[g]))
    }
  } else {
    cat('  Employment targeting: DISABLED\n')
  }

  #--------------
  # Run baseline
  #--------------

  cat('\n  --- Baseline ---\n')

  baseline_result <- run_scenario(
    scenario_info      = baseline_info,
    supply_params      = supply_params,
    demand_params      = demand_params,
    parent_units_list  = parent_units_list,
    year               = year,
    initial_prices     = sim_ctx$prev_baseline_prices,
    n_draws_per_record = n_draws_per_record,
    year_seed          = year_seed,
    macro_projections  = sim_ctx$macro_projections,
    target_employment_rates = target_emp_rates
  )

  if (baseline_result$converged) {
    sim_ctx$prev_baseline_prices <- baseline_result$prices

    # Write employment targeting diagnostics to CSV
    if (employment_targeting_enabled &&
        !is.null(baseline_result$employment_shifts) && !is.null(target_emp_rates)) {
      do_demand_policy <- load_demand_policy(baseline_info$equilibrium$policy_demand)
      do_cdctc_policy <- load_cdctc_policy(baseline_info$policy_cdctc %||% 'baseline')

      # Create output directory if needed
      eq_output_dir <- file.path(baseline_info$paths$output, 'models', 'equilibrium')
      dir.create(eq_output_dir, showWarnings = FALSE, recursive = TRUE)

      # Compute achieved employment rates at final solution
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

      # Build diagnostics dataframe
      diag_df <- tibble(
        year = year,
        group = EMPLOYMENT_TARGETING_GROUPS,
        target_rate = target_emp_rates[EMPLOYMENT_TARGETING_GROUPS],
        achieved_rate = achieved_rates[EMPLOYMENT_TARGETING_GROUPS],
        delta_shift = baseline_result$employment_shifts[EMPLOYMENT_TARGETING_GROUPS],
        rate_gap = achieved_rates[EMPLOYMENT_TARGETING_GROUPS] - target_emp_rates[EMPLOYMENT_TARGETING_GROUPS]
      )

      # Write CSV
      output_file <- file.path(eq_output_dir, paste0('employment_targeting_', year, '.csv'))
      fwrite(diag_df, output_file, na = 'NA')

      # Print summary
      cat('  Employment targeting diagnostics:\n')
      for (g in EMPLOYMENT_TARGETING_GROUPS) {
        cat(sprintf('    %s: target=%.4f, achieved=%.4f, delta=%.4f\n',
                    g, target_emp_rates[g], achieved_rates[g], baseline_result$employment_shifts[g]))
      }
    }
  }

  # Aggregate summaries
  baseline_year_summaries <- list(
    allocation               = aggregate_year_allocation(baseline_result, year),
    employment               = aggregate_year_employment(baseline_result, year),
    fiscal_cost              = aggregate_year_fiscal_cost(baseline_result, year),
    median_income_thresholds = tibble(
      year = year,
      family_size_1 = median_income_lookup['1'],
      family_size_2 = median_income_lookup['2'],
      family_size_3 = median_income_lookup['3'],
      family_size_4 = median_income_lookup['4'],
      family_size_5_plus = median_income_lookup['5']
    )
  )
  accumulate(baseline_id, baseline_year_summaries)

  sim_ctx$baseline_results[[as.character(year)]] <- NULL
  if (baseline_result$converged) {
    sim_ctx$baseline_results[[as.character(year)]] <- baseline_result
  }

  #---------------------
  # Run counterfactuals
  #---------------------

  for (scenario_id in counterfactual_ids) {
    gc()

    {
      cat('\n  --- Counterfactual:', scenario_id, '---\n')

      scenario_info <- sim_ctx$counterfactual_infos[[scenario_id]]
      first_year <- min(sim_ctx$years_to_run)
      is_first_year <- (year == first_year)

      # Distributional impact runs at first year of full phase-in (or year 1 if no phase-in)
      dist_idx <- which(scenario_info$phase_in >= 1.0)[1]
      distributional_year <- sim_ctx$years_to_run[dist_idx]
      is_distributional_year <- (year == distributional_year)

      run_fiscal_npv <- exists('fiscal_npv') && fiscal_npv
      baseline_year_result <- sim_ctx$baseline_results[[as.character(year)]]

      # Compute MECHANICAL effect first (policy cost with no behavioral response)
      if (baseline_result$converged) {
        # Compute employer subsidy rate for mechanical effect
        # Rate = L_req %*% subsidy ($/hour of care by sector)
        if (isTRUE(scenario_info$employer_subsidy_config$enabled)) {
          config <- scenario_info$employer_subsidy_config
          if (config$type == 'percentage') {
            # Percentage-based: use baseline wages to compute dollar equivalent
            dollar_equiv <- baseline_result$w * config$rate
            employer_subsidy_rate <- as.vector(supply_params$L_req %*% dollar_equiv)
          } else {
            # Dollar-based: adjust for inflation
            subsidy_base_year <- config$base_year %||% 2019
            subsidy_growth_factor <- get_hourly_wage_growth_factor(sim_ctx$macro_projections, subsidy_base_year, year)
            adjusted_subsidy <- config$subsidy * subsidy_growth_factor
            employer_subsidy_rate <- as.vector(supply_params$L_req %*% adjusted_subsidy)
          }
          # Zero out uncovered sectors
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

        accumulate(scenario_id, list(mechanical_fiscal = mechanical_effect))
      }

      # Run full equilibrium for counterfactual
      initial_prices_cf <- sim_ctx$prev_counterfactual_prices[[scenario_id]]
      if (baseline_result$converged &&
          !is.null(baseline_result$prices) &&
          isTRUE(all.equal(initial_prices_cf, INITIAL_PRICES))) {
        # Warm-start counterfactual from baseline equilibrium in the same year
        initial_prices_cf <- baseline_result$prices
      }

      result <- run_scenario(
        scenario_info      = scenario_info,
        supply_params      = supply_params,
        demand_params      = demand_params,
        parent_units_list  = parent_units_list,
        year               = year,
        initial_prices     = initial_prices_cf,
        n_draws_per_record = n_draws_per_record,
        year_seed          = year_seed,
        macro_projections  = sim_ctx$macro_projections,
        target_employment_rates = target_emp_rates,
        baseline_employment_shifts = baseline_result$employment_shifts
      )

      # Update warm start for next year
      if (result$converged) {
        sim_ctx$prev_counterfactual_prices[[scenario_id]] <- result$prices
      }

      # Aggregate scenario results
      scenario_allocation <- aggregate_year_allocation(result, year)
      scenario_employment <- aggregate_year_employment(result, year)
      scenario_fiscal <- aggregate_year_fiscal_cost(result, year)

      accumulate(scenario_id, list(
        allocation = scenario_allocation,
        employment = scenario_employment,
        fiscal_cost = scenario_fiscal
      ))

      # Compute child earnings impact (first year only, requires --fiscal-npv flag)
      if (run_fiscal_npv && is_first_year && result$converged && !is.null(baseline_year_result)) {
        child_earnings_micro <- calculate_child_earnings(
          baseline_result   = baseline_year_result,
          policy_result     = result,
          year              = year,
          tax_data_path     = sim_ctx$baseline_info$paths$`Tax-Data`,
          macro_projections = sim_ctx$macro_projections
        )

        if (nrow(child_earnings_micro) > 0) {
          child_earnings_agg <- aggregate_child_earnings(child_earnings_micro)

          accumulate(scenario_id, list(
            child_earnings_overall       = child_earnings_agg$overall,
            child_earnings_by_quintile   = child_earnings_agg$by_quintile,
            child_earnings_by_transition = child_earnings_agg$by_transition,
            child_earnings_by_age        = child_earnings_agg$by_age_group
          ))

          # Calculate fiscal NPV using raw fiscal data (not phase-in adjusted)
          baseline_fiscal <- aggregate_year_fiscal_cost(baseline_year_result, year)
          policy_fiscal <- aggregate_year_fiscal_cost(result, year)

          baseline_fiscal_by_age <- aggregate_year_fiscal_cost_by_age(baseline_year_result, year)
          policy_fiscal_by_age <- aggregate_year_fiscal_cost_by_age(result, year)

          child_fiscal_npv <- calculate_child_fiscal_npv(
            child_earnings_micro    = child_earnings_micro,
            baseline_fiscal         = baseline_fiscal,
            policy_fiscal           = policy_fiscal,
            baseline_fiscal_by_age  = baseline_fiscal_by_age,
            policy_fiscal_by_age    = policy_fiscal_by_age,
            year                    = year,
            tax_data_path           = sim_ctx$baseline_info$paths$`Tax-Data`,
            macro_projections       = sim_ctx$macro_projections,
            tax_sim_full_mtr_path   = sim_ctx$baseline_info$paths$`Tax-Simulator-Full-MTR`
          )

          accumulate(scenario_id, list(
            fiscal_npv = child_fiscal_npv$summary,
            fiscal_npv_by_quintile = child_fiscal_npv$by_quintile
          ))
        }
      }

      # Compute distributional impact (at full phase-in year)
      if (is_distributional_year && result$converged && !is.null(baseline_year_result)) {
        distributional_impact <- aggregate_year_distributional_impact(
          baseline_result    = baseline_year_result,
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
          cpi_deflator <- get_cpi_growth_factor(sim_ctx$macro_projections, first_year, distributional_year)
          distributional_impact <- distributional_impact %>%
            mutate(
              avg_baseline_net_income = avg_baseline_net_income / cpi_deflator,
              avg_mechanical_income_change = avg_mechanical_income_change / cpi_deflator,
              avg_welfare_income_change = avg_welfare_income_change / cpi_deflator,
              avg_total_income_change = avg_total_income_change / cpi_deflator
            )
        }

        accumulate(scenario_id, list(distributional = distributional_impact))
      }

      # Compute poverty impact (first year only)
      if (is_first_year && result$converged && !is.null(baseline_year_result)) {
        if (!is.null(aged_spm) && !is.null(sim_ctx$sim_base_hh$pu_spm_xwalk)) {
          poverty_impact <- aggregate_year_poverty_impact(
            baseline_result        = baseline_year_result,
            policy_result          = result,
            policy_demand_file     = scenario_info$equilibrium$policy_demand,
            policy_cdctc_file      = scenario_info$policy_cdctc %||% 'baseline',
            policy_tax_file        = scenario_info$policy_tax %||% 'baseline',
            year                   = year,
            aged_spm               = aged_spm,
            pu_spm_xwalk           = sim_ctx$sim_base_hh$pu_spm_xwalk,
            spm_parent_earnings    = aged_spm_parent_earnings,
            demand_params          = demand_params,
            cpi_growth_factor      = supply_params$cpi_factor
          )

          if (nrow(poverty_impact) > 0) {
            accumulate(scenario_id, list(poverty = poverty_impact))
          }
        }
      }

      if (result$converged && !is.null(baseline_year_result)) {
        write_price_deltas(
          baseline_result = baseline_year_result,
          result          = result,
          year            = year,
          scenario_info   = scenario_info
        )
      }
    }
  }

  return(sim_ctx)
}



age_microdata_to_year <- function(exogenous_hh, macro_projections, target_year, acs_base_year) {

  #----------------------------------------------------------------------------
  # Age exogenous household microdata from base year to target year by
  # applying demographic and economic growth factors.
  #
  # Params:
  #   - exogenous_hh (list): household microdata tables from base year
  #   - macro_projections (df): macro projections for growth factors
  #   - target_year (int): year to age data to
  #   - acs_base_year (int): ACS base year of the microdata
  #
  # Returns: (list) household microdata tables aged to target year
  #----------------------------------------------------------------------------

  # Apply demographic and economic aging
  aged_hh <- exogenous_hh %>%
    age_demographics(macro_projections, target_year, acs_base_year) %>%
    age_economics(macro_projections, target_year, acs_base_year)

  return(aged_hh)
}



age_demographics <- function(processed_acs, macro_projections, target_year, acs_base_year) {

  #----------------------------------------------------------------------------
  # Apply demographic growth factors to microdata weights for a given target
  # year. Adjusts household, person, parent unit, child, and tax unit weights
  # based on projected population changes by age and marital status.
  #
  # Params:
  #   - processed_acs (list): processed ACS microdata tables
  #   - macro_projections (df): macro projections with demographic columns
  #   - target_year (int): year to project weights to
  #   - acs_base_year (int): ACS base year of the microdata
  #
  # Returns: (list) microdata tables with updated weights
  #----------------------------------------------------------------------------

  # Skip base year
  if (target_year <= acs_base_year) {
    return(processed_acs)
  }

  # Extract and process demographic projections
  demog <- macro_projections %>%
    select(year, contains('married_')) %>%
    pivot_longer(
      cols      = -year,
      names_sep = '_',
      names_to  = c('married', 'age'),
      values_to = 'n'
    ) %>%
    mutate(
      married = as.integer(married == 'married'),
      age     = as.integer(age)
    ) %>%
    group_by(year, married, age) %>%
    summarise(n = sum(n), .groups = 'drop')

  # Calculate demographic growth factors for this year
  growth_factors <- demog %>%
    filter(year %in% c(acs_base_year, target_year)) %>%
    mutate(year = if_else(year == target_year, 'target', 'base')) %>%
    pivot_wider(
      names_from  = year,
      values_from = n
    ) %>%
    mutate(growth_factor = target / base) %>%

    # Give under-16 married records population average
    filter(age > 15 | married == 0) %>%
    bind_rows(
      (.) %>%
        summarise(growth_factor = weighted.mean(growth_factor, base)) %>%
        expand_grid(age = 0:15, married = 1)
    ) %>%
    select(age, married, growth_factor)

  # Attach factors to all household members
  person_growth_factors = processed_acs$household_members %>%
    left_join(growth_factors, by = c('age', 'married')) %>%
    select(ends_with('_id'), growth_factor)

  # Aggregate over households and tax units
  hh_growth_factors = person_growth_factors %>%
    group_by(hh_id) %>%
    summarise(growth_factor = mean(growth_factor))
  tax_unit_growth_factors = person_growth_factors %>%
    group_by(tax_unit_id) %>%
    summarise(growth_factor = mean(growth_factor))

  # Apply growth factors to households
  processed_acs$households <- processed_acs$households %>%
    left_join(hh_growth_factors, by = 'hh_id') %>%
    mutate(hh_weight = hh_weight * growth_factor) %>%
    select(-growth_factor)

  # Apply growth factors to household_members
  processed_acs$household_members <- processed_acs$household_members %>%
    mutate(per_weight = per_weight * person_growth_factors$growth_factor)

  # Apply person-specific growth factors to parent_units
  processed_acs$parent_units <- processed_acs$parent_units %>%
    left_join(
      person_growth_factors %>%
        filter(!is.na(hhm_id)) %>%
        select(hh_id, hhm_id1 = hhm_id, growth_factor1 = growth_factor),
      by = c('hh_id', 'hhm_id1')
    ) %>%
    left_join(
      person_growth_factors %>%
        filter(!is.na(hhm_id)) %>%
        select(hh_id, hhm_id2 = hhm_id, growth_factor2 = growth_factor),
      by = c('hh_id', 'hhm_id2')
    ) %>%
    mutate(
      per_weight1 = per_weight1 * growth_factor1,
      per_weight2 = per_weight2 * growth_factor2
    ) %>%
    select(-starts_with('growth_factor'))

  # Apply growth factors to children
  processed_acs$children <- processed_acs$children %>%
    left_join(
      person_growth_factors %>%
        filter(!is.na(child_id)) %>%
        select(hh_id, child_id, growth_factor),
      by = c('hh_id', 'child_id')
    ) %>%
    mutate(child_weight = child_weight * growth_factor) %>%
    select(-growth_factor)

  # Apply growth factors to tax_units
  processed_acs$tax_units <- processed_acs$tax_units %>%
    left_join(tax_unit_growth_factors, by = 'tax_unit_id') %>%
    mutate(tax_unit_weight = tax_unit_weight * growth_factor) %>%
    select(-growth_factor)

  return(processed_acs)
}



age_economics <- function(processed_acs, macro_projections, target_year, acs_base_year) {

  #----------------------------------------------------------------------------
  # Apply economic growth factors to microdata income and earnings fields.
  # Uses hourly wage growth for earnings variables and GDP per capita growth
  # for AGI and household income.
  #
  # Params:
  #   - processed_acs (list): processed ACS microdata tables
  #   - macro_projections (df): macro projections with gdp and wage data
  #   - target_year (int): year to project income/earnings to
  #   - acs_base_year (int): ACS base year of the microdata
  #
  # Returns: (list) microdata tables with updated income and earnings
  #----------------------------------------------------------------------------

  # Skip base year
  if (target_year <= acs_base_year) {
    return(processed_acs)
  }

  # Calculate growth factors
  hourly_wage_growth <- get_hourly_wage_growth_factor(macro_projections, acs_base_year, target_year)
  income_growth_factor <- get_gdp_per_capita_growth_factor(macro_projections, acs_base_year, target_year)

  # Apply nominal GDP per capita growth to household total income
  processed_acs$households <- processed_acs$households %>%
    mutate(income = income * income_growth_factor)

  # Apply hourly wage growth to earnings, GDP per capita growth to AGI
  processed_acs$parent_units <- processed_acs$parent_units %>%
    mutate(
      # Wage variables: use hourly wage growth
      earnings1 = earnings1 * hourly_wage_growth,
      earnings2 = earnings2 * hourly_wage_growth,
      hourly_earnings1 = hourly_earnings1 * hourly_wage_growth,
      hourly_earnings2 = hourly_earnings2 * hourly_wage_growth,
      # AGI: use nominal GDP per capita growth
      agi1 = agi1 * income_growth_factor,
      agi2 = agi2 * income_growth_factor
    )

  # Apply nominal GDP per capita growth to tax unit AGI
  processed_acs$tax_units <- processed_acs$tax_units %>%
    mutate(agi = agi * income_growth_factor)

  return(processed_acs)
}



age_spm_units <- function(spm_units, macro_projections, target_year, acs_base_year) {

  #----------------------------------------------------------------------------
  # Age SPM unit data by applying growth factors: GDP per capita for resources
  # and expenses, CPI for thresholds, and hourly wage growth for earnings.
  #
  # Params:
  #   - spm_units (df): SPM unit data with resources, thresholds, expenses
  #   - macro_projections (df): macro projections for growth factors
  #   - target_year (int): year to age data to
  #   - acs_base_year (int): ACS base year of the microdata
  #
  # Returns: (df) tibble of aged SPM units with updated monetary values
  #----------------------------------------------------------------------------

  # No aging needed for base year
  if (target_year <= acs_base_year) {
    return(spm_units)
  }

  # Get growth factors
  income_growth <- get_gdp_per_capita_growth_factor(macro_projections, acs_base_year, target_year)
  hourly_wage_growth <- get_hourly_wage_growth_factor(macro_projections, acs_base_year, target_year)
  cpi_growth <- get_cpi_growth_factor(macro_projections, acs_base_year, target_year)

  # Age monetary values
  spm_units %>%
    mutate(
      # Resources scale with nominal GDP per capita
      spm_resources = spm_resources * income_growth,

      # Threshold scales with CPI (poverty line tracks inflation)
      spm_threshold = spm_threshold * cpi_growth,

      # Expense amounts scale with nominal GDP per capita
      spm_childcare_exp = spm_childcare_exp * income_growth,
      spm_work_exp = spm_work_exp * income_growth,
      spm_capped_exp = spm_capped_exp * income_growth,

      # Scale the expense cap reference by hourly wage growth (pure wage variable)
      min_adult_earnings = min_adult_earnings * hourly_wage_growth,

      # Recompute baseline poverty status
      is_poor_baseline = (spm_resources < spm_threshold)
    )
}



age_spm_parent_earnings <- function(spm_parent_earnings, macro_projections, target_year, acs_base_year) {

  #----------------------------------------------------------------------------
  # Age SPM parent earnings by applying hourly wage growth factor from base
  # year to target year.
  #
  # Params:
  #   - spm_parent_earnings (df): SPM parent earnings with baseline_earnings col
  #   - macro_projections (df): macro projections for wage growth
  #   - target_year (int): year to age earnings to
  #   - acs_base_year (int): ACS base year of the microdata
  #
  # Returns: (df) tibble with aged baseline_earnings column
  #----------------------------------------------------------------------------

  if (is.null(spm_parent_earnings) || nrow(spm_parent_earnings) == 0) {
    return(spm_parent_earnings)
  }

  # No aging needed for base year
  if (target_year <= acs_base_year) {
    return(spm_parent_earnings)
  }

  # Get hourly wage growth factor and scale earnings
  hourly_wage_growth <- get_hourly_wage_growth_factor(macro_projections, acs_base_year, target_year)

  spm_parent_earnings %>%
    mutate(baseline_earnings = baseline_earnings * hourly_wage_growth)
}



prepare_parent_units <- function(parent_units, children, households, donor_pool,
                                 pct_thresholds, wage_thresholds, tax_max,
                                 cached_price_wedge) {

  #----------------------------------------------------------------------------
  # Prepare parent units in wide format for equilibrium solving. Matches tax
  # donors, computes taxes, joins children, splits 3+ child families into
  # pseudofamilies, computes demographic groups, and pivots to wide format.
  #
  # Params:
  #   - parent_units (df): parent unit microdata with earnings and AGI
  #   - children (df): child microdata with age and weights
  #   - households (df): household data with region
  #   - donor_pool (df): tax donor pool for statistical matching
  #   - pct_thresholds (df): AGI percentile thresholds for donor matching
  #   - wage_thresholds (df): wage thresholds for EMTR lookup
  #   - tax_max (dbl): OASI tax maximum for the year
  #   - cached_price_wedge (df or NULL): pre-computed price wedge values
  #
  # Returns: (list) named list with c1 and c2plus parent unit dataframes
  #   in wide format ready for equilibrium solving
  #----------------------------------------------------------------------------


  parent_units_with_taxes <- parent_units %>%
    match_tax_donors(donor_pool, pct_thresholds) %>%
    calculate_taxes_with_donors(donor_pool, wage_thresholds, tax_max) %>%
    mutate(
      agi          = agi1 + replace_na(agi2, 0),
      total_tax    = liab_iit + liab_payroll,
      income_tax   = liab_iit,
      oasi_tax_max = tax_max,
      hours_pc     = if_else(primary_caregiver == 1, hours1, hours2),
      hours_secondary = if_else(primary_caregiver == 1, hours2, hours1)
    )

  # Filter to children aged 0-4 and assign within-family index
  # Explicit ordering required for deterministic pseudofamily_id construction
  children_filtered <- children %>%
    filter(age < 5) %>%
    group_by(hh_id, parent_unit_id) %>%
    arrange(age, child_id, .by_group = TRUE) %>%
    mutate(child_index = row_number()) %>%
    ungroup()

  children_wide <- children_filtered %>%
    select(hh_id, parent_unit_id, child_index, child_id, age, child_weight, tax_unit_id) %>%
    pivot_wider(
      id_cols     = c(hh_id, parent_unit_id),
      names_from  = child_index,
      values_from = c(child_id, age, child_weight, tax_unit_id),
      names_glue  = '{.value}.{child_index}'
    ) %>%
    rename_with(~ gsub('^age[.]', 'child_age.', .x)) %>%
    rename_with(~ gsub('^tax_unit_id[.]', 'child_tax_unit.', .x))

  # Join parent units with child data and split 3+ child families into pseudofamilies
  parent_units_joined <- parent_units_with_taxes %>%
    inner_join(children_wide, by = c('hh_id', 'parent_unit_id')) %>%
    left_join(households %>% select(hh_id, region), by = 'hh_id') %>%
    pseudofamily_split(
      n_child           = 'n_children_original',
      child_weight_stub = 'child_weight',
      other_weights     = c('per_weight1', 'per_weight2')
    )

  # n_children (effective) is min(n_children_original, 2) for choice set sizing
  parent_units_joined <- parent_units_joined %>%
    mutate(n_children = pmin(n_children_original, 2L))

  # Pre-compute minimum parent earnings for SPM poverty analysis
  parent_units_joined <- parent_units_joined %>%
    mutate(
      min_parent_earnings = case_when(
        is.na(earnings2) ~ pmax(0, earnings1),
        is.na(earnings1) ~ pmax(0, earnings2),
        TRUE ~ pmin(pmax(0, earnings1), pmax(0, earnings2))
      )
    )

  # Ensure married2 exists (single-parent units may have no spouse fields in some pipelines)
  if (!('married2' %in% names(parent_units_joined))) {
    parent_units_joined <- parent_units_joined %>% mutate(married2 = NA_integer_)
  }

  # Compute primary caregiver group for employment targeting
  # pc_male: 1 if primary caregiver is male, 0 if female
  # pc_age: age of primary caregiver
  # pc_age_bin: age bin for employment targeting (u25, 25_35, 35_45, 45plus)
  # pc_group: sex x age bin (e.g., F_u25, M_35_45)
  parent_units_joined <- parent_units_joined %>%
    mutate(
      pc_male = if_else(primary_caregiver == 1, male1, male2),
      pc_married = if_else(primary_caregiver == 1, married1, married2),
      pc_married = replace_na(pc_married, 0L),
      per_weight_pc = if_else(primary_caregiver == 1, per_weight1, per_weight2),
      pc_age = if_else(primary_caregiver == 1, age1, age2),
      pc_age_bin = case_when(
        pc_age < 25 ~ 'u25',
        pc_age < 35 ~ '25_35',
        pc_age < 45 ~ '35_45',
        TRUE ~ '45plus'
      ),
      pc_group = paste0(if_else(pc_male == 1, 'M', 'F'), '_', pc_age_bin)
    )

  # -- Assertions: pre-pivot data integrity ----

  # Each (hh_id, parent_unit_id, pseudofamily_id) must have exactly 3 rows
  # (none, pt, ft). If a household has fewer, the pivot will fill with NA
  # and downstream tax/AGI columns will be silently wrong. If more, the
  # pivot will create list-columns that break everything downstream.
  n_pre_pivot <- nrow(parent_units_joined)
  emp_counts <- parent_units_joined %>%
    count(hh_id, parent_unit_id, pseudofamily_id) %>%
    pull(n)
  stopifnot(all(emp_counts == 3))

  # Employment choices must be exactly {none, pt, ft} — no misspellings,
  # NAs, or extra levels that would create unexpected pivot columns.
  stopifnot(setequal(unique(parent_units_joined$employment_choice), c('none', 'pt', 'ft')))

  # Pivot to wide format by employment_choice
  parent_units_wide <- parent_units_joined %>%
    pivot_wider(
      id_cols = c(hh_id, parent_unit_id, pseudofamily_id,
                  n_children, n_children_original, child_category, n_parents,
                  hhm_id1, hhm_id2,
                  per_weight1, per_weight2, primary_caregiver, male1, male2,
                  pc_male, pc_married, per_weight_pc, pc_age, pc_age_bin, pc_group,
                  age1, age2, married1, married2,
                  tax_unit_id1, tax_unit_id2, filing_status1, filing_status2,
                  hours_secondary, oasi_tax_max, region,
                  starts_with('child_id.'), starts_with('child_age.'), starts_with('child_weight.'),
                  starts_with('child_tax_unit.')),
      names_from  = employment_choice,
      values_from = c(agi, hours_pc, total_tax, income_tax, min_parent_earnings, p_employment,
                      agi1, agi2, earnings1, earnings2, liab_iit1, liab_iit2),
      names_sep   = '.'
    )

  # Join cached price_wedge values (persists across years)
  if (!is.null(cached_price_wedge)) {
    parent_units_wide <- parent_units_wide %>%
      left_join(cached_price_wedge, by = c('hh_id', 'parent_unit_id'))
  } else {
    parent_units_wide <- parent_units_wide %>%
      mutate(
        price_wedge.center_low  = 1.0,
        price_wedge.center_high = 1.0,
        price_wedge.home        = 1.0
      )
  }

  # -- Assertions: pivot produced expected result ----

  # Pivot should produce exactly 1/3 the rows (3 emp choices -> 1 wide row)
  stopifnot(nrow(parent_units_wide) == n_pre_pivot / 3)

  # The .none, .pt, .ft columns must exist for the key value columns.
  # If pivot silently dropped an employment level, these would be missing.
  stopifnot(all(c('agi.none', 'agi.pt', 'agi.ft') %in% names(parent_units_wide)))
  stopifnot(all(c('total_tax.none', 'total_tax.pt', 'total_tax.ft') %in% names(parent_units_wide)))

  # Key uniqueness: (hh_id, parent_unit_id, pseudofamily_id) must be unique
  # after pivot. If not, two households share an identity and their weights
  # will be silently double-counted in every downstream aggregation.
  n_unique_keys <- parent_units_wide %>%
    distinct(hh_id, parent_unit_id, pseudofamily_id) %>%
    nrow()
  stopifnot(n_unique_keys == nrow(parent_units_wide))

  # Split by child_category
  list(
    c1     = parent_units_wide %>% filter(child_category == '1'),
    c2plus = parent_units_wide %>% filter(child_category == '2+')
  )
}



match_tax_donors <- function(choices, donor_pool, pct_thresholds, seed = NULL) {

  #----------------------------------------------------------------------------
  # Match tax donors to parent units using percentile-based 3-tier statistical
  # matching. Assigns donor_id and ETR for both parents in each unit.
  #
  # Params:
  #   - choices (df): parent unit choices with AGI and filing status
  #   - donor_pool (df): tax donor pool with ETR and EMTR schedules
  #   - pct_thresholds (df): AGI percentile thresholds for bin assignment
  #   - seed (int or NULL): random seed for reproducible matching
  #
  # Returns: (df) choices with donor_id1, donor_id2, etr1, etr2 columns added
  #----------------------------------------------------------------------------


  set.seed(if (is.null(seed)) random_seed_base + 5 else seed)
  n_rows_before_matching <- nrow(choices)

  cat('  Matching tax donors to parent units...\n')

  # --- Match donors for parent 1 ---

  # Get unique tax units (one row per hh_id + tax_unit_id1)
  tax_units1 <- choices %>%
    distinct(hh_id, tax_unit_id1, baseline_agi1, filing_status1, n_dep_u13_1) %>%
    mutate(
      pct_bin1 = get_agi_pct_bin(baseline_agi1, pct_thresholds),
      joint1 = as.integer(filing_status1 == 2),
      has_dep1 = as.integer(n_dep_u13_1 > 0)
    )

  # Sample one donor per tax unit
  tax_units1_matched <- sample_donors_for_units_pct(
    units = tax_units1,
    donor_pool = donor_pool,
    pct_bin_col = 'pct_bin1',
    joint_col = 'joint1',
    n_dep_u13_col = 'n_dep_u13_1',
    has_dep_col = 'has_dep1'
  )

  # Join back to choices (same donor for all parent units sharing a tax unit)
  choices <- choices %>%
    left_join(
      tax_units1_matched %>% select(hh_id, tax_unit_id1, donor_id1 = donor_id, etr1 = etr),
      by = c('hh_id', 'tax_unit_id1')
    )


  # --- Match donors for parent 2 (where applicable) ---

  if (any(!is.na(choices$baseline_agi2))) {

    # Get unique tax units for parent 2
    tax_units2 <- choices %>%
      filter(!is.na(baseline_agi2)) %>%
      distinct(hh_id, tax_unit_id2, baseline_agi2, filing_status2, n_dep_u13_2) %>%
      mutate(
        pct_bin2 = get_agi_pct_bin(baseline_agi2, pct_thresholds),
        joint2 = as.integer(filing_status2 == 2),
        has_dep2 = as.integer(n_dep_u13_2 > 0)
      )

    # Sample one donor per tax unit
    tax_units2_matched <- sample_donors_for_units_pct(
      units = tax_units2,
      donor_pool = donor_pool,
      pct_bin_col = 'pct_bin2',
      joint_col = 'joint2',
      n_dep_u13_col = 'n_dep_u13_2',
      has_dep_col = 'has_dep2'
    )

    # Join back to choices
    choices <- choices %>%
      left_join(
        tax_units2_matched %>% select(hh_id, tax_unit_id2, donor_id2 = donor_id, etr2 = etr),
        by = c('hh_id', 'tax_unit_id2')
      )

  } else {
    choices <- choices %>%
      mutate(donor_id2 = NA_integer_, etr2 = NA_real_)
  }

  # --- Validate: all parent units must have tax donors assigned ---
  n_missing_donor1 <- sum(is.na(choices$donor_id1))
  n_missing_donor2 <- sum(is.na(choices$donor_id2) & !is.na(choices$baseline_agi2))

  if (n_missing_donor1 > 0 || n_missing_donor2 > 0) {
    stop(sprintf(
      'Tax donor matching failed: %d parent1 units and %d parent2 units missing donors. This indicates percentile bins without matching donors in the donor pool.',
      n_missing_donor1, n_missing_donor2
    ))
  }

  # -- Assertions: donor matching quality ----

  # Matched ETRs should span a range. If all ETRs are identical, the
  # matching collapsed to a single donor (e.g., one pct_bin matched
  # everyone, or the donor pool had only one record per bin).
  if (nrow(choices) > 100) {
    stopifnot(sd(choices$etr1) > 0)
  }

  # Matched ETRs should correlate with AGI direction. The top half of
  # AGI should have a higher median ETR than the bottom half. If the
  # correlation is reversed or absent, donors may have been matched to
  # the wrong AGI bins (e.g., pct_thresholds from wrong year/population).
  if (nrow(choices) > 100) {
    median_agi <- median(choices$baseline_agi1, na.rm = TRUE)
    etr_low <- median(choices$etr1[choices$baseline_agi1 < median_agi], na.rm = TRUE)
    etr_high <- median(choices$etr1[choices$baseline_agi1 >= median_agi], na.rm = TRUE)
    stopifnot(etr_high > etr_low)
  }

  # Row count must not change through matching. The left_joins above
  # should be 1-to-1 (one donor per tax unit). If the join keys allow
  # many-to-many, rows would silently expand.
  stopifnot(nrow(choices) == n_rows_before_matching)

  return(choices)
}



sample_donors_for_units_pct <- function(units, donor_pool,
                                         pct_bin_col, joint_col,
                                         n_dep_u13_col, has_dep_col) {

  #----------------------------------------------------------------------------
  # Sample one donor per unit using percentile-based 3-tier matching.
  # Tier 1 (<p90): match on pct_bin, joint, n_dep_u13 with has_dep fallback.
  # Tier 2 (p90-p98): match on pct_bin, joint, has_dep.
  # Tier 3 (>p98): match on pct_bin, joint only.
  #
  # Params:
  #   - units (df): tax units to match with AGI and filing info
  #   - donor_pool (df): donor pool with ETR, weights, and matching keys
  #   - pct_bin_col (chr): column name for AGI percentile bin
  #   - joint_col (chr): column name for joint filing indicator
  #   - n_dep_u13_col (chr): column name for number of dependents under 13
  #   - has_dep_col (chr): column name for has-dependent indicator
  #
  # Returns: (df) units with donor_id and etr columns added
  #----------------------------------------------------------------------------


  # Rename columns for easier processing
  units <- units %>%
    rename(
      pct_bin = !!pct_bin_col,
      joint = !!joint_col,
      n_dep_u13 = !!n_dep_u13_col,
      has_dep = !!has_dep_col
    ) %>%
    mutate(unit_row_id = row_number())


  sample_donors_by_group <- function(units_sub, donors_sub, join_keys) {

    #--------------------------------------------------------------------------
    # Match units to donors by specified join keys and sample one donor per
    # unit using weighted sampling. Uses chunked approach to avoid materializing
    # a Cartesian product (which can exceed 8GB with 100% ACS).
    #
    # Params:
    #   - units_sub (df): subset of units to match
    #   - donors_sub (df): subset of donor pool for this tier
    #   - join_keys (chr vec): column names to join on
    #
    # Returns: (df) units with donor_id and etr columns added
    #--------------------------------------------------------------------------

    if (nrow(units_sub) == 0) return(tibble())
    donors_sub <- donors_sub %>% select(all_of(c(join_keys, 'donor_id', 'etr', 'weight')))

    # Split units by join key combinations and sample within each group
    unit_groups <- units_sub %>% group_by(across(all_of(join_keys))) %>% group_split()
    results <- vector('list', length(unit_groups))

    for (i in seq_along(unit_groups)) {
      grp <- unit_groups[[i]]
      n_units <- nrow(grp)
      key_vals <- grp[1, join_keys, drop = FALSE]

      # Find matching donors for this key combination
      matching <- donors_sub %>% semi_join(key_vals, by = join_keys)

      if (nrow(matching) == 0) {
        grp$donor_id <- NA_integer_
        grp$etr <- NA_real_
      } else {
        sampled_idx <- sample.int(nrow(matching), size = n_units,
                                  replace = TRUE, prob = matching$weight)
        grp$donor_id <- matching$donor_id[sampled_idx]
        grp$etr <- matching$etr[sampled_idx]
      }
      results[[i]] <- grp
    }

    bind_rows(results)
  }


  # --- Tier 1 (below p90): match on (pct_bin, joint, n_dep_u13) ---
  # Falls back to (pct_bin, joint, has_dep) if exact cell is empty
  units_tier1 <- units %>% filter(pct_bin >= 0 & pct_bin < TIER1_MAX_PERCENTILE)

  if (nrow(units_tier1) > 0) {
    # Donors for exact matching (by n_dep_u13)
    donors_tier1_exact <- donor_pool %>%
      filter(pct_bin >= 0 & pct_bin < TIER1_MAX_PERCENTILE) %>%
      select(pct_bin, joint, n_dep_u13, donor_id, etr, weight)

    # Try exact match on (pct_bin, joint, n_dep_u13) using chunked sampling
    matched_exact_all <- sample_donors_by_group(
      units_tier1, donors_tier1_exact, c('pct_bin', 'joint', 'n_dep_u13'))

    # Identify units with no donors (NA donor_id means empty cell)
    matched_exact <- matched_exact_all %>% filter(!is.na(donor_id))
    units_without_donors <- matched_exact_all %>% filter(is.na(donor_id)) %>%
      select(-donor_id, -etr)

    # Fallback: for units without exact matches, use (pct_bin, joint, has_dep)
    if (nrow(units_without_donors) > 0) {
      donors_tier1_fallback <- donor_pool %>%
        filter(pct_bin >= 0 & pct_bin < TIER1_MAX_PERCENTILE) %>%
        select(pct_bin, joint, has_dep, donor_id, etr, weight)

      matched_fallback <- sample_donors_by_group(
        units_without_donors, donors_tier1_fallback, c('pct_bin', 'joint', 'has_dep'))

      matched_tier1 <- bind_rows(matched_exact, matched_fallback)
    } else {
      matched_tier1 <- matched_exact
    }
  } else {
    matched_tier1 <- tibble()
  }


  # Tier 2 (p90-p98): match on (pct_bin, joint, has_dep)
  matched_tier2 <- sample_donors_by_group(
    units %>% filter(pct_bin >= TIER1_MAX_PERCENTILE & pct_bin < TIER2_MAX_PERCENTILE),
    donor_pool %>% filter(pct_bin >= TIER1_MAX_PERCENTILE & pct_bin < TIER2_MAX_PERCENTILE),
    c('pct_bin', 'joint', 'has_dep'))

  # Tier 3 (above p98): match on (pct_bin, joint)
  matched_tier3 <- sample_donors_by_group(
    units %>% filter(pct_bin >= TIER2_MAX_PERCENTILE),
    donor_pool %>% filter(pct_bin >= TIER2_MAX_PERCENTILE),
    c('pct_bin', 'joint'))

  # Negative AGI: match on (pct_bin, joint)
  matched_neg <- sample_donors_by_group(
    units %>% filter(pct_bin < 0),
    donor_pool %>% filter(pct_bin < 0),
    c('pct_bin', 'joint'))

  # Combine results
  matched <- bind_rows(matched_tier1, matched_tier2, matched_tier3, matched_neg)

  # Check for unmatched units
  n_unmatched <- sum(is.na(matched$donor_id))
  if (n_unmatched > 0) {
    unmatched <- matched %>% filter(is.na(donor_id))
    stop(sprintf(
      'Failed to match %d units. First unmatched: pct_bin=%s, joint=%d, n_dep_u13=%d',
      n_unmatched,
      unmatched$pct_bin[1],
      unmatched$joint[1],
      unmatched$n_dep_u13[1]
    ))
  }

  # Rename back to original column names and return
  result <- matched %>%
    select(-unit_row_id)

  result <- result %>%
    rename(
      !!pct_bin_col := pct_bin,
      !!joint_col := joint,
      !!n_dep_u13_col := n_dep_u13,
      !!has_dep_col := has_dep
    )

  return(result)
}



calculate_taxes_with_donors <- function(choices, donor_pool, wage_thresholds, tax_max) {

  #----------------------------------------------------------------------------
  # Calculate tax liabilities using matched donor ETR and EMTR schedules.
  # Computes baseline income tax (ETR * baseline AGI), delta income tax
  # (EMTR * earnings delta), and payroll taxes from counterfactual earnings.
  #
  # Params:
  #   - choices (df): parent unit choices with donor_id and earnings columns
  #   - donor_pool (df): donor pool with EMTR schedules
  #   - wage_thresholds (df): wage thresholds for EMTR lookup
  #   - tax_max (dbl): OASI tax maximum for the year
  #
  # Returns: (df) choices with liab_iit, liab_payroll, and component columns
  #----------------------------------------------------------------------------


  # Look up EMTR for parent 1 at counterfactual earnings
  emtr1 <- lookup_emtr(donor_pool, wage_thresholds, choices$donor_id1, choices$earnings1)

  # Look up EMTR for parent 2 at counterfactual earnings (where applicable)
  emtr2 <- lookup_emtr(donor_pool, wage_thresholds, choices$donor_id2, choices$earnings2)

  out <- choices %>%
    mutate(
      emtr1 = !!emtr1,
      emtr2 = !!emtr2,

      # Baseline income tax (ETR * baseline AGI)
      baseline_liab_iit1 = etr1 * baseline_agi1,
      baseline_liab_iit2 = if_else(!is.na(baseline_agi2), etr2 * baseline_agi2, NA),

      # Delta income tax (EMTR * earnings delta)
      delta_liab_iit1 = emtr1 * earnings_delta1,
      delta_liab_iit2 = if_else(!is.na(earnings_delta2), emtr2 * earnings_delta2, NA),

      # Total counterfactual income tax
      liab_iit1 = baseline_liab_iit1 + delta_liab_iit1,
      liab_iit2 = if_else(!is.na(baseline_agi2), baseline_liab_iit2 + delta_liab_iit2, NA),
      liab_iit  = liab_iit1 + replace_na(liab_iit2, 0),

      # Calculate payroll taxes (directly from counterfactual earnings)
      liab_payroll1 = (MEDICARE_TAX_RATE * earnings1) + (OASI_TAX_RATE * pmin(earnings1, tax_max)),
      liab_payroll2 = if_else(!is.na(earnings2), (MEDICARE_TAX_RATE * earnings2) + (OASI_TAX_RATE * pmin(earnings2, tax_max)), NA),
      liab_payroll  = liab_payroll1 + replace_na(liab_payroll2, 0)
    )

  for (col in c('delta_liab_iit2', 'emtr2', 'liab_payroll2')) {
    n_bad <- sum(!is.na(out$baseline_agi2) & is.na(out[[col]]))
    if (n_bad > 0) stop(sprintf('Tax imputation encountered NA %s for %d married units (unexpected).', col, n_bad))
  }

  return(out)
}



lookup_emtr <- function(donor_pool, wage_thresholds, donor_ids, earnings) {

  #----------------------------------------------------------------------------
  # Look up effective marginal tax rate (EMTR) from each donor's schedule at
  # the wage threshold nearest to the given earnings level.
  #
  # Params:
  #   - donor_pool (df): donor pool with mtr_wages_* columns
  #   - wage_thresholds (df): wage percentile thresholds with wages and percentile
  #   - donor_ids (int vec): donor IDs to look up
  #   - earnings (num vec): earnings values for threshold matching
  #
  # Returns: (num vec) EMTR values for each donor-earnings pair
  #----------------------------------------------------------------------------


  # Build sorted wage thresholds with column names
  thresholds <- wage_thresholds %>%
    arrange(wages) %>%
    mutate(mtr_col = paste0('mtr_wages_', percentile))

  wage_breaks <- thresholds$wages
  mtr_col_names <- thresholds$mtr_col

  # Vectorized: find nearest threshold index for each earnings value
  # Using findInterval with midpoints for nearest-neighbor matching
  midpoints <- (wage_breaks[-1] + wage_breaks[-length(wage_breaks)]) / 2
  threshold_idx <- findInterval(earnings, midpoints) + 1
  threshold_idx[is.na(earnings)] <- NA_integer_

  # Get unique threshold columns needed
  unique_cols <- unique(mtr_col_names[threshold_idx[!is.na(threshold_idx)]])

  # Get donor schedules (pivot to long format for efficient join)
  donor_schedules_long <- donor_pool %>%
    filter(donor_id %in% unique(donor_ids[!is.na(donor_ids)])) %>%
    select(donor_id, all_of(unique_cols)) %>%
    pivot_longer(
      cols = all_of(unique_cols),
      names_to = 'mtr_col',
      values_to = 'emtr'
    )

  # Create lookup table
  col_used <- mtr_col_names[threshold_idx]

  lookup_df <- tibble(
    row_id = seq_along(donor_ids),
    donor_id = donor_ids,
    mtr_col = col_used
  )

  # Join to get EMTR values
  result <- lookup_df %>%
    left_join(donor_schedules_long, by = c('donor_id', 'mtr_col')) %>%
    arrange(row_id) %>%
    pull(emtr)

  # If EMTR is missing at zero/negative earnings or when using the p0 column, treat as 0
  result[is.na(result) & !is.na(earnings) & earnings <= 0] <- 0
  result[is.na(result) & col_used == 'mtr_wages_p0'] <- 0

  # Any remaining NA is unexpected and should fail loudly
  bad_idx <- which(is.na(result) & !is.na(donor_ids))
  if (length(bad_idx) > 0) {
    ex <- head(bad_idx, 5)
    examples <- paste(
      sprintf(
        '[row=%d donor_id=%s earnings=%s col=%s]',
        ex,
        donor_ids[ex],
        earnings[ex],
        col_used[ex]
      ),
      collapse = ' '
    )
    stop(sprintf('EMTR lookup returned NA for %d rows (unexpected). Examples: %s', length(bad_idx), examples))
  }

  return(result)
}



pseudofamily_split <- function(df, n_child, child_weight_stub, other_weights = NULL) {

  #----------------------------------------------------------------------------
  # Split 3+ child families into 2-child pseudofamilies using all unordered
  # pairs of children. Adjusts family weights by choose(m,2) and child
  # weights by (m-1) to preserve population and child totals.
  #
  # Params:
  #   - df (df): parent unit dataframe with child-indexed columns (x.1, x.2, ...)
  #   - n_child (chr): column name containing original number of children
  #   - child_weight_stub (chr): stub for child weight columns (e.g., 'child_weight')
  #   - other_weights (chr vec or NULL): family/parent weight column names to adjust
  #
  # Returns: (df) dataframe with 3+ child families expanded into 2-child
  #   pseudofamilies, each with a pseudofamily_id column
  #----------------------------------------------------------------------------


  # Identify child-slot columns x.1, x.2, ..., x.K (K arbitrary)
  child_cols_all <- names(df)[grepl('\\.\\d+$', names(df))]
  if (length(child_cols_all) == 0L) {
    stop('No child-suffixed columns found (expected columns like var.1, var.2, ...).')
  }

  child_idx_all <- as.integer(sub('.*\\.(\\d+)$', '\\1', child_cols_all))

  # We will keep only the .1 and .2 slots in the output
  child_cols_drop <- child_cols_all[!child_idx_all %in% c(1L, 2L)]

  # Split into small (<=2 kids) and big (>=3 kids)
  df_small <- df %>%
    filter(is.na(.data[[n_child]]) | .data[[n_child]] <= 2) %>%
    select(-any_of(child_cols_drop))

  df_big <- df %>%
    filter(.data[[n_child]] >= 3) %>%
    mutate(.orig_row_id = row_number())

  if (nrow(df_big) == 0L) {
    return(df_small)
  }

  # Number of pseudo-families per original family = choose(m, 2)
  # Child appearance count = m - 1 (each child appears in m-1 pairs)
  df_big <- df_big %>%
    mutate(
      .n_children = .data[[n_child]],
      split_count = as.integer(choose(.n_children, 2)),
      child_appear_count = .n_children - 1L
    )

  # Replicate rows using base R (faster than tidyr::uncount)
  reps <- df_big$split_count
  idx_rep <- rep(seq_len(nrow(df_big)), times = reps)
  df_rep <- df_big[idx_rep, , drop = FALSE]

  # Pair id (1..choose(m,2)) within each original row
  df_rep$pair_id <- sequence(reps)

  # Precompute all unordered pairs for each unique m in the data
  m_vals <- sort(unique(df_rep$.n_children))
  pair_map <- setNames(
    lapply(m_vals, function(m) combn(seq_len(m), 2)),  # 2 x choose(m,2)
    as.character(m_vals)
  )

  # Assign the two child indices (child_idx_1, child_idx_2) for each replicated row
  Nrep <- nrow(df_rep)
  child_idx_1 <- integer(Nrep)
  child_idx_2 <- integer(Nrep)

  nchild_vec <- df_rep$.n_children
  for (m in m_vals) {
    rows_m <- which(nchild_vec == m)
    mat_m  <- pair_map[[as.character(m)]]
    rvec   <- df_rep$pair_id[rows_m]
    child_idx_1[rows_m] <- mat_m[1, rvec]
    child_idx_2[rows_m] <- mat_m[2, rvec]
  }

  # Direct-copy approach:
  # For each base variable "x" with columns x.1, x.2, ..., create:
  #   x.1 <- x.<child_idx_1>
  #   x.2 <- x.<child_idx_2>
  # Then we drop all x.k for k>=3 at the end.
  base_vars <- unique(sub('\\.\\d+$', '', child_cols_all))

  for (b in base_vars) {

    # Candidate source columns for this base var
    # (We check existence because some bases may not have all child slots.)
    # We'll only ever write into b.1 and b.2.
    src_candidates <- names(df_rep)[startsWith(names(df_rep), paste0(b, '.')) & grepl('\\.\\d+$', names(df_rep))]
    if (length(src_candidates) == 0L) next

    # Use the first existing source as a type template
    template <- df_rep[[src_candidates[1]]]
    dest1 <- template; dest1[] <- NA
    dest2 <- template; dest2[] <- NA

    # Fill dest1/dest2 by child index, in chunks (keeps types, avoids reshaping)
    # Only loop over the indices that actually appear.
    for (c in sort(unique(child_idx_1))) {
      col_c <- paste0(b, '.', c)
      if (!col_c %in% names(df_rep)) next
      rows <- which(child_idx_1 == c)
      dest1[rows] <- df_rep[[col_c]][rows]
    }

    for (c in sort(unique(child_idx_2))) {
      col_c <- paste0(b, '.', c)
      if (!col_c %in% names(df_rep)) next
      rows <- which(child_idx_2 == c)
      dest2[rows] <- df_rep[[col_c]][rows]
    }

    df_rep[[paste0(b, '.1')]] <- dest1
    df_rep[[paste0(b, '.2')]] <- dest2
  }

  # Drop all child slots other than .1 and .2
  df_rep <- df_rep %>%
    select(-any_of(child_cols_drop))

  # Adjust weights to account for splitting:
  #   - Family/parent weights: divide by split_count = choose(m,2) to preserve family totals
  #   - Child weights: divide by child_appear_count = (m-1) to preserve child totals
  #
  # Rationale: Each child appears in (m-1) pairs, so dividing by (m-1) ensures
  # that summing across all pseudofamilies recovers the original child weight.
  child_weight_cols <- paste0(child_weight_stub, '.', c(1, 2))
  child_weight_cols <- child_weight_cols[child_weight_cols %in% names(df_rep)]
  family_weight_cols <- other_weights[other_weights %in% names(df_rep)]

  # Adjust family/parent weights by split_count
  if (length(family_weight_cols) > 0L) {
    df_rep <- df_rep %>%
      mutate(across(all_of(family_weight_cols), ~ .x / split_count))
  }

  # Adjust child weights by child_appear_count
  if (length(child_weight_cols) > 0L) {
    df_rep <- df_rep %>%
      mutate(across(all_of(child_weight_cols), ~ .x / child_appear_count))
  }

  # Rename pair_id to pseudofamily_id for clarity
  df_rep <- df_rep %>%
    rename(pseudofamily_id = pair_id)

  # Clean up helper columns (keep pseudofamily_id for pivot uniqueness)
  df_rep <- df_rep %>%
    select(-any_of(c('.orig_row_id', 'split_count', 'child_appear_count', '.n_children')))

  # Add pseudofamily_id = 1 to small families for consistency
  df_small <- df_small %>%
    mutate(pseudofamily_id = 1L)

  # Combine back with <=2-kid families
  bind_rows(df_small, df_rep)
}



compute_baseline_employment_rates <- function(parent_units_list) {

  #----------------------------------------------------------------------------
  # Compute weighted baseline employment rates by demographic group from
  # random forest employment probabilities. Groups are sex x age bin
  # combinations (e.g., F_u25, M_35_45).
  #
  # Params:
  #   - parent_units_list (list): named list of parent unit dataframes by type
  #
  # Returns: (num vec) named numeric vector of employment rates per
  #   sex x age bin group, with zeros for missing groups
  #----------------------------------------------------------------------------


  # Combine all parent units
  all_pu <- bind_rows(lapply(names(parent_units_list), function(nm) {
    df <- parent_units_list[[nm]]
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>%
      select(pc_group, per_weight1, per_weight2, primary_caregiver, p_employment.pt, p_employment.ft, any_of('per_weight_pc')) %>%
      mutate(pu_type = nm)
  }))

  if (is.null(all_pu) || nrow(all_pu) == 0) {
    warning('compute_baseline_employment_rates: no parent units found')
    return(setNames(rep(0, length(EMPLOYMENT_TARGETING_GROUPS)), EMPLOYMENT_TARGETING_GROUPS))
  }

  # Compute employment probability = pt + ft for each unit
  # Use primary caregiver weight (per_weight1 if PC is parent 1, per_weight2 if PC is parent 2)
  all_pu <- all_pu %>%
    mutate(emp_prob = replace_na(p_employment.pt, 0) + replace_na(p_employment.ft, 0))

  if ('per_weight_pc' %in% names(all_pu)) {
    all_pu <- all_pu %>%
      mutate(weight = if_else(
        !is.na(per_weight_pc),
        replace_na(per_weight_pc, 0),
        if_else(primary_caregiver == 1, replace_na(per_weight1, 0), replace_na(per_weight2, 0))
      ))
  } else {
    all_pu <- all_pu %>%
      mutate(weight = if_else(primary_caregiver == 1, replace_na(per_weight1, 0), replace_na(per_weight2, 0)))
  }

  # Compute weighted employment rate by group
  rates <- all_pu %>%
    group_by(pc_group) %>%
    summarise(
      emp_rate = sum(emp_prob * weight) / sum(weight),
      .groups = 'drop'
    )

  # Convert to named vector with all groups (missing groups get 0)
  result <- setNames(rep(0, length(EMPLOYMENT_TARGETING_GROUPS)), EMPLOYMENT_TARGETING_GROUPS)
  for (i in seq_len(nrow(rates))) {
    g <- rates$pc_group[i]
    if (g %in% EMPLOYMENT_TARGETING_GROUPS) {
      result[g] <- rates$emp_rate[i]
    }
  }

  return(result)
}



