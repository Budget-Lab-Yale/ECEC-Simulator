#------------------------------------------------------------------------------
# load_baseline_year_result.R
#
# Loads a converged baseline year result from a prior run's output so the
# baseline equilibrium solve can be skipped entirely (--baseline-interface).
#
# The counterfactual code path consumes five fields from baseline_result:
#   $converged, $prices, $w, $employment_shifts, $parent_units
# All of these are recoverable from a prior run's baseline output:
#   - parent_units: simulation/baseline/data/parent_units_<type>_<year>.csv
#   - prices, w:    simulation/baseline/supply/supply_<year>.yaml
#   - shifts:       simulation/baseline/models/equilibrium/employment_targeting_<year>.csv
#
# Validity requirement: the Gumbel epsilon draws for counterfactuals in the
# current run must be identical to the draws the prior baseline used. That
# holds iff the prior run used the same estimation data, sample percentages,
# seed offset, and draws per record. validate_baseline_interface() enforces
# this from the prior run's metadata; load_baseline_year_result() additionally
# asserts key-set and value agreement against the freshly prepared parent
# units as a backstop.
#
# Called from: 3a (validation), 3b (sequential year loop), slurm.R (year worker)
#------------------------------------------------------------------------------



validate_baseline_interface <- function(baseline_interface, years_to_run,
                                        counterfactual_infos) {

  #----------------------------------------------------------------------------
  # Validates that a prior run's baseline output is complete and compatible
  # with the current run before any simulation work starts. Stops with a
  # clear message on any incompatibility.
  #
  # Params:
  #   - baseline_interface (chr): Timestamp of the prior run to reuse
  #   - years_to_run (int vec): Years the current run will simulate
  #   - counterfactual_infos (list): Scenario info lists for counterfactuals
  #
  # Returns: nothing (side effects only; stops on incompatibility)
  #----------------------------------------------------------------------------

  src_root     <- file.path(default_paths$roots$output, baseline_interface)
  baseline_dir <- file.path(src_root, 'simulation', 'baseline')

  if (!dir.exists(baseline_dir)) {
    stop('Baseline interface not found: ', baseline_dir, '\n',
         '  Check that ', baseline_interface, ' is a completed run with a baseline scenario.')
  }

  # Helper: read a value from the prior run's run_info.txt field list
  parse_run_info_field <- function(lines, field) {

    #--------------------------------------------------------------------------
    # Extracts a named field value from run_info.txt lines.
    #
    # Params:
    #   - lines (chr vec): Lines of run_info.txt
    #   - field (chr): Field name to extract
    #
    # Returns: (chr) Field value, or NA if not found
    #--------------------------------------------------------------------------

    pattern <- paste0('^\\s*', field, '\\s*:\\s*(.*)$')
    matches <- grep(pattern, lines, value = TRUE)
    if (length(matches) == 0) return(NA_character_)
    trimws(sub(pattern, '\\1', matches[1]))
  }

  # Helper: get a runscript column with a default for missing/blank values
  get_runscript_col <- function(df, col, default) {

    #--------------------------------------------------------------------------
    # Reads a column from a runscript baseline row, applying a default when
    # the column is absent, NA, or blank.
    #
    # Params:
    #   - df (df): Single-row data frame (the baseline runscript row)
    #   - col (chr): Column name
    #   - default (chr): Default value
    #
    # Returns: (chr) Column value or default
    #--------------------------------------------------------------------------

    if (!col %in% names(df)) return(default)
    val <- as.character(df[[col]][1])
    if (is.na(val) || val == '') return(default)
    val
  }

  #-----------------------------------------
  # Check 1: baseline policies must match
  #-----------------------------------------

  src_runscript_path <- file.path(src_root, 'metadata', 'runscript.csv')
  if (!file.exists(src_runscript_path)) {
    stop('Baseline interface is missing metadata/runscript.csv: ', src_runscript_path)
  }
  src_runscript <- read_csv(src_runscript_path, show_col_types = FALSE)

  src_baseline <- src_runscript %>% filter(id == 'baseline')
  cur_baseline <- runscript %>% filter(id == 'baseline')
  if (nrow(src_baseline) == 0) {
    stop('Baseline interface runscript has no baseline row: ', src_runscript_path)
  }

  policy_defaults <- c(
    policy_demand    = 'baseline',
    policy_supply    = 'baseline',
    policy_cdctc     = 'baseline',
    policy_tax       = 'baseline',
    policy_transfer  = 'baseline',
    employer_subsidy = 'none',
    wage_floor       = 'none'
  )
  for (col in names(policy_defaults)) {
    src_val <- get_runscript_col(src_baseline, col, policy_defaults[col])
    cur_val <- get_runscript_col(cur_baseline, col, policy_defaults[col])
    if (src_val != cur_val) {
      stop('Baseline policy mismatch with interface ', baseline_interface, ':\n',
           '  ', col, ': interface has "', src_val, '", current runscript has "', cur_val, '"\n',
           '  The reused baseline must have been generated under identical baseline policies.')
    }
  }

  #-----------------------------------------
  # Check 2: run parameters must match
  #-----------------------------------------

  run_info_path <- file.path(src_root, 'metadata', 'run_info.txt')
  if (!file.exists(run_info_path)) {
    stop('Baseline interface is missing metadata/run_info.txt: ', run_info_path)
  }
  info_lines <- readLines(run_info_path)

  compare_fields <- list(
    seed_offset         = as.character(seed_offset),
    calib_sample        = as.character(calib_sample),
    sim_sample          = as.character(sim_sample),
    disable_price_wedge = as.character(exists('disable_price_wedge') && isTRUE(disable_price_wedge))
  )
  for (field in names(compare_fields)) {
    src_val <- parse_run_info_field(info_lines, field)
    if (is.na(src_val)) next  # Older run_info without this field: skip check
    if (src_val != compare_fields[[field]]) {
      stop('Run parameter mismatch with baseline interface ', baseline_interface, ':\n',
           '  ', field, ': interface used ', src_val, ', current run uses ',
           compare_fields[[field]], '\n',
           '  Gumbel draw pairing requires identical samples, seed, and price wedge settings.')
    }
  }

  # n_draws_per_record: newer runs record it as a field; older runs only in the
  # raw command line (-n <val> or --n-draws-per-record <val>). Default is 10.
  src_n_draws <- parse_run_info_field(info_lines, 'n_draws_per_record')
  if (is.na(src_n_draws)) {
    raw_idx <- grep('^Raw Command Line', info_lines)
    src_n_draws <- '10'
    if (length(raw_idx) > 0 && (raw_idx[1] + 2) <= length(info_lines)) {
      tokens <- strsplit(trimws(info_lines[raw_idx[1] + 2]), '\\s+')[[1]]
      flag_idx <- which(tokens %in% c('-n', '--n-draws-per-record'))
      if (length(flag_idx) > 0 && (flag_idx[1] + 1) <= length(tokens)) {
        src_n_draws <- tokens[flag_idx[1] + 1]
      }
    }
  }
  if (src_n_draws != as.character(n_draws_per_record)) {
    stop('n_draws_per_record mismatch with baseline interface ', baseline_interface, ':\n',
         '  interface used ', src_n_draws, ', current run uses ', n_draws_per_record, '\n',
         '  Gumbel draw pairing requires identical draws per record.')
  }

  # Estimation lineage: warn if the current run's estimation data does not
  # come from the baseline interface itself (cannot be fully verified here;
  # the per-year key/value assertions in load_baseline_year_result() are the
  # backstop against silent mismatches).
  resolved_calibration <- if (exists('calibration_interface')) calibration_interface else NULL
  if (is.null(resolved_calibration) || resolved_calibration != baseline_interface) {
    cat('  WARNING: --baseline-interface is set but --calibration-interface (-C) does not\n')
    cat('           point at the same run. Baseline reuse requires identical estimation\n')
    cat('           data; per-year integrity assertions will abort on any mismatch.\n')
  }

  #-----------------------------------------
  # Check 3: employment targeting consistency
  #-----------------------------------------

  targeting_enabled <- !(exists('disable_employment_targeting') &&
                          isTRUE(disable_employment_targeting))
  targeting_file <- file.path(baseline_dir, 'models', 'equilibrium',
                              paste0('employment_targeting_', min(years_to_run), '.csv'))
  if (targeting_enabled && !file.exists(targeting_file)) {
    stop('Baseline interface ', baseline_interface, ' has no employment targeting output\n',
         '  (expected ', targeting_file, ').\n',
         '  The prior run likely used --no-employment-targeting. Pass the same flag, or\n',
         '  reuse a baseline that was solved with employment targeting enabled.')
  }
  if (!targeting_enabled && file.exists(targeting_file)) {
    stop('Baseline interface ', baseline_interface, ' was solved WITH employment targeting,\n',
         '  but the current run passes --no-employment-targeting. Drop the flag, or reuse\n',
         '  a baseline that was solved without targeting.')
  }

  #-----------------------------------------
  # Check 4: per-year files must exist
  #-----------------------------------------

  for (year in years_to_run) {
    required_files <- c(
      file.path(baseline_dir, 'data', paste0('parent_units_c1_', year, '.csv')),
      file.path(baseline_dir, 'data', paste0('parent_units_c2plus_', year, '.csv')),
      file.path(baseline_dir, 'supply', paste0('supply_', year, '.yaml'))
    )
    missing <- required_files[!file.exists(required_files)]
    if (length(missing) > 0) {
      stop('Baseline interface ', baseline_interface, ' is missing files for year ', year, ':\n',
           paste0('  ', missing, collapse = '\n'), '\n',
           '  The prior run must cover every year in the current runscript (baseline may\n',
           '  not have converged for this year, or the run used a shorter year range).')
    }
  }

  #-----------------------------------------
  # Check 5: percentage employer subsidies need stored equilibrium wages
  #-----------------------------------------

  needs_wages <- any(sapply(counterfactual_infos, function(info) {
    isTRUE(info$employer_subsidy_config$enabled) &&
      identical(info$employer_subsidy_config$type, 'percentage')
  }))
  if (needs_wages) {
    first_yaml <- read_yaml(file.path(baseline_dir, 'supply',
                                      paste0('supply_', min(years_to_run), '.yaml')))
    if (is.null(first_yaml$equilibrium_wages)) {
      stop('A counterfactual uses a percentage employer subsidy, which needs equilibrium\n',
           '  wages from the baseline, but interface ', baseline_interface, ' predates the\n',
           '  equilibrium_wages field in supply_<year>.yaml. Re-run the baseline with\n',
           '  current code, or use a dollar-type employer subsidy.')
    }
  }

  cat('Baseline interface', baseline_interface, 'validated:\n')
  cat('  Years covered:', min(years_to_run), '-', max(years_to_run), '\n')
  cat('  Baseline equilibrium solves will be skipped.\n')
}



load_baseline_year_result <- function(baseline_interface, year, parent_units_list,
                                      median_income_lookup, cpi_factor_2019,
                                      cpi_chain_factor_2019, cpi_chain_factor_2026,
                                      n_draws_per_record,
                                      wage_growth_factor_2019 = 1.0,
                                      wage_growth_factor_2026 = 1.0) {

  #----------------------------------------------------------------------------
  # Loads one year of converged baseline results from a prior run, returning
  # a synthetic baseline_result with the fields the counterfactual path
  # consumes. Asserts key-set and value agreement with the freshly prepared
  # parent units so a stale or mismatched interface fails loudly.
  #
  # Params:
  #   - baseline_interface (chr): Timestamp of the prior run to reuse
  #   - year (int): Simulation year to load
  #   - parent_units_list (list): Freshly prepared parent units for this year
  #       (pre-expansion), used for integrity assertions
  #   - median_income_lookup (named vec): Median AGI by family size for this year
  #   - cpi_factor_2019 (dbl): CPI growth factor 2019 -> year
  #   - cpi_chain_factor_2019 (dbl): Chained CPI growth factor 2019 -> year
  #   - cpi_chain_factor_2026 (dbl): Chained CPI growth factor 2019 -> 2026
  #   - n_draws_per_record (int): Epsilon draws per record in the current run
  #   - wage_growth_factor_2019 (dbl): Nominal hourly wage growth factor 2019 -> year
  #   - wage_growth_factor_2026 (dbl): Nominal hourly wage growth factor 2019 -> 2026
  #
  # Returns: (list) Synthetic baseline result with $converged, $prices, $w,
  #   $employment_shifts, $parent_units, $supply_subsidy
  #----------------------------------------------------------------------------

  baseline_dir <- file.path(default_paths$roots$output, baseline_interface,
                            'simulation', 'baseline')

  #-----------------------------------------
  # Load collapsed parent units per type
  #-----------------------------------------

  # Prefer the exact binary copy (parent_units_<year>.rds); fall back to the
  # CSVs for runs made before the RDS was added. CSV doubles round-trip with
  # ~1e-11 errors, which can flip boundary classifications (e.g. SPM poverty)
  # in the distributional/poverty comparisons.
  rds_path <- file.path(baseline_dir, 'data', paste0('parent_units_', year, '.rds'))
  raw_parent_units <- NULL
  if (file.exists(rds_path)) {
    raw_parent_units <- readRDS(rds_path)
  } else {
    cat('  Note: no parent_units_', year, '.rds in baseline interface; falling back\n',
        '        to CSVs. Poverty/distributional outputs may show tiny boundary\n',
        '        differences vs the source run (~1e-11 value round-trip).\n', sep = '')
  }

  loaded_parent_units <- list()
  for (pu_name in PARENT_UNIT_NAMES) {
    if (!is.null(raw_parent_units)) {
      loaded <- raw_parent_units[[pu_name]]
      if (is.null(loaded)) {
        stop('Baseline interface RDS is missing parent unit type ', pu_name,
             ' for year ', year, ': ', rds_path)
      }
    } else {
      pu_path <- file.path(baseline_dir, 'data',
                           paste0('parent_units_', pu_name, '_', year, '.csv'))
      if (!file.exists(pu_path)) {
        stop('Baseline interface file not found: ', pu_path)
      }
      loaded <- tibble(fread(pu_path, na.strings = 'NA'))
    }

    prepared <- parent_units_list[[pu_name]]
    if (is.null(prepared) || nrow(prepared) == 0) {
      stop('No prepared parent units for type ', pu_name, ' in year ', year,
           ' but baseline interface has ', nrow(loaded), ' rows.')
    }

    # -- Assertion: row count = prepared units x epsilon draws ----
    expected_rows <- nrow(prepared) * n_draws_per_record
    if (nrow(loaded) != expected_rows) {
      stop('Baseline interface row count mismatch for ', pu_name, ' in year ', year, ':\n',
           '  loaded ', nrow(loaded), ' rows, expected ', expected_rows,
           ' (', nrow(prepared), ' units x ', n_draws_per_record, ' draws).\n',
           '  The interface was generated with a different sample or draw count.')
    }

    # -- Assertion: unit key sets must be identical ----
    key_cols <- c('hh_id', 'parent_unit_id', 'pseudofamily_id')
    loaded_keys <- loaded %>%
      distinct(across(all_of(key_cols))) %>%
      mutate(across(everything(), as.character))
    prepared_keys <- prepared %>%
      distinct(across(all_of(key_cols))) %>%
      mutate(across(everything(), as.character))
    n_unmatched <- nrow(anti_join(loaded_keys, prepared_keys, by = key_cols)) +
                   nrow(anti_join(prepared_keys, loaded_keys, by = key_cols))
    if (n_unmatched > 0) {
      stop('Baseline interface key mismatch for ', pu_name, ' in year ', year, ':\n',
           '  ', n_unmatched, ' parent unit keys differ between the interface and the\n',
           '  freshly prepared data. The interface was generated from different\n',
           '  estimation data or sample. Use -C pointing at the same run.')
    }

    # -- Assertion: aged values must agree (catches vintage mismatches) ----
    # agi.ft catches macro projection / aging differences; total_tax.ft
    # additionally catches donor pool differences. Expanded rows carry
    # per_weight1 / n_draws, so the sum over all epsilon rows reproduces the
    # pre-expansion total directly. Tolerance covers CSV float round-tripping.
    for (check_col in c('agi.ft', 'total_tax.ft')) {
      loaded_total   <- sum(loaded[[check_col]] * loaded$per_weight1, na.rm = TRUE)
      prepared_total <- sum(prepared[[check_col]] * prepared$per_weight1, na.rm = TRUE)
      rel_diff <- abs(loaded_total - prepared_total) / max(abs(prepared_total), 1)
      if (rel_diff > 1e-6) {
        stop('Baseline interface value mismatch for ', pu_name, ' in year ', year, ':\n',
             '  weighted ', check_col, ' differs by ', signif(rel_diff, 3), ' (relative).\n',
             '  The interface was generated under different macro projections, tax data,\n',
             '  or aging inputs. Regenerate the baseline or align data vintages.')
      }
    }

    loaded_parent_units[[pu_name]] <- loaded
  }

  # Reattach simulation context columns (stripped before disk output)
  loaded_parent_units <- add_simulation_variables(
    loaded_parent_units,
    median_income_lookup,
    cpi_factor_2019       = cpi_factor_2019,
    cpi_chain_factor_2019 = cpi_chain_factor_2019,
    cpi_chain_factor_2026 = cpi_chain_factor_2026,
    wage_growth_factor_2019 = wage_growth_factor_2019,
    wage_growth_factor_2026 = wage_growth_factor_2026
  )

  #-----------------------------------------
  # Load equilibrium prices and wages
  #-----------------------------------------

  supply_yaml_path <- file.path(baseline_dir, 'supply', paste0('supply_', year, '.yaml'))
  if (!file.exists(supply_yaml_path)) {
    stop('Baseline interface supply file not found: ', supply_yaml_path)
  }
  supply_yaml <- read_yaml(supply_yaml_path)

  if (is.null(supply_yaml$equilibrium_prices)) {
    stop('No equilibrium_prices in ', supply_yaml_path, '\n',
         '  The baseline for year ', year, ' did not converge in the source run.')
  }
  sector_keys <- c('unpaid_center_based', 'low_price_center_based',
                   'high_price_center_based', 'paid_home_based')
  prices <- as.numeric(unlist(supply_yaml$equilibrium_prices[sector_keys]))
  stopifnot(length(prices) == 4, all(is.finite(prices)))

  # Equilibrium wages (present only in runs made after the field was added)
  w <- NULL
  if (!is.null(supply_yaml$equilibrium_wages)) {
    w <- c(supply_yaml$equilibrium_wages$no_ba, supply_yaml$equilibrium_wages$ba)
  }

  #-----------------------------------------
  # Load employment shifts
  #-----------------------------------------

  employment_shifts <- NULL
  targeting_path <- file.path(baseline_dir, 'models', 'equilibrium',
                              paste0('employment_targeting_', year, '.csv'))
  if (file.exists(targeting_path)) {
    targeting_df <- read_csv(targeting_path, show_col_types = FALSE)
    employment_shifts <- setNames(targeting_df$delta_shift, targeting_df$group)
    missing_groups <- setdiff(EMPLOYMENT_TARGETING_GROUPS, names(employment_shifts))
    if (length(missing_groups) > 0) {
      stop('Baseline interface employment targeting file is missing groups: ',
           paste(missing_groups, collapse = ', '), '\n  File: ', targeting_path)
    }
  }

  cat('  Loaded baseline from interface', baseline_interface, 'for year', year, '\n')
  cat(sprintf('    Prices: [%.4f, %.4f, %.4f, %.4f]\n',
              prices[1], prices[2], prices[3], prices[4]))

  list(
    converged         = TRUE,
    prices            = prices,
    w                 = w,
    parent_units      = loaded_parent_units,
    employment_shifts = employment_shifts,
    supply_subsidy    = c(0, 0, 0, 0)
  )
}
