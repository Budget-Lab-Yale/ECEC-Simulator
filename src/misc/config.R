#----------------------------------------------------------
# config.R
#
# Functions and operations to configure file paths and
# parse tax law
#----------------------------------------------------------


# Parse command line arguments
parse_args <- function() {

  #----------------------------------------------------------------------------
  # Parses command line arguments for the ECEC Simulator
  #
  # Params: none
  #
  # Returns: (list) Parsed command line arguments
  #----------------------------------------------------------------------------

  args <- commandArgs(trailingOnly = TRUE)

  # Default values
  runscript_id            <- NULL
  nsece_interface         <- NULL
  acs_interface           <- NULL
  calibration_interface   <- NULL
  use_cached_etrs         <- FALSE
  use_cached_donors       <- FALSE # If TRUE, use cached donor pool; if FALSE, rebuild
  calib_sample            <- 100
  sim_sample              <- 100
  n_draws_per_record      <- 10
  seed_offset             <- 0
  setup_only              <- FALSE
  slurm_phase             <- NULL  # 'setup', 'year', or 'finalize'
  slurm_scratch_dir       <- NULL  # Path to scratch directory for inter-job communication
  slurm_year_index        <- NULL  # 1-based index into years vector
  fiscal_npv              <- FALSE # Requires Tax-Simulator data
  disable_price_wedge     <- FALSE # If TRUE, set all price wedges to 1.0 (no heterogeneity)
  disable_employment_targeting <- FALSE # If TRUE, skip employment targeting in equilibrium
  reference_interface     <- NULL  # If set, produce regression delta CSVs vs this prior run
  calib_target1_name      <- 'part_cost'  # First calibration target elasticity
  calib_target1_value     <- NULL         # Value (NULL = use default from constants.R)
  calib_target2_name      <- 'hours_income'  # Second calibration target elasticity
  calib_target2_value     <- NULL         # Value (NULL = use default -0.05)

  consume_arg <- function(flag_name) {

    #--------------------------------------------------------------------------
    # Consumes the next argument value and advances the index.
    #
    # Params:
    #   - flag_name (chr): Name of the flag (for error messages)
    #
    # Returns: (chr) The consumed argument value
    #--------------------------------------------------------------------------

    if (i + 1 > length(args)) {
      stop(paste('Error:', flag_name, 'requires a value'))
    }
    val <- args[i + 1]
    i <<- i + 2
    val
  }

  # Parse arguments
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]

    if (arg == '--runscript' || arg == '-r') {
      runscript_id <- consume_arg('--runscript')
    } else if (arg == '--nsece-interface' || arg == '-N') {
      nsece_interface <- consume_arg('--nsece-interface')
    } else if (arg == '--acs-interface' || arg == '-A') {
      acs_interface <- consume_arg('--acs-interface')
    } else if (arg == '--calibration-interface' || arg == '-C') {
      calibration_interface <- consume_arg('--calibration-interface')
    } else if (arg == '--base-year-interface' || arg == '-B') {
      consume_arg('--base-year-interface')  # Deprecated: consume and discard
      cat('Warning: --base-year-interface is deprecated and will be ignored.\n')
      cat('         Base year data is now read directly from estimation output.\n')
      cat('         Use --calibration-interface (-C) instead to reuse all estimation data.\n')
    } else if (arg == '--use-cached-etrs' || arg == '-c') {
      use_cached_etrs <- TRUE
      i <- i + 1
    } else if (arg == '--use-cached-donors' || arg == '-d') {
      use_cached_donors <- TRUE
      i <- i + 1
    } else if (arg == '--calib-sample' || arg == '-a') {
      calib_sample <- as.integer(consume_arg('--calib-sample'))
      if (is.na(calib_sample) || calib_sample < 1 || calib_sample > 100) {
        stop('Error: --calib-sample must be an integer between 1 and 100')
      }
    } else if (arg == '--sim-sample') {
      sim_sample <- as.integer(consume_arg('--sim-sample'))
      if (is.na(sim_sample) || sim_sample < 1 || sim_sample > 100) {
        stop('Error: --sim-sample must be an integer between 1 and 100')
      }
    } else if (arg == '--setup-only') {
      setup_only <- TRUE
      i <- i + 1
    } else if (arg == '--n-draws-per-record' || arg == '-n') {
      n_draws_per_record <- as.integer(consume_arg('--n-draws-per-record'))
      if (is.na(n_draws_per_record) || n_draws_per_record < 1) {
        stop('Error: --n-draws-per-record must be a positive integer')
      }
    } else if (arg == '--seed' || arg == '-s') {
      seed_offset <- as.integer(consume_arg('--seed'))
      if (is.na(seed_offset)) {
        stop('Error: --seed must be an integer')
      }
    } else if (arg == '--slurm-phase') {
      slurm_phase <- consume_arg('--slurm-phase')
      valid_phases <- c('setup', 'year', 'finalize')
      if (!(slurm_phase %in% valid_phases)) {
        stop(paste('Error: --slurm-phase must be one of:', paste(valid_phases, collapse = ', ')))
      }
    } else if (arg == '--slurm-scratch') {
      slurm_scratch_dir <- consume_arg('--slurm-scratch')
    } else if (arg == '--slurm-year-index') {
      slurm_year_index <- as.integer(consume_arg('--slurm-year-index'))
      if (is.na(slurm_year_index) || slurm_year_index < 1) {
        stop('Error: --slurm-year-index must be a positive integer')
      }
    } else if (arg == '--fiscal-npv' || arg == '-f') {
      fiscal_npv <- TRUE
      i <- i + 1
    } else if (arg == '--per-type-calibration') {
      stop('--per-type-calibration has been removed. Calibration is pooled across parent unit types.')
    } else if (arg == '--calib-target1-name') {
      calib_target1_name <- consume_arg('--calib-target1-name')
      valid_names <- c('part_wage', 'part_cost', 'paid_cost', 'hours_income')
      if (!(calib_target1_name %in% valid_names)) {
        stop(paste('Error: --calib-target1-name must be one of:', paste(valid_names, collapse = ', ')))
      }
    } else if (arg == '--calib-target1-value') {
      calib_target1_value <- as.numeric(consume_arg('--calib-target1-value'))
      if (is.na(calib_target1_value)) {
        stop('Error: --calib-target1-value must be a number')
      }
    } else if (arg == '--calib-target2-name') {
      calib_target2_name <- consume_arg('--calib-target2-name')
      valid_names <- c('part_wage', 'part_cost', 'paid_cost', 'hours_income')
      if (!(calib_target2_name %in% valid_names)) {
        stop(paste('Error: --calib-target2-name must be one of:', paste(valid_names, collapse = ', ')))
      }
    } else if (arg == '--calib-target2-value') {
      calib_target2_value <- as.numeric(consume_arg('--calib-target2-value'))
      if (is.na(calib_target2_value)) {
        stop('Error: --calib-target2-value must be a number')
      }
    } else if (arg == '--reference' || arg == '-R') {
      reference_interface <- consume_arg('--reference')
    } else if (arg == '--no-price-wedge') {
      disable_price_wedge <- TRUE
      i <- i + 1
    } else if (arg == '--no-employment-targeting') {
      disable_employment_targeting <- TRUE
      i <- i + 1
    } else if (arg == '--help' || arg == '-h') {
      print_usage()
      quit(status = 0)
    } else {
      stop(paste('Error: Unknown argument:', arg))
    }
  }

  # Validate required arguments
  if (is.null(runscript_id)) {
    cat('Error: runscript_id is required\n')
    print_usage()
    quit(status = 1)
  }

  # Validate sim_sample <= calib_sample
  if (sim_sample > calib_sample) {
    stop(paste0('Error: --sim-sample (', sim_sample, ') must be <= --calib-sample (', calib_sample, ')'))
  }

  return(list(
    runscript_id            = runscript_id,
    nsece_interface         = nsece_interface,
    acs_interface           = acs_interface,
    calibration_interface   = calibration_interface,
    use_cached_etrs         = use_cached_etrs,
    use_cached_donors       = use_cached_donors,
    calib_sample            = calib_sample,
    sim_sample              = sim_sample,
    n_draws_per_record      = n_draws_per_record,
    seed_offset             = seed_offset,
    setup_only              = setup_only,
    slurm_phase             = slurm_phase,
    slurm_scratch_dir       = slurm_scratch_dir,
    slurm_year_index        = slurm_year_index,
    fiscal_npv              = fiscal_npv,
    disable_price_wedge     = disable_price_wedge,
    disable_employment_targeting = disable_employment_targeting,
    reference_interface     = reference_interface,
    calib_target1_name      = calib_target1_name,
    calib_target1_value     = calib_target1_value,
    calib_target2_name      = calib_target2_name,
    calib_target2_value     = calib_target2_value
  ))
}



print_usage <- function() {

  #----------------------------------------------------------------------------
  # Prints usage information for the ECEC Simulator.
  #
  # Params: none
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  cat('Usage: Rscript src/main.R --runscript <runscript_id> [OPTIONS]\n')
  cat('\n')
  cat('Arguments:\n')
  cat('  --runscript, -r             ID of the runscript to use (e.g., test)\n')
  cat('  --nsece-interface, -N       Timestamp of NSECE estimation interface to reuse (optional)\n')
  cat('                              Reuses NSECE household data, supply params, and price models\n')
  cat('  --acs-interface, -A         Timestamp of ACS data interface to reuse (optional)\n')
  cat('                              Reuses ACS data only (households, enrollment, imputations)\n')
  cat('                              Calibration will run fresh. If -N not specified, loads NSECE too\n')
  cat('  --calibration-interface, -C Timestamp of calibration interface to reuse (optional)\n')
  cat('                              Reuses ACS data + calibration data + demand params\n')
  cat('                              Implies -A (no ACS processing). If -N not specified, loads NSECE too\n')
  cat('  --base-year-interface, -B   [Deprecated] Now ignored - base year data read from estimation\n')
  cat('                              Use --calibration-interface (-C) instead\n')
  cat('  --use-cached-etrs, -c       Use cached ETR lookup tables instead of Tax-Simulator data\n')
  cat('                              Useful when Tax-Simulator data is not available\n')
  cat('  --use-cached-donors, -d     Use cached donor pool instead of rebuilding from Tax-Simulator\n')
  cat('                              Default: always rebuild. Cache must exist or will fail loudly\n')
  cat('  --calib-sample, -a          Percentage of ACS sample for calibration (1-100, default: 100)\n')
  cat('                              Controls sample size for processing + calibration phases\n')
  cat('  --sim-sample                Percentage of ACS sample for simulation (1-100, default: 100)\n')
  cat('                              Must be <= --calib-sample. Sub-samples from calibration data\n')
  cat('  --setup-only                Run processing + calibration only, skip simulation\n')
  cat('                              Useful for verifying setup before launching simulation jobs\n')
  cat('  --n-draws-per-record, -n    Number of Monte Carlo epsilon draws per record (default: 10)\n')
  cat('                              Higher values reduce simulation noise but increase runtime\n')
  cat('  --seed, -s                  Seed offset for Gumbel epsilon draws (default: 0)\n')
  cat('                              Different values produce different random draws for variance testing\n')
  cat('  --slurm-phase <phase>       SLURM job array phase: setup, year, or finalize\n')
  cat('                              setup: processing + calibration + save context to scratch\n')
  cat('                              year: run one year (use with --slurm-year-index)\n')
  cat('                              finalize: combine year results and write output\n')
  cat('  --slurm-scratch <path>      Path to scratch directory for inter-job communication\n')
  cat('                              Required when --slurm-phase is specified\n')
  cat('  --slurm-year-index <N>      1-based index into years vector (maps to SLURM_ARRAY_TASK_ID)\n')
  cat('                              Required when --slurm-phase is year\n')
  cat('  --fiscal-npv, -f            Enable fiscal NPV and child earnings calculations\n')
  cat('                              Requires Tax-Simulator data (tax_units files)\n')
  cat('  --calib-target1-name <name> First calibration target elasticity (default: part_cost)\n')
  cat('                              Valid: part_wage, part_cost, paid_cost, hours_income\n')
  cat('  --calib-target1-value <val> Value for first target (default: from constants.R)\n')
  cat('  --calib-target2-name <name> Second calibration target elasticity (default: hours_income)\n')
  cat('                              Valid: part_wage, part_cost, paid_cost, hours_income\n')
  cat('  --calib-target2-value <val> Value for second target (default: -0.05 for hours_income)\n')
  cat('  --reference, -R             Timestamp of a prior run to compare against (regression deltas)\n')
  cat('                              Produces regression.csv per common scenario in totals/\n')
  cat('  --no-price-wedge            Disable price wedge heterogeneity (set all wedges to 1.0)\n')
  cat('                              Useful for testing or isolating price heterogeneity effects\n')
  cat('  --no-employment-targeting   Disable employment targeting in equilibrium solver\n')
  cat('                              Skips solving for employment-shift deltas\n')
  cat('  --help, -h                  Show this help message\n')
  cat('\n')
  cat('Examples:\n')
  cat('  Rscript src/main.R --runscript test\n')
  cat('  Rscript src/main.R -r test -C 202509091920                  # Reuse all (NSECE + ACS + calibration)\n')
  cat('  Rscript src/main.R -r test -A 202509091920                  # Reuse ACS, run fresh calibration\n')
  cat('  Rscript src/main.R -r test -N 202509091920                  # Reuse NSECE only, run fresh ACS+calibration\n')
  cat('  Rscript src/main.R -r test -N 202509091920 -A 202509091920  # Reuse NSECE+ACS, run fresh calibration\n')
  cat('  Rscript src/main.R -r test -N 202509091920 -C 202509091920  # Reuse all with explicit NSECE\n')
  cat('  Rscript src/main.R -r test -c  # Use cached ETRs\n')
  cat('  Rscript src/main.R -r test -d  # Use cached donor pool (must exist)\n')
  cat('  Rscript src/main.R -r test -a 10  # Use 10% of ACS for calibration\n')
  cat('  Rscript src/main.R -r test --sim-sample 10  # 100% calibration, 10% simulation\n')
  cat('  Rscript src/main.R -r test --setup-only  # Run processing + calibration only\n')
  cat('  Rscript src/main.R -r test --slurm-phase setup --slurm-scratch slurm/scratch/test\n')
  cat('  Rscript src/main.R -r test --slurm-phase year --slurm-scratch slurm/scratch/test --slurm-year-index 1\n')
  cat('  Rscript src/main.R -r test --slurm-phase finalize --slurm-scratch slurm/scratch/test\n')
  cat('  Rscript src/main.R -r test -f  # Enable fiscal NPV calculations\n')
  cat('  Rscript src/main.R -r test --calib-target1-name part_cost --calib-target1-value -0.15\n')
  cat('  Rscript src/main.R -r test --calib-target1-name paid_cost --calib-target2-name hours_income\n')
}



resolve_interface_hierarchy <- function(nsece_interface, acs_interface, calibration_interface) {

  #----------------------------------------------------------------------------
  # Resolves the interface hierarchy to determine effective interfaces.
  #
  # Interface hierarchy (each level implies the previous):
  #   calibration_interface > acs_interface > nsece_interface
  #
  # This means:
  #   - If calibration_interface is set, it provides NSECE, ACS, and calibration data
  #   - If acs_interface is set, it provides NSECE and ACS data
  #   - If nsece_interface is set, it provides only NSECE data
  #
  # Params:
  #   - nsece_interface (chr): Explicit NSECE interface timestamp (or NULL)
  #   - acs_interface (chr): Explicit ACS interface timestamp (or NULL)
  #   - calibration_interface (chr): Explicit calibration interface timestamp (or NULL)
  #
  # Returns:
  #   list with:
  #     - nsece (chr or NULL): Effective NSECE interface
  #     - acs (chr or NULL): Effective ACS interface
  #     - calibration (chr or NULL): Effective calibration interface
  #----------------------------------------------------------------------------

  list(
    nsece = nsece_interface %||% calibration_interface %||% acs_interface,
    acs = calibration_interface %||% acs_interface,
    calibration = calibration_interface

)
}



get_estimation_info <- function() {

  #----------------------------------------------------------------------------
  # Creates configuration info for estimation data preparation
  #
  # Params: none
  #
  # Returns: (list) Paths and options for estimation data preparation
  #----------------------------------------------------------------------------

  estimation_info <- list()

  # Create data dependency paths
  estimation_info$paths = default_paths$dependencies %>%
    map(.f = ~ file.path(default_paths$roots$input, .x))

  # Create estimation output paths
  estimation_info$paths$output = file.path(output_root, 'estimation')
  estimation_info$paths$data   = file.path(estimation_info$paths$output, 'data')
  estimation_info$paths$models = file.path(estimation_info$paths$output, 'models')
  estimation_info$paths$supply = file.path(estimation_info$paths$output, 'supply')

  dir.create(estimation_info$paths$output, recursive = T, showWarnings = F)
  dir.create(estimation_info$paths$data, showWarnings = F)
  dir.create(estimation_info$paths$models, showWarnings = F)
  dir.create(estimation_info$paths$supply, showWarnings = F)

  return(estimation_info)
}



get_scenario_info <- function(scenario_id) {

  #----------------------------------------------------------------------------
  # Parses runscript info for given scenario ID into list format.
  # All scenarios (including baseline) are now loaded from runscript.
  #
  # Params:
  #   - scenario_id (chr): Scenario ID from runscript
  #
  # Returns: (list) Runscript elements for specified scenario
  #----------------------------------------------------------------------------

  # Get scenario info from runscript
  scenario_info <- runscript %>%
    filter(id == scenario_id) %>%
    unlist() %>%
    as.list()

  # Parse years
  years <- scenario_info$years %>%
    str_split_1(':') %>%
    as.integer()
  scenario_info$years = years[1]:years[2]

  # Create data dependency paths
  scenario_info$paths = list()
  scenario_info$paths = default_paths$dependencies %>%
    map(.f = ~ file.path(default_paths$roots$input, .x))

  # Create output paths (under simulation/ subdirectory)
  scenario_info$paths$output = file.path(output_root, 'simulation', scenario_id)
  dir.create(scenario_info$paths$output, recursive = T, showWarnings = F)
  dir.create(file.path(scenario_info$paths$output, 'data'), showWarnings = F)
  dir.create(file.path(scenario_info$paths$output, 'models'), showWarnings = F)
  dir.create(file.path(scenario_info$paths$output, 'supply'), showWarnings = F)
  dir.create(file.path(scenario_info$paths$output, 'totals'), showWarnings = F)
  dir.create(file.path(scenario_info$paths$output, 'totals', 'levels'), showWarnings = F)

  # Create deltas folder for non-baseline scenarios
  if (scenario_id != 'baseline') {
    dir.create(file.path(scenario_info$paths$output, 'totals', 'deltas'), showWarnings = F)
  }

  # Add runtime options
  scenario_info$options = list(
    use_cached_etrs = if (exists('use_cached_etrs')) use_cached_etrs else FALSE,
    use_cached_donors = if (exists('use_cached_donors')) use_cached_donors else FALSE
  )

  # Equilibrium solver functions
  scenario_info$equilibrium = list(
    policy_demand = scenario_info$policy_demand,
    policy_supply = scenario_info$policy_supply
  )

  # Load wage floor configuration
  wage_floor_name <- scenario_info$wage_floor
  if (is.null(wage_floor_name) || is.na(wage_floor_name) || wage_floor_name == '') {
    stop('wage_floor is required in runscript. Use "none" for no wage floor policy.')
  }
  wf_config_path <- file.path('./config/wage_floor', paste0(wage_floor_name, '.yaml'))
  if (!file.exists(wf_config_path)) {
    stop(paste0('Wage floor config file not found: ', wf_config_path))
  }
  wf_config <- read_yaml(wf_config_path)
  if (!isTRUE(wf_config$enabled)) {
    scenario_info$wage_floor_config <- list(enabled = FALSE)
  } else {
    scenario_info$wage_floor_config <- list(
      enabled   = TRUE,
      floor     = c(wf_config$floor$no_ba, wf_config$floor$ba_plus),
      base_year = wf_config$base_year
    )
  }

  # Load employer subsidy configuration (defaults to 'none' if not specified)
  employer_subsidy_name <- scenario_info$employer_subsidy
  if (is.null(employer_subsidy_name) || is.na(employer_subsidy_name) ||
      employer_subsidy_name == '') {
    employer_subsidy_name <- 'none'
  }
  es_config_path <- file.path('./config/employer_subsidy', paste0(employer_subsidy_name, '.yaml'))
  if (!file.exists(es_config_path)) {
    stop(paste0('Employer subsidy config file not found: ', es_config_path))
  }
  es_config <- read_yaml(es_config_path)
  if (!isTRUE(es_config$enabled)) {
    scenario_info$employer_subsidy_config <- list(enabled = FALSE)
  } else {
    es_subsidy_type <- es_config$type %||% 'dollar'
    if (es_subsidy_type == 'percentage') {
      scenario_info$employer_subsidy_config <- list(
        enabled         = TRUE,
        type            = 'percentage',
        rate            = es_config$rate,
        covered_sectors = es_config$covered_sectors
      )
    } else {
      scenario_info$employer_subsidy_config <- list(
        enabled         = TRUE,
        type            = 'dollar',
        subsidy         = c(es_config$subsidy$no_ba, es_config$subsidy$ba_plus),
        base_year       = es_config$base_year,
        covered_sectors = es_config$covered_sectors
      )
    }
  }

  # Parse phase_in_years for transition/take-up modeling
  # If specified, linearly interpolate from baseline to full policy over N years
  # e.g., phase_in_years = 4 means 25%, 50%, 75%, 100% in years 1-4
  if (!is.null(scenario_info$phase_in_years) &&
      !is.na(scenario_info$phase_in_years) &&
      scenario_info$phase_in_years != '') {
    n_phase_in <- as.integer(scenario_info$phase_in_years)
    n_years <- length(scenario_info$years)

    # Build phase-in sequence: ramp up over n_phase_in years, then stay at 1
    phase_in_seq <- pmin(seq_len(n_years) / n_phase_in, 1)
    scenario_info$phase_in <- phase_in_seq
  } else {
    # Default: immediate full implementation (a_t = 1 for all years)
    scenario_info$phase_in <- rep(1, length(scenario_info$years))
  }

  return(scenario_info)
}


save_run_metadata <- function() {

  #----------------------------------------------------------------------------
  # Saves run metadata to output folder for reproducibility.
  # Creates a metadata/ subdirectory containing:
  #   - run_info.txt: Command line args and runtime parameters
  #   - runscript.csv: Copy of the runscript used for this run
  #
  # Params: none (uses global variables)
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------


  # Create metadata directory
  metadata_dir <- file.path(output_root, 'metadata')
  dir.create(metadata_dir, showWarnings = FALSE)

  # Build run info content
  null_or_val <- function(x) {
    #--------------------------------------------------------------------------
    # Returns 'NULL' string if x is NULL, otherwise x.
    #
    # Params:
    #   - x (any): Value to check
    #
    # Returns: (any) x or 'NULL' string
    #--------------------------------------------------------------------------
    if (is.null(x)) 'NULL' else x
  }
  exist_or_default <- function(name, default) {
    #--------------------------------------------------------------------------
    # Returns the value of a variable if it exists, otherwise the default.
    #
    # Params:
    #   - name (chr): Variable name to look up
    #   - default (any): Default value if variable does not exist
    #
    # Returns: (any) Variable value or default
    #--------------------------------------------------------------------------
    if (exists(name)) get(name) else default
  }

  arg_fields <- list(
    runscript_id             = runscript_id,
    nsece_interface          = null_or_val(nsece_interface),
    acs_interface            = null_or_val(acs_interface),
    calibration_interface    = null_or_val(calibration_interface),
    use_cached_etrs          = use_cached_etrs,
    use_cached_donors        = use_cached_donors,
    calib_sample             = calib_sample,
    sim_sample               = sim_sample,
    seed_offset              = exist_or_default('seed_offset', 0),
    acs_base_year            = '2019 (fixed for alpha matrix matching)',
    slurm_phase              = null_or_val(slurm_phase),
    slurm_scratch_dir        = null_or_val(slurm_scratch_dir),
    slurm_year_index         = null_or_val(slurm_year_index),
    fiscal_npv               = fiscal_npv,
    disable_price_wedge      = exist_or_default('disable_price_wedge', FALSE),
    calib_target1_name       = exist_or_default('calib_target1_name', 'part_cost'),
    calib_target1_value      = if (exists('calib_target1_value') && !is.null(calib_target1_value)) calib_target1_value else 'default',
    calib_target2_name       = exist_or_default('calib_target2_name', 'hours_income'),
    calib_target2_value      = if (exists('calib_target2_value') && !is.null(calib_target2_value)) calib_target2_value else 'default'
  )
  arg_lines <- paste('  ', names(arg_fields), ':', sapply(arg_fields, as.character))

  run_info <- c(
    'ECEC Simulator Run Metadata',
    paste(rep('=', 50), collapse = ''),
    '',
    paste('Timestamp:', time_stamp),
    paste('Run Date:', format(Sys.time(), '%Y-%m-%d %H:%M:%S')),
    '',
    'Command Line Arguments:',
    paste(rep('-', 30), collapse = ''),
    arg_lines,
    '',
    'Raw Command Line (if non-interactive):',
    paste(rep('-', 30), collapse = ''),
    paste(' ', paste(commandArgs(trailingOnly = TRUE), collapse = ' '))
  )

  # Write run info
  writeLines(run_info, file.path(metadata_dir, 'run_info.txt'))

  # Copy runscript to metadata folder
  runscript_source <- file.path('./config/runscripts/', paste0(runscript_id, '.csv'))
  file.copy(runscript_source, file.path(metadata_dir, 'runscript.csv'))

  cat('Run metadata saved to', metadata_dir, '\n')
}



#---------------
# Executed code
#---------------


# Source centralized constants (care hours, tax rates, optimizer settings)
source('./src/shared_functions/constants.R')

# Parse command line arguments if running from terminal
if (!interactive()) {
  args <- parse_args()
  list2env(args, envir = environment())
  acs_base_year <- 2019  # Fixed for alpha matrix matching
}

# Read runscript
runscript <- file.path('./config/runscripts/', paste0(runscript_id, '.csv')) %>%
  read_csv(show_col_types = F)

# Instantiate global file roots
default_paths <- read_yaml('./config/default_paths.yaml')

# Make seed_offset globally accessible for simulation
seed_offset <<- if (exists('seed_offset')) seed_offset else 0

# Create output root (append seed suffix for non-default seeds to avoid collisions)
time_stamp <- format(Sys.time(), '%Y%m%d%H%M')
if (seed_offset != 0) {
  time_stamp <- paste0(time_stamp, '_seed', seed_offset)
}
output_root <- file.path(default_paths$roots$output, time_stamp)
dir.create(output_root, recursive = T)

# Save run metadata (command line args and runscript) for reproducibility
save_run_metadata()
