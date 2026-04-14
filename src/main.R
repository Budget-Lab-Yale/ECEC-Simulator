#==============================================================================
# ECEC SIMULATOR - MAIN ENTRY POINT
#==============================================================================
#
# This is the main entry point for the Early Childhood Education and Care
# (ECEC) Simulator. The simulator models household childcare choices and
# labor supply decisions under various policy scenarios.
#
# The complete model flow is linear and can be traced through numbered scripts:
#
#   Phase 0: Configuration (this file)
#   Phase 1: Input Processing
#     1a. NSECE Processing  (src/1_processing/1a_nsece_processing.R)
#     1b. ACS Processing    (src/1_processing/1b_acs_processing.R)
#   Phase 2: Calibration
#     2a. Demand Estimation (src/2_calibration/2a_calibration.R)
#   Phase 3: Simulation
#     3a. Initialize        (src/3_simulation/3a_initialize_simulation.R)
#     3b. Year Loop         (src/3_simulation/3b_run_simulation_year.R)
#     3c.   Scenario Solver (src/3_simulation/3c_run_scenario.R)
#     3d.   Accumulate      (src/3_simulation/3d_accumulate_results.R)
#   Phase 4: Output
#     4a. Finalize          (src/4_output/4a_finalize_output.R)
#
# Shared utilities are in src/shared_functions/ (one function per file)
#
#==============================================================================

#------------------------------------------------------------------------------
# SECTION 0: CONFIGURATION
#------------------------------------------------------------------------------
# - Interactive parameters (for RStudio users)
# - Random seed for reproducibility
# - Library loading
# - Source all project files
#------------------------------------------------------------------------------

#----------------
# Set parameters
#----------------

# Interactive users: specify runscript name and other command line args here
if (interactive()) {
  runscript_id             <- 'single_year_2026'
  nsece_interface          <- NULL
  acs_interface            <- NULL
  calibration_interface    <- NULL
  use_cached_etrs          <- F
  use_cached_donors        <- F  # Set to TRUE to use cached donor pool
  calib_sample             <- 100
  sim_sample               <- 100
  acs_base_year            <- 2019  # Fixed for alpha matrix matching
  n_draws_per_record       <- 1
  seed_offset              <- 0
  setup_only               <- F
  slurm_phase              <- NULL  # 'setup', 'year', or 'finalize'
  slurm_scratch_dir        <- NULL  # Path to scratch directory
  slurm_year_index         <- NULL  # 1-based index into years vector
  fiscal_npv               <- F  # Requires Tax-Simulator data
  disable_price_wedge      <- F  # Set to TRUE to disable price heterogeneity
  disable_employment_targeting <- F  # Set to TRUE to skip employment targeting
  calib_target1_name       <- 'part_cost'  # First calibration target elasticity
  calib_target1_value      <- NULL         # Value (NULL = use default)
  calib_target2_name       <- 'hours_income'  # Second calibration target elasticity
  calib_target2_value      <- NULL         # Value (NULL = use default)
}


#-----------
# Configure
#-----------

# Base seed for reproducibility - referenced by processing functions
random_seed_base <<- 76
set.seed(random_seed_base)

# Load required libraries
library(tidyverse)
library(data.table)
library(yaml)
library(Hmisc)
library(sampleSelection)
library(ranger)
library(openxlsx)

# Parse command line arguments and source all functions
source('./src/misc/config.R')
walk(
  .x = list.files('./src', pattern = '\\.R$', recursive = T),
  .f = ~ if (.x != 'main.R' & .x != 'misc/config.R' & .x != '4_output/report_figures.R') source(file.path('./src/', .x))
)


#------------------------------------------------------------------------------
# SLURM PHASE ROUTING
#------------------------------------------------------------------------------
# If --slurm-phase is set, route to the appropriate SLURM handler.
# 'year' and 'finalize' phases skip processing/calibration entirely.
# 'setup' falls through to normal processing + calibration, then saves context.
#------------------------------------------------------------------------------

if (!is.null(slurm_phase)) {
  if (slurm_phase == 'year') {
    slurm_run_year(slurm_scratch_dir, slurm_year_index)
    quit(status = 0)
  } else if (slurm_phase == 'finalize') {
    slurm_finalize(slurm_scratch_dir)
    quit(status = 0)
  }
  # slurm_phase == 'setup' falls through to normal processing + calibration
}


#------------------------------------------------------------------------------
# SECTION 1: INPUT PROCESSING
#------------------------------------------------------------------------------
#
# 1a. NSECE Processing
#     - 2019 NSECE household data
#     - 2019 NSECE supply parameters (wages, labor requirements)
#     - Price heterogeneity models (QRF)
#
# 1b. ACS Processing (demand-side data)
#     - 2019 ACS hierarchical processing
#     - Earnings imputation (Heckman)
#     - ECEC enrollment imputation (Random Forest)
#     - Net cost imputation
#     - Employment probability imputation (Random Forest)
#     - SPM unit processing
#
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# INTERFACE COPY FUNCTIONS
#------------------------------------------------------------------------------

copy_interface_files <- function(source_path, target_path, folders = NULL,
                                 file_patterns = NULL, count_pattern = NULL) {
  
  #----------------------------------------------------------------------------
  # Core helper for copying interface files between directories.
  #
  # Params:
  #   - source_path (chr): Full path to source folder
  #   - target_path (chr): Full path to target folder
  #   - folders (chr vec): Subfolder names to copy recursively
  #   - file_patterns (list): Named list of patterns to copy from subfolders
  #   - count_pattern (chr): Optional regex to filter file count (for logging)
  #
  # Returns: (int) Number of files copied/in target
  #----------------------------------------------------------------------------
  
  dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
  
  if (!is.null(folders)) {
    for (folder in folders) {
      source_folder <- file.path(source_path, folder)
      if (dir.exists(source_folder)) {
        file.copy(source_folder, target_path, recursive = TRUE, overwrite = TRUE)
      }
    }
  }
  
  if (!is.null(file_patterns)) {
    for (subfolder in names(file_patterns)) {
      if (subfolder == '.') {
        source_subfolder <- source_path
        target_subfolder <- target_path
      } else {
        source_subfolder <- file.path(source_path, subfolder)
        target_subfolder <- file.path(target_path, subfolder)
      }
      
      if (dir.exists(source_subfolder)) {
        dir.create(target_subfolder, recursive = TRUE, showWarnings = FALSE)
        for (pattern in file_patterns[[subfolder]]) {
          matching_files <- list.files(source_subfolder, pattern = pattern, full.names = TRUE)
          for (f in matching_files) {
            file.copy(f, file.path(target_subfolder, basename(f)), overwrite = TRUE)
          }
        }
      }
    }
  }
  
  if (!is.null(count_pattern)) {
    n_files <- length(list.files(target_path, recursive = TRUE, pattern = count_pattern))
  } else {
    n_files <- length(list.files(target_path, recursive = TRUE))
  }
  
  return(n_files)
}



copy_interface <- function(type, source_interface, target_interface) {
  
  #----------------------------------------------------------------------------
  # Data-driven interface copy dispatcher for nsece, acs, or calibration.
  #
  # Params:
  #   - type (chr): Interface type ('nsece', 'acs', or 'calibration')
  #   - source_interface (chr): Timestamp of source interface
  #   - target_interface (chr): Timestamp of target interface
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------
  
  configs <- list(
    nsece = list(
      folders = c('models', 'supply'),
      file_patterns = list(data = c('^nsece_.*\\.csv$')),
      count_pattern = NULL,
      label = 'NSECE estimation'
    ),
    acs = list(
      folders = NULL,
      file_patterns = list(data = c('^acs_.*\\.csv$')),
      count_pattern = 'acs_',
      label = 'ACS data'
    ),
    calibration = list(
      folders = NULL,
      file_patterns = list(
        data = c('^acs_.*\\.csv$', '^calibration_.*\\.csv$'),
        '.'  = c('^demand_params.*\\.yaml$', '^alpha_.*\\.rds$', '^calibration_details\\.yaml$')
      ),
      count_pattern = '(acs_|calibration_|demand_params|alpha_)',
      label = 'calibration'
    )
  )
  
  cfg <- configs[[type]]
  source_path <- file.path(default_paths$roots$output, source_interface, 'estimation')
  target_path <- file.path(default_paths$roots$output, target_interface, 'estimation')
  
  if (!dir.exists(source_path)) {
    stop('Source ', cfg$label, ' interface not found: ', source_path)
  }
  
  n_files <- copy_interface_files(source_path, target_path,
                                  folders = cfg$folders,
                                  file_patterns = cfg$file_patterns,
                                  count_pattern = cfg$count_pattern)
  cat('Copied', n_files, cfg$label, 'files from interface', source_interface, 'to', target_interface, '\n')
}

# Resolve interface hierarchy: calibration > acs > nsece
interfaces <- resolve_interface_hierarchy(nsece_interface, acs_interface, calibration_interface)

#-------------------------------------------
# 1a. NSECE Processing
#-------------------------------------------

if (is.null(interfaces$nsece)) {
  cat('No NSECE interface specified. Generating NSECE estimation data with interface', time_stamp, '\n')
  run_nsece_processing()
} else {
  cat('Copying NSECE data from interface', interfaces$nsece, '\n')
  copy_interface('nsece', interfaces$nsece, time_stamp)
}

#-------------------------------------------
# 1b. ACS Processing (demand-side)
#-------------------------------------------

if (is.null(interfaces$acs)) {
  cat('No ACS interface specified. Generating ACS data with interface', time_stamp, '\n')
  run_acs_processing()
  gc()
} else {
  cat('Copying ACS data from interface', interfaces$acs, '\n')
  copy_interface('acs', interfaces$acs, time_stamp)
}


#------------------------------------------------------------------------------
# SECTION 2: CALIBRATION
#------------------------------------------------------------------------------
#
# - Build calibration data (wide format with net_income, choice probabilities)
# - Run demand estimation (CRRA utility model calibration)
# - Write alpha matrices and demand parameters
#
# Calibration targets configured via --calib-target1-name/value and
# --calib-target2-name/value. Defaults: part_cost (-0.15), hours_income (-0.05)
#
#------------------------------------------------------------------------------

if (is.null(interfaces$calibration)) {
  cat('No calibration interface specified. Running calibration with interface', time_stamp, '\n')
  run_calibration()
  gc()
} else {
  cat('Copying calibration data from interface', interfaces$calibration, '\n')
  copy_interface('calibration', interfaces$calibration, time_stamp)
}

# Exit early if --setup-only flag was passed
if (setup_only) {
  cat('\n--setup-only flag set. Processing and calibration complete.\n')
  cat('Output:', output_root, '\n')
  cat('ECEC Simulator setup completed successfully!\n')
  quit(status = 0)
}


#------------------------------------------------------------------------------
# SECTION 3: SIMULATION
#------------------------------------------------------------------------------
#
# 3a. Initialize simulation context (load scenarios, microdata, parameters)
# 3b. Year-outer loop: age data, prepare parent units, run scenarios
#     3c. Per-scenario: solve equilibrium, expand with Gumbel shocks, discrete choices
#     3d. Per-scenario: accumulate results (allocation, fiscal, distributional, etc.)
#
#------------------------------------------------------------------------------

cat('\nECEC Simulator starting...\n')
cat('Runscript ID:', runscript_id, '\n')
cat('Interface:', time_stamp, '\n')
cat('Base Year:', acs_base_year, '\n')
cat('\n')

# Run simulation or save SLURM setup context
if (!is.null(slurm_phase) && slurm_phase == 'setup') {
  slurm_setup_simulation(slurm_scratch_dir)
} else {
  cat('=== ECEC Simulator ===\n')
  cat('Interface:', time_stamp, '\n\n')

  # 3a. Initialize simulation context
  sim_ctx <- initialize_simulation()

  # 3b-3d. Year-outer, scenario-inner loop
  for (year in sim_ctx$years_to_run) {
    sim_ctx <- run_simulation_year(sim_ctx, year)
  }

  # 4a. Write summary files, deltas, and Excel reports
  finalize_simulation(sim_ctx)

  cat('\nSimulation complete!\n')
}

cat('ECEC Simulator completed successfully!\n')
