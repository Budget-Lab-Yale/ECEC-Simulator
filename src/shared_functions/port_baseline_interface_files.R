#------------------------------------------------------------------------------
# port_baseline_interface_files.R
#
# Copies a source run's baseline outputs for one year into the current run's
# baseline folder when the baseline is reused via --baseline-interface (-b).
#
# Porting the full set (record-level parent units, solver diagnostics, supply
# and employment-targeting files) makes the new interface self-contained:
# record-level analyses and report figures work without the source run, and
# the new interface can itself serve as a -b source later.
#
# Called from: 3b (run_simulation_year), slurm.R (run_year_standalone)
#------------------------------------------------------------------------------



port_baseline_interface_files <- function(baseline_interface, year,
                                          dest_output_dir) {

  #----------------------------------------------------------------------------
  # Copies the source run's baseline outputs for one year into the current
  # run's baseline output folder.
  #
  # Params:
  #   - baseline_interface (chr): Timestamp of the source run
  #   - year (int): Simulation year whose files to copy
  #   - dest_output_dir (chr): Current run's baseline output folder
  #       (scenario_info$paths$output for the baseline scenario)
  #
  # Returns: nothing (side effects only)
  #----------------------------------------------------------------------------

  src_baseline_dir <- file.path(default_paths$roots$output, baseline_interface,
                                'simulation', 'baseline')

  files_to_port <- c(
    file.path('supply', paste0('supply_', year, '.yaml')),
    file.path('models', 'equilibrium',
              paste0('employment_targeting_', year, '.csv')),
    file.path('models', 'equilibrium',
              paste0('solver_results_', year, '.txt')),
    file.path('data', paste0('parent_units_c1_', year, '.csv')),
    file.path('data', paste0('parent_units_c2plus_', year, '.csv')),
    file.path('data', paste0('parent_units_', year, '.rds'))
  )

  for (rel in files_to_port) {
    src_file <- file.path(src_baseline_dir, rel)
    if (file.exists(src_file)) {
      dest_file <- file.path(dest_output_dir, rel)
      dir.create(dirname(dest_file), showWarnings = FALSE, recursive = TRUE)
      file.copy(src_file, dest_file, overwrite = TRUE)
    }
  }
}
