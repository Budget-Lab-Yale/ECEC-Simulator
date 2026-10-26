#------------------------------------------------------------------------------
# integrated_validation.R -- state run validation summary, every geography-year:
# solver convergence, max paid-sector price gap ($/child-hour), employment
# target misses. Reads run's own outputs only (solver_results_<yr>.txt,
# employment_targeting_<yr>.csv under simulation/baseline/states/<GEO>/).
#
# Writes <prefix>.csv (state, year, converged, code, max_gap),
# <prefix>_employment.csv (targeting rows + state), <prefix>_summary.csv,
# <prefix>.provenance.txt (run root, md5 of state supply YAMLs read by run).
#
# Usage, from repo root on Bouchet:
#   Rscript state_data/cluster/integrated_validation.R <run root> <out prefix>
#   <run root> = shared output root/<run timestamp>
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr)})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop('Usage: integrated_validation.R <run root> <out prefix>')
run_root <- args[1]
prefix   <- args[2]
states_dir <- file.path(run_root, 'simulation', 'baseline', 'states')
if (!dir.exists(states_dir)) stop('No state outputs under ', states_dir, '. Check run timestamp.')



read_solver <- function(path) {

  #----------------------------------------------------------------------------
  # Converged flag, optimizer code, max paid-sector |delta_gap| from one
  # solver_results_<yr>.txt.
  #
  # Params:
  #   - path (chr): solver results file
  #
  # Returns: (list) converged (lgl), code (int), max_gap (dbl, $/child-hour)
  #----------------------------------------------------------------------------

  x <- readLines(path)
  pick <- function(pattern) sub(pattern, '\\1', grep(pattern, x, value = TRUE)[1])
  list(converged = as.logical(trimws(pick('^Converged: *(\\S+).*$'))),
       code      = as.integer(trimws(pick('^Convergence code: *(\\S+).*$'))),
       max_gap   = as.numeric(pick('^Max \\|delta_gap\\| \\(paid sectors\\): \\$([0-9.eE+-]+)/hour.*$')))
}



geos <- sort(list.dirs(states_dir, recursive = FALSE, full.names = FALSE))
cat('Validating', length(geos), 'geographies in', run_root, '\n')
solver <- bind_rows(lapply(geos, function(g) {
  files <- list.files(file.path(states_dir, g, 'models', 'equilibrium'),
                      pattern = '^solver_results_[0-9]{4}\\.txt$', full.names = TRUE)
  bind_rows(lapply(files, function(f) {
    s <- read_solver(f)
    tibble::tibble(state = g, year = as.integer(sub('.*_([0-9]{4})\\.txt$', '\\1', f)),
                   converged = s$converged, code = s$code, max_gap = s$max_gap)
  }))
}))
employment <- bind_rows(lapply(geos, function(g) {
  files <- list.files(file.path(states_dir, g, 'models', 'equilibrium'),
                      pattern = '^employment_targeting_[0-9]{4}\\.csv$', full.names = TRUE)
  bind_rows(lapply(files, read_csv, show_col_types = FALSE)) %>% mutate(state = g)
}))

failed <- solver %>% filter(is.na(converged) | !converged | is.na(max_gap) | max_gap > 0.01)
summary <- tibble::tibble(
  run_root = run_root, geographies = length(geos), solver_records = nrow(solver),
  failed_records = nrow(failed), max_price_gap = max(solver$max_gap),
  employment_rows = nrow(employment),
  employment_max_abs_gap = max(abs(employment$rate_gap), na.rm = TRUE))

write_csv(solver, paste0(prefix, '.csv'))
write_csv(employment, paste0(prefix, '_employment.csv'))
write_csv(summary, paste0(prefix, '_summary.csv'))
yamls <- list.files(file.path('config', 'state', 'supply'), pattern = '\\.yaml$', full.names = TRUE)
writeLines(c(paste('run_root:', run_root), paste('date:', format(Sys.time())),
             paste(yamls, unname(tools::md5sum(yamls)), sep = '\t')),
           paste0(prefix, '.provenance.txt'))
print(as.data.frame(summary), row.names = FALSE)
if (nrow(failed)) print(as.data.frame(failed), row.names = FALSE)
