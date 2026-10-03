#-----------------------------------------------------------------------------
# Do the committed outputs match what the code produces right now?
#
# Other checks read committed outputs as given -- stale files would pass them.
# This rebuilds every geography in memory, compares to committed demand
# targets (config/state/demand/<GEO>.csv). Writes nothing.
#
#   Rscript state_data/tests/check_outputs_are_current.R
#
# Exit status is 1 on any mismatch.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({library(readr); library(dplyr)})

# main() is guarded; this only loads
source('state_data/code/00_build_demand_targets.R')
source('src/shared_functions/adjust_alpha_for_state.R')

OUT <- file.path(STATE_DATA_ROOT, 'out')
TOL <- 1e-6                        # relative; these should agree to the bit

# Simulator reads config/state/demand/<GEO>.csv, build's only copy of
# demand targets. Every file, NYC's incl., must equal rebuild, pass
# simulator's own reader.
cat('rebuilding every geography and comparing to', SIMULATOR_DEMAND_DIR, '\n\n')

fails <- 0
for (g in GEOGRAPHIES) {

  f <- file.path(SIMULATOR_DEMAND_DIR, paste0(g, '.csv'))
  if (!file.exists(f)) {
    cat('  [FAIL]', g, '-', f, 'does not exist\n'); fails <- fails + 1; next
  }

  quiet <- utils::capture.output(res <- build_geo(load_spec(g)))
  built <- setNames(res$annual_hours, res$ecec_type)
  have  <- read_csv(f, show_col_types = FALSE)
  onf   <- setNames(have$annual_hours, have$ecec_type)

  missing <- setdiff(ECEC_TYPES, names(onf))
  if (length(missing)) {
    cat('  [FAIL]', g, '- rows missing from the file:',
        paste(missing, collapse = ', '), '\n')
    fails <- fails + 1; next
  }

  d <- abs(onf[ECEC_TYPES] - built[ECEC_TYPES]) / pmax(abs(built[ECEC_TYPES]), 1)
  read_ok <- tryCatch({ read_state_demand_targets(f, g); TRUE },
                      error = function(e) { cat('  ', conditionMessage(e), '\n'); FALSE })

  if (max(d) > TOL) {
    cat(sprintf('  [FAIL] %-4s worst row differs by %.4f%%\n', g, 100 * max(d)))
    for (t in ECEC_TYPES[d > TOL]) {
      cat(sprintf('           %-20s file %14.2f   code %14.2f\n', t, onf[[t]], built[[t]]))
    }
    fails <- fails + 1
  } else if (!read_ok) {
    cat(sprintf('  [FAIL] %-4s simulator reader rejects it\n', g))
    fails <- fails + 1
  } else {
    cat(sprintf('  [ok]   %-4s six rows agree to %.0e, simulator reader accepts it\n',
                g, TOL))
  }
}

## what this does NOT cover ------------------------------------------------
# Two more committed files in out/. nsch_participation_rate_by_wave.csv:
# presence only; 01_nsch_participation_rate.R rebuilds it from NSCH files in
# raw/US/. nsece_constants.csv (source of NSECE constants in
# 00_build_demand_targets.R): licensed NSECE microdata on cluster, nothing
# here to rebuild from -- listed so gap is visible.
cat('\nchecking out/nsch_participation_rate_by_wave.csv\n\n')
rate_f <- file.path(OUT, 'nsch_participation_rate_by_wave.csv')
if (!file.exists(rate_f)) {
  cat('  [FAIL] out/nsch_participation_rate_by_wave.csv does not exist\n')
  fails <- fails + 1
} else {
  cat('  [ok]   present; 01_nsch_participation_rate.R rebuilds it from the',
      'NSCH files in raw/US/\n')
}
cat('  [--]   nsece_constants.csv   cluster-derived, not rebuildable here\n')

if (fails == 0) {
  cat('\ndemand targets and out/ are current: every figure this machine can',
      'rebuild matches the code\n')
} else {
  cat('\n', fails, ' output check(s) FAILED. Re-run:\n', sep = '')
  cat('  Rscript state_data/code/00_build_demand_targets.R --all --check\n')
  quit(status = 1)
}
