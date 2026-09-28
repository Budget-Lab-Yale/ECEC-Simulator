#-----------------------------------------------------------------------------
# Do the committed outputs match what the code produces right now?
#
# The memo checks compare the MEMO to out/*.csv, so a stale out/ would pass
# them both. This rebuilds every geography in memory and compares it to the
# committed files. It writes nothing.
#
#   Rscript state_data/tests/check_outputs_are_current.R
#
# Exit status is 1 on any mismatch.
#-----------------------------------------------------------------------------
suppressPackageStartupMessages({library(readr); library(dplyr)})

source('state_data/code/00_main.R')     # main() is guarded; this only loads

OUT <- file.path(STATE_DATA_ROOT, 'out')
TOL <- 1e-6                        # relative; these should agree to the bit

cat('rebuilding every geography and comparing to out/\n\n')

fails <- 0
for (g in GEOGRAPHIES) {

  f <- file.path(OUT, paste0(g, '.csv'))
  if (!file.exists(f)) {
    cat('  [FAIL]', g, '- out/', basename(f), 'does not exist\n'); fails <- fails + 1; next
  }

  spec  <- load_spec(g)
  quiet <- utils::capture.output(res <- build_geo(spec))
  built <- setNames(res$annual_hours, res$ecec_type)

  have  <- read_csv(f, show_col_types = FALSE)
  onf   <- setNames(have$annual_hours, have$ecec_type)

  missing <- setdiff(ECEC_TYPES, names(onf))
  if (length(missing)) {
    cat('  [FAIL]', g, '- rows missing from the file:',
        paste(missing, collapse = ', '), '\n')
    fails <- fails + 1; next
  }

  d <- abs(onf[ECEC_TYPES] - built[ECEC_TYPES]) /
         pmax(abs(built[ECEC_TYPES]), 1)
  worst <- max(d)

  if (worst > TOL) {
    cat(sprintf('  [FAIL] %-4s worst row differs by %.4f%%\n', g, 100 * worst))
    for (t in ECEC_TYPES[d > TOL]) {
      cat(sprintf('           %-20s file %14.2f   code %14.2f\n',
                  t, onf[[t]], built[[t]]))
    }
    fails <- fails + 1
  } else {
    cat(sprintf('  [ok]   %-4s six rows agree to %.0e\n', g, TOL))
  }
}

## interval files ------------------------------------------------------------
# The loop above reads out/<GEO>.csv only, so the interval files get their own
# rebuild and comparison. They are a shipped output like any other.
cat('\nchecking the interval files\n\n')
for (g in GEOGRAPHIES) {
  f <- file.path(OUT, paste0(g, '_interval.csv'))
  if (!file.exists(f)) {
    cat('  [FAIL]', g, '- out/', basename(f), 'does not exist\n')
    fails <- fails + 1; next
  }
  spec <- load_spec(g)
  se <- spec$nonparental_rate_se
  # Same bound function as the build -- a bound the data reject is NA there,
  # must be NA here too.
  lo <- interval_bound(spec, spec$nonparental_rate - 1.96 * se)
  hi <- interval_bound(spec, spec$nonparental_rate + 1.96 * se)

  have <- read_csv(f, show_col_types = FALSE,
                   col_types = cols(ecec_type = 'c', .default = 'd'))
  rel <- function(file, code) {
    code <- unname(code[have$ecec_type])
    if (!identical(is.na(file), is.na(code))) return(Inf)   # NA in one only
    if (all(is.na(code))) return(0)
    max(abs(file - code) / pmax(abs(code), 1), na.rm = TRUE)
  }
  d <- max(rel(have$low, lo), rel(have$high, hi))
  if (d > TOL) {
    cat(sprintf('  [FAIL] %-4s interval bounds differ by %.4f%%\n', g, 100 * d))
    fails <- fails + 1
  } else if (anyNA(lo)) {
    cat(sprintf('  [ok]   %-4s high agrees to %.0e; no low bound -- below %.1f%%, programs exceed formal care\n',
                g, TOL, 100 * min_feasible_rate(spec)))
  } else {
    cat(sprintf('  [ok]   %-4s low and high agree to %.0e\n', g, TOL))
  }
}

## simulator copies ----------------------------------------------------------
# Simulator reads config/state/demand/<GEO>.csv, not out/. Each whole-state
# copy must equal out/<GEO>.csv, pass simulator's own reader. County-defined
# geography (NYC) must have no copy -- simulator would read it as a state.
cat('\nchecking the simulator copies in', SIMULATOR_DEMAND_DIR, '\n\n')
source('src/shared_functions/adjust_alpha_for_state.R')
for (g in GEOGRAPHIES) {
  spec  <- load_spec(g)
  sim_f <- file.path(SIMULATOR_DEMAND_DIR, paste0(g, '.csv'))
  if (!is.null(spec$counties)) {
    if (file.exists(sim_f)) {
      cat('  [FAIL]', g, '- county-defined, yet', sim_f, 'exists\n')
      fails <- fails + 1
    } else {
      cat(sprintf('  [--]   %-4s county-defined, no simulator copy\n', g))
    }
    next
  }
  if (!file.exists(sim_f)) {
    cat('  [FAIL]', g, '-', sim_f, 'does not exist\n'); fails <- fails + 1; next
  }
  same <- identical(readLines(sim_f), readLines(file.path(OUT, paste0(g, '.csv'))))
  read_ok <- tryCatch({ read_state_demand_targets(sim_f, g); TRUE },
                      error = function(e) { cat('  ', conditionMessage(e), '\n'); FALSE })
  if (!same || !read_ok) {
    cat(sprintf('  [FAIL] %-4s %s\n', g,
                if (!same) 'differs from out/' else 'simulator reader rejects it'))
    fails <- fails + 1
  } else {
    cat(sprintf('  [ok]   %-4s equals out/, simulator reader accepts it\n', g))
  }
}

## what this does NOT cover ------------------------------------------------
# out/ holds two more committed files. nsch_rate.csv is checked for presence
# here; 01_nsch_rate.R --check compares it to the survey. nsece_universe.csv
# cannot be rebuilt: it comes
# from licensed NSECE microdata held on the cluster, so there is nothing here
# to rebuild it from. It is the source of the NSECE constants in 00_main.R,
# and it is listed here so that the gap is visible rather than implied.
cat('\nchecking out/nsch_rate.csv\n\n')
rate_f <- file.path(OUT, 'nsch_rate.csv')
if (!file.exists(rate_f)) {
  cat('  [FAIL] out/nsch_rate.csv does not exist\n'); fails <- fails + 1
} else {
  cat('  [ok]   present; 01_nsch_rate.R --check compares it to the survey\n')
}
for (f in c('nsece_universe.csv')) {
  cat(sprintf('  [--]   %-20s cluster-derived, not rebuildable here\n', f))
}

if (fails == 0) {
  cat('\nout/ is current: every figure this machine can rebuild matches the code\n')
} else {
  cat('\n', fails, ' output check(s) FAILED. Re-run:\n', sep = '')
  cat('  Rscript state_data/code/00_main.R --all --check --interval\n')
  quit(status = 1)
}
