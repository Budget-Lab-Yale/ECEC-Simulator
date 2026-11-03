#------------------------------------------------------------------------------
# analyze_state_fit_shifts.R -- 2019 baseline-fit shifts (memo 3.4) per
# geography, parsed from state run logs. Diagnostic only, no input writes.
#
# adjust_alpha_for_state() prints, per geography: iterations to converge, one
# delta per care-type group, one employment shift per caregiver group. Run
# logs (main.R stdout) only record them -- this script saves them as a table.
#
# Summary: per shift, min, median, max over 50 states + DC (NYC row kept in
# per-geo table, left out of summary -- overlaps NY), plus iteration range.
# A geography appearing in two logs must carry identical values, else stop.
#
# Usage: Rscript state_data/code/analyze_state_fit_shifts.R <out prefix> <log>...
#   writes <out prefix>.csv (per geo) + <out prefix>_summary.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr)})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop('Usage: analyze_state_fit_shifts.R <out prefix> <log>...')
out_prefix <- args[1]
logs <- args[-1]
miss <- logs[!file.exists(logs)]
if (length(miss)) stop('Missing log(s): ', paste(miss, collapse = ', '))



parse_fit_log <- function(path) {
  #----------------------------------------------------------------------------
  # Fit lines of one run log -> long table.
  #
  # Params:
  #   - path (chr): main.R stdout log
  #
  # Returns: (tibble) geo, kind (care|employment), name, shift, iterations
  #----------------------------------------------------------------------------
  lines <- readLines(path, warn = FALSE)
  rows <- list(); geo <- NA_character_; iters <- NA_integer_
  for (ln in lines) {
    m <- regmatches(ln, regexec('State demand adjustment \\(([A-Z]+)\\): converged in ([0-9]+) iterations', ln))[[1]]
    if (length(m)) { geo <- m[2]; iters <- as.integer(m[3]); next }
    if (is.na(geo)) next
    m <- regmatches(ln, regexec('^ +employment ([FM]_[a-z0-9_]+) +shift=([-+0-9.]+)', ln))[[1]]
    if (length(m)) {
      rows[[length(rows) + 1]] <- tibble(geo = geo, kind = 'employment', name = m[2],
                                         shift = as.numeric(m[3]), iterations = iters)
      next
    }
    m <- regmatches(ln, regexec('^ +([A-Za-z -]+?) +delta=([-+0-9.]+)', ln))[[1]]
    if (length(m)) {
      rows[[length(rows) + 1]] <- tibble(geo = geo, kind = 'care', name = m[2],
                                         shift = as.numeric(m[3]), iterations = iters)
    }
  }
  bind_rows(rows)
}



fit <- bind_rows(lapply(logs, parse_fit_log)) %>% distinct()
dup <- fit %>% count(geo, kind, name) %>% filter(n > 1)
if (nrow(dup)) stop('Geography fit differs across logs: ', paste(unique(dup$geo), collapse = ', '))
n_geo <- n_distinct(fit$geo)
cat('Parsed', n_geo, 'geographies from', length(logs), 'log(s)\n')
if (n_geo != 52) warning('Expected 52 geographies, found ', n_geo)

summary <- fit %>%
  filter(geo != 'NYC') %>%
  group_by(kind, name) %>%
  summarise(n_geos = n(), min = min(shift), median = median(shift), max = max(shift),
            .groups = 'drop')
iters <- fit %>% distinct(geo, iterations) %>% filter(geo != 'NYC')
summary <- bind_rows(summary, tibble(kind = 'iterations', name = 'all', n_geos = nrow(iters),
                                     min = min(iters$iterations), median = median(iters$iterations),
                                     max = max(iters$iterations)))

write_csv(fit, paste0(out_prefix, '.csv'))
write_csv(summary, paste0(out_prefix, '_summary.csv'))
print(as.data.frame(summary), digits = 3)
cat('wrote', paste0(out_prefix, '.csv'), 'and', paste0(out_prefix, '_summary.csv'), '\n')
