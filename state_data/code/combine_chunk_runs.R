#------------------------------------------------------------------------------
# combine_chunk_runs.R -- one 52-geography result set from a state run split
# into chunk runs (one main.R run per group of geographies -- full sample of
# 52 geographies exceeds one day-partition job).
#
# Per chunk dir (copied from cluster): alpha_drift_state.csv,
# baseline_{children,employment,care_shift}_state.csv,
# integrated_validation_<chunk>{,_employment}.csv. Per-geo rows stacked; drift
# summary recomputed as alpha_drift.R (50 states + DC, NYC left out),
# validation summary as integrated_validation.R. Every geography exactly once.
#
# Usage: Rscript state_data/code/combine_chunk_runs.R <out dir> <chunk dir>...
#   writes <out dir>/alpha_drift_state{,_summary}.csv,
#   baseline_*_state.csv, integrated_validation_current{,_employment,_summary}.csv,
#   combine_chunk_runs_provenance.txt
#------------------------------------------------------------------------------

suppressPackageStartupMessages({library(dplyr); library(readr)})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop('Usage: combine_chunk_runs.R <out dir> <chunk dir>...')
out_dir <- args[1]
chunks <- args[-1]
N_GEOS <- 52



stack <- function(pattern) {

  #----------------------------------------------------------------------------
  # Rows of one file type across chunk dirs; stops if a chunk lacks it.
  #
  # Params:
  #   - pattern (chr): file name regex within a chunk dir
  #
  # Returns: (tibble) stacked rows
  #----------------------------------------------------------------------------

  bind_rows(lapply(chunks, function(d) {
    f <- list.files(d, pattern = pattern, full.names = TRUE)
    if (length(f) != 1) stop(d, ': want one file matching ', pattern, ', found ', length(f))
    d <- read_csv(f, show_col_types = FALSE)
    if ('run' %in% names(d)) d$run <- as.character(d$run)
    d
  }))
}



one_per_geo <- function(d, key, what) {

  #----------------------------------------------------------------------------
  # Stops unless every geography appears exactly once per key.
  #
  # Params:
  #   - d (df); key (chr vec): columns w/ geo first; what (chr): label
  #
  # Returns: d
  #----------------------------------------------------------------------------

  if (anyDuplicated(d[key])) stop(what, ': geography in more than one chunk')
  if (n_distinct(d[[key[1]]]) != N_GEOS) stop(what, ': ', n_distinct(d[[key[1]]]), ' geographies, want ', N_GEOS)
  d
}



drift <- one_per_geo(stack('^alpha_drift_state\\.csv$'), 'geo', 'alpha drift')
summ <- drift %>%
  filter(geo != 'NYC') %>%
  summarise(alpha_price = first(alpha_price), run = paste(sort(unique(run)), collapse = ';'),
            n_geos = n(),
            across(c(pp_change_smooth, pp_change_drawn),
                   list(mean_equal = mean, mean_abs_equal = ~ mean(abs(.x)),
                        min = min, max = max,
                        mean_pop = ~ weighted.mean(.x, children_2019),
                        mean_abs_pop = ~ weighted.mean(abs(.x), children_2019))),
            geo_min_smooth = geo[which.min(pp_change_smooth)],
            geo_max_smooth = geo[which.max(pp_change_smooth)],
            max_qd_rel_gap = max(qd_rel_gap),
            all_qd_check_pass = all(qd_check_pass))

baseline <- lapply(c('children', 'employment', 'care_shift'), function(x) {
  stack(paste0('^baseline_', x, '_state\\.csv$'))
})
names(baseline) <- c('children', 'employment', 'care_shift')

val <- one_per_geo(stack('^integrated_validation_c[0-9]+\\.csv$'), c('state', 'year'), 'validation')
emp <- stack('^integrated_validation_c[0-9]+_employment\\.csv$')
val_summ <- tibble(geographies = n_distinct(val$state), solver_records = nrow(val),
                   failed_records = sum(!val$converged | val$code != 0),
                   max_price_gap = max(val$max_gap), employment_rows = nrow(emp),
                   employment_max_abs_gap = max(abs(emp$rate_gap), na.rm = TRUE))

write_csv(drift, file.path(out_dir, 'alpha_drift_state.csv'))
write_csv(summ, file.path(out_dir, 'alpha_drift_state_summary.csv'))
for (x in names(baseline)) write_csv(baseline[[x]], file.path(out_dir, paste0('baseline_', x, '_state.csv')))
write_csv(val, file.path(out_dir, 'integrated_validation_current.csv'))
write_csv(emp, file.path(out_dir, 'integrated_validation_current_employment.csv'))
write_csv(val_summ, file.path(out_dir, 'integrated_validation_current_summary.csv'))
writeLines(c(paste('combine_chunk_runs.R run at', format(Sys.time())),
             paste('chunks:', paste(chunks, collapse = ' ')),
             paste('runs:', summ$run)),
           file.path(out_dir, 'combine_chunk_runs_provenance.txt'))

print(as.data.frame(val_summ))
print(t(as.data.frame(summ)))
