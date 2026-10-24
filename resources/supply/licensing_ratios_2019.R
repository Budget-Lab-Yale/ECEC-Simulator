# Reviewed October 2019 rules; unresolved rows retain documented 2017 fallback.
# Run analyze_licensing_2019.py first to validate source hashes.
suppressPackageStartupMessages({library(readr); library(dplyr)})

build_reviewed_staffing <- function(path = file.path('state_data', 'out', 'licensing_2019_audit.csv'),
                                     output_path = file.path('resources', 'supply',
                                                             'licensing_ratios_reviewed_2019.csv')) {

  # Params: path, output_path (chr). Returns: (df) reviewed inputs with provenance.
  audit <- read_csv(path, col_types = cols(.default = col_character()))
  keys <- c('state', 'setting', 'age_group')
  if (nrow(audit) != 312 || anyDuplicated(audit[keys])) {
    stop('Staffing audit must contain 312 unique records. Rebuild audit before supply inputs.')
  }
  source('src/shared_functions/constants.R', local = TRUE)
  source('state_data/code/shared_nsece_ages.R', local = TRUE)
  expected <- expand.grid(state = c(names(STATE_POSTAL_TO_FIPS), names(SUBSTATE_GEOGRAPHIES)),
                          age_group = unname(NSECE_AGE_POINT), stringsAsFactors = FALSE)
  expected$setting <- 'center'
  expected <- bind_rows(expected, data.frame(state = unique(expected$state),
                         age_group = 'all_under_school_age', setting = 'home'))
  if (anyNA(audit[keys]) || nrow(anti_join(expected, audit, by = keys)) ||
      anyNA(audit$snapshot_date) || any(audit$snapshot_date != '2019-10-01')) {
    stop('Staffing audit has invalid geography, age, setting, or snapshot coverage.')
  }
  reviewed <- audit$status %in% c('verified_archive', 'verified_history', 'verified_conditional')
  values <- suppressWarnings(as.numeric(audit$verified_value))
  if (any(reviewed & (!is.finite(values) | values <= 0))) {
    stop('Reviewed staffing row lacks positive verified value. Review evidence before adoption.')
  }
  dates <- suppressWarnings(as.Date(audit$effective_from, format = '%Y-%m-%d'))
  invalid_date <- !is.na(audit$effective_from) &
    (is.na(dates) | format(dates, '%Y-%m-%d') != audit$effective_from)
  future <- !is.na(dates) & dates > as.Date('2019-10-01')
  missing_history <- (audit$status == 'verified_archive' & is.na(dates)) |
    (audit$status %in% c('verified_history', 'verified_conditional') &
       is.na(audit$verification_basis))
  if (any(reviewed & (future | missing_history | invalid_date))) {
    stop('Reviewed staffing rule lacks historical verification covering October 2019.')
  }
  for (i in which(audit$status == 'verified_conditional')) {
    permitted <- if (is.na(audit$permitted_values[i])) numeric(0) else
      suppressWarnings(as.numeric(strsplit(audit$permitted_values[i], ';')[[1]]))
    if (is.na(audit$selection_basis[i]) || !values[i] %in% permitted) {
      stop('Conditional staffing row lacks permitted value or selection basis.')
    }
  }
  if (anyNA(audit$candidate_measure[reviewed]) ||
      any(audit$candidate_measure[reviewed & audit$setting == 'center'] != 'children_per_staff') ||
      any(!grepl('capacity', audit$candidate_measure[reviewed & audit$setting == 'home']))) {
    stop('Staffing measure must distinguish center ratios from home capacity proxies.')
  }
  out <- audit %>% transmute(
    state, setting, age_group,
    children_per_staff = ifelse(reviewed, values, as.numeric(baseline_value)),
    source = ifelse(reviewed, source_id, baseline_source),
    status, snapshot_date, effective_from,
    measure = ifelse(reviewed, candidate_measure, baseline_measure),
    fallback = !reviewed, fallback_reason, permitted_values, selection_basis,
    locator, notes)
  if (any(!is.finite(out$children_per_staff[out$setting == 'center'])) ||
      any(out$children_per_staff <= 0, na.rm = TRUE)) {
    stop('Staffing inputs must be positive; only unlicensed home fallbacks may be missing.')
  }
  write_csv(out, output_path)
  cat('Reviewed staffing: ', sum(reviewed), ' adopted records; ', sum(!reviewed),
      ' explicit fallback records\n', sep = '')
  invisible(out)
}

if (sys.nframe() == 0) build_reviewed_staffing()
