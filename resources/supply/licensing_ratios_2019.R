# Reviewed October 2019 rules; unresolved rows retain documented 2017 fallback.
# Run analyze_licensing_2019.py first to validate source hashes.
# Centre input: ordinary age bands (licensing_2019_center_bands.csv), month-
# weighted to one children-per-staff value per year of age 0-4; audited five
# age points check bands. NY centre = NYS, NYC rules blended by child share.
suppressPackageStartupMessages({library(readr); library(dplyr)})

BANDS <- file.path('resources', 'licensing', 'licensing_2019_center_bands.csv')

# Audited centre age point -> month of age checked against bands
POINT_MONTH <- c('11_months' = 11, '18_months' = 18, '35_months' = 35,
                 '3_years' = 36, '4_years' = 48)

substate_child_shares <- function(pop_file = file.path('resources', 'census_population',
                                    'US_PEP__population_children_age_0_4_by_county__2019.csv'),
                                  substate = file.path('config', 'substate_geographies.csv')) {

  # Share of parent state's children 0-4 (PEP 2019 county counts) living in each
  # sub-state geography (config/substate_geographies.csv counties) -- pure, no
  # prints. Weights parent centre staff per child: sub-state vs rest-of-state rules.
  # Params: pop_file, substate (chr). Returns: (dbl) named by sub-state geo, in (0, 1).
  pop <- read_csv(pop_file, col_types = cols(.default = col_character()))
  sg <- read_csv(substate, col_types = cols(.default = col_integer(), geo = col_character()))
  vapply(split(sg, sg$geo), function(g) {
    fips <- unique(g$statefip)
    in_state <- as.integer(pop$STATE) == fips
    inside <- in_state & as.integer(pop$COUNTY) %in% g$countyfip
    if (length(fips) != 1 || sum(inside) != nrow(g)) stop(pop_file, ': county rows missing for ', g$geo[1])
    share <- sum(as.numeric(pop$TOT_POP[inside])) / sum(as.numeric(pop$TOT_POP[in_state]))
    if (!is.finite(share) || share <= 0 || share >= 1) stop(pop_file, ': invalid share for ', g$geo[1])
    share
  }, numeric(1))
}



centre_age_years <- function(points, bands_path = BANDS) {

  # Month-weighted centre staffing, every geography: staff per child for age a
  # = mean over months 12a..12a+11 of 1 / band ratio. Bands must tile 0-60
  # months, agree w/ audited age points. NY blends NYS, NYC staff per child by
  # NYC share of NY children 0-4 (substate_child_shares()).
  # Params: points (df): reviewed centre point rows (state, age_group,
  #   children_per_staff); bands_path (chr). Returns: (df) reviewed centre rows,
  #   age_group age_0..age_4.
  b <- read_csv(bands_path, col_types = cols(.default = col_character())) %>%
    mutate(from_months = as.integer(from_months), to_months = as.integer(to_months),
           children_per_staff = as.numeric(children_per_staff)) %>%
    arrange(geo, from_months)
  bad <- b %>% group_by(geo) %>%
    dplyr::summarise(bad = first(from_months) != 0 || last(to_months) != 60 ||
                       any(head(to_months, -1) != tail(from_months, -1)), .groups = 'drop')
  if (!setequal(b$geo, points$state) || any(bad$bad) || any(b$status != 'verified') ||
      any(!is.finite(b$children_per_staff) | b$children_per_staff <= 0)) {
    stop(bands_path, ': bands must be verified, positive, tile 0-60 months for every geography.')
  }
  ratio_at <- function(g, m) b$children_per_staff[b$geo == g & b$from_months <= m & b$to_months > m]
  check <- mapply(ratio_at, points$state, POINT_MONTH[points$age_group])
  if (any(check != points$children_per_staff)) {
    stop(bands_path, ': band disagrees w/ audited age point for ',
         paste(unique(points$state[check != points$children_per_staff]), collapse = ', '))
  }
  ages <- 0:4
  s <- t(vapply(sort(unique(b$geo)), function(g) {
    vapply(ages, function(a) mean(1 / vapply(12 * a + 0:11, function(m) ratio_at(g, m), numeric(1))),
           numeric(1))
  }, numeric(length(ages))))
  nyc_share <- substate_child_shares()[['NYC']]
  s['NY', ] <- (1 - nyc_share) * s['NY', ] + nyc_share * s['NYC', ]
  src <- b %>% group_by(geo) %>%
    dplyr::summarise(source = paste(unique(source_id), collapse = ';'),
                     locator = paste(unique(locator), collapse = '; '), .groups = 'drop')
  tibble::tibble(state = rep(rownames(s), each = length(ages)),
                 setting = 'center', age_group = paste0('age_', rep(ages, nrow(s))),
                 children_per_staff = 1 / as.vector(t(s))) %>%
    left_join(src, by = c('state' = 'geo')) %>%
    mutate(source = ifelse(state == 'NY', paste0(source, ';', src$source[src$geo == 'NYC']), source),
           status = 'month_weighted_bands', snapshot_date = '2019-10-01',
           effective_from = NA_character_, measure = 'children_per_staff', fallback = FALSE,
           fallback_reason = NA_character_, permitted_values = NA_character_,
           selection_basis = ifelse(state == 'NY',
             sprintf('Month-weighted ordinary bands; NYS, NYC staff per child blended, NYC share %.4f.', nyc_share),
             'Month-weighted ordinary bands.'),
           notes = NA_character_)
}



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
  # Audited centre points check bands; month-weighted age years replace them
  out <- bind_rows(filter(out, setting == 'home'),
                   centre_age_years(filter(out, setting == 'center')))
  write_csv(out, output_path)
  cat('Reviewed staffing: ', sum(reviewed), ' adopted records; ', sum(!reviewed),
      ' explicit fallback records\n', sep = '')
  invisible(out)
}

if (sys.nframe() == 0) build_reviewed_staffing()
