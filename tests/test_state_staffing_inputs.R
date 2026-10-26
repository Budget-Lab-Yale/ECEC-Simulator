# Independently reconcile reviewed staffing, national normalization, saved YAMLs.
suppressPackageStartupMessages({library(dplyr); library(readr); library(yaml)})
source('resources/supply/state_supply_params.R')
source('resources/supply/licensing_ratios_2019.R')

audit <- read_csv('state_data/out/licensing_2019_audit.csv', show_col_types = FALSE)
rules <- read_csv(RATIOS, show_col_types = FALSE)
keys <- c('state', 'setting', 'age_group')
joined <- inner_join(audit, rules, by = keys)
stopifnot(nrow(audit) == 312, nrow(joined) == 52, all(joined$setting == 'home'),
          !anyDuplicated(rules[keys]), nrow(rules) == 52 * 6,
          all(!joined$fallback), all(joined$children_per_staff == joined$verified_value))

# Centre rows: independent month-weighting of ordinary bands; audited points sit
# inside bands; NY = NYS, NYC staff per child blended by NYC share of children 0-4
population <- read_csv(POP_0_4, col_types = cols(.default = col_character()))
state_pop <- tapply(as.numeric(population$TOT_POP), as.integer(population$STATE), sum)
states <- names(STATE_POSTAL_TO_FIPS)
geos <- c(states, names(SUBSTATE_GEOGRAPHIES))
pop <- setNames(as.numeric(state_pop[as.character(STATE_POSTAL_TO_FIPS)]), states)
bands <- read_csv(BANDS, show_col_types = FALSE)
monthly <- bands[rep(seq_len(nrow(bands)), bands$to_months - bands$from_months), ]
monthly$month <- unlist(Map(seq, bands$from_months, bands$to_months - 1))
stopifnot(!anyDuplicated(monthly[c('geo', 'month')]), nrow(monthly) == 52 * 60)
points <- audit[audit$setting == 'center', ]
at_point <- inner_join(points %>% mutate(month = POINT_MONTH[age_group]),
                       monthly, by = c('state' = 'geo', 'month'))
stopifnot(nrow(at_point) == 260,
          all(at_point$verified_value == at_point$children_per_staff))
staff <- tapply(1 / monthly$children_per_staff,
                list(monthly$geo, paste0('age_', monthly$month %/% 12)), mean)
counties <- read_csv('config/substate_geographies.csv', show_col_types = FALSE)
in_ny <- as.integer(population$STATE) == 36
nyc_share <- sum(as.numeric(population$TOT_POP[in_ny & as.integer(population$COUNTY) %in%
                                                 counties$countyfip[counties$geo == 'NYC']])) /
  sum(as.numeric(population$TOT_POP[in_ny]))
stopifnot(nyc_share > 0.4, nyc_share < 0.5)
staff['NY', ] <- (1 - nyc_share) * staff['NY', ] + nyc_share * staff['NYC', ]
centre_rows <- rules[rules$setting == 'center', ]
stopifnot(max(abs(1 / centre_rows$children_per_staff -
                  staff[cbind(centre_rows$state, centre_rows$age_group)])) < 1e-12)
center_raw <- setNames(vapply(geos, function(st) {
  sum(NSECE_CENTRE_ENROLLED * staff[st, NSECE_AGES_0_4]) / sum(NSECE_CENTRE_ENROLLED)
}, numeric(1)), geos)
home_raw <- setNames(vapply(geos, function(st) {
  1 / rules$children_per_staff[rules$state == st & rules$setting == 'home']
}, numeric(1)), geos)
center <- center_raw / weighted.mean(center_raw[states], pop)
home_lic <- home_raw / weighted.mean(home_raw[states], pop)
# Licensed share of paid home children, recomputed from saved NSECE weighted sums
share <- read_csv(file.path('resources', 'supply', 'US_NSECE__home_licensed_share__2019.csv'),
                  show_col_types = FALSE)
share <- share[share$measure == 'children_0_5', ]
s <- share$licensed_children / (share$licensed_children + share$unlicensed_children)
stopifnot(nrow(share) == 1, abs(s - share$licensed_share) < 1e-12, s > 0.2, s < 0.3)
home <- s * home_lic + (1 - s)
stopifnot(abs(weighted.mean(center[states], pop) - 1) < 1e-12,
          abs(weighted.mean(home[states], pop) - 1) < 1e-12)
national <- read_yaml(NATIONAL)
for (st in geos) {
  saved <- read_yaml(file.path(OUT_DIR, paste0(st, '.yaml')))
  stopifnot(is.null(saved$labor_supply), identical(saved$elasticities, national$elasticities))
  for (sector in names(national$labor_requirements)) {
    scale <- if (sector %in% CENTRE_SECTORS) center[st] else home[st]
    want <- unlist(national$labor_requirements[[sector]]) * scale
    stopifnot(max(abs(unlist(saved$labor_requirements[[sector]]) - want)) < 1e-12)
  }
}
cat('PASS: 312 reviewed records, month-weighted bands, NY blend, independent population normalization, all 52 YAML staffing payloads\n')

# Rejected audit must never overwrite existing derived inputs.
input <- tempfile(fileext = '.csv')
output <- tempfile(fileext = '.csv')
writeLines('preserve', output)
original <- read_csv('state_data/out/licensing_2019_audit.csv',
                      col_types = cols(.default = col_character()))
reject <- function(x) {
  write_csv(x, input)
  err <- tryCatch({ build_reviewed_staffing(input, output); NULL }, error = identity)
  stopifnot(inherits(err, 'error'), identical(readLines(output), 'preserve'))
}
x <- original; x$state[1] <- 'XX'; reject(x)
x <- original; x$age_group[1] <- '12_months'; reject(x)
x <- original; x$snapshot_date[1] <- '2020-10-01'; reject(x)
x <- original; x$effective_from[1] <- '2020-01-01'; reject(x)
x <- original; x$effective_from[1] <- 'invalid'; reject(x)
x <- original; x$verified_value[1] <- '0'; reject(x)
x <- original; x$candidate_measure[1] <- 'home_capacity_proxy'; reject(x)
i <- which(original$status == 'verified_conditional')[1]
x <- original; x$selection_basis[i] <- NA_character_; reject(x)
x <- original; x$permitted_values[i] <- '999'; reject(x)
# Audited point outside its band -> reject (bands, points must agree)
j <- which(original$setting == 'center' & original$age_group == '4_years')[1]
x <- original; x$verified_value[j] <- '99'; reject(x)
build_reviewed_staffing(output_path = output)
rebuilt <- read_csv(output, show_col_types = FALSE)
stopifnot(isTRUE(all.equal(rules, rebuilt, check.attributes = FALSE)))
unlink(c(input, output))
cat('PASS: invalid coverage, dates, measures, selections rejected before write; reviewed CSV reproducible\n')
