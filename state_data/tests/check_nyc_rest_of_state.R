# NY/NYC structural checks plus cross-calibration sensitivity, annual hours.
# Default: fail invalid partitions, stale controls, inconsistent decomposition.
# --strict-additivity: also apply historical 1% / 0.1%-of-total thresholds.
suppressPackageStartupMessages(library(dplyr))
source('state_data/code/00_build_demand_targets.R')

MAX_GAP <- 0.01
MAX_GAP_TOTAL <- 0.001
NYC_PARTITION_FILE <- file.path('resources', 'acs', 'US_ACS__p0_children_nyc_partition__2019.csv')



validate_nyc_partition <- function(partition, reference = NULL) {

  # Same-rank national p0 must partition; own NYC p0 stays independent.
  # Params: partition, reference (df). Returns: TRUE invisibly, stops on mismatch.
  fields <- c('geo', 'ecec_type', 'hours_choice', 'children')
  if (!all(fields %in% names(partition)) || anyNA(partition[fields]) ||
      anyDuplicated(partition[c('geo', 'ecec_type', 'hours_choice')]) ||
      !setequal(partition$geo, c('NY', 'NYC', 'NYC_parent', 'NYR')) ||
      !is.numeric(partition$children) || any(!is.finite(partition$children) | partition$children < 0)) {
    stop('Invalid NYC partition cells or geography coverage. Rebuild partition controls.')
  }
  key <- function(x) paste(x$ecec_type, x$hours_choice)
  base <- partition[partition$geo == 'NY', ]
  values <- function(d, geo) {
    x <- d[d$geo == geo, ]
    if (!setequal(key(x), key(base))) stop('NYC partition care/hour keys differ: ', geo)
    x$children[match(key(base), key(x))]
  }
  close <- function(x, y) all(abs(x - y) <= 1e-6 + 1e-10 * pmax(abs(x), abs(y)))
  if (!close(values(partition, 'NY'), values(partition, 'NYC_parent') + values(partition, 'NYR'))) {
    stop('NYC parent-rank controls do not partition NY. Never subtract own-rank NYC p0 from NY.')
  }
  if (!close(sum(values(partition, 'NYC')), sum(values(partition, 'NYC_parent')))) {
    stop('NYC own/parent controls cover different weighted child populations.')
  }
  if (!is.null(reference)) {
    for (geo in c('NY', 'NYC')) {
      if (!close(values(partition, geo), values(reference, geo))) {
        stop('NYC partition controls differ from production p0 for ', geo, '. Refresh both from one interface.')
      }
    }
  }
  invisible(TRUE)
}



nyc_rest_of_state_diagnostic <- function(partition_path = NYC_PARTITION_FILE,
                                        write_output = TRUE) {

  # Returns: (df) own-calibration gap, same-rank aggregation, NYC rank effect.
  if (!file.exists(partition_path)) {
    stop('Missing NYC partition controls: ', partition_path,
         '. Run acs_p0_shares.R and copy its .nyc_partition.csv sidecar here.')
  }
  partition <- readr::read_csv(partition_path, show_col_types = FALSE)
  original_p0 <- p0_rows
  reference <- bind_rows(original_p0('NY'), original_p0('NYC')) %>%
    mutate(children = num(children))
  validate_nyc_partition(partition, reference)

  # Override diagnostic geographies only; restore resolver on every exit.
  build_env <- environment(build_geo)
  assign('p0_rows', function(geo, types = NULL, hint = '') {
    if (!geo %in% c('NYR', 'NYC_parent')) return(original_p0(geo, types, hint))
    x <- partition[partition$geo == geo, ]
    if (!is.null(types)) x <- x[x$ecec_type %in% types, ]
    x
  }, envir = build_env)
  on.exit(assign('p0_rows', original_p0, envir = build_env), add = TRUE)

  ny <- load_spec('NY'); nyc <- load_spec('NYC')
  pep <- read_chr(raw_path('US_PEP__population_children_age_0_4_by_county__2019.csv'))
  ny_counties <- paste0(pep$STATE, pep$COUNTY)[num(pep$STATE) == ny$fips]
  rest <- ny
  rest$geo <- 'NYR'; rest$name <- 'New York outside NYC'
  rest$county_fips <- setdiff(ny_counties, nyc$county_fips)
  rest$programs$n_children <- ny$programs$n_children - nyc$programs$n_children
  rest$prek_all_settings <- ny$prek_all_settings - nyc$prek_all_settings
  rest$age5_share <- AGE5_NHES_NATIONAL
  city_parent <- nyc
  city_parent$geo <- 'NYC_parent'; city_parent$name <- 'NYC parent-rank diagnostic control'
  city_parent$age5_share <- c(prek = 0, head_start = 0)

  quiet <- function(spec) { utils::capture.output(x <- build_geo(spec)); x }
  n <- quiet(ny); c <- quiet(nyc); r <- quiet(rest); cp <- quiet(city_parent)
  d <- tibble::tibble(
    ecec_type = n$care$ecec_type,
    ny_less_nyc = n$care$annual_hours - c$care$annual_hours,
    rest_of_state = r$care$annual_hours,
    children_gap = n$care$children - c$care$children - r$care$children,
    children_hours_effect = children_gap * r$hours$annual_per_child,
    part_time_hours_effect = n$care$children * (n$hours$annual_per_child - r$hours$annual_per_child) -
      c$care$children * (c$hours$annual_per_child - r$hours$annual_per_child),
    same_rank_aggregation_hours = n$care$annual_hours - cp$care$annual_hours - r$care$annual_hours,
    city_rank_effect_hours = cp$care$annual_hours - c$care$annual_hours,
    pt_ny = unname(n$shares$part_time), pt_city_own = unname(c$shares$part_time),
    pt_city_parent = unname(cp$shares$part_time), pt_rest = unname(r$shares$part_time)) %>%
    mutate(gap = ny_less_nyc / rest_of_state - 1,
      gap_total = (ny_less_nyc - rest_of_state) / sum(rest_of_state),
      decomposition_residual = ny_less_nyc - rest_of_state - children_hours_effect - part_time_hours_effect,
      rank_decomposition_residual = ny_less_nyc - rest_of_state -
        same_rank_aggregation_hours - city_rank_effect_hours,
      material_gap = abs(gap) >= MAX_GAP & abs(gap_total) >= MAX_GAP_TOTAL)
  stopifnot(all(abs(d$decomposition_residual) < 1e-5),
            all(abs(d$rank_decomposition_residual) < 1e-5))

  # Program counts additive; p0 mix uses different geographic weights.
  i <- match('Unpaid Center-Based', d$ecec_type)
  p0 <- partition %>% filter(ecec_type == 'Unpaid Center-Based') %>%
    group_by(geo) %>% summarise(n = sum(children), .groups = 'drop')
  p0 <- setNames(p0$n, p0$geo)
  a <- p0['NYC_parent'] / p0['NY']
  b <- cp$care$children[i] / n$care$children[i]
  predicted <- -(FT_CARE_HOURS - PT_CARE_HOURS) * WEEKS_PER_YEAR * n$care$children[i] *
    (a - b) * (d$pt_city_parent[i] - d$pt_rest[i])
  stopifnot(abs(d$children_gap[i]) < 1e-6,
            abs(predicted - d$same_rank_aggregation_hours[i]) < 1e-5)
  cat('PASS: common-rank p0 partitions, production controls, program children, hours decompositions\n')
  print(as.data.frame(d %>% select(ecec_type, gap, gap_total, same_rank_aggregation_hours,
                                  city_rank_effect_hours, material_gap)), row.names = FALSE)
  cat(sprintf('Unpaid-center NYC share: p0 %.4f%%; program children %.4f%%\n', 100 * a, 100 * b))
  if (write_output) readr::write_csv(d, file.path(STATE_DATA_ROOT, 'out', 'nyc_rest_of_state_validation.csv'))
  d
}



if (sys.nframe() == 0) {
  args <- commandArgs(TRUE)
  if (length(setdiff(args, '--strict-additivity'))) stop('Unknown NYC diagnostic argument.')
  result <- nyc_rest_of_state_diagnostic()
  if (any(result$material_gap)) {
    cat('SENSITIVITY: own-calibration gaps exceed unchanged 1% / 0.1%-of-total thresholds.\n')
    if ('--strict-additivity' %in% args) quit(status = 1)
  }
}
