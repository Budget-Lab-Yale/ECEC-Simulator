# NY/NYC structural checks: NY target = NYC + rest of state exactly (build_target()), annual hours.
# Default: fail invalid partitions, stale controls, inconsistent decomposition.
# --strict-additivity: also apply historical 1% / 0.1%-of-total thresholds.
suppressPackageStartupMessages(library(dplyr))
source('state_data/code/00_build_demand_targets.R')

MAX_GAP <- 0.01
MAX_GAP_TOTAL <- 0.001



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

  # NY target = NYC + rest of state (build_target()); NYR, NYC_parent p0 read
  # from partition controls by p0_rows()
  ny <- load_spec('NY'); nyc <- load_spec('NYC')
  rest <- rest_of_state_spec(ny, nyc)
  city_parent <- nyc
  city_parent$geo <- 'NYC_parent'; city_parent$name <- 'NYC parent-rank diagnostic control'
  city_parent$age5_share <- c(prek = 0, head_start = 0)

  quiet <- function(f, spec) { utils::capture.output(x <- f(spec)); x }
  n <- quiet(build_target, ny); c <- quiet(build_geo, nyc); r <- quiet(build_geo, rest)
  cp <- quiet(build_geo, city_parent)
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

  # NY built as NYC + rest: exact by construction; city rank effect = NYC's own
  # income ranks vs parent ranks, now inside NY's target
  stopifnot(all(abs(d$ny_less_nyc - d$rest_of_state) <= 1e-6 * d$rest_of_state),
            all(abs(d$children_gap) < 1e-6), !any(d$material_gap))
  cat('PASS: common-rank p0 partitions, production controls, NY = NYC + rest of state exactly, hours decompositions\n')
  print(as.data.frame(d %>% select(ecec_type, gap, gap_total, city_rank_effect_hours)), row.names = FALSE)
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
