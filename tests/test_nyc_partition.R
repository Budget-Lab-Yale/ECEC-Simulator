# Own NYC calibration must differ freely; shared-rank partition must add exactly.
suppressPackageStartupMessages(library(dplyr))
source('state_data/cluster/acs_p0_shares.R')
source('state_data/tests/check_nyc_rest_of_state.R')

ids <- nyc_partition_hh_ids(list(NY = 1:4, NYC = 1:2))
stopifnot(identical(ids$NYR, 3:4), is.null(nyc_partition_hh_ids(list(NY = 1:4))))
err <- tryCatch(nyc_partition_hh_ids(list(NY = 1:4, NYC = 5L)), error = identity)
stopifnot(inherits(err, 'error'))
catalog <- get_choice_catalog(1)
ad <- list(p0 = matrix(1 / nrow(catalog), nrow = 4, ncol = nrow(catalog)),
  row_ids = tibble(hh_id = 1:4), child_weights = list(child_weight.1 = c(10, 20, 30, 40)))
own <- ad
own$p0[,] <- 0
own$p0[, 1] <- 1
fixture <- bind_rows(geo_children(ad, 1, ids), geo_children(own, 1, list(NYC = 1:2))) %>%
  tidyr::separate(key, c('ecec_type', 'hours_choice'), sep = '[|]')
stopifnot(validate_nyc_partition(fixture))
parent <- filter(fixture, geo == 'NYC_parent')
city <- filter(fixture, geo == 'NYC')
stopifnot(sum(parent$children) == sum(city$children), any(parent$children != city$children))
reject <- function(x, reference = NULL) {
  err <- tryCatch(validate_nyc_partition(x, reference), error = identity)
  stopifnot(inherits(err, 'error'))
}
x <- fixture; x$children[x$geo == 'NYR'][1] <- x$children[x$geo == 'NYR'][1] + 1; reject(x)
x <- fixture; x$children[x$geo == 'NYC_parent'] <- x$children[x$geo == 'NYC']; reject(x)
reject(bind_rows(fixture, fixture[1, ]))
x <- fixture; x$children[1] <- -1; reject(x)
x <- fixture; x$children[1] <- NA_real_; reject(x)
reference <- filter(fixture, geo %in% c('NY', 'NYC'))
reference$children[1] <- reference$children[1] + 1
reject(fixture, reference)
real <- readr::read_csv(NYC_PARTITION_FILE, show_col_types = FALSE)
stopifnot(validate_nyc_partition(real))
cat('PASS: own-calibration differences allowed; mixed-rank subtraction, stale controls, invalid partitions rejected\n')
