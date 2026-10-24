library(dplyr)
source('src/3_simulation/3b_run_simulation_year.R')

x <- tibble(hh_id = 1:3, n_children = 1:3, family_weight = c(10, 20, 30),
            child_id.1 = c(11L, 21L, 31L), child_id.2 = c(NA, 22L, 32L),
            child_id.3 = c(NA, NA, 33L), child_weight.1 = c(10, 20, 30),
            child_weight.2 = c(NA, 20, 30), child_weight.3 = c(NA, NA, 30))
split <- function(z) pseudofamily_split(z, 'n_children', 'child_weight', 'family_weight')

# No large families: retain IDs and original weights on early return.
small <- split(x[1:2, ])
stopifnot(nrow(small) == 2, identical(small$pseudofamily_id, c(1L, 1L)),
          identical(small$family_weight, x$family_weight[1:2]),
          identical(small$child_weight.1, x$child_weight.1[1:2]),
          !'child_id.3' %in% names(small))
stopifnot(identical(split(x[1, ])$pseudofamily_id, 1L),
          'pseudofamily_id' %in% names(split(x[0, ])))

# Delaware-sized singleton-only schema lacks second-child columns entirely.
one <- x[1, ] %>% select(-matches('\\.[23]$'))
one_split <- split(one)
stopifnot(identical(one_split$child_id.2, NA_integer_),
          identical(one_split$child_weight.2, NA_real_),
          sum(one_split$child_weight.1 + coalesce(one_split$child_weight.2, 0)) == 10,
          one_split$family_weight == 10)

# Mixed sample: all three unordered pairs, family/child totals preserved.
y <- split(x)
big <- filter(y, hh_id == 3)
stopifnot(nrow(y) == 5, identical(big$pseudofamily_id, 1:3),
          sum(y$family_weight) == sum(x$family_weight),
          all(big$family_weight == 10),
          identical(paste(big$child_id.1, big$child_id.2), c('31 32', '31 33', '32 33')))
for (id in 31:33) {
  total <- sum(big$child_weight.1[big$child_id.1 == id]) +
    sum(big$child_weight.2[big$child_id.2 == id])
  stopifnot(total == 30)
}
cat('PASS: singleton, no-large-family and empty samples retain pseudo IDs; mixed samples preserve pairs and weights\n')
