library(dplyr)
source('src/shared_functions/prepare_year_context.R')

cache <- tibble(hh_id = 1:3, parent_unit_id = 1L,
                price_wedge.center_low = 0.34, price_wedge.center_high = 1.72,
                price_wedge.home = 0.39, price_wedge.other_paid = 0.63)
validate <- function(x, z = cache) validate_prepared_price_wedges(list(c2plus = x), z)
expect_failure <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, 'error'), grepl(pattern, conditionMessage(err)))
}

# Constant Alaska-sized group, repeated pseudofamilies, shuffled cache order.
stopifnot(validate(cache), validate(cache[c(1, 1, 2), ]),
          validate(cache, cache[3:1, ]), validate(cache[1, ]))
expect_failure(validate(cache, cache[-1, ]), 'missing from')
expect_failure(validate(cache, bind_rows(cache, cache[1, ])), 'duplicate')
wrong <- cache; wrong$price_wedge.center_high[1] <- 1
expect_failure(validate(wrong), 'differs from')
for (bad in c(NA_real_, Inf, 0, -1)) {
  wrong <- cache; wrong$price_wedge.home[1] <- bad
  expect_failure(validate(wrong), 'finite and positive')
}
expect_failure(validate(select(cache, -price_wedge.center_low)), 'required columns')
disabled <- cache %>% mutate(across(starts_with('price_wedge.'), ~ 1))
stopifnot(validate(disabled, NULL))
expect_failure(validate(cache, NULL), 'must equal one')
stopifnot(validate_prepared_price_wedges(list(c1 = cache[0, ], c2plus = NULL), cache))
cat('PASS: constant and singleton wedges, keyed cache agreement, missing/duplicate keys, invalid values, disabled wedges\n')
