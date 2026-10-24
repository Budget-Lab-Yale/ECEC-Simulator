# Extract nested solver helpers without loading simulation dependencies.
find_assignment <- function(x, name) {
  if (!is.call(x) && !is.expression(x)) return(NULL)
  if (is.call(x) && identical(x[[1]], as.name('<-')) &&
      identical(x[[2]], as.name(name))) return(x)
  for (i in seq_along(x)) {
    if (identical(x[[i]], quote(expr = ))) next
    hit <- find_assignment(x[[i]], name)
    if (!is.null(hit)) return(hit)
  }
  NULL
}

code <- parse('src/3_simulation/3c_run_scenario.R')
eval(find_assignment(code, 'emp_prob_with_shift'))
eval(find_assignment(code, 'solve_group_employment_shift'))

check_shift <- function(prob, target, weights = rep(1, length(prob))) {
  cache <- list(test = list(emp0 = prob, weight = weights))
  shift <- solve_group_employment_shift(NULL, 'test', target, NULL, cache)
  achieved <- weighted.mean(emp_prob_with_shift(prob, shift), weights)
  stopifnot(is.finite(shift), abs(achieved - target) < 1e-6)
  shift
}

# Florida-like near-certain employment needs shift below old -20 bound.
stopifnot(check_shift(1 - 1e-12, 0.894236668625194) < -20,
          check_shift(1e-12, 0.6) > 20)
check_shift(c(0.1, 0.4, 0.9), 0.65, c(1, 2, 3))
check_shift(c(0, 1), 0.4)
stopifnot(solve_group_employment_shift(NULL, 'test', 0.5, NULL,
          list(test = list(emp0 = numeric(0), weight = numeric(0)))) == 0)
cat('PASS: extreme and mixed employment probabilities, expanded bounds, empty groups\n')
