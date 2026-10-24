# Explicit source overrides must beat canonical repository resources.
source('src/shared_functions/get_raw_data_path.R')
root <- tempfile()
dir.create(file.path(root, 'custom', 'nested'), recursive = TRUE)
name <- 'US_BLS_OEWS__childcare_occupation_wages__2019.csv'
custom <- file.path(root, 'custom', 'nested', name)
writeLines('fixture', custom)
stopifnot(identical(get_raw_data_path(name, root), custom),
          identical(get_raw_data_path(sub('US_BLS_OEWS__', 'US_OES__', name), root), custom),
          grepl('resources/bls_oews/', get_raw_data_path(name), fixed = TRUE))
dir.create(file.path(root, 'another'))
stopifnot(file.copy(custom, file.path(root, 'another', name)))
err <- tryCatch(get_raw_data_path(name, root), error = identity)
stopifnot(inherits(err, 'error'), grepl('configured raw root', conditionMessage(err)))
missing <- 'US_REVIEW__missing.csv'
err <- tryCatch(get_raw_data_path(missing, root), error = identity)
stopifnot(inherits(err, 'error'),
          identical(get_raw_data_path(missing, root, FALSE), file.path(root, 'US', missing)))
stopifnot(startsWith(normalizePath(root, winslash = '/'),
                     paste0(normalizePath(tempdir(), winslash = '/'), '/')))
unlink(root, recursive = TRUE)
cat('PASS: custom nested source overrides, OES alias, duplicate and missing-input guards\n')
