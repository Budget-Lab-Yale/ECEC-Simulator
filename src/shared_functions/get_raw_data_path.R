get_raw_data_path <- function(filename, raw_root = file.path('state_data', 'raw'),
                               must_exist = TRUE) {

  # Resolve resource names, aliases, state folders; explicit raw root first.
  # Params: filename (chr), raw_root (chr), must_exist (logical).
  # Returns: (chr) canonical or compatible legacy input path.
  name <- basename(filename)
  scope <- sub('_.*$', '', name)
  scopes <- unique(c(scope, if (scope == 'NYS') 'NY' else if (scope == 'NY') 'NYS' else scope,
                     if (scope == 'US') '1_US' else scope, 'local'))
  legacy <- file.path(raw_root, scopes, name)
  if (raw_root != file.path('state_data', 'raw') && any(file.exists(legacy))) {
    return(legacy[which(file.exists(legacy))[1]])
  }
  name <- sub('^US_OES__', 'US_BLS_OEWS__', name)
  if (raw_root != file.path('state_data', 'raw')) {
    files <- list.files(raw_root, recursive = TRUE, full.names = TRUE)
    found <- files[basename(files) %in% c(basename(filename), name)]
    if (length(found) == 1) return(found)
    if (length(found) > 1) stop('Duplicate raw input basename in configured raw root: ', name)
  }
  files <- list.files('resources', recursive = TRUE, full.names = TRUE)
  found <- files[basename(files) == name]
  if (length(found) == 1) return(found)
  if (length(found) > 1) stop('Duplicate raw input basename: ', name)
  if (any(file.exists(legacy))) return(legacy[which(file.exists(legacy))[1]])
  files <- list.files(raw_root, recursive = TRUE, full.names = TRUE)
  found <- files[basename(files) == name]
  if (length(found) == 1) return(found)
  if (length(found) > 1) stop('Duplicate raw input basename: ', name)
  if (must_exist) stop('Raw input missing: ', name, '. Check resources source folders or configured raw root.')
  file.path(raw_root, scope, name)
}
