get_price_model_cache_path <- function(nsece_year = DEFAULT_NSECE_YEAR) {

  #----------------------------------------------------------------------------
  # Returns the path of the cached price heterogeneity models (QRF) for one
  # NSECE survey year. 1a writes the file; calibration (2a) and simulation
  # initialization (3a) read it. The 2019 file keeps its original name so
  # existing caches still work; other years carry the survey year, so a 2024
  # run does not overwrite the 2019 models.
  #
  # Params:
  #   - nsece_year (int): NSECE survey year (default: DEFAULT_NSECE_YEAR)
  #
  # Returns: (chr) Path to the cached price model file
  #----------------------------------------------------------------------------

  cache_dir <- './cache/price_models'
  if (nsece_year == 2019) {
    return(file.path(cache_dir, 'price_qrf_models.rds'))
  }
  file.path(cache_dir, paste0('price_qrf_models_', nsece_year, '.rds'))
}
