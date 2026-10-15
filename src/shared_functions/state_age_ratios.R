#------------------------------------------------------------------------------
# state_age_ratios.R -- state population-growth ratios for demographic aging.
#
# Experiment (env var ECEC_STATE_AGING=state, read in config.R; default
# 'national' = off, outputs unchanged). age_demographics() (3b) multiplies the
# national growth factor of each (age, married) cell by state/national growth
# ratio at that age. Ratios: config/state/aging/state_age_ratios.csv
# (state_data/code/15_state_age_ratios.R; Census estimates 2019 -> 2024).
# Years after 2024 use 2024 ratio. Called from: 3a (attach to state base
# households), 3b age_demographics().
#------------------------------------------------------------------------------



get_state_aging_mode <- function() {

  #----------------------------------------------------------------------------
  # Active mode -- global state_aging_mode (config.R), else env var
  # ECEC_STATE_AGING, else 'national'.
  #
  # Returns: (chr) 'national' | 'state'
  #----------------------------------------------------------------------------

  m <- if (exists('state_aging_mode', envir = globalenv())) {
    get('state_aging_mode', envir = globalenv())
  } else {
    Sys.getenv('ECEC_STATE_AGING', 'national')
  }
  if (identical(m, '')) m <- 'national'
  if (!m %in% c('national', 'state')) {
    stop('ECEC_STATE_AGING = "', m, '" unknown. Use national (default) or state.')
  }
  m
}



load_state_age_ratios <- function(geo, path = file.path('config', 'state', 'aging',
                                                         'state_age_ratios.csv')) {

  #----------------------------------------------------------------------------
  # Rows of geo in ratio file.
  #
  # Params:
  #   - geo (chr): postal or sub-state code
  #   - path (chr): ratio csv (state, age, year, ratio)
  #
  # Returns: (tibble) age, year, ratio
  #----------------------------------------------------------------------------

  if (!file.exists(path)) {
    stop('ECEC_STATE_AGING=state: ', path, ' missing. Run ',
         'Rscript state_data/code/15_state_age_ratios.R first.')
  }
  r <- read.csv(path, stringsAsFactors = FALSE)
  r <- r[r$state == geo, c('age', 'year', 'ratio')]
  if (nrow(r) == 0) stop('ECEC_STATE_AGING=state: no ratio rows for ', geo, ' in ', path)
  tibble::as_tibble(r)
}



state_age_ratio_for_year <- function(ratios, target_year) {

  #----------------------------------------------------------------------------
  # Ratio by age at target_year; years past the file's last year use the last.
  #
  # Params:
  #   - ratios (tibble): load_state_age_ratios() output
  #   - target_year (int): year to age to
  #
  # Returns: (tibble) age, ratio
  #----------------------------------------------------------------------------

  y <- min(target_year, max(ratios$year))
  ratios[ratios$year == y, c('age', 'ratio')]
}
