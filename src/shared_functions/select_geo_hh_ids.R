select_geo_hh_ids <- function(households, geo, fips) {

  #----------------------------------------------------------------------------
  # Household ids for one run geography: a state by STATEFIP, or a sub-state
  # geography (SUBSTATE_GEOGRAPHIES, constants.R) by STATEFIP + COUNTYFIP.
  # Interface w/out countyfip cannot select a sub-state geography -- fails
  # loudly rather than falling back to the whole state.
  #
  # Params:
  #   - households (df): simulation households, w/ statefip (+ countyfip)
  #   - geo (chr): runscript code, e.g. 'NY', 'NYC'
  #   - fips (int): state FIPS for geo (get_run_states())
  #
  # Returns: (vec) hh_id of every household in geo
  #----------------------------------------------------------------------------

  hh <- households %>%
    filter(statefip == fips)

  substate <- SUBSTATE_GEOGRAPHIES[[geo]]
  if (!is.null(substate)) {
    if (!'countyfip' %in% names(hh) || all(is.na(hh$countyfip))) {
      stop('Run requests sub-state geography ', geo, ' but the households ',
           'table has no countyfip values. Regenerate the interface from a ',
           'raw ACS pull carrying COUNTYFIP.')
    }
    hh <- filter(hh, countyfip %in% substate$countyfip)
    cat('  Sub-state: STATEFIP ', fips, ' x COUNTYFIP ',
        paste(substate$countyfip, collapse = ', '), '\n', sep = '')
  }

  pull(hh, hh_id)
}
