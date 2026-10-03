select_geo_hh_ids <- function(households, geo, fips) {

  #----------------------------------------------------------------------------
  # Household ids for one run geography: state by STATEFIP, sub-state
  # (SUBSTATE_GEOGRAPHIES, constants.R) by STATEFIP + COUNTYFIP. Sub-state w/out
  # countyfip fails loudly, never falls back to whole state.
  #
  # Params:
  #   - households (df): Simulation households, w/ statefip (+ countyfip)
  #   - geo (chr): Runscript code, e.g. 'NY', 'NYC'
  #   - fips (int): geo's state FIPS (get_run_states())
  #
  # Returns: (vec) hh_id of every household in geo
  #----------------------------------------------------------------------------

  hh <- filter(households, statefip == fips)

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
