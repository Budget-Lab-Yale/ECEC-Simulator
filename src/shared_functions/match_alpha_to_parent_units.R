match_alpha_to_parent_units <- function(parent_units_df, type_params, pu_name) {

  #----------------------------------------------------------------------------
  # Reorders a calibrated alpha matrix so its rows align with parent_units_df.
  #
  # Alpha matrices are calibrated once (2a) and stored with row_ids keyed by
  # (hh_id, parent_unit_id, pseudofamily_id). Simulation dataframes may be
  # subsampled or reordered relative to calibration, so each use must match
  # alpha rows to the current dataframe rows by key.
  #
  # Fails loudly on duplicate keys (would silently expand rows through the
  # join) and on unmatched rows (calibration data must cover all simulation
  # households).
  #
  # Params:
  #   - parent_units_df (df): Parent units dataframe for a single type
  #   - type_params (list): Demand parameters for this type; must contain
  #       alpha (matrix) and row_ids (df with hh_id, parent_unit_id, and
  #       optionally pseudofamily_id)
  #   - pu_name (chr): Parent unit type name (for error messages)
  #
  # Returns: (matrix) alpha matrix with rows reordered to match
  #   parent_units_df row order
  #----------------------------------------------------------------------------

  alpha <- type_params$alpha
  row_ids <- type_params$row_ids
  n_rows <- nrow(parent_units_df)

  if (is.null(alpha)) {
    stop('match_alpha_to_parent_units: alpha matrix not found for ', pu_name, '. ',
         'Re-run calibration to generate alpha matrices.')
  }

  # Extract keys from parent_units_df
  has_pseudofamily <- 'pseudofamily_id' %in% names(parent_units_df)

  if (has_pseudofamily) {
    pu_keys <- parent_units_df %>%
      select(hh_id, parent_unit_id, pseudofamily_id) %>%
      mutate(row_idx = row_number())
  } else {
    # For types without pseudofamily_id, use default value of 1
    pu_keys <- parent_units_df %>%
      select(hh_id, parent_unit_id) %>%
      mutate(pseudofamily_id = 1L, row_idx = row_number())
  }

  # Convert row_ids to tibble for joining
  row_ids_tbl <- tibble(row_ids) %>%
    mutate(alpha_idx = row_number())

  # Handle case where row_ids may not have pseudofamily_id
  if (!('pseudofamily_id' %in% names(row_ids_tbl))) {
    row_ids_tbl$pseudofamily_id <- 1L
  }

  # Ensure type consistency for join keys (coerce to character)
  pu_keys <- pu_keys %>%
    mutate(
      hh_id = as.character(hh_id),
      parent_unit_id = as.character(parent_unit_id),
      pseudofamily_id = as.character(pseudofamily_id)
    )
  row_ids_tbl <- row_ids_tbl %>%
    mutate(
      hh_id = as.character(hh_id),
      parent_unit_id = as.character(parent_unit_id),
      pseudofamily_id = as.character(pseudofamily_id)
    )

  # Validate key uniqueness to avoid many-to-many joins (silent row expansion)
  dup_alpha <- row_ids_tbl %>%
    count(hh_id, parent_unit_id, pseudofamily_id) %>%
    filter(n > 1)
  if (nrow(dup_alpha) > 0) {
    stop('match_alpha_to_parent_units: alpha row_ids contain duplicate keys. ',
         'This would create an ambiguous match.')
  }
  dup_pu <- pu_keys %>%
    count(hh_id, parent_unit_id, pseudofamily_id) %>%
    filter(n > 1)
  if (nrow(dup_pu) > 0) {
    stop('match_alpha_to_parent_units: parent_units_df contains duplicate keys. ',
         'This indicates duplicated rows for (hh_id, parent_unit_id, pseudofamily_id).')
  }

  # Match: find alpha_idx for each parent_units_df row
  matched <- pu_keys %>%
    left_join(row_ids_tbl, by = c('hh_id', 'parent_unit_id', 'pseudofamily_id'))

  # Check for unmatched rows (should not happen if data is consistent)
  n_unmatched <- sum(is.na(matched$alpha_idx))
  if (n_unmatched > 0) {
    stop(sprintf(
      'match_alpha_to_parent_units: %d of %d rows have no matching alpha. ',
      n_unmatched, n_rows
    ), 'Check that calibration data covers all simulation households.')
  }

  # Reorder alpha matrix to match parent_units_df row ordering
  alpha[matched$alpha_idx, , drop = FALSE]
}
