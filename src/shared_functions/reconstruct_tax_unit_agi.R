#------------------------------------------------------------------------------
# reconstruct_tax_unit_agi.R
#
# Reconstructs one AGI value per corrected Census tax unit from person-level
# records that carry legacy-fragment AGI values.
#
# Background (see docs/ecec_team_taxid_memo_2026-08-17.md): the April 2024
# Census correction changed tax-unit identifiers, not AGI. Person-level AGI
# remains attached to legacy tax-unit fragments -- typically the couple's
# joint AGI repeated on one spouse's legacy unit and zero on the other. A
# corrected unit's AGI therefore cannot be taken as first(), max(), or a raw
# person-level sum:
#   - first()/max() drop the positive second component in married pairs where
#     both legacy fragments carry positive AGI
#   - a raw person sum double counts within multi-person fragments
#   - a fragment sum double counts when a legacy unit splits across corrected
#     units (both fragments carry the full legacy AGI)
#
# Rule implemented here, validated against the macro team's audit
# (docs/taxid_audit_20260817/):
#   1. A fragment is the set of members of one legacy tax unit within one
#      corrected unit; its AGI is the (single) value its members carry.
#   2. Fragments of non-split legacy units contribute their AGI; the
#      corrected unit's AGI is the sum of contributing fragments. This
#      handles the dominant positive-plus-zero married merge.
#   3. Fragments of legacy units that split across corrected units contribute
#      only in the corrected unit whose ID equals the legacy ID (the
#      'survivor' fragment), preventing within-household double counting.
#      In the audit, the survivor fragment's AGI matched the corrected
#      assignment in all 363 units where first() and max() disagree.
#   4. If a corrected unit has no contributing fragment (possible only when
#      all its fragments are non-survivor pieces of split legacy units;
#      exactly 1 unit in the full file), fall back to the first fragment's
#      AGI.
#
# Full-file validation (2026-08-17): the result equals the survivor
# fragment's AGI in all 1,689,810 identifiable corrected units, and the
# split-fragment exclusion engaged in exactly 9 units. That includes the
# audit's 7 married pairs where both legacy fragments carry positive AGI:
# those spouse fragments are themselves pieces of split legacy units, so the
# rule keeps the spouse's AGI with the corrected unit that retains their
# legacy ID rather than counting it twice within the household. (The memo's
# suggestion to sum both components in those 7 pairs conflicts with its own
# split-unit caution; no-double-counting wins here. Total effect: 7 units,
# ~$1.1M of AGI nationally.)
#
# Called from: 1b (run_acs_processing); standalone validation scripts
#------------------------------------------------------------------------------



reconstruct_tax_unit_agi <- function(persons) {

  #----------------------------------------------------------------------------
  # Reconstructs corrected-unit AGI from legacy-fragment person records.
  #
  # Params:
  #   - persons (df): person-level records with columns:
  #       - hh_id (int): household ID (tax units never span households)
  #       - tax_unit_id (dbl): corrected Census tax-unit ID
  #       - tax_unit_id_legacy (dbl): legacy IPUMS TAXID
  #       - agi (dbl): person-level AGI (attached to legacy fragments)
  #
  # Returns: (df) one row per (hh_id, tax_unit_id) with columns:
  #   - agi (dbl): reconstructed corrected-unit AGI
  #   - n_legacy_fragments (int): number of legacy fragments merged in
  #   - has_survivor_fragment (lgl): whether any fragment's legacy ID equals
  #     the corrected ID
  #----------------------------------------------------------------------------

  # One row per fragment: members of one legacy unit within one corrected unit
  fragments <- persons %>%
    group_by(hh_id, tax_unit_id, tax_unit_id_legacy) %>%
    summarise(fragment_agi = first(agi), .groups = 'drop') %>%

    # Flag legacy units that split across multiple corrected units
    group_by(hh_id, tax_unit_id_legacy) %>%
    mutate(legacy_split = n() > 1) %>%
    ungroup() %>%

    mutate(
      is_survivor = tax_unit_id_legacy == tax_unit_id,
      contributes = !legacy_split | is_survivor
    )

  fragments %>%
    group_by(hh_id, tax_unit_id) %>%
    summarise(
      agi_contributing      = sum(fragment_agi * contributes),
      agi_fallback          = first(fragment_agi),
      any_contributing      = any(contributes),
      n_legacy_fragments    = n(),
      has_survivor_fragment = any(is_survivor),
      .groups = 'drop'
    ) %>%
    mutate(agi = if_else(any_contributing, agi_contributing, agi_fallback)) %>%
    select(hh_id, tax_unit_id, agi, n_legacy_fragments, has_survivor_fragment)
}
