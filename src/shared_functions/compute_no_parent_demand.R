compute_no_parent_demand <- function(children, no_parent_care_shares) {

  #----------------------------------------------------------------------------
  # Fixed market demand of children under 5 w/o parent unit. These children
  # stay outside choice model (no parent to choose work, care); their care
  # held at base-year NSECE shares by age, added to demand when prices clear
  # (Anna, 2026-11-17). Grows w/ aged child weights -- not w/ prices or policy.
  #
  # Params:
  #   - children (df): ACS children table (aged to sim year), w/ hh_id,
  #     parent_unit_id, age, child_weight
  #   - no_parent_care_shares (df): age, ecec_choice, ecec_hours_choice,
  #     share (from 1b; shares sum to 1 within age)
  #
  # Returns: (num vec) annual child-hours by market sector (length 4, order
  #   of MARKET_SECTORS); zeros if no shares table
  #----------------------------------------------------------------------------

  if (is.null(no_parent_care_shares) || nrow(no_parent_care_shares) == 0) {
    return(rep(0, length(MARKET_SECTORS)))
  }

  # Weighted children under 5 w/o parent unit, by age
  pu <- as.character(children$parent_unit_id)
  weight_by_age <- children %>%
    filter(age < 5, is.na(pu) | pu == '' | pu == 'NA') %>%
    group_by(age) %>%
    summarise(child_weight = sum(child_weight), .groups = 'drop')

  missing_ages <- setdiff(weight_by_age$age, no_parent_care_shares$age)
  if (length(missing_ages) > 0) {
    stop('compute_no_parent_demand: no care shares for ages ', paste(missing_ages, collapse = ', '),
         '. Rebuild no_parent_care_shares in 1b.')
  }

  # Child-hours by sector: weight x share x annual hours of hours choice;
  # non-market care types (Parent Only, Other, K-8) carry no sector
  hours_by_sector <- no_parent_care_shares %>%
    inner_join(weight_by_age, by = 'age') %>%
    mutate(
      sector_id = match(ecec_choice, MARKET_SECTORS),
      hours     = unname(HOURS_ANNUAL[ecec_hours_choice])
    ) %>%
    filter(!is.na(sector_id), !is.na(hours)) %>%
    group_by(sector_id) %>%
    summarise(child_hours = sum(child_weight * share * hours), .groups = 'drop')

  Qd_fixed <- rep(0, length(MARKET_SECTORS))
  Qd_fixed[hours_by_sector$sector_id] <- hours_by_sector$child_hours

  if (any(!is.finite(Qd_fixed)) || any(Qd_fixed < 0)) {
    stop('compute_no_parent_demand: non-finite or negative fixed demand.')
  }

  return(Qd_fixed)
}
