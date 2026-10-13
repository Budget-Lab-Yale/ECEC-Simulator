#------------------------------------------------------------------------------
# licensing_ratios_2017.R -- writes
# US_ICPSR37700__center_and_home_child_staff_ratios_by_age__2017.csv, licensed
# children per staff by state, setting, age point, read by state_supply_params.R
# (staffing index). Also writes state_data/out/licensing_ratio_change_2014_2017.csv
# (2014 vs 2017 ratios, indices, both at 3 points) and
# state_data/out/licensing_ratio_change_3pt_5pt.csv (2017 centre index, 3 vs 5
# age points).
#
# SOURCE. 2017 Child Care Licensing Study (ACF/OPRE, ICPSR 37700), state
# regulations in effect 2017:
#   state_data/raw/US/US_ICPSR37700__licensing_center_regulations__2017.rda
#     (da37700.0002; PS_RAT_* 'Ratio by Age', text 'N:1')
#   state_data/raw/US/US_ICPSR37700__licensing_small_family_home_regulations__2017.rda
#     (da37700.0003; F_D_CH_MAXCHLD 'Maximum Number of Children Allowed', text)
#   dictionaries: same names w/ _dictionary__2017.xlsx
# Former input (2014 study via CAP 2018 Table 1, PN3 2020 Table 6):
#   state_data/raw/US/US_ACF__center_and_home_child_staff_ratios_by_age__2014.csv
#
# CENTRE AGE POINTS -- one representative point per year of age 0-4
#   (NSECE_AGE_POINT, shared_nsece_ages.R): 11_months = PS_RAT_11MTHS,
#   18_months = PS_RAT_18MTHS, 35_months = PS_RAT_35MTHS, 3_years =
#   PS_RAT_3YRS, 4_years = PS_RAT_4YRS. One centre index, no extra sectors.
#   First 2017 version used 3 of them (2014 input's points: infant = 11
#   months, toddler = 35 months, preschool = 3 years).
#   11 months, not 6 weeks: only NV differs (6 wks 4:1, 11 mo 6:1); CAP's NV
#   infant 6 matches 11 months.
#   Missing 18_months / 4_years -> point used before (35_months / 3_years),
#   logged, noted in source. 2017 study: none missing.
#   ID: point system, table 0s -- 12 points per staff; child under 24 months
#   2 points, 24-36 months 1.5, 3-5 years 1, 5-13 years 0.5 (Idaho Code
#   39-1109 to 2024; IDAPA 16.06.03.300 from 7-1-24; 2025 law changed
#   values -- not base year)
#   -> each point from rule directly: 12 / points at point's age (ID_POINTS:
#   11, 18 months 2 -> 6; 35 months 1.5 -> 8; 3, 4 years 1 -> 12)
#   (2026-10-13; 2014 bands before -- 18 months took toddler 8, rule gives
#   6). NYC 3 bands -> 5 points by NSECE_AGE_BAND: 11_months infant;
#   18_months, 35_months toddler; 3_years, 4_years preschool.
# HOME -- max children excl. school-age add-ons (PN3 convention): number
#   before '(N plus M SAC)' when text has one (IL 'up to 12 (8 plus 4 SAC)'
#   -> 8), else leading number ('6 (plus 2 SAC)' -> 6; DE '4 (plus 2 SAC) or
#   5 preschool-age' -> 4; GA '6 (plus 2 children 3 years and older)' -> 6,
#   add-on two 1-hour periods daily only; TN '7 (plus 5 related)' -> 7).
#   Blank (no small FCC licence: AZ ID IN LA NJ OH SD) -> NA, index 1.
# NYC: study has no city rows -- centre rows from 2014 input (Health Code
#   Article 47, DOHMH June 2019); home = NY 2017 value (NYS rules).
#
# Usage, from repo root: Rscript resources/supply/licensing_ratios_2017.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr); library(dplyr)
})

# STATE_POSTAL_TO_FIPS
source('src/shared_functions/constants.R')
# NSECE_AGE_POINT, NSECE_AGE_BAND, NSECE_AGES_0_4
source('state_data/code/shared_nsece_ages.R')

RAW      <- file.path('state_data', 'raw', 'US')
CENTRE17 <- file.path(RAW, 'US_ICPSR37700__licensing_center_regulations__2017.rda')
HOME17   <- file.path(RAW, 'US_ICPSR37700__licensing_small_family_home_regulations__2017.rda')
OLD14    <- file.path(RAW, 'US_ACF__center_and_home_child_staff_ratios_by_age__2014.csv')
OUT      <- file.path('resources', 'supply',
                      'US_ICPSR37700__center_and_home_child_staff_ratios_by_age__2017.csv')
CHANGE   <- file.path('state_data', 'out', 'licensing_ratio_change_2014_2017.csv')
CHANGE_PTS <- file.path('state_data', 'out', 'licensing_ratio_change_3pt_5pt.csv')

POINT_VAR <- c('11_months' = 'PS_RAT_11MTHS', '18_months' = 'PS_RAT_18MTHS',
               '35_months' = 'PS_RAT_35MTHS', '3_years' = 'PS_RAT_3YRS',
               '4_years' = 'PS_RAT_4YRS')
stopifnot(setequal(names(POINT_VAR), NSECE_AGE_POINT))
# Point missing in a state -> point used before 5-point method
FALLBACK <- c('18_months' = '35_months', '4_years' = '3_years')
# 2014 band -> its 2017 age point (3-point method)
BAND_POINT <- c(infant = '11_months', toddler = '35_months', preschool = '3_years')
# Age 0-4 -> point, 3-point method (2014-2017 comparison, 3pt vs 5pt file)
AGE_POINT_3PT <- setNames(unname(BAND_POINT[NSECE_AGE_BAND]), names(NSECE_AGE_BAND))
# Age point -> 2014 band (NYC rows onto 5 points)
POINT_BAND <- setNames(unname(NSECE_AGE_BAND[NSECE_AGES_0_4]),
                       unname(NSECE_AGE_POINT[NSECE_AGES_0_4]))
# ID centres: point system, not tabulated in 2017 study -- points per child at
# each age point (under 24 months 2, 24-36 months 1.5, 3-5 years 1), 12 points
# per staff (Idaho Code 39-1109 before 2024 = IDAPA 16.06.03.300, 7-1-24)
POINT_RULE_CENTRE   <- 'ID'
ID_POINTS           <- c('11_months' = 2, '18_months' = 2, '35_months' = 1.5,
                         '3_years' = 1, '4_years' = 1)
ID_POINTS_PER_STAFF <- 12
SRC_C <- setNames(paste0('US_ICPSR37700__licensing_center_regulations__2017.rda, ', POINT_VAR),
                  names(POINT_VAR))
SRC_H <- paste0('US_ICPSR37700__licensing_small_family_home_regulations__2017.rda, ',
                'F_D_CH_MAXCHLD (excl. school-age add-ons; NA = no small FCC licence)')



load_rda <- function(f) {

  #----------------------------------------------------------------------------
  # Loads one-object .rda; STATE trimmed to chr.
  #
  # Params:
  #   - f (chr): path
  #
  # Returns: (data.frame)
  #----------------------------------------------------------------------------

  if (!file.exists(f)) stop('MISSING INPUT: ', f)
  e <- new.env()
  d <- e[[load(f, e)]]
  d$STATE <- trimws(as.character(d$STATE))
  d
}



parse_ratio <- function(x) {

  #----------------------------------------------------------------------------
  # 'N:1' -> N; anything else NA.
  #
  # Params:
  #   - x (chr)
  #
  # Returns: (num)
  #----------------------------------------------------------------------------

  x <- trimws(as.character(x))
  ok <- grepl('^[0-9]+:1$', x)
  ifelse(ok, as.numeric(sub(':1$', '', x)), NA_real_)
}



parse_max_children <- function(x) {

  #----------------------------------------------------------------------------
  # F_D_CH_MAXCHLD text -> max children excl. school-age add-ons (header).
  #
  # Params:
  #   - x (chr)
  #
  # Returns: (num) NA where blank
  #----------------------------------------------------------------------------

  x <- trimws(as.character(x))
  inner <- regmatches(x, regexpr('[(][0-9]+ plus', x))
  out <- rep(NA_real_, length(x))
  has_inner <- grepl('[(][0-9]+ plus', x)
  out[has_inner] <- as.numeric(gsub('[^0-9]', '', inner))
  lead <- !has_inner & grepl('^[0-9]+', x)
  out[lead] <- as.numeric(sub('^([0-9]+).*$', '\\1', x[lead]))
  out
}



build_2017 <- function() {

  #----------------------------------------------------------------------------
  # 2017 ratios in 2014 input's schema, 50 states + DC + NYC; centre rows at
  # 5 age points (POINT_VAR), home one row.
  #
  # Returns: (tibble) state, setting, age_group, children_per_staff, source
  #----------------------------------------------------------------------------

  states <- names(STATE_POSTAL_TO_FIPS)
  ce <- load_rda(CENTRE17) %>% filter(STATE %in% states)
  fh <- load_rda(HOME17) %>% filter(STATE %in% states)
  old <- read_csv(OLD14, col_types = cols(.default = col_character()))
  if (nrow(ce) != length(states) || nrow(fh) != length(states)) {
    stop('2017 study: expected 51 rows per file')
  }

  centre <- bind_rows(lapply(names(POINT_VAR), function(p) {
    tibble::tibble(state = ce$STATE, setting = 'center', age_group = p,
                   children_per_staff = parse_ratio(ce[[POINT_VAR[[p]]]]),
                   source = SRC_C[[p]])
  })) %>%
    filter(!state %in% POINT_RULE_CENTRE)

  # Missing 18_months / 4_years -> state's value at point used before
  for (p in names(FALLBACK)) {
    miss <- centre$age_group == p & is.na(centre$children_per_staff)
    for (st in centre$state[miss]) {
      v <- centre$children_per_staff[centre$state == st & centre$age_group == FALLBACK[[p]]]
      i <- centre$state == st & centre$age_group == p
      centre$children_per_staff[i] <- v
      centre$source[i] <- paste0(SRC_C[[p]], ' not N:1 -- ', FALLBACK[[p]], ' value (',
                                 POINT_VAR[[FALLBACK[[p]]]], ')')
      cat('fallback: ', st, ' ', p, ' -> ', FALLBACK[[p]], ' (', v, ')\n', sep = '')
    }
  }
  if (anyNA(centre$children_per_staff)) {
    stop('2017 centre ratio not N:1 for ',
         paste(unique(centre$state[is.na(centre$children_per_staff)]), collapse = ' '))
  }

  # NYC: 2014 rows (Article 47), 3 bands -> 5 points via POINT_BAND
  keep14 <- old %>%
    filter(setting == 'center', state == 'NYC') %>%
    mutate(children_per_staff = as.numeric(children_per_staff),
           source = paste0('US_ACF__center_and_home_child_staff_ratios_by_age__2014.csv row -- ',
                           source))
  keep14 <- bind_rows(lapply(names(POINT_BAND), function(p) {
    keep14 %>%
      filter(age_group == POINT_BAND[[p]]) %>%
      mutate(age_group = p,
             source = paste0(source, '; ', POINT_BAND[[p]], ' band -> ', p))
  }))
  if (nrow(keep14) != length(POINT_BAND)) stop(OLD14, ': NYC centre band missing')

  # ID: point rule at each age point -- children per staff = 12 / points
  if (!setequal(names(ID_POINTS), names(POINT_VAR))) {
    stop('ID_POINTS must cover every age point: ', paste(names(POINT_VAR), collapse = ', '))
  }
  idr <- tibble::tibble(state = POINT_RULE_CENTRE, setting = 'center',
                        age_group = names(ID_POINTS),
                        children_per_staff = ID_POINTS_PER_STAFF / unname(ID_POINTS),
                        source = paste0('Idaho point rule (Idaho Code 39-1109 / IDAPA 16.06.03.300): ',
                                        ID_POINTS_PER_STAFF, ' points per staff, ',
                                        unname(ID_POINTS), ' points per child at ', names(ID_POINTS)))
  keep14 <- bind_rows(keep14, idr)

  home <- tibble::tibble(state = fh$STATE, setting = 'home',
                         age_group = 'all_under_school_age',
                         children_per_staff = parse_max_children(fh$F_D_CH_MAXCHLD),
                         source = SRC_H)
  home <- bind_rows(home, home %>% filter(state == 'NY') %>%
                      mutate(state = 'NYC', source = paste0(SRC_H, '; NY value (NYS rules)')))

  bind_rows(centre, keep14, home) %>%
    arrange(setting, state, match(age_group, c(names(POINT_VAR), 'all_under_school_age')))
}



main <- function() {

  #----------------------------------------------------------------------------
  # Writes OUT, CHANGE, CHANGE_PTS; prints changed states and index moves.
  #
  # Returns: (tibble) CHANGE_PTS rows, invisibly
  #----------------------------------------------------------------------------

  new <- build_2017()
  write_csv(new, OUT, na = 'NA')
  cat('wrote ', OUT, ' (', nrow(new), ' rows)\n', sep = '')

  # licensing_indices(), RATIOS; main() guarded
  source('resources/supply/state_supply_params.R')
  old <- read_csv(OLD14, col_types = cols(.default = col_character()))

  wide <- function(d, tag) {
    d %>%
      transmute(state, k = paste0(tag, '_', age_group),
                v = suppressWarnings(as.numeric(children_per_staff))) %>%
      tidyr::pivot_wider(names_from = k, values_from = v)
  }

  # 2014 vs 2017, both at 3 points -- isolates ratio vintage
  new_3pt <- new %>%
    filter(setting == 'home' | age_group %in% BAND_POINT) %>%
    mutate(age_group = ifelse(setting == 'home', age_group,
                              names(BAND_POINT)[match(age_group, BAND_POINT)]))
  ix14 <- licensing_indices(OLD14, NSECE_AGE_BAND) %>%
    rename(centre_2014 = centre, home_2014 = home)
  ix17 <- licensing_indices(OUT, AGE_POINT_3PT) %>%
    rename(centre_2017 = centre, home_2017 = home)

  ch <- wide(old, 'r14') %>%
    inner_join(wide(new_3pt, 'r17'), by = 'state') %>%
    inner_join(ix14, by = 'state') %>%
    inner_join(ix17, by = 'state') %>%
    mutate(centre_changed = r14_infant != r17_infant | r14_toddler != r17_toddler |
                            r14_preschool != r17_preschool,
           home_changed   = is.na(r14_all_under_school_age) != is.na(r17_all_under_school_age) |
                            (r14_all_under_school_age != r17_all_under_school_age) %in% TRUE,
           centre_pct     = 100 * (centre_2017 / centre_2014 - 1),
           home_pct       = 100 * (home_2017 / home_2014 - 1)) %>%
    arrange(state)
  write_csv(ch, CHANGE)

  cat('\ncentre ratios changed (any band): ', sum(ch$centre_changed), ' of ',
      nrow(ch), ' geographies\n', sep = '')
  cat('home max children changed:        ', sum(ch$home_changed), '\n', sep = '')
  cat('centre index change, %: ', paste(sprintf('%.1f', range(ch$centre_pct)), collapse = ' to '),
      '; |change| > 5%: ', sum(abs(ch$centre_pct) > 5), '\n', sep = '')
  cat('home index change, %:   ', paste(sprintf('%.1f', range(ch$home_pct)), collapse = ' to '),
      '; |change| > 5%: ', sum(abs(ch$home_pct) > 5), '\n\n', sep = '')
  print(as.data.frame(ch %>% filter(centre_changed | home_changed) %>%
                        mutate(across(where(is.double), ~ round(.x, 3)))),
        row.names = FALSE)
  cat('\nwrote ', CHANGE, '\n', sep = '')

  # 2017 centre index, 3 vs 5 age points -- staffing_index() uses 5
  ix5 <- licensing_indices(OUT, NSECE_AGE_POINT)
  pts <- new %>%
    filter(setting == 'center') %>%
    group_by(state) %>%
    dplyr::summarise(mapped_from_2014_bands = any(grepl('band ->', source)),
                     fallback = paste(age_group[grepl('not N:1', source)], collapse = ';'),
                     .groups = 'drop')
  cp <- wide(new %>% filter(setting == 'center'), 'r') %>%
    inner_join(pts, by = 'state') %>%
    inner_join(ix17 %>% select(state, centre_3pt = centre_2017), by = 'state') %>%
    inner_join(ix5 %>% select(state, centre_5pt = centre), by = 'state') %>%
    mutate(centre_pct = 100 * (centre_5pt / centre_3pt - 1)) %>%
    arrange(state)
  write_csv(cp, CHANGE_PTS)

  cat('\ncentre index, 3 vs 5 age points (', nrow(cp), ' geographies, unweighted)\n',
      sep = '')
  cat('change, %: ', paste(sprintf('%.1f', range(cp$centre_pct)), collapse = ' to '),
      '; |change| > 1%: ', sum(abs(cp$centre_pct) > 1),
      '; |change| > 5%: ', sum(abs(cp$centre_pct) > 5), '\n', sep = '')
  cat('5-point index: ', sprintf('%.3f', min(cp$centre_5pt)), ' (',
      cp$state[which.min(cp$centre_5pt)], ') to ', sprintf('%.3f', max(cp$centre_5pt)),
      ' (', cp$state[which.max(cp$centre_5pt)], ')\n', sep = '')
  cat('fallbacks: ', sum(cp$fallback != ''), '\n\n', sep = '')
  print(as.data.frame(cp %>% select(-mapped_from_2014_bands, -fallback) %>%
                        mutate(across(where(is.double), ~ round(.x, 3)))),
        row.names = FALSE)
  cat('\nwrote ', CHANGE_PTS, '\n', sep = '')
  invisible(cp)
}



if (sys.nframe() == 0) main()
