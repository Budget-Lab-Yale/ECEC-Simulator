#------------------------------------------------------------------------------
# nsece_parent_attributes.R
#
# NSECE parent sex, caregiver, employment -- rules shared by 1a (parent
# units), 1b (forest features), tests (2026-10-12).
#
# Sex. Public file carries sex of respondent only (HH9_RGENDER); roster
# sex (HH9_B1C_GENDER_X) not released. Non-respondent parent's sex read
# from household counts of mothers, fathers (HH9_HHCOMP_NUMMOTHER /
# NUMFATHER) after respondent's own count taken out. Before: male1 set only
# when parent 1 = respondent -> 3.9% of children 0-4 out of forest frame.
#
# Employment. Concept = ACS's, since simulator's employment choice is ACS's:
# primary caregiver's (mother in two-parent unit, else parent) usual hours
# worked, past 12 months (UHRSWORK) -- same variable sets observed choice,
# hourly earnings, counterfactual states (1b), so forest feature must mean
# same thing. ACS ESR (reference week) not used: would band mother who
# worked earlier in year 'none' while simulator holds her pt / ft. Each
# NSECE status mapped to UHRSWORK:
#   worked for pay last week (HH9_D1A_WORK = 1) -> current job, reference-
#     week hours in work, school, training, commuting (HH9_WST_HRS_TOT_PHI/
#     PLO) -- pt under 35, ft 35+; no calendar hours -> NA. Public file has
#     no work-only hours (calendar released as WST totals only; commute
#     minutes D2_COMMUTE not released) -- commute stays in. School /
#     training last week too (HH9_D1B_SCHOOL / HH9_D1C_TRAINING = 1) -> WST
#     band kept. Tested NA for these at 35+ hrs (2026-10-12): 4.0% of
#     children 0-4 left frame, 84% of them in non-parental care -- frame
#     Parent Only 43.5 -> 44.7%, ACS-NSECE gap -3.2 -> -3.9 points; not
#     adopted (selection on outcome worse than band error)
#   not working last week, still holds job (last-job date 9999.99, e.g. on
#     leave -- ACS 'with job, not at work', UHRSWORK > 0) -> usual hours at
#     that job (HH9_D5C_HOUR: < 15, 15-30 -> pt; > 30 -> ft)
#   not working last week, last job ended within 12 months of interview
#     (HH9_D5B_LASTWRKMOYR_R; interview month from child ages,
#     nsece_interview_time()) -- ACS unemployed / NILF w/ UHRSWORK > 0 ->
#     usual hours at last job, as above
#   else -> none; DK / refused -> NA
# NA employment -> family out of forest frame (was 'none').
#------------------------------------------------------------------------------

# Hours cut between pt / ft -- same as 1b derive_employment_choice()
EMP_FT_HOURS <- 35



infer_parent_sex <- function(links, hh) {

  #----------------------------------------------------------------------------
  # Sex of every parent (HH9_PARCH link) in household. Respondent (roster
  # member 1) w/ known HH9_RGENDER: that. Other parents (+ respondent parent
  # w/ sex unknown) form pool: household's mothers / fathers minus known
  # respondent's own count must equal pool size -- all mothers -> 0, all
  # fathers -> 1, mixed / inconsistent -> NA.
  #
  # Params:
  #   - links (df): hh_id, hhm_id -- parent links (any child), duplicates OK
  #   - hh (df): hh_id, resp_male (num 1 / 0 / NA), n_mothers (num),
  #       n_fathers (num)
  #
  # Returns: (tibble) hh_id, hhm_id, male (num 1 / 0 / NA), sex_source (chr:
  #   'respondent' | 'household counts' | 'unresolved')
  #----------------------------------------------------------------------------

  par <- links %>%
    distinct(hh_id, hhm_id) %>%
    left_join(hh %>% select(hh_id, resp_male, n_mothers, n_fathers), by = 'hh_id') %>%
    mutate(known = hhm_id == 1 & !is.na(resp_male))
  cnt <- par %>%
    group_by(hh_id) %>%
    summarise(
      n_pool = sum(!known),
      m_left = first(n_mothers) - sum(known & resp_male == 0),
      f_left = first(n_fathers) - sum(known & resp_male == 1),
      .groups = 'drop'
    ) %>%
    mutate(
      ok = !is.na(m_left) & !is.na(f_left) & m_left >= 0 & f_left >= 0 &
           m_left + f_left == n_pool,
      pool_male = case_when(
        !ok                        ~ NA_real_,
        f_left == 0 & m_left > 0   ~ 0,
        m_left == 0 & f_left > 0   ~ 1,
        TRUE                       ~ NA_real_
      )
    )
  par %>%
    left_join(cnt %>% select(hh_id, pool_male), by = 'hh_id') %>%
    mutate(
      male = if_else(known, resp_male, pool_male),
      sex_source = case_when(
        is.na(male)  ~ 'unresolved',
        known        ~ 'respondent',
        TRUE         ~ 'household counts'
      )
    ) %>%
    select(hh_id, hhm_id, male, sex_source)
}



nsece_person_employment <- function(worked, wst_hours, last_work, last_hours,
                                    last_work_cutoff) {

  #----------------------------------------------------------------------------
  # Employment band of one NSECE adult, ACS concept (usual hours, past 12
  # months) as near as public file allows -- see file header.
  #
  # Params:
  #   - worked (num vec): HH9_D1A_WORK (1 yes, 2 no, 3 DK / refused)
  #   - wst_hours (num vec): reference-week work / school / training /
  #       commuting hours (HH9_WST_HRS_TOT_PHI/PLO); negative / NA = no
  #       calendar
  #   - last_work (num vec): HH9_D5B_LASTWRKMOYR_R, year + (month - 1) / 12
  #       of last job; NA = never worked / not asked; >= 9000 (9999.99) =
  #       still holds job (interviewer enters 33/33 -- User's Guide p.159),
  #       counts as recent
  #   - last_hours (num vec): HH9_D5C_HOUR (1 < 15, 2 15-30, 3 > 30, 4 DK)
  #   - last_work_cutoff (num vec): interview date minus one year, same
  #       scale -- last job at / after counts as within 12 months
  #
  # Returns: (chr vec) 'none' | 'pt' | 'ft' | NA (hours / recency unknown)
  #----------------------------------------------------------------------------

  wst <- if_else(!is.na(wst_hours) & wst_hours >= 0, as.numeric(wst_hours), NA_real_)
  # 9999.99 = still holds job (e.g. on leave) -- recent, banded by D5C
  current_job <- !is.na(last_work) & last_work >= 9000
  recent <- current_job |
            (!is.na(last_work) & !current_job & !is.na(last_work_cutoff) &
               last_work >= last_work_cutoff)
  case_when(
    worked %in% 1 & !is.na(wst) & wst > 0 & wst < EMP_FT_HOURS ~ 'pt',
    worked %in% 1 & !is.na(wst) & wst >= EMP_FT_HOURS          ~ 'ft',
    worked %in% 1                                              ~ NA_character_,
    worked %in% 2 & recent & last_hours %in% c(1, 2)           ~ 'pt',
    worked %in% 2 & recent & last_hours %in% 3                 ~ 'ft',
    worked %in% 2 & recent                                     ~ NA_character_,
    worked %in% 2 & !is.na(last_work) & is.na(last_work_cutoff) ~ NA_character_,
    worked %in% 2                                              ~ 'none',
    TRUE                                                       ~ NA_character_
  )
}



nsece_interview_time <- function(age_months, age_months_sep1) {

  #----------------------------------------------------------------------------
  # Interview date, year + (month - 1) / 12 scale, from child's age in
  # months at interview minus age at 2018-09-01 (HHC9_AGE_AT_SEP1_18 built
  # from interview month; dates themselves suppressed on public file).
  #
  # Params:
  #   - age_months (num vec): HHC9_AGE_X
  #   - age_months_sep1 (num vec): HHC9_AGE_AT_SEP1_18_X; negative / NA =
  #       born after 2018-09-01
  #
  # Returns: (num vec) interview time; NA where sep1 age missing
  #----------------------------------------------------------------------------

  ok <- !is.na(age_months) & !is.na(age_months_sep1) & age_months_sep1 >= 0 &
        age_months >= age_months_sep1
  if_else(ok, 2018 + 8 / 12 + (age_months - age_months_sep1) / 12, NA_real_)
}



nsece_primary_caregiver <- function(hhm_id2, male1, male2) {

  #----------------------------------------------------------------------------
  # Primary caregiver, ACS rule (1b get_primary_caregiver(): mother in
  # two-parent unit, else parent 1) -- but NA where two-parent unit's
  # mother unknown (ACS always knows both sexes).
  #
  # Params:
  #   - hhm_id2 (int vec): parent 2 roster number (NA = one parent)
  #   - male1, male2 (num vec): parent sex (1 / 0 / NA)
  #
  # Returns: (int vec) 1L | 2L | NA
  #----------------------------------------------------------------------------

  case_when(
    is.na(hhm_id2)                 ~ 1L,
    male1 %in% 0                   ~ 1L,
    male2 %in% 0                   ~ 2L,
    male1 %in% 1 & male2 %in% 1    ~ 1L,
    TRUE                           ~ NA_integer_
  )
}
