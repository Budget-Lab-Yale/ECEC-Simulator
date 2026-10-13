#------------------------------------------------------------------------------
# 10_idea_preschool_special_education.R -- IDEA Part B children 3-5 by
# educational environment, by state, set beside step 4 programme counts.
#
# DIAGNOSTIC ONLY -- writes nothing simulator or 00_ reads.
#
# Source: US Dept of Education, EDFacts, IDEA Part B Child Count and
# Educational Environments 2018-19, Table 12 (bchildcountandedenvironment_12;
# fall snapshot; ages 3 through 5, no age split; WI flagged '*' questionable,
# blank here):
#   raw/US/US_IDEA__child_count_educational_environment_age_3_5__2018-19.xlsx
#   sheet AGE3TO5_EDENVIRONMENT_ALL_N
# Counts: raw children (administrative count, not survey-weighted).
#
# Columns:
#   reg_ec           regular early childhood programme, all four cells (10+
#                    hrs / under 10 hrs x majority of services there / elsewhere)
#                    -- Head Start, pre-K, private preschool, child care centre,
#                    kindergarten alike
#   separate         separate class + separate school (district special-
#                    education class, no regular programme)
#   other_setting    home + service provider location + residential facility
#   nieer_sped_only  NIEER 2019 Table 4 pre-K + special education (unduplicated)
#                    less Table 2 state pre-K, ages 3-4 -- special-education
#                    children outside state pre-K
#                    (raw/US/US_NIEER__prek_and_head_start_enrollment_age_3_4__2018-19.csv,
#                    raw/US/US_NIEER__state_prek_enrollment_age_3_4__2018-19.csv)
#   ccd_prek, prek_taken, prek_source, program_children -- build_of() step 4
# NYC: no IDEA city row -- omitted.
#
# Usage: Rscript state_data/code/10_idea_preschool_special_education.R
#   writes out/idea_preschool_special_education.csv
#------------------------------------------------------------------------------

# build_of(), get_spec(), GEOGRAPHIES, read_chr(), raw_path(), num(); main() guarded
source('state_data/code/00_build_demand_targets.R')

IDEA_FILE  <- 'US_IDEA__child_count_educational_environment_age_3_5__2018-19.xlsx'
IDEA_SHEET <- 'AGE3TO5_EDENVIRONMENT_ALL_N'
IDEA_OUT   <- file.path(STATE_DATA_ROOT, 'out', 'idea_preschool_special_education.csv')

# Table 12 column -> short name (header row text, matched by keyword)
IDEA_COLS <- c(total = '^Environments total', reg10_else = 'at least 10 .*some other',
               reg10_here = 'at least 10 .*that location',
               reglt10_else = 'less than 10 .*some other',
               reglt10_here = 'less than 10 .*that location', home = '^Home$',
               residential = '^Residential', sep_class = '^Separate class',
               sep_school = '^Separate school', provider = '^Service provider')



read_idea <- function() {

  #----------------------------------------------------------------------------
  # Table 12 counts by state, 50 states + DC.
  #
  # Returns: (tibble) geo + IDEA_COLS names (num; NA where flagged/suppressed)
  #----------------------------------------------------------------------------

  f <- raw_path(IDEA_FILE)
  if (!file.exists(f)) stop('MISSING INPUT: ', f)
  d <- suppressMessages(readxl::read_excel(f, sheet = IDEA_SHEET, col_names = FALSE,
                                           col_types = 'text'))
  hdr <- which(d[[1]] == 'State')
  if (length(hdr) != 1) stop(IDEA_FILE, ': no State header row')
  h <- gsub('[0-9]+$', '', trimws(unlist(d[hdr, ])))
  idx <- vapply(IDEA_COLS, function(p) {
    j <- grep(p, h)
    if (length(j) != 1) stop(IDEA_FILE, ': header match ', p, ' -> ', length(j))
    j
  }, 1L)

  body <- d[(hdr + 1):nrow(d), ]
  names_full <- trimws(body[[1]])
  geo <- names(STATE_POSTAL_TO_FIPS)[match(names_full, STATE_NAMES)]
  out <- tibble::tibble(geo = geo)
  for (k in names(idx)) out[[k]] <- num(body[[idx[[k]]]])
  out <- dplyr::filter(out, !is.na(geo))
  if (nrow(out) != length(STATE_POSTAL_TO_FIPS)) stop(IDEA_FILE, ': expected 51 rows')
  out
}



# Full state name by postal code, STATE_POSTAL_TO_FIPS order
STATE_NAMES <- c(datasets::state.name, 'District of Columbia')[
  match(names(STATE_POSTAL_TO_FIPS), c(datasets::state.abb, 'DC'))]



main <- function() {

  #----------------------------------------------------------------------------
  # Joins IDEA, NIEER special-education-only, step 4 counts; writes IDEA_OUT.
  #
  # Returns: (tibble) IDEA_OUT rows, invisibly
  #----------------------------------------------------------------------------

  idea <- read_idea() %>%
    dplyr::mutate(reg_ec        = reg10_else + reg10_here + reglt10_else + reglt10_here,
                  separate      = sep_class + sep_school,
                  other_setting = home + provider + residential)

  t4 <- read_chr(raw_path(NIEER_T4_FILE))
  t2 <- read_chr(raw_path(NIEER_T2_FILE))
  nieer <- dplyr::inner_join(t4, t2, by = 'state') %>%
    dplyr::transmute(geo = state,
                     nieer_sped_only = num(prek_sped_age3) + num(prek_sped_age4) -
                                       num(prek_age3) - num(prek_age4))

  geos <- setdiff(GEOGRAPHIES, names(SUBSTATE_GEOGRAPHIES))
  steps <- dplyr::bind_rows(lapply(geos, function(g) {
    r <- build_of(g)
    p <- r$programs
    tibble::tibble(geo = g, prek_source = p$prek_source,
                   ccd_prek = unname(get_spec(g)$programs$n_children[
                     get_spec(g)$programs$program == 'public pre-K']),
                   prek_taken = unname(p$slots[['public pre-K']]),
                   hs_children = unname(p$net[['Head Start']]),
                   program_children = p$total)
  }))

  out <- idea %>%
    dplyr::left_join(nieer, by = 'geo') %>%
    dplyr::left_join(steps, by = 'geo') %>%
    dplyr::mutate(reg_ec_pct_of_program  = 100 * reg_ec / program_children,
                  separate_pct_of_program = 100 * separate / program_children) %>%
    dplyr::arrange(geo)
  tot <- out %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    dplyr::mutate(geo = 'US', prek_source = NA_character_,
                  reg_ec_pct_of_program  = 100 * reg_ec / program_children,
                  separate_pct_of_program = 100 * separate / program_children)
  out <- dplyr::bind_rows(out, tot)

  write.csv(out, IDEA_OUT, row.names = FALSE)
  cat('== IDEA Part B ages 3-5, fall 2018 (raw children) vs step 4\n\n')
  show_df(out %>% dplyr::transmute(geo, total, reg_ec, separate, other_setting,
                                   nieer_sped_only, prek_source,
                                   program_children = round(program_children),
                                   reg_pct = round(reg_ec_pct_of_program, 1),
                                   sep_pct = round(separate_pct_of_program, 1)))
  cat('\nwrote ', IDEA_OUT, '\n', sep = '')
  invisible(out)
}



if (sys.nframe() == 0) main()
