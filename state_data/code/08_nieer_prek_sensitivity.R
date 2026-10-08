#------------------------------------------------------------------------------
# 08_nieer_prek_sensitivity.R -- demand targets w/ NIEER pre-K count and NIEER
# Head Start overlap in place of CCD pre-K and site-match overlap.
#
# SENSITIVITY ONLY -- main build stays 00_build_demand_targets.R (larger of
# CCD, all-settings pre-K; overlap matching count taken). This version takes
# NIEER's count in every state, even where CCD is larger. Writes demand files in simulator schema to
# out/nieer_sensitivity/<GEO>.csv; copy over config/state/demand/ in a
# separate work dir to run simulator on them.
#
# ONE RULE, every state:
#   pre-K        main build's all-settings count (NIEER Table 2 state pre-K,
#                ages 3 + 4, every setting), even where
#                CCD is larger
#   HS overlap   main build's rule for an all-settings count: NIEER share
#                where Table 4 marks an unduplicated count (dagger), else
#                fallback (pooled_overlap_share())
# CA, MI keep main spec -- pre-K figure already NIEER's. NYC keeps main spec
# -- NIEER has no city count.
#
# RUN 2026-10-06 (Bouchet 202610061347 main vs 202610061349 NIEER, smoke
# policies, KY NJ NY NYC VA; main = CCD pre-K only, earlier build): 2027 budget
# effect moves <= 0.8% KY, VA; 1.2-2.1% NJ; 2.8-4.5% NY (demand subsidy
# -5.5%, child poverty effect +0.2-0.3 pp); child UBI scenarios <= 0.2%.
# Main build since takes larger of CCD, NIEER: national Unpaid Center-Based
# hours 10.0% above this NIEER-everywhere version (Table 2 counts).
#
# Usage: Rscript state_data/code/08_nieer_prek_sensitivity.R
#   reads raw/US/US_NIEER__prek_and_head_start_enrollment_age_3_4__2018-19.csv
#   writes out/nieer_sensitivity/<GEO>.csv, out/nieer_sensitivity_summary.csv
#------------------------------------------------------------------------------

# build_geo(), load_spec(), GEOGRAPHIES; main() guarded
source('state_data/code/00_build_demand_targets.R')

NIEER_T4_FILE <- 'US_NIEER__prek_and_head_start_enrollment_age_3_4__2018-19.csv'
KEEP_MAIN_SPEC <- c('CA', 'MI', 'NYC')
SENS_DIR <- file.path(STATE_DATA_ROOT, 'out', 'nieer_sensitivity')



nieer_spec <- function(spec) {

  #----------------------------------------------------------------------------
  # Main spec -> NIEER sensitivity spec (header, ONE RULE).
  #
  # Params:
  #   - spec (list): load_spec() result
  #
  # Returns: (list) spec w/ CCD pre-K 0, so build takes all-settings count
  #----------------------------------------------------------------------------

  if (spec$geo %in% KEEP_MAIN_SPEC) return(spec)
  # CCD 0 -> build takes all-settings count (Table 2 state pre-K)
  spec$programs$n_children[spec$programs$program == 'public pre-K'] <- 0
  spec
}



main <- function() {

  #----------------------------------------------------------------------------
  # Builds every geography both ways, writes NIEER demand files and summary.
  #
  # Returns: (tibble) summary, invisibly
  #----------------------------------------------------------------------------

  dir.create(SENS_DIR, showWarnings = FALSE, recursive = TRUE)

  rows <- lapply(GEOGRAPHIES, function(g) {
    spec <- load_spec(g)
    capture.output({
      main_res <- build_geo(spec)
      sens_res <- build_geo(nieer_spec(spec))
    })
    readr::write_csv(dplyr::select(sens_res$care, ecec_type, annual_hours),
                     file.path(SENS_DIR, paste0(g, '.csv')))
    tibble::tibble(geo = g, ecec_type = main_res$care$ecec_type,
                   main_hours = main_res$care$annual_hours,
                   nieer_hours = sens_res$care$annual_hours[
                     match(main_res$care$ecec_type, sens_res$care$ecec_type)])
  })
  summ <- dplyr::bind_rows(rows) %>%
    dplyr::mutate(pct_change = round(100 * (nieer_hours / main_hours - 1), 1))
  readr::write_csv(summ, file.path(STATE_DATA_ROOT, 'out', 'nieer_sensitivity_summary.csv'))

  cat('\n== Unpaid Center-Based hours, NIEER vs main (% change)\n')
  u <- dplyr::filter(summ, ecec_type == 'Unpaid Center-Based')
  print(as.data.frame(dplyr::select(u, geo, pct_change)), row.names = FALSE)
  tot <- dplyr::summarise(dplyr::group_by(dplyr::filter(summ, geo != 'NYC'), ecec_type),
                          main = sum(main_hours), nieer = sum(nieer_hours), .groups = 'drop') %>%
    dplyr::mutate(pct_change = round(100 * (nieer / main - 1), 1))
  cat('\n== 50 states + DC, all care types\n')
  print(as.data.frame(tot), row.names = FALSE)
  cat('\nwrote ', SENS_DIR, '/<GEO>.csv, out/nieer_sensitivity_summary.csv\n', sep = '')
  invisible(summ)
}


if (sys.nframe() == 0) main()
