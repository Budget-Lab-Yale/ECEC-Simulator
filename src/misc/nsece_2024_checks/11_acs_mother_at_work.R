#------------------------------------------------------------------------------
# 11_acs_mother_at_work.R -- ACS benchmark for the NSECE working-mother share
#
# Share of children under 5 living w/ a parent whose mother (or the one
# parent, if no mother in the household) was at work last week (EMPSTATD 10),
# child person weights, households only (GQ 1, 2, 5). Same definition as
# docs/nsece_2024/employment_and_reweighting.md: reproduces its 61.1 (2019) and
# 62.6 (2023), then gives 2024 from the 2024 extract (config key ACS-2024).
#
# Run on Bouchet through sbatch (reads ~1.3 GB; about 96 GB memory):
#   Rscript src/misc/nsece_2024_checks/11_acs_mother_at_work.R [acs root]
#
# main.R sources every .R file under src/ -- body runs only under Rscript
# (sys.nframe() == 0), never when sourced
#------------------------------------------------------------------------------



mother_at_work <- function(path, years) {

  #----------------------------------------------------------------------------
  # Print the at-work share of the mother (or one parent) of children under 5.
  #
  # Params:
  #   - path (chr): IPUMS person file (ipums_usa.csv)
  #   - years (int): Survey years to keep
  #
  # Returns: nothing (prints tables)
  #----------------------------------------------------------------------------

  d <- fread(path, select = c('YEAR', 'SERIAL', 'PERNUM', 'AGE', 'MOMLOC', 'POPLOC', 'EMPSTATD', 'PERWT', 'GQ'))
  d <- d[YEAR %in% years & GQ %in% c(1, 2, 5)]

  parents <- d[, .(YEAR, SERIAL, PERNUM, at_work = EMPSTATD == 10)]
  kids    <- d[AGE < 5 & (MOMLOC > 0 | POPLOC > 0)]
  kids[, parent_loc := fifelse(MOMLOC > 0, MOMLOC, POPLOC)]
  kids[, parent     := fifelse(MOMLOC > 0, 'mother', 'father only')]
  kids <- merge(kids, parents, by.x = c('YEAR', 'SERIAL', 'parent_loc'), by.y = c('YEAR', 'SERIAL', 'PERNUM'))

  print(kids[, .(children_unweighted = .N, at_work_pct = round(100 * weighted.mean(at_work, PERWT), 2)), by = YEAR][order(YEAR)])
  print(kids[, .(children_unweighted = .N, at_work_pct = round(100 * weighted.mean(at_work, PERWT), 2)), by = .(YEAR, parent)][order(YEAR, parent)])
}



if (sys.nframe() == 0) {
  suppressPackageStartupMessages(library(data.table))

  args     <- commandArgs(trailingOnly = TRUE)
  acs_root <- if (length(args) > 0) args[1] else '/nfs/roberts/project/pi_nrs36/shared/raw_data/ACS/v1'

  cat('== ACS v1/2023 file: 2019 and 2023\n')
  mother_at_work(file.path(acs_root, '2023/ipums_usa.csv'), c(2019, 2023))

  cat('\n== ACS v1/2024 file: 2024\n')
  mother_at_work(file.path(acs_root, '2024/ipums_usa.csv'), 2024)
}
