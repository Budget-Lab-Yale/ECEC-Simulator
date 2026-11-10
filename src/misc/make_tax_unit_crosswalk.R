#------------------------------------------------------------------------------
# make_tax_unit_crosswalk.R
#
# Converts Affordability-Index ACS tax units, AGI (Budget-Lab-Yale/
# Affordability-Index, full income chain) to 1b tax-unit crosswalk
# (serialno, sporder, tax_unit, agi). 1b reads it as
# tax_unit_crosswalk_<year>.csv beside ipums_usa.csv for ACS years where
# IPUMS lacks TAXID, ADJGINC (2024).
#
# Inputs, under <ai_income_dir>/<year>/ (Affordability-Index
# data/interim/income): tax_units_<year>.parquet (person -> unit),
# files/*_federal.parquet (one per implicate -- PUF donor returns taxed by
# pinned Tax-Simulator).
#
# AGI: primary returns only (dependents' own returns excluded), mean over
# implicates; unit w/o return -> 0. tax_unit = household serial * 100 +
# primary filer PERNUM.
#
# --check: stale (exit 1) if any input newer than crosswalk.
#
# Usage:
#   Rscript src/misc/make_tax_unit_crosswalk.R [--check] <year> <ai_income_dir> <out.csv>
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(data.table)
})



build_tax_unit_crosswalk <- function(year, ai_income_dir) {

  #----------------------------------------------------------------------------
  # Builds 1b crosswalk from Affordability-Index units, AGI.
  #
  # Params:
  #   - year (int): ACS year
  #   - ai_income_dir (chr): Affordability-Index data/interim/income
  #
  # Returns: (tibble) serialno, sporder, tax_unit, agi -- one row per person
  #----------------------------------------------------------------------------

  year_dir <- file.path(ai_income_dir, year)
  units <- read_parquet(
    file.path(year_dir, sprintf('tax_units_%d.parquet', year)),
    col_select = c('hh_id', 'tu_id', 'PERNUM', 'CBSERIAL')
  )

  fed_files <- list.files(file.path(year_dir, 'files'), pattern = '_federal[.]parquet$', full.names = TRUE)
  if (length(fed_files) == 0) {
    stop('No *_federal.parquet files in ', file.path(year_dir, 'files'), '\n',
         'Run the Affordability-Index income chain through tax_federal for this year first.')
  }
  unit_agi <- bind_rows(lapply(fed_files, read_parquet, col_select = c('tu_id', 'is_dependent_return', 'agi'))) %>%
    filter(!is_dependent_return) %>%
    group_by(tu_id) %>%
    summarise(agi = mean(agi), .groups = 'drop')
  cat('  Persons', nrow(units), '| units', n_distinct(units$tu_id), '| federal files', length(fed_files),
      '| units w/o AGI', n_distinct(setdiff(units$tu_id, unit_agi$tu_id)), '\n')

  # hh_id, tu_id end in household serial, primary filer PERNUM
  out <- units %>%
    left_join(unit_agi, by = 'tu_id') %>%
    mutate(
      agi      = coalesce(agi, 0),
      tax_unit = as.numeric(sub('^.*-', '', hh_id)) * 100 + as.numeric(sub('^.*-', '', tu_id)),
      serialno = as.numeric(CBSERIAL) - year * 1e9
    )
  stopifnot(n_distinct(out$tax_unit) == n_distinct(out$tu_id))

  out <- out %>%
    transmute(serialno, sporder = PERNUM, tax_unit, agi)
  stopifnot(!anyNA(out))
  out
}



if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  check_only <- length(args) > 0 && args[1] == '--check'
  if (check_only) args <- args[-1]
  if (length(args) != 3) {
    stop('Usage: Rscript src/misc/make_tax_unit_crosswalk.R [--check] <year> <ai_income_dir> <out.csv>')
  }
  year <- as.integer(args[1])

  if (check_only) {
    year_dir <- file.path(args[2], year)
    inputs   <- c(file.path(year_dir, sprintf('tax_units_%d.parquet', year)),
                  list.files(file.path(year_dir, 'files'), pattern = '_federal[.]parquet$', full.names = TRUE))
    newer    <- inputs[!file.exists(args[3]) | file.mtime(inputs) > file.mtime(args[3])]
    if (length(newer) == 0) {
      cat('Crosswalk', year, 'is current.\n')
    } else {
      cat('Crosswalk', year, 'is STALE. Newer inputs:\n', paste0('  ', newer, '\n'))
      cat('Rebuild: Rscript src/misc/make_tax_unit_crosswalk.R', year, args[2], args[3], '\n')
      quit(status = 1)
    }
  } else {
    crosswalk <- build_tax_unit_crosswalk(year, args[2])
    fwrite(crosswalk, args[3])
    cat('Wrote', args[3], ':', nrow(crosswalk), 'persons,', n_distinct(crosswalk$tax_unit), 'tax units\n')
  }
}
