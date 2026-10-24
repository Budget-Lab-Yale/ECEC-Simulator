# Supply-input rebuild after source acquisition and licensing hash validation.
# Usage: Rscript resources/supply/00_update_state_supply.R [--skip-annual-wages]
args <- commandArgs(TRUE)
if (length(setdiff(args, '--skip-annual-wages'))) stop('Unknown supply-build argument.')
# Refresh audit from evidence; fail before writing supply inputs on hash mismatch.
status <- system2(Sys.getenv('PYTHON', 'python'),
                  'state_data/code/analyze_licensing_2019.py')
if (status != 0) stop('Staffing source validation failed. Check sources and PYTHON executable.')
source('resources/supply/licensing_ratios_2019.R')
build_reviewed_staffing()
if (!'--skip-annual-wages' %in% args) {
  source('resources/supply/annual_wage_ratios.R')
  build_annual_wage_ratios()
} else {
  cat('Annual wage extraction skipped; existing annual panel retained when present.\n')
}
source('resources/supply/state_supply_params.R')
main()
