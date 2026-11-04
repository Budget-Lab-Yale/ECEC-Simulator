# Supply-input rebuild after source acquisition.
# Usage: Rscript resources/supply/00_update_state_supply.R [--skip-annual-wages]
# Legal ratios (licensing_ratios_reviewed_2019.csv) frozen, diagnostic only -- builder removed 2026-11-04.
args <- commandArgs(TRUE)
if (length(setdiff(args, '--skip-annual-wages'))) stop('Unknown supply-build argument.')
if (!'--skip-annual-wages' %in% args) {
  source('resources/supply/annual_wage_ratios.R')
  build_annual_wage_ratios()
} else {
  cat('Annual wage extraction skipped; existing annual panel retained when present.\n')
}
source('resources/supply/state_supply_params.R')
main()
