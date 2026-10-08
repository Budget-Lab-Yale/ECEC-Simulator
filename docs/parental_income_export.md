# Parental earnings and tax exports

ECEC exports the parental earnings base and tax components behind its fiscal
score so downstream models can identify income already taxed by ECEC. The
export retains ECEC's employment choices, weights, payroll convention, policy
phase-in, and calendar-to-fiscal-year timing.

## Definitions

Earnings combine wage and self-employment income. For each parent, the fiscal
calculation selects earnings at the saved employment choice, treats missing
and negative earnings as zero, and applies the Social Security cap separately.
Employee payroll equals 1.45% of earnings plus 6.2% of capped earnings. ECEC
includes an equal employer-side mirror, including on its self-employment
income component. This is the existing ECEC convention, not a separate W-2
compensation estimate or a new statutory self-employment tax calculation.

Income tax and policy credits are the residual of the saved household `taxes`
field after employee payroll. This component can be negative. The identity is:

```
total tax revenue = income tax and policy credits
                  + employee payroll + employer payroll
```

Earnings and tax flows in the aggregate CSVs are billions of nominal dollars.
Counts are weighted persons or parent units, as labeled. Medicare-taxable
earnings equal total parental earnings under this convention. A downstream
conversion to compensation, or a W-2-only employer-payroll calculation, must
supply and identify its own adjustment.

## Native simulation outputs

New simulations add earnings and tax components to `budget_effect.csv` and
write `totals/levels/parental_income.csv` for each scenario and
`totals/deltas/parental_income.csv` for each policy. The delta export retains
full policy-minus-baseline changes and behavioral changes scaled by the policy
phase-in. Mechanical tax-policy changes belong to income tax and policy
credits. When mechanical decomposition is unavailable, total tax components
use the existing full-change fallback.

Fiscal-year flows equal 25% of the preceding calendar year plus 75% of the
current calendar year. The first modeled year uses zero for the preceding
year. Behavioral phase-in is applied before this blend.
`calendar_year_phase_in_factor` remains the current calendar year's factor;
it is not blended. Counts are also not blended.

Older saved fiscal summaries with no earnings-component columns can still be
finalized into their existing budget outputs. Their parental-income export is
skipped with a warning. A partially populated component schema is an error.
For an older archive with saved parent records, use the extractor below.

## Extract from an existing archive

From the repository root, with R and `data.table` installed:

```sh
Rscript --vanilla src/4_output/extract_parental_income_archive.R \
  --run-root=/path/to/completed/ecec-run \
  --output-dir=/path/to/new-parental-income-export \
  --fiscal-years=2026:2055
```

The source archive is read-only. The destination must be new, its parent must
exist, and it must be outside the source run, including after resolving
symlinks. No machine-specific path is required. The command does not rerun
processing, calibration, equilibrium, or discrete choices.

Options:

| Option | Meaning |
| --- | --- |
| `--run-root` | Completed ECEC archive containing `metadata/runscript.csv`, saved parent records, and canonical fiscal totals |
| `--output-dir` | New destination directory |
| `--fiscal-years` | Inclusive range such as `2026:2055`, a single year, or comma-separated years |
| `--scenarios` | Optional comma-separated policy IDs; baseline is always included |
| `--workers` | Parallel workers, default 1; multiple workers require a fork-capable platform |
| `--tolerance-b` | Absolute accounting tolerance in billions of dollars, default `1e-6` ($1,000) |
| `--help` | Usage summary |

The archive extractor currently supports saved single-draw parent records
(`epsilon_id = 1` in every row), with both `c1` and `c2plus` files available.
It rejects other draw configurations rather than interpreting their weights.
All selected scenarios must have the same year range as baseline. An omitted
`phase_in_years` column defaults to immediate implementation, as in the native
simulator. A requested
fiscal year also reads its preceding calendar year when that year is inside
the source horizon. Canonical fiscal totals must contain finite mechanical,
behavioral, and total tax changes that reconcile. The small canonical tax
table must cover the source horizon start through the latest requested year
so the extractor can verify phase-in consistency.

The output contains `calendar_levels.csv`, `fiscal_levels.csv`,
`fiscal_deltas.csv`, `reconciliation.csv`, `input_manifest.csv`, `MANIFEST.txt`,
and `COMPLETE`. The input manifest inventories the parent records; the run
manifest identifies the source archive, years, scenarios, runtime, and script
hash. Canonical fiscal files and runscript metadata are also required inputs.
The extractor writes into a staging directory and publishes the requested
destination only after all checks pass.

The archive retains fiscal-year mechanical and behavioral tax changes, but
not their underlying calendar-year series. The extractor uses those canonical
components. To check their consistency with phase-in metadata, it recovers
calendar mechanical taxes by inverting the fiscal-year blend from the source
horizon start, then checks the implied phased behavioral taxes. It independently
reconstructs tax levels and full tax changes from parent records; recovering
the mechanical series is an internal consistency check, not an independent
estimate of mechanical taxes.

## Bouchet execution

Submit heavy archive reads through Slurm, from the checkout root:

```sh
ECEC_PROJECT_DIR="$PWD" sbatch slurm/extract_parental_income.slurm \
  /path/to/completed/ecec-run /path/to/new-export 2030
```

The wrapper uses Bouchet's R 4.4.1 module and account/partition defaults. Pass
resource overrides to `sbatch` and set `R_LIBS_USER` for your library if needed.
`ECEC_PROJECT_DIR` selects the checkout; otherwise the wrapper uses
`SLURM_SUBMIT_DIR`. Keep source archives read-only and choose a destination
you own. For the historical 2026–2055 archive, four workers and 24 GB were
sufficient for 360 parent-record files totaling approximately 19.26 GB.

## Tests

```sh
Rscript --vanilla tests/test_parental_income_export.R
```

The tests require `data.table`, `dplyr`, `tidyr`, and `testthat`. They use only
synthetic records and temporary directories. They cover payroll caps, absent
partners, signed credits and earnings changes, phase-in, fiscal timing,
legacy summaries, native/archive agreement, source immutability, single-year
extraction, parallel extraction, and rejection before publication on invalid
inputs or failed reconciliation.
