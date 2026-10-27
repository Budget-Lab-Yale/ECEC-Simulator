# ECEC-Simulator

A microsimulation model of the U.S. early childhood education and care (ECEC) market. Simulates behavioral responses to policy interventions by solving for equilibrium between childcare demand (families) and supply (providers).

## Overview

ECEC-Simulator evaluates childcare policy proposals by:
- Modeling family decisions over employment and childcare arrangements using a structural discrete choice model
- Solving for market equilibrium prices across four care sectors
- Computing fiscal costs, distributional impacts, and poverty effects
- Decomposing effects into mechanical (policy generosity) and behavioral (response) components
- Projecting outcomes forward across multi-decade horizons (e.g. 2026-2055)

**Data Foundation**: IPUMS USA (ACS), corrected Census ACS-SPM tax units, NSECE (National Survey of Early Care and Education), Tax-Simulator output, Macro-Projections

## Quick Start

```bash
# Basic run with a test scenario
Rscript src/main.R --runscript speed_test

# Reuse all processed data from a prior run
Rscript src/main.R -r speed_test -C 202601151200

# Run with reduced sample for quick testing
Rscript src/main.R -r speed_test -a 1

# See all options
Rscript src/main.R --help
```

### Command-Line Arguments

| Argument | Short | Description |
|----------|-------|-------------|
| `--runscript` | `-r` | Runscript ID (required, e.g., `speed_test`) |
| `--nsece-interface` | `-N` | Reuse NSECE data from prior run |
| `--acs-interface` | `-A` | Reuse ACS data from prior run |
| `--calibration-interface` | `-C` | Reuse all estimation data (NSECE + ACS + calibration) from prior run |
| `--baseline-interface` | `-b` | Reuse converged baseline results from prior run (skips baseline equilibrium solves) |
| `--use-cached-etrs` | `-c` | Use cached effective tax rates |
| `--use-cached-donors` | `-d` | Use cached donor pool instead of rebuilding |
| `--calib-sample` | `-a` | ACS sample percentage for calibration, 1-100 (default: 100) |
| `--sim-sample` | | ACS sample percentage for simulation, 1-100 (must be <= `--calib-sample`) |
| `--setup-only` | | Run processing + calibration only, skip simulation |
| `--n-draws-per-record` | `-n` | Monte Carlo draws per record for discrete simulation (default: 10) |
| `--seed` | `-s` | Seed offset for Gumbel epsilon draws (default: 0) |
| `--fiscal-npv` | `-f` | Enable child fiscal NPV calculations (requires Tax-Simulator-Full-MTR) |
| `--reference` | `-R` | Timestamp of prior run for regression comparison |
| `--no-price-wedge` | | Disable price wedge heterogeneity (set all wedges to 1.0) |
| `--no-employment-targeting` | | Skip employment targeting in equilibrium solver |
| `--nsece-year` | | NSECE survey year to calibrate on: `2019` (default) or `2024`. Sets the paired ACS year, which is the model base year (see Calibration year below). Pass the same value when reusing a run with `-N`, `-A`, or `-C` |
| `--nsece-2024-options` | | Optional 2024 NSECE data treatments, comma-separated, or `all` (default: none; no effect with the 2019 survey): `hours` (parents with a missing work-hours record get hours from their "worked last week" answer), `weights` (child weights of children under 5 raked to the March 2024 CPS share of children by mother's work status and family type), `supply` (2024 posted center rates at the 5th/95th percentile caps set to missing, as in 2019, and center staffing from directors' staff counts). See Calibration year below |
| `--calib-target1-name` | | First calibration target (default: `part_cost`) |
| `--calib-target1-value` | | Value for first calibration target |
| `--calib-target2-name` | | Second calibration target (default: `hours_income`) |
| `--calib-target2-value` | | Value for second calibration target |
| `--slurm-phase` | | SLURM job array phase: `setup`, `year`, or `finalize` |
| `--slurm-scratch` | | Path to scratch directory for inter-job communication |
| `--slurm-year-index` | | 1-based year index (maps to `$SLURM_ARRAY_TASK_ID`) |
| `--help` | `-h` | Show help and exit |

**Interface hierarchy**: Each level implies the ones below it: `-C` includes ACS + NSECE; `-A` includes NSECE; `-N` is standalone.

**Baseline reuse** (`-b`): Because the baseline is policy-invariant, a completed run's converged baseline (equilibrium prices, employment shifts, collapsed choices) can be reused, roughly halving run time when iterating on counterfactual policies. Requirements — enforced by startup validation and per-year integrity assertions:
- Identical baseline policies, `--calib-sample`/`--sim-sample`, `--seed`, and `-n` as the source run
- Same estimation data — combine with `-C` pointing at the same run (e.g., `-r my_runs -C 202603111322 -b 202603111322`)
- The source run must cover every year in the current runscript, and its baseline must have converged in each

Percentage-type employer subsidies additionally need equilibrium wages in the source run's `supply_<year>.yaml`, which older runs predate.

## Project Structure

```
src/
├── main.R                    # Entry point
├── misc/config.R             # CLI parsing, interface hierarchy, scenario config
├── 1_processing/             # NSECE + ACS data processing
├── 2_calibration/            # Model parameter estimation (CRRA demand)
├── 3_simulation/             # Equilibrium solver & choice simulation
├── 4_output/                 # Summaries, deltas, Excel workbooks
└── shared_functions/         # Reusable utilities (one function per file)

config/
├── default_paths.yaml        # File system paths & data dependencies
├── runscripts/               # Scenario definitions (CSV files)
├── policy_demand/            # Demand-side subsidy policies
├── policy_supply/            # Supply-side policies
├── cdctc/                    # Child & Dependent Care Tax Credit policies
├── policy_tax/               # Tax policies (CTC-like credits)
├── policy_transfer/          # Choice-dependent cash transfer policies
├── employer_subsidy/         # Employer wage subsidy configs (YAML)
├── wage_floor/               # Wage floor configs (YAML)
├── demand/                   # Calibrated demand parameters (timestamped)
└── supply/                   # Calibrated supply parameters (timestamped)

tests/                        # testthat test suite
slurm/                        # SLURM parallel execution scripts
```

## Model Flow

1. **Processing** - Process NSECE (supply-side) and ACS (demand-side) microdata. Imputes earnings (Heckman selection), childcare enrollment and prices (random forest), employment (random forest), taxes (percentile-based donor matching), and SPM poverty units. Tax units use the Census Bureau's corrected April 2024 ACS-SPM `Tax_unit` identifiers (joined via a crosswalk; the legacy IPUMS `TAXID` splits nearly all married couples into separate tax units), with unit AGI reconstructed from deduplicated legacy fragments (`src/shared_functions/reconstruct_tax_unit_agi.R`; see `docs/ecec_team_taxid_memo_2026-08-17.md`).

   NSECE processing (`run_nsece_processing(nsece_year)` in `src/1_processing/1a_nsece_processing.R`) reads the 2019 survey by default and the 2024 public-use files (ICPSR 39466) with `--nsece-year 2024`: `get_nsece_year_settings()` holds each year's file names and the household-side low/high center price cut ($4.75 in 2019 dollars, grown by the CPI day care and preschool index to $5.76 for 2024), and the `harmonize_nsece_2024_*()` functions recode the 2024 reserve codes, respondent-sex items, and administrative-data centers to the 2019 conventions.

   **Calibration year.** The model is calibrated on one NSECE survey and the ACS year paired with it (`NSECE_ACS_YEAR_PAIRS` in `src/shared_functions/constants.R`): the 2019 NSECE with the 2019 ACS (default), or the 2024 NSECE with the 2023 ACS, the latest ACS year on disk. The ACS year is the model base year: utility is measured in its dollars, growth factors and population aging start from it, Tax-Simulator donors and the OASI tax maximum are taken for it, and estimation files carry its suffix (`acs_*_<year>.csv`, `supply_<year>.yaml`, `demand_params_<year>.yaml`, `alpha_*_<year>.rds`). The NSECE household tables keep the survey year (`nsece_*_2024.csv`). When the survey year differs from the base year, 1a converts NSECE dollars to base-year dollars (care worker wages by the hourly wage index, the non-labor price residuals and the Other Paid price by CPI-U) and records the factors and the survey-year values in the supply YAML. The corrected Census tax-unit crosswalk exists only for 2019; the 2023 ACS uses the IPUMS `TAXID` as is. The price-model cache is `cache/price_models/price_qrf_models.rds` for 2019 and `price_qrf_models_<survey year>.rds` otherwise.

   **Optional 2024 treatments** (`--nsece-2024-options`, all off by default, 2024 survey only; defined in `NSECE_2024_OPTIONS` in `constants.R` and applied in 1a). `hours`: the 2024 survey blanks both parents' calendar work hours (code -10) when either has a missing calendar block, so these parents never reach the donor-hours rule; with this option a parent with no hours record who said yes to "worked last week" (D1a) gets hours drawn from working parents of the same earner rank, and one who said no gets zero. `weights`: multiplies the child weights of children under 5 by the factors in `config/nsece_reweight/nsece2024_child_weight_factors_cps2024.csv`, which rake the 2024 NSECE to the March 2024 CPS shares of children under 5 in five groups (two parents with the mother at work or not, one parent at work or not, no parent); this lowers the share of young children with a working mother or single parent from 64.3% to 59.6%. `supply`: sets 2024 positive posted weekly rates at or beyond each age's 5th or 95th percentile to missing, the way the 2019 public file trimmed them, and measures center staffing per FTE child from the director's count of teachers and aides for children 0-5 (BA share from the workforce file) instead of the workforce file's national totals. Because each run rebuilds the price-model cache in `cache/price_models/`, runs with different options must not share one code folder at the same time. The comparison of the 2019 and 2024 surveys, the studies behind these options, and the decisions still open are in `docs/nsece_2024/` (start with `decisions.md`).

2. **Calibration** - Estimates CRRA demand parameters (beta, rho) by targeting empirical elasticities. Backs out household-level preference heterogeneity (alpha matrices) from observed/imputed choice probabilities.

3. **Simulation** - For each year: ages the population, applies macro projections, loads policies, and solves for market equilibrium. Post-equilibrium, expands with Gumbel epsilon shocks and collapses to discrete choices via argmax.

4. **Output** - Writes level summaries and delta files (scenario minus baseline), generates formatted Excel workbooks.

## Key Design Features

### Equilibrium

Solves for market-clearing prices across four care sectors (unpaid center-based, low-priced center-based, high-priced center-based, paid home-based) using L-BFGS-B with warm-start from the previous year. Demand is multinomial logit over (employment, care type, care hours) choices; supply is constant elasticity labor supply.

### Parent Unit Types

Families are classified into 2 types by number of children:
- **c1**: 1-child families (45 choice alternatives)
- **c2plus**: 2+ child families (675 choice alternatives)

Each choice combines: primary caregiver employment (FT/PT/NW) x child care arrangements (type x hours per child).

### Two-Stage Choice Simulation

1. **Equilibrium**: Uses softmax probabilities (smooth, differentiable)
2. **Post-equilibrium**: Expands with Gumbel epsilon shocks, collapses to discrete choices via argmax

### Effect Decomposition

Results decompose policy impacts into mechanical (pure generosity change at baseline behavior) and behavioral (additional impact from changed choices) components.

## Policy Channels

The simulator supports six independent policy channels, all specified per-scenario in the runscript CSV:

| Channel | Config directory | Function | Description |
|---------|-----------------|----------|-------------|
| Demand subsidies | `config/policy_demand/` | `do_demand_policy()` | Returns subsidy matrix (n_units x n_choices) |
| CDCTC | `config/cdctc/` | `do_cdctc_policy()` | Child & Dependent Care Tax Credit |
| Supply | `config/policy_supply/` | `do_supply_policy()` | Transforms market prices to supplier-received prices |
| Tax credits | `config/policy_tax/` | `do_tax_policy()` | CTC-like credits that modify total tax by employment status |
| Transfers | `config/policy_transfer/` | `do_transfer_policy()` | Choice-dependent cash transfers (n_units x n_choices); enter income, not care costs, with their own budget line |
| Employer subsidies | `config/employer_subsidy/` | YAML config | Percentage or dollar wage subsidies to providers |

See existing policy files for function signatures and examples.

## Runscripts

Scenarios are defined in CSV files under `config/runscripts/`. Example from `report_runs.csv`:

```csv
id,policy_demand,policy_supply,policy_cdctc,policy_tax,employer_subsidy,wage_floor,years,phase_in_years
baseline,baseline,baseline,baseline,baseline,none,none,2026:2055,
universal,universal,baseline,baseline,baseline,none,none,2026:2055,5
means_tested,means_tested,baseline,baseline,baseline,none,none,2026:2055,5
```

| Column | Description |
|--------|-------------|
| `id` | Scenario identifier (first row must be `baseline`) |
| `policy_demand` | Demand policy file (without `.R`) |
| `policy_supply` | Supply policy file |
| `policy_cdctc` | CDCTC policy file |
| `policy_tax` | Tax policy file |
| `policy_transfer` | Transfer policy file (optional; default `baseline` = none) |
| `employer_subsidy` | Employer subsidy config (without `.yaml`) |
| `wage_floor` | Wage floor config (without `.yaml`) |
| `years` | Year range, colon-separated (e.g., `2026:2055`) |
| `phase_in_years` | Optional linear ramp from 0% to 100% over N years |

Not all columns are required. Simpler runscripts (e.g., `speed_test.csv`) can omit columns that have defaults.

## Output Structure

```
{output_root}/{timestamp}/
├── metadata/
│   ├── run_info.txt            # Command line args, runtime parameters, data dependency paths
│   └── runscript.csv           # Copy of the runscript used
├── estimation/                 # Processed data, models, calibrated params
└── simulation/
    ├── baseline/
    │   └── totals/levels/      # Aggregated outcomes (allocation, employment, budget, poverty)
    └── {scenario_id}/
        └── totals/
            ├── levels/         # Absolute values
            └── deltas/         # Differences from baseline
```

Each scenario also gets an Excel workbook with formatted summary tables.

## SLURM Parallelization

Simulation years can be processed in parallel via SLURM job arrays:

```bash
./slurm/launch_array.sh -r report_runs -a 20 -n 20 -N 202602052017 -f
```

Three-phase architecture: (1) setup — processing + calibration, (2) year workers — one SLURM array task per year, (3) finalize — combine results and write output.

## Configuration

Edit `config/default_paths.yaml` to set data paths:

```yaml
roots:
  input: '/path/to/input/data'
  output: '/path/to/output'

dependencies:
  NSECE: '/raw_data/NSECE/v1'
  ACS: '/raw_data/ACS/v1/2023'
  Census-ACS-SPM: '/raw_data/Census-ACS-SPM/v1/2019'  # Corrected tax-unit crosswalk
  Macro-Projections: '/model_data/Macro-Projections/v3/.../baseline'
  Tax-Simulator: '/model_data/Tax-Simulator/v1/.../baseline_ex_cdctc'
  Tax-Simulator-Full-MTR: '/model_data/Tax-Simulator/v1/.../baseline'  # Required for --fiscal-npv
  Tax-Data: '/model_data/Tax-Data/v1/.../baseline'
```

## Requirements

- R 4.0+
- Key packages: data.table, tidyverse, yaml, ranger, openxlsx, sampleSelection, Hmisc
