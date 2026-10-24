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
- State runs: the source run must cover every state in the current runscript; each state's baseline (parent units, solver results, supply, employment targeting) is ported from the source run's `states/<ST>/` folder

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
├── substate_geographies.csv  # Sub-state geographies (NYC) by county FIPS; read by constants.R + ACS Python scripts
├── runscripts/               # Scenario definitions (CSV files)
├── policy_demand/            # Demand-side subsidy policies
├── policy_supply/            # Supply-side policies
├── cdctc/                    # Child & Dependent Care Tax Credit policies
├── policy_tax/               # Tax policies (CTC-like credits)
├── policy_transfer/          # Choice-dependent cash transfer policies
├── employer_subsidy/         # Employer wage subsidy configs (YAML)
├── wage_floor/               # Wage floor configs (YAML)
├── demand/                   # Calibrated demand parameters (timestamped)
├── supply/                   # Calibrated supply parameters (timestamped)
└── state/                    # State inputs: demand, supply, employment, aging (per geography)

tests/                        # testthat test suite
slurm/                        # SLURM parallel execution scripts
```

## Model Flow

1. **Processing** - Process NSECE (supply-side) and ACS (demand-side) microdata. Imputes earnings (Heckman selection), childcare enrollment (random forest), price wedges (weighted empirical NSECE price distribution per care type, by income quintile for low-priced center and Other Paid), employment (random forest), taxes (percentile-based donor matching), and SPM poverty units. Tax units use the Census Bureau's corrected April 2024 ACS-SPM `Tax_unit` identifiers (joined via a crosswalk; the legacy IPUMS `TAXID` splits nearly all married couples into separate tax units), with unit AGI reconstructed from deduplicated legacy fragments (`src/shared_functions/reconstruct_tax_unit_agi.R`; see `docs/ecec_team_taxid_memo_2026-08-17.md`).

2. **Calibration** - Estimates CRRA demand parameters (beta, rho) by targeting empirical elasticities. Backs out household-level preference heterogeneity (alpha matrices) from observed/imputed choice probabilities.

3. **Simulation** - For each year: ages the population, applies macro projections, loads policies, and solves for market equilibrium. Post-equilibrium, expands with Gumbel epsilon shocks and collapses to discrete choices via argmax.

4. **Output** - Writes level summaries and delta files (scenario minus baseline), generates formatted Excel workbooks.

## Key Design Features

### Equilibrium

Solves for market-clearing prices across four care sectors (unpaid center-based, low-priced center-based, high-priced center-based, paid home-based) using L-BFGS-B with warm-start from the previous year. Demand is multinomial logit over (employment, care type, care hours) choices; supply is constant elasticity labor supply. For quantity-limited policies, sector offer rates are additional equilibrium objects solved by a nested root-find at each price evaluation (state-conditional choice probabilities don't depend on the offer rates, so the inner solve operates on precomputed per-state aggregates).

### Parent Unit Types

Families are classified into 2 types by number of children:
- **c1**: 1-child families (45 choice alternatives)
- **c2plus**: 2+ child families (675 choice alternatives)

Each choice combines: primary caregiver employment (FT/PT/NW) x child care arrangements (type x hours per child).

Family preparation retains pseudo-family IDs in samples with no families of three or more children and adds missing second-child slots in samples containing only one-child families. `Rscript tests/test_pseudofamily_split.R` checks these cases and verifies that splitting larger families preserves family and child weights.

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

### Quantity-Limited (Rationed) Demand Policies

Demand policies can limit program slots by paid sector (e.g., New York's 2K
program). Offers are drawn **per eligible child, per rationed sector** as
independent Bernoulli lotteries; the equilibrium solves sector offer rates
(nested inside each price evaluation) so that expected take-up — counted in
weighted children — equals the slot count, or the offer rate hits 1 with slack
when the program is undersubscribed.

A rationed policy file (see `config/policy_demand/ny_2k_style.R`) additionally
defines:

- `rationing$slots` — length-4 slot counts by market sector (sector 1 must be
  0), or a `function(year)` for a slot schedule (program ramps should use this
  rather than the runscript `phase_in_years` column, to avoid double-counting)
- `is_child_program_eligible(parent_units_df, child_idx)` — which units' child
  can enter the lottery (must match the eligibility the subsidy function
  applies internally)
- `do_demand_policy(..., offered1, offered2)` — offer-conditional subsidies

Existing (unrationed) policies need no changes. Post-equilibrium, the discrete
stage draws actual offers per record from a dedicated RNG stream (epsilon
draws stay aligned with the baseline) and persists them as `offer_child*.j`
columns. The mechanical effect uses **Option A**: baseline records inherit the
policy run's offer draws, so mechanical take-up intentionally differs from the
slot count (windfall to lottery-winning inframarginal users; see
`docs/mechanical_effect_rationed_programs.html`). Per-scenario diagnostics
land in `models/equilibrium/rationing_<year>.csv` (slots, offer rates,
expected/realized/mechanical take-up) and in the solver results file.

Rationed policies are supported for counterfactual scenarios only (not the
baseline).

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
| `state` | Optional state-level analysis: USPS postal code(s), semicolon-separated (e.g., `NY` or `NY;CA`). Absent/blank/`all` = national run |

Not all columns are required. Simpler runscripts (e.g., `speed_test.csv`) can omit columns that have defaults.

## State-Level Analysis

Setting the `state` runscript column (USPS codes, semicolon-separated, e.g. `NY;CA`) runs the simulation for each geography instead of nationally. `NYC` can be named beside the states. `config/runscripts/state_targets_smoke.csv` runs KY, NJ, NY, and VA. `state_targets_all.csv` runs the 50 states, DC, and NYC. The full design is in `docs/state_level_analysis.md`, the method is in the memo `state_data/docs/methodology-<date>.pdf`, and the input builds are in [state_data/README.md](state_data/README.md).

The national run (no `state` column, default settings) is main's. State machinery adds inputs and switches and changes nothing in it. Processing and calibration stay national. Households are filtered to the geography at simulation initialization, before any `--sim-sample` subsampling. Each geography solves its own equilibrium, summary tibbles gain a `state` column, and output goes to `simulation/{scenario_id}/states/<ST>/`. Run provenance is written to `metadata/state_analysis.yaml`.

### State Inputs

| Input | Location | Description |
|-------|----------|-------------|
| Demand | `config/state/demand/<GEO>.csv` | Base-year 2019 annual hours by care type. Re-anchors the calibrated alpha matrices with a per-care-type contraction (`adjust_alpha_for_state.R`). |
| Supply | `config/state/supply/<GEO>.yaml` | Same schema as `supply_2019.yaml`. State wages, staffing scaled by licensing ratios, and the non-labor residual scaled by the BEA housing-services price parity. Elasticities stay national. |
| Employment | `config/state/employment/<GEO>.csv` | State employment growth in place of the national CBO path. |

`state_data/code/00_build_demand_targets.R` writes the demand files, and `resources/supply/state_supply_params.R` writes the supply files. NYC households are selected by STATEFIP 36 and the five borough county FIPS (`config/substate_geographies.csv`), which needs an ACS pull with `COUNTYFIP`. A state run needs an interface that stores `STATEFIP`, `p0`, and child weights with the alpha matrices.

The October 1, 2019 staffing-rule audit is documented in
[state_data/README.md](state_data/README.md#staffing-rule-audit-october-1-2019).
All 312 geography/setting/age records have historical-source review; 28 change
a numerical input and none remain unresolved. `state_data/code/analyze_licensing_2019.py
--require-verified` checks sources and writes detail and coverage tables without
changing production inputs.

Rebuild supply inputs with `Rscript resources/supply/00_update_state_supply.R`.
It refreshes the source/hash audit before writing staffing inputs, then builds
annual wages and all supply YAMLs. Set `PYTHON` to a working Python executable
if it is absent from PATH. `--skip-annual-wages` reuses an existing annual panel.
`state_supply_params.R --check` validates annual wage paths as well as staffing.
`Rscript tests/test_state_staffing_inputs.R` independently checks the 312 reviewed
records, population normalization, and all 52 saved YAML staffing payloads.
`Rscript tests/test_state_orchestration.R` checks national delegation and state
isolation across sequential, parallel, and finalization paths using small fixtures.
Configured raw-data roots take precedence over repository resources, including
nested source folders; check with `Rscript tests/test_raw_data_paths.R`.

NY and NYC retain their own production calibration. The NY/NYC diagnostic now
uses a separate common-rank partition sidecar from `acs_p0_shares.R`; its default
checks structural identities and reports cross-calibration gaps. Add
`--strict-additivity` to retain the historical 1% / 0.1%-of-total gap test.
See `Rscript tests/test_nyc_partition.R`. Home-staffing scenarios are diagnostic
only: `Rscript state_data/code/analyze_home_staffing.R` writes staffing and
2019 anchor-price sensitivities. The national anchor already includes NSECE
helpers; state capacity proxies don't encode required adult-count differences.
`Rscript state_data/code/review_home_staffing_inputs.R` audits all 52 home inputs
against documented configurations, retaining unresolved mappings explicitly.
`Rscript state_data/code/analyze_home_prices.R` compares anchors with NDCP family-home
prices using fixed county/age weights. The national home anchor uses household net
payments; NDCP market-rate prices measure a different concept. These diagnostics
do not select a staffing baseline or refit residuals. Regression checks:
`Rscript tests/test_home_staffing_review.R`.
Acquisition scratch text and download logs belong in git-ignored
`state_data/work/licensing/`; source documents and evidence remain in their
registered locations.
Git preserves registered source snapshots byte-for-byte. Check their line-ending
attributes with `python tests/test_source_snapshot_attributes.py` before committing
new sources; publisher whitespace stays intact for source-hash verification.

### Settings

| Variable | Values | Description |
|----------|--------|-------------|
| `ECEC_URBAN_DENSITY` | `0` (default), `1` | `1` sets the center tier line by region, urban density, and age band. `0` uses the national median by age band, as main. State setup uses `1`. |
| `ECEC_INCOME_RANK` | `national`, `national_rpp` | `national` ranks nominal income, as main. `national_rpp` divides income by the state price level (BEA RPP). Unset, it follows `ECEC_URBAN_DENSITY` (`1` -> `national_rpp`, the state baseline; `0` -> `national`, as main). |
| `ECEC_STATE_ALPHA_PRICE` | `state` (default), `national` | `state` rebuilds each state's alpha at the state's own 2019 supply prices before re-anchoring, and checks the base-year identity and sector hours. `national` keeps the alpha at national prices. National runs do not read it. |

Each calibration stage records both settings, and a later stage stops if its setting differs from the interface's.

### Price Wedge

Each parent unit with a child under 5 gets one price multiplier for each paid care type. The multiplier is a quantile of a quantile regression forest (`ranger`, 500 trees) trained on NSECE provider rows. The forest target is the household payment divided by the base price. Its features are income percentile, Census region, number of parents, and mean age of the children under 5. ACS parent units rank baseline AGI, and NSECE ranks household income, both under `ECEC_INCOME_RANK`. One percentile from 1 to 99 is drawn for each parent unit and shared across care types. The multiplier is clipped to [0.1, 5]. 1a writes the diagnostics to `estimation/models/`.

Year preparation checks finite positive wedges against the cache by household and parent-unit keys. Constant wedges are valid in small samples. Missing or duplicate cache keys and changed joined values stop the run. Check this guard with `Rscript tests/test_price_wedge_integrity.R`.

### Center Price Tiers

A paid center is high-priced at or above a tier line. Centers use their posted price and families use their hourly payment. With `ECEC_URBAN_DENSITY=0` the line is the national median posted price for the age band, weighted by survey weight times FTE enrollment. With `ECEC_URBAN_DENSITY=1` it is the line for the region, community urban density, and age band (`src/shared_functions/urban_density.R`). 1a writes the line to `estimation/data/nsece_tier_thresholds_2019.csv`.

### Recalibration

State demand targets, supply files, employment paths, simulation code, policies, and runscripts take effect at simulation start. Reuse an interface with `-C`. A new ACS pull, a new `--calib-sample`, processing or calibration code, or new national supply parameters need a new interface (`--setup-only`). After one, update the state inputs in order:

```bash
# 1. p0 summaries on the new interface (cluster); replace outputs in resources/acs and state_data/out
Rscript state_data/cluster/acs_p0_shares.R <interface> <out.csv>
# 2. NSECE constants, if NSECE processing changed; copy them into 00_ and 05_
Rscript state_data/code/02_derive_nsece_constants.R
# 3. Demand targets
Rscript state_data/code/00_build_demand_targets.R --all --check
# 4. Copy estimation/supply/supply_2019.yaml over resources/supply/US_NSECE__national_supply_parameters__2019.yaml, then
Rscript resources/supply/00_update_state_supply.R
# 5. Tests
Rscript tests/test_state_analysis.R
Rscript tests/test_state_staffing_inputs.R
```

A fresh clone also needs `cache/price_models/price_qrf_models.rds`, which is not in git. Copy it from an existing checkout or run 1a.

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

## State supply input updates

`resources/supply/00_update_state_supply.R` rebuilds state supply files using reviewed October 2019 staffing rules and annual wage ratios. National runs retain their existing supply inputs.

1. Validate staffing source hashes: `python state_data/code/analyze_licensing_2019.py`.
2. Acquire annual BEA/BLS data: `python resources/supply/acquire_annual_supply_data.py` (requires `lxml`). Store OEWS all-data ZIPs in `resources/bls_oews/` and BEA archives in `resources/bea/`. Download URLs and hashes are recorded in `resources/supply/annual_supply_sources.json`.
3. Rebuild supply: `Rscript resources/supply/00_update_state_supply.R`. While annual BLS files are pending, explicitly pass `--skip-annual-wages` to retain 2019 wage anchors.
4. Regenerate care-mix weights from the calibration interface, with `ECEC_URBAN_DENSITY=1`, `ECEC_INCOME_RANK=national_rpp`. The price comparison requires a `.provenance.txt` sidecar beside its p0-by-age input.
5. Run `Rscript state_data/code/16_ndcp_model_prices.R`; run `Rscript state_data/code/17_historical_price_validation.R <macro_dir>` on a compute node for historical price validation. Historical comparisons hold 2019 county population and care-mix weights fixed.

Annual wage paths multiply each education group's 2019 wage by its local-to-national ratio change, in addition to national wage growth. The last observed ratio is carried forward; missing interior years stop the run. Annual all-items RPPs are available through `get_regional_price_parity()` for survey-year matching; the 2019 training ranks remain fixed. Non-labor costs retain fixed 2019 geographic housing factors and CPI growth over time. Staffing evidence and source status are recorded in `state_data/docs/SOURCES.csv` and the detailed licensing audit registry.

## Requirements

- R 4.0+
- Key packages: data.table, tidyverse, yaml, ranger, openxlsx, sampleSelection, Hmisc

State input diagnostics: `Rscript state_data/code/analyze_annual_wages.R` checks saved annual wage paths; `Rscript state_data/code/analyze_national_care_totals.R` writes six care-type gaps against the price-adjusted national calibration, excluding NYC. Historical price validation uses fixed 2019 geographic housing factors with CPI growth.

State-input validation (22 October 2026): the updated build contains all 28
numerical staffing corrections; all 312 geography/setting/age records are reviewed.
The 52-geography, 1% ACS sample for 2026--2027 completed successfully (job
`30293994`, output `202610221441`). All 104 price solves and employment targets
pass: maximum paid-sector gap $0.000319/child-hour and maximum employment miss
0.0002545 percentage points. Local source, supply, and annual wage checks pass.
The refreshed 2019 NDCP comparison covers 46 states (weekly RMSE $34.73,
correlation 0.892). The strict NY/NYC hours-additivity diagnostic still fails;
the methodology appendix explains the care-mix difference. Adaptive employment-shift
bounds are covered by `Rscript tests/test_employment_shift_bounds.R`.
