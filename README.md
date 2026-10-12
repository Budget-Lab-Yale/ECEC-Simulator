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
└── supply/                   # Calibrated supply parameters (timestamped)

tests/                        # testthat test suite
slurm/                        # SLURM parallel execution scripts
```

## State-level demand inputs

`state_data/` builds the state demand targets the simulator re-anchors to:
one six-row annual-hours file per geography. It builds 52: the 50 states, DC,
and New York City. Puerto Rico is not built: it has no NSCH rate and no NSECE
region, and the simulator's ACS households do not include it.
Unpaid Center-Based comes from each state's program counts (public pre-K, Head
Start, and Early Head Start), less the Head Start that CCD already counts. The
other five types divide the rest by the simulator's own base-year choice
probabilities, summed over the state's ACS households
(`state_data/code/05_compare_nsece_shares.R` compares the result with regional
NSECE shares).

- The **state method memos** (`state_data/docs/<date>_summary.pdf`, one per
  update, newest first by name) are the documents to read — what every number means and how solid it is. Finished memos are in
  `state_data/docs/archive/`; the memo in progress is kept outside git.
- [state_data/README.md](state_data/README.md) is the map: how to build, the
  provenance of every input, the five checks that each exit non-zero on
  failure, and the known traps.

The build and the simulator are wired together.
`state_data/code/00_build_demand_targets.R` writes each file once, to
`config/state/demand/<GEO>.csv`, and the simulator reads it from there (`adjust_alpha_for_state.R`). The simulator takes
six target groups: one Center-Based target covers both center tiers, and
the model determines the split between them.
A runscript can name `NYC` beside the states: the simulator selects its
households by STATEFIP 36 and the five borough county FIPS
(`SUBSTATE_GEOGRAPHIES` in `constants.R`, read from
`config/substate_geographies.csv`). That needs an interface built from
a raw ACS pull that carries `COUNTYFIP`. `config/runscripts/state_targets_smoke.csv` runs the
baseline for KY, NJ, NY, and VA; `state_targets_all.csv` runs it for all 50
states, DC, and NYC. A state run needs an interface built after
state support was added, because the alpha files must carry `p0` and child
weights.

## Model Flow

1. **Processing** - Process NSECE (supply-side) and ACS (demand-side) microdata. Imputes earnings (Heckman selection), childcare enrollment and prices (random forest), employment (random forest), taxes (percentile-based donor matching), and SPM poverty units. Tax units use the Census Bureau's corrected April 2024 ACS-SPM `Tax_unit` identifiers (joined via a crosswalk; the legacy IPUMS `TAXID` splits nearly all married couples into separate tax units), with unit AGI reconstructed from deduplicated legacy fragments (`src/shared_functions/reconstruct_tax_unit_agi.R`; see `docs/ecec_team_taxid_memo_2026-08-17.md`).

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

Setting the `state` runscript column runs the simulation per state instead of
nationally (see `docs/state_level_analysis.md` for the full design):

- Processing and calibration run unchanged at the national level; households
  are filtered to the state at simulation initialization (before any
  `--sim-sample` subsampling). ACS processing reweights children 0-4 to the
  Census single-year age mix (`resources/acs/US_PEP__national_population_by_single_year_of_age_0_4__2019.csv`).
- **Demand**: if `config/state/demand/<ST>.csv` exists (base-year 2019 annual
  hours by care type, 7 non-parental types), the calibrated alpha matrices
  are re-anchored via a per-care-type contraction so state base-year
  aggregates match; otherwise the state uses national alphas, filter only.
- **Supply**: `config/state/supply/<ST>.yaml` (same schema as
  `estimation/supply/supply_2019.yaml`) replaces the NSECE-derived parameters
  when present. `resources/supply/state_supply_params.R` builds it: state
  wages (OES May 2019 childcare occupation wages, mixed at national ACS
  education weights), staffing scaled by state licensing
  ratios; the non-labor residual and elasticities stay national.
- **Employment growth**: `config/state/employment/<ST>.csv` (the 16-59 cells
  of `resources/epop/epop_projections.csv`) replaces the national CBO epop
  projections when present (`resources/epop/state_epop_projections.R`: each
  geography's own ACS rates relative to the nation, applied to national CBO
  rates); base-year employment rates always come from the state-filtered
  data, weighted by 2019 caregiver weights.
- Multiple states run as independent equilibria (a within-year loop); all
  summary tibbles gain a `state` column and per-state output is written under
  `simulation/{scenario_id}/states/<ST>/`. Run provenance (deltas, supply and
  epop sources) is recorded in `metadata/state_analysis.yaml`.

State runs require an interface generated after state support was added
(`STATEFIP` in the ACS pull and `p0`/weights stored with the alpha matrices);
older interfaces fail loudly at initialization. Known v1 limitations (state
income taxes, national wage/CPI growth factors) are documented in the design
doc.

### When to recalibrate

Most changes take effect at simulation start. These do **not** need a new
calibration; reuse one with `-C <interface>`:

- **State demand targets** (`config/state/demand/`). The re-anchoring fits the
  national alphas to each state's targets at the start of every run.
- **State supply files** (`config/state/supply/`), such as wage ratios. The
  simulator also recomputes base-year labor from the model's own hours at the
  start of each run.
- **Employment paths** (`config/state/employment/`).
- **Simulation code** (`src/3_simulation/`), policies, and runscripts.

These **do** need a new calibration (`--setup-only` builds a new interface):

- **The household data**: a new ACS pull, a different `--calib-sample`, or new
  variables. Adding `COUNTYFIP` for NYC needed a new interface.
- **Processing or calibration code** (`src/1_processing/`,
  `src/2_calibration/`): NSECE processing, the choice set, the utility
  specification, or the calibration targets.
- **National supply parameters**, or anything estimated from NSECE.

**The share model reads the calibration.** The demand build's p0 shares
(`state_data/raw/US/US_ACS__p0_children_by_care_type_and_hours__2019.csv`) are
summed from one calibration interface (now `202610011031`). After a new
calibration, do these steps in order, or the demand targets keep the old
calibration's mix:

1. Run `state_data/cluster/acs_p0_shares.R <new interface> <out.csv>` on the
   cluster, and replace the raw file with its output.
2. Rebuild the demand targets: `Rscript state_data/code/00_build_demand_targets.R --all --check`.
3. Copy the new `estimation/supply/supply_2019.yaml` over
   `resources/supply/US_NSECE__national_supply_parameters__2019.yaml` -- the
   calibration re-estimates staffing and the non-labor residual for each
   center tier.
4. Rebuild the supply files: `Rscript resources/supply/state_supply_params.R`.

A fresh clone also needs `cache/price_models/price_qrf_models.rds`, which is
not in git and which `-C` does not copy. Copy it from an existing checkout
before the first run.

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
