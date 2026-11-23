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
| `--no-stanford-wedges` | | Drop the Stanford labor-cost multipliers, which are on by default (see Equilibrium below) |
| `--no-employment-targeting` | | Skip employment targeting in equilibrium solver |
| `--nsece-year` | | NSECE survey year to calibrate on: `2019` (default) or `2024`. Sets the paired ACS year, which is the model base year (see Calibration year below). Pass the same value when reusing a run with `-N`, `-A`, or `-C` |
| `--nsece-2024-options` | | Optional 2024 NSECE data treatments, comma-separated, or `all` (default: none, no effect with the 2019 survey. The missing-hours fill, formerly `hours`, is always on in both years, and that name is accepted and ignored): `weights` (child weights of children under 5 raked to the March 2024 CPS share of children by mother's work status and family type), `supply` (center staffing from directors' staff counts. Censored rates are filled in both years without an option, as crosswalk mismatch item 4 describes). See Calibration year below |
| `--calib-target1-name` | | First calibration target (default: `part_cost`) |
| `--calib-target1-value` | | Value for first calibration target |
| `--calib-target2-name` | | Second calibration target (default: `hours_income`) |
| `--calib-target2-value` | | Value for second calibration target |
| `--slurm-phase` | | SLURM job array phase: `setup`, `year`, or `finalize` |
| `--slurm-scratch` | | Path to scratch directory for inter-job communication |
| `--slurm-year-index` | | 1-based year index (maps to `$SLURM_ARRAY_TASK_ID`) |
| `--help` | `-h` | Show help and exit |

**Interface hierarchy**: Each level implies the ones below it. `-C` includes ACS and NSECE, `-A` includes NSECE, and `-N` stands alone.

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

1. **Processing** - Process NSECE (supply-side) and ACS (demand-side) microdata. Imputes earnings (Heckman selection), childcare enrollment and prices (random forest), employment (random forest), taxes (percentile-based donor matching), and SPM poverty units. Tax units use the Census Bureau's corrected April 2024 ACS-SPM `Tax_unit` identifiers (joined by a crosswalk, because the legacy IPUMS `TAXID` splits nearly all married couples into separate tax units), with unit AGI reconstructed from deduplicated legacy fragments (`src/shared_functions/reconstruct_tax_unit_agi.R`, described in `docs/ecec_team_taxid_memo_2026-08-17.md`).

   **NSECE survey year.** `run_nsece_processing(nsece_year)` in `src/1_processing/1a_nsece_processing.R` reads the 2019 survey by default and the 2024 public-use files (ICPSR 39466) with `--nsece-year 2024`. `get_nsece_year_settings()` holds each year's file names and the household-side low/high center price cut (the within-year weighted median hourly out-of-pocket price of paid center care: $4.75 in 2019, $6.86 in 2024). The `harmonize_nsece_2024_*()` functions recode the 2024 reserve codes, respondent-sex items, and negative rate codes to the 2019 conventions. The rules below apply in both years unless they say otherwise. `docs/nsece_2024/decisions.md` gives the reason and evidence for each, and "Mismatches between the two years" in `docs/nsece_2019_to_2024_crosswalk.md` gives the detail.

   - Administrative-data centers (public-school programs with no prices or workforce data) stay out of the price medians and the workforce ratios. The free ones join Unpaid Center-Based. The paying ones join Low- and High-Priced Center-Based. They are split by the FTE shares of interviewed paying public-school programs in each (71.9% Low in 2024, 85.1% in 2019), since the 2024 file withholds their price. Each uses its own total staff count, split by the BA share of interviewed public-school centers with the same payment status. The 2024 counts NORC imputed are refit by a random forest on log enrollment, funding sources, poverty density, and urban density, trained on the counts that come from the records.
   - The 2019 run puts its care-type coding on the 2024 rules (`recode_2019_center_coding()`). The 2024 survey's new item C17 moved some arrangements from other organizational care to center care. So 2019 children with only other organizational care move to center care. They are taken in random order within weekly-hours groups (matched to the hours mix of the 2024 arrangements C17 moved) until the weighted share with any center use rises by OPRE's factors (7.2% under age 3, 8.7% at ages 3 to 5).
   - Posted rates NORC censors at the 5th and 95th percentiles (deleted in 2019, set to the cap in 2024) are filled with the lognormal mean beyond the cut (`fill_censored_rates()`). The 2019 run reads the 2024 center file to fit which tail each censored rate is in. Posted weekly center rates above $2,000 are set to missing (unit errors).
   - A payment of don't know or refused is missing, not unpaid. 2019 centers that answered "don't know" to whether families pay get a missing price, not a free one, as in 2024.
   - Home-based enrollment and worker sums use the provider weight (`hb_weight`). They were unweighted, which made home care under 1% of `L_0`.
   - The paid home-based supply price is total paid divided by total hours (child weights), not the mean of each arrangement's hourly price. The mean rose 38% from 2019 to 2024 because of high hourly prices on short part-time arrangements.
   - Missing work hours: the survey blanks both parents' calendar work hours (code −10) when either has a missing calendar block. A parent with no hours who said yes to "worked last week" (D1a) gets the hours of their own adult calendar where the 2024 calendar file (`39466-0004`) has one (`parent_calendar_hours()`), else a donor of the same earner rank, child's weekly care hours, household income quintile, wage band, region, and urban density (`draw_donor_hours()`). One who said no gets zero. This was formerly the `hours` option.
   - Typical-week children (2024 households that answered for a typical week, `HH9_METH_QUEXVERSION` 2 or 3, instead of the weekly calendar) take a care category and hours class drawn from a probability forest fit on calendar children: age, poverty group, family type, the primary caregiver's "worked last week" answer, and region, with child weights. In the 2026-11-17 holdout test it came within 1.4 points of the held-out care-category shares, against 3.1 for the review's donor rule. Prices from the child's own provider rows do not change. The former option name `calendar_forest` is accepted and ignored.
   - Household size coded don't know (−1, 18 households in 2019) takes the household roster count.
   - Children whose household income ties with others get the middle income percentile the tie spans, not the lowest (`assign_income_pctile()`, used by the care forest, the price forest, and the price wedge cache).
   - The care forests use a grandparent-in-household flag (`gp`): NSECE `HHCOMP_RELATIONS_GPARENT`, and in the ACS a grandparent of any child 12 and under found from parent links and relationship to the householder (14% of ACS and 13% of NSECE children under 5 with a parent, weighted).
   - The 2024 typical-week care fill (1a) draws siblings together: a family ties at the same-care rate of calendar sibling pairs and otherwise draws each child separately.
   - In the ACS, a parent works if at work in the reference week, civilian or armed forces (`EMPSTATD` 10 or 14). A parent with a job but not at work counts as not working, as in the NSECE. The ACS keeps families with a parent under 18, as the NSECE training data do.

   The 2019 don't-know payment rule, the $2,000 rate cap, and the home-based weights change 2019 results compared with `main`.

   **Calibration year and ACS pairing.** The model is calibrated on one NSECE survey and the ACS year paired with it (`NSECE_ACS_YEAR_PAIRS` in `src/shared_functions/constants.R`): the 2019 NSECE with the 2019 ACS (default), or the 2024 NSECE with the 2024 ACS. The ACS year is the model base year: utility is measured in its dollars, growth factors and population aging start from it, Tax-Simulator donors and the OASI tax maximum are taken for it, and estimation files carry its suffix (`acs_*_<year>.csv`, `supply_<year>.yaml`, `demand_params_<year>.yaml`, `alpha_*_<year>.rds`). The NSECE household tables keep the survey year (`nsece_*_2024.csv`). When the survey year differs from the base year, 1a converts NSECE dollars to base-year dollars (care worker wages by the hourly wage index, the non-labor price residuals and the Other Paid price by CPI-U) and records the factors and the survey-year values in the supply YAML. The price-model cache is `cache/price_models/price_qrf_models.rds` for 2019 and `price_qrf_models_<survey year>.rds` otherwise.

   **ACS files and tax units.** 1b reads the ACS from the `ACS-<year>` dependency in `config/default_paths.yaml` when one exists (`ACS-2024`), else from `ACS` (the 2019 records). The 2023 ACS is not used as a stand-in. Tax units and AGI come from, in order:
   - an own tax-unit crosswalk, `tax_unit_crosswalk_<year>.csv`, beside `ipums_usa.csv` in that ACS folder, in any year (since 2026-11-16 this includes 2019). Columns: `serialno`, `sporder`, `tax_unit`, `agi` (Census serial number, person number, a numeric unit id unique across households, and the unit's AGI on every member). The file comes from the Affordability-Index tax units and AGI (`Budget-Lab-Yale/Affordability-Index`, full income chain, described in Open item 3 of `docs/nsece_2019_to_2024_crosswalk.md`). `Rscript src/misc/make_tax_unit_crosswalk.R <year> <ai_income_dir> <out.csv>` converts that repo's `data/interim/income/<year>` output: units from `tax_units_<year>.parquet`, and AGI from the `*_federal.parquet` files (primary returns only, mean over the implicates). Add `--check` to see whether those files are newer than the crosswalk.
   - else, for 2019, the corrected Census tax-unit crosswalk,
   - else the IPUMS `TAXID` and `ADJGINC` as they are. The 2024 file (IPUMS extract 36, `us2024a`) has neither, so 1b stops with a message naming them when no own crosswalk exists.

   **Optional 2024 treatments.** `--nsece-2024-options` treatments are all off by default and apply to the 2024 survey only. They are defined in `NSECE_2024_OPTIONS` in `constants.R` and applied in 1a.
   - `weights` multiplies the child weights of children under 5 by the factors in `config/nsece_reweight/nsece2024_child_weight_factors_cps2024.csv`. Those factors rake the 2024 NSECE to the March 2024 CPS shares of children under 5 in five groups (two parents with the mother at work or not, one parent at work or not, no parent). This lowers the share of young children with a working mother or single parent from 64.3% to 59.6%.
   - `supply` measures center staffing per FTE child from the director's count of teachers and aides for children 0-5 (BA share from the workforce file) instead of the workforce file's national totals.

   Each run rebuilds the price-model cache in `cache/price_models/`, so runs with different options must not share one code folder at the same time.

2. **Calibration** - Estimates CRRA demand parameters (beta, rho) by targeting empirical elasticities. Backs out household-level preference heterogeneity (alpha matrices) from observed/imputed choice probabilities.

3. **Simulation** - For each year: ages the population, applies macro projections, loads policies, and solves for market equilibrium. Post-equilibrium, expands with Gumbel epsilon shocks and collapses to discrete choices via argmax.

4. **Output** - Writes level summaries and delta files (scenario minus baseline), generates formatted Excel workbooks.

## Key Design Features

### Equilibrium

Solves for market-clearing prices across four care sectors (unpaid center-based, low-priced center-based, high-priced center-based, paid home-based) using L-BFGS-B with warm-start from the previous year. Demand is multinomial logit over choices of employment, care type, and care hours. Supply is constant-elasticity labor supply.

The base-year labor supply `L_0` is the provider-side workforce from the NSECE: `L_req` times each sector's child-hours (1a). Calibration does not read it. Labor demanded at survey prices is below it (81% without a BA and 73% with one in 2019), so the base-year simulation solves wages below the survey wage.

Labor cost uses the Stanford Childcare Model's employer cost multipliers (`STANFORD_WAGE_MULTIPLIERS` in `constants.R`): 1.25 times the wage at centers for fringe benefits, 1.1 for home-based helpers for payroll tax, and 0.8 for home-based owners as a self-employment discount. The home-based multiplier for each education group is the weighted owner and helper mix of listed providers (about 0.93 without a BA and 0.90 with one). The multipliers raise cost only, as in the Stanford model: unit labor cost is `(L_req * M) %*% w` (`get_labor_cost_matrix()`), where `M` is the 4 x 2 multiplier matrix saved as `labor_cost_multipliers` in the supply YAML. Staff ratios (`L_req`), labor demand, and `L_0` stay in worker-hours, and the percentage wage subsidy (`employer_tax_credit.yaml`) pays its rate on wages (`L_req %*% w`), not on benefits. Base-year prices do not change, and the non-labor residuals absorb the change. `--no-stanford-wedges` turns the multipliers off. Supply files written before 2026-11-23 have no `labor_cost_multipliers` and fold the multipliers into staffing, so rerun 1a rather than reuse them.

### Parent Unit Types

Families are classified into 2 types by number of children:
- **c1**: 1-child families (45 choice alternatives)
- **c2plus**: 2+ child families (675 choice alternatives)

Each choice combines: primary caregiver employment (FT/PT/NW) x child care arrangements (type x hours per child).

Children with no parent in the household (about 3.6% of ACS children under 5 in households) get a caregiver in 1b. The ACS names no caregiver. It gives each person's relationship to the householder and asks grandparents whether they are responsible for most of the basic needs of grandchildren in the household (IPUMS `GCRESPON`). The caregiver is a grandparent 18 or older who answers yes (the householder if among them, else the one listed first). Otherwise it is the householder, at any age. In the 3,249 ACS 2024 households where a grandparent reports being the caregiver, the householder is that caregiver 87.6% of the time, and data-based pickers (cell averages, a random forest, a conditional logit, donors) do no better in 5-fold cross-validation (85.3% to 88.1%). In the 2024 ACS, of children under 5 in households, 0.97% have a responsible grandparent and 2.67% take the householder. `GCRESPON` comes from `grandparent_caregivers_<year>.csv` (`SERIAL`, `PERNUM`, `GCRESPON`) beside `ipums_usa.csv`. Without that file, 1b uses the householder rule and prints a warning. A caregiver who is already a parent in the household takes the child into that parent unit. Otherwise the child forms a caregiver unit (`caregiver_unit = 1`). The caregiver is treated like a father: their work stays at what was observed, and only the children's care responds to prices and policy. Their care probabilities are the NSECE care distribution of no-parent children by whether the caregiver worked last week (`no_parent_care_dist` in 1b, `cg_works` from 1a: the guardian the NSECE names in `PRGCH`, else the respondent), not the care forest. The ACS caregiver's observed hours pick the distribution. Two children in a caregiver unit share the same care with probability `sibling_tie_rate`, the excess same-care rate of sibling pairs in NSECE calendar households with parents (0.63 in 2024), and otherwise draw from their own distribution. 1b first links an unlinked grandchild to a mother when exactly one woman grandchild of the householder 15 to 45 years older lives there, since the ACS records great-grandchildren as grandchildren and IPUMS does not link grandchild to grandchild. Caregiver units have zero weight in the calibration elasticity targets, take `pc_group = 'caregiver'` (no employment targeting), and are left out of parental employment results.

No child with no parent is dropped. Children in group quarters are already excluded.

The primary caregiver is the mother in a two-parent family. In a same-sex couple it is the parent with fewer weekly work hours, and on a tie the parent listed first (`get_primary_caregiver()` in 1b). In NSECE couples where both sexes are known, the parent who works less is the mother about 75% of the time.

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
| Transfers | `config/policy_transfer/` | `do_transfer_policy()` | Choice-dependent cash transfers (n_units x n_choices). They enter income, not care costs, with their own budget line |
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
| `policy_transfer` | Transfer policy file (optional, default `baseline` = none) |
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

The distributional impact tables (`aggregate_all_groups()` in `3d_accumulate_results.R`) break families out by income quintile (tax-filer and parent cutoffs), AGI bracket, marital status, number of children, and the primary caregiver's age, race and ethnicity, and education. Race and ethnicity: Hispanic first, then White, Black, Asian or Pacific Islander, and other or multiple (ACS `HISPAN`, `RACE`). Education: less than high school, high school, some college, bachelor's or more (ACS `EDUC`, with `EDUCD` moving 12th grade with no diploma to less than high school and less than one year of college to some college, so the groups match the NSECE within 2 points). These groups are for reporting only. The care-choice model does not use them.

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
  ACS: '/raw_data/ACS/v1/2023'       # Holds the 2019 records; any year without its own ACS-<year> key
  ACS-2024: '/raw_data/ACS/v1/2024'  # 2024 ACS (no TAXID, ADJGINC, or SPM yet)
  Census-ACS-SPM: '/raw_data/Census-ACS-SPM/v1/2019'  # Corrected tax-unit crosswalk
  Macro-Projections: '/model_data/Macro-Projections/v3/.../baseline'
  Tax-Simulator: '/model_data/Tax-Simulator/v1/.../baseline_ex_cdctc'
  Tax-Simulator-Full-MTR: '/model_data/Tax-Simulator/v1/.../baseline'  # Required for --fiscal-npv
  Tax-Data: '/model_data/Tax-Data/v1/.../baseline'
```

## Requirements

- R 4.0+
- Key packages: data.table, tidyverse, yaml, ranger, openxlsx, sampleSelection, Hmisc
