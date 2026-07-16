# Claude Code Configuration

## Documentation Maintenance

**IMPORTANT**: When implementing new features or making meaningful changes to the codebase, update the README.md to reflect those changes. This includes:
- New command-line arguments or configuration options
- New policy files or runscripts
- Changes to output structure or metrics
- Architectural changes or new modules
- Breaking changes or migration steps


## Coding Style Preferences

### R Code Style
- **Quotes**: Always use single quotes (`'`) instead of double quotes (`"`) for strings
- **Docstring format**: Use `(int)` instead of `(integer)` for integer parameters in function documentation
- **File structure**: Prefer modular code with helper functions in separate files
- **Comments**: Use `#` comments extensively for code documentation
- **Spacing**: Three lines separate function definitions
- **Assignment**: Use `<-` for assignment, not `=`
- **Piping**: Use `%>%` from magrittr/dplyr for data transformations

### General Preferences
- **Approach**: Prefer editing existing files over creating new ones unless explicitly required
- **Directory assumptions**: Assume output directories exist by definition
- **Console output**: Use informative `cat()` statements to show progress
- **Error handling**: Provide clear error messages with next steps


## Project Structure

The model flow is linear and can be traced through numbered scripts:

```
src/
├── main.R                              # Entry point + interface copy helpers
├── misc/
│   └── config.R                        # CLI parsing, interface hierarchy, scenario config
├── 1_processing/                       # NSECE + ACS data processing
│   ├── 1a_nsece_processing.R          # run_nsece_processing() + all NSECE helpers
│   └── 1b_acs_processing.R            # run_acs_processing() + ACS/earnings/ECEC/labor/SPM helpers
├── 2_calibration/                      # Model parameter estimation
│   └── 2a_calibration.R               # run_calibration() + calibration data + demand estimation
├── 3_simulation/                       # Equilibrium solver & choice simulation
│   ├── 3a_initialize_simulation.R     # initialize_simulation() (load data, params, price cache)
│   ├── 3b_run_simulation_year.R       # run_simulation_year() (age data, prepare PUs, taxes)
│   ├── 3c_run_scenario.R             # run_scenario() (equilibrium, Gumbel, discrete choices)
│   ├── 3d_accumulate_results.R        # accumulate_year_results() (all output aggregation)
│   └── slurm.R                        # SLURM parallel execution handlers
├── 4_output/
│   └── 4a_finalize_output.R          # finalize_simulation() (summaries, deltas, Excel)
└── shared_functions/                   # Mostly one function per file (multi-caller utilities)
    ├── constants.R                    # Global constants (care hours, tax rates, optimizer, parent unit types)
    ├── get_choice_catalog.R           # Choice set definitions + catalog cache
    ├── get_working_choice_mask.R      # Working choice mask for employment targeting
    ├── get_sector_hours_matrices.R    # Choice-to-sector-hours mapping matrices (cached)
    ├── compute_probs_from_alpha.R    # Choice probability computation from alpha matrices
    ├── compute_policy_components.R   # Policy component computation (offer-variant aware)
    ├── compute_policy_utility_matrix.R # Policy pipeline -> NI -> V (single V-construction path)
    ├── compute_utility_from_net_income.R # CRRA utility + 2019 deflation + employment shifts
    ├── match_alpha_to_parent_units.R  # Alpha matrix row alignment by household keys
    ├── rationing.R                    # Quantity-limited program machinery (offer states,
    │                                  #   nested offer-rate solve, lottery collapse, Option A helpers)
    ├── compute_target_employment_rates.R        # Employment targeting targets (shared 3b + slurm)
    ├── write_employment_targeting_diagnostics.R # Targeting diagnostics CSV (shared 3b + slurm)
    ├── prepare_donor_pool.R           # Tax donor pool preparation + AGI binning helpers
    ├── compute_price_wedge_cache.R   # Price wedge cache computation
    ├── load_policy.R                  # Base policy loader (isolated environments)
    ├── load_cdctc_policy.R            # CDCTC policy loader
    ├── load_demand_policy.R           # Demand policy loader (returns policy object w/ rationing)
    ├── load_supply_policy.R           # Supply policy loader
    ├── add_simulation_variables.R     # Simulation context column management
    ├── prepare_year_context.R         # Year-level data preparation (shared by 3b + slurm)
    ├── filter_sim_base_hh.R           # Household filter/subsample cascade (shared by 3a paths)
    ├── adjust_alpha_for_state.R       # State demand re-anchoring contraction (state-level runs)
    ├── get_hourly_wage_growth_factor.R          # Hourly wage growth projection
    ├── get_employment_rate_growth_factor.R      # Employment rate growth projection (epop_path override for state runs)
    └── get_cpi_growth_factor.R                  # CPI growth projection

config/
├── default_paths.yaml        # File system paths & data dependencies
├── runscripts/               # Scenario definitions (CSV files)
├── policy_demand/            # Demand-side subsidy policies
├── policy_supply/            # Supply-side policies
├── cdctc/                    # Child & Dependent Care Tax Credit policies
├── policy_tax/               # Tax policies (CTC-like credits)
├── employer_subsidy/         # Employer wage subsidy configs (YAML)
├── wage_floor/               # Wage floor configs (YAML)
├── state/                    # State-level analysis inputs (demand/, supply/, employment/ per <ST>)
├── demand/                   # Calibrated demand parameters (timestamped)
└── supply/                   # Calibrated supply parameters (timestamped)

tests/                        # testthat test suite
cache/                        # Cached donor pools & ETRs
docs/                         # Documentation & memos
```


## Key Architectural Patterns

### State-Level Analysis
Optional runscript `state` column (USPS postal codes, semicolon-separated) runs
the simulation per state: processing/calibration stay national; simulation init
builds per-state contexts (household filter, alpha re-anchoring via
`adjust_alpha_for_state()` when `config/state/demand/<ST>.csv` exists, supply
yaml / epop CSV overrides); years loop over states; summaries carry a `state`
column and finalize writes per-state output under
`simulation/{scenario}/states/<ST>/`. See `docs/state_level_analysis.md`.

### Parent Unit Classification
Parent units are split 2 ways by number of children:
- `c1`: 1-child families (45 choices)
- `c2plus`: 2+ child families (675 choices, uses 2-child choice set)

### Policy Functions
Policies are pluggable R functions. See existing files for full signatures:
- Demand: `config/policy_demand/*.R` — `do_demand_policy()` returns subsidy matrix (n_units x n_choices).
  `load_demand_policy()` returns a policy OBJECT: `list(do_demand_policy, rationing, is_child_program_eligible)`.
  Quantity-limited (rationed) policies define `rationing$slots` (by sector, or function(year)),
  `is_child_program_eligible()`, and offer-conditional `do_demand_policy(..., offered1, offered2)`;
  see `ny_2k_style.R` and README. Unrationed policies need no changes (auto-wrapped).
- Supply: `config/policy_supply/*.R` — `do_supply_policy(P)` returns supplier-received prices
- CDCTC: `config/cdctc/*.R` — `do_cdctc_policy()` returns CDCTC credit matrix
- Tax: `config/policy_tax/*.R` — `do_tax_policy()` modifies total_tax columns by employment status
- Employer subsidy: `config/employer_subsidy/*.yaml` — percentage or dollar wage subsidies

### Equilibrium Solving
- Uses L-BFGS-B optimizer with softmax demand (smooth for convergence)
- After equilibrium: expand with Gumbel epsilon shocks, collapse to discrete choices
- Warm-start: previous year's prices used as initial guess
- Rationed policies: sector offer rates are solved NESTED inside each price
  evaluation (projected Gauss-Seidel on precomputed per-offer-state aggregates;
  r = 1 corner with slack when undersubscribed). Offers are per eligible child
  per rationed sector; the lottery is drawn in the discrete stage from a
  dedicated RNG stream (`year_seed + 100 + i`) so epsilon draws stay aligned
  across scenarios. Mechanical effects use Option A (baseline records inherit
  the policy run's offer draws) — see `docs/mechanical_effect_rationed_programs.html`

### Tax Integration
- Percentile-based AGI donor matching from Tax-Simulator output
- Donor pools cached in `cache/donor_pools/`
- EMTR (effective marginal tax rates) used for employment choice taxation


## Code Organization Guidelines

- Keep functions focused and single-purpose
- Use clear, descriptive variable names
- Separate concerns (I/O, processing, configuration)
- Include comprehensive function documentation with:
  - Description of purpose
  - Params section with types and descriptions
  - Returns section describing output
- Load policies via `load_supply_policy()` / `load_demand_policy()` (isolated environments)
- Avoid global state; pass data explicitly through function parameters


## Testing

- Unit tests in `tests/` directory (testthat framework)
- Run all tests: `Rscript tests/run_tests.R`
- Diagnostic scripts prefixed with `analyze_`, `compare_`, `debug_`


## Common Tasks

### Adding a New Policy
1. Create `config/policy_demand/my_policy.R` with `do_demand_policy()` function
2. Add scenario to a runscript CSV in `config/runscripts/`
3. Update README.md if the policy represents a significant addition

### Running the Simulation
```bash
Rscript src/main.R --runscript speed_test
Rscript src/main.R -r speed_test -C 202601151200
```

### Minimal Testing (Quick Verification)
When doing a quick smoke test to verify code changes, use a 1% sample:
```bash
# Run with 1% ACS sample (rebuilds donor pools fresh)
Rscript src/main.R -r speed_test -a 1
```
This runs the full speed_test runscript with 1% of the ACS sample for fast verification.

**IMPORTANT**: Never use `-d` (cached donors) or `-c` (cached ETRs) flags during testing after code changes. Cached files may be stale and cause confusing errors that have nothing to do with your changes. Always rebuild fresh during verification testing.

### Debugging Equilibrium Issues
- Check `models/equilibrium/` output for solver diagnostics
- Verify convergence: both optimizer code=0 AND residual gap < threshold
- Use `tests/` scripts for isolated component testing
