# State-Level Analysis Inputs

Optional per-state input files for state-level runs (runscript `state` column).
Missing inputs fall back to national inputs, except a supply override requires
a demand file: base-year labor is computed from re-anchored state demand.
`<ST>` is an uppercase USPS abbreviation or sub-state code (e.g. `NYC`).
Full design: `docs/state_level_analysis.md`.

```
state/
├── demand/<ST>.csv       # Base-year (2019) annual hours by care type
├── supply/<ST>.yaml      # Supply params (schema = estimation/supply/supply_2019.yaml)
└── employment/<ST>.csv   # Epop projections (schema = resources/epop/epop_projections.csv)
```

## demand/<ST>.csv

Base-year statewide annual hours by care type. Six rows are required: one
Center-Based row covers both price tiers, so the model sets the high/low split
(`Parent Only` is the numeraire; a row naming a price tier is rejected); hours
must be positive. Built by `state_data/code/00_build_demand_targets.R`, which
also writes `demand_counts/<ST>.csv` (children and part-time children behind
each target).

```csv
ecec_type,annual_hours
Center-Based,123456789
Paid Home-Based,...
Unpaid Center-Based,...
Unpaid Home-Based,...
Other Paid,...
Other Unpaid,...
```

When present, the calibrated alpha matrices are re-anchored so the state's
base-year aggregates match these targets (docs/state_level_analysis.md §5).
When absent, the state uses national alphas (filter only).

## supply/<ST>.yaml

Replaces national `labor_requirements`, `wages`, `elasticities`, and
`per_unit_residual`. State files omit `labor_supply`; initialization computes
it from state base-year sector hours. `wage_path` holds annual local-to-national
wage changes relative to 2019, with the last observed year carried forward.

`Rscript resources/supply/00_update_state_supply.R` validates source hashes,
refreshes the staffing audit, and rebuilds reviewed staffing, annual wages,
and supply YAMLs. Set `PYTHON` to a working Python executable if needed.
`Rscript resources/supply/state_supply_params.R --check` validates complete
payloads without writing. Staffing uses reviewed October 1, 2019 rules;
home inputs are licensed-capacity proxies, with assistant conditions retained
in `resources/licensing/licensing_2019_home_configurations.csv`.

## employment/<ST>.csv

State epop projections used for the employment-rate growth factors. Same
schema as `resources/epop/epop_projections.csv` (columns `year`,
`epop_m_16_17`, ..., `epop_f_65plus`). Base-year employment *levels* always
come from the state-filtered data; only the growth path is taken from here.
