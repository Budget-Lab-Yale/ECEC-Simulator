# State-Level Analysis Inputs

Optional per-state input files for state-level runs (runscript `state` column).
All three are optional and independent; each falls back to the national input
when absent. `<ST>` is the USPS postal abbreviation, uppercase (e.g. `NY`).
Full design: `docs/state_level_analysis.md`.

```
state/
├── demand/<ST>.csv       # Base-year (2019) annual hours by care type
├── supply/<ST>.yaml      # Supply params (schema = estimation/supply/supply_2019.yaml)
└── employment/<ST>.csv   # Epop projections (schema = resources/epop/epop_projections.csv)
```

## demand/<ST>.csv

Base-year statewide annual hours by care type. All 7 non-parental care types
are required (`Parent Only` is the numeraire); hours must be positive.

```csv
ecec_type,annual_hours
High-Priced Center-Based,123456789
Low-Priced Center-Based,...
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

Replaces the NSECE-derived national supply parameters for the state. Same
schema as `estimation/supply/supply_2019.yaml`: `labor_requirements`, `wages`,
`labor_supply`, `elasticities`, `per_unit_residual`.

## employment/<ST>.csv

State epop projections used for the employment-rate growth factors. Same
schema as `resources/epop/epop_projections.csv` (columns `year`,
`epop_m_16_17`, ..., `epop_f_65plus`). Base-year employment *levels* always
come from the state-filtered data; only the growth path is taken from here.
