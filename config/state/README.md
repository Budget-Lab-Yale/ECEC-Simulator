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

Base-year statewide annual hours by TARGET GROUP. All 6 groups are required
(`Parent Only` is the numeraire); hours must be positive.

```csv
ecec_type,annual_hours
Center-Based,123456789
Paid Home-Based,...
Unpaid Center-Based,...
Unpaid Home-Based,...
Other Paid,...
Other Unpaid,...
```

`Center-Based` covers BOTH `Low-Priced Center-Based` and `High-Priced
Center-Based`: the two bands share one solved adjustment, so the target
constrains their combined hours and leaves their modelled ratio alone. No
state publishes the price dispersion a split would need, so a file naming
either band is rejected.

When present, the calibrated alpha matrices are re-anchored so the state's
base-year aggregates match these targets (docs/state_level_analysis.md §5).
When absent, the state uses national alphas (filter only).

## supply/<ST>.yaml

Replaces the NSECE-derived national supply parameters for the state. Same
schema as `estimation/supply/supply_2019.yaml`: `labor_requirements`, `wages`,
`labor_supply`, `elasticities`, `per_unit_residual`. The override is
**all-or-nothing** — all five blocks are read, so a file supplying one of them
must supply the rest.

**These files are required, not optional, and they are built rather than
hand-written.** `resources/supply/state_supply_params.R` writes one per state
from the committed national parameters and the state's demand targets.

The reason is `labor_supply`. The equilibrium solves

```
w_market = w_0 * nominal_wage_factor * (L / L_0)^(1/e)
```

with `L = colSums(L_req * Qd)`. In a state run `Qd` is state-sized, because the
household filter leaves ACS weights alone. `L_0` comes from this parameter set —
so without a state file it is the **national** figure, and the curve is
evaluated at the state's *share* of national labour rather than at 1. A run of
KY, NJ, NY and VA on 2026-09-24 measured the consequence: the ratio came out
0.021 to 0.084, Kentucky's no-BA wage solved at $6.40 against a base of $12.06,
and the BA wage solved *below* the no-BA wage in every state, because its
elasticity is 2 rather than 4 and the same ratio bites twice as hard.

So each file carries the national parameters unchanged with one quantity
re-based to the state:

```
labor_supply = colSums(L_req * state base-year hours)
```

which is the definition `1a_nsece_processing.R` uses nationally, applied to the
state's own base-year hours.

**`wages` are state-specific too.** `resources/supply/state_childcare_wages.R`
measures hourly earnings of childcare workers by state and education from the
2018 and 2019 records of the 2015–2019 ACS 5-year sample (IPUMS extract `usa:33`, defined in
`resources/supply/acs_ipums_extract_2019.R`; the summary is written on the
cluster by `resources/supply/acs_ccwage_2019.py`), and the national wage is
scaled by each state's ratio to the national figure. Earnings are wage plus
business income, so self-employed providers — 16.8% of weighted childcare
staff — count. Every ratio carries a replicate-weight standard error
(largest 0.103, VT/no_ba). ACS puts the national level at $12.75 no-BA and
$19.49 BA against NSECE's $12.06 and $19.70; using a *ratio* rather than the
ACS level keeps NSECE's definition — which the rest of the parameter set is
built on — and cancels a difference common to every state. A cell under 50 ACS
workers takes ratio 1 and keeps the national wage; one cell is that thin
(VT/no_ba, 49 records).

`labor_requirements`, `elasticities` and `per_unit_residual` stay national.
`per_unit_residual` is the one worth stating: the equilibrium solves
`P_supply = unit_cost + delta_0 * cpi_factor`, so `delta_0` is a per-unit
residual in **dollars** covering everything that is not labour — rent, food,
insurance, administration, margin. State wages move `unit_cost`, and the
state's price moves with it by the full labour-cost difference while the
non-labour margin stays at its national dollar value. That is the conservative
choice: nothing measured here says the non-labour margin varies by state, and
rent plainly does not scale with childcare wages.

## employment/<ST>.csv

State epop projections used for the employment-rate growth factors. Same
schema as `resources/epop/epop_projections.csv` (columns `year`,
`epop_m_16_17`, ..., `epop_f_65plus`). Base-year employment *levels* always
come from the state-filtered data; only the growth path is taken from here.

**These files are built, not hand-written.** `resources/epop/state_epop_projections.R`
writes one per state from IPUMS ACS 2006–2024, and
`resources/epop/US_ACS__epop_by_state_sex_age__2006-2024.csv` is the
derivation it reads, written on the cluster by `resources/epop/acs_epop.py`. Eighteen states are present — the whole states the demand
build covers. Add one by extending `STATES` in that script and rerunning it.

Each file is the national path times the state's ACS epop over national ACS
epop, year by year, with the ratio held flat outside 2006–2024. It is a ratio
rather than a level because ACS runs 1 to 3 points below the CPS-based national
series for men and above it for women — a reference-period difference about the
survey, not the state — and that difference cancels in a ratio. The script's
header explains the choice in full.
