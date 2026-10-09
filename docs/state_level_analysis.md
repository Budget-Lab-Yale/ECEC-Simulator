# State-Level Analysis: Design

*Status: agreed design, v1. Author: HE + Claude, July 2026. Updated September
2026: 50 states and DC, built supply and employment files.*

This document specifies the design for running the ECEC Simulator at the
state level rather than (only) the national level. It records the decisions
made in design discussion and the mechanics the implementation follows.

## 1. Motivation and constraints

We want to analyze state-specific childcare policies (e.g. a New York
program) on state-representative data. Two constraints shape the design:

- **NSECE is nationally representative, not state-representative.** Supply
  parameters derived from it cannot simply be filtered to a state.
- **ACS imputed choice probabilities are calibrated nationally.** Filtering
  households to a state keeps national *preferences*; the state's actual
  care mix will generally differ.

The design therefore runs processing and calibration **unchanged at the
national level**, then restricts and re-anchors the data per state at
simulation time, using optional state-specific input files where we have
them.

## 2. Runscript interface

A new optional runscript column: `state`.

| Value | Behavior |
|---|---|
| column absent, blank, or `all` | National run — exact current behavior, bit-identical output |
| `NY` | Single-state run for New York |
| `NY;CA;TX` | Multiple states — independent per-state runs (see §6) |
| `NYC` | New York City — the five boroughs, a sub-state geography |

- Format: **USPS postal abbreviations**, semicolon-separated (CSV-safe).
  Crosswalked in code to FIPS codes (IPUMS `STATEFIP` is numeric FIPS) via a
  51-entry table (50 states + DC) in `constants.R`.
- **Sub-state geographies.** `SUBSTATE_GEOGRAPHIES` in `constants.R` names
  each one with its state FIPS and IPUMS county codes. It reads them from
  `config/substate_geographies.csv`, which the ACS Python scripts also read. NYC is STATEFIP 36 with
  COUNTYFIP 005 (Bronx), 047 (Kings), 061 (New York), 081 (Queens), and 085
  (Richmond). Every NYC PUMA nests in one borough, so IPUMS identifies all
  five. `select_geo_hh_ids()` applies the county filter. A sub-state
  geography runs as its own independent market, like a state, and can run
  beside its state (`NY;NYC`).
- The value must be identical on every row of the runscript (it is a
  run-level setting, validated at parse time).
- Unknown abbreviation, or a state with zero households after filtering,
  fails loudly at initialization.

## 3. Data requirements

- **`STATEFIP` must be present in the raw ACS pull** and is threaded through
  ACS processing into the simulation base tables (`households` gains a
  `statefip` column). Interfaces built before this feature do not contain
  it, so **state runs require a freshly generated interface** (`-N/-A/-C`
  interfaces predating this feature cannot be used for state runs; national
  runs remain compatible with old interfaces).
- **A sub-state run also needs `COUNTYFIP`** in the raw ACS pull, which ACS
  processing carries into `households` as `countyfip`. A NYC run on an
  interface without it stops at initialization; it never falls back to the
  whole state.
- **Calibration additionally stores the base-year choice-probability
  matrices (`p0`)** alongside the alpha matrices (same `.rds`, same
  `row_ids` keys). The state demand adjustment (§5) needs `p0`; recomputing
  it at simulation time would require rebuilding base-year systematic
  utility, which we avoid.

## 4. Architecture

```
processing (national)  →  calibration (national)
        →  simulation init: build one context per state
              - filter households to state (cascade to all tables)
              - recompute alphas to hit state demand targets   (§5)
              - override supply params if state yaml present    (§7)
              - resolve state epop file if present              (§8)
              - take the state's rows of the price wedge cache
                (income percentiles rank within each state, in
                calibration and simulation alike; the price model
                trains on NSECE incomes ranked within Census region,
                the finest NSECE geography, without region as a
                predictor -- the state market price carries the
                geographic price level)
        →  each simulation year: loop over states               (§6)
              - per-state equilibrium (baseline + counterfactuals)
              - per-state warm-start prices
              - summaries stamped with a state column
        →  finalize: per-state summaries and deltas
```

The base year is **2019** (ACS 2019, NSECE 2019). All state target files are
expressed in base-year terms.

## 5. Demand-side adjustment: recomputing alphas

### Why alphas

Calibration defines, per household `i` and choice `j`:

```
alpha_ij = log(p0_ij) − V_ij        (normalized within row)
```

so that simulated base-year probabilities `softmax(V + alpha)` reproduce
`p0` exactly. Alpha is the model's base-year anchor and is carried into
**every simulation year and both baseline and counterfactual**. Adjusting
alpha (rather than post-hoc probabilities) makes the state adjustment
structural: baseline and counterfactual move together and deltas stay
meaningful. Because alpha is pinned at base-year prices, the adjustment
also decouples cleanly from the simulation-year price equilibrium.

### Target file

`config/state/demand/<ST>.csv`, base-year (2019) **annual hours** by TARGET
GROUP, for the state:

```csv
ecec_type,annual_hours
Center-Based,123456789
Paid Home-Based,...
Unpaid Center-Based,...
Unpaid Home-Based,...
Other Paid,...
Other Unpaid,...
```

- A target names a GROUP, not a care type. `STATE_TARGET_GROUPS` maps the 6
  groups onto the 7 non-parental `ecec_type` values from `CHILD_CARE_CHOICES`;
  **`Parent Only` is the numeraire** (its delta is 0, and its "hours" are
  residual by construction).
- **Every group is a singleton except `Center-Based`**, which covers both
  `Low-Priced Center-Based` and `High-Priced Center-Based`. The tiers fall
  below or at/above the provider-file median posted price for the Census region
  and age band (`1a_nsece_processing.R`); the public-use NSECE files do not
  identify state, and no state source counts the tiers. The pair shares ONE
  delta: it calibrates total paid-center hours with no tier-share target. A
  common shift keeps each family's odds between the tiers, but the aggregate
  split can still move as families reweight, so the model determines it.
- All 6 rows are required. A file naming `Low-Priced Center-Based` or
  `High-Priced Center-Based` is REJECTED -- it has invented a split.
- **If the file is absent**: no adjustment — the state run uses filtered
  households with national alphas.

### Mechanics

We solve for one constant `δ_t` per care type `t`, common to all
households in the state, such that aggregate base-year annual hours by care
type match the CSV. For a choice `j` that places child `c` in care type
`t_c(j)`:

```
p_ij(δ) ∝ p0_ij · exp( Σ_c δ_{t_c(j)} )          δ_ParentOnly ≡ 0
H_t(δ)  = Σ_i Σ_j p_ij(δ) · Σ_c w_ic · h_c(j) · 1{t_c(j) = t}
```

where `w_ic` are child weights and `h_c(j)` annual hours (`HOURS_ANNUAL`).
Note the per-child additivity: a 2-child choice contributes both children's
deltas to the utility shift and both children's hours to the aggregates.

Solved by the standard contraction:

```
δ_t ← δ_t + log( H_t^target / H_t(δ) )
```

iterated to convergence (`max |log(H_target/H_model)| < tol`). With one
delta per targeted type and one target per type, the system is exactly
identified.

The stored (reference-normalized) alpha matrix is then updated as

```
alpha_j += d_j − d_1     where d_j = Σ_c δ_{t_c(j)}
```

(the reference-choice shift `d_1` keeps the stored normalization; choice
probabilities are invariant to it).

### Failure modes — fail loudly

- A care type with **zero base support** in the state (`H_t(0) = 0`) but a
  positive target: no finite delta can reach it. Stop with a clear error
  (this almost always means the imputation or the CSV is wrong).
- A **zero target** for a type with positive support: also an error (would
  require `δ → −∞`; set a small positive target instead if intended).
- Non-convergence after the iteration cap: error with the worst offending
  type and its log-gap.

### Interaction with employment targeting

Choices bundle care and employment, so re-anchoring the care mix shifts
implied base-year employment. This is handled naturally because
`baseline_rates_2019` for employment targeting are computed **from the
state-filtered parent-unit data with the adjusted alphas' RF-implied
probabilities** — i.e., the state's own base rates, not national ones. The
two targeting layers then operate at different levels (care mix: base-year
anchor; employment: simulation-year growth path) and do not fight.

## 6. Multiple states: independent runs via a within-year loop

Each state is its own childcare market: own equilibrium prices, own labor
market. States are therefore simulated **independently** — never pooled
into one equilibrium.

Implementation: simulation initialization builds a **list of per-state
contexts**; each simulation year loops over states and runs the full
existing per-year body (baseline equilibrium, counterfactuals,
accumulation) per state. This keeps the SLURM three-phase chain intact
(years remain the parallel dimension; each year job loops states
internally).

Consequences threaded through the code:

- **Warm-start prices** are keyed by state (each state carries its own
  previous-year baseline and counterfactual price vectors).
- **Employment-targeting base-rate cache** is keyed by state.
- **Accumulated summary tibbles** gain a `state` column (stamped at
  accumulation). National runs add no column and produce bit-identical
  output to the current code.
- **Finalize** groups by state where the column is present; per-state
  results are written under per-state totals.

## 7. Supply side

`config/state/supply/<ST>.yaml` replaces the NSECE-derived national supply
parameters for the state. Same schema as
`<output_root>/estimation/supply/supply_2019.yaml`, which calibration writes
(committed copy: `resources/supply/US_NSECE__national_supply_parameters__2019.yaml`):
`labor_requirements`, `wages`, `labor_supply`, `elasticities`,
`per_unit_residual`. The override is **all-or-nothing** — the simulator reads
every block from the state file. The one exception is `labor_supply`: the state
file does not hold it, and the simulator sets it from the state's base-year
hours (below).

**A state with a demand file needs its supply file, and the files are built
rather than hand-written.** `resources/supply/state_supply_params.R` writes one
per state from the committed national parameters and the state's demand
targets.

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

So a state run re-bases `L_0` to the state. **The simulator computes it; the
supply file does not hold it.** At state initialisation
(`3a_initialize_simulation.R`), `adjust_alpha_for_state()` returns the model's
own base-year hours by supply sector at the solved deltas, and the simulator
sets

```
L_0 = colSums(L_req * sector_hours)
```

This is the definition `1a_nsece_processing.R` uses nationally, applied to the
state's own base-year hours. The two center bands differ sharply in BA
labour per hour (0.035 against 0.076), so `L_0` must use the same band hours
the contraction reaches. From the model's own hours, `L / L_0 = 1` in the
base year by construction.

This is the only state definition of `L_0`, so a state supply file needs a
demand file. A state run with `config/state/supply/<ST>.yaml` but no
`config/state/demand/<ST>.csv` stops with an error that names the missing file
and `state_data/code/00_build_demand_targets.R`. A state with neither file runs
on the national parameters. Until 5 October 2026 the supply file also held an
ACS-based `labor_supply` (national L_0 × the state's share of national ACS
childcare workers) as a fallback. No run used it, because every geography has
a demand file, and the build no longer computes it.

**`wages` come from BLS OES May 2019 childcare occupations.**
`resources/supply/oes_childcare_wage_ratios_2019.R` reads the OES state,
national, and MSA files (extract in
`state_data/raw/US/US_OES__childcare_occupation_wages__2019.csv`). OES is an
employer survey with SOC-coded occupations, so its samples are larger and its
occupation codes cleaner than the ACS self-reports. OES has no education, so
each education group mixes the occupation wages at national ACS weights:

```
ratio(g, s) = sum_o w(o, g) h(o, s) / sum_o w(o, g) h(o, US)
```

`h` is the OES hourly median and `w(o, g)` the national share of group `g`'s
childcare workers in occupation `o` (ACS 2015-2019 five-year, all five years,
person weights; `resources/supply/acs_childcare_occupation_weights_2019.py`,
output `resources/supply/childcare_occupation_weights_by_education_acs_2019.csv`;
same worker definition as the former ACS wage ratios):

| SOC | Occupation | no-BA | BA |
|---|---|---|---|
| 39-9011 | Childcare workers | 0.726 | 0.426 |
| 25-2011 | Preschool teachers, except special education | 0.241 | 0.454 |
| 25-2051 | Special education teachers, preschool | 0.000 | 0.004 |
| 11-9031 | Education and childcare administrators, preschool and daycare | 0.032 | 0.116 |

OES publishes no hourly median for 25-2051 (annual pay only), so its weight
folds into 25-2011. The weights stay national, so a state's ratio moves with
its OES wages only, not with small state education samples. An occupation
with a suppressed state wage drops from both sums for that state, and the
other weights renormalize: DC has no 11-9031 wage. A group with no occupation
left would take the state's all-occupations median ratio; none does. Ratios
run 0.764 (AL) to 1.298 (DC) for no-BA and 0.746 (AL) to 1.278 (DC) for BA.
OES 2019 has no metropolitan divisions, so NYC takes the New
York-Newark-Jersey City MSA (35620), suburbs included.

`labor_requirements` scale by a state licensing staffing index. For
centres: required staff per child (1 ÷ licensed children per staff by age),
averaged over ages 0–4 with the NSECE 2019 centre enrolment mix, relative to
the PEP-weighted mean over the 50 states and DC; it scales all three centre
sectors. For homes: 1 ÷ licensed maximum children per family child care home,
relative to the mean over states that license homes (1 where a state does
not); it scales `paid_home_based`. Rules: HHS ACF 2014 licensing study (CAP
2018 Table 1; PN3 2020 Table 6); NYC centres use the city Health Code
(Article 47, June 2019). Centre index runs 0.731 (LA) to 1.499 (DC); home
index 0.425 (AR) to 1.698 (DE).

`per_unit_residual` stays national. The equilibrium solves
`P_supply = unit_cost + delta_0 * cpi_factor`, so `delta_0` is a per-unit
residual in **dollars** covering everything that is not labour — rent, food,
insurance, administration, margin. No reliable sector-specific geographic
adjustment exists (the all-items Regional Price Parity measures consumer
prices, not child care costs), so it stays national; state prices still
differ through wages and staffing.

`elasticities` stay national (no-BA 4, BA 2). The national values are
assumptions (Borowsky et al. 2022, NBER w30140, section 4.4); no state
estimate exists, and two candidate proxies (labour-market slack, outside-option
wage ratio) disagreed on the direction of most states' deviation.

## 8. Employment-rate growth (CBO epop)

Simulation-year employment targets are
`base_rate_g(2019) × growth_g(2019→year)`:

- **Base rates**: computed from the state-filtered parent-unit data
  (automatic once filtering is in place; no file needed).
- **Growth factors**: from `config/state/employment/<ST>.csv`, which keeps
  the `year` column and the 20 cells of `resources/epop/epop_projections.csv`
  that the simulator reads (`epop_m_16_17`, …, `epop_f_55_59`), if present;
  otherwise the national epop file. The file also carries `pop_<bin>`
  columns, the geography's ACS population per fine bin (both sexes); the
  aggregation (fine bins → 8 targeting groups) weights by the base-year row
  of those columns. The national file has none and keeps the
  `macro_projections` population weights.

**The employment files are built, not hand-written.**
`resources/epop/state_epop_projections.R` writes one per state, DC, and NYC
from each geography's own ACS employment rates and the national cell rates:

1. **Local rate.** For each sex × age cell (12 groups, 16+), the geography's
   ACS employment rate, pooled over two years (y−1 and y) to halve sampling
   noise (`resources/epop/US_ACS__epop_by_state_sex_age__2006-2024.csv`,
   `acs_epop.py`; NYC rows select the five boroughs by `COUNTYFIP`).
2. **Relative rate.** Local rate ÷ the national ACS rate in the same cell and
   years (sum over the 50 states and DC). ACS-CPS level differences cancel.
3. **Path.** Cell rate(t) = national cell rate(t) × relative rate(t), with t
   held at 2024 after 2024 (and at 2007 before 2007).

Only the change since 2019 reaches the simulator: growth = national growth ×
relative rate(2024) ÷ relative rate(2019). The state files leave out the
60–64, 65+, and 16+ cells, which the simulator does not read.
Base-year employment *levels* always come from the state-filtered data; only
the growth path is taken from the file. The level is the mean of the model's
2019 employment probabilities, weighted by each primary caregiver's un-aged
2019 weight (`per_weight_pc_base`), so it is a 2019 rate in every run year.

## 9. Sampling

- `calib_sample` is untouched (calibration is national).
- `sim_sample` applies **after** the state filter (filter, then subsample),
  so the requested percentage refers to the state's households.
- Small states at low `sim_sample` can produce very thin cells; v1 does not
  guard against this beyond reporting the post-filter household count.

## 10. Metadata

Run metadata records, per state: the state list, whether the demand
adjustment was applied (and the solved deltas), the supply-parameter source
(`nsece` vs `state yaml`), and the epop source (`national` vs `state csv`).

## 11. v1 limitations (accepted, documented)

1. **No state income taxes** — federal-only tax logic (no state EITC/CDCTC
   supplements). Significant for state-level affordability results; to be
   revisited.
2. **Partial supply anchoring.** Each state supply yaml re-bases labour
   supply to the state's base-year hours and takes state wages and staffing
   (§7); the non-labour residual and elasticities stay national. Base-year
   *quantities* match; price *responses* use national elasticities. The demand and
   supply files are not forced to jointly reproduce an observed state
   equilibrium.
3. **National growth factors** for wages/CPI (`macro_projections`).
4. **Small-state precision** — no minimum-N guard yet.
5. **`Other Paid` price** remains the national calibrated base price.

## 12. File contract summary

```
config/state/
├── demand/<ST>.csv       # 2019 annual hours by target group (6 rows); state_data/code/00_build_demand_targets.R
├── supply/<ST>.yaml      # supply params, schema = supply_2019.yaml; resources/supply/state_supply_params.R
└── employment/<ST>.csv   # epop projections, year + 16-59 cells of epop_projections.csv; resources/epop/state_epop_projections.R
```

Each falls back to the national input when absent. Demand and employment are
optional; a state with a demand file also needs its supply file, and a supply
file without a demand file stops the run (§7). All
three are built, for the 50 states, DC, and NYC, by the scripts named above.
`<ST>` is the USPS postal abbreviation, uppercase, or a sub-state code
(`NYC`). NYC's supply file takes the OES New York MSA wage ratio; its
employment file takes New York's age-sex composition,
which has no city figure. How the demand targets are built is
in `state_data/README.md`.
