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
              - take the state's rows of the national price
                wedge cache (income percentiles stay national,
                as the price models were trained)
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
  `Low-Priced Center-Based` and `High-Priced Center-Based`. No state publishes
  the within-state price dispersion that split needs, so state sources support
  the SUM. The pair therefore shares ONE delta: that constrains their combined
  hours and leaves their modelled ratio untouched, because a common additive
  utility shift cancels out of the odds between them.
- All 6 rows are required (v1 assumes a complete file; partial targeting is
  a possible later extension). A file naming `Low-Priced Center-Based` or
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
`per_unit_residual`. The override is **all-or-nothing** — all five blocks are
read, so a file supplying one of them must supply the rest.

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

So each file carries the national parameters with `labor_supply` re-based to
the state. Until 1 October 2026 the file held

```
labor_supply = colSums(L_req * state base-year hours)
```

which is the definition `1a_nsece_processing.R` uses nationally, applied to the
state's own base-year hours. The file now holds the ACS-based figure instead:
national L_0 × the state's share of national ACS childcare workers (below).

**When demand is re-anchored, the simulator replaces that figure.** At state
initialisation (`3a_initialize_simulation.R`), `adjust_alpha_for_state()`
returns the model's own base-year hours by supply sector at the solved deltas,
and the simulator sets `L_0 = colSums(L_req * sector_hours)`. The contraction
pins combined center hours but not the low/high split, and the two bands differ
sharply in BA labour per hour (0.035 against 0.076), so a file value built from
a fixed national split would leave the base-year ratio off 1 for BA labour.
From the model's own hours, `L / L_0 = 1` in the base year by construction.
The file's `labor_supply` remains the fallback for a state with no demand file.

**`wages` and the labour quantity come from ACS childcare workers, by measured
education.** `resources/supply/acs_childcare_wage_by_education_2019.py` reads
IPUMS extract usa:33 (`resources/supply/acs_ipums_extract_2019.R`) on the
cluster. It uses the 2018 and 2019 records of the 2015-2019 5-year sample, for
about twice the 1-year records. The workers are the ACS occupations of the four
SOC codes that care for children under school age:

| ACS OCC | SOC | Occupation | Kept |
|---|---|---|---|
| 4600 | 39-9011 | Childcare workers | every industry except private households (IND 9290) |
| 2300 | 25-2010 | Preschool and kindergarten teachers | child day care services (IND 8470) |
| 2330 | 25-2050 | Special education teachers | child day care services |
| 230 | 11-9030 | Education and childcare administrators | child day care services |

The ACS pools each of the last three across school levels, so they count only
in child day care services. Private-household childcare workers are nannies and
babysitters, the simulator's Other Paid, not a center or home provider. The
group is the worker's own education: BA = `EDUC` ≥ 10 (four or more years of
college). An occupation mapping does not work: BLS Employment Projections
Table 5.3 (2023-24) puts 22.6% of childcare workers at BA or higher and 46.2%
of preschool teachers below it. In the ACS 2019 1-year file, the BA share is
15.6% for childcare workers and 36.3% for preschool teachers in child day care
services.

Hourly earnings are `INCEARN` ÷ (weeks × usual hours), with earnings ≤ 0,
under 100 annual hours, and hourly outside $2-200 dropped. Each state's wage is
the national NSECE wage × the state mean ÷ the national mean, by group. The
ratio keeps NSECE's wage definition, which the rest of the parameter set is
built on. Ratios run 0.754 (SD) to 1.291 (NH) for no-BA and 0.730 (KY) to
1.386 (NV) for BA. A cell under 30 unweighted workers takes ratio 1: BA in AK,
DC, DE, HI, ID, MT, ND, NM, WV, and WY. Standard errors use the 80 replicate
weights; the median is 0.05 (no-BA) and 0.08 (BA). NYC is the five boroughs
(`COUNTYFIP`), with its own ratio and labour share. The ratios correlate with
the occupation-mapped OEWS May 2019 ratios they replaced (2 October 2026) at
0.72 (no-BA) and 0.61 (BA).

Each file's `labor_supply` is the national NSECE L_0 × the state's share of
national ACS workers in that group. A run with a demand file replaces it with
the model's own base-year hours (above). The fallback is weak: against labour
requirements × the state's base-year hours, the ACS-based L_0 ran from 0.69
(HI) to 2.00 (NE) for no-BA and 0.34 (DC) to 2.26 (SD) for BA.

`labor_requirements`, `elasticities` and `per_unit_residual` stay national.
`per_unit_residual` is the one worth stating: the equilibrium solves
`P_supply = unit_cost + delta_0 * cpi_factor`, so `delta_0` is a per-unit
residual in **dollars** covering everything that is not labour — rent, food,
insurance, administration, margin. State wages move `unit_cost`, and the
state's price moves with it by the full labour-cost difference while the
non-labour margin stays at its national dollar value. That is the conservative
choice: nothing measured here says the non-labour margin varies by state, and
rent plainly does not scale with childcare wages.

## 8. Employment-rate growth (CBO epop)

Simulation-year employment targets are
`base_rate_g(2019) × growth_g(2019→year)`:

- **Base rates**: computed from the state-filtered parent-unit data
  (automatic once filtering is in place; no file needed).
- **Growth factors**: from `config/state/employment/<ST>.csv` **in the same
  format as `resources/epop/epop_projections.csv`** (columns `year`,
  `epop_m_16_17`, …, `epop_f_65plus`) if present; otherwise the national
  epop file. The existing aggregation (fine bins → 8 targeting groups using
  `macro_projections` population weights) is reused with the file path
  swapped.

**The employment files are built, not hand-written.**
`resources/epop/state_epop_projections.R` writes one per state, DC, and NYC
from the national CBO cell rates and each state's own age-sex composition:

1. **Composition.** The share of the state's population 16+ in each sex × age
   cell, from the ACS (`resources/epop/US_ACS__epop_by_state_sex_age__2006-2024.csv`).
   The national composition is the sum over the states.
2. **Implied rate.** The rate the state would have if each cell had the CBO
   national rate: Σ cell share × CBO cell rate. The same sum with the national
   composition gives the national rate.
3. **Uniform scale.** Every cell is the CBO cell rate × k, where k = national
   rate ÷ state implied rate. So the state's aggregate equals the national
   rate, and a state with the national composition gets the national path.
   The script checks this on every build, for every state and year, and stops
   if any aggregate misses the national rate.

The national file has no 60–64 or 65+ rates from 2026 on, so each year's sum
runs over the cells that have a rate, for the state and the nation alike.
Composition is held at its nearest ACS year outside 2006–2024, so runs from
2026 use the 2024 composition. The shares freeze, not k: k is recomputed every
year, so it still moves after 2024 when CBO cell rates move differently. Only
the change in k since 2019 reaches the simulator, as a rate ratio per cell:
r_s,g(t) / r_s,g(2019) = r_US,g(t) / r_US,g(2019) × k(t) / k(2019). A state's
growth path differs from the national one only through its age-sex
composition. k(2024)/k(2019) runs 0.989 (DC) to 1.013 (DE) across the 52 files, against 0.960 to
1.043 under the LAUS method it replaced (30 September to 1 October 2026; its
input and download script were then removed). The state's own labour market
does not enter. NYC takes New York's composition until a county-level one is
built.
Base-year employment *levels* always come from the state-filtered data; only
the growth path is taken from the file.

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
   supply to the state's base-year hours and takes state wages (§7);
   labour requirements, elasticities, and the non-labour residual stay
   national. Base-year *quantities* match; equilibrium *prices* and price
   *responses* reflect national supply apart from wages. The demand and
   supply files are not forced to jointly reproduce an observed state
   equilibrium.
3. **National growth factors** for wages/CPI (`macro_projections`) and
   national population weights inside the epop aggregation.
4. **Small-state precision** — no minimum-N guard yet.
5. **`Other Paid` price** remains the national calibrated base price.

## 12. File contract summary

```
config/state/
├── demand/<ST>.csv       # 2019 annual hours by target group (6 rows); state_data/code/00_build_demand_targets.R
├── supply/<ST>.yaml      # supply params, schema = supply_2019.yaml; resources/supply/state_supply_params.R
└── employment/<ST>.csv   # epop projections, schema = epop_projections.csv; resources/epop/state_epop_projections.R
```

Each falls back to the national input when absent. Demand and employment are
optional; a state with a demand file also needs its supply file (§7). All
three are built, for the 50 states, DC, and NYC, by the scripts named above.
`<ST>` is the USPS postal abbreviation, uppercase, or a sub-state code
(`NYC`). NYC's supply file takes its own ACS wage ratio and labour share (the
five boroughs); its employment file takes New York's age-sex composition,
which has no city figure. How the demand targets are built is
in `state_data/README.md`.
