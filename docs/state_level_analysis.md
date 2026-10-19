# State-Level Analysis: Design

*Status: agreed design. Author: HE + Claude. Updated 19 October 2026: 50
states, DC, and NYC, with built demand, supply, and employment files.*

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
- **A sub-state geography has its own alpha files**
  (`alpha_<type>_2019_NYC.rds`, from `acs_enrollment[_joint]_2019_NYC.csv`)
  under the `national_rpp` income-rank scheme (§5, "Sub-state
  income ranks"). An interface without them runs NYC on the national alphas.

## 4. Architecture

```
processing (national)  →  calibration (national)
        →  simulation init: build one context per state
              - filter households to state (cascade to all tables)
              - recompute alphas to hit state demand targets   (§5)
              - override supply params if state yaml present    (§7)
              - resolve state epop file if present              (§8)
              - take the state's rows of the price wedge cache
                (each care type's price forest, a quantile regression
                forest on income percentile, region, parents, and child
                age, at one random percentile per parent unit as in
                main (set.seed + sample); the NYC
                run takes its own NYC-rank alphas and NYC-rank wedge
                rows, §5)
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
  below or at/above a density-adjusted tier line for the Census region,
  community urban density, and age band (`1a_nsece_processing.R`, rule 6
  below); the public-use NSECE files do not
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

iterated to convergence (`max |log(H_target/H_model)| < tol`, with
`tol = 1e-8` and at most 5,000 rounds). With one
delta per targeted type and one target per type, the system is exactly
identified.

**Caregiver employment is solved in the same contraction.** Care and work go
together in `p0`, so raising care to the targets alone also raises base-year
employment above the forest rate (1.0 to 2.5 points on average by caregiver
group in run 202610151655, `state_data/out/baseline_employment_state.csv`).
The yearly employment targets grow from the forest rate, so the yearly
shifts would then pull employment down and cut care (8.6% of non-parental
hours on average at fixed 2026 prices, `baseline_care_shift_state.csv`). So
3a passes the state's caregivers (`caregiver_groups_for_state()`: sex x
age band of the primary caregiver, caregiver weight, same rule as 3b) and
the contraction adds one shift `s_g` per targeting group `g` on working
choices:

```
p_ij ∝ p0_ij · exp( Σ_c δ_{t_c(j)} + s_{g(i)} · working_j )
s_g ← s_g + logit(e_g^target) − logit(e_g)
```

`e_g^target` is the group's base-year forest rate: `p0`'s working-choice mass
(the employment forest's `p_employment.pt + .ft`), weighted by caregiver
weight split over the unit's pseudofamily rows. Both sets converge to `1e-8`.
The yearly targeting shifts then carry only the change since 2019.
`base_employment_shifts` in the run metadata records `s_g`.

The stored (reference-normalized) alpha matrix is then updated as

```
alpha_j += d_j − d_1 + s_{g(i)} · (working_j − working_1)     where d_j = Σ_c δ_{t_c(j)}
```

(the reference-choice shift keeps the stored normalization; choice
probabilities are invariant to it).

### Failure modes — fail loudly

- A care type with **zero base support** in the state (`H_t(0) = 0`) but a
  positive target: no finite delta can reach it. Stop with a clear error
  (this almost always means the imputation or the CSV is wrong).
- A **zero target** for a type with positive support: also an error (would
  require `δ → −∞`; set a small positive target instead if intended).
- Non-convergence after the iteration cap: error with the worst offending
  type and its log-gap.

### NSECE care labels and forest features

The care-choice forest that gives `p0` trains on NSECE children, and every run
follows main on the labels and features: a regular provider counts with
positive reference-week hours, a type 8 row counts as Other, 1a makes one label
draw, the caregiver is the respondent parent in NSECE and the mother in a
two-parent unit (else the parent) in ACS, and the employment band comes from
weekly hours with a 35-hour cutoff. State setup changes only the tier line
(item 6) and the income-rank scheme.

6. **The paid-center tier line moves with urban density (state runs).**
   This item applies only when `ECEC_URBAN_DENSITY=1`, which state runs
   set; the switch turns on all state machinery (density tier line and
   RPP-adjusted income ranks). With `0` (the default, national runs), the
   tier line is the national age band median posted price
   (`compute_tier_thresholds_age_bin()`), and incomes are ranked nominally
   (`national` scheme). 1a, 1b, the
   price forest file, and the alpha files record the setting. 1b, 2a, 3a,
   and `load_substate_alpha()` stop when the run's setting differs from the
   interface's. A file with no record counts as `1` (built before the
   switch on 2026-10-14). With `1`: one dollar does
   not buy the same care in a rural market and in a dense city. I set the
   line for region `r`, density `d`, and age band `a` to
   `m(r, a | d = 1) x M(d, a) / M(1, a)`: `m` is the regional median posted
   price among high-density centers, and `M` is the national median for the
   density level. All medians weight centers by survey weight times FTE
   enrollment, over positive prices (`compute_tier_thresholds()`,
   `urban_density.R`). I use the national ratio, not the cell median,
   because a region x rural x age cell holds only 5-35 centers. A center
   uses its own `CB9_COMM_URBAN_DENSITY`; a family uses its
   `HH9_COMM_URBAN_DENSITY`. The price multipliers use observed hourly
   payments only.

### Sub-state income ranks (NYC ranked twice)

Income enters two models through the income percentile. The first is the
care-choice forest that gives `p0` (`1b_acs_processing.R`). It ranks
household income (HHINCOME) against cutpoints weighted by children under 5.
The second is the price wedge (`compute_price_wedge_cache.R`). Every run
uses the price forest: one quantile
regression forest per care type on income percentile, region, number of
parents, and the mean age of the children under 5 (500 trees, `mtry` 4,
minimum node size 10, 50,000-row weighted sample). As in main, one random
percentile per unit (`set.seed(random_seed_base + 2)`, then
`sample(1:99, n, replace = TRUE)`, shared by the care types) picks the
quantile level; there is no keyed price draw, so the global RNG moves and
the wedge depends on unit order. The forest predicts the 99-level grid once
for each distinct feature row; 1a trains it with no own seed, as main.
`price_wedge_income_pctiles()` ranks ACS parent units for the wedge by
baseline AGI (`baseline_agi1 + baseline_agi2`, weighted by the mean parent
weight), as the original simulator did, with the same RPP adjustment as the
scheme and national cutpoints. The care-choice forest keeps household
income. AGI of 0 or less gets percentile 1; an unknown AGI gets no wedge.
Ties get a keyed
jitter in place of `runif()`, so the global RNG does not move. The scheme
comes from
`ECEC_INCOME_RANK` (`income_rank.R`); unset, it follows the urban-density
switch. Both schemes rank nationally:
`national_rpp` (default with `ECEC_URBAN_DENSITY=1`) divides income by the
state RPP (NSECE: the Census region RPP), and `national` (default with the
switch off, as main) uses nominal income. NYC households get two versions
of their ranks:

1. **The NY run and every national use** divide NYC income by the NY state
   RPP. These are the national `alpha_<type>_2019.rds`, unchanged.
2. **The NYC run** uses NYC rank inputs (`substate_rank_inputs()`):
   - `national_rpp` (default with the switch on): NYC income is divided by the NYC metro RPP
     (`msa_35620`, 114.899) in place of the NY state RPP (109.426). The
     cutpoints stay the national ones, built with the NY value for NYC.
   - `national`: the two versions are identical. The code skips the second
     version and logs it.

How the NYC version is built:

1. **ACS processing** runs the trained forests a second time on the NYC
   households with the NYC rank inputs. It writes
   `acs_enrollment_2019_NYC.csv` and `acs_enrollment_joint_2019_NYC.csv`.
   The extra step has its own seed and restores the RNG state, so the
   national outputs do not change.
2. **Calibration** first estimates beta and rho nationally, as before. Then
   `compute_substate_alphas()` (`compute_substate_alpha.R`) rebuilds the
   NYC rows of the calibration data with the NYC `p0` and NYC price wedge
   rows. The NYC wedge rows take the income percentile from the NYC rank
   inputs (`price_wedge_income_pctiles(..., geo = 'NYC')`), so the
   wedges can differ from the national rows. The NYC cache draws its own
   percentiles (`set.seed(random_seed_base + 2)`, then `sample(1:99)` over
   the NYC units, as main), so a NYC unit's drawn percentile can differ from
   its national one. Each cache call sets the seed and so moves the global
   RNG stream. It writes
   `alpha = log p0_NYC - V_NYC` as `alpha_<type>_2019_NYC.rds` (same fields
   as the national file, plus `geo`, `income_rank_scheme`, and
   `urban_density`). It stops if
   the NYC rows or child weights do not match the NYC rows of the national
   file.
3. **Simulation init** gives the NYC context the NYC alpha file
   (`load_substate_alpha()`) and NYC price wedge rows built the same way as
   in calibration. The re-anchoring to
   `config/state/demand/NYC.csv` then starts from the NYC alphas. An
   interface without the NYC files runs NYC on the national alphas and logs
   it. An NYC file built under a different scheme than the run stops. An
   NYC file with no scheme label, or built under `state` or `region`, also
   stops: the interface must be recalibrated.
4. **The demand build** (`state_data/cluster/acs_p0_shares.R`,
   `acs_p0_shares_by_age.R`) reads the NYC `p0` for the NYC rows when the
   NYC files exist, and the national `p0` otherwise (logged).

The rule is generic over `SUBSTATE_GEOGRAPHIES`; NYC is the only entry.

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
on the national parameters.

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
not); it scales `paid_home_based`. Rules: the 2017 Child Care Licensing Study
(ICPSR 37700), extracted by `resources/supply/licensing_ratios_2017.R`. The
centre ratio is read at five of the study's age points, one per year of age:
age 0 at 11 months, age 1 at 18 months, age 2 at 35 months, age 3 at 3 years,
and age 4 at 4 years (`NSECE_AGE_POINT` in
`state_data/code/shared_nsece_ages.R`). The weights are NSECE 2019
centre-enrolled children by single year of age (ACF 2023, Exhibit 1;
weighted counts): 426,000, 718,000, 1,010,000, 2,170,000, and 2,340,000, or
6.4%, 10.8%, 15.2%, 32.6%, and 35.1%. I use NSECE because it is a direct
survey count of centre enrolment; the ACS children-by-care-type file is a
forest prediction. This is a representative-age approximation: the ratio at
one age stands for the whole year of age, and a state rule with a cut-off
inside a year (for example at 12 or 24 months) is not split. The five points
make one centre index, so the model adds no care sectors and no choices. If a
state has no 18-month or 4-year ratio, the build uses the point from the
three-point method (35 months or 3 years) and logs it; in the 2017 study
every state except Idaho has all five. The index stays relative to the
national mean, so a state at the mean keeps the national NSECE staffing. The
home value is the maximum children in a small family child care home,
without school-age add-ons. Idaho centres use a point system that the study
does not tabulate: 12 points per staff member, and 2 points for a child
under 24 months, 1.5 points from 24 to 36 months, and 1 point from 3 to 5
years (Idaho Code 39-1109 before 2024, IDAPA 16.06.03.300 from July 2024). I
read it at each of the five points (12 / points): 6, 6, 8, 12, and 12
children per staff member (`ID_POINTS`). NYC centres
use the city Health Code (Article 47, June 2019), which has three bands; they
map onto the five points as infant to 11 months, toddler to 18 and 35 months,
and preschool to 3 and 4 years. Centre index runs 0.719 (GA) to 1.453 (VT); home index 0.421 (AR) to 1.682 (DE).
`state_data/out/licensing_ratio_change_3pt_5pt.csv` compares the five-point
index with a three-point index (11 months, 35 months, and 3 years; ages 1 and
2 at 35 months, age 4 at 3 years). The five-point index differs by more
than 1% in 40 of 52 geographies and by more than 5% in 14 (unweighted
counts), from -18.0% (AL, 4-year ratio 18) to +13.1% (MA, 18-month ratio 4).
I use the 2017 study, not the 2014 study (CAP 2018 Table 1; PN3 2020 Table
6), because CAP read different age points in different states. Against the
2014 rules, ten centre rows and one home row (MA, 10 to 6) differ;
`state_data/out/licensing_ratio_change_2014_2017.csv` lists them, with both
years at the three points.

`per_unit_residual` is state-specific. The equilibrium solves
`P_supply = unit_cost + delta_0 * cpi_factor`, so `delta_0` is a per-unit
residual in **dollars** covering everything that is not labour — rent, food,
insurance, administration, margin. Each national residual (0.510055 low-price
centre, 4.6145667 high-price centre, 2.8273432 paid home; unpaid centre stays
unset) is multiplied by the state's BEA 2019 housing-services RPP / 100
(`resources/supply/US_BEA__regional_price_parities_housing_services__2019.csv`,
line "RPPs: Services: Housing"; NYC = New York MSA 35620). Factor range 0.55
to 1.70. Housing-services prices are a proxy for geographic variation in
non-labour costs. Price check against NDCP 2019
(`state_data/code/16_ndcp_model_prices.R`; definitions in its header, results in
`state_data/out/ndcp_model_prices_summary.csv`): 2019 base posted centre price x 40
vs child-weighted county medians, 46 states, same states + weights for both
residuals. Pearson correlation 0.89 housing-scaled vs 0.83 national; model mean
$213.2 vs NDCP $190.6 a week (ratio 1.12; national residual 1.15). No power is fitted (k = 1). `state_supply_params.R --national-residual`
restores the national value.

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
2019 employment probabilities, weighted by each primary caregiver's weight
aged to the run year, as in main.

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
A sub-state geography that runs on its own alphas also records
`income_ranks: own (<scheme>)`. `state_analysis.yaml` also records
`alpha_price` (`state` or `national`, §13) and `base_year_labor`, the
base-year labour `L_0` by education group (`no_ba`, `ba`).

## 11. v1 limitations (accepted, documented)

1. **No state income taxes** — federal-only tax logic (no state EITC/CDCTC
   supplements). Significant for state-level affordability results; to be
   revisited.
2. **Partial supply anchoring.** Each state supply yaml re-bases labour
   supply to the state's base-year hours and takes state wages and staffing
   (§7). The non-labour residual is scaled by the state's housing-services
   price parity (§7); the elasticities stay national. Base-year
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
├── demand_counts/<ST>.csv # children, part_time by target group (6 rows); same script; read by state_data/cluster/baseline_consistency.R
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

## 13. State-price alpha: `ECEC_STATE_ALPHA_PRICE=state`

Calibration builds `alpha = log p0 - V` at the national supply prices. A
state run prices care at the state's own wages and staffing. The §5
contraction shifts the stored alpha with `p0` only, so a state's base year sits
at national prices while its supply sits at state prices. The first simulated
year then jumps by the gap. The default (`state`) rebuilds alpha at state prices first. `ECEC_STATE_ALPHA_PRICE=national` is the §5 contraction
on the stored alpha, unchanged. Adopted 15 October 2026. Evidence runs: 52 geographies, interface 202610151431, run 202610151654 (`national`) and 202610151655 (`state`). 2019-to-2026 change in the paid-center share of children 0-4 (expected probabilities; same numerator, denominator, and child weights both years; 50 states + DC, equal weight), `state_data/cluster/alpha_drift.R`, outputs and provenance in `state_data/out/alpha_drift_*`: `national` mean absolute change 3.13 points, range -9.79 (DC) to +5.42 (NC); `state` 0.34 points, range -0.86 (DC) to +0.89 (WV). Population-weighted mean absolute change 3.45 vs 0.31 points. Rebuilt 2026 sector hours match the solver within 3.4e-7 relative in every geography. Drift only, not accuracy against observed 2026 care.

1. **Rebuild.** `reprice_alpha_to_supply()` (`reprice_alpha_to_state_supply.R`)
   sets `alpha_state = log p0 - V_state`, where `V_state` uses the state's base-year
   supply prices: state `L_req %*% wages` plus the state residual (housing-parity
   scaled, §7), with
   sector 1 at zero. Only the cost of sectors 2--4 changes (cost = wedge x
   price x hours). The Other Paid base price stays national. The stored alpha
   has choice 1 as reference, so
   `alpha_state[, j] = alpha[, j] + dV[, j] - dV[, 1]` with
   `dV = V_nat - V_state`. `Y` comes from
   `estimation/data/calibration_<type>_2019.csv`. Re-anchoring to the hours
   targets then runs on this alpha, as in §5.
2. **Checks at simulation start.** Each stops the run on a gap.
   - Net income rebuilt at national prices (`Y - C_nat`) equals the
     calibration's `net_income` (skipped for sub-state geographies, which use
     their own wedges).
   - Identity: `softmax(alpha_state + V_state)` reproduces `p0`.
   - Frozen base year (`check_state_base_year()`): after re-anchoring, sector
     hours at `V_state` equal the demand adjustment's sector hours, which set
     base-year labor.
3. **Metadata.** `alpha_price` in `state_analysis.yaml` is `state` or
   `national` for each geography.
4. **Needs.** The state supply file and demand target file for the geography,
   the price wedge (not `--no-price-wedge`), and the calibration data of the
   interface.

### How to compare the two settings on Bouchet

Use a runscript with the `state` column (`config/runscripts/state_targets_smoke.csv`
or `state_targets_all.csv`). Set the switch in the launch shell; the SLURM jobs
inherit it:

```bash
ECEC_STATE_ALPHA_PRICE=national ./slurm/launch_array.sh -r state_targets_smoke -C <interface>
```

Run the same runscript with the default as the reference. Start the runs at
least one minute apart, because the output folder name is the minute timestamp.
