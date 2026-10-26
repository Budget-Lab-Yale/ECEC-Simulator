# State-level demand inputs

Builds the demand file the simulator reads — `config/state/demand/<GEO>.csv`,
six rows, annual hours by care type — from each state's population and NSCH non-parental rate,
divided with shares from the simulator's own base-year probabilities over the
state's ACS households, with Unpaid Center-Based from each state's program
counts.

```bash
Rscript state_data/code/00_build_demand_targets.R --geo KY         # build one
Rscript state_data/code/00_build_demand_targets.R --all            # build all 52
Rscript state_data/code/00_build_demand_targets.R --all --check    # with invariants
```

**State setup must run with `ECEC_URBAN_DENSITY=1`.** The simulator's urban
density switch turns on all state machinery: the NSECE community density
tier threshold and tier-draw cells, region donor cells in the hours and
paid-status draws, and RPP-adjusted income ranks (the care-choice forest has
no density feature). It is off by default (= main) and on for state runs.
Unset `ECEC_INCOME_RANK` follows the switch. The interface
whose `p0` feeds the demand targets must be built under `1` (1a, 1b, 2a). Set
the variable in the shell for the interface build and for
`state_data/cluster/acs_p0_shares.R`, `acs_p0_shares_by_age.R`,
`nsece_shares_by_age.R`, and `state_data/code/02_derive_nsece_constants.R`,
which stop with a clear message if it is `0`. `acs_p0_shares.R` also stops if
the interface's alpha files record `0`. Alpha files with no record were built
before the switch (2026-10-14) and count as `1`. Example:
`ECEC_URBAN_DENSITY=1 Rscript state_data/cluster/acs_p0_shares.R <interface> <out.csv>`.

**The method memos, `docs/methodology-MM-DD.pdf` (finished ones in `docs/archive/`), are the documents to read.** They
say what every number means, where it comes from, and how solid it is. Finished
memos (PDF) are in `docs/archive/`; the memo in progress and the .tex sources
are kept outside git. This file is the map, and the record of every
source. A machine-readable source register (one row per source and geography,
with its URL, vintage, and retrieval date) is kept outside git.

## Status: paused 26 October

Work on this branch is paused. Two items are open:

1. **The state staffing inputs still have an issue.** The audit below passes, but
   the inputs are not final. Do not treat the 312 reviewed records as settled.
2. **The paired state smoke from 24 October did not run.** The 24 October commit
   (`cc915b7`) waited on it after the Bouchet SSH master dropped. No ECEC job
   ran on Bouchet after 23 October.

## 22 October supply update

Annual wage inputs contain 1,473 occupation records and 728 geography/year/education
ratios. All 52 saved wage paths match the source panel. The staffing review covers
312 of 312 unweighted geography/setting/age records. On 22 October, 28 changed a
number and the other 284 confirmed the old input. On 26 October the one-adult home
rule and the ordinary center band (below) changed 10 more: 38 records now differ
from the 2017 input (31 changed values, 7 home values with no 2017 value) and 274
confirm it. Center inputs are now month-weighted from full age bands. Supply files
have been rebuilt with all reviewed values and no unresolved staffing records.
Local source, supply, and annual wage checks pass.

The 1% ACS sample completed 2026--2027 for all 52 geographies on 26 October with
the current staffing inputs (one-adult home rule, month-weighted ordinary center
bands, NY blend, licensed-share home blend) on calibration interface
`202610261104` (run `202610261506`, job `30885764`, exit 0, 36 minutes). All 104
price solves and employment targets passed. The largest paid-sector price gap was
$0.00033 per child-hour (OR 2026); the largest employment miss was 0.00019
percentage points. `Rscript state_data/cluster/integrated_validation.R <run root>
<out prefix>` on Bouchet writes `out/integrated_validation_current.csv` (solver),
`_employment.csv`, `_summary.csv`, and a provenance file with the supply YAML
hashes. The supply YAMLs the run read match the committed ones.

The refreshed 2019 NDCP comparison covers 46 states: weekly RMSE is $33.98,
correlation is 0.897, and the child-weighted model mean is 11.7% above NDCP.
2020--2022 weekly RMSE is $38.34, $41.09, and $46.93 across 46, 41, and 45 states.
Program-count and other source checks pass. The corrected 23 October NY/NYC
diagnostic validates common-rank partition identities and reports differences
between independently calibrated geographies. `--strict-additivity` retains
the historical 1% / 0.1%-of-total limits; see the partition discussion below.

## Staffing-rule audit: October 1, 2019

The audit has 312 unweighted records: five center age points and one home input
for each of 52 geographies. These are records, not counts of children or providers.
I check the rule that applied on **October 1, 2019**, including its age bands,
program type, and need for an assistant. A source's publication year alone is not
enough. A reviewed record can confirm an existing value without changing it.

Current coverage is 312 reviewed records and zero unresolved records. The strict
`--require-verified` source/hash check passes. Job `30293994` (22 October) ran
with the first 28 numerical corrections, not the 26 October changes.

Run `python state_data/code/analyze_licensing_2019.py` from the repository root.
It checks source hashes and writes `out/licensing_2019_audit.csv` and
`out/licensing_2019_coverage.csv`. Add `--require-verified` to fail while any
fallback remains. Downloading a candidate does not mark it verified.

The registry is under `resources/licensing/` in the repository root:

- `licensing_2019_sources.csv`: source URLs, saved files, hashes, review status.
- `licensing_2019_evidence.csv`: chosen values, dates, rule sections, caveats.
- `licensing_2019_search_log.csv`: unresolved questions and next source to check.
- `licensing_2019_home_configurations.csv`: total and preschool capacity,
  required adults, infant limits, and school-age additions.
- `licensing_2019_center_bands.csv`: each geography's ordinary center ratio bands,
  0 to 60 months (220 rows, all read in the cited source).

For a conditional record, `permitted_values` lists legal alternatives and
`selection_basis` explains the chosen model input. This does not imply one ratio
applies to every provider or every child of that age.

For centers, I use the **ordinary band**: the ratio that applies to a child of
that age by default, without opt-in groupings (special age groups, toddler
extensions that need parent agreement, Montessori or half-day ratios, two-adult
group bonuses). Where bands overlap, a month takes the band that starts latest at
or before it. I **month-weight** each year of age: staff per child for age a is
the mean of 1 / ratio over months 12a to 12a + 11. The five audited age points
(11, 18, 35, 36, and 48 months) must equal the band at that month, so the audit
checks the bands. On 26 October the ordinary band changed six points: AL 35
months 8 to 11, CO 35 months 7 to 8, KS 35 months 7 to 10, MN 35 months 7 to 10,
VT 35 months 5 to 6, and VT 3 years 6 to 10. The NY center input blends the New
York State and NYC rules by staff per child, at NYC's share of New York children
ages 0-4 (0.4647, PEP 2019); the NYC run keeps NYC's own rules. For homes,
I use one rule in every geography: **one adult, mixed-age**. The input is the
maximum number of preschool children (ages 0-4, school-age-only additions excluded)
that one adult may care for when an infant may be present. I never use a
configuration that needs a second adult, and I never use a preschool-only
configuration that excludes infants. NYC homes use the New York State rule.
Licensing applies only to licensed homes, so I blend the home index toward 1:
home index = 0.257 x licensing index + 0.743. The 0.257 is the share of children
ages 0-5 in paid home-based care who are in licensed (listed) homes, NSECE 2019
home-based provider survey with survey weights
(`state_data/cluster/nsece_home_licensed_share.R` writes
`resources/supply/US_NSECE__home_licensed_share__2019.csv`; paid children of any
age give 0.243, all children 0.209). Unlicensed paid home care keeps the national
staffing level. Home indices run from 0.873 (SD) to 1.135 (AZ, MS, and RI).
On 26 October this rule changed seven home inputs:

- Arkansas: 16 (two or three adults) to 8 (one caregiver, at most one child under 2).
- Kansas: 7 (no child under 18 months) to 6 (one to three children under 18 months).
- Maine: 8 (all children ages 2-5) to 6 (at most three children under 2).
- Mississippi: 12 (all children age 2 or older) to 4. The youngest child sets the
  ratio, and an infant under 1 sets it at 4. If the youngest child is age 1, the
  ratio is 8.
- Rhode Island: 8 (full-time assistant) to 4. A provider alone with any child under
  18 months may have at most four children under 6.
- Virginia: 8 (all children ages 2-4) to 7. One infant (4 points) plus six children
  ages 2-4 (2 points each) uses the 16-point limit.
- Washington: 12 (qualified assistant) to 6. A second staff member is required when
  more than six children are in care and any child is under 2.

Florida (6) and Wisconsin (8) keep their numbers but now cite a configuration that
admits infants. Nevada stays at 6 with all children age 3 or older: its 2017 table
does not settle the capacity of a mixed group with an infant. The model uses
inverse capacity. The national NSECE anchor already includes respondent plus helpers.

`Rscript state_data/code/analyze_home_staffing.R` from repository root writes
`out/home_staffing_sensitivity.csv`. It swaps AR, RI, and WA to their licensed
multi-adult configurations (adults divided by capacity) and renormalizes over
50 states and DC. The two-adult configurations give the same staff per child as
the one-adult inputs (2/16, 2/8, and 2/12), so their anchor prices do not change.
Three adults at AR's 16 raises AR's 2019 home-care anchor price by 26.1%. These are
illustrative base-price sensitivities, not fitted staffing estimates or equilibrium
results. RI's source also permits limited provider absence when an assistant is
present; licensed headcounts do not establish an exact adult-hour requirement.
Age limits and own-child counting remain in the evidence and configuration records.

`Rscript state_data/code/review_home_staffing_inputs.R` writes
`out/home_staffing_input_review.csv`: one record for each of 52 geographies,
selected capacity/source/conditions, adult-count configurations at that capacity,
and explicit unresolved age mix, worker hours, occupancy, and category weights.
The adult-count range covers documented configurations, not every legally possible
composition. No unresolved adult count is filled with one.

`Rscript state_data/code/analyze_home_prices.R` writes `out/home_price_benchmark*.csv`
and provenance. It uses NDCP family-home columns and their own imputation flags,
2019 county child-population weights, and a fixed national paid-home child age mix
from ACS p0. All three age-group prices must be present; NYC is reported separately
and excluded from national fit statistics. Household net payments anchor the national
home price; NDCP reports market rates. The comparison is external evidence, not a
calibration target. `Rscript tests/test_home_staffing_review.R` checks home/center
column isolation, unknown flags, audit coverage, and unchanged production YAMLs.

Source documents stay under `raw/<STATE>/`; national source files are under
`resources/<SOURCE>/` in the repository root. The full source list,
`docs/SOURCES.csv`, is kept outside git; the tracked staffing register is
`resources/licensing/licensing_2019_sources.csv`, and each derived file carries a
`.provenance.txt`. Acquisition logs record successful and failed downloads separately.

The 13 final capacity-source reviews, supply rebuild, center NDCP refresh, and
52-geography baseline validation are complete. A home capacity is not a complete
adult- and age-adjusted labor measure; the separate home review above retains that
limitation. Future evidence changes require the same rebuild
and validation sequence. `Rscript resources/supply/00_update_state_supply.R`
refreshes the source/hash audit before adopting reviewed values; set `PYTHON`
to a working executable if needed. `Rscript tests/test_state_staffing_inputs.R`
checks all 312 records and independently reconciles all 52 YAML staffing payloads.
Acquisition text and intermediate download logs live in git-ignored
`work/licensing/`; registered source documents stay at their recorded paths.

## What is built

Fifty-two geographies: the 50 states, DC, and New York City. The memo
describes five: **KY, NJ, NY, NYC, VA**. **Puerto Rico is not built.** NSCH
covers only the 50 states and DC, NSECE places Puerto Rico in no Census region,
and the simulator's ACS households do not include it. Building it needs a participation rate, a region or a stand-in
for one, a wage, and households. New York is all 62 counties, city
included. New York City is the five boroughs, built on its own. NY includes
NYC. Each production run retains its own calibration. The NY/NYC diagnostic
uses separate common-rank controls to obtain a true rest-of-state household
partition; subtracting own-rank NYC p0 from national-rank NY p0 is invalid.
The corrected Unpaid Center-Based annual-hour gap is 1.98%, or 0.265% of
all rest-of-state annual hours: 1.00 million annual hours from NYC's own
income-rank calibration and 0.69 million from different geographic weights
for program children and p0 part-time shares. Estimated program-child counts
still add exactly. These differences don't require either production run
to use the other's calibration.

`tests/check_nyc_rest_of_state.R` checks the common-rank p0 partition,
agreement with production controls, and exact hours decompositions. It always
reports material cross-calibration gaps in `out/nyc_rest_of_state_validation.csv`.
`--strict-additivity` retains the historical 1% per-type and 0.1%-of-total
thresholds and currently exits 1. Default structural checks pass.
The partition file in `resources/acs/` was regenerated from interface
`202610191203` on 23 October (Bouchet job `30447222`, exit 0).


This directory writes demand only. The simulator's per-state supply and
employment files, and how the simulator reads all three, are in
[`docs/state_level_analysis.md`](../docs/state_level_analysis.md).

## The method

Per geography, in six steps:

The step numbers name quantities everywhere in `state_data/`. The build runs
them in the order 1, 4, then 2, 3, and 5 together, then 6.

1. **Children in regular care** = population 0–4 × NSCH participation rate ×
   the NSECE universe ratio.
2. **Formal and informal totals** — a result of steps 4 and 5, not an input.
3. **Informal care** — split by the **share model** (below).
4. **Unpaid Center-Based** — each state's own **program counts**: public
   pre-K (CCD) + Head Start + Early Head Start, converted from funded slots to
   enrolled children, less the Head Start the pre-K count already holds. CCD
   misses community-based public
   pre-K, so pre-K is the larger of CCD and an all-settings count: NIEER Table
   2 state pre-K (no special education) for a state, the city's own DOE count
   for NYC
   (`nieer_prek_inputs()`; 19 geographies take it). The Head Start overlap matches the count taken:
   NIEER's share where NIEER reports one, and the 0.263 fallback otherwise.
5. **Centers and homes** — split by the share model. The share model divides
   step 1 less step 4 over the five other types, so steps 2, 3, and 5 come
   out of one division.
6. **Annual hours** = children × the simulator's 10/40 grid × 50, split
   part-time / full-time by the share model. Unpaid Center-Based takes the
   same rule. `code/14_program_hours_sensitivity.R` builds its hours program
   by program instead, as a sensitivity only (see "Program hours" under step
   4).

**The share model.** Steps 2, 3, 5, and 6 take their shares from the
simulator itself. `cluster/acs_p0_shares.R` sums the calibration's base-year
choice probabilities (p0) over each geography's own ACS households, weighted by
child weight: children 0–4 by care type and by 10 or 40 hours a week. So each
state's mix follows its own income, parent employment, family type, and child
ages, under the national preferences the simulator calibrates. NYC uses the
households in its five counties. The summary is
`resources/acs/US_ACS__p0_children_by_care_type_and_hours__2019.csv`, built on the cluster
from a full-sample interface. The simulator then re-anchors to this file: `adjust_alpha_for_state()` solves one
utility constant per care type until the state's base-year hours match it.
With p0 shares, the file already carries the simulator's own mix, so the
re-anchoring moves mainly the level (step 1) and Unpaid Center-Based (step 4).

**Regional NSECE comparison.** `code/05_compare_nsece_shares.R` divides the
same total and program count by regional NSECE shares instead, steps in number
order (step 2 takes the regional formal share, step 4 takes the programs out
of formal care, step 5 splits the rest of formal care). NSECE carries only
Census region; a regional cell under 30 unweighted records takes the national
value. It writes `out/nsece_share_comparison.csv` and nothing the simulator
reads. It also reruns the step 2 five-hour factor with the NSECE short share
by region and by care type in place of the national 0.340, and writes the
change in children in care to `out/short_share_sensitivity.csv` (at most 1.13%
by region, 0.95% by type).

```bash
Rscript state_data/code/05_compare_nsece_shares.R          # all 52 geographies
```

**Sensitivity.** The build does not write an error interval for the NSCH
rate. A change in the rate does not move every row in proportion. Program
counts stay fixed, and the other five types absorb the change. An error in a
program count moves care between formal and informal, because the five other
types share what the programs leave. With no Head Start deduction, total hours
change by less than 0.1% in the five memo geographies; Unpaid Center-Based
hours rise by up to 5.3% (VA). Both figures were measured on the regional
NSECE build.

## Layout

```
code/shared_setup.R                     root path, region map, universe labels, CLI and
                                        print helpers -- sourced by 00, 01, and 02; every
                                        script runs from the repo root
code/00_build_demand_targets.R          the method, the share model, the universe
                                        ratio, and the CLI; build_geo() returns one
                                        list per geography (care counts, part-time
                                        counts, and hours by type; program counts at
                                        each step; participation inputs and
                                        conversion factors), and the demand CSV and
                                        05 and 07 read that list (build_of())
code/05_compare_nsece_shares.R          comparison: the build against regional NSECE
                                        shares (writes nothing the simulator reads)
code/07_compare_nhes.R                 validation: the build against NHES-ECPP 2019, same
                                        population and care-type rules, by Census region
code/09_ccdf_subsidy_children.R         diagnostic: CCDF FY2019 children 0-4 by setting,
                                        hours, and $0 copay, against Unpaid Center-Based
                                        (writes nothing the simulator reads)
code/10_idea_preschool_special_education.R
                                        diagnostic: IDEA Part B 2018-19 children 3-5 by
                                        environment, against step 4 (writes nothing the
                                        simulator reads)
code/11_ndcp_price_comparison.R         diagnostic: NDCP 2019 centre prices by age group,
                                        child-weighted mean of county medians, against the
                                        model high-priced centre share (writes nothing the
                                        simulator reads)
code/16_ndcp_model_prices.R             diagnostic: model 2019 base posted centre price
                                        (x 40) against NDCP 2019 county medians, housing-
                                        scaled vs national residual on the same 46 states
                                        and weights (writes nothing the simulator reads)
code/12_participation_bias.R            diagnostic: national non-parental share in NSECE,
                                        p0, NSCH, targets, and NHES; aligned NHES
                                        five-to-ten-hour ratio (prints only)
code/14_program_hours_sensitivity.R     sensitivity: Unpaid Center-Based hours program by
                                        program (NHES usual weekly hours, PIR schedule
                                        mix, operating weeks) against step 6 (writes
                                        nothing the simulator reads)
code/06_compare_state_shares.R          comparison: care mix of all 52 geographies
                                        (targets, model) against region's NHES and
                                        NSECE shares; total error = dissimilarity index
cluster/acs_p0_shares.R                 p0 children by geography, care type, and hours,
                                        from a full-sample interface (runs on the cluster)
cluster/acs_p0_shares_by_age.R          the same p0 summary split by child age and
                                        kindergarten, for the NHES check (runs on the cluster)
cluster/nsece_shares_by_age.R           NSECE care-type shares of children 0-4 by age,
                                        simulator's classification -- puts the model's
                                        training survey beside NHES (runs on the cluster)
cluster/alpha_drift.R                   paid-centre share of children, re-anchored 2019
                                        vs 2026 baseline (smooth and drawn), per geography,
                                        for one alpha-price setting; rebuilds alpha as the
                                        run did (runs on the cluster)
cluster/baseline_consistency.R          re-anchored 2019 children vs target counts; 2019
                                        employment vs forest base rate; 2026 care hours
                                        with vs without employment shifts (runs on the
                                        cluster; alpha_drift.sbatch <mode> <run>
                                        <calib> <run dir> baseline_consistency)
cluster/state_check_summary.sh          start-of-run state checks per geography from a
                                        state run's log (out/state_checks_<run>.csv)
cluster/acs_kindergarten_flags_2019.py  ACS 2019 children 0-4 already in kindergarten
                                        (IPUMS usa:34 GRADEATT), dropped from the NHES
                                        check because NHES leaves them out
states.R                                one geo_spec() call per geography, with program
                                        counts (funded slots) and Census region,
                                        hand-researched
code/01_nsch_participation_rate.R       derives each state's rate and standard error
code/02_derive_nsece_constants.R        derives every NSECE constant (runs on the cluster)
code/shared_nsece_ages.R                NSECE 2019 centre enrollment by age 0-4, the
                                        age-to-licensing-point and age-to-band maps -- sourced by
                                        resources/supply/state_supply_params.R
code/config_local.example.yaml          copy to state_data/config_local.yaml (git-ignored)
tests/                                  the check suite
docs/archive/                           finished method memos (.pdf)
docs/                                   memo in progress, .tex sources, source register (local only, git-ignored)
raw/                                    the input files, one folder per scope
../resources/<source>/                  national files and reports, grouped by source
raw/NYC/                                New York City report (Health Code staffing rules)
raw/local/                              files too large for git (git-ignored); the price database is on Bouchet
                                        (shared/raw_data/DOL-NDCP/v1/2008-2022/)
out/nsch_participation_rate_by_wave.csv the NSCH derivation, every wave and pool
out/nsch_participation_rate_base.csv    BASE_WAVE (2016-19 pool) -- what load_spec() reads
out/nsece_constants.csv                 the NSECE derivation, full precision
out/nsece_shares_by_age.csv             NSECE care-type shares of children 0-4 by age
                                        (cluster/nsece_shares_by_age.R)
out/state_share_comparison.csv          06_: share by geography and care type, four columns
out/state_share_errors.csv              06_: total error by geography vs NHES, NSECE
out/nhes_comparison.csv                 build vs NHES-ECPP 2019 by region and care type;
                                        US rows add NHES at the ACS age mix
out/nhes_comparison_by_age.csv          the same, ages 0-2 and 3-4
out/nhes_age_mix.csv                    single-year age mix of children 0-4 not in
                                        kindergarten: NHES, model ACS, Census PEP 2019
out/program_hours_sensitivity.csv       14_: children and annual hours by geography and
                                        program, four versions (grid_50, usual_50,
                                        usual_ow, sched_ow)
out/ccdf_subsidy_children.csv           09_: CCDF children 0-4 by state (weighted)
out/idea_preschool_special_education.csv 10_: IDEA children 3-5 by state (raw counts)
out/ndcp_price_comparison.csv           11_: NDCP price ($/week) and model high-priced
                                        share by geography and age group
out/ndcp_price_correlation.csv          11_: state correlation of the two, by age group
out/licensing_ratio_change_2014_2017.csv licensing ratios and staffing indices, 2014
                                        vs 2017, both at 3 age points
                                        (resources/supply/licensing_ratios_2017.R)
out/licensing_ratio_change_3pt_5pt.csv  2017 centre ratios at 5 age points, centre index
                                        at 3 vs 5 points (same script)
../config/state/demand/<GEO>.csv        the output: the demand targets the simulator
                                        reads
../config/state/demand_counts/<GEO>.csv children and part-time children behind each
                                        target (00 writes;
                                        cluster/baseline_consistency.R reads)
out/alpha_drift_<mode>*.csv             cluster/alpha_drift.R: 2019-to-2026 paid-centre
                                        share per geography, summary, provenance
out/state_checks_<run>.csv              cluster/state_check_summary.sh on a run log
```

## Adding a geography

`states.R` is **data, not logic**: one `geo_spec()` call per geography. Copy the closest existing one.

1. Add to `states.R`: one `SPEC_<GEO> <- geo_spec(...)` call (helper in
   `00_build_demand_targets.R`) with `geo`, `name`, `fips`, `region`, and the
   three program counts `prek`, `head_start`, and `early_head_start`, each with its source
   beside it. A sub-state geography also passes `county_fips`, which selects
   its population, and `counties`, the same counties by name for the files
   with no FIPS, like NYC (see `SPEC_NYC` in `states.R`). The simulator needs a matching entry in
   `SUBSTATE_GEOGRAPHIES` (`src/shared_functions/constants.R`). Do NOT write
   `nonparental_rate` -- `load_spec()` fills it in live; see below.
2. `GEOGRAPHIES` in `00_build_demand_targets.R` takes every code in
   `STATE_POSTAL_TO_FIPS` and `SUBSTATE_GEOGRAPHIES`, so it needs no edit.
3. A sub-state geography takes its parent state's NSCH rate (`parent_geo()`),
   so `01_nsch_participation_rate.R` needs no change.
4. `Rscript state_data/code/00_build_demand_targets.R --geo <GEO> --check`

A geography needs three things:

- **A Census region**, which selects every NSECE constant.
- **Program counts** for step 4 — public pre-K (CCD), Head Start, and Early
  Head Start (NIEER funded slots), hand-researched with a source next to each
  number.
- **A measured non-parental rate** from NSCH `k6q20`, which covers all 51:

```
filter k6q20 in (1,2), sc_age_years <= 4, weight by fwc, subset fipsst
```

### How `load_spec()` reads the derived rate

`nonparental_rate` is not hand-researched -- it is the output of
`01_nsch_participation_rate.R`, a single deterministic pass over raw data with
no per-geography judgment call. Keeping a hand copy in `states.R` bought
nothing but a second place for it to drift, so `load_spec()` reads it live,
after sourcing the state file: that geography's row of
`out/nsch_participation_rate_base.csv` (`BASE_WAVE`, the 2016-19 pool). NYC
has no NSCH records of its own, so `load_spec('NYC')` reads NY's row.

The `out/` file ships committed, so a normal build never regenerates it.
Rerun `01_nsch_participation_rate.R` only when the raw NSCH data changes.

## Sources

Nothing downloads at build time: each file is fetched by hand and held in
`raw/`, so a build is reproducible offline.

### File naming

```
raw/<SCOPE>/                <SCOPE>_<SOURCE_KEY>__<content>__<vintage>.<ext>
raw/local/            files too large for git (git-ignored); large shared files live in
                      Bouchet shared/raw_data/ (DOL-NDCP)
cluster/              scripts that derive a raw/ summary from cluster data
```

- `SCOPE` is `US` for a national file, or the state's postal code. The
  file goes in the folder of its scope; `NYS_` and `NY_` files go in `raw/NY/`.
- `content` is lowercase snake_case.
- **`vintage` is the year the data describes, never the download date** — ISO
  where a month matters, a school year as `2018-19`.
- Double underscores separate the fields, so the name parses back apart.

Base year is **2019**. Where a source's nearest vintage differs, the table says
so.

### Inputs

| Input | Source | Held | Vintage | Feeds |
|---|---|---|---|---|
| Population 0–4 | Census Population Estimates, county by age | `resources/census_population/US_PEP__…` | 2019 | step 1 |
| Participation rate | NSCH topical file, question `k6q20` | `resources/nsch/US_NSCH__…` (2018, 2019, 2020) | 2019 | step 1 |
| NSECE constants | NSECE household file, ICPSR 37941 dataset 0009 | cluster only (licensed) | 2019 | steps 2, 4, 5, 6 |
| Public pre-K | NCES Common Core of Data, pre-K membership by county | `resources/ccd/US_CCD__…by_county…` | 2018–19 | step 4 |
| California and Michigan public pre-K | NIEER, *State of Preschool 2019*, state pages (CA: State Preschool Program only) | constant | 2018–19 | step 4, CA and MI |
| Head Start | NIEER, *State of Preschool 2019*, Appendix B | constant | 2018–19 | step 4 |
| Early Head Start | NIEER, *The State(s) of Head Start and Early Head Start* (2023) | constant | 2018–19 | step 4 |
| Slots to enrolled children | OHS PIR 2018–19: Services Snapshots, Report to Congress Appendix C | constant, `00_build_demand_targets.R` | 2018–19 | step 4 |
| Head Start in district schools | ACF Head Start service locations × CCD pre-K by school × CCD directory × NCES EDGE geocodes | constants in `states.R`; historical matching inputs are not shipped with the build | 2026, 2018–19 | step 4 deduction; NYC shares |
| NIEER Head Start / state pre-K overlap | NIEER, *State of Preschool 2019*, Table 4 less Appendix B | `resources/nieer/US_NIEER__state_of_preschool_yearbook__2019.pdf`, constant | 2018–19 | step 4 deduction: NIEER overlap share |
| Head Start program options | OHS PIR 2018–19 Services Snapshots (HS, EHS, AIAN): home-based and family child care funded slots | constant, `00_build_demand_targets.R` | 2018–19 | step 4 |
| Five-year-olds in programs | NHES-ECPP 2019 public-use file, national share per program; NYC 0 (DOE calendar-year enrolment, 45 CFR 1302.12(b)) | `resources/nhes/US_NHES__…` → constant; `raw/NYC/NYC_DOE__…` | 2019 | step 4 |
| Base-year wage ratio | BLS OES May 2019, state / national: hourly medians of childcare workers (39-9011), preschool teachers (25-2011), and preschool and childcare administrators (11-9031), mixed at national ACS shares of each education group's workers in each occupation; NYC = New York MSA 35620 | `resources/bls_oews/US_BLS_OEWS__childcare_occupation_wages__2019.csv` → `resources/supply/state_childcare_wage_ratios_oes.csv` (`resources/supply/oes_childcare_wage_ratios_2019.R`) | May 2019 | step 7 |
| National supply parameters | Simulator's NSECE calibration | `resources/supply/US_NSECE__national_supply_parameters__2019.yaml` | 2019 | step 7; simulator base-year labor |
| State employment paths | ACS state age-sex composition × CBO national cell rates, scaled to the national rate | `resources/epop/US_ACS__epop_by_state_sex_age__2006-2024.csv`, `resources/epop/epop_projections.csv` | 2006–2024 composition | step 8 |
| Center enrollment by age | ACF, *Measuring Supply Capacity at Center-based CCEE Programs* (2023), Exhibit 1, NSECE 2019, ages 0–4 | constant, `code/shared_nsece_ages.R` (with the age-to-licensing-point map) | 2019 | supply centre staffing index weights |
| Licensing ratios | Reviewed October 1, 2019 ordinary center ratio bands, month-weighted by year of age, and home capacity excluding school-age additions; 2017 study retained as comparison baseline | `resources/licensing/licensing_2019_evidence.csv`, `licensing_2019_center_bands.csv` → `resources/supply/licensing_ratios_reviewed_2019.csv` (`resources/supply/00_update_state_supply.R`) | October 1, 2019 | supply staffing index; home input is one-adult, mixed-age capacity with an infant present, assistant configurations retained separately |
| CCDF subsidy records | ACF-801 FY2019 public-use sample, ICPSR 38677 (child, family, setting, pooling factor) | `resources/ccdf/US_ICPSR38677__ccdf_acf801_*__FY2019.rda` | FY2019 | diagnostic only (`code/09_`) |
| Preschool special education | IDEA Part B Child Count and Educational Environments, Table 12, ages 3-5 | `resources/idea/US_IDEA__child_count_educational_environment_age_3_5__2018-19.xlsx` | 2018–19 | diagnostic only (`code/10_`) |
| Centre prices by county | DOL Women's Bureau NDCP 2008-2022 county file, STUDYYEAR 2019: MCINFANT, MCTODDLER, MCPRESCHOOL, five-digit imputation flags; definitions from the ICF technical report | `resources/ndcp/US_DOL__national_database_childcare_prices_county__2008-2022.xlsx` (git-ignored, from Bouchet), `resources/ndcp/US_DOL__ndcp_technical_report__2020-10.pdf` | 2019 | diagnostic only (`code/11_`) |

A **file** is read by the code. A **constant** is read by hand once and written
into `states.R` next to its provenance comment.

### NSCH — the participation rate

`k6q20` asks whether the child receives at least 10 hours a week of care from
someone other than a parent or guardian. The prompt names every arrangement
type and never asks which one applies.

The build reads each rate from `out/nsch_participation_rate_base.csv`.
`01_nsch_participation_rate.R` derives that file from the survey:

```bash
Rscript state_data/code/01_nsch_participation_rate.R
# writes out/nsch_participation_rate_by_wave.csv and
# out/nsch_participation_rate_base.csv
```

The error uses the **survey design** — strata `FIPSST × STRATUM`, PSU `HHID`,
weight `FWC`, each state read as a domain of the national design.

The 2019 file is the Census re-release (`nsch_2019e_topical`, 2024), all 51
states. An earlier copy held only six states; its 3,545 records match the
re-release on every column the build reads, so no rate or error moved.

The build pools the four pre-COVID waves, 2016-2019 (`BASE_WAVE` in
`01_nsch_participation_rate.R`). One wave is about half noise: in 2019 the
state rates spread 7.8 points (standard deviation) with a median design error
of 5.8 points, so only 46% of the spread is real. The pool cuts the median
error to 2.8 points (86% real). The four waves are flat -- state means 54.0,
54.5, 54.2, and 54.9% -- so the pool moves the level little (54.4% against
54.9%). No pre-COVID window centred on 2019 exists: 2020-2022 were fielded
through or after the pandemic (state means 48.2, 48.7, and 51.7%), and from
2023 NSCH drops the question. The 2020-2022 files are held for that check
only.

State cells run 505–954 records (unweighted children, 2016-19 pooled). NSCH has no
sub-state identifier, so NYC takes the New York State rate as is
(`nsch_rate_of()` in `00_build_demand_targets.R`).

### NSECE — the regional constants

The build uses only `NSECE_UNIVERSE_RATIO` (step 1), in
`00_build_demand_targets.R`: one national five-hour ratio for every geography.
The build does not scale it by a geography's predicted part-time share, so the
targets do not depend on the forest or the income-rank scheme.
`NSECE_SHORT_SHARE_OF_PT` (NSECE children
under 10 total hours a week over children part-time in the simulator's sense,
primary hours under 20) stays for `05_compare_nsece_shares.R`. The regional
shares live in `05_compare_nsece_shares.R`, which uses them for comparison only.

The formal share, the unpaid-center share, the center-home split, the informal
mix, and the weekly hours all come from the NSECE 2019 household file,
child-weighted, by Census region. NSECE carries no state, so region is the
finest geography it offers.

**NSECE is not in this repository.** ICPSR 37941 is licensed and lives on the
cluster at `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1`. Pass
`--nsece <dir>` or set `NSECE_ROOT`, and submit with `sbatch` — the file is
117 MB and several thousand columns wide.

```bash
Rscript state_data/code/02_derive_nsece_constants.R --nsece <dir>
```

It writes each constant with its unweighted record count to
`out/nsece_constants.csv`, over two universes:

- **Universe A** — every child 0–4 with a regular non-parental arrangement,
  NSECE's own five-hour screener. With reference-week hours only, it
  reproduced NSECE's published figures exactly; the counting rule below
  moves it off them. **This is the universe the build uses**,
  because the simulator sets no hours threshold of its own.
- **Universe B** — the same children whose regular non-parental care totals at
  least 10 hours a week, school excluded. This matches the NSCH threshold.
  `NSECE_UNIVERSE_RATIO` carries the NSCH rate from B up to A; B is otherwise
  used only for sensitivity.

**Which arrangements count.** 02 and the simulator (1a) use the
same rule. A provider with `HH9_REGCARE` = 1
and positive reference-week hours counts. A type 8 (type unknown) row counts as
Other. A payment amount the file does not hold takes an unpaid label. 02 and 1a
make one label draw.

A regional cell under 30 records takes the national value; only Unpaid
Home-Based falls below it, in all four regions (Northeast 6, Midwest 6, South
7, West 19 records).

`out/nsece_constants.csv` is the only on-disk record of where the constants in
`00_build_demand_targets.R` come from, and it arrives by hand because NSECE
lives on the cluster. `check_constants_match_derivation.R` compares every constant back to
its universe-A row, value and record count, so the pair cannot drift apart
unnoticed. **Refresh the file whenever `02_derive_nsece_constants.R` is rerun.**

### Program counts — step 4

**Why step 4 uses program counts.** Public pre-K enrolment and Head Start
funded slots are *published, state by state*, so this is the one care type
whose count comes from a state source rather than from the share model.
`program_children()` keeps only children who are in a center, are 0–4, and
are counted once. `build_geo()` prints each line:

```
program children = public pre-K + Head Start + Early Head Start   (enrolled children)
                 - Head Start and EHS children in the home-based or family child care option
                 - pre-K and Head Start children aged 5 or older
                 - Head Start children whom the pre-K count already holds
```

| Geography | Enrolled | Not in a center | Age 5+ | Head Start in pre-K | Program children |
|---|---|---|---|---|---|
| KY | 42,919 | 1,245 | 8,185 | 698 | 32,790 |
| NJ | 67,166 | 1,433 | 14,024 | 3,872 | 47,837 |
| NY | 171,762 | 4,554 | 15,199 | 9,717 | 142,292 |
| NYC | 91,379 | 2,229 | 0 | 3,599 | 85,551 |
| VA | 47,105 | 1,063 | 9,411 | 2,087 | 34,543 |

Every figure is a count of children from administrative sources (Head Start
funded slots converted to enrolled children), not a survey estimate. The
age 5+ column is the one place a survey enters: a national NHES share
(below). The comparison script
(`05_compare_nsece_shares.R`) sets the result beside the regional NSECE
unpaid-center share.

The counts are not a clean measure of the care type, and the memo says so:
they measure enrolment and funded slots, not the primary arrangement (a pre-K
child also in paid care is Center-Based), and they include part-day children
under the 10-hour line. They are used anyway because being state-specific
matters more here than being exactly on concept — this is the one care type
where a state source exists at all.

**CCD — public pre-K.** Pre-K membership, school by school, summed by county,
for 2018–19, on or near October 1. It counts a **grade**, not an age, so it
includes some five-year-olds (removed below, "Ages 0–4"), and it counts more
than state pre-K programs alone — in Kentucky 28,465 against the 21,351 NIEER
reports for the Kentucky Preschool Program, because the grade also holds
district-funded places and preschool special education.

- **California has no CCD pre-K rows** for 2018–19. Its pre-K count is NIEER's
  State Preschool Program, 140,920 children. It excludes preschool special
  education (52,307), which CCD counts elsewhere. I leave out transitional
  kindergarten (104,694): TK is for children born 2 September to 2 December,
  so nearly every TK child is 5 by the middle of the year (NIEER CA page,
  printed p.58).
- **Michigan reports no CCD pre-K count** for 2018–19: 1,064 of its 1,219 pre-K
  school rows are flagged "Not reported", and the rest add up to 0. Its pre-K
  count is NIEER's: Great Start Readiness Program, 37,140 children (MI
  profile). GSRP includes community-based sites.
- **Oregon (216) and Pennsylvania (8,655)** report every school, so their low
  CCD counts are real — most of their state pre-K runs outside public schools.

**NIEER — Head Start and Early Head Start.** Both figures are **funded
enrollment slots**, which NIEER takes from ACF. They are not counts of unique
children, and they are not observed by age. NIEER's methodology (*The
State(s) of Head Start and Early Head Start*, 2023, methods note):

> Funded enrollment was not provided by single year of age. Therefore,
> federally-funded enrollment by age was estimated based on the proportion of
> the cumulative enrollment that was either under 1-, 1-, 2-, 3-, 4- or
> 5-years-old. [...] Because HS is intended to serve 3- and 4-year-olds, we
> counted 2-year-olds in HS as 3-year-olds; and we counted 5-year-olds as
> 4-year-olds. Similarly, since EHS is intended to serve infants and toddlers,
> we considered 3-year-olds enrolled in EHS to be under 3 years old.

So the Head Start count includes some five-year-olds by construction. NHES
sizes them ("Ages 0–4" below). The specs added in September 2026 (32 states
and DC) take their counts from the same three sources, read by the functions in
`tests/check_program_counts_match_sources.R`: CCD pre-K by county, the NIEER
Appendix B Head Start row (federal, AIAN, and Migrant and Seasonal slots, ages
3 and 4), and the NIEER 2023 Early Head Start bar.

**Slots to enrolled children.** The specs keep Head Start and Early Head Start
as funded slots, as the sources print them; `program_children()` converts them
with three national figures from the 2018–19 Head Start Program Information
Report (PIR). No public source gives them by state: state PIR data is available
only on request, which this build does not use.

- **Pregnant women.** Early Head Start funded slots include slots for pregnant
  women: 5,988 of 166,693 (3.6%), "Total Funded Pregnant Women" in the EHS
  Services Snapshot. The build removes that share from every state's EHS count.
- **Enrolled children per funded slot.** Children enrolled at the end of the
  program year are cumulative enrollment less the children who left and did not
  re-enroll (Biennial Report to Congress FY2019, Appendix C, A.15, A.19, and
  A.20): Head Start 775,902 − 112,939, EHS children 230,069 − 76,419. The
  funded side is the HS and EHS snapshots (675,734 and 166,693 − 5,988), plus
  AIAN child slots (22,894 − 282). The AIAN snapshot gives one AIAN total, so I
  split it by the AIAN cumulative age mix (0–2: 6,433, 3–5: 18,648). The
  result: **0.957 for Head Start, 0.923 for Early Head Start** (0.890 per EHS
  funded slot, after the pregnant-women share). The split matters: all AIAN
  slots to one program gives Head Start 0.949 to 0.981 and EHS 0.838 to 0.956.
  The age split matches the program definitions the build uses (Head Start
  3 to kindergarten, EHS under 3).
- **Rejected: cumulative over funded** (Head Start 1.12, EHS children 1.38).
  Cumulative enrollment counts the children who left and the children who took
  their places, so it is a flow over the year, not a count at one time.
- **Rejected: KIDS COUNT table 9786** (Head Start enrollment by age). It is
  cumulative, and it puts pregnant women in its 0-to-2 group.
- The enrolled count is at the end of the program year. CCD counts children
  on or near October 1.

**Center care only.** Head Start funds four program options. Two are not
center care, so I remove them from both Head Start counts:

- **Home-based option.** A home visitor comes once a week. The child stays in
  the parent's care. The child stays inside the NSCH total, in whatever care
  type the p0 mix gives.
- **Family child care option.** The child is in a provider's home. The p0 mix
  places these children among the five other care types.

National funded slots, 2018–19 Services Snapshots: Head Start 11,043
home-based + 1,277 family child care of 675,734; Early Head Start 57,221 +
5,722 of 166,693 − 5,988 pregnant women; AIAN 1,493 + 16, split by the AIAN
age mix as above. The shares are **1.9% of Head Start and 38.0% of Early Head
Start**. No public file gives the options by state. The center-based and
locally designed options stay. Childcare-partner centers (Appendix C A.10,
48,006 positions, all programs) also stay: the child is in a center, and Head
Start services carry no fee.

**Ages 0–4.** The population is children 0–4. The pre-K grade and the Head
Start 3–4 slots also hold children who turn 5 during the program year. I
remove one national share per program: the NHES-ECPP 2019 share aged 5 or
older on 31 December 2018 (`resources/nhes/`; children not yet in
kindergarten, in a center program now, age 3 or older, weighted by `FEWT`).
It is 24.4% for pre-K (483 records) and 10.7% for Head Start (247). NYC is 0
for both programs: the DOE enrolls by calendar year (2019 kindergarten guide,
students born in 2014), and Head Start age follows the public-school date
(45 CFR 1302.12(b)). New York State applies the national share to the rest of
the state only. Both pre-K sources get the same share, so the larger-of rule
gives the same answer before or after this step. Early Head Start has no
five-year-olds (EHS snapshot). `check_constants_match_derivation.R` recomputes
the NHES shares.

**Head Start and pre-K can count the same children.** A state can fund pre-K
places inside Head Start, and a district can run Head Start in a school. Step 4
deducts the Head Start children that the pre-K figure already holds. The
deduction is one share of a state's Head Start slots
(`overlap_branch()` in `00_build_demand_targets.R`).

*State of Preschool 2019*, Table 4 (p.28), gives 3- and 4-year-olds in pre-K
plus special education, without and with Head Start, each child counted once.
Appendix B Head Start less the Table 4 increment is the Head Start count in
state pre-K. Table 4 counts the overlap with state pre-K in every setting, and
CCD counts public schools only, so the build uses a NIEER share only where it
takes the all-settings count.

Eighteen states report an overlap in Table 4 (NY 11,136, MI 5,840, NJ 4,621,
NC 4,510, WV 4,358, GA 4,339, IL 4,041, TN 3,579, MA 2,834, DC 2,150, IA
2,077, NE 1,946, AL 1,746, MS 931, CT 843, ME 800, KY 783, MD 436 slots).

- **NIEER overlap share** (every other geography where the build takes the
  all-settings count and Table 4 marks an unduplicated count: AL, CT, GA, IA,
  MA, MD, NC, NJ, NY; NYC takes NY's). Same source and universe as the pre-K
  count.
- **Fallback** (the other 40 geographies): 0.263, the Head Start-slot-weighted
  mean NIEER overlap share of those nine states (`pooled_overlap_share()`).
  It assumes overlap there is like overlap in the nine states.

**Overlaps not removed.** Each needs a count that no held file gives:

- **West Virginia collaborative pre-K.** WV CCD (15,479) is above all WV state
  pre-K (13,534), so CCD can hold the collaborative classrooms run by Head
  Start grantees. Table 4 reports 4,358 Head Start slots in WV pre-K; the
  own-school share covers 1,108. The rest, up to 3,250 slots, can be a double
  count. To remove it, I need a state count of Head Start children in WV
  district membership.
- **Head Start children with an IEP.** 13.2% of Head Start children have an
  IEP (HS snapshot, 99,961). A district that serves such a child can enter
  the child in its pre-K membership. No file gives that count.
- **Preschool special education with services only.** CCD's pre-K grade can
  hold children who get only itinerant services (for example, speech therapy)
  and are in no regular care arrangement. IDEA Table 12, fall 2018
  (`code/10_idea_preschool_special_education.R`), gives the size: 802,486
  children 3-5 in 50 states and DC (raw counts; WI flagged), of whom 536,450
  attend a regular early childhood program, 199,906 are in a separate class or
  school, and 66,130 get services at home or at a provider. Table 12 has no
  age split and no program type, so I cannot tell which regular-program
  children step 4 already holds. In the 18 states that take NIEER Table 2
  (no special education), separate-class children are not in the count:
  79,560 children 3-5, 4.3% of national Unpaid Center-Based children (FL
  12.6%, NJ 16.4%, NY 11.9% of their own). I do not add them: Table 12 has no
  age split, and many attend part-day.
- **Paid care alongside the program.** A pre-K or Head Start child whose
  family also pays for a center is Center-Based under the primary-type rule.
  The program count puts the child in Unpaid Center-Based. Childcare-partner
  families who pay a subsidy copay for extra hours are the same case.
- **Program hours.** Step 6 gives every Unpaid Center-Based child the
  share model's hours: 1,557 hours a year per child nationally (about 31
  hours a week for 50 weeks). The programs run fewer hours.
  `code/14_program_hours_sensitivity.R` builds hours for each program from
  the step 4 net children (each child once), and writes
  `out/program_hours_sensitivity.csv`:
  - **Usual weekly hours** come from NHES-ECPP 2019 (`CPHRS`, weight `FEWT`,
    per-day answers corrected -- see NHES-ECPP below): pre-K 23.3 (ages 3-4,
    354 records), Head Start 26.8 (ages 3-4, 214 records), and Early Head
    Start 29.0 (ages 0-2, 74 records). As reported, 19.3, 22.8, and 24.3. These are
    parent reports of usual hours, not an attendance rate. No public
    attendance rate exists at this vintage, so I keep scheduled hours as
    scheduled.
  - **Scheduled weekly hours** for Head Start and Early Head Start come from
    the PIR 2018-19 center-based option mix (part-day 3.5 hours a day,
    full-day 6.5, full working day 10; 4 or 5 days a week): Head Start 29.2,
    Early Head Start 39.1. Pre-K has no enrolment-weighted schedule (NIEER
    prints program minimums), so pre-K uses the NHES hours.
  - **Operating weeks**: pre-K 36 (a 180-day school year; 44 of 62 NIEER
    programs run a school year). Head Start and Early Head Start take 32
    weeks for part-day slots, 36 for other full-day slots, and 50 for
    full-calendar-year slots: Head Start 35.9, Early Head Start 40.8.
  - **Result**, national Unpaid Center-Based hours against the build:
    usual hours × 50 weeks -34.9% (-40.7% DC to -23.0% WY); usual hours ×
    operating weeks -52.8% (-56.9% to -43.4%); scheduled hours × operating
    weeks -46.9% (-52.5% to -26.2%).
  - **Why not in the build.** The simulator holds each child at 10 or 40
    hours a week for 50 weeks, and re-anchoring moves one constant per care
    type. A lower hours target therefore reads as fewer Unpaid Center-Based
    children, not as shorter hours, against step 4's own count. The other
    five types and the NSECE calibration also count usual weekly hours × 50,
    so a school-year cut for the programs alone is not like for like. The
    schedule mix is also national only, and the NHES Early Head Start cell
    is small. The gap says that p0 puts too many program children at 40
    hours; I record it here and do not change the build.
- **Subsidised children with a $0 copay.** The opposite case: a CCDF child at
  a priced center whose family pays nothing is Unpaid Center-Based demand, and
  the program count leaves the child out. CCDF ACF-801 FY2019
  (`code/09_ccdf_subsidy_children.R`, weight `A_FCHWT`) gives 218,276 such
  children 0-4 in an average month (weighted), 11.8% of national Unpaid
  Center-Based children (0.3% IL to 52.8% DE; GA not in the file). I do not
  add them. A $0 copay does not mean the family pays nothing (a provider can
  charge above the subsidy rate), CCDF also funds wraparound hours for pre-K
  and Head Start children, and the targets are already 33% above the NSECE
  unpaid-center count nationally.
- **Five-year-olds by state.** One national share per program; state
  kindergarten cutoffs differ (the South has more early cutoffs), and NYC is the
  only geography with a documented source rule.

**The NYC share.** ACF's service-location file gives funded slots by site.
NYC's Head Start and Early Head Start counts are the city's shares of New York
State slots at open sites in that file, applied to the 2018–19 NIEER state
figures: 7,064 of 20,518 Head Start slots (program types 1, 3, 5) and 4,426 of
8,590 Early Head Start slots (types 2, 4, 6). The file is the same 2026-09-23
export (`resources/head_start/US_HSSL__...`), so this assumes the city's
shares were the same in 2019. `tests/check_program_counts_match_sources.R`
recomputes both.

### Replacing NSECE — the candidates

NSECE is 2019-only, licensed, and carries no state, so every behavioural share
here is regional. I searched for a state source that can replace it. None of
the public candidates can.

**Every candidate is checked for a part-time / full-time marker first.** The
simulator holds a child at 10 or 40 hrs/week and nothing between, so the build
states its target on that cut. A source that cannot say which side a child sits
on cannot feed the grid, whatever else it supplies — an hours source without
the marker gives a mean, and a mean implies the wrong child count once it is
placed on a two-point grid; a price source without it prices a part-time child
at the full-time rate, and that error runs straight into the equilibrium,
because price drives it. Five of the 11 NSECE constants need the marker: the
four hours constants and `price_band_share`. NSECE supplies the marker, cutting
at 20 hrs/week, the same cut `1a_nsece_processing.R` uses.

**Nothing public measures hours at state level.**

| Source | State? | Hours? | Verdict |
|---|---|---|---|
| NSECE 2019 | no (region only) | yes | what the build uses |
| NHES-ECPP 2019 public-use | **no** (region only) | **yes** | independent regional check, not a state source |
| NHES-ECPP restricted-use | yes | yes | **needs an IES licence** — the one route that would work |
| SIPP 2018–2023 | yes | **no** | rejected below |
| NDCP 2008–2022 | county | full-time only | rejected below |
| CCDF ACF-801 FY2019 (ICPSR 38677, held) | yes | yes (monthly hours) | subsidy children only (757,000 children 0-4, weighted); diagnostic in `code/09_` |
| NSECE restricted-use (ICPSR 38445) | unconfirmed | yes | ICPSR does not document its geography; would need to ask |

**SIPP — rejected.** SIPP identifies every state and asks about child care, but
it cannot replace the regional constants. In the pooled 2018–2023 public-use
files, SIPP has no hours variable, its one price (`TPAYWK`) covers all of a
parent's children together, and only 108 of the 306 state × care type cells
(50 states and DC) reach 30 distinct children.

**NDCP — rejected as the price source.** Prices in the build are wholly
national. The DOL Women's Bureau National Database of Childcare Prices gives
county prices by provider type and age, but every price is a full-time weekly
price, so the build's 10-hour point has no NDCP price. Each state also defines
"full-time" itself (Maine 21–30 hours, Indiana 35, Michigan 45, New Jersey
51.7), so a state NDCP price mixes that definition with real price variation.

#### NHES-ECPP — real hours, but region only

NHES-ECPP 2019 (NCES) is the one public source that measures hours of care
per arrangement, so it gives a part-time share at the build's 20 hrs/week cut.
Its finest geography is the Census region (`CENREG`). The public-use file has
no state and no county, so it cannot give state values. State exists only in
the IES restricted-use file, which needs a licence.

NHES hours can be cut at any threshold, so NHES can measure the universe ratio
that `NSECE_UNIVERSE_RATIO` carries (**1.0995**, universe A over universe B).

**Per-day answers.** Some NHES parents give hours per day in the weekly-hours
items (`RCHRS`, `NCHRS`, `CPHRS`). For example, 111 center records for
children 0–4 report 8 hours a week on 5 days. I read an answer under 2 hours a
day on 4 or more days as hours per day, and multiply it by the days
(`nhes_fix_per_day_hours()`, `code/shared_setup.R`). Short weeks on 2–3 days
stay as reported, because part-day preschool runs that way. The correction
changes 324 center, 168 relative, and 61 non-relative records: 13.0%, 12.9%,
and 9.0% of the weighted children 0–4 with each arrangement. Every script that
reads NHES hours applies it (`07`, `12`, `14`, and `06` through `07`).

The evidence for this reading:

- The questionnaire (https://nces.ed.gov/nhes/pdf/early/2019_ecpp.pdf, Q7–8,
  Q25–26, Q47–48) asks "How many
  days each week" and then "How many hours each week". No item records the
  unit of the hours answer. `RCUNIT`, `NCUNIT`, and `CPUNIT` are the unit of the
  cost answer.
- The corrected answers are reported, not imputed: NCES imputed 8 of the 324
  center records (2.5%, against 1.5% of all center records). They occur at
  the same rate in each survey mode (`MODECOMP`, 12–14%).
- The next question asks how well the arrangement covers the hours needed for
  work. Corrected center records answer like full-time children: 53.7% "very
  well" and 3.7% "not well", against 67.9% and 1.3% at 30 or more hours, and
  20.8% and 22.8% for real short weeks (1–3 days, under 10 hours). Relative
  and non-relative care show the same pattern.

- **Universe ratio.** As reported, NHES puts the five-hour universe at 1.194
  times the ten-hour one (`CAREHOURX`, summed per-arrangement hours give
  1.194 too). Corrected, it is **1.077**, 2.0% below NSECE. The per-day answers
  made most of the old 11% gap: they put short weeks over the 5-hour line and
  under the 10-hour line. The build keeps the NSECE ratio.
- **Part-time share.** Corrected NHES part-time shares agree with NSECE for
  the three largest care types: paid center 27.3% against 24.5%, unpaid center
  28.9% against 31.3%, and other unpaid 27.1% against 27.0% (37.0%, 42.3%, and
  38.4% as reported). Other paid and paid home-based still differ by about 10
  points, in opposite directions, on 569 and 190 NHES records.
- **NSECE long weeks.** Some NSECE parents report 168 hours a week for a
  relative who cares for the child at home. Summed within the type, a few
  children reach more than 168 hours (24 child-type sums, all eight largest
  Other Unpaid). These rows are full-time on any reading, and 8 priced rows are
  over 80 hours (under 2% of weight in each type), so labels and prices barely
  move. The mean weekly hours of other unpaid care are high for this reason
  (43 in NSECE, 29 in NHES).

### Reference documents

National reports sit with their data in `resources/<source>/` at repository
root. State and city reports remain in `raw/<SCOPE>/`. Only the
documents behind numbers the build uses are held. Removed material stays in
git history: documents read only for context, method, or a rejected source
(removed 2 October 2026), and the licensing-register and industry-count
comparisons with their data and reports (commits `6dff713` and `7ffdb2d` hold
them).

| Folder | File | What it is |
|---|---|---|
| `resources/nieer/` | `US_NIEER__state_of_preschool_yearbook__2019.pdf` | NIEER yearbook 2019: Head Start ages 3–4, Table 4 overlap (NIEER share), state pre-K totals, CA and MI pre-K, CA TK birth dates |
| `resources/nieer/` | `US_NIEER__states_of_head_start_and_early_head_start__2023.pdf` | NIEER State(s) of Head Start: Early Head Start 2018–19 by state |
| `resources/head_start/` | `US_OHS__services_snapshot_ehs__2018-19.pdf` | OHS EHS Services Snapshot, national, 2018–19: EHS funded slots, pregnant-women slots, home-based and family child care slots, cumulative enrollment |
| `resources/head_start/` | `US_OHS__services_snapshot_hs__2018-19.pdf` | OHS Head Start Services Snapshot, national, 2018–19: Head Start funded slots, home-based and family child care slots |
| `resources/head_start/` | `US_OHS__services_snapshot_aian__2018-19.pdf` | OHS AIAN Services Snapshot, 2018–19: AIAN funded slots, home-based and family child care slots, and age mix, for the HS/EHS split |
| `resources/head_start/` | `US_OHS__biennial_report_to_congress__FY2019.pdf` | Appendix C (national PIR) feeds the enrolled-per-slot factors. Its state HS + EHS funded enrollment was evaluated as an Early Head Start cross-check and rejected: it matches only where every Head Start slot is ages 3–4 |
| `resources/nsece/` | `US_NSECE__measuring_supply_capacity_centers__2023-12.pdf` | ACF report on center enrollment and capacity, NSECE 2019: Exhibit 1 enrollment by age 0–4, the supply staffing index weights |

## Checks

Six scripts, all exit 1 on structural failure. Cross-calibration NY/NYC gaps
are sensitivities; add `--strict-additivity` to enforce the historical gap limits. The NYC/rest-of-state check also writes
`out/nyc_rest_of_state_validation.csv`; the other five checks are read-only.
`load_spec()` (`00_build_demand_targets.R`) reads the NSCH rate live from
`out/`, so there is no hand copy left to go stale -- see "How `load_spec()`
reads the derived rate" above.

```bash
Rscript state_data/tests/check_outputs_are_current.R       # demand targets are what the code produces
Rscript state_data/tests/check_sources_are_ascii.R         # no literal depends on locale
Rscript state_data/tests/check_constants_match_derivation.R # constants match out/nsece_constants.csv
Rscript state_data/tests/check_program_counts_match_sources.R # program counts match their sources
Rscript state_data/tests/check_nyc_rest_of_state.R          # common-rank partition and cross-calibration sensitivity
python  state_data/tests/check_summarise_shadowing.py      # scans for the shadowing trap
```

**Run `check_outputs_are_current.R` first.** It is the only check that
compares the demand targets in `config/state/demand/` to the code. The other
checks read the committed files as they are, so a stale file can pass them.

## Known traps

- **Source files are read all-character and coerced explicitly.** Type guesses
  vary by locale and by which rows land first. `read_chr()` stops on a CSV row
  with more fields than the header, because that row can put one column's
  value in another.
- **Use the pre-pandemic survey wave.** NSCH 2020 was fielded through the
  pandemic, and its care rates fall 5 to 18 points in KY, NJ, and VA.
- **A non-ASCII literal in the code makes the build locale-dependent.** An R
  source file is only UTF-8 where the machine's native encoding is, so a
  non-ASCII literal can fail to match on a legacy codepage, and nothing errors.
  `check_sources_are_ascii.R` enforces ASCII.
- **A name shadowed inside `summarise()` fails silently.** `summarise(x = sum(x),
  y = sum(x[flag]))` makes the second expression index a scalar, so a rate reads
  exactly 0% or exactly 100% and nothing errors. The same trap divides by a
  scalar: `summarise(w = sum(w), m = sum(x * w) / sum(w))` gives a mean that is
  wrong by a factor of the group size (the OEWS national wage read $611 an hour
  on 1 October 2026). `check_summarise_shadowing.py` scans every R file in
  `state_data/` (incl. `code/` and `cluster/`) and `resources/` for both forms.
  It does not scan `src/`.

## How the simulator reads this

`00_build_demand_targets.R` writes each file once, to
`config/state/demand/<GEO>.csv`, where the simulator reads it. There is no
copy in `out/`.
`read_state_demand_targets()` in `src/shared_functions/adjust_alpha_for_state.R`
takes the same six rows. One Center-Based target covers both of the
simulator's center tiers with one shared adjustment; the model determines the
split between them.

NYC gets a simulator file too. The simulator selects NYC households by
STATEFIP 36 and COUNTYFIP 005, 047, 061, 081, and 085 (`SUBSTATE_GEOGRAPHIES`
in `src/shared_functions/constants.R`). The build selects NYC's population on
the same five codes (`county_fips` in `states.R`). The county names in
`counties` are kept only for the one file that carries no FIPS: the Head Start
site file. A NYC run needs an interface built from a raw
ACS pull that carries `COUNTYFIP`; without one, the run stops at
initialization.

`check_outputs_are_current.R` rebuilds each geography and confirms that its
simulator file equals the rebuild and that the simulator's reader accepts it.

```bash
Rscript src/main.R -r state_targets_smoke    # baseline for KY, NJ, NY, VA
Rscript src/main.R -r state_targets_all      # baseline for all 50 states, DC, NYC
```

`cluster/acs_p0_shares.R` also writes `<out.csv>.nyc_partition.csv`, containing
NY, NYC under parent ranks, true rest-of-state (NYR), and production NYC.
Copy it to `resources/acs/US_ACS__p0_children_nyc_partition__2019.csv` with
its provenance when refreshing the production p0 summaries. The sidecar is
diagnostic only; it never replaces either geography's own production calibration.
