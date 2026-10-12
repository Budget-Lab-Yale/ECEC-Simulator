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

**The method memos, `docs/<date>_summary.pdf`, are the documents to read.** They
say what every number means, where it comes from, and how solid it is. The
memos and their .tex sources are kept outside git; ask Anna for the current
version. This file is the map, and the record of every
source. A machine-readable source register (one row per source and geography,
with its URL, vintage, and retrieval date) is kept with the memos, outside git.

## What is built

Fifty-two geographies: the 50 states, DC, and New York City. The memo
describes five: **KY, NJ, NY, NYC, VA**. **Puerto Rico is not built.** NSCH
covers only the 50 states and DC, NSECE places Puerto Rico in no Census region,
and the simulator's ACS households do not include it. Building it needs a participation rate, a region or a stand-in
for one, a wage, and households. New York is all 62 counties, city
included. New York City is the five boroughs, built on its own. NY includes
NYC. NY minus NYC is close to a rest-of-state build made the same way, within
0.9% of the hours in each care type. The rate and the p0 mix of each are
ratios over its own households, so the difference is not exact
(`tests/check_nyc_rest_of_state.R`).

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
   part-time / full-time by the share model.

**The share model.** Steps 2, 3, 5, and 6 take their shares from the
simulator itself. `cluster/acs_p0_shares.R` sums the calibration's base-year
choice probabilities (p0) over each geography's own ACS households, weighted by
child weight: children 0–4 by care type and by 10 or 40 hours a week. So each
state's mix follows its own income, parent employment, family type, and child
ages, under the national preferences the simulator calibrates. NYC uses the
households in its five counties. The summary is
`raw/US/US_ACS__p0_children_by_care_type_and_hours__2019.csv`, built on the cluster
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
code/08_nieer_prek_sensitivity.R        sensitivity: demand targets with NIEER pre-K and
                                        NIEER Head Start overlap in place of CCD and the
                                        fallback share (writes nothing the simulator reads)
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
cluster/acs_kindergarten_flags_2019.py  ACS 2019 children 0-4 already in kindergarten
                                        (IPUMS usa:34 GRADEATT), dropped from the NHES
                                        check because NHES leaves them out
states/<GEO>.R                          one per geography: a geo_spec() call with program
                                        counts (funded slots) and Census region,
                                        hand-researched
code/01_nsch_participation_rate.R       derives each state's rate and standard error
code/02_derive_nsece_constants.R        derives every NSECE constant (runs on the cluster)
code/shared_nsece_ages.R                NSECE 2019 centre enrollment by age 0-4 and the
                                        age-to-licensing-band map -- sourced by
                                        resources/supply/state_supply_params.R
code/config_local.example.yaml          copy to state_data/config_local.yaml (git-ignored)
tests/                                  the check suite
docs/                                   method memos (.tex, .pdf) and source register (local only, git-ignored)
raw/                                    the input files, one folder per scope
raw/US/                                 national files and reports
raw/NYC/                                New York City report (Health Code staffing rules)
raw/local/                              files too large for git (git-ignored)
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
out/nieer_sensitivity/<GEO>.csv         NIEER sensitivity demand targets, simulator schema
out/nieer_sensitivity_summary.csv       hours by geography and care type, main vs NIEER
../config/state/demand/<GEO>.csv        the output: the demand targets the simulator
                                        reads (the build writes only this copy)
```

## Adding a geography

A state file is **data, not logic**: one `geo_spec()` call. Copy the closest existing one.

1. Write `states/<GEO>.R`: one `SPEC_<GEO> <- geo_spec(...)` call (helper in
   `00_build_demand_targets.R`) with `geo`, `name`, `fips`, `region`, and the
   three program counts `prek`, `head_start`, and `early_head_start`, each with its source
   beside it. A sub-state geography also passes `county_fips`, which selects
   its population, and `counties`, the same counties by name for the files
   with no FIPS, like NYC (see `states/NYC.R`). The simulator needs a matching entry in
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
no per-geography judgment call. Keeping a hand copy in `states/<GEO>.R` bought
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
raw/local/            files too large for git (git-ignored, cluster only)
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
| Population 0–4 | Census Population Estimates, county by age | `raw/US/US_PEP__…` | 2019 | step 1 |
| Participation rate | NSCH topical file, question `k6q20` | `raw/US/US_NSCH__…` (2018, 2019, 2020) | 2019 | step 1 |
| NSECE constants | NSECE household file, ICPSR 37941 dataset 0009 | cluster only (licensed) | 2019 | steps 2, 4, 5, 6 |
| Public pre-K | NCES Common Core of Data, pre-K membership by county | `raw/US/US_CCD__…by_county…` | 2018–19 | step 4 |
| California and Michigan public pre-K | NIEER, *State of Preschool 2019*, state pages (CA: State Preschool Program only) | constant | 2018–19 | step 4, CA and MI |
| Head Start | NIEER, *State of Preschool 2019*, Appendix B | constant | 2018–19 | step 4 |
| Early Head Start | NIEER, *The State(s) of Head Start and Early Head Start* (2023) | constant | 2018–19 | step 4 |
| Slots to enrolled children | OHS PIR 2018–19: Services Snapshots, Report to Congress Appendix C | constant, `00_build_demand_targets.R` | 2018–19 | step 4 |
| Head Start in district schools | ACF Head Start service locations × CCD pre-K by school × CCD directory × NCES EDGE geocodes | `raw/US/US_HSSL__…2026-09-23`, `raw/US/US_CCD__…`, `raw/US/US_EDGE__…` | 2026, 2018–19 | step 4 deduction; NYC shares |
| NIEER Head Start / state pre-K overlap | NIEER, *State of Preschool 2019*, Table 4 less Appendix B | `raw/US/US_NIEER__state_of_preschool_yearbook__2019.pdf`, constant | 2018–19 | step 4 deduction: NIEER overlap share |
| Head Start program options | OHS PIR 2018–19 Services Snapshots (HS, EHS, AIAN): home-based and family child care funded slots | constant, `00_build_demand_targets.R` | 2018–19 | step 4 |
| Five-year-olds in programs | NHES-ECPP 2019 public-use file, national share per program; NYC 0 (DOE calendar-year enrolment, 45 CFR 1302.12(b)) | `raw/local/US_NHES__…` (git-ignored) → constant; `raw/NYC/NYC_DOE__…` | 2019 | step 4 |
| Base-year wage ratio | BLS OES May 2019, state / national: hourly medians of childcare workers (39-9011), preschool teachers (25-2011), and preschool and childcare administrators (11-9031), mixed at national ACS shares of each education group's workers in each occupation; NYC = New York MSA 35620 | `raw/US/US_OES__childcare_occupation_wages__2019.csv` → `resources/supply/state_childcare_wage_ratios_oes.csv` (`resources/supply/oes_childcare_wage_ratios_2019.R`) | May 2019 | step 7 |
| National supply parameters | Simulator's NSECE calibration | `resources/supply/US_NSECE__national_supply_parameters__2019.yaml` | 2019 | step 7; simulator base-year labor |
| State employment paths | ACS state age-sex composition × CBO national cell rates, scaled to the national rate | `resources/epop/US_ACS__epop_by_state_sex_age__2006-2024.csv`, `resources/epop/epop_projections.csv` | 2006–2024 composition | step 8 |
| Center enrollment by age | ACF, *Measuring Supply Capacity at Center-based CCEE Programs* (2023), Exhibit 1, NSECE 2019, ages 0–4 | constant, `code/shared_nsece_ages.R` (with the age-to-licensing-band map) | 2019 | supply centre staffing index weights |

A **file** is read by the code. A **constant** is read by hand once and written
into `states/<GEO>.R` next to its provenance comment.

### NSCH — the participation rate

`k6q20` asks whether the child receives at least 10 hours a week of care from
someone other than a parent or guardian. The prompt names every arrangement
type and never asks which one applies. Wording is in
`raw/US/US_NSCH__topical_variable_list__2019.pdf`.

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

The build uses only `NSECE_UNIVERSE_RATIO` and `NSECE_SHORT_SHARE_OF_PT`
(step 1), in `00_build_demand_targets.R`. The short share is the NSECE
children under 10 total hours a week over the children part-time in the
simulator's sense (primary hours under 20). The build applies it to the
simulator's predicted part-time children of each geography, from the p0
summary, to give each geography a five-hour ratio relative to the nation. The regional shares live in
`05_compare_nsece_shares.R`, which uses them for comparison only.

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
  NSECE's own five-hour screener. It reproduces NSECE's published figures
  exactly, which validates the code. **This is the universe the build uses**,
  because the simulator sets no hours threshold of its own.
- **Universe B** — the same children whose regular non-parental care totals at
  least 10 hours a week, school excluded. This matches the NSCH threshold.
  `NSECE_UNIVERSE_RATIO` carries the NSCH rate from B up to A; B is otherwise
  used only for sensitivity.

A regional cell under 30 records takes the national value; only Unpaid
Home-Based falls below it, in all four regions (Northeast 8, Midwest 11, South
5, West 14 records).

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
  printed p.58). The build used 245,614 (State Preschool + TK) before
  5 October 2026.
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
older on 31 December 2018 (`raw/local/`, git-ignored; children not yet in
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
  It assumes overlap there is like overlap in the nine states. It replaced a
  school-site match (`03_head_start_school_overlap.R`, removed 2026-10-08)
  for consistency: one fallback rule in place of 2026 site data, district
  matching, and a 150 m threshold.

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
  and are in no regular care arrangement. IDEA educational-environment counts
  by state are not held.
- **Paid care alongside the program.** A pre-K or Head Start child whose
  family also pays for a center is Center-Based under the primary-type rule.
  The program count puts the child in Unpaid Center-Based. Childcare-partner
  families who pay a subsidy copay for extra hours are the same case.
- **Five-year-olds by state.** One national share per program; state
  kindergarten cutoffs differ (the South has more early cutoffs), and NYC is the
  only geography with a documented source rule.

**The NYC share.** ACF's service-location file gives funded slots by site.
NYC's Head Start and Early Head Start counts are the city's shares of New York
State slots at open sites in that file, applied to the 2018–19 NIEER state
figures: 7,064 of 20,518 Head Start slots (program types 1, 3, 5) and 4,426 of
8,590 Early Head Start slots (types 2, 4, 6). The file is the same 2026-09-23
export (`raw/US/US_HSSL__...`), so this assumes the city's
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
| CCDF ACF-801 / Table 14 | yes | yes | subsidy children only, and `acf.gov` answers 202 with an empty body from both Windows and Bouchet |
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

Its continuous total hours (`CAREHOURX`) can be cut at any threshold, so NHES
can measure the universe ratio that `NSECE_UNIVERSE_RATIO` carries. Weighted
by `FEWT`, ages 0–4, NHES puts the five-hour universe at **1.194** times the
ten-hour one. NSECE puts it at **1.098** (universe A over universe B), so the
two disagree by 8.7%. With the NHES ratio, total hours rise 8.8–8.9% in the
five memo geographies, and the Unpaid Center-Based child count does not
change. Both ratios use a total-hours cut at 10. NHES enters an arrangement
on a looser screen ("at least once a week"), but a looser screen pushes its
ratio down, not up, so the construct difference does not explain the gap.
The build keeps the NSECE ratio and does not correct one source to the other,
because it is not settled which ratio is right.

### Reference documents

Each report sits beside the data of its scope, in `raw/<SCOPE>/`. Only the
documents behind numbers the build uses are held. Removed material stays in
git history: documents read only for context, method, or a rejected source
(removed 2 October 2026), and the licensing-register and industry-count
comparisons with their data and reports (commits `6dff713` and `7ffdb2d` hold
them).

| Folder | File | What it is |
|---|---|---|
| `raw/US/` | `US_NIEER__state_of_preschool_yearbook__2019.pdf` | NIEER yearbook 2019: Head Start ages 3–4, Table 4 overlap (NIEER share), state pre-K totals, CA and MI pre-K, CA TK birth dates |
| `raw/US/` | `US_NIEER__states_of_head_start_and_early_head_start__2023.pdf` | NIEER State(s) of Head Start: Early Head Start 2018–19 by state |
| `raw/US/` | `US_OHS__services_snapshot_ehs__2018-19.pdf` | OHS EHS Services Snapshot, national, 2018–19: EHS funded slots, pregnant-women slots, home-based and family child care slots, cumulative enrollment |
| `raw/US/` | `US_OHS__services_snapshot_hs__2018-19.pdf` | OHS Head Start Services Snapshot, national, 2018–19: Head Start funded slots, home-based and family child care slots |
| `raw/US/` | `US_OHS__services_snapshot_aian__2018-19.pdf` | OHS AIAN Services Snapshot, 2018–19: AIAN funded slots, home-based and family child care slots, and age mix, for the HS/EHS split |
| `raw/US/` | `US_OHS__biennial_report_to_congress__FY2019.pdf` | Appendix C (national PIR) feeds the enrolled-per-slot factors. Its state HS + EHS funded enrollment was evaluated as an Early Head Start cross-check and rejected: it matches only where every Head Start slot is ages 3–4 |
| `raw/US/` | `US_NSCH__topical_variable_list__2019.pdf` | NSCH 2019 variable list, incl. `k6q20` wording |
| `raw/US/` | `US_NSECE__measuring_supply_capacity_centers__2023-12.pdf` | ACF report on center enrollment and capacity, NSECE 2019: Exhibit 1 enrollment by age 0–4, the supply staffing index weights |

## Checks

Five scripts, all read-only, all exit 1 on failure.
`load_spec()` (`00_build_demand_targets.R`) reads the NSCH rate live from
`out/`, so there is no hand copy left to go stale -- see "How `load_spec()`
reads the derived rate" above.

```bash
Rscript state_data/tests/check_outputs_are_current.R       # demand targets are what the code produces
Rscript state_data/tests/check_sources_are_ascii.R         # no literal depends on locale
Rscript state_data/tests/check_constants_match_derivation.R # constants match out/nsece_constants.csv
Rscript state_data/tests/check_program_counts_match_sources.R # program counts match their sources
Rscript state_data/tests/check_nyc_rest_of_state.R          # NY minus NYC near a rest-of-state build
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
the same five codes (`county_fips` in `states/NY.R`). The county names in
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
