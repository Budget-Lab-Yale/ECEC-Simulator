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

**The method memo, `docs/SUMMARY_method.pdf`, is the document to read.** It
says what every number means, where it comes from, and how solid it is. The
memo and its .tex source are kept outside git; ask Anna for the current
version. This file is the map, and the record of every
source. `docs/SOURCES.csv` is the machine-readable register: one row per source and
geography, with its URL, vintage, and retrieval date.

## What is built

Fifty-two geographies: the 50 states, DC, and New York City. The memo
describes five: **KY, NJ, NY, NYC, VA**. **Puerto Rico is not built.** NSCH
covers only the 50 states and DC, NSECE places Puerto Rico in no Census region,
and the simulator's ACS households do not include it. Building it needs a participation rate, a region or a stand-in
for one, a wage, and households. New York is all 62 counties, city
included. New York City is the five boroughs, built on its own. NY includes
NYC. NY minus NYC is close to the rest of the state, within 0.6% of the hours
in each care type. Each divides its care by the p0 mix of its own households,
so the difference is not exact.

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
   enrolled children, less the Head Start a district runs inside one of its own
   pre-K schools, which CCD already counts.
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
reads.

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
                                        ratio, and the CLI
code/05_compare_nsece_shares.R          comparison: the build against regional NSECE
                                        shares (writes nothing the simulator reads)
code/06_compare_industry_counts.R      validation: the build against 2019 QCEW child
                                        day care employment and nonemployer counts
code/07_compare_nhes.R                 validation: the build against NHES-ECPP 2019, same
                                        population and care-type rules, by Census region
cluster/acs_p0_shares.R                 p0 children by geography, care type, and hours,
                                        from a full-sample interface (runs on the cluster)
cluster/acs_p0_shares_by_age.R          the same p0 summary split by child age, for the
                                        NHES check by age (runs on the cluster)
cluster/nsece_listed_home_share.py      listed-home share of Paid Home-Based children,
                                        NSECE home provider file (runs on the cluster)
states/<GEO>.R                          one per geography: a geo_spec() call with program
                                        counts (funded slots) and Census region,
                                        hand-researched
code/01_nsch_participation_rate.R       derives each state's rate and standard error
code/02_derive_nsece_constants.R        derives every NSECE constant (runs on the cluster)
code/03_head_start_school_overlap.R     measures the Head Start a district runs in its
                                        own pre-K school, for step 4
code/04_compare_licensing_registers.R   validation: centre counts against five
                                        licensing registers (writes nothing the
                                        simulator reads)
code/config_local.example.yaml          copy to state_data/config_local.yaml (git-ignored)
tests/                                  the check suite
docs/SUMMARY_method.tex                 the method memo and its .pdf (local only, git-ignored)
docs/SOURCES.csv                        every source, where it came from, what it feeds
raw/                                    the input files, one folder per scope
raw/US/                                 national files and reports
raw/KY/, raw/NJ/, raw/VA/               state register files and reports
raw/NY/                                 New York State files and report (NYS_ and NY_)
raw/NYC/                                New York City register file
raw/local/                              files too large for git (git-ignored)
out/nsch_participation_rate_by_wave.csv the NSCH derivation, every wave and pool
out/nsch_participation_rate_2019.csv    BASE_WAVE only -- what load_spec() reads
out/nsece_constants.csv                 the NSECE derivation, full precision
out/head_start_school_overlap.csv       the school-overlap share per geography
out/nsece_listed_home_share.csv         the listed-home share, national and by region
out/licensing_register_comparison.csv   build vs register children, centres and homes, KY NJ NY NYC VA
out/industry_count_comparison.csv       build vs QCEW and nonemployer counts, 52 geographies
out/nhes_comparison.csv                 build vs NHES-ECPP 2019 by region and care type
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
   `SUBSTATE_GEOGRAPHIES` (`src/shared_functions/constants.R`). Do NOT write `hs_in_schools_share`
   or `nonparental_rate` -- `load_spec()` fills both in live; see below.
2. `GEOGRAPHIES` in `00_build_demand_targets.R` takes every code in
   `STATE_POSTAL_TO_FIPS` and `SUBSTATE_GEOGRAPHIES`, so it needs no edit.
3. Rerun `03_head_start_school_overlap.R`, so
   `out/head_start_school_overlap.csv` carries a row for the geography. A
   sub-state geography takes its parent state's NSCH rate (`parent_geo()`), so
   `01_nsch_participation_rate.R` needs no change.
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

### How `load_spec()` reads the two derived fields

`hs_in_schools_share` and `nonparental_rate` are not
hand-researched -- they are the output of `03_head_start_school_overlap.R`
and `01_nsch_participation_rate.R`, each a single deterministic pass over raw
data with no per-geography judgment call. Keeping a hand copy of a fully-derived number in
`states/<GEO>.R` bought nothing but a second place for it to drift, so
`load_spec()` reads both live, after sourcing the state file:

- `nonparental_rate` — that geography's row of
  `out/nsch_participation_rate_2019.csv` (`01_nsch_participation_rate.R`,
  `BASE_WAVE` only). NYC has no NSCH records of its own, so `load_spec('NYC')`
  reads NY's row.
- `hs_in_schools_share` — that geography's row of
  `out/head_start_school_overlap.csv` (`03_head_start_school_overlap.R`). NYC
  has its own row here (its Head Start slots are its own), unlike the NSCH
  rate.

Both `out/` files ship committed, so a normal build never regenerates them.
Rerun `01_nsch_participation_rate.R` / `03_head_start_school_overlap.R` only
when the raw NSCH or Head Start site data changes, or when a new geography
needs a row.

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
| California and Michigan public pre-K | NIEER, *State of Preschool 2019*, state pages | constant | 2018–19 | step 4, CA and MI |
| Head Start | NIEER, *State of Preschool 2019*, Appendix B | constant | 2018–19 | step 4 |
| Early Head Start | NIEER, *The State(s) of Head Start and Early Head Start* (2023) | constant | 2018–19 | step 4 |
| Slots to enrolled children | OHS PIR 2018–19: Services Snapshots, Report to Congress Appendix C | constant, `00_build_demand_targets.R` | 2018–19 | step 4 |
| Head Start in district schools | ACF Head Start service locations × CCD pre-K by school × CCD directory × NCES EDGE geocodes | `raw/US/US_HSSL__…2026-09-23`, `raw/US/US_CCD__…`, `raw/US/US_EDGE__…` | 2026, 2018–19 | step 4 deduction; NYC shares |
| NIEER Head Start / state pre-K overlap | NIEER, *State of Preschool 2019*, Table 4 | not held | 2018–19 | evaluated, not used |
| Base-year wage ratio and state labour share | IPUMS ACS 2018-2019 (5-year sample), childcare occupations (OCC 4600, 2300, 2330, 230), by measured education | IPUMS extract usa:33, cluster only (`raw/local/`) → `resources/supply/state_childcare_wage_ratios_acs.csv` | 2018–2019 | step 7 |
| National supply parameters | Simulator's NSECE calibration | `resources/supply/US_NSECE__national_supply_parameters__2019.yaml` | 2019 | step 7; simulator base-year labor |
| State employment paths | ACS state age-sex composition × CBO national cell rates, scaled to the national rate | `resources/epop/US_ACS__epop_by_state_sex_age__2006-2024.csv`, `resources/epop/epop_projections.csv` | 2006–2024 composition | step 8 |
| Industry counts | BLS QCEW private employment, NAICS 624410; Census Nonemployer Statistics, NAICS 6244 | `raw/US/US_QCEW__…`, `raw/US/US_NES__…` | 2019 | industry check |
| Licensing registers | KY CCAoA (CHFS), NJ DCF + KIDS COUNT registered family child care count, NY OCFS + NYC DOHMH, VA VDSS + ELPG program counts | `raw/KY/KY_…`, `raw/NJ/NJ_…`, `raw/NY/NYS_…`, `raw/NYC/NYC_…`, `raw/VA/VA_…` | 2018-09 to 2026-09 | register check |

A **file** is read by the code. A **constant** is read by hand once and written
into `states/<GEO>.R` next to its provenance comment.

### NSCH — the participation rate

`k6q20` asks whether the child receives at least 10 hours a week of care from
someone other than a parent or guardian. The prompt names every arrangement
type and never asks which one applies. Wording is in
`raw/US/US_NSCH__topical_variable_list__2019.pdf`.

The build reads each rate from `out/nsch_participation_rate_2019.csv`.
`01_nsch_participation_rate.R` derives that file from the survey:

```bash
Rscript state_data/code/01_nsch_participation_rate.R
# writes out/nsch_participation_rate_by_wave.csv and
# out/nsch_participation_rate_2019.csv
```

The error uses the **survey design** — strata `FIPSST × STRATUM`, PSU `HHID`,
weight `FWC`, each state read as a domain of the national design.

The 2019 file is the Census re-release (`nsch_2019e_topical`, 2024), all 51
states. An earlier copy held only six states; its 3,545 records match the
re-release on every column the build reads, so no rate or error moved.

The build uses the single 2019 wave (`BASE_WAVE` in
`01_nsch_participation_rate.R`). The 2018 and 2020 files are held for reference only: 2018-19 and 2019-20 pools are
printed beside the single-wave table, and 2020 sits out of every pool because
NSCH 2020 was fielded through the pandemic, with care rates falling 5 to 18
points in KY, NJ, and VA.

State cells run 99–206 records (unweighted children, 2019). NSCH has no
sub-state identifier, so NYC takes the New York State rate times a p0
factor: p0's non-parental share of NYC's children over the same
share for the state (`nsch_rate_of()` in `00_build_demand_targets.R`).

### NSECE — the regional constants

The build uses only `NSECE_UNIVERSE_RATIO` (step 1), in
`00_build_demand_targets.R`. The regional shares live in
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
`program_children()` sums public pre-K (CCD) + Head Start + Early Head Start,
then subtracts the Head Start a school district runs in one of its own pre-K
schools, because CCD already counts those children — `03_head_start_school_overlap.R`
measures that overlap (VA 21.5%, NJ 11.4%, KY 6.8%, NYS 0.2%, NYC 0.0%). The
comparison script (`05_compare_nsece_shares.R`) sets the result beside the
regional NSECE unpaid-center share (program count over the NSECE count: PA
0.48 to DC 2.93).

The counts are not a clean measure of the care type, and the memo says so:
they measure enrolment and funded slots, not the primary arrangement (a pre-K
child also in paid care is Center-Based), and they include part-day children
under the 10-hour line. They are used anyway because being state-specific
matters more here than being exactly on concept — this is the one care type
where a state source exists at all.

**CCD — public pre-K.** Pre-K membership, school by school, summed by county,
for 2018–19, on or near October 1. It counts a **grade**, not an age, so it
includes some five-year-olds, and it counts more than state pre-K programs
alone — in Kentucky 28,465 against the 21,351 NIEER reports for the Kentucky
Preschool Program, because the grade also holds district-funded places and
preschool special education.

- **California has no CCD pre-K rows** for 2018–19. Its pre-K count is NIEER's:
  State Preschool Program 140,920 + transitional kindergarten 104,694 = 245,614.
  It excludes preschool special education (52,307), which CCD counts elsewhere.
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

So the Head Start count includes some five-year-olds by construction, and
nothing held here can size them. The specs added in September 2026 (32 states
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

**Head Start and pre-K can count the same children.** A state can fund pre-K
places inside Head Start, and a district can run Head Start in a school. Step 4
deducts only the part CCD already counts: Head Start that a district runs in
one of its own pre-K schools. `03_head_start_school_overlap.R` finds it from four
national files — ACF service locations, CCD pre-K by school, the CCD school
directory, and NCES EDGE geocodes — so every state is measured the same way.

NIEER's own overlap count is not used. *State of Preschool 2019*, Table 4
(p.28), gives 3- and 4-year-olds in pre-K plus special education, without and
with Head Start, each child counted once; NIEER's methodology (p.193) says it
subtracts Head Start children in state pre-K where a state reports them. The
difference between the two columns is Head Start net of the overlap. Special
education cancels: Virginia reports no overlap, and its difference is exactly
its gross Head Start, 11,579. The overlap is with state pre-K, in every
setting; CCD counts public schools only, so Table 4 is the wrong correction for
step 4. Of the 18 states read by hand before September 2026, seven report an
overlap: CT, IL, KY, MA, NJ, NY, WV. Deducting where reported and nowhere else
would treat states differently for a reason that is not about care. NIEER marks
the others "some Head Start children may also be counted in state pre-K" and
deducts nothing for them. A gap of two children or fewer is rounding in the
table (five states read ±1).

**The NYC share.** ACF's service-location file gives funded slots by site.
NYC's Head Start and Early Head Start counts are the city's shares of New York
State slots at open sites in that file, applied to the 2018–19 NIEER state
figures: 7,064 of 20,518 Head Start slots (program types 1, 3, 5) and 4,426 of
8,590 Early Head Start slots (types 2, 4, 6). The file is the same 2026-09-23
export that `03_head_start_school_overlap.R` reads, so this assumes the city's
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

### Licensing registers — the center-count check

An earlier build (commit `31307ce`, 18 September 2026) took formal care from
five geographies' licensing registers and then scaled them to the NSECE formal
share. `04_compare_licensing_registers.R` reads the same registers with the
same conversions, but without that scaling. So the registers are an independent
count of the children in licensed centers, and the check compares the build to
them.

```bash
Rscript state_data/code/04_compare_licensing_registers.R
# prints the table, writes out/licensing_register_comparison.csv
```

**Capacity to children.** Each register gives licensed capacity. The script
removes school-age capacity, rebases a 2026 export to the base year, and
converts capacity to children:

| Geography | Register | Vintage | Rebase | Capacity to children |
|---|---|---|---|---|
| KY | CCAoA republication of the CHFS licensed and certified registers | 2018-09 | none | measured enrolled per provider: 42 per center, 9 per home (2019 workforce study, Table 20) |
| NJ | DCF licensed centers; homes from the DCF registered family child care count (KIDS COUNT, 1,492 at 2019-12-31) | 2019-05; homes 2019-12 | none | centers: NJ fill rate 0.6733 (MRS 2021, Figure 7); homes: 5 children per provider (legal maximum) × under-5 slot share 0.7955 × fill 0.4402 (MRS 2021, p. 18 and Table 17, 2020 column) |
| NY | OCFS (centers outside NYC, all homes) + NYC DOHMH group child care | 2026-09 | 2019/2025 by program type, OCFS modality series | national fill: centers 0.9029, homes 0.7954 |
| NYC | the same two registers, five boroughs | 2026-09 | as NY | as NY |
| VA | VDSS, every approval route | 2026-09 | licensed and voluntary registered homes: 2019 / 2026 program count per type (ELPG); everything else: statewide factor, VDSS 2020-06 matrix | centers 0.9029, homes 0.7954 x 0.7546 |

The home under-5 share is 0.7546, the NSECE listed-provider stratum. The
September 18 build held 0.7567, a published figure that the later derivation
replaced.

**What a register center holds.** A licensed center holds private paid care,
most Head Start and Early Head Start, and state pre-K places contracted to
community centers. It does not hold public-school pre-K, which is
license-exempt in all five geographies. So the check compares the register
with Center-Based + Head Start + Early Head Start (enrolled, less the
in-school share). Homes compare with Paid Home-Based × 0.7633, the listed-home
share. Paid Home-Based holds every paid home with no prior relationship,
licensed or not; a register holds licensed or registered homes only.
`cluster/nsece_listed_home_share.py` measures the share from the NSECE 2019
home provider file (ICPSR 37941-0007): children 0–5 in listed homes (588,260)
over children with no prior relationship in listed homes (465,968) and in
unlisted paid homes (304,754), care in the provider's home. 738 listed homes
with withheld age counts take the listed stratum's under-school-age share,
0.7546. By region it runs 0.64 to 0.92, but the Northeast has 27 unlisted paid
providers, so the check uses the national share.

**Read the gap by conversion.** KY and NJ convert capacity with their own
measured enrollment, so their gap is the most reliable. NY, NYC, and VA use
the national NSECE fill rate. Kentucky's own survey puts its centers at 57%
full against the national 90%. So in NY, NYC, and VA the register figure is
probably high. The build is below the register in all five, so each true gap
there is smaller than the table shows.

### Reference documents

Each report sits beside the data of its scope, in `raw/<SCOPE>/`. Only the
documents behind numbers the build uses are held: the reports, plus the VA VDSS
workbook that the register check's CSV comes from. On 2 October 2026 I
removed the documents read only for context, method, or a rejected source,
and the NHES codebook; earlier commits still hold them.

| Folder | File | What it is |
|---|---|---|
| `raw/US/` | `US_NIEER__state_of_preschool_yearbook__2019.pdf` | NIEER yearbook 2019: Head Start ages 3–4, Table 4 overlap, state pre-K totals, CA and MI pre-K |
| `raw/US/` | `US_NIEER__states_of_head_start_and_early_head_start__2023.pdf` | NIEER State(s) of Head Start: Early Head Start 2018–19 by state |
| `raw/US/` | `US_OHS__services_snapshot_ehs__2018-19.pdf` | OHS EHS Services Snapshot, national, 2018–19: EHS funded slots, pregnant-women slots, cumulative enrollment |
| `raw/US/` | `US_OHS__services_snapshot_hs__2018-19.pdf` | OHS Head Start Services Snapshot, national, 2018–19: Head Start funded slots |
| `raw/US/` | `US_OHS__services_snapshot_aian__2018-19.pdf` | OHS AIAN Services Snapshot, 2018–19: AIAN funded slots and age mix, for the HS/EHS split |
| `raw/US/` | `US_OHS__biennial_report_to_congress__FY2019.pdf` | Appendix C (national PIR) feeds the enrolled-per-slot factors. Its state HS + EHS funded enrollment was evaluated as an Early Head Start cross-check and rejected: it matches only where every Head Start slot is ages 3–4 |
| `raw/US/` | `US_NSCH__topical_variable_list__2019.pdf` | NSCH 2019 variable list, incl. `k6q20` wording |
| `raw/US/` | `US_NSECE__measuring_supply_capacity_centers__2023-12.pdf` | ACF report on center enrollment and capacity, NSECE 2019 |
| `raw/KY/` | `KY_CCAKY__child_care_workforce_study__2019.pdf` | Child Care Aware of Kentucky workforce study |
| `raw/NJ/` | `NJ_MRS21__child_care_market_rate_survey__2021.pdf` | New Jersey market rate survey, 2021 |
| `raw/NY/` | `NY_OCFS__child_care_facts_and_figures__2019.pdf` | New York OCFS child care facts and figures, 2019 |
| `raw/VA/` | `VA_VDSS__child_care_capacity_and_demand_matrix__2020-06.xlsx` | Virginia VDSS capacity and demand matrix by locality |

## Checks

Five scripts, all read-only, all exit 1 on failure.
`01_nsch_participation_rate.R` and `03_head_start_school_overlap.R` used to
carry a `--check` that compared their
derivation to a hand copy of `nonparental_rate`/`nonparental_rate_se`/
`hs_in_schools_share` in `states/<GEO>.R`. `load_spec()`
(`00_build_demand_targets.R`) now reads the rate and the share live from the `out/` files those two scripts
write, so there is no hand copy left to go stale, and neither script is a check any more --
see "How `load_spec()` reads the two derived fields" above.

```bash
Rscript state_data/tests/check_outputs_are_current.R       # demand targets are what the code produces
Rscript state_data/tests/check_sources_are_ascii.R         # no literal depends on locale
Rscript state_data/tests/check_constants_match_derivation.R # constants match out/nsece_constants.csv
Rscript state_data/tests/check_program_counts_match_sources.R # program counts match their sources
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
simulator's center price bands, and both bands get one shared adjustment.

NYC gets a simulator file too. The simulator selects NYC households by
STATEFIP 36 and COUNTYFIP 005, 047, 061, 081, and 085 (`SUBSTATE_GEOGRAPHIES`
in `src/shared_functions/constants.R`). The build selects NYC's population on
the same five codes (`county_fips` in `states/NY.R`). The county names in
`counties` are kept only for the two files that carry no FIPS: the Head Start
site file and the NY registers. A NYC run needs an interface built from a raw
ACS pull that carries `COUNTYFIP`; without one, the run stops at
initialization.

`check_outputs_are_current.R` rebuilds each geography and confirms that its
simulator file equals the rebuild and that the simulator's reader accepts it.

```bash
Rscript src/main.R -r state_targets_smoke    # baseline for KY, NJ, NY, VA
Rscript src/main.R -r state_targets_all      # baseline for all 50 states, DC, NYC
```
