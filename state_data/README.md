# State-level demand inputs

Builds the demand file the simulator reads — `out/<GEO>.csv`, six rows, annual
hours by care type — from each state's population and NSCH non-parental rate,
divided with regional NSECE shares, with Unpaid Center-Based from each state's
program counts.

```bash
Rscript state_data/code/00_main.R --geo KY                  # build one
Rscript state_data/code/00_main.R --all                     # build all 52
Rscript state_data/code/00_main.R --all --check --interval  # with invariants and error bounds
```

**[docs/SUMMARY_method.pdf](docs/SUMMARY_method.pdf) is the document to read.**
It says what every number means, where it comes from, and how solid it is. Its
source is beside it as a .tex. This file is the map, and the record of every
source. `docs/SOURCES.csv` is the machine-readable register: one row per source and
geography, with its URL, vintage, and retrieval date.

## What is built

Fifty-two geographies: the 50 states, DC, and New York City. The memo
describes five: **KY, NJ, NY, NYC, VA**. **Puerto Rico is not built.** NSCH
covers only the 50 states and DC, NSECE places Puerto Rico in no Census region,
the ACS wage file has no Puerto Rico rows, and the simulator's ACS households
do not include it. Building it needs a participation rate, a region or a stand-in
for one, a wage, and households. New York is all 62 counties, city
included. New York City is the five boroughs, built on its own. NY includes
NYC, and NY minus NYC gives the rest of the state — both use the same rate and
the same Northeast constants.

This directory writes demand only. The simulator's per-state supply and
employment files, and how the simulator reads all three, are in
[`docs/state_level_analysis.md`](../docs/state_level_analysis.md).

## The method

Per geography, in six steps:

1. **Children in regular care** = population 0–4 × NSCH participation rate ×
   the NSECE universe ratio.
2. **Formal and informal totals** — the regional NSECE formal share.
3. **Unpaid Center-Based** — each state's own **program counts**: public
   pre-K (CCD) + Head Start + Early Head Start, converted from funded slots to
   enrolled children, less the Head Start a district runs inside one of its own
   pre-K schools, which CCD already counts. The regional NSECE unpaid-centre
   share is printed beside it as a cross-check, not used. **This is the only
   step where a state-specific source replaces an NSECE share.**
4. **Centres and homes** — the rest of formal care, split by the regional NSECE
   centre share.
5. **Informal care** — split by the regional NSECE informal mix.
6. **Annual hours** = children × the simulator's 10/40 grid × 50, split
   part-time / full-time by the regional NSECE share.

NSECE carries only Census region, so every NSECE constant is regional: the
region `states/<GEO>.R` declares (the four Census regions; DC is South). A
regional cell under 30 unweighted records takes the national value. The
simulator then re-anchors to this file: `adjust_alpha_for_state()` solves one
utility constant per care type until the state's base-year hours match it.

**Sensitivity.** Step 2 fixes the formal and informal totals, so no error in
steps 3 or 4 moves care between formal and informal. The NSCH interval does not
move every row in proportion: program counts stay fixed, and the other formal
types absorb the change, so `write_interval()` runs the build again at each
bound. The interval covers NSCH-rate sampling error only — not the universe
ratio disagreement, the step 4 assumptions, or NSECE sampling error. With no
Head Start deduction, total hours change by less than 0.1% in the five memo
geographies; Unpaid Center-Based hours rise by up to 5.3% (VA).

## Layout

```
code/00_main.R                the method, the NSECE constants, and the CLI
states/<GEO>.R               one per geography: program counts (funded
                               slots) and Census region, hand-researched
code/01_nsch_rate.R           derives each state's rate and standard error
code/02_nsece_constants.R     derives every NSECE constant (runs on the cluster)
code/03_head_start_in_schools.R measures the Head Start a district runs in its
                               own pre-K school, for step 4
code/04_replacement_register.R one row per NSECE constant: what it feeds, its
                               geography today, and the replacement search
code/config_local.example.yaml copy to state_data/config_local.yaml (git-ignored)
tests/                       the check suite
docs/SUMMARY_method.tex      the method memo, and the .pdf built from it
docs/SOURCES.csv             every source, where it came from, what it feeds
raw/                         the input files
raw/documentation/           reports behind hand-extracted numbers
raw/local/                   files too large for git (git-ignored)
cluster/                     scripts that derive raw/ summaries from
                               cluster-only data (NHES, SIPP, NDCP)
cluster/out/                 SIPP feasibility cell counts, per year
out/<GEO>.csv                the output
out/<GEO>_interval.csv       low and high bounds, from the NSCH rate
out/nsch_rate.csv            the NSCH derivation, every wave and pool
out/nsch_rate_base.csv       BASE_WAVE only -- what load_spec() reads
out/nsece_universe.csv       the NSECE derivation, full precision
out/head_start_in_schools.csv the school-overlap share per geography
out/head_start_in_schools_review.csv every matched site, for review
out/replacement_register.csv what could replace each NSECE constant
out/replacement_register_cells.csv SIPP records per state x care type
```

## Adding a geography

A state file is **data, not logic**. Copy the closest existing one.

1. Write `states/<GEO>.R`: a `SPEC_<GEO>` list with `fips`, `region`, and
   `programs`. A sub-state geography also declares `counties` and
   `county_fips`, like NYC. Do NOT write `hs_in_schools_share`,
   `nonparental_rate`, or `nonparental_rate_se` -- `load_spec()` fills all
   three in live; see below.
2. Add the code to `GEOGRAPHIES` in `00_main.R`.
3. Add the geography to `01_nsch_rate.R`'s `GEO_FIPS` (via
   `STATE_POSTAL_TO_FIPS`) and rerun it, and to `03_head_start_in_schools.R`'s
   run, so `out/nsch_rate_base.csv` and `out/head_start_in_schools.csv` carry
   a row for it.
4. `Rscript state_data/code/00_main.R --geo <GEO> --check`

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

`hs_in_schools_share` and `nonparental_rate`/`nonparental_rate_se` are not
hand-researched -- they are the output of `03_head_start_in_schools.R` and
`01_nsch_rate.R`, each a single deterministic pass over raw data with no
per-geography judgment call. Keeping a hand copy of a fully-derived number in
`states/<GEO>.R` bought nothing but a second place for it to drift, so
`load_spec()` reads both live, after sourcing the state file:

- `nonparental_rate` / `nonparental_rate_se` — that geography's row of
  `out/nsch_rate_base.csv` (`01_nsch_rate.R`, `BASE_WAVE` only). NYC has no
  NSCH records of its own, so `load_spec('NYC')` reads NY's row.
- `hs_in_schools_share` — that geography's row of
  `out/head_start_in_schools.csv` (`03_head_start_in_schools.R`). NYC has its
  own row here (its Head Start slots are its own), unlike the NSCH rate.

Both `out/` files ship committed, so a normal build never regenerates them.
Rerun `01_nsch_rate.R` / `03_head_start_in_schools.R` only when the raw NSCH
or Head Start site data changes, or when a new geography needs a row.

## Sources

Nothing downloads at build time: each file is fetched by hand and held in
`raw/`, so a build is reproducible offline.

### File naming

```
raw/                  <SCOPE>_<SOURCE_KEY>__<content>__<vintage>.<ext>
raw/documentation/    reports and codebooks behind a hand-extracted number
raw/local/            files too large for git (git-ignored, cluster only)
cluster/              scripts that derive a raw/ summary from cluster data
```

- `SCOPE` is `US` for a national file, or the state's postal code.
- `content` is lowercase snake_case.
- **`vintage` is the year the data describes, never the download date** — ISO
  where a month matters, a school year as `2018-19`.
- Double underscores separate the fields, so the name parses back apart.

Base year is **2019**. Where a source's nearest vintage differs, the table says
so.

### Inputs

| Input | Source | Held | Vintage | Feeds |
|---|---|---|---|---|
| Population 0–4 | Census Population Estimates, county by age | `raw/US_PEP__…` | 2019 | step 1 |
| Participation rate | NSCH topical file, question `k6q20` | `raw/US_NSCH__…` (2018, 2019, 2020) | 2019 | step 1 |
| NSECE constants | NSECE household file, ICPSR 37941 dataset 0009 | cluster only (licensed) | 2019 | steps 2, 4, 5, 6 |
| Public pre-K | NCES Common Core of Data, pre-K membership by county | `raw/US_CCD__…by_county…` | 2018–19 | step 4 |
| California and Michigan public pre-K | NIEER, *State of Preschool 2019*, state pages | constant | 2018–19 | step 4, CA and MI |
| Head Start | NIEER, *State of Preschool 2019*, Appendix B | constant | 2018–19 | step 4 |
| Early Head Start | NIEER, *The State(s) of Head Start and Early Head Start* (2023) | constant | 2018–19 | step 4 |
| Slots to enrolled children | OHS PIR 2018–19: Services Snapshots, Report to Congress Appendix C | constant, `00_main.R` | 2018–19 | step 4 |
| Head Start in district schools | ACF Head Start service locations × CCD pre-K by school × CCD directory × NCES EDGE geocodes | `raw/US_HSSL__…2026-09-23`, `raw/US_CCD__…`, `raw/US_EDGE__…` | 2026, 2018–19 | step 4 deduction; NYC shares |
| NIEER Head Start / state pre-K overlap | NIEER, *State of Preschool 2019*, Table 4 | not held | 2018–19 | evaluated, not used |
| Base-year wage ratio | ACS 2015–2019 five-year (2018 and 2019 records), IPUMS | `resources/supply/US_ACS__childcare_hourly_wage_by_state_and_education__2019.csv` | 2018–19 | step 7 |
| National supply parameters | Simulator's NSECE calibration | `resources/supply/US_NSECE__national_supply_parameters__2019.yaml` | 2019 | step 7; simulator base-year labor |
| State employment paths | BLS LAUS state employment-population ratios; CPS national ratio | `resources/epop/US_BLS__laus_state_epop__2006-2024.csv` | 2006–2024 | step 8 |
| SIPP care-type counts | SIPP annual public-use files, pooled | `raw/US_SIPP__…` | 2018–2023 | replacement register |
| NHES hours universes | NHES-ECPP public-use, cut at 5 and 10 hrs/week | `raw/US_NHES__…` | 2019 | cross-source check |

A **file** is read by the code. A **constant** is read by hand once and written
into `states/<GEO>.R` next to its provenance comment.

### NSCH — the participation rate

`k6q20` asks whether the child receives at least 10 hours a week of care from
someone other than a parent or guardian. The prompt names every arrangement
type and never asks which one applies. Wording is in
`raw/documentation/US_NSCH__topical_variable_list__2019.pdf`.

The build reads each rate and standard error from `out/nsch_rate_base.csv`.
`01_nsch_rate.R` derives that file from the survey:

```bash
Rscript state_data/code/01_nsch_rate.R  # out/nsch_rate.csv + out/nsch_rate_base.csv
```

The error uses the **survey design** — strata `FIPSST × STRATUM`, PSU `HHID`,
weight `FWC`, each state read as a domain of the national design.

The 2019 file is the Census re-release (`nsch_2019e_topical`, 2024), all 51
states. An earlier copy held only six states; its 3,545 records match the
re-release on every column the build reads, so no rate or error moved.

The build uses the single 2019 wave (`BASE_WAVE` in `01_nsch_rate.R`). The 2018
and 2020 files are held for reference only: 2018-19 and 2019-20 pools are
printed beside the single-wave table, and 2020 sits out of every pool because
NSCH 2020 was fielded through the pandemic, with care rates falling 5 to 18
points in KY, NJ, and VA.

State cells run 99–206 records (unweighted children, 2019). NSCH has no
sub-state identifier, so NYC uses the New York State rate and standard error.

### NSECE — the regional constants

The formal share, the unpaid-centre share, the centre-home split, the informal
mix, and the weekly hours all come from the NSECE 2019 household file,
child-weighted, by Census region. NSECE carries no state, so region is the
finest geography it offers.

**NSECE is not in this repository.** ICPSR 37941 is licensed and lives on the
cluster at `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1`. Pass
`--nsece <dir>` or set `NSECE_ROOT`, and submit with `sbatch` — the file is
117 MB and several thousand columns wide.

```bash
Rscript state_data/code/02_nsece_constants.R --nsece <dir>
```

It writes each constant with its unweighted record count to
`out/nsece_universe.csv`, over two universes:

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

`out/nsece_universe.csv` is the only on-disk record of where the constants in
`00_main.R` come from, and it arrives by hand because NSECE lives on the
cluster. `check_constants_match_derivation.R` compares every constant back to
its universe-A row, value and record count, so the pair cannot drift apart
unnoticed. **Refresh the file whenever `02_nsece_constants.R` is rerun.**

### Program counts — step 4

**Why step 4 uses program counts.** Public pre-K enrolment and Head Start
funded slots are *published, state by state*, so this is one of the very few
places where the build does not have to fall back on a regional NSECE share.
`program_children()` sums public pre-K (CCD) + Head Start + Early Head Start,
then subtracts the Head Start a school district runs in one of its own pre-K
schools, because CCD already counts those children — `03_head_start_in_schools.R`
measures that overlap (VA 21.5%, NJ 11.4%, KY 6.8%, NYS 0.2%, NYC 0.0%). The
build prints the regional NSECE share beside the result as a cross-check
(program count over the NSECE count: PA 0.48 to DC 2.93); it does not use it.

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
children, and they are not observed by age. NIEER's methodology, held at
`raw/documentation/US_NIEER__head_start_methodology__2023.pdf`:

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
one of its own pre-K schools. `03_head_start_in_schools.R` finds it from four
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
export that `03_head_start_in_schools.R` reads, so this assumes the city's
shares were the same in 2019. `tests/check_program_counts_match_sources.R`
recomputes both.

### Replacing NSECE — the register and the candidates

NSECE is 2019-only, licensed, and carries no state, so every behavioural share
here is regional. `04_replacement_register.R` enumerates the job of replacing
it: one row per NSECE constant, what it feeds, the geography it has today, the
smallest cell it rests on, and the state of the search. The constant list is
**derived** from universe A of `out/nsece_universe.csv`, so a constant added to
the build appears in the register with no candidate rather than being silently
missed.

```bash
Rscript state_data/code/04_replacement_register.R          # table + out/replacement_register.csv
Rscript state_data/code/04_replacement_register.R --check  # register covers every quantity
```

**Every candidate is checked for a part-time / full-time marker first.** The
simulator holds a child at 10 or 40 hrs/week and nothing between, so the build
states its target on that cut. A source that cannot say which side a child sits
on cannot feed the grid, whatever else it supplies — an hours source without
the marker gives a mean, and a mean implies the wrong child count once it is
placed on a two-point grid; a price source without it prices a part-time child
at the full-time rate, and that error runs straight into the equilibrium,
because price drives it. The register's `needs_pt_ft_marker` column says which
constants carry the requirement: **5 of 11**, being the four hours constants and
`price_band_share`. NSECE supplies the marker, cutting at 20 hrs/week, the same
cut `1a_nsece_processing.R` uses.

**Nothing public measures hours at state level.**

| Source | State? | Hours? | Verdict |
|---|---|---|---|
| NSECE 2019 | no (region only) | yes | what the build uses |
| NHES-ECPP 2019 public-use | **no** (region only) | **yes** | independent regional check, not a state source |
| NHES-ECPP restricted-use | yes | yes | **needs an IES licence** — the one route that would work |
| SIPP 2018–2023 | yes | **no** | ruled out below |
| NDCP 2008–2022 | county | full-time only | no part-time price; full-time state-defined |
| CCDF ACF-801 / Table 14 | yes | yes | subsidy children only, and `acf.gov` answers 202 with an empty body from both Windows and Bouchet |
| NSECE restricted-use (ICPSR 38445) | unconfirmed | yes | ICPSR does not document its geography; would need to ask |

#### SIPP — evaluated, and rejected

SIPP identifies every state and asks about child care, so it is the obvious
candidate to replace the regional constants with state ones. It cannot, and the
reason is worth keeping because it will come up again.

Tested against the 2018–2023 annual public-use files on the cluster
(`shared/raw_data/SIPP/<year>/pu<year>.csv`), pooled, by
`cluster/sipp_feasibility.py` (per-year cell sizes, `cluster/out/`) and
`cluster/sipp_pass2.py` (the two `raw/US_SIPP__…` files). Three findings, in
order of how badly each one binds:

1. **No hours variable.** SIPP's child care battery is fourteen yes/no flags,
   one per arrangement — `EDAYCARE`, `ENUR`, `EFAM`, `ENREL`, `EGRAN` and the
   rest, with child-level recodes `RDAYCARE` and so on. Nothing asks how many
   hours. The only quantity in the whole battery is `TPAYWK`. Step 6 needs
   hours, so this alone rules SIPP out for the hours constants.
2. **Price does not attach to a child.** `TPAYWK` sits on the reference
   parent's record and covers all that parent's children together. Of 6,307
   children 0–4 with a non-parental arrangement, 2,891 are in a household
   reporting an amount — and none of those amounts can be split between two
   children or between two arrangements.
3. **Cells are too thin.** Pooling gives 8,319 **distinct** children 0–4 —
   distinct, not person-years: 2,911 appear in more than one panel year, so
   naive pooling double-counts them. Split by state and care type, 108 of the
   306 state × type cells for the 50 states and DC reach 30 records, the bar
   the build already applies to a thin NSECE regional cell. No state clears
   more than four of the six types; 12 clear none, Vermont among them. Counts
   are in `out/replacement_register_cells.csv`.

SIPP also never asks whether home-based care is paid, so it has no question
corresponding to Unpaid Home-Based, and its `EPAY` is one household answer
rather than a per-arrangement one.

#### NDCP — the candidate price source, and why full-time matters

Prices in the build are wholly national. The DOL Women's Bureau **National
Database of Childcare Prices** is the obvious state source: median and 75th
percentile prices by **county × provider type (center, family) × age**, 2008
through 2022, covering 47 states.

**It is reachable, but only from the cluster.** `www.dol.gov` returns 403 to a
WAF from Windows and answers 200 from a Bouchet login node. The county file is
92 MB (`NDCP2022.xlsx`), the state summary 417 KB.

**Every NDCP price is a full-time weekly price, and there is no part-time
series.** The build places a child at 10 or 40 hrs/week, so the 10-hour point
has no NDCP price at all.

**And "full-time" is not a fixed number of hours.** Each state's market rate
survey sets its own definition, and the technical report's per-state notes show
how far they spread:

| State | What "full-time" means there |
|---|---|
| Maine | respondents gave a range; **21–30 hours** most frequent |
| Indiana | weekly rates based on a **35-hour week** |
| Michigan | weekly rates defined as **45 hours a week** |
| New Jersey | providers averaged **51.7 hours a week**; no single definition |
| MS, NE, MI | "respondents make their own determination" |
| Iowa | a **five-hour** timeframe, so ICF treated the rates as half-day |

So a state-varying NDCP price would carry variation in the *definition of
full-time* alongside real price variation, and the two cannot be separated from
the published file. Where a state reported hourly, ICF converted at **× 40
hours/week** — which matches the simulator's full-time point, but only by
coincidence.

The conversion formulas, from the technical report:

| Reported mode | Weekly price |
|---|---|
| Hourly | × 40 (hours/week) |
| Half-day | × 10 (half-days/week) |
| Daily | × 5 (days/week) |
| Monthly | × 12/52 |
| Yearly | ÷ 52 |

The technical report is at
`raw/documentation/US_NDCP__national_database_of_childcare_prices_technical_report__2022.pdf`.
The state summary is `raw/US_NDCP__state_level_estimates_and_rankings__2022.xlsx`.
The county file (92 MB) is too large for git: it is held on the cluster at
`raw/local/US_NDCP__childcare_prices_by_county__2022.xlsx` inside the checkout,
git-ignored. `cluster/inspect_ndcp.py` reads either file and reports the
hours and part-time columns. Nothing in the build reads them yet.

#### NHES-ECPP — the only source with real hours, and it stops at region

The four hours constants are the hardest part of replacing NSECE, because the
simulator's 10/40 grid needs a part-time / full-time cut and almost nothing
measures hours of care. **NHES-ECPP is the exception, and it still does not
reach state level.**

The 2019 Early Childhood Program Participation public-use file (NCES,
`ecpp_pu_pert.csv`, 7,092 children, 625 variables) carries hours per
arrangement — `CPHRS` center-based, `RCHRS` relative, `NCHRS` non-relative,
`CAREHOURX` the total, `MOSTHRSX` — and so gives a part-time share at the
build's own 20 hrs/week cut.

**On the five-hour screener the build actually uses** — NSECE universe A's
analogue, 4,132 children:

| Census region | n | mean hrs/week | share under 20 hrs/week |
|---|---:|---:|---:|
| Northeast | 787 | 32.3 | **0.311** |
| South | 1,458 | 31.1 | **0.314** |
| Midwest | 974 | 32.5 | **0.270** |
| West | 913 | 31.4 | **0.330** |

On *any* positive hours, a looser universe than the build screens on, the same
shares read 0.339 NE, 0.311 S, 0.294 MW and 0.350 W. The two are easy to
confuse — 0.311 appears in both, for different regions — so the register fills
its own copy of these figures from the derivation file rather than carrying a
transcription.

**But its finest geography is `CENREG`, four Census regions — the same wall as
NSECE.** The public-use file has no state and no county; the only sub-region
variable is `ZIPLOCL`, an NCES locale code (12 city/suburb/town/rural
categories), plus two ZIP-level poverty and minority indicators. State exists
only in the IES **restricted-use** file, which needs a licence.

So NHES-ECPP does not solve the state problem. It earns its place anyway, as
the build's **first cross-source check**. It downloads from `nces.ed.gov`,
which answers from Bouchet.

**The universe ratio, measured twice.** `CAREHOURX` is continuous, so NHES can
be cut at any threshold. That makes it the only source that can reproduce
`NSECE_UNIVERSE_RATIO` — the constant whose whole job is converting NSCH's
ten-hour universe to the five-hour one NSECE screens on. Weighted by `FEWT`,
ages 0–4:

| Universe | n | weighted | mean hrs/week | share under 20 hrs |
|---|---:|---:|---:|---:|
| any positive hours | 4,253 | 11,693,762 | 30.9 | 0.327 |
| ≥ 5 hrs/week | 4,132 | 11,349,871 | 31.7 | 0.307 |
| ≥ 10 hrs/week | 3,475 | 9,506,173 | 36.4 | 0.173 |

| Ratio | Value |
|---|---|
| NSECE, universe A / universe B | **1.098** |
| NHES, ≥5 / ≥10 | **1.194** |
| NHES, any positive / ≥10 | 1.230 |

**The two disagree by 8.7%.** NSECE says the five-hour universe is 9.8% larger
than the ten-hour one; NHES says 19.4%. The ratio multiplies the step 1 control
total. Program care is fixed by its source counts, so the change goes to the
rest of formal care and to informal care, not to every row in proportion. With
the NHES ratio, total hours rise 8.8–8.9% in the five memo geographies, and the
Unpaid Center-Based child count does not change.

The comparison is like-for-like on the one thing that matters: NSECE universe B
is a **total**-hours cut at 10 (`filter(prim, total_hours >= 10)`), and
`CAREHOURX` is total hours too. NHES does enter on looser terms — an
arrangement "at least once a week" against NSECE's five-hour screener — but a
looser entry inflates numerator and denominator together and would push NHES's
ratio **down**, not up. So the construct difference does not explain the gap.

`check_nsece_against_nhes.R` records the gap and fails if either source moves.
It does not correct one to the other, because which number is right is not
settled. Mean hours and the part-time share are printed for context only: NHES
pools every arrangement into one figure while NSECE reports per care type, so
those two are not the same quantity.

The summary is `raw/US_NHES__ecpp_children_0_4_by_hours_universe__2019.csv`,
derived by `cluster/nhes_summary.py` from the public-use file
`raw/US_NHES__ecpp_public_use_file__2019.csv` (NCES `ecpp_pu_pert.csv`, 19 MB).
The codebook is `raw/documentation/US_NHES__ecpp_public_use_codebook__2019.pdf`.

### Reference documents

`raw/documentation/` holds the reports behind hand-extracted numbers and the
evidence for choices the method makes:

| File | What it is |
|---|---|
| `US_NIEER__head_start_methodology__2023.pdf` | NIEER's method for Head Start funded slots by age |
| `US_NIEER__state_of_preschool_yearbook__2019.pdf` | NIEER yearbook 2019: Head Start ages 3–4, Table 4 overlap, state pre-K totals, CA and MI pre-K |
| `US_NIEER__states_of_head_start_and_early_head_start__2023.pdf` | NIEER State(s) of Head Start: Early Head Start 2018–19 by state |
| `US_OHS__services_snapshot_ehs__2018-19.pdf` | OHS EHS Services Snapshot, national, 2018–19: EHS funded slots, pregnant-women slots, cumulative enrollment |
| `US_OHS__services_snapshot_hs__2018-19.pdf` | OHS Head Start Services Snapshot, national, 2018–19: Head Start funded slots |
| `US_OHS__services_snapshot_aian__2018-19.pdf` | OHS AIAN Services Snapshot, 2018–19: AIAN funded slots and age mix, for the HS/EHS split |
| `US_OHS__services_snapshot_all_programs__2018-19.pdf` | OHS Services Snapshot, all programs, 2018–19: confirms the program totals add up |
| `US_OHS__services_snapshot_mshs__2018-19.pdf` | OHS Migrant and Seasonal Services Snapshot, 2018–19: confirms the program totals add up |
| `US_OHS__program_facts_page__FY2019.html` | OHS Program Facts FY2019 (archived page): state funded enrollment, HS and EHS combined. Evaluated and rejected: no pregnant-women split |
| `US_OHS__full_enrollment_initiative_pi_hs_18_04__2018.html` | OHS full-enrollment rule (97% threshold). Context for the enrolled-per-slot factors |
| `US_AECF__kidscount_head_start_enrollment_by_age_table9786__2024.html` | KIDS COUNT Head Start enrollment by age. Evaluated and rejected: cumulative, pregnant women inside 0–2 |
| `US_OHS__biennial_report_to_congress__FY2019.pdf` | Appendix C (national PIR) feeds the enrolled-per-slot factors. Its state HS + EHS funded enrollment was evaluated as an Early Head Start cross-check and rejected: it matches only where every Head Start slot is ages 3–4 |
| `US_NSCH__topical_variable_list__2019.pdf` | NSCH 2019 variable list, incl. `k6q20` wording |
| `US_NSECE__home_based_provider_chartbook__2021-05.pdf` | NSECE home-based provider chartbook |
| `US_NSECE__measuring_supply_capacity_centers__2023-12.pdf` | ACF report on centre enrollment and capacity, NSECE 2019 |
| `US_PWBM__data_processing_for_tax_module__2020-01.pdf` | PWBM tax-module data processing |
| `VA_EDWP__authorized_vs_current_capacity__2024.pdf` | Virginia provider survey on authorized versus current capacity |
| `KY_CCAKY__child_care_workforce_study__2019.pdf` | Child Care Aware of Kentucky workforce study |
| `KY_CCAKY__cost_addendum_report__2018.pdf` | Child Care Aware of Kentucky cost addendum |
| `KY_MRS17__market_rate_study_report__2017.pdf` | Kentucky market rate study, 2017 |
| `KY_MRS20__market_rate_study_report__2020.pdf` | Kentucky market rate study, 2020 |
| `NJ_MPS16__child_care_market_price_study__2016.pdf` | New Jersey market price study, 2016 |
| `NJ_MRS21__child_care_market_rate_survey__2021.pdf` | New Jersey market rate survey, 2021 |
| `NYS_MRS__market_rate_survey_report__2019.pdf` | New York State market rate survey, 2019 |
| `NYS_MRS22__market_rate_survey_report__2022.pdf` | New York State market rate survey, 2022 |
| `NYS_MRS24__market_rate_survey_report__2024.pdf` | New York State market rate survey, 2024 |
| `NYS_NIEERHS__head_start_state_profile__2020-21.pdf` | NIEER Head Start profile, New York, 2020-21 |
| `NYS_NIEER__state_preschool_yearbook_profile__2018-19.pdf` | NIEER state pre-K profile, New York, 2018-19 |
| `NY_OCFS__child_care_facts_and_figures__2019.pdf` | New York OCFS child care facts and figures, 2019 |
| `VA_MRS18__market_rate_survey_report__2018.pdf` | Virginia market rate survey, 2018 |
| `VA_VDSS__child_care_capacity_and_demand_matrix__2020-06.xlsx` | Virginia VDSS capacity and demand matrix by locality |
| `US_TBL__tax_microsimulation_model_documentation__2024.pdf` | Budget Lab tax microsimulation model documentation |
| `US_CCDF__preliminary_data_table_<N>_...__2019.tsv` | CCDF FY2019 preliminary data tables 1, 3, 4, 5, 6, 9, 14 (setting, regulation, age, hours) |
| `US_FACES__head_start_interaction_with_federal_state_local_systems_brief__2022.pdf` | FACES brief: Head Start and state and local systems |
| `US_FACES__head_start_interaction_with_federal_state_local_systems_appendix__2022.pdf` | FACES appendix to the brief |
| `US_NHES__ecpp_public_use_codebook__2019.pdf` | NHES-ECPP 2019 public-use codebook (`CAREHOURX`, `FEWT`, `CENREG`) |
| `US_NIEER__state_preschool_yearbook__2018-19.pdf` | Shorter copy (197 pages) of the same *State of Preschool 2019* yearbook. Nothing reads it; the 369-page `US_NIEER__state_of_preschool_yearbook__2019.pdf` is the one the checks and specs cite |
| `US_NSCH__enhancements_technical_document__2024.pdf` | NSCH survey enhancements technical document |
| `US_NSCH__stata_codebook_dofile__2019.do` | NSCH 2019 Stata label and codebook do-file |
| `US_NSECE__center_based_programs_characteristics_initial_findings__2012.pdf` | NSECE 2012 centre-based program characteristics |
| `US_NSECE__center_based_providers_counts_and_characteristics_chartbook__2012-2019.pdf` | NSECE centre-based provider chartbook, 2012 and 2019 |
| `US_NSECE__household_participation_counts_and_characteristics__2023.pdf` | NSECE household participation counts and characteristics |
| `US_NSECE__snapshot_child_care_cost_burden__2022.pdf` | NSECE snapshot: child care cost burden |
| `US_NSECE__snapshot_publicly_funded_participation__2024.pdf` | NSECE snapshot: publicly funded participation |
| `US_NSECE__usage_and_out_of_pocket_cost_tabulations_2012_data__2018.pdf` | NSECE 2012 usage and out-of-pocket cost tabulations |

## Checks

Ten scripts, all read-only, all exit 1 on failure. `01_nsch_rate.R` and
`03_head_start_in_schools.R` used to carry a `--check` that compared their
derivation to a hand copy of `nonparental_rate`/`nonparental_rate_se`/
`hs_in_schools_share` in `states/<GEO>.R`. `load_spec()` (`00_main.R`) now
reads all three live from the `out/` files those two scripts write, so there
is no hand copy left to go stale, and neither script is a check any more --
see "How `load_spec()` reads the two derived fields" below.

`check_docs_describe_the_checks.R` guards this paragraph: it derives the suite
from disk and fails if the count or the read-only split drifts.

```bash
Rscript state_data/tests/check_outputs_are_current.R       # out/ is what the code produces
Rscript state_data/tests/check_memo_matches_build.R        # memo prints what out/ holds
Rscript state_data/tests/check_memo_prose_numbers.R        # and so do its sentences
Rscript state_data/tests/check_memo_names_real_files.R     # the memo names no repo file
Rscript state_data/tests/check_sources_are_ascii.R         # no literal depends on locale
Rscript state_data/tests/check_docs_describe_the_checks.R  # this paragraph is right
Rscript state_data/tests/check_constants_match_derivation.R # constants match out/nsece_universe.csv
Rscript state_data/tests/check_nsece_against_nhes.R        # NSECE against a second survey
Rscript state_data/tests/check_program_counts_match_sources.R # program counts match their sources
python  state_data/tests/check_summarise_shadowing.py      # scans for the shadowing trap
```

**Run `check_outputs_are_current.R` first.** It is the only one that compares
`out/` to the code. The memo checks compare the memo to `out/`, so a stale
`out/` would pass them.

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
  exactly 0% or exactly 100% and nothing errors. `check_summarise_shadowing.py`
  scans every R file for the pattern.

## How the simulator reads this

`00_main.R` writes each whole-state file twice: to `out/<GEO>.csv`, and to
`config/state/demand/<GEO>.csv`, where the simulator reads it.
`read_state_demand_targets()` in `src/shared_functions/adjust_alpha_for_state.R`
takes the same six rows. One Center-Based target covers both of the
simulator's centre price bands, and both bands get one shared adjustment.

NYC gets no simulator file. The simulator selects households by state FIPS,
so it cannot read a county-defined geography.

`check_outputs_are_current.R` confirms that each simulator file equals its
`out/` file and that the simulator's reader accepts it.

```bash
Rscript src/main.R -r state_targets_smoke    # baseline for KY, NJ, NY, VA
Rscript src/main.R -r state_targets_all      # baseline for all 50 states and DC
```
