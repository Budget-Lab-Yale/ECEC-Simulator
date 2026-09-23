# State-level demand inputs

Builds the demand file the simulator reads — `out/<GEO>.csv`, six rows, annual
hours by care type — from each state's population and NSCH non-parental rate,
divided with regional NSECE shares. Published programme counts are a check.

```bash
Rscript state_data/00_main.R --geo KY                  # build one
Rscript state_data/00_main.R --all                     # build all 19
Rscript state_data/00_main.R --all --check --interval  # with invariants and error bounds
```

**[docs/SUMMARY_method.pdf](docs/SUMMARY_method.pdf) is the document to read.**
It says what every number means, where it comes from, and how solid it is. Its
source is beside it as a .tex. This file is the map.

## What is built

Nineteen geographies. The first five: **KY, NJ, NY, NYC, VA**. The other 14 are
every state with a full section in the September 2026 Flashpoint States and
Expansion States memos: **AZ, CA, CO, CT, IL, IN, MA, MN, NM, TX, VT, WA, WI,
WV**. New York is all 62 counties, city
included. New York City is the five boroughs, built on its own. NY includes
NYC, and NY minus NYC gives the rest of the state — both use the same rate and
the same Northeast constants.

This directory writes demand only.

## The method

Per geography, in six steps:

1. **Children in regular care** = population 0–4 × NSCH participation rate.
2. **Formal and informal totals** — the regional NSECE formal share.
3. **Unpaid Center-Based** — formal care × the regional NSECE unpaid-centre
   share. Programme counts are a printed check, never a target.
4. **Centres and homes** — the rest of formal care, split by the regional NSECE
   centre share.
5. **Informal care** — split by the regional NSECE informal mix.
6. **Annual hours** = children × regional NSECE weekly hours × 50.

NSECE carries only Census region, so every NSECE constant is regional: the
Northeast for CT, MA, NJ, NY, NYC, and VT, the South for KY, TX, VA, and WV,
the Midwest for IL, IN, MN, and WI, and the West for AZ, CA, CO, NM, and WA. A
regional cell
under 30 unweighted records takes the national value. The simulator then
re-anchors to this file: `adjust_alpha_for_state()` solves one utility constant
per care type until the state's base-year hours match it.

## Layout

```
00_main.R                    the method, the NSECE constants, and the CLI
states/<GEO>.R               one per geography: programme counts, NSCH rate
                               and standard error, Census region
01_nsch_rate.R               derives each state's rate and standard error
02_nsece_constants.R         derives every NSECE constant (runs on the cluster)
03_national_sensitivity.R    national-NSECE sensitivity run
04_head_start_in_schools.R   measures the Head Start a district runs in its
                               own pre-K school, for step 3
05_replacement_register.R    one row per NSECE constant: what it feeds, its
                               geography today, and the replacement search
tests/                       the check suite
docs/SUMMARY_method.tex      the method memo, and the .pdf built from it
SOURCES.md, SOURCES.csv      every source, where it came from, what it feeds
raw/                         the input files
raw/documentation/           reports behind hand-extracted numbers
out/<GEO>.csv                the output
out/<GEO>_interval.csv       low and high bounds, from the NSCH rate
out/nsch_rate.csv            the NSCH derivation
out/nsece_universe.csv       the NSECE derivation, full precision
out/national_sensitivity.csv regional versus national NSECE
out/head_start_in_schools.csv the school-overlap share per geography
out/replacement_register.csv what could replace each NSECE constant
config_local.example.yaml    copy to config_local.yaml (git-ignored)
```

## Adding a geography

A state file is **data, not logic**. Copy the closest existing one.

1. Write `states/<GEO>.R`: a `SPEC_<GEO>` list with `fips`, `region`,
   `programmes`, `nieer_undup_34`, `nieer_state_prek`, `nonparental_rate`, and
   `nonparental_rate_se`. A sub-state
   geography also declares `counties`, like NYC.
2. Add the code to `GEOGRAPHIES` in `00_main.R`.
3. `Rscript state_data/00_main.R --geo <GEO> --check`

A geography needs three things:

- **A Census region**, which selects every NSECE constant.
- **Programme counts**, for the step 3 check only — public pre-K (CCD), Head
  Start, and Early Head Start (NIEER), and the NIEER Table 4 pair for the Head
  Start / pre-K overlap.
- **A measured non-parental rate** from NSCH `k6q20`, which covers all 51:

```
filter k6q20 in (1,2), sc_age_years <= 4, weight by fwc, subset fipsst
```

## Deriving the inputs

**Why step 3 does not use programme counts.** Pre-K (CCD) + Head Start + Early
Head Start counts a child twice when a Head Start child also sits in state
pre-K. NIEER's *State of Preschool 2019*, Table 4 (p.28), gives that overlap,
but only seven states report it (CT, IL, KY, MA, NJ, NY, WV) -- a deduction
where reported and none elsewhere treats states differently for a reason that
is not about care. Counts also measure enrollment, not the primary type (a
pre-K child in paid care too is Center-Based), and include part-day children
under the 10-hour line. So step 3 takes the regional NSECE share, and
`programme_children()` computes the counts, net of reported overlap, only for
the check the build prints (modeled / net: VT 0.34 to IN 1.13). The yearbook is
in `raw/documentation/`.

**California pre-K** (check only) comes from NIEER, not CCD: CCD has no
California pre-K rows for 2018-19. State Preschool Program 140,920 + transitional kindergarten
104,694.

**The NSCH rate** and its standard error are constants in `states/<GEO>.R`.
`01_nsch_rate.R` is where they come from:

```bash
Rscript state_data/01_nsch_rate.R           # table + out/nsch_rate.csv
Rscript state_data/01_nsch_rate.R --check   # compare states/<GEO>.R back to NSCH
```

The error uses the **survey design** — strata `FIPSST x STRATUM`, PSU `HHID`,
weight `FWC`, each state read as a domain of the national design.

**The NSECE constants** come from `02_nsece_constants.R`, over the NSECE
2019 household file. NSECE is licensed and is not in this repository; it lives
on the cluster at `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1`.
Pass `--nsece <dir>` or set `NSECE_ROOT`, and submit with `sbatch` — the file is
117 MB and several thousand columns wide.

```bash
Rscript state_data/02_nsece_constants.R --nsece <dir>
```

It writes each constant with its unweighted record count to
`out/nsece_universe.csv`, over two universes: **universe A**, every child 0–4 in
regular care, which reproduces NSECE's published figures and so validates the
code — and **universe B**, children at 10+ hours a week, the NSCH threshold.
**The build reads universe A**, because the simulator sets no hours threshold;
`NSECE_UNIVERSE_RATIO` carries the NSCH rate from B up to A. B stays for that
ratio and for sensitivity.

`out/nsece_universe.csv` is the only on-disk record of where the constants in
`00_main.R` come from, and it arrives by hand because NSECE lives on the
cluster. `check_constants_match_derivation.R` compares every constant back to
its universe-A row, value and record count, so the pair cannot drift apart
unnoticed. **Refresh the file whenever `02_nsece_constants.R` is rerun.**

**The replacement register.** NSECE is 2019-only, licensed, and carries no
state, so every behavioural share here is regional. `05_replacement_register.R`
enumerates the job of replacing it: one row per NSECE constant, what it feeds,
the geography it has today, the smallest cell it rests on, and the state of the
search. The constant list is **derived** from universe A of
`out/nsece_universe.csv`, so a constant added to the build appears in the
register with no candidate rather than being silently missed.

```bash
Rscript state_data/05_replacement_register.R          # table + out/replacement_register.csv
Rscript state_data/05_replacement_register.R --check  # register covers every quantity
```

It also counts what SIPP could carry. SIPP identifies every state and asks
about child care, so it is the obvious candidate — and it fails three ways:
it has **no hours variable at all**, its only price (`TPAYWK`) sits on the
parent and covers all that parent's children together, and 50 of 108
state × type cells reach the 30-record bar the build already applies to a thin
NSECE cell. `SOURCES.md` records the test. Counts are in
`out/replacement_register_cells.csv`.

**Sensitivity.** Step 2 fixes the formal and informal totals, so no error in
steps 3 or 4 moves care between formal and informal. Every step is a share of
the control total, so the NSCH interval moves every row in proportion.

**National NSECE constants.** The build is regional by default.
`03_national_sensitivity.R` reruns every geography two more ways, at the NSCH
rate and at ±1.96 standard errors: a national build, with every NSECE constant
national, and a hybrid build, with the regional shares but national weekly
hours. It writes `out/national_sensitivity.csv`. It does not touch
the simulator output files, and it stops unless its regional run matches
`out/<GEO>_interval.csv`. The memo's sensitivity section prints the result for
the first five. The three pooled national shares it needs come from
`02_nsece_constants.R`.

```bash
Rscript state_data/03_national_sensitivity.R
```

## Checks

Ten scripts, all exit 1 on failure. Eight are read-only; the other two,
`01_nsch_rate.R --check` and `04_head_start_in_schools.R --check`, rewrite their
own `out/` file before comparing, so each is a check that also refreshes its
own input.

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
Rscript state_data/01_nsch_rate.R --check                  # constants match the survey
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
Rscript src/main.R -r state_targets_all      # baseline for all 18 whole states
```
