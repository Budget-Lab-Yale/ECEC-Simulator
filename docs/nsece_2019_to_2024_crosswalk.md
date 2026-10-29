# NSECE 2019 → 2024 crosswalk for `1a_nsece_processing.R`

Working notes for moving the ECEC-Simulator from the 2019 to the 2024 NSECE. The work sits on John Ricco's branch `feat/nsece-2024`, with no branch of its own.
Everything here was checked on Bouchet against the raw public-use files and the 2024 User's Guides
(ICPSR 39466 v1, released 2026-06-11). Raw data: `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1/{2019,2024}`.
The checks that produced the numbers are in [`src/misc/nsece_2024_checks/`](../src/misc/nsece_2024_checks/);
`02`–`06` re-implement parts of `1a` in Python, so the R pipeline should reproduce them.

## Status (2026-10-29)

- John's branch already holds the code changes listed below: a `1a` that reads both years, the `--nsece-year` switch, the optional 2024 treatments in `docs/nsece_2024/`, and the primary-caregiver and point-in-time commits.
- This file, the checks, and the decisions in the last section sit on top of his commits.
- `1a` runs on 2024. `1b` runs on 2024 only when a tax-unit crosswalk exists beside the ACS file; none does yet, because IPUMS has not released the 2024 `TAXID` and `ADJGINC` (Open item 3).
- The choices that are still open are explained in the next section, and listed with their numbers under *Open items* at the end.

## The open choices

None of these is decided. Each paragraph says what the original model (2019, as on `main`) does, what 2024 does now, and what the choice is.

Sectors. The supply side has four sectors, which are kinds of provider the model prices on their own: Unpaid Center-Based (families pay nothing), Low-Priced Center-Based, High-Priced Center-Based, and Paid Home-Based. Low and High split at the median posted price of the age group, weighted by FTE enrollment. Each sector has a price, a staff count per child-hour, and a non-labor residual, which is price minus labor cost. Parent care, other paid care, and other unpaid care are not supply-side sectors.

`L_0` (Open item 1). `L_0` is the care workforce at the base year, by staff type (no BA, BA). `1a` builds it from the provider side:
- `L_0[type]` is the sum over the four sectors of `L_req[sector, type]` times `child-hours[sector]`.
- `L_req[sector, type]` is staff of that type per child-hour. For centers it is the workforce survey's classroom staff, divided by the sector's FTE enrollment. For the home-based sector it is the provider plus helpers of listed providers in the home-based file, divided by their enrollment.
- `child-hours[sector]` is the sector's FTE enrollment times 40 hours a week times 50 weeks.

The simulation then sets care wages from `w = w_0 × wage growth × (L / L_0)^(1/e)`, where `L` is the labor that simulated care choices need (`3c_run_scenario.R`) and `e` is 4 for no BA and 2 for BA. A policy that raises demand above `L_0` raises wages. The choice is whether `L_0` stays provider-side or becomes the labor that households' care choices imply at the calibration hours. The two differ, so the choice moves the starting point of the wage curve. For the 2019 and 2024 gaps, see Open item 1.

Sector wages (Open item 2). The original model uses one wage pair for every sector, one wage for staff without a BA and one for staff with a BA, from the workforce survey. Low-priced centers pay less than the average, so in 2024 their labor cost comes out too high and the residual is negative (−$0.47 per child-hour). 2019 does not show this (+$0.47). Sector wages give each sector its own pair. That makes the 2024 Low-priced residual +$0.26 and moves the 2019 one from +$0.47 to +$0.95, so the choice also changes 2019 results.

Reweighting (Open item 4). The original model does not reweight: it uses the NSECE weights as given. John's `weights` option rakes the 2024 child weights to an outside benchmark of mothers' employment by family type. The NSECE is off from the CPS in opposite directions in the two years: 53.6% of mothers working in 2019 against 57.1%, and 64.3% in 2024 against 59.6%. Reweighting only 2024 would treat the years differently. The consistent choices are to reweight both years or neither. The 2024 targets can be the March 2024 CPS or the 2024 ACS.

Censored rates (mismatch item 4). NORC hides extreme posted rates to limit disclosure, by censoring them at the 5th and 95th percentile of each time unit and age. The 2019 file deletes them (code −2). `1a` reads −2 as a missing price, so the cell has no sector, and its children and staff leave the sector rows and `L_0`. The 2024 file replaces each one with the cap value, and `1a` uses it as a real price. The choice is whether to delete the 2024 caps, as 2019 does. The caps are real rates cut to the cap value, so deleting also removes usable information. It lowers 2024 `L_0` by about 8%.

Administrative-data centers (mismatch item 5). These are programs built from administrative records, mostly public-school programs, with no price and no workforce data. In the original model, the ones where no family pays (2019 rate flag −8, which `main` reads as free) already sit in the Unpaid sector's enrollment, but with no staff, because they have no workforce records. That makes Unpaid staff per FTE child too low: 0.0517 for staff without a BA in 2019, against 0.0550 with their own staff counts. This branch gives them their own staff count in both years (469 of 483 centers in 2019, 930 of 1,141 in 2024). The ones where families pay sit outside every sector in both years: 13 centers in 2019 and 179 in 2024 (3.0% of interviewed Low- plus High-priced FTE enrollment). The 2024 file withholds their price. Most 2019 ones carry a posted rate (6,413 of 6,762 FTE children), but `1a` sets all admin centers aside before pricing. The choice is where the paying ones belong: outside, in Low-priced, split between Low and High, or, for 2019, by their own price. A fit on region, size, and community context gave no gain over the fixed split in 2024 (mismatch item 5).

NORC's imputed staff counts (mismatch item 7). In 2019, 403 of the 469 free admin centers have a valid staff count; for the 178 records that also carry director items, the count equals the reported classroom staff, so the 2019 counts look reported. The 2019 center file has no imputation flag for the staff count (the 2024 file has `CB4_ADMIN_STAFF_IMP`), and the 2019 user's guide says the administrative data came from district, state, and federal sources, with no imputation of staff counts (checked 2026-10-29 by searching the guide for imputation and reading section 4.2). In 2024, NORC imputed the count for 578 of the 930 free admin centers (enrollment times a district ratio, or 0.11 where no center in the district reported). For large centers that is too high. The code uses NORC's count in both years. The choice is whether to keep it or replace it with an estimate by center size.

The `supply` option. John Ricco added it on 2026-10-17. It does two things: it deletes the 2024 censoring caps, and it measures center staffing from the director's staff count. The original model gets the first from the 2019 file and does not do the second (it uses the workforce file). So the answer to "should we turn it on" is split, and the two parts should be decided separately (see *John's optional 2024 treatments*).

## Files

| Survey | 2019 (ICPSR 37941) | 2024 (ICPSR 39466) | 2024 rows × vars |
|---|---|---|---|
| Household | `37941-0009-Data.tsv` | `39466-0005-Data.tsv` | 6,403 × 6,516 |
| Household calendar | (in household file) | `39466-0004-Data.tsv` | 6,403 × 16,129 — not needed; the code uses the `WST_*` summaries in 0005 |
| Center-based | `37941-0006-Data.tsv` | `39466-0002-Data.tsv` | 7,283 × 342 |
| Home-based | `37941-0007-Data.tsv` | `39466-0003-Data.tsv` | 4,845 × 269 |
| Workforce | `37941-0005-Data.tsv` | `39466-0001-Data.tsv` | 7,511 × 173 |

Restricted-use files (DS0006–0009) are documentation only on Bouchet.

## Naming

Every prefix swaps `9` → `4`: `HH9_`→`HH4_`, `HHC9_`→`HHC4_`, `CB9_`→`CB4_`, `HB9_`→`HB4_`, `WF9_`→`WF4_`.
All variable stems the code uses exist in 2024 except `HH9_RGENDER` (see below).

## Changes that require code edits

All changes in this section are made in `1a` and the `harmonize_nsece_2024_*()` functions. The text records what each one fixes.

### Loop sizes (silent drops if not fixed)
| Loop | 2019 | 2024 | Old code (fixed in `1a`: regex `[0-9]+`) |
|---|---|---|---|
| Household members (`HHM_AGE_X`, `D1A_WORK_X`, `D3D_*_X`, `PARCH_X_Y` member index) | 1–9 | **1–13** | regex `[1-9]` and `str_sub(-1)` → members 10–13 dropped or mis-indexed |
| Providers (`*_X_Y` arrangement vars, `PRIORREL_Y`, `HRS_WEEK_PROVY_X`) | 1–14 | **1–15** | regex `(?:[1-9]\|1[0-4])` → provider 15 dropped |
| Children | 1–9 | 1–9 | no change |

### Respondent gender
2019 `HH9_RGENDER`: 1=Male, 2=Female, 3=DK/Ref, −4=valid skip.
2024 splits into dummies `HH4_RGENDER_1` (Male, 0/1) and `HH4_RGENDER_2` (Female, 0/1); Other and DK are restricted-use only.
Rule: `resp_male = 1 if RGENDER_1==1; 0 if RGENDER_2==1; NA otherwise` (the 2024 dummies are converted in `harmonize_nsece_2024_household()`).

### Child age reference date
`HHC9_AGE_AT_SEP1_18_X` → `HHC4_AGE_AT_SEP1_23_X` (age in months on Sept 1, 2023). The regex already generalizes. −3 = child not born yet; already set to NA.

### Center-based price status (most consequential)
| Code | 2019 `B1_3A_AGERATE_X_R` | 2024 `B1_3A_AGERATE_X_R` |
|---|---|---|
| −8 | **No families pay** (→ code labels FREE) | **CB admin case** |
| −3 | — | Valid skip |
| −1 | — | DK/Ref |
| 1 / 2 | Yes / No rate | Yes / No rate |
| 3 | DK/Ref | — |

Crosstab against `CB4_B1_3_FAMPAY` (2YR shown): −3 & FAMPAY=2 → 1,407 (= free); −3 & FAMPAY=−1 → 103 (missing); −8 → 1,141 admin centers.
New FREE rule: `AGERATE == -3 & B1_3_FAMPAY == 2`. `1a` sets admin cases aside; free ones join Unpaid Center-Based (see *Administrative-data centers*).

### Center-based enrollment and staffing
- `ENRL_*` now carries −8 (admin, 1,141) and −1 (DK). Summed directly, these give **negative enrollment**. `harmonize_nsece_2024_centers()` sets enrollment by age to 0 for both codes before the sum.
- `ENRL_FT_X`: −2 = doesn't serve age, −4 = not enough info (new), −8 = admin. The existing `< 0` handling covers it.
- `STDRATE_WKLY_X`: −9 = no rate or families don't pay, −8 = admin, −1 = not enough info. **−2 no longer exists** (the 2019 MISS code); −1 still maps to MISS.
- `F3_*_TC`, `F3_YOUNGMONTHS_R`: −8 admin, −1 DK. Same pattern as 2019.
- `F15_CHILD_FUND_HS_IND` / `F16_CHILD_FUND_PK_IND`: new codes −3 valid skip, −5/−6/−7 "DK but ≥75% / >50% / <50%". Only 774 HS and 630 PK valid 0/1 vs 1,024 / 873 in 2019.

### Home-based helpers
| | 2019 | 2024 |
|---|---|---|
| `H2_NUMHELP` no helpers | −2 (code sets to 0) | **−3** valid skip |
| `STAFF_EDUC_PRCNT_4YR` no helpers | −2 | **−3** |
| `STAFF_EDUC_PRCNT_4YR` not enough info | — | −5 (new) |

Without a recode, `n_workers = 1 + (−3)` would be wrong for every no-helper provider, and the percent filter would drop them. `harmonize_nsece_2024_home()` maps NUMHELP −3 to the 2019 no-helper code (−2, which `1a` sets to 0) and −1 to the 2019 don't-know code (−4); it maps PRCNT −3 to −2 and −5 to the dropped code −3.

### Workforce wage
New codes −3 (not paid: volunteer/owner, 16) and −5 (not enough info, 34). They fall through to `ADMIN`. `recode_missing_admin_to_na()` sets both `ADMIN` and `MISS` to NA, and no later step reads `wage_status`, so these 50 workers leave the wage averages either way. No change is needed. −8 = admin-spawned (1,141) is unchanged in meaning.

### Household labor
- `D1A_WORK_X`: 3=DK in 2019 → −1 in 2024; −3 valid skip and −33 invalid skip are new. The code keeps only 1/2, so no change is needed beyond the loop size.
- `D3D_UNIT_X`: 2019 blanks → 2024 −3 / −1. Already mapped to NA; 7=Other already excluded.
- `WST_PARCH_HI/LO_X` and `WST_HRS_TOT_*`: −10 (a parent has a missing calendar block) rose from 11 / 1 / 5 child loops
  (HRS_TOT_PHI / HRS_TOT_PLO / PARCH_HI) in 2019 to 412 / 417 / 363 in 2024. The code drops these (`hhm_id > 0, hours >= 0`);
  the comment there now gives these counts. See *Decisions*.

## Unchanged (verified codes match)
`METH_CASEID`, `METH_WEIGHT`, `HHCOMP_MEMBERS`, `HHCOMP_NUMMOTHER/NUMFATHER`, `REGION` (1–4), `RSPOUSEPARTNER_LOOP` (−1 / −2 / HHM index), `HHC_GENDER` (1/2/−1), `PARCH` (adds −2 not-asked; code keeps ==1), `TYPEOFCARE_AGG` (1–8), `CARELOC`, `REGCARE`, `HEADSTART`, `PUBPREK`, `CHILDREN`, `PRIORREL` (2019 code 2 gone), `SUBSIDY`, `WEEKLY_PAYMENT_TC` (0 = free), `HRS_WEEK_PROVY_X` (same provider-then-child order), `HB` `METH_SAMPLE_SOURCE` (1 unlisted / 2 listed), `PRGM_PAID`, `CAREER_EDUC`, `ENRL_TOTAL`, `WF` `WORK_FT`, `WORK_ROLE`, `CHAR_EDUC`, `WORK_WAGE_IMP`, `CB` `WEIGHT_CLSM`.

## Care types (the 15 options)

The household classification (`1a`, provider-level `ecec_choice`, then primary care per child) uses only
`TYPEOFCARE_AGG` (1–3 individual, 4 center/HS/pre-K, 5 other org., 6 K-8, 7 irregular, 8 unknown),
`CARELOC`, `PRIORREL`, `REGCARE`, `WEEKLY_PAYMENT_TC`, and `HRS_WEEK_PROV`. All six have the same
categories and reserve codes in 2024, so the type logic carries over unchanged once loop sizes are fixed.

Primary care of children under 5 in each year, from the current `1a` (child weights; the low/high cutoff is $4.75 in 2019 and $6.86 in 2024; runs `19_new` and `24_new`, 2026-10-29):

| Primary care | 2019 | 2024 |
|---|---|---|
| High-priced center | 7.01% | 9.03% |
| Low-priced center | 6.56% | 9.22% |
| Unpaid center | 5.64% | 5.45% |
| Paid home-based | 3.31% | 3.02% |
| Other paid | 7.81% | 8.34% |
| Unpaid home-based | 0.38% | 0.44% |
| Other unpaid | 12.08% | 15.32% |
| Parent only | 50.91% | 45.31% |
| No category (unclassifiable, or only don't-know arrangements) | 6.31% | 3.87% |

Findings:
- The supply side splits by **age-bin** medians (2024: $7.00 / $5.94 / $5.25 for 0–1 / 2–3 / 4–5 vs 2019 $5.00 /
  $4.44 / $4.00), while the household side uses one cutoff. That inconsistency predates 2024.
- **K-8 is 0% of under-5s in both years**, so transitional kindergarten is not leaking into the K-8 type.
- `TYPEOFCARE_AGG` = 8 (setting unknown) rose from 0.5% to 2.2% of regular arrangements; these land in Other.
  Type 5 (other organizational ECE) fell from 8.7% to 5.5%.
- Before 2026-10-28, in both years, a center arrangement with DK payment (−1) was treated as **unpaid** (cost
  became NA, `is_paid` was FALSE) and landed in Unpaid Center-Based. That is about 0.39M (2019) and 0.29M (2024)
  weighted child-arrangements (all ages, regular and irregular). It is now missing (see *Don't-know cost → missing*):
  those children get no category, the last row of the table.
- Thin cells in 2024: Unpaid home-based PT n=10, FT n=24; Paid home-based PT n=26. The ACS imputation forest
  and the two-child joint outcome (225 cells) will be sparser than in 2019 with 35% fewer children.

## Related work: the Stanford Childcare Model (white paper, October 2026)

Brown, Gibbs, Mahoney and Ninan's model is already estimated on the 2024 NSECE. Choices relevant here:
- **ACS:** household income comes from the **2023 ACS** (in 2024 dollars, CPI-U), and child poverty uses SPM thresholds
  from the **2023 ACS SPM research file**, i.e. the same 2024 ACS/SPM gap, resolved by using 2023.
- **DK costs:** informal-care arrangements with DK/refused cost are excluded; $0 is kept as a real price. Same as our decision.
- **Sample:** drops households whose mother's work hours can't be determined (133) or with more than two resident
  parents (88); keeps the two youngest children 0–4 (or 5, not yet in kindergarten).
- **Classification differs from ours:** six types (public center, private center, home-based, informal relative,
  informal non-relative, maternal care), and the primary arrangement is the one with the **most hours**, not
  Borowsky's paid-first priority. No low/high price split. School, irregular and unknown arrangements are not
  regular care.
- **Supply:** listed home-based providers only (the 2024 home-based guide advises against pooling listed and
  unlisted); center staffing ratios from the sampled classroom, FTE-adjusted.

## Base year → 2024: ACS (open)

The 2024 NSECE is ready to use; the gap is on the ACS side.

- **What's on Bouchet.** `ACS: '/raw_data/ACS/v1/2023'` in `config/default_paths.yaml` actually holds a **2019**
  IPUMS extract (`YEAR = 2019`). The lab's shared 2024 extracts (`raw_data/ACS/acs_common_v2/us2024a`, `us2024c`;
  fixed-width + DDI, not the `ipums_usa.csv` the code reads) have every person variable `1b` uses **except
  `TAXID`, `ADJGINC`, `SPMFAMUNIT`, `SPMTHRESH`, `SPMTOTRES`, `SPMCHXPNS`, `SPMWKXPNS`, `SPMCAPXPNS`.**
  The same shared extracts carry all eight for 2009–2019 and 2021–2023.
- **Why they're missing.** IPUMS takes them from Census's ACS SPM research file, released separately from the
  ACS microdata. A direct IPUMS API request for them on `us2024a` was rejected ("not available in any of the
  samples currently selected"). Census's ACS SPM research-file page lists 2009–2019 and 2021–2023 (page dated
  April 2025); no 2024 file. IPUMS said in December 2025 that it expected the 2024 SPM variables in the first
  half of 2026; as of 2026-10-27 they are not out and there is no date.
  ([Census ACS SPM research files](https://www.census.gov/data/datasets/time-series/demo/supplemental-poverty-measure/acs-research-files.html),
  [IPUMS forum](https://forum.ipums.org/t/spm-variables-in-2024-acs/6898))
- **What each one feeds.**
  - `TAXID`: the tax units that the Tax-Simulator donor match and taxes are computed on. (`main` also replaces it
    with a corrected Census crosswalk built from `raw_data/Census-ACS-SPM/v1/2019`, which exists only for 2019.)
  - `ADJGINC`: baseline AGI and the AGI percentile used in the donor match.
  - `SPM*`: only the SPM poverty-impact output. `1b` already skips that output when the variables are absent.
- **Options.**
  1. **Use the 2023 ACS** (the 2024 NSECE would then pair with a 2023 ACS; `1b` runs unchanged). Open question: use the 2023 ACS, or make 2024 tax units (option 3). The 2023 ACS keeps the IPUMS `TAXID` and `ADJGINC`, but mixes 2023 incomes and demographics with the 2024 survey. The 2023 tax units cannot be attached to 2024 persons: `TAXID` links persons inside one ACS sample, and the 2023 and 2024 samples hold different people.
  2. **Wait** for Census and IPUMS.
  3. **Build 2024 tax units ourselves.** `Budget-Lab-Yale/Affordability-Index` (private repo) has an ACS tax-unit builder (`R/02_income/02_tax_units.R`, pointer method with qualifying-child and qualifying-relative rules). It builds its own units from the IPUMS household pointers (`SPLOC`, `MOMLOC`, `POPLOC`) and uses `TAXID` only as a benchmark, so it does not need the missing 2024 `TAXID`. Tax-Simulator cannot do this step: it computes tax on its own PUF-based tax units and has no ACS input.
     - Units, built 2026-10-29 (`ai-private` on Bouchet, jobs 31284243 for 2024 and 31284244 for 2023; spine and tax-unit stages only). 2024: 3,238,311 persons (unweighted) form 1,758,184 units. No unit spans two households, and every unit has one primary filer. 2023, the same code against IPUMS `TAXID`: 3,228,659 persons; the partitions agree exactly for 88.9% of persons. The builder makes 1,750,114 units against 1,796,379 for IPUMS. It splits IPUMS units for 269,068 persons and merges IPUMS units for 127,207. The builder is a different construction from the IPUMS one, and `1b` uses the IPUMS units in 2023.
     - `1b` change, done 2026-10-29. `1b` reads `tax_unit_crosswalk_<year>.csv` beside `ipums_usa.csv` for any ACS year other than 2019 when the file exists (columns `serialno`, `sporder`, `tax_unit`, `agi`; `tax_unit` is a numeric id unique across households, `agi` is the unit's AGI on every member). It then needs no `TAXID` or `ADJGINC` in the ACS file. Each unit counts as one legacy fragment, so `reconstruct_tax_unit_agi()` returns the unit AGI unchanged. Without the file, the 2024 run still stops at the missing `TAXID` and `ADJGINC`, and the message names the file path. Tested on a toy file with both paths (no `TAXID` or `ADJGINC` columns, then IPUMS columns). Not run on real data: no 2024 crosswalk file exists yet.
     - Plan for AGI, not started. The crosswalk file holds the builder's unit ID and an AGI computed from ACS income components.
       1. Compute unit AGI from the ACS income components of the unit's members.
       2. Run the same computation on the 2023 ACS and compare it with `ADJGINC`. Report the weighted mean difference, the share of units within $1,000, and the AGI percentile shift, since the donor match uses the AGI percentile.
       3. Run it on 2024 and write the crosswalk.
     - This AGI is a proxy. The ACS lacks most capital gains, tax-exempt interest, and several adjustments, so it will understate AGI at the top. The earlier plan required a real tax calculation on the units, through the Affordability-Index donor chain and Tax-Simulator, validated against `ADJGINC` and SOI. The donor chain is the larger piece of work, and its 2024 coverage depends on Tax-Data. Whether the proxy is good enough is decided by the 2023 comparison.
     - SPM poverty output stays off until Census publishes the 2024 SPM file.
- **Not a source:** `model_data/Tax-Data-Bridge/.../tax_units_2024.parquet` is the aged PUF (the same 220,896
  records every year), not ACS persons, so it can't supply ACS tax-unit IDs. It's the donor pool the model
  already matches to.

## Decisions and findings (2026-10-27 to 2026-10-29)

Evidence comes from the checks `03` to `10` in `src/misc/nsece_2024_checks/`, run on Bouchet against both survey years, and from John's memos in `docs/nsece_2024/`.

### Branch

The work sits on `feat/nsece-2024`, John's branch, which already held three of the first five code changes planned here. The `fix/nsece-primary-caregiver` and `fix/point-in-time-employment` commits are inside it.

### Base year: 2024 ACS

The model pairs the 2024 NSECE with the 2024 ACS. John's branch had paired it with the 2023 ACS and converted dollars to 2023; `NSECE_ACS_YEAR_PAIRS` now maps 2024 to 2024, so the dollar conversion factors are 1 (the conversion code in `get_nsece_dollar_conversion.R` stays for reuse). The 2024 ACS person file is on Bouchet at `raw_data/ACS/v1/2024/ipums_usa.csv` (IPUMS extract 36, `us2024a`, 3,422,888 person records, unweighted), named `ACS-2024` in `config/default_paths.yaml`; `1b` reads `ACS-<year>` when that key exists, else `ACS`. The file has no `TAXID` or `ADJGINC`. Where 2024 tax units and AGI come from is open (see *Base year → 2024: ACS* and Open item 3).

### Don't-know cost → missing, for every care type

The status helper in `1a` labels −1 as MISS, which sets the price to NA. The `is_paid` test then turned NA into "unpaid", so a don't-know provider of any type landed in an unpaid category. John's branch keeps that behavior. `is_paid` is now `paid_calc == 1`, so it stays NA: the arrangement gets no category, and the child is classified by its other arrangements or is NA. The change applies to both years. Measured on 2026-10-28 (`1a` with the old and new rule; children under 5, child weights), it moves 2.3 points of 2019 children into no category (4.03% to 6.31%) and 2.7 points of 2024 children (1.18% to 3.87%). Unpaid Center-Based falls from 7.10% to 5.64% in 2019 and from 6.78% to 5.45% in 2024. Other Unpaid falls 0.9 and 1.1 points, and Unpaid Home-Based 0.4 and 0.3 points. Children with no category are left out of the care-choice forest's training data. Among children with a category, Unpaid Center-Based falls 1.4 points in 2019 (7.40% to 6.02%) and 1.2 points in 2024 (6.86% to 5.67%), and Parent Only rises 1.7 and 1.2 points. Regular center arrangements, children under 5:

| | 2019 DK | 2019 free | 2019 paid | 2024 DK | 2024 free | 2024 paid |
|---|---|---|---|---|---|---|
| n (weighted) | 83 (0.30M) | 399 (1.17M) | 664 (2.73M) | 56 (0.25M) | 296 (1.10M) | 616 (3.46M) |
| Head Start or public pre-K | 7% | 56% | 12% | 1% | 32% | 7% |
| Median HH income | $62.5k | $42k | $100k | $103k | $48k | $150k |
| Subsidy item also DK | 50% | 1% | 0% | 37% | 0% | 1% |

The don't-know group looks unlike the free group (higher income, little public pre-K), so "unpaid" is the wrong label for it.

### Low/high center price cutoff

The cutoff was hard-coded at $4.75 per hour. Borowsky et al. (2022, NBER w30140) define it as the median hourly expenditure on the youngest child in the 2019 NSECE (fees over hours, paid providers only). `05_price_cutoff.py` tried the plausible readings on 2019. Only paid center care, youngest child under 5, child-weighted, positive fees, reproduces it: $4.73, against the published $4.75. All paid care gives $4.25, household weights give $4.62, and "regular only" changes nothing because cost is only asked for regular providers. The same recipe gives $6.86 on 2024, so the model keeps $4.75 for 2019 and uses the within-year median for 2024 (`get_nsece_year_settings()`). At $6.86 the 2024 split is 9.0% High-priced and 9.2% Low-priced of children under 5 (2019 at $4.75: 7.0% and 6.6%); keeping $4.75 would give 11.6% and 6.6%. John's branch grew $4.75 by the CPI for day care to $5.76. His own memo calls that index a poor guide, and his within-year median is $6.68.

### Posted-rate codes

A posted rate of −9 ("no rate or families don't pay") would read as a price of −$0.23 per hour if a center with that code were not already flagged free or no-rate. `1a` now sets any negative rate to MISS. `06_supply_checks.py` finds no such center in the 2019 or 2024 public files, so the guard changes no current result.

### Home-based labor totals

The home-based sums of enrollment and workers had no provider weights, in both years. `main` already reads the provider weight (`hb_weight = HB9_METH_WEIGHT`) but never multiplies by it; the code has been that way since the initial commit, so the history gives no reason. Home-based care came to under 1% of the labor stock `L_0`, against 7–16% when weighted. The sums now use `hb_weight`. This changes 2019 results too.

Measured on the 2019 home-based file (listed providers, 3,855 used): weighting multiplies total enrollment, the base for care hours, from 32,097 to 710,414 (22 times). It barely moves staff per child for staff without a BA (0.160 to 0.163, +1.8%) and moves staff with a BA by −12.4% (0.0381 to 0.0334). The no-BA ratio barely moves, but the BA ratio falls 12.4%, so the weight changes the totals and the BA staffing ratio. The center sectors already used center weights, so home-based was the only sector affected.

### Care-choice forest

Decision, 2026-10-28: I want to preserve the original forest specification. `1b` again trains each forest on 50,000 records drawn with replacement in proportion to the child weight, with leaf size 10 and all features at each split, as on `main`. The experimental settings (child weights as case weights, leaf size 50, square-root `mtry`, 63% of records drawn without replacement per tree) are removed. The caregiver-first parent ages and the caregiver rule for NSECE training records stay; they align the training records with the ACS records and are not forest settings.

The experiment is recorded in `docs/nsece_2024/care_choice_shift.md`. It is not an approved model change.

Comparison with the restored forest, 2019 (`1b` run `19_oldforest_1b`, whose code is the same as this branch's 2019 path; children under 5, ACS imputed shares with observed employment, against NSECE shares):
- The absolute gaps over the eight care types sum to 16.8 points (10.4 with the experimental forest). Parent Only is 46.4% in the ACS against 54.3% in the NSECE. Other Paid is 11.9% against 8.3%, and Other Unpaid 14.4% against 12.9%.
- Labor demanded at ACS hours is 83.6% (no BA) and 72.4% (BA) of `L_0`, and 64.5% and 62.7% for centers alone (78.6% and 67.5% with the experimental forest).
- No 2024 `1b` comparison exists: `1b` stops on the missing 2024 tax units (Open item 3).

### Administrative-data centers

Facts:
- Administrative-data centers (`CB9_METH_DATA_SOURCE = 2`) are public-school programs built from administrative records: 483 in 2019 and 1,141 in 2024.
- Their workforce records carry no wage or education data, and they have no classroom data. The center file does carry a staff count for most of them (see "Staff counts in the administrative records" below).
- In 2019 the public file gives their enrollment by age. In 2024 it gives only total enrollment under age 6 and whether the program runs full-time or part-time.
- 75% or more report that no family pays, so they do not move the price medians.

Measured on Bouchet (`04`, `06`, and `07`; Python re-implementation of `1a`, weighted FTE enrollment):

| | 2019 Unpaid | 2019 Low | 2019 High | 2024 Unpaid | 2024 Low | 2024 High |
|---|---|---|---|---|---|---|
| Interviewed centers | 1,452,303 | 1,613,967 | 1,670,810 | 1,287,070 | 1,878,676 | 1,929,731 |
| Admin centers | 126,283 | 5,528 | 885 | 298,430 | 0 | 0 |
| Admin share of sector | 8.0% | 0.3% | 0.1% | 18.8% | 0% | 0% |

- In 2024 every admin center that lands in a sector lands in Unpaid. The 2019 Low and High amounts come from a few centers with a posted rate.
- Admin centers by family-pay item, weighted enrollment under age 6 (headcount):

| | Families pay | No family pays | Don't know |
|---|---|---|---|
| 2019 | 13 centers, 12,700 | 469 centers, 166,215 | 1 center, 43 |
| 2024 | 179 centers, 134,898 | 930 centers, 337,032 | 32 centers, 5,164 |

- The 2024 admin centers where families pay have no price in `1a`, so they fall in no sector under every option. They hold 134,898 children, or 114,999 FTE children, which is 3.0% of Low and High FTE enrollment combined (mismatch item 5). Leaving them out matches 2019, where the same group holds 12,700 children.
- Staff per FTE child, Unpaid sector, `1a` as written against admin dropped from both sides: 2019 no-BA 0.0517 against 0.0562, BA 0.0565 against 0.0614. 2024 no-BA 0.0860 against 0.1060, BA 0.0631 against 0.0777. With the 2024 imputation kept, the 2024 Unpaid ratio looks far higher than 2019 for a reason that has nothing to do with staffing. With admin dropped, the 2019 to 2024 rise is real.
- Confirmed in R on 2026-10-29: `main`'s own `1a` on 2019 (run `19_main`, code of `origin/main`) gives Unpaid staff per FTE child of 0.0517 (no BA) and 0.0565 (BA), the same as the "as written" 2019 figures above. In `main`, a 2019 rate flag of −8 becomes `FREE` (price 0), so free admin centers sit in the Unpaid enrollment with no workers. `main`'s 2019 `L_0` is 1.268 billion staff-hours (no BA) and 0.546 (BA), against 1.492 and 0.603 on this branch (+17.6% and +10.4%). That difference has not been split by source. The changes between the two runs that touch the supply side are the home-based weights, the admin staff counts, and the don't-know and $2,000 fixes.
- Admin enrollment times the interviewed-center ratio comes to about 1.2% of no-BA and 3.0% of BA `L_0` in 2019, and about 3.4% and 6.2% in 2024.

Why the decision is open again:
- NORC's user guides say the interviewed centers alone are not a valid population. They advise restricting to centers not at a public school (`METH_PUBSCHFLAG = 0`) or imputing the administrative cases (2024 center user's guide, PDF p. 11, as cited in `docs/nsece_2024/survey_design_changes.md`).
- Dropping the admin centers gives public-school programs the staffing of the interviewed Unpaid centers. That is correct only if the two staff alike.

Staff counts in the administrative records (`08_admin_staffing.py`; staff per child enrolled under age 6; weighted by `CB9_METH_WEIGHT`; "used" is the unweighted count of records with valid items):
- 2019: 178 of 483 admin records have all four director staff items for ages 0 to 5 (teacher and aide counts, percent full time). For those 178, the total staff count (`CB9_STAFF_TYPE_TOTAL_NUM`) equals the 0 to 5 classroom staff (ratio 0.999). For interviewed centers the 0 to 5 classroom staff are 53% to 72% of the total.
- 2024: no admin record has the four 0 to 5 items (all −8). All 1,141 have `CB9_STAFF_TYPE_TOTAL_NUM`. NORC imputed it for 623 of them (`CB4_ADMIN_STAFF_IMP = 1`), 578 of them among the 930 free admin centers (mismatch item 7).

| Group | 2019 used | 2019 staff per FTE child | 2024 used | 2024 staff per FTE child |
|---|---|---|---|---|
| Admin, total staff count | 412 | 0.113 | 1,141 | 0.120 |
| Admin, 0 to 5 classroom staff FTE | 178 | 0.137 | 0 | none |
| Interviewed public-school, 0 to 5 classroom staff FTE | 685 | 0.202 | 550 | 0.200 |
| Interviewed not public-school, no family pays, 0 to 5 classroom staff FTE | 708 | 0.169 | 772 | 0.230 |
| Interviewed, families pay, 0 to 5 classroom staff FTE | 3,662 | 0.202 | 3,506 | 0.215 |

- Classroom staff FTE counts full-time staff as 1 and part-time staff as 0.5, as option `supply` in `1a` does. The admin total count is a headcount, so it is an upper bound on their staff FTE.
- Admin centers report about 0.12 to 0.14 staff per FTE child. Interviewed centers report 0.17 to 0.23. So admin centers staff at about 55% to 80% of the interviewed rate.
- Dropping the admin centers therefore overstates the staff that public-school programs need. In 2024 they hold 18.8% of Unpaid FTE enrollment, so the bias in the Unpaid ratio is material.
- The admin staff count is measured, not implied, so including the admin centers adds their own staff to `L_0` and their own enrollment to `hours_by_sector`. It does not assume that public-school programs staff like interviewed centers.
- The admin records give no BA split. The interviewed public-school BA share is the nearest substitute.

Decision on 2026-10-27 (it replaced an earlier decision to drop them in both years): include the admin centers in the Unpaid sector with their own staff count, in both years. `1a` does this as follows:
1. It sets the admin centers aside before the price medians, the classroom file, and the workforce ratios. They have no data for any of those.
2. It keeps the admin centers where no family pays. It uses the same rule as for interviewed centers: a rate flag of −8. Admin centers where families pay have no price, so they stay out of every sector, as before.
3. It computes their FTE enrollment under age 6. In 2019 it uses enrollment by age and the full-time share by age. In 2024 it uses total enrollment under 6 and the full-time share from `CB9_ADMIN_FULLTIME_CARE` and `CB9_ADMIN_PARTTIME_CARE` (full time only 100%, part time only 0%, else 50%).
4. It computes staff per FTE child from `CB9_STAFF_TYPE_TOTAL_NUM`. Centers without a valid count get the average of the others.
5. It converts the headcount to FTE, and splits it by BA, with the shares of interviewed public-school teachers and assistants in the workforce file.
6. It adds the admin staff and FTE enrollment to the Unpaid row. The Unpaid ratios become the pooled ratios, and `hours_by_sector` and `L_0` include the admin centers.

The pooled step runs after the optional `supply` treatment, so it applies under either staffing measure. This changes 2019 results.

Measured in the R pipeline (runs `202610271556` for 2019 and `202610271600` for 2024, `1a` only for 2024):

| | 2019 | 2024 |
|---|---|---|
| Admin centers in Unpaid (of all admin centers) | 469 of 483 | 930 of 1,141 |
| Admin centers with a valid staff count | 403 | 930 |
| Admin FTE children (weighted) | 126,283 | 298,430 |
| Admin staff per FTE child (headcount × FTE factor) | 0.114 × 0.869 = 0.099 | 0.117 × 0.922 = 0.108 |
| BA share (interviewed public-school staff) | 0.589 | 0.630 |
| Unpaid no-BA ratio, interviewed only → pooled | 0.0562 → 0.0550 | 0.1060 → 0.0935 |
| Unpaid BA ratio, interviewed only → pooled | 0.0614 → 0.0611 | 0.0777 → 0.0759 |
| `L_0` no-BA (staff-hours) | 1.489 → 1.499 billion | 1.872 billion |
| `L_0` BA (staff-hours) | 0.591 → 0.606 billion | 0.793 billion |

These are the 2026-10-27 figures. Fixes 1 and 2 of 2026-10-28 changed the 2019 results: the pooled Unpaid ratios are now 0.0544 (no BA) and 0.0624 (BA), and `L_0` is 1.492 and 0.603 billion staff-hours (run `19_new`).

- The ratios match a hand pooling of the interviewed and admin ratios by FTE enrollment.
- The Low-priced, High-priced, and home-based inputs do not change. The 2024 Low-priced residual stays at −$0.47.

### Negative non-labor cost for low-priced centers in 2024

The 2024 residual is −$0.47 per child-hour because labor cost uses one wage pair for all centers ($16.80 no-BA, $24.22 BA), while Low-priced centers pay less (sector wages $14.26 and $20.40, from `06`). With sector-specific wages the residual is +$0.26. The Unpaid sector pays the most ($20.73 and $30.39). The same pattern holds in 2019, where the residual is +$0.47 with one wage pair (`1a`; check `06` gives $0.48) and +$0.94 with sector wages (`06`). Neither admin treatment changes the Low residual. The choice is whether to use sector wages in both years; that changes 2019 results.

Checked 2026-10-29 (supply parameters of runs `19_new` and `24_new`):
- The Low-priced residual is the only negative one. 2024: High-priced $4.56, home-based $3.86, Unpaid has no price and so no residual. 2019: Low-priced +$0.47, High-priced $3.32, home-based +$2.89.
- Low-priced labor cost per child-hour rose from $2.84 (2019: 0.1786 × $12.06 + 0.0350 × $19.70) to $4.77 (2024: 0.2034 × $16.80 + 0.0560 × $24.22), up 68%. The implied Low-priced price (labor cost plus residual) rose from $3.32 to $4.30, up 30%.
- The wage pair is the weighted mean wage of workforce-survey teachers and assistants. The 2019 workforce user's guide says that survey interviewed classroom-assigned instructional staff selected from completed center-based provider interviews, so it has no home-based wages. Paid Home-Based uses the same pair, and sector wages as tested apply to the three center sectors only.
- The simulation sets the supplier price as labor cost plus the residual (`P_supply − c(Qd) = delta_0 × cpi_factor`, `3c_run_scenario.R`). With a negative residual, Low-priced labor cost is 111% of its price ($4.77 over $4.30). A 10% wage change then moves that price by 11%, and a 25% payroll subsidy would cut it by 28%.

Sector wage cells (teachers and assistants with known education and a wage, admin centers dropped; unweighted counts of workers, weighted mean hourly wages):

| | 2019 no BA | 2019 BA | 2024 no BA | 2024 BA |
|---|---|---|---|---|
| Unpaid | 418, $13.82 | 390, $26.23 | 642, $20.73 | 544, $30.37 |
| Low | 892, $10.35 | 280, $14.91 | 1,279, $14.25 | 418, $20.40 |
| High | 894, $12.90 | 365, $15.47 | 1,314, $17.62 | 639, $22.06 |

Every cell has at least 280 workers. About 1,000 workers per year have a wage but no sector (no classroom age bin or no usable center price). They enter today's all-center wage but would leave the sector wages. John's option (e5) in `supply_side.md` restates staffing in average-wage hours so `L_0` keeps one unit; it gives +$0.26 for 2024.

### Calendar cases coded −10

Only the parents' hours records drop, not the children, and those parents are then coded as not working. That understates 2024 parent employment by about 3.5 to 5 points. The missing-hours fill (formerly John's `hours` option) fixes it; it is always on for 2024 since 2026-10-28 (mismatch item 3). Only 73% of 2024 households did the weekly calendar, against 94% in 2019, and from June 2024 most got a typical-week question. The wording change alone lowers parent-only care by about 3.4 points. The 2024 survey also puts 64.3% of young children with a working mother, against 59.6% in the March 2024 CPS and 62.9% in the 2024 ACS (check `11`). See `docs/nsece_2024/calendar_and_work_hours.md` and `employment_and_reweighting.md`.

### John's optional 2024 treatments (`--nsece-2024-options`)

`weights` and `supply` are off by default; `hours` was off until 2026-10-28. Sizes come from John's runs, summarized in `docs/nsece_2024/decisions.md`:
- `hours` (always on for 2024 since 2026-10-28, no longer an option) fills in hours for parents with a −10 calendar block from similar workers, as the 2019 pipeline does for other gaps. 2024 employment of two-parent primary caregivers goes from 58.6% to 62.2%, single parents from 64.5% to 68.0%, and partners from 84.9% to 89.8%. Applying the same fix to 2019 moves 2019 employment by 0.4 to 0.7 points. The option now draws donors by earner rank and hourly wage band (mismatch item 3).
- `weights` reweights 2024 child weights to the March 2024 CPS on mothers' employment by family type. It shrinks the 2019 to 2024 change in parent-only care from −6.7 points to between −2.2 and −3.5, and in any paid care from +4.2 to between +0.1 and +1.3. NSECE mothers' employment is 53.6% in 2019 and 64.3% in 2024, against 57.1% and 59.6% in the CPS.
- `supply` (John Ricco, commit `fb56186`, 2026-10-17) does two things. It sets 2024 posted rates equal to NORC's censoring caps to missing, as the 2019 file deleted them. Since 2026-10-29 it finds the caps with the per-unit rule in mismatch item 4; John's version cut at the 5th and 95th percentiles across all pay units, which removes other rates. It also measures center staffing from the director's staff count instead of the workforce file. The original model has neither: its 2019 data arrive with the censored rates already deleted, and it measures staffing from the workforce file. Current figures are in Open item 4.
- The calendar wording change (typical-week households) has no option. Every treatment John tried moves 2024 parent-only care by 1.2 points or less, except counting usual providers in both years, which also moves 2019 (52.6% to 47.9%).

### Other findings, not yet resolved

- John's option studies in `docs/nsece_2024/` quote $5.76 and the 2023 ACS as the setting of his study runs. They are a record of those runs. Each now opens with a status note that points here, and `decisions.md` gives the status of each decision. His 2019 variable inventory was removed on 2026-10-28: its line numbers referred to an old commit, and this crosswalk and check `01` cover its content (it stays in commit `a442c83`).
- The finalize step prints "There were 50 or more warnings" on `main` and on this branch. All 148 are one message: `4a_finalize_output.R` writes column 18 of the Excel distribution sheet from `avg_welfare_ecec_burden_level`, which `3d` never creates, so that column is always blank. It is in `main` since its first commit and does not touch the 2024 work or any model result.

### Mismatches between the two years

Measured on 2026-10-28 on the code of `7b12f29`: `09_definition_checks.py` on the raw files, and `1a` re-run on Bouchet per variant. Weighted figures use the survey weights. "Cells" are center-age cells with enrollment, unweighted.

1. Don't-know payment counts as free in 2019 supply, but not in 2024.
   - In 2019, NORC sets the rate flag `CB9_B1_3A_AGERATE_*_R` to −8 ("No families pay") for every age of a center that answered `B1_3_FAMPAY` = 2 (none pay) or 3 (don't know). `1a` reads −8 as free. In 2024, −8 marks admin cases, so `harmonize_nsece_2024_centers()` sets free status from FAMPAY = 2 only, and don't know becomes a missing price.
   - The two rules agree on 99.3% of 2019 interviewed-center FTE enrollment. The rest is 142 cells and 43,688 weighted FTE children with FAMPAY = 3: 3.0% of interviewed Unpaid Center-Based FTE enrollment.
   - The household side already treats don't know as missing (see *Don't-know cost → missing*), so 2019 supply is the only place that calls it free.
   - The 66 don't-know centers look like paying centers, not free ones (`10_fix_evidence.py` section E, unweighted shares of centers). 76.8% report tuition revenue (93.3% of paying centers, 9.4% of free centers). 26.0% are for profit (46.1% and 6.5%). 13.9% are public schools (10.7% and 49.6%). In 2024 the 103 don't-know centers are alike: 70.3% report tuition, 48.4% are for profit. They have no posted rate, so a missing price is the only choice that does not invent one.
   - Fix, approved and implemented on 2026-10-28: in 2019, treat −8 cells of FAMPAY = 3 centers (66 centers) as a missing price. Measured before the fix went in: the 2019 Unpaid staff per FTE child-hour goes from 0.0550 to 0.0544 (no BA) and from 0.0611 to 0.0624 (BA), and `L_0` falls 0.4% (no BA) and 0.2% (BA). No other supply parameter changes. 2024 already follows this rule.
2. Posted rates above $50 per hour.
   - The NORC censoring (item 4) works within each time unit, so it leaves outliers in the weekly rate.
   - Cells with a weekly rate above $2,000 ($50 per hour at 40 hours): 38 in 2019 (6 at age 2, 15 at age 3, 17 at age 4; the largest is $146,000 per week) and 8 in 2024. Each one gets a price in `1a`.
   - They are unit errors, not real rates (`10_fix_evidence.py` section F). All 38 are paying centers (FAMPAY = 1). Most are $2,800 to $3,990 per week, which is $604 to $918 per week if read as a month; often the same center has the error at several ages. Others read as a year: $48,960 is $942 per week, and $146,000 is $2,808. Some sit next to normal rates of $144 to $230 per week at the other ages of the same center.
   - They change results. The sector price is a weighted mean (`1a`, and `main` too); the median sets only the Low/High split. `main` uses them as well. John's `docs/nsece_2024/prices.md` puts the 2019 High price at $8.08 with them and $6.70 without them.
   - Adjusting them was tested on 2026-10-29 (Bouchet, job 31305818) and is not safe. The public file has only the converted weekly rate, with no reported time unit or raw amount. There are 40 flagged 2019 cells in 24 centers (10 in 6 centers in 2024), counting cells without enrollment. Only 8 (3 in 2024) sit at a center with a rate for another age. Of those 8, reading the amount as monthly (divide by 4.34524) lands within half to double of the center's other-age median for 2 cells, and reading it as yearly (divide by 52) for none. In 2024 the yearly reading fits 1 of 3.
   - Fix, approved and implemented on 2026-10-28: set weekly rates above $2,000 to missing in both years. Measured before the fix went in: the 2019 High-priced residual falls from $4.72 to $3.32 per child-hour, so the outliers lift it by $1.40 (42%). The 2024 High-priced residual falls from $4.60 to $4.56. The Low-priced residuals move by less than $0.01, and `L_0` by 0.3% or less. With the item 4 variant as well, the High residual is $4.60 (run `24_caps50`). The run removes 10 rates above $2,000 in 2024: the 8 cells above count only cells with enrollment, the run counts every rate.
3. 2024 parent work hours missing for calendar code −10.
   - Without `hours`, those parents count as not working. The share of children under 5 whose mother works (child weights, hours above 0) is 58.85% in 2024 without `hours` and 62.17% with it. Two-parent fathers go from 85.27% to 90.48% (household weights).
   - Checked 2026-10-29 by comparing the 2024 runs without and with the fill (`24_none`, `24_new`). Of 5,598 parents of children under 5 (unweighted), 263 had no hours record (4.7%). The fill covers 252: 209 get donor hours and 43 get zero. 11 stay missing, because they have no usable D1a answer. No parent who already had hours changed. After the fill, 1,539 parents have zero hours and 4,048 have hours above 0.
   - The zero is the parent's own answer, not an imputation: a parent with no hours record who answers D1a "no" did no paid work last week. The 2019 pipeline applies the same rule when calendar hours conflict with D1a: parents with positive calendar hours who say they did not work are set to zero (328 parents in 2019, 271 in 2024), because the calendar also counts school, training, and commuting. A parent who was truly not working stays at zero.
   - The filled parents earn less than donors: 37.9% have no pay record against 17.5%, and 11.2% earn $40 or more an hour against 23.0% (weighted). The first version drew donors by earner rank only, so it gave these parents the hours of higher-paid parents.
   - Fixed on 2026-10-28: the `hours` option now draws donors with the same earner rank and the same hourly wage band (under $15, $15–25, $25–40, $40 or more, no wage). The smallest donor cell has 270 parents. Weighted mean weekly hours of the filled parents against donors: first earners 44.7 against 43.9, second earners 35.1 against 35.9 (run `24_hours2`). Before the fix, two-parent fathers got 39.9. Within single wage bands the filled means differ from the donor means by up to 12.4 hours, because each band holds 5 to 61 filled parents; the draw is random, not biased.
   - Work rates do not change with the donor rule (hours above 0 either way): mothers in two-parent units 59.33% without `hours` and 63.18% with it. The hours enter `1b` and calibration only.
   - Approved and implemented on 2026-10-28: the fill is always on for 2024 and is no longer an option (`--nsece-2024-options hours` is accepted and ignored). It changes no care share and no supply parameter.
4. 2019 deletes the censored rates, 2024 keeps them at the cap.
   - Both years use one rule: each reported time unit (hourly, half-day, full-day, weekly, monthly, term, and so on) is censored at the 5th and 95th percentiles of the weekly rates in that unit, for each age. NORC does this "to limit disclosure". 2019 deletes the censored rates (code −2, "Trimmed Value"): 9.0% to 9.5% of positive rates per age. 2024 gives each one the exact cap value of its unit, with no flag (2024 center user's guide p. CB-508).
   - The 2024 caps are therefore visible as pile-ups of one value. Per age, the four largest are the weekly and the monthly caps. Age 3: $130 (115 rates) and $390 (106) per week; $69.04 (63) and $540.82 (62), which are $300 and $2,350 per month. These four hold 9.2% to 10.3% of 2024 positive rates per age, close to the 2019 share. Smaller pile-ups (for example $775 at age 3, 12 rates) are the caps of rarer units.
   - John's original `supply` trim cut at the 5th and 95th percentiles across all units, which removes 10.0% to 11.2% of 2024 rates but not the same rates as NORC. The option now uses the rule in the next bullet (2026-10-29).
   - The rule (in the `supply` option since 2026-10-29): in 2024, set positive rates equal to the four caps of their age to missing (−2), as 2019 does. The rule finds the caps itself: per age, the most frequent value below the 15th and above the 85th percentile, separately for monthly amounts (rate × 4.34524 is a whole dollar) and for the others. Measured (run `24_caps2`, current code): the Low-priced residual goes from −$0.47 to −$0.25 per child-hour and the High-priced from $4.56 to $4.60. Low no-BA staff per FTE child goes from 0.2034 to 0.2010 and High from 0.1560 to 0.1513. `L_0` falls 8.0% (no BA) and 7.4% (BA), because centers without a price leave the sector rows, as the trimmed 2019 centers already do.
   - Deleted rates drop their center-age cells, with their staff, from the sector rows in both years; in 2024 that is the 8% fall in `L_0`. `main` does the same in 2019.
   - Priced enrollment each year keeps (`10_fix_evidence.py` section I; interviewed centers where families pay, weighted FTE children by 1a age bin; a cell is priced if any rate in the bin is positive):

     | Cell class | 2019 | 2024 now | 2024, caps deleted |
     |---|---|---|---|
     | Priced | 75.5% | 87.4% | 79.2% |
     | Trimmed (−2, or caps deleted) | 9.2% | 0.0% | 8.2% |
     | No time unit (−1) | 6.2% | 6.3% | 6.3% |
     | No rate or other code | 9.2% | 6.3% | 6.3% |
     | All, weighted FTE children | 4,371,347 | 4,365,088 | 4,365,088 |

   - With the caps deleted, the trimmed share is 8.2% in 2024 against 9.2% in 2019, and the priced share 79.2% against 75.5%. The "no rate or other" share differs by 2.9 points; that is not a censoring difference.
   - Open: whether cells without a price should keep a sector and their staff. That change would also move the 2019 results of `main`. Measured on 2026-10-29 with fixes 1 to 3 in place (runs `19_keep`, `24_keep`, `24_capskeep` against `19_new`, `24_new`, `24_caps2`). Test rule: an enrolled cell without a price takes its center's class from the center's other priced bins (the mean of price over bin median: 0 is Unpaid, below 1 is Low, 1 or more is High). Its price stays missing, so sector prices do not change; its FTE children and staff join the sector.
   - Most unpriced cells cannot use this rule, because their center has no priced bin at all. Cells and FTE children below are unweighted cells and weighted FTE children:

     | | 2019 | 2024 | 2024, caps deleted |
     |---|---|---|---|
     | Enrolled cells without a price | 3,481 | 2,304 | 3,120 |
     | Cells given a sector | 1,122 | 869 | 1,095 |
     | Weighted FTE children given a sector | 225,199 of 1,142,289 (19.7%) | 160,717 of 661,835 (24.3%) | 242,127 of 1,014,923 (23.9%) |
     | Of these: Low / High / Unpaid | 116,687 / 92,198 / 16,314 | 75,946 / 73,742 / 11,029 | 136,495 / 94,414 / 11,218 |
     | `L_0` no BA / BA | +7.6% / +2.6% | +3.1% / +3.7% | +5.1% / +6.3% |
     | Low residual ($ per child-hour) | +0.47 to +0.41 | −0.47 to −0.46 | −0.25 to −0.21 |
     | High residual ($ per child-hour) | 3.32 to 3.34 | 4.56 to 4.54 | 4.60 to 4.50 |

   - With the caps deleted and the cells kept, 2024 `L_0` is 3.3% (no BA) and 1.5% (BA) below the current code, against 8.0% and 7.4% below with the caps deleted alone. Without the caps deleted, the rule adds more staff in 2019 (+7.6% no-BA `L_0`) than in 2024 (+3.1%), because 2019 has more unpriced enrollment (1,142,289 weighted FTE children against 661,835). Not decided; test variant only.
5. Admin centers where families pay are outside every sector.
   - 2024: 179 centers, 114,999 weighted FTE children (3.0% of interviewed Low- plus High-priced FTE enrollment), plus 32 don't-know centers with 3,366. 2019: 13 centers, 6,762 (0.2%).
   - The 2024 file withholds their price, so the code puts them in no sector. In 2019 most carry a posted rate (6,413 of the 6,762 FTE children), but `1a` still sets admin centers aside, so they sit in no sector there too. The 2019 admin centers with a posted rate are 86.2% Low-priced (6,413 FTE; mean $3.66 per child-hour). Paying public-school programs, the nearest interviewed match, are 77.9% Low-priced in 2019 ($6.40 mean, 193,793 FTE) and 72.1% in 2024 ($4.79 mean, 138,960 FTE) (`10_fix_evidence.py` section G).
   - Their staffing: 0.1165 staff per FTE child in 2024 (own counts, all 179 valid; headcount × the public-school FTE factor), against 0.1079 for free admin centers. Interviewed Low-priced centers have 0.2594 (no BA plus BA) and High-priced 0.2311. 2019 has 13 centers, 9 with a count; they take the free admin rate, 0.0991.
   - Sector fit, tested 2026-10-29 (Bouchet, `nsece_pricefit`, jobs 31305163 and 31305458; scratch scripts, not in the repo). It predicts the Low or High band of the 110 (2024) and 116 (2019) interviewed paying public-school programs from variables that paying admin centers share with them (region, log FTE size, community urban and poverty density, school sponsor, building type), by a ridge-penalized logit. The class is a center-level rule (mean of price over its age-bin median, 1 or more is High), so its Low shares differ from the 77.9% and 72.1% above by 2 to 5 points. Weighted 5-fold cross-validation, 20 repeats, log loss (lower is better):
     - 2019: fixed split 0.485, region and size 0.416, with context 0.367 (AUC 0.75 and 0.83). On the 12 paying admin centers that carry a price, 0.419 for the fixed split and 0.344 with context.
     - 2024: fixed split 0.667, region and size 0.692, with context 0.712 (AUC below 0.5). The fit moves the Low share of the 114,999 FTE children from 70.3% to 71.3% to 72.3%.
     - Recommendation: the fixed 72/28 split for 2024 (the price is withheld and the covariates carry no signal); each 2019 center's own price where it has one. Not decided.
   - Measured, with their FTE enrollment and staff added to the sector rows (runs `24_admlow`, `24_admhigh`, `24_admsplit`; price unchanged, as they have no price; the split puts 72% in Low and 28% in High, the shares of paying public-school programs):

     | 2024 | Now | In Low | In High | Split 72/28 |
     |---|---|---|---|---|
     | Low no BA / BA per FTE child | 0.2034 / 0.0560 | 0.1942 / 0.0570 | unchanged | 0.1966 / 0.0567 |
     | High no BA / BA per FTE child | 0.1559 / 0.0752 | unchanged | 0.1496 / 0.0751 | 0.1541 / 0.0752 |
     | Low residual ($ per child-hour) | −0.47 | −0.34 | −0.47 | −0.37 |
     | `L_0` no BA / BA | | +0.5% / +2.1% | +0.5% / +2.1% | +0.5% / +2.1% |

     The High-priced residual is not shown: these runs predate the $2,000 fix, which moved it ($4.60 became $4.56).

   - `L_0` gains the same staff under every placement. In 2019 every placement moves each ratio by less than 0.4% and `L_0` by 0.1% (runs `19_admlow`, `19_admhigh`, `19_admsplit`).
   - Not decided; all three placements are test variants only. The price evidence puts 72% to 86% of comparable enrollment in Low, so Low and the split are the placements the evidence supports. They differ by $0.03 in the Low residual.
6. Free status is center-wide in both years, not by age.
   - The 2019 −8 flag is set for every age of a center, as FAMPAY is. 2024 has no by-age flag. The two years match, so no fix is needed.
7. NORC imputed most 2024 admin staff counts.
   - NORC imputed `CB9_STAFF_TYPE_TOTAL_NUM` for 578 of the 930 free 2024 admin centers (flag `CB4_ADMIN_STAFF_IMP`). They report 0.1138 staff per FTE child against 0.1228 for the 352 reported ones.
   - NORC's method: total enrollment 0–5 times the district's staff:student ratio, or 0.11 where no center in the district reported (2024 center user's guide p. CB-156). The imputed counts are not whole numbers, and 437 of the 578 sit at exactly 0.110 staff per enrolled child.
   - Tests in `10_fix_evidence.py` section J (weighted by center weights):
     - The reported counts are whole numbers; 92.0% of the imputed counts are not. 75.6% of the imputed centers sit at exactly 0.110 staff per enrolled child (NORC's default ratio); the other 24.4% got a district ratio.
     - The imputed centers are larger (mean 45.2 FTE children against 30.7) and more often full time (full-time share 0.91 against 0.62). Their region mix also differs: region 3 is 16.7% of imputed and 66.5% of reported centers.
     - Reported staff per FTE child falls with size, from 0.186 in the smallest quarter to 0.088 in the largest. NORC's imputed counts are almost flat: 0.138 to 0.112. In the largest quarter, which holds 234 of the 578 imputed centers, NORC is 28% above the reported centers of the same size.
     - Every imputed center lies inside the reported centers' size range (0.8 to 192.5 FTE children), so the size-based estimates below do not extrapolate.
   - Weighted staff totals, and their effect in `1a` (runs `24_stflat`, `24_stcells`, `24_stloglog`):

     | Rule for the 578 imputed centers | Staff, imputed centers | All 930, vs NORC | Unpaid no BA / BA per FTE child | `L_0` no BA / BA |
     |---|---|---|---|---|
     | NORC's counts (current code) | 21,504 | 0% | 0.0935 / 0.0759 | |
     | Flat reported ratio | 23,203 | +4.9% | 0.0939 / 0.0765 | +0.1% / +0.2% |
     | Reported ratio by size quarter × full time | 15,716 | −16.6% | 0.0923 / 0.0738 | −0.2% / −0.8% |
     | Log-log size fit (slope 0.465) | 18,540 | −8.5% | 0.0929 / 0.0748 | −0.1% / −0.4% |
     | Log-log size fit + region (`24_streg`) | 15,530 | −17.1% | 0.0922 / 0.0737 | −0.2% / −0.9% |
     | Log-log size fit + region + full-time share (`24_stregs`) | 14,362 | −20.4% | 0.0920 / 0.0733 | −0.3% / −1.0% |

     Runs `24_stbase`, `24_stflat`, `24_stcells`, `24_stloglog`, `24_streg`, and `24_stregs` (2026-10-29, current code, 352 reported centers in the fits). The first four reproduce the 2026-10-28 numbers exactly.

   - Which is more accurate: within the same size range, the reported centers imply fewer staff than NORC's counts for the large, full-time centers that make up most of the imputed group. So the flat reported ratio is the least accurate rule, and NORC's counts are probably too high. Region does not explain the gap (section J6): reported centers in regions 1 and 2 have about 0.06 staff per FTE child and in regions 3 and 4 about 0.13, and most imputed centers are in regions 1 and 4. A log-log fit with region gives 15,530 staff for the imputed centers (−27.8% against NORC; −17.1% for all 930), and with the full-time share as well 14,362 (−33.2%; −20.4%). Region 1 has only 18 reported centers, so its coefficient is uncertain. NORC used its 0.110 default for 75% to 92% of imputed centers in regions 1, 2, and 4, against 21% in region 3, so most imputed counts carry no local information.
   - Not decided. The production code uses NORC's counts. The choice moves 2024 Unpaid staff per child by −2.8% to +0.8% for the rules with both ratios read, and `L_0` by +0.1% to −0.3% (no BA) and +0.2% to −1.0% (BA), including the region fits.

### Open items

1. What `L_0` is anchored to (provider-side staffing, as now, or labor demanded at calibration hours). At NSECE household-side hours (children under 5; part time 520 and full time 2,080 hours a year), labor demanded is 72.0% (no BA) and 66.0% (BA) of `L_0` in 2019, and 74.9% and 70.8% in 2024. For centers alone it is 58.0% and 66.3% (no BA). These figures include fixes 1 to 3 (runs `19_new`, `24_new`, 2026-10-29). At the ACS hours the model uses (2019 `1b` with the restored care-choice forest, children under 5, observed employment), it is 83.6% (no BA) and 72.4% (BA) of `L_0`, and 64.5% and 62.7% for centers alone. Anna, 2026-10-29: this is an equilibrium model whose demand side is matched to the base year later, by design, so the pre-calibration gap is not an NSECE 2019 to 2024 choice, and the 2024 status memo leaves it out. Not checked against the calibration code. `supply_side.md` Choice 6 shows the first-year wage solving 2% to 15% below the survey wage under provider-side `L_0`, so confirm how calibration treats that before closing this item.
2. Sector-specific wages for the center labor cost, in both years, to remove the negative Low-priced residual in 2024 (explained in *The open choices*). Measured on 2026-10-29 with fixes 1 to 3 in place (runs `19_secw`, `24_secw`), as John's option e5: each center sector's staffing is restated in all-center-wage hours (ratio × sector wage / all-center wage), so labor cost is the sector wage times staffing and `L_req` and `L_0` stay in one labor unit. The ratios in the table are all-center-wage staff-hours per child-hour, not staff per child. The sector wage cells are in *Negative non-labor cost for low-priced centers in 2024*.

   | | 2019 now | 2019, sector wages | 2024 now | 2024, sector wages |
   |---|---|---|---|---|
   | Low residual ($ per child-hour) | +0.47 | +0.95 | −0.47 | +0.26 |
   | High residual ($ per child-hour) | 3.32 | 3.52 | 4.56 | 4.59 |
   | Unpaid no BA / BA per FTE child-hour | 0.0544 / 0.0624 | 0.0625 / 0.0829 | 0.0935 / 0.0759 | 0.1154 / 0.0951 |
   | Low no BA / BA | 0.1786 / 0.0350 | 0.1532 / 0.0265 | 0.2034 / 0.0560 | 0.1725 / 0.0472 |
   | High no BA / BA | 0.1559 / 0.0758 | 0.1667 / 0.0592 | 0.1560 / 0.0752 | 0.1636 / 0.0685 |
   | `L_0` no BA / BA | | −1.4% / −3.3% | | −0.9% / +0.2% |

3. 2024 tax units, AGI, and SPM (*Base year → 2024: ACS* has the detail). `1b` reads `raw_data/ACS/v1/2024/ipums_usa.csv` (extract 36, config key `ACS-2024`) and needs a tax-unit crosswalk beside it, because IPUMS has not released `TAXID`, `ADJGINC`, or the six SPM variables for 2024. Open question: pair the 2024 NSECE with the 2023 ACS, or make 2024 tax units and AGI. The units are built and the `1b` change is done; the AGI step and the crosswalk file are not.
4. Decide whether to turn on John's `weights` and `supply` options (explained in *The open choices*; `hours` is now always on). Measured on 2026-10-29 on the current code (`1a` per option; children under 5, child weights, children with no care category in the denominator; runs `19_new`, `24_new`, `24_caps2`, `24_supply2`, `24_wcps`, `24_wacs`):

   | | 2019 | 2024 now | `supply` | `supply`, rate deletion only |
   |---|---|---|---|---|
   | Parent Only | 50.91% | 45.31% | 45.31% | 45.31% |
   | Any paid care | 24.68% | 29.61% | 29.61% | 29.61% |
   | Low-priced residual per child-hour | +$0.47 | −$0.47 | +$0.71 | −$0.25 |
   | High-priced residual per child-hour | $3.32 | $4.56 | $4.57 | $4.60 |
   | Unpaid no-BA / BA staff per FTE child-hour | 0.0544 / 0.0624 | 0.0935 / 0.0759 | 0.1090 / 0.0872 | 0.0935 / 0.0759 |
   | `L_0` no BA / BA, billion staff-hours | 1.492 / 0.603 | 1.872 / 0.793 | 1.637 / 0.734 | 1.722 / 0.735 |

   - 2019 staffing and `L_0` include fixes 1 and 2. Fixes 1 to 3 change no care share in either year.
   - `supply` does two things, and they can be decided separately. The first deletes the 2024 censoring caps, as the 2019 file did (mismatch item 4); it removes 253, 309, 346, and 360 rates at the four ages, the same rates as run `24_caps2`. The second measures center staffing from the director's count. The rate deletion alone cuts `L_0` by 8.0% (no BA) and 7.4% (BA). Both together cut it by 12.6% and 7.5%, so director staffing adds a cut of 4.6 points for staff without a BA and almost none for staff with a BA. With John's original percentile trim the Low-priced residual was +$0.86 and `L_0` 1.645 / 0.706.
   - `weights`, with the March 2024 CPS or the 2024 ACS as targets. The ACS factors use John's five groups and raking code on the 2024 ACS (`nsece_evidence/acsw` on Bouchet). On John's 2023 ACS they reproduce his factor range, 0.828 to 1.711. On the 2024 ACS the range is 0.805 to 1.802, against 0.695 to 1.232 for the CPS. Raked to the ACS, 62.9% of children under 5 with a parent have their caregiver at work, against 64.3% with the survey weights alone. Care shares in 2024, children under 5:

     | 2024 | No `weights` | `weights`, CPS | `weights`, ACS |
     |---|---|---|---|
     | Parent Only | 45.31% | 47.79% | 46.10% |
     | Any paid care | 29.61% | 28.64% | 29.21% |
     | Fall in Parent Only from 2019 | 5.6 points | 3.1 points | 4.8 points |
     | Rise in any paid care from 2019 | 4.9 points | 4.0 points | 4.5 points |
     | Home-based residual ($ per child-hour) | 3.86 | 4.09 | 3.97 |

   - The ACS targets move 2024 less than the CPS targets, because the ACS puts more mothers at work (62.9%) than the CPS (59.6%). The ACS is the survey the model calibrates to. `weights` changes no supply parameter except the 2024 home-based residual. Among children with a care category, the shares the care-choice forest trains on, the CPS targets cut the fall in Parent Only from 7.2 to 4.6 points and the rise in any paid care from 4.5 to 3.5 points.
   - John's sizes in *John's optional 2024 treatments* above come from his runs on older settings.
5. Run a 1% smoke test on 2024 (no `-d` or `-c`) once item 3 is resolved. The 2019 smoke test passed on the admin-center code (jobs 31007018 and 31007019), and again on 2026-10-29 with fixes 1 to 3 (jobs 31281999, 31282000, and 31282001). The ACS-path change (now in `02f1495`) was tested on its own (jobs 31132240 and 31132400).
