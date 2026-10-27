# NSECE 2019 → 2024 crosswalk for `1a_nsece_processing.R`

Working notes for moving the ECEC-Simulator from the 2019 to the 2024 NSECE. The work sits on John Ricco's branch `feat/nsece-2024`, with no branch of its own.
Everything here was checked on Bouchet against the raw public-use files and the 2024 User's Guides
(ICPSR 39466 v1, released 2026-06-11). Raw data: `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1/{2019,2024}`.
The checks that produced the numbers are in [`src/misc/nsece_2024_checks/`](../src/misc/nsece_2024_checks/);
`02`–`06` re-implement parts of `1a` in Python, so the R pipeline should reproduce them.

## Status (2026-10-27)

- John's branch already holds the code changes listed below: a `1a` that reads both years, the `--nsece-year` switch, the optional 2024 treatments in `docs/nsece_2024/`, and the primary-caregiver and point-in-time commits.
- This file, the checks, and the decisions in the last section sit on top of his commits.
- Nothing from the R pipeline has run on 2024 yet. All numbers come from the Python checks.
- Open items are listed at the end of *Decisions and findings*.

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

### Loop sizes (silent drops if not fixed)
| Loop | 2019 | 2024 | Code today |
|---|---|---|---|
| Household members (`HHM_AGE_X`, `D1A_WORK_X`, `D3D_*_X`, `PARCH_X_Y` member index) | 1–9 | **1–13** | regex `[1-9]` and `str_sub(-1)` → members 10–13 dropped or mis-indexed |
| Providers (`*_X_Y` arrangement vars, `PRIORREL_Y`, `HRS_WEEK_PROVY_X`) | 1–14 | **1–15** | regex `(?:[1-9]\|1[0-4])` → provider 15 dropped |
| Children | 1–9 | 1–9 | no change |

### Respondent gender
2019 `HH9_RGENDER`: 1=Male, 2=Female, 3=DK/Ref, −4=valid skip.
2024 splits into dummies `HH4_RGENDER_1` (Male, 0/1) and `HH4_RGENDER_2` (Female, 0/1); Other and DK are restricted-use only.
New rule: `resp_male = 1 if RGENDER_1==1; 0 if RGENDER_2==1; NA otherwise` (used at lines ~154 and ~307).

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
New FREE rule: `AGERATE == -3 & B1_3_FAMPAY == 2`. Admin cases → the existing ADMIN status (see *Decisions*).

### Center-based enrollment and staffing
- `ENRL_*` now carries −8 (admin, 1,141) and −1 (DK). The code sums `ENRL` directly → **negative enrollment**. Must set `< 0` to NA.
- `ENRL_FT_X`: −2 = doesn't serve age, −4 = not enough info (new), −8 = admin. The existing `< 0` handling covers it.
- `STDRATE_WKLY_X`: −9 = no rate or families don't pay, −8 = admin, −1 = not enough info. **−2 no longer exists** (the 2019 MISS code); −1 still maps to MISS.
- `F3_*_TC`, `F3_YOUNGMONTHS_R`: −8 admin, −1 DK. Same pattern as 2019.
- `F15_CHILD_FUND_HS_IND` / `F16_CHILD_FUND_PK_IND`: new codes −3 valid skip, −5/−6/−7 "DK but ≥75% / >50% / <50%". Only 774 HS and 630 PK valid 0/1 vs 1,024 / 873 in 2019.

### Home-based helpers
| | 2019 | 2024 |
|---|---|---|
| `H2_NUMHELP` no helpers | −2 (code sets to 0) | **−3** valid skip |
| `STAFF_EDUC_PRCNT_4YR` no helpers | −2 | **−3** (code currently drops −3) |
| `STAFF_EDUC_PRCNT_4YR` not enough info | — | −5 (new) |

As written, the code would compute `n_workers = 1 + (−3)` for every no-helper provider and drop them on the percent filter. New rule: NUMHELP −3 → 0; PRCNT −3 → 0 (irrelevant when no helpers); drop −1 and −5.

### Workforce wage
New codes −3 (not paid: volunteer/owner, 16) and −5 (not enough info, 34). Today they fall through to `ADMIN`. Map both to `MISS`. −8 = admin-spawned (1,141) is unchanged in meaning.

### Household labor
- `D1A_WORK_X`: 3=DK in 2019 → −1 in 2024; −3 valid skip and −33 invalid skip are new. The code keeps only 1/2, so no change is needed beyond the loop size.
- `D3D_UNIT_X`: 2019 blanks → 2024 −3 / −1. Already mapped to NA; 7=Other already excluded.
- `WST_PARCH_HI/LO_X` and `WST_HRS_TOT_*`: −10 (a parent has a missing calendar block) rose from 11 / 1 / 5 child loops
  (HRS_TOT_PHI / HRS_TOT_PLO / PARCH_HI) in 2019 to 412 / 417 / 363 in 2024. The code drops these (`hhm_id > 0, hours >= 0`);
  the comment there ("12 records") needs updating. See *Decisions*.

## Unchanged (verified codes match)
`METH_CASEID`, `METH_WEIGHT`, `HHCOMP_MEMBERS`, `HHCOMP_NUMMOTHER/NUMFATHER`, `REGION` (1–4), `RSPOUSEPARTNER_LOOP` (−1 / −2 / HHM index), `HHC_GENDER` (1/2/−1), `PARCH` (adds −2 not-asked; code keeps ==1), `TYPEOFCARE_AGG` (1–8), `CARELOC`, `REGCARE`, `HEADSTART`, `PUBPREK`, `CHILDREN`, `PRIORREL` (2019 code 2 gone), `SUBSIDY`, `WEEKLY_PAYMENT_TC` (0 = free), `HRS_WEEK_PROVY_X` (same provider-then-child order), `HB` `METH_SAMPLE_SOURCE` (1 unlisted / 2 listed), `PRGM_PAID`, `CAREER_EDUC`, `ENRL_TOTAL`, `WF` `WORK_FT`, `WORK_ROLE`, `CHAR_EDUC`, `WORK_WAGE_IMP`, `CB` `WEIGHT_CLSM`.

## Care types (the 15 options)

The household classification (`1a`, provider-level `ecec_choice`, then primary care per child) uses only
`TYPEOFCARE_AGG` (1–3 individual, 4 center/HS/pre-K, 5 other org., 6 K-8, 7 irregular, 8 unknown),
`CARELOC`, `PRIORREL`, `REGCARE`, `WEEKLY_PAYMENT_TC`, and `HRS_WEEK_PROV`. All six have the same
categories and reserve codes in 2024, so the type logic carries over unchanged once loop sizes are fixed.

Replicated in Python on both years (`src/misc/nsece_2024_checks/02_care_types.py`; children 0–4, child-weighted):

| Primary care | 2019 ($4.75) | 2024 ($4.75) | 2024 ($6.86, recomputed) |
|---|---|---|---|
| High-priced center FT / PT | 4.4 / 2.6 | 8.6 / 3.0 | 6.5 / 2.5 |
| Low-priced center FT / PT | 5.9 / 0.7 | 5.5 / 1.1 | 7.6 / 1.6 |
| Unpaid center FT / PT | 4.8 / 2.3 | 5.3 / 1.5 | same |
| Paid home-based FT / PT | 3.1 / 0.3 | 2.4 / 0.7 | same |
| Other paid FT / PT | 5.5 / 2.2 | 6.7 / 1.6 | same |
| Unpaid home-based FT / PT | 0.7 / 0.1 | 0.5 / 0.2 | same |
| Other unpaid FT / PT | 9.2 / 3.8 | 11.1 / 5.4 | same |
| Parent only | 50.5 | 45.4 | same |
| NA (unclassifiable) | 4.0 | 1.2 | same |
| Sample (children 0–4) | 6,522 | 4,270 | |

Findings:
- **Low/high center cutoff is hard-coded at $4.75/hr** (line ~636). Borowsky et al. (2022, NBER w30140) define
  it as the median hourly expenditure on the youngest child in the 2019 NSECE (fees / hours, paid providers only).
  `05_price_cutoff.py` tries the plausible readings on 2019; the one that reproduces it is **paid center care,
  youngest child under 5, child-weighted: $4.73 in 2019**. Other readings miss: all paid care gives $4.25, and
  household weights give $4.62. "Regular only" changes nothing, because cost is only asked for regular providers.
  The same recipe on 2024 gives **$6.86**. Recommendation: compute the cutoff with this recipe in `1a` rather than
  hard-code it. At $6.86 the 2024 split is 9.0% high / 9.2% low-priced (2019 at $4.75: 7.0% / 6.6%); keeping
  $4.75 would give 11.6% / 6.6%.
- The supply side splits by **age-bin** medians (2024: $7.00 / $5.94 / $5.25 for 0–1 / 2–3 / 4–5 vs 2019 $5.00 /
  $4.44 / $4.00), while the household side uses one cutoff. That inconsistency predates 2024.
- **K-8 is 0% of under-5s in both years**, so transitional kindergarten is not leaking into the K-8 type.
- `TYPEOFCARE_AGG` = 8 (setting unknown) rose from 0.5% to 2.2% of regular arrangements; these land in Other.
  Type 5 (other organizational ECE) fell from 8.7% to 5.5%.
- Pre-existing, both years: a center arrangement with DK payment (−1) is treated as **unpaid** (cost becomes
  NA, `is_paid` is FALSE) and lands in Unpaid Center-Based. About 0.39M (2019) and 0.29M (2024) weighted
  child-arrangements (all ages, regular and irregular). Decided: treat as missing (see *Decisions*). The table
  above still uses today's rule.
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
  1. **Use the 2023 ACS** (rejected: the 2024 NSECE pairs with the 2024 ACS; 1b stops until a 2024 extract exists).
  2. **Wait** for Census and IPUMS.
  3. **Build 2024 tax units ourselves.** `Budget-Lab-Yale/Affordability-Index` has an ACS tax-unit builder
     (`R/02_income/02_tax_units.R`, pointer method with qualifying-child/relative rules; a 2024 ACS spine
     exists, 2024 units not yet built). AGI would then need a real tax calculation on those units (that repo
     gets it from Tax-Simulator through its donor match and IRS alignment), not a gross-income proxy, and
     should be validated on 2023 against `ADJGINC` and SOI. SPM poverty output would be off until Census publishes.
- **Not a source:** `model_data/Tax-Data-Bridge/.../tax_units_2024.parquet` is the aged PUF (the same 220,896
  records every year), not ACS persons, so it can't supply ACS tax-unit IDs. It's the donor pool the model
  already matches to.

## Decisions and findings (2026-10-27)

Evidence comes from `03_dk_costs_and_calendar_gaps.py`, `04_admin_centers.py`, `05_price_cutoff.py`, and `06_supply_checks.py`, run on Bouchet against both survey years, and from John's memos in `docs/nsece_2024/`.

### Branch

The work sits on `feat/nsece-2024`, John's branch, because it is further along than the first crosswalk draft. Three of the first five commits matched changes he already had. The `fix/nsece-primary-caregiver` and `fix/point-in-time-employment` commits are inside his branch.

### Base year: 2024 ACS

John's branch pairs the 2024 NSECE with the 2023 ACS and converts dollars to 2023. Anna decided not to use the 2023 ACS. `NSECE_ACS_YEAR_PAIRS` now maps 2024 to 2024, so the dollar conversion factors are 1. `1b_acs_processing.R` stops with a message until an ACS extract with `YEAR == 2024` is on disk. The conversion code in `get_nsece_dollar_conversion.R` stays for reuse. Where 2024 tax units, AGI, and SPM variables come from is still open (see *Base year → 2024: ACS*).

### Don't-know cost → missing, for every care type

The status helper in `1a` labels −1 as MISS, which sets the price to NA. The `is_paid` test then turned NA into "unpaid", so a don't-know provider of any type landed in an unpaid category. John's branch keeps that behavior. `is_paid` is now `paid_calc == 1`, so it stays NA: the arrangement gets no category, and the child is classified by its other arrangements or is NA. The change applies to both years. The effect on care shares is not measured yet. Regular center arrangements, children under 5:

| | 2019 DK | 2019 free | 2019 paid | 2024 DK | 2024 free | 2024 paid |
|---|---|---|---|---|---|---|
| n (weighted) | 83 (0.30M) | 399 (1.17M) | 664 (2.73M) | 56 (0.25M) | 296 (1.10M) | 616 (3.46M) |
| Head Start or public pre-K | 7% | 56% | 12% | 1% | 32% | 7% |
| Median HH income | $62.5k | $42k | $100k | $103k | $48k | $150k |
| Subsidy item also DK | 50% | 1% | 0% | 37% | 0% | 1% |

The don't-know group looks unlike the free group (higher income, little public pre-K), so "unpaid" is the wrong label for it.

### Low/high center price cutoff

The cutoff is the within-year weighted median hourly out-of-pocket price of paid center care for the youngest child under 5 (child weights, positive fees only). The recipe gives $4.73 on 2019, which matches the published $4.75, so the model keeps $4.75 for 2019. It gives $6.86 on 2024. John's branch grows $4.75 by the CPI for day care to $5.76. His own memo calls that index a poor guide, and his within-year median is $6.68. `get_nsece_year_settings()` now holds $6.86 for 2024.

### Posted-rate codes

A posted rate of −9 ("no rate or families don't pay") would read as a price of −$0.23 per hour if a center with that code were not already flagged free or no-rate. `1a` now sets any negative rate to MISS. `06_supply_checks.py` finds no such center in the 2019 or 2024 public files, so the guard changes no current result.

### Home-based labor totals

The home-based sums of enrollment and workers had no provider weights, in both years. Home-based care came to under 1% of the labor stock `L_0`, against 7–16% when weighted. The sums now use `hb_weight`. This changes 2019 results too.

### Care-choice forest

The old recipe drew 50,000 records with replacement in proportion to the weight. With 1,000–2,000 families per group, each family appeared 25–50 times, so the forest memorized families. Its held-out log loss was 2.66 in 2019 and 2.94 in 2024, worse than a forecast with no family features (1.55 and 1.62). The new recipe uses the original records with child weights as case weights, leaf size 50, square-root `mtry`, and 63% of records drawn without replacement per tree. It scores 1.35 and 1.42, the best of nine settings tried (`docs/nsece_2024/care_choice_shift.md`). This changes 2019 results too.

### Administrative-data centers

Facts:
- Administrative-data centers (`CB9_METH_DATA_SOURCE = 2`) are public-school programs built from administrative records: 483 in 2019 and 1,141 in 2024.
- They carry no staffing, wage, education, or classroom data, and their workforce records carry none either.
- In 2019 the public file gives their enrollment by age. In 2024 it gives only total enrollment under age 6 and whether the program runs full-time or part-time.
- In 2019 they were 126K of 1.58M Unpaid Center-Based FTE enrollment (8.0%), and under 0.5% of Low-Priced and High-Priced. The 2024 unpaid-sector share is 19%.
- 75% or more report that no family pays, so they do not move the price medians.

How the model uses center enrollment:
- The sector labor ratio is weighted worker FTE over weighted center FTE enrollment (`center_inputs`).
- `hours_by_sector` is FTE enrollment times care hours, and `L_0` is the sum over sectors of the ratio times those hours. For centers, `L_0` equals the weighted worker total, so it does not change with the admin choice.
- The simulation computes labor demand as `L_req` times the households' simulated hours `Qd`, and sets the wage to `w_0 × (L / L_0)^(1/e)` (`3c_run_scenario.R`). The households' Unpaid Center hours include the children in these programs.
- The first crosswalk draft said supply enrollment levels are not used downstream. That was wrong. The levels set `L_0`.

Options:
- John's branch keeps them and splits total enrollment across ages using interviewed public-school centers where no family pays. Their workers are still missing, so the unpaid-sector ratio is too low (by 8% in 2019 and 19% in 2024). Baseline `L` and `L_0` stay consistent, but labor demand and the employer-subsidy cost are too low for any policy that expands unpaid care.
- The current code drops them from the center file in both years. The ratio is right for interviewed centers. `L_0` stays at the full worker count.
- A third option was considered: take the ratios from interviewed centers, and build `hours_by_sector` from all centers including admin ones, so that `L_0` rises by the admin centers' implied staff. The measurements below show it moves `L_0` further from labor demand, so it is not recommended.

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

- The 2024 admin centers where families pay have no price in `1a`, so they fall in no sector under every option. Their 134,898 children are about 3.5% of Low and High FTE enrollment combined. Leaving them out matches 2019, where the same group holds 12,700 children.
- Staff per FTE child, Unpaid sector, `1a` as written against admin dropped from both sides: 2019 no-BA 0.0517 against 0.0562, BA 0.0565 against 0.0614. 2024 no-BA 0.0860 against 0.1060, BA 0.0631 against 0.0777. With the 2024 imputation kept, the 2024 Unpaid ratio looks far higher than 2019 for a reason that has nothing to do with staffing. With admin dropped, the 2019 to 2024 rise is real.
- Admin enrollment times the interviewed-center ratio comes to about 1.2% of no-BA and 3.0% of BA `L_0` in 2019, and about 3.4% and 6.2% in 2024.
- Labor demanded at baseline is already well below `L_0`, so the households' hours do not match NSECE enrollment. John's national 2019 runs (`docs/nsece_2024/supply_side.md`, section 3.2) give labor demanded at calibration hours of 91% (no-BA) and 78% (BA) of `L_0`. For centers alone it is 72%. In the Unpaid sector, no-BA staff hours are 0.16 billion in `L_0` against 0.12 billion demanded. ACS center hours are 6.93 billion, against 9.74 billion provider-side hours in `L_0`.
- So dropping admin centers raises unpaid-sector `L_req` and moves `L` toward `L_0`. The third option would raise `L_0` and widen the gap.
- `L_0` sets only the first-year wage level. The wage response to a policy depends on the percentage change in labor demanded, which depends on `L_req` and the sector mix of hours (`supply_side.md`, section 3.3). For that response the interviewed-center ratio is the correct one.
- Decision: keep the current code (admin centers dropped in both years). The gap between `L` and `L_0` belongs to a separate choice: what `L_0` is anchored to (`supply_side.md`, Choice 6, where one option sets `L_0` equal to labor demanded at calibration hours).

### Negative non-labor cost for low-priced centers in 2024

The 2024 residual is −$0.47 per child-hour because labor cost uses one wage pair for all centers ($16.80 no-BA, $24.22 BA), while Low-priced centers pay less (sector wages $14.26 and $20.40, from `06`). With sector-specific wages the residual is +$0.26. The Unpaid sector pays the most ($20.73 and $30.39). The same pattern holds in 2019, where the residual is +$0.48 with one wage pair and +$0.94 with sector wages. Neither admin treatment changes the Low residual. The choice is whether to use sector wages in both years; that changes 2019 results.

Sector wage cells (teachers and assistants with known education and a wage, admin centers dropped; unweighted counts of workers, weighted mean hourly wages):

| | 2019 no BA | 2019 BA | 2024 no BA | 2024 BA |
|---|---|---|---|---|
| Unpaid | 418, $13.82 | 390, $26.23 | 642, $20.73 | 544, $30.37 |
| Low | 892, $10.35 | 280, $14.91 | 1,279, $14.25 | 418, $20.40 |
| High | 894, $12.90 | 365, $15.47 | 1,314, $17.62 | 639, $22.06 |

Every cell has at least 280 workers. About 1,000 workers per year have a wage but no sector (no classroom age bin or no usable center price). They enter today's all-center wage but would leave the sector wages. John's option (e5) in `supply_side.md` restates staffing in average-wage hours so `L_0` keeps one unit; it gives +$0.26 for 2024.

### Calendar cases coded −10

The first crosswalk draft said these children drop. That was wrong. Only the parents' hours records drop, and those parents are then coded as not working. That understates 2024 parent employment by about 3.5 to 5 points. John's optional `hours` treatment fixes it; it is not turned on yet. Only 73% of 2024 households did the weekly calendar, against 94% in 2019, and from June 2024 most got a typical-week question. The wording change alone lowers parent-only care by about 3.4 points. The 2024 survey also puts 64.3% of young children with a working mother, against 59.6% in the CPS. See `docs/nsece_2024/calendar_and_work_hours.md` and `employment_and_reweighting.md`.

### John's optional 2024 treatments (`--nsece-2024-options`)

All three are off by default. Sizes come from John's runs, summarized in `docs/nsece_2024/decisions.md`:
- `hours` fills in hours for parents with a −10 calendar block from similar workers, as the 2019 pipeline does for other gaps. 2024 employment of two-parent primary caregivers goes from 58.6% to 62.2%, single parents from 64.5% to 68.0%, and partners from 84.9% to 89.8%. Applying the same fix to 2019 moves 2019 employment by 0.4 to 0.7 points.
- `weights` reweights 2024 child weights to the March 2024 CPS on mothers' employment by family type. It shrinks the 2019 to 2024 change in parent-only care from −6.7 points to between −2.2 and −3.5, and in any paid care from +4.2 to between +0.1 and +1.3. NSECE mothers' employment is 53.6% in 2019 and 64.3% in 2024, against 57.1% and 59.6% in the CPS.
- `supply` trims 2024 posted rates at the 5th and 95th percentiles as the 2019 file did, and measures center staffing from the director's staff count. With admin centers dropped, the trim moves the 2024 Low-priced residual from −$0.47 to −$0.27 (`06`).
- The calendar wording change (typical-week households) has no option. Every treatment John tried moves 2024 parent-only care by 1.2 points or less, except counting usual providers in both years, which also moves 2019 (52.6% to 47.9%).

### Other findings, not yet resolved

- Low-priced centers' non-labor cost per child-hour is negative in 2024 (−$0.47). One wage pair for all sectors causes it (see *Negative non-labor cost for low-priced centers in 2024*).
- The 2019 public file censors posted rates by deleting values outside the 5th–95th percentiles. The 2024 file caps them at those values instead. About 40 impossible rates above $50 per hour remain in the 2019 data.
- Several of John's memos quote $5.76 and the 2023 ACS as the setting of his study runs. They are a record of those runs and do not describe the current code.

### Open items

1. What `L_0` is anchored to (provider-side staffing, as now, or labor demanded at calibration hours). This replaces the admin-center open item.
2. Sector-specific wages for the center labor cost, in both years, to remove the negative Low-priced residual in 2024.
3. 2024 ACS tax units, AGI, and SPM. A 2024 IPUMS extract without them is on Bouchet (`raw_data/ACS/acs_common_v2/us2024a`, 1-year, and `us2024c`, 5-year). It has every other variable `1b` reads, including `EMPSTATD`. IPUMS rejects `TAXID`, `ADJGINC`, and the six SPM variables for `us2024a` (request of 2026-10-27). These come from the Census SPM file, which IPUMS expected in the first half of 2026; it is not out yet. `raw_data/Census-ACS-SPM/v1` holds 2019 only.
4. Decide whether to turn on John's `hours`, `weights`, and `supply` options.
5. Run a 1% smoke test on 2019 and then 2024 (no `-d` or `-c`), and a national baseline to compare `L` with `L_0`.
