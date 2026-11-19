# NSECE 2019 → 2024 crosswalk for `1a_nsece_processing.R`

Working notes for moving the ECEC-Simulator from the 2019 to the 2024 NSECE. The work sits on John Ricco's branch `feat/nsece-2024`, with no branch of its own.
Everything here was checked on Bouchet against the raw public-use files and the 2024 User's Guides
(ICPSR 39466 v1, released 2026-06-11). Raw data: `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1/{2019,2024}`.
The checks that produced the numbers are in [`src/misc/nsece_2024_checks/`](../src/misc/nsece_2024_checks/).
`02`–`06` re-implement parts of `1a` in Python, so the R pipeline should reproduce them.

## Status (2026-11-17)

- John's branch holds the code changes listed below: a `1a` that reads both years, the `--nsece-year` switch, the optional 2024 treatments in `docs/nsece_2024/`, and the primary-caregiver and point-in-time commits. The `fix/nsece-primary-caregiver` and `fix/point-in-time-employment` commits are inside it.
- This file, the checks, and the decisions below sit on top of his commits.
- `1a` runs on 2024. `1b` runs on 2024 only when a tax-unit crosswalk sits beside the ACS file, because IPUMS has not released the 2024 `TAXID` and `ADJGINC` (Open item 3). The 2024 1% smoke test passed on the Affordability-Index crosswalk (job 32703540).
- Decided: sector wages (Open item 2, one wage pair kept, 2026-11-16, `docs/nsece_2024/decisions.md` decision 7), the 2024 tax units and AGI (Open item 3, 2026-11-09), censored rates (mismatch item 4, 2026-11-06), administrative-data centers (2026-11-12), and the refit of NORC's imputed staff counts (mismatch item 7, 2026-11-17).
- Open: what `L_0` is anchored to (Open item 1), whether 2019 also switches to the Affordability-Index tax units (Open item 3), and John's `weights` and `supply` options (Open item 4).

## Supply-side terms

Sectors. The supply side has four sectors, which are kinds of provider the model prices on their own: Unpaid Center-Based (families pay nothing), Low-Priced Center-Based, High-Priced Center-Based, and Paid Home-Based. Low and High split at the median posted price of the age group, weighted by FTE enrollment. Each sector has a price, a staff count per child-hour, and a non-labor residual, which is price minus labor cost. Parent care, other paid care, and other unpaid care are not supply-side sectors.

`L_0`. `L_0` is the care workforce at the base year, by staff type (no BA, BA). `1a` builds it from the provider side:
- `L_0[type]` is the sum over the four sectors of `L_req[sector, type]` times `child-hours[sector]`.
- `L_req[sector, type]` is staff of that type per child-hour. For centers it is the workforce survey's classroom staff, divided by the sector's FTE enrollment. For the home-based sector it is the provider plus helpers of listed providers in the home-based file, divided by their enrollment.
- `child-hours[sector]` is the sector's FTE enrollment times 40 hours a week times 50 weeks.

The simulation sets care wages from `w = w_0 × wage growth × (L / L_0)^(1/e)`, where `L` is the labor that simulated care choices need (`3c_run_scenario.R`) and `e` is 4 for no BA and 2 for BA. A policy that raises demand above `L_0` raises wages.

## The main choices

Each paragraph says what the original model (2019, as on `main`) does, what the code does now, and where the detail is.

`L_0` (Open item 1, open). The choice is whether `L_0` stays provider-side or becomes the labor that households' care choices imply at the calibration hours. The two differ, so the choice moves the starting point of the wage curve.

Sector wages (Open item 2, decided 2026-11-16). The original model uses one wage pair for every sector, one wage for staff without a BA and one for staff with a BA, from the workforce survey. Low-priced centers pay less than the average, so in 2024 their labor cost comes out too high and the residual is negative (−$0.47 per child-hour). 2019 does not show this (+$0.47). Sector wages would make the 2024 Low-priced residual +$0.26 and move the 2019 one from +$0.47 to +$0.95. The model keeps one wage pair and accepts the negative residual (*Negative non-labor cost for low-priced centers in 2024*).

Reweighting (Open item 4, open). The original model uses the NSECE weights as given. John's `weights` option rakes the 2024 child weights to an outside benchmark of mothers' employment by family type. The NSECE is off from the CPS in opposite directions in the two years: 53.6% of mothers working in 2019 against 57.1%, and 64.3% in 2024 against 59.6%. Reweighting only 2024 would treat the years differently. The consistent choices are to reweight both years or neither. The 2024 targets can be the March 2024 CPS or the 2024 ACS.

Censored rates (mismatch item 4, decided 2026-11-06). NORC censors extreme posted rates. The 2019 file deletes them, and the 2024 file sets them to the cap. `1a` fills each censored rate in both years with the lognormal mean beyond its cut, so the cell keeps its sector.

Administrative-data centers (decided 2026-11-12). `1a` keeps the free admin centers in the Unpaid sector and the paying ones in the Low- and High-priced sectors, each with its own staff count (*Administrative-data centers*). The 2024 staff counts that NORC imputed are refit with a random forest (mismatch item 7).

The `supply` option (open). John Ricco added it on 2026-10-17. It did two things: it deleted the 2024 censoring caps, and it measures center staffing from the director's staff count. The original model gets the first from the 2019 file and does not do the second (it uses the workforce file). The cap deletion is gone since 2026-11-06. The director staffing is still an option (*John's optional 2024 treatments*).

## Files

| Survey | 2019 (ICPSR 37941) | 2024 (ICPSR 39466) | 2024 rows × vars |
|---|---|---|---|
| Household | `37941-0009-Data.tsv` | `39466-0005-Data.tsv` | 6,403 × 6,516 |
| Household calendar | (in household file) | `39466-0004-Data.tsv` | 6,403 × 16,129 (read only for the parent calendar hours, see mismatch item 3) |
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
| Household members (`HHM_AGE_X`, `D1A_WORK_X`, `D3D_*_X`, `PARCH_X_Y` member index) | 1–9 | 1–13 | regex `[1-9]` and `str_sub(-1)` → members 10–13 dropped or mis-indexed |
| Providers (`*_X_Y` arrangement vars, `PRIORREL_Y`, `HRS_WEEK_PROVY_X`) | 1–14 | 1–15 | regex `(?:[1-9]\|1[0-4])` → provider 15 dropped |
| Children | 1–9 | 1–9 | no change |

### Respondent gender
2019 `HH9_RGENDER`: 1=Male, 2=Female, 3=DK/Ref, −4=valid skip.
2024 splits into dummies `HH4_RGENDER_1` (Male, 0/1) and `HH4_RGENDER_2` (Female, 0/1). Other and DK are restricted-use only.
Rule: `resp_male = 1 if RGENDER_1==1; 0 if RGENDER_2==1; NA otherwise` (the 2024 dummies are converted in `harmonize_nsece_2024_household()`).

### Child age reference date
`HHC9_AGE_AT_SEP1_18_X` → `HHC4_AGE_AT_SEP1_23_X` (age in months on Sept 1, 2023). The regex already generalizes. −3 = child not born yet, already set to NA.

### Center-based price status (most consequential)
| Code | 2019 `B1_3A_AGERATE_X_R` | 2024 `B1_3A_AGERATE_X_R` |
|---|---|---|
| −8 | No families pay (→ code labels FREE) | CB admin case |
| −3 | — | Valid skip |
| −1 | — | DK/Ref |
| 1 / 2 | Yes / No rate | Yes / No rate |
| 3 | DK/Ref | — |

Crosstab against `CB4_B1_3_FAMPAY` (2YR shown): −3 & FAMPAY=2 → 1,407 (= free), −3 & FAMPAY=−1 → 103 (missing), −8 → 1,141 admin centers.
New FREE rule: `AGERATE == -3 & B1_3_FAMPAY == 2`. `1a` sets admin cases aside before the price step (see *Administrative-data centers*).

### Center-based enrollment and staffing
- `ENRL_*` now carries −8 (admin, 1,141) and −1 (DK). Summed directly, these give negative enrollment. `harmonize_nsece_2024_centers()` sets enrollment by age to 0 for both codes before the sum.
- `ENRL_FT_X`: −2 = doesn't serve age, −4 = not enough info (new), −8 = admin. The existing `< 0` handling covers it.
- `STDRATE_WKLY_X`: −9 = no rate or families don't pay, −8 = admin, −1 = not enough info. −2 (the 2019 MISS code) no longer exists, and −1 still maps to MISS.
- `F3_*_TC`, `F3_YOUNGMONTHS_R`: −8 admin, −1 DK. Same pattern as 2019.
- `F15_CHILD_FUND_HS_IND` / `F16_CHILD_FUND_PK_IND`: new codes −3 valid skip, −5/−6/−7 "DK but ≥75% / >50% / <50%". Only 774 HS and 630 PK valid 0/1 vs 1,024 / 873 in 2019.

### Home-based helpers
| | 2019 | 2024 |
|---|---|---|
| `H2_NUMHELP` no helpers | −2 (code sets to 0) | −3 valid skip |
| `STAFF_EDUC_PRCNT_4YR` no helpers | −2 | −3 |
| `STAFF_EDUC_PRCNT_4YR` not enough info | — | −5 (new) |

Without a recode, `n_workers = 1 + (−3)` would be wrong for every no-helper provider, and the percent filter would drop them. `harmonize_nsece_2024_home()` maps NUMHELP −3 to the 2019 no-helper code (−2, which `1a` sets to 0) and −1 to the 2019 don't-know code (−4). It maps PRCNT −3 to −2 and −5 to the dropped code −3.

### Workforce wage
New codes −3 (not paid: volunteer/owner, 16) and −5 (not enough info, 34). They fall through to `ADMIN`. `recode_missing_admin_to_na()` sets both `ADMIN` and `MISS` to NA, and no later step reads `wage_status`, so these 50 workers leave the wage averages either way. No change is needed. −8 = admin-spawned (1,141) is unchanged in meaning.

Definitions (2024 workforce questionnaire, section B, and user guide pp. WF-95 to WF-109). The 2019 wage has the same construction:

| Variable | Item | Construction | What it leaves out |
|---|---|---|---|
| `WORK_WAGE` | B4_M: pay "before taxes and deductions" and its unit (hour, day, week, 2 weeks, month, year, other). 2024 adds the B4a confirmation | Converted to hourly with B1 hours per week, the center's days open per week, and its weeks open per year. Imputed when rate or unit is missing (`WORK_WAGE_IMP`). Top-coded: above the lowest cutoff of the worker's urbanicity, auspice, and education cells, set to the median of the top-coded pool | Benefits, employer payroll tax, extra payments |
| `B12_OTHPAY` (2024 only) | "In addition to the regular payments you earn from this center, have you received any other payments for early care and education workers in the past 12 months?" Bonus, special service pay, performance award, training incentives, other money outside regular pay | Yes 2,366, no 3,859 (unweighted) | Source of the money: not limited to the center |
| `B13_OTHPAY_AMT` (2024 only) | Total of those payments in the last 12 months, $0--$250,000 | Median $1,000, mean $1,948 (unweighted, n 2,083) | A 12-month total, not hourly |
| `B11_WORK_MONTH_ECE` | Months of the last 12 worked "at this or another" ECE program | Mean 10.2 | Not limited to this center |
| `WORK_HRS_CAT`, `WORK_FT` | B1 usual hours per week at this program | Six ranges in the public file (exact hours restricted). FT is 35 hours or more | |
| `WORK_ROLE` | Role reported by the center respondent, not the worker | 1 aide or assistant, 2 teacher or lead teacher, 3 other | `1a` keeps 1 and 2 |
| `CHAR_EDUC` | Highest level completed | 1 less than high school, 3 GED, 4 high school, 5 some college, 6 associate, 7 BA, 8 graduate (no code 2) | `1a`: BA = 7, 8 |

`1a` sets the wage pair to the worker-weighted mean of `WORK_WAGE` for roles 1--2 by BA. Points still to settle (2026-11-09):
- The mean is per worker, not per hour worked, so a part-time aide counts as much as a full-time teacher. Labor cost per child-hour argues for weighting by hours (worker weight × the FT factor used for staffing).
- Extra pay can include public workforce money paid to the worker directly (state wage supplements, retention bonuses), which is not a provider cost. It belongs in worker compensation (labor supply) more clearly than in provider labor cost (the residual), and the survey cannot split the two.
- Top-coding replaces the highest wages with a pool median, which lowers the BA mean most.

### Household labor
- `D1A_WORK_X`: 3=DK in 2019 → −1 in 2024. −3 valid skip and −33 invalid skip are new. The code keeps only 1/2, so no change is needed beyond the loop size.
- `D3D_UNIT_X`: 2019 blanks → 2024 −3 / −1. Already mapped to NA. 7=Other already excluded.
- `WST_PARCH_HI/LO_X` and `WST_HRS_TOT_*`: −10 (a parent has a missing calendar block) rose from 11 / 1 / 5 child loops (HRS_TOT_PHI / HRS_TOT_PLO / PARCH_HI) in 2019 to 412 / 417 / 363 in 2024. The code fills these parents' hours (mismatch item 3), and the comment there gives these counts.

## Unchanged (verified codes match)
`METH_CASEID`, `METH_WEIGHT`, `HHCOMP_MEMBERS`, `HHCOMP_NUMMOTHER/NUMFATHER`, `REGION` (1–4), `RSPOUSEPARTNER_LOOP` (−1 / −2 / HHM index), `HHC_GENDER` (1/2/−1), `PARCH` (adds −2 not-asked, code keeps ==1), `TYPEOFCARE_AGG` (1–8), `CARELOC`, `REGCARE`, `HEADSTART`, `PUBPREK`, `CHILDREN`, `PRIORREL` (2019 code 2 gone), `SUBSIDY`, `WEEKLY_PAYMENT_TC` (0 = free), `HRS_WEEK_PROVY_X` (same provider-then-child order), `HB` `METH_SAMPLE_SOURCE` (1 unlisted / 2 listed), `PRGM_PAID`, `CAREER_EDUC`, `ENRL_TOTAL`, `WF` `WORK_FT`, `WORK_ROLE`, `CHAR_EDUC`, `WORK_WAGE_IMP`, `CB` `WEIGHT_CLSM`.

## Care types (the 15 options)

The household classification (`1a`, provider-level `ecec_choice`, then primary care per child) uses only
`TYPEOFCARE_AGG` (1–3 individual, 4 center/HS/pre-K, 5 other org., 6 K-8, 7 irregular, 8 unknown),
`CARELOC`, `PRIORREL`, `REGCARE`, `WEEKLY_PAYMENT_TC`, and `HRS_WEEK_PROV`. All six have the same
categories and reserve codes in 2024, so the type logic carries over unchanged once loop sizes are fixed.

Primary care of children under 5 in each year, from the `1a` of 2026-10-29 (child weights, the low/high cutoff is $4.75 in 2019 and $6.86 in 2024, runs `19_new` and `24_new`):

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
- The supply side splits by age-bin medians (2024: $7.00 / $5.94 / $5.25 for 0–1 / 2–3 / 4–5 vs 2019 $5.00 / $4.44 / $4.00), while the household side uses one cutoff. That inconsistency predates 2024.
- K-8 is 0% of under-5s in both years, so transitional kindergarten does not fall into the K-8 type.
- `TYPEOFCARE_AGG` = 8 (setting unknown) rose from 0.5% to 2.2% of regular arrangements. These land in Other. Type 5 (other organizational ECE) fell from 8.7% to 5.5%.
- A center arrangement with DK payment (−1) is now missing, not unpaid (see *Don't-know cost → missing*). Those children get no category, the last row of the table. Before 2026-10-28, in both years, it landed in Unpaid Center-Based (cost NA, `is_paid` FALSE): about 0.39M (2019) and 0.29M (2024) weighted child-arrangements (all ages, regular and irregular).
- Thin cells in 2024: Unpaid home-based PT n=10, FT n=24, Paid home-based PT n=26. The ACS imputation forest and the two-child joint outcome (225 cells) will be sparser than in 2019 with 35% fewer children.

### 2019 care-type recode (C17)

Part of the fall in type 5 is a coding change. A new 2024 item (C17) asks what kind of single activity a provider offers, and NORC used it to code some arrangements as center care (type 4) or K-8 school (type 6) that the 2019 survey would have coded as type 5. The 2019 file has no C17, so no 2019 record can be recoded exactly. OPRE raises its 2019 estimates of any center use by 7.2% for children under 3 and 8.7% for children 3 to 5 (2024 household book of tables, Appendix B).

Current rule (since later on 2026-11-16, `recode_2019_center_coding()` and `fixed_total_draw()` in `1a`): in each age band, eligible children have a regular type-5 arrangement and no regular type-4 arrangement. The code splits the band's target weight across four groups of weekly type-5 hours (under 10, 10–19, 20–34, 35 or more) by the hours mix of the 2024 children whose arrangements C17 could have moved (17.4%, 12.9%, 30.0%, and 39.7%). In each group it takes eligible children in random order until the drawn weight reaches the group's target, and recodes their regular type-5 arrangements to type 4. The draws use their own seed (76) and leave the global random-number state unchanged.
- Each draw hits OPRE's factor, not only on average: over 500 draws, the factor under 3 runs from 1.063 to 1.080 (5th to 95th percentile), compared with 1.050 to 1.097 with a chance per child.
- The recoded children average 28.7 hours a week, 69.8% at 20 hours or more, and 57.9% of them pay for care, compared with 64.2% of the 2024 children.
- The 2024 file implies factors of 1.075 and 1.086 from those arrangements, so OPRE's factors are an upper bound (cluster `agr55/nsece_evidence/mtg1113/`, `checks_1116.R`, `checks_1116b.R`, `checks_1116c.R`).

Rules compared. Of five draw rules (uniform, proportional to hours, hours groups, hours groups and payment, 20 hours or more only), hours groups came closest: 28.7 hours a week and 69.7% at 20 hours or more, against 28.5 and 69.7% for the 2024 children. The uniform draw gave 23.3 and 49.7%. A logit fit on 2024 children matched payment (68%–71%) but not hours (25.4–25.6 hours a week) (cluster `agr55/nsece_evidence/rf1116/c17v.R`).

History. 2026-11-12: uniform draw with a chance set so that the expected weighted share with any center use rises by OPRE's factor. Earlier on 2026-11-16: the chance per child depended on the hours group.

## Related work: the Stanford Childcare Model (white paper, October 2026)

Brown, Gibbs, Mahoney and Ninan's model is already estimated on the 2024 NSECE. Choices relevant here:
- ACS. Household income comes from the 2023 ACS (in 2024 dollars, CPI-U), and child poverty uses SPM thresholds from the 2023 ACS SPM research file. They meet the same 2024 ACS and SPM gap by using 2023.
- DK costs. Informal-care arrangements with DK/refused cost are excluded, and $0 is kept as a real price. Same as our decision.
- Sample. It drops households whose mother's work hours can't be determined (133) or with more than two resident parents (88), and keeps the two youngest children 0–4 (or 5, not yet in kindergarten).
- Classification. Six types (public center, private center, home-based, informal relative, informal non-relative, maternal care), and the primary arrangement is the one with the most hours, not Borowsky's paid-first priority. No low/high price split. School, irregular, and unknown arrangements are not regular care.
- Supply. Listed home-based providers only (the 2024 home-based guide advises against pooling listed and unlisted). Center staffing ratios come from the sampled classroom, FTE-adjusted.
- Demographics (white paper section 2, footnote 5). A nested logit with preference shifters for Hispanic ethnicity, mother's college education, single parent, and child age, on the employment nest, care type, and care intensity. Household income comes from ACS cell means by mother's age, ethnicity, partner status, and education. No other race groups, and none on the worker side.

## Base year → 2024: ACS

The 2024 NSECE is ready to use. The gap is on the ACS side. Current state: Open item 3.

- On Bouchet, `ACS: '/raw_data/ACS/v1/2023'` in `config/default_paths.yaml` holds a 2019 IPUMS extract (`YEAR = 2019`). The lab's shared 2024 extracts (`raw_data/ACS/acs_common_v2/us2024a`, `us2024c`, fixed-width + DDI, not the `ipums_usa.csv` the code reads) have every person variable `1b` uses except `TAXID`, `ADJGINC`, `SPMFAMUNIT`, `SPMTHRESH`, `SPMTOTRES`, `SPMCHXPNS`, `SPMWKXPNS`, and `SPMCAPXPNS`. The same shared extracts carry all eight for 2009–2019 and 2021–2023.
- IPUMS takes them from Census's ACS SPM research file, released separately from the ACS microdata. A direct IPUMS API request for them on `us2024a` was rejected ("not available in any of the samples currently selected"). Census's ACS SPM research-file page lists 2009–2019 and 2021–2023 (page dated April 2025), with no 2024 file. IPUMS said in December 2025 that it expected the 2024 SPM variables in the first half of 2026. As of 2026-10-27 they are not out and there is no date. ([Census ACS SPM research files](https://www.census.gov/data/datasets/time-series/demo/supplemental-poverty-measure/acs-research-files.html), [IPUMS forum](https://forum.ipums.org/t/spm-variables-in-2024-acs/6898))
- Each variable feeds a different step.
  - `TAXID`: the tax units that the Tax-Simulator donor match and taxes are computed on. (`main` also replaces it with a corrected Census crosswalk built from `raw_data/Census-ACS-SPM/v1/2019`, which exists only for 2019.)
  - `ADJGINC`: baseline AGI and the AGI percentile used in the donor match.
  - `SPM*`: only the SPM poverty-impact output. `1b` already skips that output when the variables are absent. It stays off until Census publishes the 2024 SPM file.
- Options considered on 2026-10-27: (1) use the 2023 ACS, which keeps the IPUMS `TAXID` and `ADJGINC` but mixes 2023 incomes and demographics with the 2024 survey (2023 tax units cannot be attached to 2024 persons, because `TAXID` links persons inside one ACS sample), (2) wait for Census and IPUMS, or (3) build 2024 tax units ourselves. Option 3 was chosen (Open item 3).
- `Budget-Lab-Yale/Affordability-Index` (private repo) has an ACS tax-unit builder (`R/02_income/02_tax_units.R`, pointer method with qualifying-child and qualifying-relative rules). It builds units from the IPUMS household pointers (`SPLOC`, `MOMLOC`, `POPLOC`) and uses `TAXID` only as a benchmark, so it does not need the missing 2024 `TAXID`. Tax-Simulator cannot do this step: it computes tax on its own PUF-based tax units and has no ACS input.
  - Units, built 2026-10-29 (`ai-private` on Bouchet, jobs 31284243 for 2024 and 31284244 for 2023, spine and tax-unit stages only). 2024: 3,238,311 persons (unweighted) form 1,758,184 units. No unit spans two households, and every unit has one primary filer. 2023, the same code against IPUMS `TAXID`: 3,228,659 persons, and the partitions agree exactly for 88.9% of persons. The builder makes 1,750,114 units against 1,796,379 for IPUMS. It splits IPUMS units for 269,068 persons and merges IPUMS units for 127,207. `1b` uses the IPUMS units in 2023.
  - `1b` change, done 2026-10-29. `1b` reads `tax_unit_crosswalk_<year>.csv` beside `ipums_usa.csv` when the file exists (columns `serialno`, `sporder`, `tax_unit`, `agi`, where `tax_unit` is a numeric id unique across households and `agi` is the unit's AGI on every member). It then needs no `TAXID` or `ADJGINC` in the ACS file. Each unit counts as one legacy fragment, so `reconstruct_tax_unit_agi()` returns the unit AGI unchanged. Without the file, a 2024 run stops at the missing `TAXID` and `ADJGINC`, and the message names the file path. Tested on a toy file with both paths (no `TAXID` or `ADJGINC` columns, then IPUMS columns), and on real data on 2026-11-09 (Open item 5).
  - Units and AGI come from the Affordability-Index full income chain (John Iselin's runs). `src/misc/make_tax_unit_crosswalk.R` converts them to the `1b` crosswalk (Open item 3).
- Not a source: `model_data/Tax-Data-Bridge/.../tax_units_2024.parquet` is the aged PUF (the same 220,896 records every year), not ACS persons, so it can't supply ACS tax-unit IDs. It's the donor pool the model already matches to.

## Decisions and findings

Evidence comes from the checks `03` to `10` in `src/misc/nsece_2024_checks/`, run on Bouchet against both survey years, and from John's memos in `docs/nsece_2024/`. The work sits on `feat/nsece-2024`, John's branch, which already held three of the first five code changes planned here.

### Base year: 2024 ACS

The model pairs the 2024 NSECE with the 2024 ACS. John's branch had paired it with the 2023 ACS and converted dollars to 2023. `NSECE_ACS_YEAR_PAIRS` now maps 2024 to 2024, so the dollar conversion factors are 1 (the conversion code in `get_nsece_dollar_conversion.R` stays for reuse). The 2024 ACS person file is on Bouchet at `raw_data/ACS/v1/2024/ipums_usa.csv` (IPUMS extract 36, `us2024a`, 3,422,888 person records, unweighted), named `ACS-2024` in `config/default_paths.yaml`. `1b` reads `ACS-<year>` when that key exists, else `ACS`. The file has no `TAXID` or `ADJGINC` (Open item 3).

### ACS employment: armed forces at work

Decided by Anna on 2026-11-09. `1b` counts a parent as working if at work in the reference week, civilian or armed forces (`EMPSTATD` 10 or 14). Before, armed forces at work (14) counted as not working. They are 1.0% of parents of children under 5 in the 2019, 2023, and 2024 ACS (weighted). The share of these parents at work goes from 74.5% to 75.5% in 2019 and from 75.7% to 76.8% in 2024 (2026-11-09, `newitems1109/af.R` on Bouchet). The NSECE work item, D1a, tells respondents to include work in the military. Parents with a job but not at work (12, 2.2%–2.3%, 15, under 0.01%) stay not working, as in the NSECE, where D1a counts only paid work last week (1.6% of NSECE parents in 2019 and 0.8% in 2024 did no paid work last week but have a job). Students have no ACS employment status: a student with a job is at work, and a parent who only attends school or training gets zero NSECE work hours. The check `11_acs_mother_at_work.R`, which builds the reweighting targets, still uses `EMPSTATD == 10` so that it reproduces the review's figures.

### Don't-know cost → missing, for every care type

`is_paid` is `paid_calc == 1`, so a don't-know cost (−1, which the status helper labels MISS and prices NA) stays NA: the arrangement gets no category, and the child is classified by its other arrangements or is NA. The rule applies to both years. Before, the `is_paid` test turned NA into "unpaid", so a don't-know provider of any type landed in an unpaid category, as on John's branch.

Measured on 2026-10-28 (`1a` with the old and new rule, children under 5, child weights): the change moves 2.3 points of 2019 children into no category (4.03% to 6.31%) and 2.7 points of 2024 children (1.18% to 3.87%). Unpaid Center-Based falls from 7.10% to 5.64% in 2019 and from 6.78% to 5.45% in 2024. Other Unpaid falls 0.9 and 1.1 points, and Unpaid Home-Based 0.4 and 0.3 points. Children with no category are left out of the care-choice forest's training data. Among children with a category, Unpaid Center-Based falls 1.4 points in 2019 (7.40% to 6.02%) and 1.2 points in 2024 (6.86% to 5.67%), and Parent Only rises 1.7 and 1.2 points. Regular center arrangements, children under 5:

| | 2019 DK | 2019 free | 2019 paid | 2024 DK | 2024 free | 2024 paid |
|---|---|---|---|---|---|---|
| n (weighted) | 83 (0.30M) | 399 (1.17M) | 664 (2.73M) | 56 (0.25M) | 296 (1.10M) | 616 (3.46M) |
| Head Start or public pre-K | 7% | 56% | 12% | 1% | 32% | 7% |
| Median HH income | $62.5k | $42k | $100k | $103k | $48k | $150k |
| Subsidy item also DK | 50% | 1% | 0% | 37% | 0% | 1% |

The don't-know group looks unlike the free group (higher income, little public pre-K), so "unpaid" is the wrong label for it.

### Low/high center price cutoff

The model keeps $4.75 for 2019 and uses the within-year median for 2024, $6.86 (`get_nsece_year_settings()`). Borowsky et al. (2022, NBER w30140) define the cutoff as the median hourly expenditure on the youngest child in the 2019 NSECE (fees over hours, paid providers only). `05_price_cutoff.py` tried the plausible readings on 2019. Only paid center care, youngest child under 5, child-weighted, positive fees, reproduces it: $4.73, against the published $4.75. All paid care gives $4.25, household weights give $4.62, and "regular only" changes nothing because cost is only asked for regular providers. At $6.86 the 2024 split is 9.0% High-priced and 9.2% Low-priced of children under 5 (2019 at $4.75: 7.0% and 6.6%). Keeping $4.75 would give 11.6% and 6.6%. John's branch grew $4.75 by the CPI for day care to $5.76. His own memo calls that index a poor guide, and his within-year median is $6.68.

### Posted-rate codes

`1a` sets any negative posted rate to MISS. A rate of −9 ("no rate or families don't pay") would otherwise read as a price of −$0.23 per hour if a center with that code were not already flagged free or no-rate. `06_supply_checks.py` finds no such center in the 2019 or 2024 public files, so the guard changes no current result.

### Home-based labor totals

The home-based sums of enrollment and workers now use the provider weight `hb_weight`, in both years. This changes 2019 results too. `main` reads the provider weight (`hb_weight = HB9_METH_WEIGHT`) but never multiplies by it, and the code has been that way since the initial commit, so the history gives no reason. Unweighted, home-based care came to under 1% of `L_0`, against 7–16% when weighted.

Measured on the 2019 home-based file (listed providers, 3,855 used): weighting multiplies total enrollment, the base for care hours, from 32,097 to 710,414 (22 times). Staff per child moves +1.8% for staff without a BA (0.160 to 0.163) and −12.4% for staff with a BA (0.0381 to 0.0334). The center sectors already used center weights, so home-based was the only sector affected.

### Care-choice forest

Decision, 2026-10-28: I want to preserve the original forest specification. `1b` trains each forest on 50,000 records drawn with replacement in proportion to the child weight, with leaf size 10 and all features at each split, as on `main`. The experimental settings (child weights as case weights, leaf size 50, square-root `mtry`, 63% of records drawn without replacement per tree) are removed. The caregiver-first parent ages and the caregiver rule for NSECE training records stay. They align the training records with the ACS records and are not forest settings. The experiment is recorded in `docs/nsece_2024/care_choice_shift.md`. It is not an approved model change.

Comparison with the restored forest, 2019 (`1b` run `19_oldforest_1b`, whose code is the same as this branch's 2019 path, children under 5, ACS imputed shares with observed employment, against NSECE shares):
- The absolute gaps over the eight care types sum to 16.8 points (10.4 with the experimental forest). Parent Only is 46.4% in the ACS against 54.3% in the NSECE. Other Paid is 11.9% against 8.3%, and Other Unpaid 14.4% against 12.9%.
- Labor demanded at ACS hours is 83.6% (no BA) and 72.4% (BA) of `L_0`, and 64.5% and 62.7% for centers alone (78.6% and 67.5% with the experimental forest).
- No 2024 `1b` comparison exists: `1b` stops on the missing 2024 tax units (Open item 3).

### Administrative-data centers

Current rule. Decision of 2026-10-27, restored on 2026-11-12 and extended to paying centers: include the admin centers in supply with their own staff count, in both years. `1a` does this as follows:
1. It sets the admin centers aside before the price medians, the classroom file, and the workforce ratios. They have no data for any of those.
2. It keeps the admin centers where no family pays (rate flag of −8, the rule for interviewed centers) for the Unpaid sector, and the ones where families pay (a posted-rate flag, or family pay = 1) for the Low and High sectors. Centers whose payment is don't know stay out of every sector, as interviewed ones do.
3. It computes their FTE enrollment under age 6. In 2019 it uses enrollment by age and the full-time share by age. In 2024 it uses total enrollment under 6 and the full-time share from `CB9_ADMIN_FULLTIME_CARE` and `CB9_ADMIN_PARTTIME_CARE` (full time only 100%, part time only 0%, else 50%).
4. It computes staff per FTE child from `CB9_STAFF_TYPE_TOTAL_NUM`, with the 2024 imputed counts refit by a random forest (mismatch item 7). Centers without a valid count get the average of the others in their payment group.
5. It converts the headcount to FTE, and splits it by BA, with the shares of teachers and assistants in interviewed public-school centers with the same payment status (free: 68.5% BA in 2019, 69.6% in 2024, paying: 47.4% and 51.9%). Free admin programs resemble free public-school centers more than any other group.
6. It adds the free centers' staff and FTE enrollment to the Unpaid row, and the paying centers' to the Low and High rows, split by the FTE shares of interviewed paying public-school programs in each (71.9% Low in 2024, 85.1% in 2019), because the 2024 file withholds their price (mismatch item 5). The paying centers add enrollment and staff, not price. The ratios become the pooled ratios, the Low and High residuals are recomputed, and `hours_by_sector` and `L_0` include the admin centers.

The pooled step runs after the optional `supply` treatment, so it applies under either staffing measure. This changes 2019 results. The children in admin programs still enter the household side through their parents' reports.

In the original model, free admin centers (2019 rate flag −8, which `main` reads as free) already sit in the Unpaid sector's enrollment, but with no staff, because they have no workforce records. That makes Unpaid staff per FTE child too low: 0.0517 for staff without a BA in 2019, against 0.0556 with admin centers dropped.

Facts:
- Administrative-data centers (`CB9_METH_DATA_SOURCE = 2`) are public-school programs built from administrative records: 483 in 2019 and 1,141 in 2024.
- Their workforce records carry no wage or education data, and they have no classroom data. The center file does carry a staff count for most of them (*Staff counts in the administrative records* below).
- In 2019 the public file gives their enrollment by age. In 2024 it gives only total enrollment under age 6 and whether the program runs full-time or part-time.
- 75% or more report that no family pays, so they do not move the price medians.
- NORC's user guides say the interviewed centers alone are not a valid population. They advise restricting to centers not at a public school (`METH_PUBSCHFLAG = 0`) or imputing the administrative cases (2024 center user's guide, PDF p. 11, as cited in `docs/nsece_2024/survey_design_changes.md`). Dropping the admin centers gives public-school programs the staffing of the interviewed Unpaid centers, which is correct only if the two staff alike.

Measured on Bouchet (`04`, `06`, and `07`, Python re-implementation of `1a`, weighted FTE enrollment, before the admin centers had their own staff):

| | 2019 Unpaid | 2019 Low | 2019 High | 2024 Unpaid | 2024 Low | 2024 High |
|---|---|---|---|---|---|---|
| Interviewed centers | 1,452,303 | 1,613,967 | 1,670,810 | 1,287,070 | 1,878,676 | 1,929,731 |
| Admin centers | 126,283 | 5,528 | 885 | 298,430 | 0 | 0 |
| Admin share of sector | 8.0% | 0.3% | 0.1% | 18.8% | 0% | 0% |

- In that measurement every 2024 admin center that lands in a sector lands in Unpaid. The 2019 Low and High amounts come from a few centers with a posted rate.
- Admin centers by family-pay item, weighted enrollment under age 6 (headcount):

| | Families pay | No family pays | Don't know |
|---|---|---|---|
| 2019 | 13 centers, 12,700 | 469 centers, 166,215 | 1 center, 43 |
| 2024 | 179 centers, 134,898 | 930 centers, 337,032 | 32 centers, 5,164 |

- The 2024 paying admin centers hold 134,898 children (mismatch item 5 gives their FTE count).
- Staff per FTE child, Unpaid sector, `1a` as written against admin dropped from both sides: 2019 no-BA 0.0517 against 0.0562, BA 0.0565 against 0.0614. 2024 no-BA 0.0860 against 0.1060, BA 0.0631 against 0.0777. With the 2024 imputation kept, the 2024 Unpaid ratio looks far higher than 2019 for a reason that has nothing to do with staffing. With admin dropped, the 2019 to 2024 rise is real.
- Confirmed in R on 2026-10-29: `main`'s own `1a` on 2019 (run `19_main`, code of `origin/main`) gives Unpaid staff per FTE child of 0.0517 (no BA) and 0.0565 (BA), the same as the "as written" 2019 figures above. In `main`, a 2019 rate flag of −8 becomes `FREE` (price 0), so free admin centers sit in the Unpaid enrollment with no workers. `main`'s 2019 `L_0` is 1.268 billion staff-hours (no BA) and 0.546 (BA), against 1.492 and 0.603 on this branch (+17.6% and +10.4%). That difference has not been split by source. The changes between the two runs that touch the supply side are the home-based weights, the admin staff counts, and the don't-know and $2,000 fixes.
- Admin enrollment times the interviewed-center ratio comes to about 1.2% of no-BA and 3.0% of BA `L_0` in 2019, and about 3.4% and 6.2% in 2024.

Staff counts in the administrative records (`08_admin_staffing.py`, staff per child enrolled under age 6, weighted by `CB9_METH_WEIGHT`, "used" is the unweighted count of records with valid items):
- 2019: 178 of 483 admin records have all four director staff items for ages 0 to 5 (teacher and aide counts, percent full time). For those 178, the total staff count (`CB9_STAFF_TYPE_TOTAL_NUM`) equals the 0 to 5 classroom staff (ratio 0.999). For interviewed centers the 0 to 5 classroom staff are 53% to 72% of the total.
- 2024: no admin record has the four 0 to 5 items (all −8). All 1,141 have `CB9_STAFF_TYPE_TOTAL_NUM`, and NORC imputed most of them (`CB4_ADMIN_STAFF_IMP = 1`, mismatch item 7).

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

Measured in the R pipeline when the rule was first adopted (runs `202610271556` for 2019 and `202610271600` for 2024, `1a` only for 2024, free admin centers only):

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

These are the 2026-10-27 figures. Fixes 1 and 2 of 2026-10-28 changed the 2019 results: the pooled Unpaid ratios are now 0.0544 (no BA) and 0.0624 (BA), and `L_0` is 1.492 and 0.603 billion staff-hours (run `19_new`). The ratios match a hand pooling of the interviewed and admin ratios by FTE enrollment. The Low-priced, High-priced, and home-based inputs do not change. The 2024 Low-priced residual stays at −$0.47.

History. From 2026-11-06 to 2026-11-12 (Anna) supply used no admin staff: `1a` dropped every admin center before the price medians, the classroom file, the workforce ratios, and the sector rows, in both years, because their only staff item is a total headcount with no BA split and NORC imputed it for 623 of the 1,141 centers in 2024. Measured on 2026-11-06 (`1a` only, Bouchet jobs 32341414 to 32341417, runs `19_base`/`19_noadm` and `24_base`/`24_noadm`, base = the 2026-10-27 rule at `013b80d`), no other supply parameter and no care share changed:

| | 2019 base → drop | 2024 base → drop |
|---|---|---|
| Unpaid no-BA staff per FTE child-hour | 0.0544 → 0.0556 | 0.0935 → 0.1060 |
| Unpaid BA staff per FTE child-hour | 0.0624 → 0.0628 | 0.0759 → 0.0777 |
| `L_0` no BA, billion staff-hours | 1.492 → 1.481 (−0.7%) | 1.872 → 1.848 (−1.3%) |
| `L_0` BA, billion staff-hours | 0.603 → 0.588 (−2.4%) | 0.793 → 0.753 (−5.1%) |

### Negative non-labor cost for low-priced centers in 2024

Decided on 2026-11-16 (`docs/nsece_2024/decisions.md` decision 7): the model keeps one wage pair for all centers and accepts the negative residual.

The 2024 residual is −$0.47 per child-hour because labor cost uses one wage pair for all centers ($16.80 no-BA, $24.22 BA), while Low-priced centers pay less (sector wages $14.26 and $20.40, from `06`). With sector-specific wages the residual is +$0.26 (Open item 2). The Unpaid sector pays the most ($20.73 and $30.39). In 2019 the residual is +$0.47 with one wage pair (`1a`, check `06` gives $0.48) and +$0.94 with sector wages (`06`). Neither admin treatment changes the Low residual.

Checked 2026-10-29 (supply parameters of runs `19_new` and `24_new`):
- The Low-priced residual is the only negative one. 2024: High-priced $4.56, home-based $3.86, Unpaid has no price and so no residual. 2019: Low-priced +$0.47, High-priced $3.32, home-based +$2.89.
- Low-priced labor cost per child-hour rose from $2.84 (2019: 0.1786 × $12.06 + 0.0350 × $19.70) to $4.77 (2024: 0.2034 × $16.80 + 0.0560 × $24.22), up 68%. The implied Low-priced price (labor cost plus residual) rose from $3.32 to $4.30, up 30%.
- The wage pair is the weighted mean wage of workforce-survey teachers and assistants. The 2019 workforce user's guide says that survey interviewed classroom-assigned instructional staff selected from completed center-based provider interviews, so it has no home-based wages. Paid Home-Based uses the same pair, and sector wages as tested apply to the three center sectors only.
- The simulation sets the supplier price as labor cost plus the residual (`P_supply − c(Qd) = delta_0 × cpi_factor`, `3c_run_scenario.R`). With a negative residual, Low-priced labor cost is 111% of its price ($4.77 over $4.30). A 10% wage change then moves that price by 11%, and a 25% payroll subsidy would cut it by 28%.

Sector wage cells (teachers and assistants with known education and a wage, admin centers dropped, unweighted counts of workers, weighted mean hourly wages):

| | 2019 no BA | 2019 BA | 2024 no BA | 2024 BA |
|---|---|---|---|---|
| Unpaid | 418, $13.82 | 390, $26.23 | 642, $20.73 | 544, $30.37 |
| Low | 892, $10.35 | 280, $14.91 | 1,279, $14.25 | 418, $20.40 |
| High | 894, $12.90 | 365, $15.47 | 1,314, $17.62 | 639, $22.06 |

Every cell has at least 280 workers. About 1,000 workers per year have a wage but no sector (no classroom age bin or no usable center price). They enter the all-center wage but would leave the sector wages. John's option (e5) in `supply_side.md` restates staffing in average-wage hours so `L_0` keeps one unit. It gives +$0.26 for 2024 (Open item 2).

### Calendar cases coded −10

Without a fill, parents with a −10 calendar block count as not working, which understates 2024 parent employment by about 3.5 to 5 points. The missing-hours fill fixes it (mismatch item 3). Only 73% of 2024 households did the weekly calendar, against 94% in 2019, and from June 2024 most got a typical-week question. The wording change alone lowers parent-only care by about 3.4 points. The 2024 survey also puts 64.3% of young children with a working mother, against 59.6% in the March 2024 CPS and 62.9% in the 2024 ACS (check `11`). See `docs/nsece_2024/calendar_and_work_hours.md` and `employment_and_reweighting.md`.

### John's optional 2024 treatments (`--nsece-2024-options`)

`weights` and `supply` are off by default. `hours` was an option until 2026-10-28. Sizes come from John's runs on older settings, summarized in `docs/nsece_2024/decisions.md`. Current figures are in Open item 4.
- `hours` is always on for 2024 since 2026-10-28 and is no longer an option (mismatch item 3). In John's runs, 2024 employment of two-parent primary caregivers went from 58.6% to 62.2%, single parents from 64.5% to 68.0%, and partners from 84.9% to 89.8%. Applying the same fix to 2019 moves 2019 employment by 0.4 to 0.7 points.
- `weights` reweights 2024 child weights to the March 2024 CPS on mothers' employment by family type. In John's runs it shrank the 2019 to 2024 change in parent-only care from −6.7 points to between −2.2 and −3.5, and in any paid care from +4.2 to between +0.1 and +1.3.
- `supply` (John Ricco, commit `fb56186`): see *The main choices*. Its cap deletion is gone since 2026-11-06 (mismatch item 4).
- The calendar wording change (typical-week households) has no option. Every treatment John tried moves 2024 parent-only care by 1.2 points or less, except counting usual providers in both years, which also moves 2019 (52.6% to 47.9%).

### Other findings

- John's option studies in `docs/nsece_2024/` quote $5.76 and the 2023 ACS as the setting of his study runs. They are a record of those runs. Each opens with a status note that points here, and `decisions.md` gives the status of each decision. His 2019 variable inventory was removed on 2026-10-28: its line numbers referred to an old commit, and this crosswalk and check `01` cover its content (it stays in commit `a442c83`).
- Fixed on 2026-11-17: the finalize step printed "There were 50 or more warnings" on `main` and on this branch. All 148 were one message: `4a_finalize_output.R` wrote column 18 of the Excel distribution sheet from `avg_welfare_ecec_burden_level`, which `3d` never creates, so that column was always blank (in `main` since its first commit, no effect on the 2024 work or any model result). The model has no welfare burden level (the welfare effect is a money measure with no cost-based counterpart), so the ECEC burden block now has three columns (baseline, mechanical, total), and the employment columns move from 20–22 to 19–21.

### Mismatches between the two years

Measured on 2026-10-28 on the code of `7b12f29`: `09_definition_checks.py` on the raw files, and `1a` re-run on Bouchet per variant. Weighted figures use the survey weights. "Cells" are center-age cells with enrollment, unweighted.

1. Don't-know payment counted as free in 2019 supply, but not in 2024.
   - Fix, approved and implemented on 2026-10-28: in 2019, `1a` treats −8 cells of FAMPAY = 3 centers (66 centers) as a missing price. 2024 already follows this rule, and the household side already treats don't know as missing (*Don't-know cost → missing*).
   - In 2019, NORC sets the rate flag `CB9_B1_3A_AGERATE_*_R` to −8 ("No families pay") for every age of a center that answered `B1_3_FAMPAY` = 2 (none pay) or 3 (don't know). `1a` reads −8 as free. In 2024, −8 marks admin cases, so `harmonize_nsece_2024_centers()` sets free status from FAMPAY = 2 only, and don't know becomes a missing price.
   - The two rules agree on 99.3% of 2019 interviewed-center FTE enrollment. The rest is 142 cells and 43,688 weighted FTE children with FAMPAY = 3: 3.0% of interviewed Unpaid Center-Based FTE enrollment.
   - The 66 don't-know centers look like paying centers, not free ones (`10_fix_evidence.py` section E, unweighted shares of centers). 76.8% report tuition revenue (93.3% of paying centers, 9.4% of free centers). 26.0% are for profit (46.1% and 6.5%). 13.9% are public schools (10.7% and 49.6%). In 2024 the 103 don't-know centers are alike: 70.3% report tuition, 48.4% are for profit. They have no posted rate, so a missing price is the only choice that does not invent one.
   - Measured before the fix went in: the 2019 Unpaid staff per FTE child-hour goes from 0.0550 to 0.0544 (no BA) and from 0.0611 to 0.0624 (BA), and `L_0` falls 0.4% (no BA) and 0.2% (BA). No other supply parameter changes.
2. Posted rates above $50 per hour.
   - Fix, approved and implemented on 2026-10-28: weekly rates above $2,000 are missing in both years. They carry their own missing code (−20), so 2019 −2 means only NORC's censoring.
   - The NORC censoring (item 4) works within each time unit, so it leaves outliers in the weekly rate. Cells with a weekly rate above $2,000 ($50 per hour at 40 hours): 38 in 2019 (6 at age 2, 15 at age 3, 17 at age 4, the largest $146,000 per week) and 8 in 2024.
   - They are unit errors, not real rates (`10_fix_evidence.py` section F). All 38 are paying centers (FAMPAY = 1). Most are $2,800 to $3,990 per week, which is $604 to $918 per week if read as a month, and often the same center has the error at several ages. Others read as a year: $48,960 is $942 per week, and $146,000 is $2,808. Some sit next to normal rates of $144 to $230 per week at the other ages of the same center.
   - They changed results. The sector price is a weighted mean (`1a`, and `main` too), and the median sets only the Low/High split. `main` uses them as well. John's `docs/nsece_2024/prices.md` puts the 2019 High price at $8.08 with them and $6.70 without them.
   - Measured before the fix went in: the 2019 High-priced residual falls from $4.72 to $3.32 per child-hour, so the outliers lift it by $1.40 (42%). The 2024 High-priced residual falls from $4.60 to $4.56. The Low-priced residuals move by less than $0.01, and `L_0` by 0.3% or less. With the item 4 variant as well, the High residual is $4.60 (run `24_caps50`). The run removes 10 rates above $2,000 in 2024: the 8 cells above count only cells with enrollment, the run counts every rate.
   - Adjusting them was tested on 2026-10-29 (Bouchet, job 31305818) and is not safe. The public file has only the converted weekly rate, with no reported time unit or raw amount. There are 40 flagged 2019 cells in 24 centers (10 in 6 centers in 2024), counting cells without enrollment. Only 8 (3 in 2024) sit at a center with a rate for another age. Of those 8, reading the amount as monthly (divide by 4.34524) lands within half to double of the center's other-age median for 2 cells, and reading it as yearly (divide by 52) for none. In 2024 the yearly reading fits 1 of 3.
3. 2024 parent work hours missing for calendar code −10.
   - Current rule (Anna, 2026-11-06, both years, `parent_calendar_hours()` and `draw_donor_hours()` in `1a`): zero calendar hours with D1a yes → donor. No hours record: D1a no → 0, D1a yes → own calendar hours where positive (2024 calendar households), else donor. A child row without a record takes the parent's hours from another child's record first. Donors are drawn at random from cells of earner rank, child's weekly care hours, income quintile, wage band, region, and urban density, and keys are dropped from the end until 5 donors match.
   - 2024 adult calendar file (`39466-0004`, codes 1 work, 2 school, 3 training, 4 missing per 15-minute block): blocks × 0.25 reproduce NORC's hours for all 3,857 complete calendars. A missing block takes the parent's own work share at the same time of day on other days. On complete calendars with a real missing pattern copied on, the error is about 10 hours, against about 15 for any donor rule.
   - The zero is the parent's own answer, not an imputation: a parent with no hours record who answers D1a "no" did no paid work last week. The 2019 pipeline applies the same rule when calendar hours conflict with D1a: parents with positive calendar hours who say they did not work are set to zero (328 parents in 2019, 271 in 2024), because the calendar also counts school, training, and commuting. A parent who was truly not working stays at zero.
   - Why these cells (masked test, scratch, parents with D1a yes and known hours, hours hidden and redrawn, 20 draws): filled parents are unlike donors, and their child has no non-parental care in 54% to 81% of cases, against 33% of donors (weighted). The true full-time gap between parents whose child has 35+ hours of care and parents whose child has none is +7.5 points (2019) and +10.8 (2024). Earner-rank draws give −6.3 and −5.8, and earner rank × wage band −5.9 and −2.6. With earner rank × wage band, the income gap (top vs bottom quintile, true +10.9 and +10.5) falls to +1.5 and +5.1. The six-key cells keep both gaps (+7.2 and +11.6, +10.8 and +12.3) and lower the hours error (14.3 vs 15.3 hours in 2019). Dropping one key at a time: care hours, income quintile, and wage band each matter, region helps in 2024, urban density matters little but is matched last, and care type, parent count, child age, and questionnaire version add nothing and thin the cells.
   - Measured 2026-11-06 (`1a` only, runs `19_hrs`, `24_hrs` against `19_tail`, `24_tail`): 2019 donor hours for 469 zero-hours and 66 no-record parents, 164 no-record parents set to 0. 2024: 232 zero-hours donor, 187 own calendar, 191 no-record donor, 79 set to 0. Donor cells keep all six keys for 367 of 533 recipients (2019) and 289 of 423 (2024), and 15 lose three or more. Work rates (hours above 0, household weights) 2019: one-parent fathers 84.43% → 86.63%, two-parent fathers 90.67% → 91.02%, one-parent mothers 51.33% → 51.85%, two-parent mothers 53.72% → 54.11%. 2024 unchanged. No care share or supply parameter changes.
   - Cell averages tested on 2026-11-13 (Harris's suggestion, cluster `agr55/nsece_evidence/mtg1113/`, `mask_m2019.out`, `mask_m2024.out`). Masked test: 20% of donors' hours hidden and filled from the other donors, same cells and key dropping, 50 masks. The cell average lowers the mean absolute error from 15.0 to 10.8 hours (2019) and from 15.6 to 11.2 (2024), but it puts every parent near the cell mean. The full-time share of first earners becomes 97% (2019) and 95% (2024) against a true 85% and 83%, and for second earners 46% against a true 58% in both years. One donor draw keeps both shares within 1.2 points. The model uses hours only through the full-time and part-time split, so the code keeps the donor draw.
   - Fixed on 2026-11-13: monthly pay converts to hourly pay at 2080/12 hours a month, as the code comment and the yearly unit (2080 hours) state. The code used 2000/12, which made monthly earners' hourly pay 4% too high. Parent pay enters only the donor cells' wage band.
   - Size of the gap. Without a fill, the share of children under 5 whose mother works (child weights, hours above 0) is 58.85% in 2024, against 62.17% with the 2026-10-28 fill. Two-parent fathers go from 85.27% to 90.48% (household weights). Checked 2026-10-29 by comparing the 2024 runs without and with the fill (`24_none`, `24_new`): of 5,598 parents of children under 5 (unweighted), 263 had no hours record (4.7%). The fill covered 252: 209 got donor hours and 43 got zero. 11 stayed missing, because they have no usable D1a answer. No parent who already had hours changed. After the fill, 1,539 parents have zero hours and 4,048 have hours above 0. The fill changes no care share and no supply parameter.
   - History. 2026-10-28 (approved and implemented, always on for 2024, `--nsece-2024-options hours` accepted and ignored): donors drawn by earner rank, then by earner rank and hourly wage band (under $15, $15–25, $25–40, $40 or more, no wage). The filled parents earn less than donors: 37.9% have no pay record against 17.5%, and 11.2% earn $40 or more an hour against 23.0% (weighted), so the earner-rank-only draw gave them the hours of higher-paid parents. With the wage band, the smallest donor cell had 270 parents. Weighted mean weekly hours of the filled parents against donors: first earners 44.7 against 43.9, second earners 35.1 against 35.9 (run `24_hours2`). Before that fix, two-parent fathers got 39.9. Within single wage bands the filled means differed from the donor means by up to 12.4 hours, because each band held 5 to 61 filled parents. The draw is random, not biased. Work rates do not change with the donor rule (hours above 0 either way): mothers in two-parent units 59.33% without `hours` and 63.18% with it. The hours enter `1b` and calibration only. The 2026-11-06 rule replaced these donor rules because they biased care choice.
4. 2019 deletes the censored rates, 2024 keeps them at the cap.
   - Both years use one rule: each reported time unit (hourly, half-day, full-day, weekly, monthly, term, and so on) is censored at the 5th and 95th percentiles of the weekly rates in that unit, for each age. NORC does this "to limit disclosure". 2019 deletes the censored rates (code −2, "Trimmed Value"): 9.0% to 9.5% of positive rates per age. 2024 gives each one the exact cap value of its unit, with no flag (2024 center user's guide p. CB-508).
   - The 2024 caps are therefore visible as pile-ups of one value. Per age, the four largest are the weekly and the monthly caps. Age 3: $130 (115 rates) and $390 (106) per week, $69.04 (63) and $540.82 (62), which are $300 and $2,350 per month. These four hold 9.2% to 10.3% of 2024 positive rates per age, close to the 2019 share. Smaller pile-ups (for example $775 at age 3, 12 rates) are the caps of rarer units.
   - Current rule (Anna, 2026-11-06, `fill_censored_rates()` in `1a`, both years, always on): fill each censored rate instead of dropping it.
     - The uncensored rates (positive, $2,000 a week or less) have one peak and fit a lognormal closely: quantile-fit R² 0.983 to 0.992 by age, against 0.91 to 0.95 for a normal in dollars.
     - 2024: the caps are known, so a censored (Tobit) lognormal fit uses them. Each censored rate gets the lognormal mean beyond its own cap: below the low cap $2.10 to $2.99 per hour by age, above the high cap $14.48 to $17.29.
     - 2019: only the count of censored rates is known. A quantile fit of the body puts that share in the two tails, and the cuts are the fitted quantiles. Since 2026-11-10 (Anna) each tail's share is the center-weighted sum of the side model's probabilities below, by age (low 7.3% to 8.5%, high 3.0% to 3.7%, against 2024's observed 6.6% to 8.5% and 2.7% to 4.0%), not half and half. Also since 2026-11-10, 2019 rates 4 or more standard deviations above their age's mean (28 to 39 rates per age, $675 to $1,850 a week) are left out of the fit only. The rates stay in the data. The 2024 file re-imputes the time unit of such rates (2024 center user's guide p. CB-508), and 2019 has no such step. With them in, the fit underpredicted the rates just inside the 2019 high cut by 11% to 20%, and without them by about 3% or less, as in 2024. Fit sigma 0.42 to 0.51 (2024: 0.49 to 0.53).
     - The tail of each 2019 censored rate comes from a logit fit on the 1,268 censored 2024 rates, where the tail is known: center traits (for profit, public school, Head Start, public pre-K, subsidy revenue, log FTE size), region, urban density, and the center's own rates at other ages. In 5-fold cross-validation on 2024 (without urban density) it gets the tail right for 88% of rates, against 70% by always guessing low. Filled values: low tail $1.46 to $2.24 per hour, high tail $11.43 to $12.84 (before 2026-11-10: $1.51 to $2.06 and $10.72 to $13.08, smoke run 202611101346). The 2019 per-unit residual moves from $0.172 to $0.176 (Low-priced) and from $3.680 to $3.721 (High-priced).
     - A skew-normal fit was tested and not used: it moves 2024 fills only 2% to 5.5%, and its skew changes sign between years (scratch tests on the cluster, `agr55/nsece_evidence/skew1110/`).
     - Every low-tail mean is below its age-bin median and every high-tail mean above it, so a filled cell lands in Low- or High-priced.
   - Measured 2026-11-06 (`1a` only, runs `19_tail` and `24_tail` against `19_noadm` and `24_noadm`, admin centers dropped in all four):

     | | 2019 before → filled | 2024 before → filled |
     |---|---|---|
     | Low residual ($ per child-hour) | 0.47 → 0.17 | −0.47 → −0.56 |
     | High residual | 3.32 → 3.68 | 4.56 → 4.83 |
     | Low no-BA / BA staff per FTE child-hour | 0.1786 / 0.0350 → 0.1784 / 0.0375 | unchanged |
     | High no-BA / BA | 0.1559 / 0.0758 → 0.1518 / 0.0760 | unchanged |
     | `L_0` no BA / BA, billion staff-hours | 1.481 / 0.588 → 1.600 / 0.642 (+8.0% / +9.1%) | unchanged |

     In 2024 the capped cells already had a sector, so only prices move. No care share changes in either year. In 2024 the fill makes the Low residual more negative, because low-tail centers are cheaper than the cap.
   - History. John's original `supply` trim cut at the 5th and 95th percentiles across all units, which removes 10.0% to 11.2% of 2024 rates but not the same rates as NORC. From 2026-10-29 to 2026-11-06 the `supply` option instead set 2024 positive rates equal to the four caps of their age to missing (−2), as 2019 does. That rule found the caps itself: per age, the most frequent value below the 15th and above the 85th percentile, separately for monthly amounts (rate × 4.34524 is a whole dollar) and for the others. Measured (run `24_caps2`): the Low-priced residual goes from −$0.47 to −$0.25 per child-hour and the High-priced from $4.56 to $4.60. Low no-BA staff per FTE child goes from 0.2034 to 0.2010 and High from 0.1560 to 0.1513. `L_0` falls 8.0% (no BA) and 7.4% (BA), because deleted rates drop their center-age cells, with their staff, from the sector rows, as the trimmed 2019 centers do (`main` does the same in 2019).
   - Priced enrollment each year keeps (`10_fix_evidence.py` section I, interviewed centers where families pay, weighted FTE children by 1a age bin, a cell is priced if any rate in the bin is positive):

     | Cell class | 2019 | 2024 now | 2024, caps deleted |
     |---|---|---|---|
     | Priced | 75.5% | 87.4% | 79.2% |
     | Trimmed (−2, or caps deleted) | 9.2% | 0.0% | 8.2% |
     | No time unit (−1) | 6.2% | 6.3% | 6.3% |
     | No rate or other code | 9.2% | 6.3% | 6.3% |
     | All, weighted FTE children | 4,371,347 | 4,365,088 | 4,365,088 |

     With the caps deleted, the trimmed share is 8.2% in 2024 against 9.2% in 2019, and the priced share 79.2% against 75.5%. The "no rate or other" share differs by 2.9 points, which is not a censoring difference.
   - Open: whether cells without a price should keep a sector and their staff. That change would also move the 2019 results of `main`. Measured on 2026-10-29 with fixes 1 to 3 in place (runs `19_keep`, `24_keep`, `24_capskeep` against `19_new`, `24_new`, `24_caps2`). Test rule: an enrolled cell without a price takes its center's class from the center's other priced bins (the mean of price over bin median: 0 is Unpaid, below 1 is Low, 1 or more is High). Its price stays missing, so sector prices do not change, and its FTE children and staff join the sector. Most unpriced cells cannot use this rule, because their center has no priced bin at all. Cells and FTE children below are unweighted cells and weighted FTE children:

     | | 2019 | 2024 | 2024, caps deleted |
     |---|---|---|---|
     | Enrolled cells without a price | 3,481 | 2,304 | 3,120 |
     | Cells given a sector | 1,122 | 869 | 1,095 |
     | Weighted FTE children given a sector | 225,199 of 1,142,289 (19.7%) | 160,717 of 661,835 (24.3%) | 242,127 of 1,014,923 (23.9%) |
     | Of these: Low / High / Unpaid | 116,687 / 92,198 / 16,314 | 75,946 / 73,742 / 11,029 | 136,495 / 94,414 / 11,218 |
     | `L_0` no BA / BA | +7.6% / +2.6% | +3.1% / +3.7% | +5.1% / +6.3% |
     | Low residual ($ per child-hour) | +0.47 to +0.41 | −0.47 to −0.46 | −0.25 to −0.21 |
     | High residual ($ per child-hour) | 3.32 to 3.34 | 4.56 to 4.54 | 4.60 to 4.50 |

     With the caps deleted and the cells kept, 2024 `L_0` is 3.3% (no BA) and 1.5% (BA) below the current code, against 8.0% and 7.4% below with the caps deleted alone. Without the caps deleted, the rule adds more staff in 2019 (+7.6% no-BA `L_0`) than in 2024 (+3.1%), because 2019 has more unpriced enrollment (1,142,289 weighted FTE children against 661,835). Not decided, test variant only.
5. Admin centers where families pay were outside every sector.
   - Decided on 2026-11-12: paying admin centers join Low and High by the 72/28 split (*Administrative-data centers*, step 6). On 2026-11-06 to 2026-11-12, when admin centers were dropped from supply, paying ones stayed in no sector.
   - 2024: 179 centers, 114,999 weighted FTE children (3.0% of interviewed Low- plus High-priced FTE enrollment), plus 32 don't-know centers with 3,366. 2019: 13 centers, 6,762 (0.2%).
   - The 2024 file withholds their price, so the code cannot place them by price. In 2019 most carry a posted rate (6,413 of the 6,762 FTE children), but `1a` uses the same split there. The 2019 admin centers with a posted rate are 86.2% Low-priced (6,413 FTE, mean $3.66 per child-hour). Paying public-school programs, the nearest interviewed match, are 77.9% Low-priced in 2019 ($6.40 mean, 193,793 FTE) and 72.1% in 2024 ($4.79 mean, 138,960 FTE) (`10_fix_evidence.py` section G).
   - Their staffing: 0.1165 staff per FTE child in 2024 (own counts, all 179 valid, headcount × the public-school FTE factor), against 0.1079 for free admin centers. Interviewed Low-priced centers have 0.2594 (no BA plus BA) and High-priced 0.2311. 2019 has 13 centers, 9 with a count, and they take the free admin rate, 0.0991.
   - Sector fit, tested 2026-10-29 (Bouchet, `nsece_pricefit`, jobs 31305163 and 31305458, scratch scripts, not in the repo). It predicts the Low or High band of the 110 (2024) and 116 (2019) interviewed paying public-school programs from variables that paying admin centers share with them (region, log FTE size, community urban and poverty density, school sponsor, building type), by a ridge-penalized logit. The class is a center-level rule (mean of price over its age-bin median, 1 or more is High), so its Low shares differ from the 77.9% and 72.1% above by 2 to 5 points. Weighted 5-fold cross-validation, 20 repeats, log loss (lower is better):
     - 2019: fixed split 0.485, region and size 0.416, with context 0.367 (AUC 0.75 and 0.83). On the 12 paying admin centers that carry a price, 0.419 for the fixed split and 0.344 with context.
     - 2024: fixed split 0.667, region and size 0.692, with context 0.712 (AUC below 0.5). The fit moves the Low share of the 114,999 FTE children from 70.3% to 71.3% to 72.3%.
     - Recommendation at the time: the fixed 72/28 split for 2024 (the price is withheld and the covariates carry no signal), each 2019 center's own price where it has one.
   - Measured, with their FTE enrollment and staff added to the sector rows (runs `24_admlow`, `24_admhigh`, `24_admsplit`, price unchanged, as they have no price, the split puts 72% in Low and 28% in High, the shares of paying public-school programs). All three placements were test variants. The price evidence puts 72% to 86% of comparable enrollment in Low, so Low and the split are the placements the evidence supports. They differ by $0.03 in the Low residual.

     | 2024 | Before | In Low | In High | Split 72/28 |
     |---|---|---|---|---|
     | Low no BA / BA per FTE child | 0.2034 / 0.0560 | 0.1942 / 0.0570 | unchanged | 0.1966 / 0.0567 |
     | High no BA / BA per FTE child | 0.1559 / 0.0752 | unchanged | 0.1496 / 0.0751 | 0.1541 / 0.0752 |
     | Low residual ($ per child-hour) | −0.47 | −0.34 | −0.47 | −0.37 |
     | `L_0` no BA / BA | | +0.5% / +2.1% | +0.5% / +2.1% | +0.5% / +2.1% |

     The High-priced residual is not shown: these runs predate the $2,000 fix, which moved it ($4.60 became $4.56). `L_0` gains the same staff under every placement. In 2019 every placement moves each ratio by less than 0.4% and `L_0` by 0.1% (runs `19_admlow`, `19_admhigh`, `19_admsplit`).
6. Free status is center-wide in both years, not by age.
   - The 2019 −8 flag is set for every age of a center, as FAMPAY is. 2024 has no by-age flag. The two years match, so no fix is needed.
7. NORC imputed most 2024 admin staff counts.
   - Current rule (Anna, 2026-11-17): `1a` replaces each 2024 count with `CB9_ADMIN_STAFF_IMP = 1` by a random forest (`ranger`, 300 trees, leaf size 10, center weights, seed 1117) of log(staff per child 0 to 5) on log enrollment 0 to 5, four funding items (federal funds, state funds, the mix of funding sources, and the state or local source of pre-K money), community poverty density, and urban density, fit on the admin centers whose count comes from the records, with out-of-bag smearing. The refit leaves out region: 18 of the 518 reported counts are in the Northeast, compared with 178 of the 623 imputed counts. The 2019 counts are used as reported.
   - 2019 counts look reported. In 2019, 403 of the 469 free admin centers have a valid staff count, and for the 178 records that also carry director items, the count equals the reported classroom staff. The 2019 center file has no imputation flag for the staff count (the 2024 file has `CB4_ADMIN_STAFF_IMP`), and the 2019 user's guide says the administrative data came from district, state, and federal sources, with no imputation of staff counts (checked 2026-10-29 by searching the guide for imputation and reading section 4.2).
   - NORC imputed `CB9_STAFF_TYPE_TOTAL_NUM` for 578 of the 930 free 2024 admin centers (623 of all 1,141). Those 578 report 0.1138 staff per FTE child against 0.1228 for the 352 reported ones. NORC's method: total enrollment 0–5 times the district's staff:student ratio, or 0.11 where no center in the district reported (2024 center user's guide p. CB-156). For large centers that is too high. The imputed counts are not whole numbers, and 437 of the 578 sit at exactly 0.110 staff per enrolled child.
   - Tests in `10_fix_evidence.py` section J (weighted by center weights):
     - The reported counts are whole numbers, and 92.0% of the imputed counts are not. 75.6% of the imputed centers sit at exactly 0.110 staff per enrolled child (NORC's default ratio), and the other 24.4% got a district ratio.
     - The imputed centers are larger (mean 45.2 FTE children against 30.7) and more often full time (full-time share 0.91 against 0.62). Their region mix also differs: region 3 is 16.7% of imputed and 66.5% of reported centers.
     - Reported staff per FTE child falls with size, from 0.186 in the smallest quarter to 0.088 in the largest. NORC's imputed counts are almost flat: 0.138 to 0.112. In the largest quarter, which holds 234 of the 578 imputed centers, NORC is 28% above the reported centers of the same size.
     - Every imputed center lies inside the reported centers' size range (0.8 to 192.5 FTE children), so the size-based estimates do not extrapolate.
   - Holdout tests, 2026-11-16 and 2026-11-17 (`nsece_evidence/admincv1116` on Bouchet: `admin_cv.R`, `admin_cv3.R`, `admin_hybrid.R`, `admin_cv4.R`, `admin_cv6.R`, jobs 33680253, 33680624, and 33681418). Each fit is tested on the 518 reported counts by 10-fold cross-validation, repeated 20 times, with center weights. A second score reweights the held-out programs by their odds of being imputed (a logistic fit on every item), so that they look like the imputed programs. That reweighting leaves an effective sample of 46 programs. The standard errors come from 200 bootstrap resamples of the reported programs.
     - Every fit gets the plain held-out total within about 1%.
     - The chosen forest has a held-out error of 0.529 in log staff per child (0.544 reweighted), overstates the reweighted total by 1.6% (bootstrap 2.1%, standard error 6.5%), and gives 11.4% fewer staff than NORC in the holdout runs (9.1% fewer with the hybrid below). In the model run (seed 1117), the refit total is 9.2% below NORC's.
     - Forests on every item have the lowest error (0.518, and 0.522 in the first round, against 0.565 for the 2026-11-12 fit and 0.611 for a constant) but overstate the reweighted total by about 7% (8.5% in the first round).
     - The 2026-11-12 fit (size bins and funding) overstates the reweighted total by 10.9% (standard error 5.6%, too high in 98% of draws). Every linear fit with funding overstates it by 9% to 10%. Log size alone overstates it by 2.1% (standard error 9.7%) and gives 11.9% fewer staff than NORC. Size bins alone: +6.1% (8.3%). Size bins and poverty: +2.5% (8.6%). Every item: +3.9% (5.4%), but 34.9% fewer staff than NORC. Fits on log size plus one more item (poverty, urban density, economic disadvantage, or payment), quasi-Poisson count fits, and fits weighted by imputed odds all land within about 4% of the reweighted total (standard errors 2% to 10%).
     - Random donor draws matched on size fifths, then funding, poverty, and urban density (dropping traits until 5 donors match, as in the hours fill) get the total within about 5% but miss single programs by 0.74 to 0.81. Cell averages and nearest-neighbor donors on size fall between the two.
     - NORC's 0.11 applied to the reported programs overstates their total by 10.6%.
     - 464 of the 623 imputed counts (69% weighted) sit at the 0.11 default. The other 159 use a district ratio. Refitting only the 0.11 defaults (the hybrid) gives 2.3% fewer staff than NORC with the 2026-11-12 fit. For the district-ratio programs, that fit gives 10.6% fewer staff than NORC.
   - History. From 2026-11-12 to 2026-11-17 the refit was a weighted least-squares fit of log(staff per child 0 to 5) on enrollment-size bins (0, 20, 40, 80, and 160 children) and the four funding items, with a smearing retransform. Its total was 5% below NORC's imputed total (11% with size alone). Adding every other item the records carry moved it to 24% to 36% below, by leaning on items where the imputed programs differ from the record programs. A region term cut the Northeast counts by 48% on those 18 programs (24% in total). The full-time share, the economically disadvantaged flag, and urban density changed the total by about 1 point (checks `admin_refit.py` and `admin_groups2.py`, NSECE status memo of 2026-11-13). The choice was closed on 2026-11-06, when admin centers were dropped from supply, and reopened on 2026-11-12, when they rejoined it. When open, it moved 2024 Unpaid staff per child by −2.8% to +0.8% for the rules with both ratios read, and `L_0` by +0.1% to −0.3% (no BA) and +0.2% to −1.0% (BA), including the region fits.
   - Earlier rules, weighted staff totals, and their effect in `1a` (runs `24_stbase`, `24_stflat`, `24_stcells`, `24_stloglog`, `24_streg`, and `24_stregs`, 2026-10-29, 352 reported centers in the fits, the first four reproduce the 2026-10-28 numbers exactly):

     | Rule for the 578 imputed centers | Staff, imputed centers | All 930, vs NORC | Unpaid no BA / BA per FTE child | `L_0` no BA / BA |
     |---|---|---|---|---|
     | NORC's counts | 21,504 | 0% | 0.0935 / 0.0759 | |
     | Flat reported ratio | 23,203 | +4.9% | 0.0939 / 0.0765 | +0.1% / +0.2% |
     | Reported ratio by size quarter × full time | 15,716 | −16.6% | 0.0923 / 0.0738 | −0.2% / −0.8% |
     | Log-log size fit (slope 0.465) | 18,540 | −8.5% | 0.0929 / 0.0748 | −0.1% / −0.4% |
     | Log-log size fit + region (`24_streg`) | 15,530 | −17.1% | 0.0922 / 0.0737 | −0.2% / −0.9% |
     | Log-log size fit + region + full-time share (`24_stregs`) | 14,362 | −20.4% | 0.0920 / 0.0733 | −0.3% / −1.0% |

     Within the same size range, the reported centers imply fewer staff than NORC's counts for the large, full-time centers that make up most of the imputed group. So the flat reported ratio is the least accurate rule, and NORC's counts are probably too high. Region does not explain the gap (section J6): reported centers in regions 1 and 2 have about 0.06 staff per FTE child and in regions 3 and 4 about 0.13, and most imputed centers are in regions 1 and 4. A log-log fit with region gives 15,530 staff for the imputed centers (−27.8% against NORC, −17.1% for all 930), and with the full-time share as well 14,362 (−33.2%, −20.4%). Region 1 has only 18 reported centers, so its coefficient is uncertain. NORC used its 0.110 default for 75% to 92% of imputed centers in regions 1, 2, and 4, against 21% in region 3, so most imputed counts carry no local information.
   - Size and age, checked on 2026-11-09 (scratch `admin_size_1109.py` and `admin_age_1109.py`, `nsece_evidence/admin1109` on Bouchet, staff per FTE child 0–5, center weights, counts are unweighted centers). Admin centers serve only children 3 to 5 (no 2019 admin record has enrollment under 3). Neither size nor age mix explains their low ratio. In every size band, admin centers report 0.08–0.20 staff per FTE child, 36%–84% of the 0.18–0.45 classroom staff headcount at interviewed public-school centers that also serve only children 3 to 5:

     | Enrolled 0–5 | 2019 admin, total count | 2019 interviewed public school, 3–5 only, classroom headcount | 2024 admin, reported count | 2024 interviewed public school, 3–5 only, classroom headcount |
     |---|---|---|---|---|
     | 0–19 | 0.161 (42) | 0.447 (117) | 0.199 (56) | 0.326 (102) |
     | 20–39 | 0.135 (178) | 0.271 (186) | 0.147 (208) | 0.176 (126) |
     | 40–79 | 0.099 (151) | 0.203 (211) | 0.109 (199) | 0.246 (133) |
     | 80–159 | 0.083 (40) | 0.180 (79) | 0.115 (49) | 0.213 (83) |

     The admin total count is comparable with interviewed classroom headcount, not with interviewed total staff (0.38–0.41, which includes staff outside the 0–5 classrooms): in 2019 the admin total equals the 0–5 classroom count. The data cannot tell whether admin programs staff more leanly or whether their counts leave out staff.

### Open items

1. What `L_0` is anchored to (provider-side staffing, as now, or labor demanded at calibration hours). Open. Proposed: set `L_0` to labor demanded at survey prices after calibration (option 3 below).
   - Calibration does not absorb the gap (checked against the code on 2026-11-17). `2a_calibration.R` fits demand at the survey wages and at prices of unit cost plus `delta_0`, and never reads `L_0`. The simulation (`3a_initialize_simulation.R`, `3c_run_scenario.R`) reads `L_0` from the supply YAML as is, and nothing rescales it. It then sets wages from `(L / L_0)^(1/e)`, with `L` the labor that simulated choices need. In the base year that `L` is below `L_0`, so the solved wage comes out 2% to 15% below the survey wage (`supply_side.md` Choice 6), and base-year prices come out below the prices calibration used. Policy changes from baseline do not depend on `L_0`, because the wage response is (L_policy / L_baseline)^(1/e). Wage, price, and cost levels in the baseline do move. On 2026-10-29 Anna had treated the gap as not an NSECE 2019 to 2024 choice, because the demand side is matched to the base year later by design, and the 2024 status memo left it out.
   - Measured on 2026-11-17 (cluster `agr55/nsece_evidence/l0_1117`, job 33684093: 2019 baseline at 1%, calibration interface 202611161752, output interface 202611171147). In the 2019 base year, labor demanded is 80.7% of `L_0` without a BA (1.298 against 1.609 billion staff-hours) and 72.8% with a BA (0.480 against 0.660). The solved wage is 5.2% below the survey wage without a BA ($11.43 against $12.06) and 14.7% below with a BA ($16.80 against $19.70). Solved prices are 7.2% (low-priced), 4.5% (high-priced), and 3.6% (home-based) below the calibration prices. The no-parent fixed demand adds about 2.4% of `L_0` in each staff type, which narrows the wage gaps to about 4.5% and 13.3% (first-order estimate). 2024 is not measured, because no 2024 run exists without a tax-unit crosswalk.
   - Options: (3) set `L_0` to labor demanded at survey prices after calibration, about 1.34 and 0.50 billion staff-hours with the fixed demand, so the base-year wage equals the survey wage by construction. (4) Keep `L_0` and raise `w_0` by (L_0 / L)^(1/e), +5.5% and +17.2%, which gives the same results but stores a `w_0` that matches no survey figure.
   - Earlier measurements (fixes 1 to 3 in place, runs `19_new`, `24_new`, 2026-10-29): at NSECE household-side hours (children under 5, part time 520 and full time 2,080 hours a year), labor demanded is 72.0% (no BA) and 66.0% (BA) of `L_0` in 2019, and 74.9% and 70.8% in 2024. For centers alone it is 58.0% and 66.3% (no BA). At the ACS hours the model uses (2019 `1b` with the restored care-choice forest, children under 5, observed employment), it is 83.6% (no BA) and 72.4% (BA) of `L_0`, and 64.5% and 62.7% for centers alone.
2. Sector-specific wages for the center labor cost, in both years, to remove the negative Low-priced residual in 2024. Decided on 2026-11-16: not used, the model keeps one wage pair (*Negative non-labor cost for low-priced centers in 2024*). Measured on 2026-10-29 with fixes 1 to 3 in place (runs `19_secw`, `24_secw`), as John's option e5: each center sector's staffing is restated in all-center-wage hours (ratio × sector wage / all-center wage), so labor cost is the sector wage times staffing and `L_req` and `L_0` stay in one labor unit. The ratios in the table are all-center-wage staff-hours per child-hour, not staff per child.

   | | 2019 now | 2019, sector wages | 2024 now | 2024, sector wages |
   |---|---|---|---|---|
   | Low residual ($ per child-hour) | +0.47 | +0.95 | −0.47 | +0.26 |
   | High residual ($ per child-hour) | 3.32 | 3.52 | 4.56 | 4.59 |
   | Unpaid no BA / BA per FTE child-hour | 0.0544 / 0.0624 | 0.0625 / 0.0829 | 0.0935 / 0.0759 | 0.1154 / 0.0951 |
   | Low no BA / BA | 0.1786 / 0.0350 | 0.1532 / 0.0265 | 0.2034 / 0.0560 | 0.1725 / 0.0472 |
   | High no BA / BA | 0.1559 / 0.0758 | 0.1667 / 0.0592 | 0.1560 / 0.0752 | 0.1636 / 0.0685 |
   | `L_0` no BA / BA | | −1.4% / −3.3% | | −0.9% / +0.2% |

3. 2024 tax units, AGI, and SPM (*Base year → 2024: ACS* has the background). `1b` reads `raw_data/ACS/v1/2024/ipums_usa.csv` (extract 36, config key `ACS-2024`) and needs a tax-unit crosswalk beside it, because IPUMS has not released `TAXID`, `ADJGINC`, or the six SPM variables for 2024.
   - Decided by Anna on 2026-11-09: 2024 runs use the Affordability-Index units and AGI (`Budget-Lab-Yale/Affordability-Index`). John Iselin has run its full chain for both years: residence-partition tax units (`02_tax_units.R`, pointer method, `mother_first` claims) and AGI from Tax-Data donor returns run through the pinned Tax-Simulator (5 implicates). Outputs, read in place: `ji252/repos/Affordability-Index/data/interim/income/<year>/` (`tax_units_<year>.parquet`, `files/B_m<k>_<year>_federal.parquet`, 2019 run 2026-10-12, 2024 run 2026-10-31). `src/misc/make_tax_unit_crosswalk.R` writes the 1b crosswalk: each person's unit, and the unit's AGI as the mean over the 5 implicates of its primary return (dependents' own returns left out). With `--check`, it reports whether any of John's files is newer than the crosswalk. The 2024 file is `agr55/ai1109/out/full/tax_unit_crosswalk_2024.csv` (3,238,311 persons, 1,758,184 units), not yet in the shared ACS folder. The 2024 1% smoke test passed on it (job 32703540). The team agreed that John Iselin's ACS processing for the affordability project (tax units, dependent-claiming rules, income adjustments) supplies the model's tax units, and he will send new files.
   - Open: 2019 still uses the Census crosswalk and `ADJGINC`, so the two years rank families on different AGI measures for the donor match. Switching 2019 too would make them consistent but changes 2019 results.
   - 2019 check against the Census crosswalk and IPUMS `ADJGINC` (weighted unless unweighted):

     | | Builder below, ACS AGI | Affordability-Index units, ACS AGI | Affordability-Index units and AGI |
     |---|---|---|---|
     | Persons in the same unit (unweighted) | 92.7% | 89.3% | 89.3% |
     | Parents of a child under 5 in the same unit | 91.5% | 88.9% | 88.9% |
     | Parents: AGI in the same decile | 98.3% | 98.6% | 69.1% |
     | Parents: AGI within $1,000 | 99.3% | 99.4% | 21.7% |

     The Affordability-Index AGI averages 14% above `ADJGINC` (parents and all persons). It is built from IRS donor returns aligned to IRS state totals, and `ADJGINC` is a Census model on ACS income, so the gap is a difference of measure, not an error in either. A unit's AGI varies across the 5 implicates by a median standard deviation of $1,406 (2019) and $1,757 (2024). One implicate ranks units with a Spearman correlation of 0.97 against the mean.
   - Provenance check, 2026-11-09 (`agr55/ai1109/prov.R`, `same.R`):
     - All 10 federal files (2 years × 5 implicates, arm B) ran on Tax-Simulator `4818d7b`, the commit in `config/income/tax_simulator_pin.csv`, under baseline law. Filers are 88.1% (2019) and 86.6% (2024) of returns.
     - The units are reproducible: the `tax_units` stage rerun on GitHub `main` (`1a3e512`) gives the same unit and the same role for every person in both years (3,087,291 and 3,238,311 persons).
     - The ECEC runs use John's current federal files (2026-10-31 for 2024, 2026-10-12 for 2019) as they stand. Pipeline changes and reruns belong to the Affordability-Index repo. Check them with `--check`, and rebuild the crosswalk when John updates them.
   - History: our own builder (2026-11-09, replaced the same day, in commit `d89994c`). It made the file from the household roster, ACS income, and IRS dependent tests. A first build on extract 36 (Bouchet job 32682313) had AGI from wages plus business income only, because extract 36 has no other income items (`agr55/nsece_evidence/taxunit1109/`). I then requested IPUMS extract 37 (2024): extract 36's variables plus `INCINVST`, `INCRETIR`, `INCSS`, `INCOTHER`, `INCTOT`, and `SCHOOL`. The builder adds investment, retirement, and other income, and taxable Social Security by the IRS provisional-income rule, to AGI when the file has them. A 19--23-year-old counts as a student if `SCHOOL` says in school, and by an earnings test (under $15,000) when the file has no `SCHOOL`. The ACS other-income item includes some untaxed money, such as child support, and cannot be split. Extracts 37 and 38 (2019, the same items plus `TAXID` and `ADJGINC`) are in `agr55/nsece_evidence/taxunit1109/ipums/`. Validation on 2019 against the corrected Census crosswalk and IPUMS `ADJGINC` (extract 38, job 32684861, earlier rules job 32682312, weighted unless unweighted):

     | | Earnings only, earnings student test | All income, earnings student test | All income, school student test |
     |---|---|---|---|
     | Persons in the same unit as Census (unweighted) | 90.8% | 90.8% | 92.7% |
     | Parents of a child under 5 in the same unit | 91.1% | 91.1% | 91.5% |
     | Parents: AGI in the same decile | 87.7% | 98.3% | 98.3% |
     | Parents: AGI within $1,000 | 83.4% | 99.3% | 99.3% |
     | All persons: AGI in the same decile | 49.6% | 87.3% | 88.7% |
     | Persons 65 and older: AGI in the same decile | 16.1% | 95.8% | 95.8% |
     | Mean AGI, ours over Census, all persons | 0.881 | 1.019 | 1.018 |

     The builder used the last column. Its final 2024 file, built from extract 37 on 2026-11-09, has 3,238,311 persons, 1,745,278 tax units, and 789,604 dependents (unweighted), in `agr55/nsece_evidence/taxunit1109/full/`. Census's own units with earnings-only AGI put 87.6% of parents in the same decile, so the earnings-only gap came from the missing income items, not the units. Married spouses present share a unit in 100% of cases (Census 99.05%). Most persons it placed in a different unit are adults 24--64 or children of the head. Other rule settings matched Census less well or the same (earnings student test, unweighted persons): students always dependents (89.7%), never dependents (90.5%), a $5,000 student earnings limit (90.6%), low-earning nonrelatives as dependents (88.5%), and dependents' income in AGI (90.8%, with all income it lowers parents in the same decile to 96.2%).
4. Decide whether to turn on John's `weights` and `supply` options (*The main choices*, `hours` is always on). Open. Measured on 2026-10-29 on the code of that day (`1a` per option, children under 5, child weights, children with no care category in the denominator, runs `19_new`, `24_new`, `24_caps2`, `24_supply2`, `24_wcps`, `24_wacs`):

   | | 2019 | 2024 now | `supply` | `supply`, rate deletion only |
   |---|---|---|---|---|
   | Parent Only | 50.91% | 45.31% | 45.31% | 45.31% |
   | Any paid care | 24.68% | 29.61% | 29.61% | 29.61% |
   | Low-priced residual per child-hour | +$0.47 | −$0.47 | +$0.71 | −$0.25 |
   | High-priced residual per child-hour | $3.32 | $4.56 | $4.57 | $4.60 |
   | Unpaid no-BA / BA staff per FTE child-hour | 0.0544 / 0.0624 | 0.0935 / 0.0759 | 0.1090 / 0.0872 | 0.0935 / 0.0759 |
   | `L_0` no BA / BA, billion staff-hours | 1.492 / 0.603 | 1.872 / 0.793 | 1.637 / 0.734 | 1.722 / 0.735 |

   - 2019 staffing and `L_0` include fixes 1 and 2. Fixes 1 to 3 change no care share in either year.
   - `supply` then did two things. The first deleted the 2024 censoring caps, as the 2019 file did (mismatch item 4). It removed 253, 309, 346, and 360 rates at the four ages, the same rates as run `24_caps2`. The second measures center staffing from the director's count. The rate deletion alone cut `L_0` by 8.0% (no BA) and 7.4% (BA). Both together cut it by 12.6% and 7.5%, so director staffing adds a cut of 4.6 points for staff without a BA and almost none for staff with a BA. With John's original percentile trim the Low-priced residual was +$0.86 and `L_0` 1.645 / 0.706.
   - `weights`, with the March 2024 CPS or the 2024 ACS as targets. The ACS factors use John's five groups and raking code on the 2024 ACS (`nsece_evidence/acsw` on Bouchet). On John's 2023 ACS they reproduce his factor range, 0.828 to 1.711. On the 2024 ACS the range is 0.805 to 1.802, against 0.695 to 1.232 for the CPS. Raked to the ACS, 62.9% of children under 5 with a parent have their caregiver at work, against 64.3% with the survey weights alone. Care shares in 2024, children under 5:

     | 2024 | No `weights` | `weights`, CPS | `weights`, ACS |
     |---|---|---|---|
     | Parent Only | 45.31% | 47.79% | 46.10% |
     | Any paid care | 29.61% | 28.64% | 29.21% |
     | Fall in Parent Only from 2019 | 5.6 points | 3.1 points | 4.8 points |
     | Rise in any paid care from 2019 | 4.9 points | 4.0 points | 4.5 points |
     | Home-based residual ($ per child-hour) | 3.86 | 4.09 | 3.97 |

   - The ACS targets move 2024 less than the CPS targets, because the ACS puts more mothers at work (62.9%) than the CPS (59.6%). The ACS is the survey the model calibrates to. `weights` changes no supply parameter except the 2024 home-based residual. Among children with a care category, the shares the care-choice forest trains on, the CPS targets cut the fall in Parent Only from 7.2 to 4.6 points and the rise in any paid care from 4.5 to 3.5 points.
5. Run a 1% smoke test on 2024 (no `-d` or `-c`). Done: it passed on the Affordability-Index crosswalk (job 32703540).
   - Record. On 2026-11-09 the 2019 smoke test passed end to end on the censored-rate fill, the new hours fill, and the admin drop (job 32680391, 8 min 42 s). Its `1a` counts match the 2026-11-06 `1a` runs exactly. The 2024 run (job 32680393) matched in `1a` and stopped in `1b` at the missing tax-unit crosswalk. Rerun the same day with our earnings-only crosswalk, read from a private copy of the input folder (job 32683121), the 2024 smoke test passed end to end. `1b` read the own crosswalk and turned off the poverty output for lack of SPM variables. It passed again on the builder's final crosswalk (all income items, school test, job 32687822, 6 min 30 s). The 2019 rerun with the armed-forces change also passed (job 32682315). Earlier, the 2019 smoke test passed on the admin-center code (jobs 31007018 and 31007019), and again on 2026-10-29 with fixes 1 to 3 (jobs 31281999, 31282000, and 31282001). The ACS-path change (now in `02f1495`) was tested on its own (jobs 31132240 and 31132400).
