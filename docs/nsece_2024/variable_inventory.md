# How the ECEC Simulator uses the 2019 NSECE: variable inventory

Code reviewed: `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/emp12`, branch `fix/point-in-time-employment`. Nothing was edited or run.

Survey files: `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1/2019` (ICPSR 37941). The model reads four of them, by hard-coded file name:

| Data set | File read | Rows | Read at |
|---|---|---|---|
| DS9 household | `2019/37941-0009-Data.tsv` | 8,576 households | `1a_nsece_processing.R:131-134` |
| DS6 center-based providers | `2019/37941-0006-Data.tsv` | 6,917 centers | `1a_nsece_processing.R:824-827` |
| DS7 home-based providers | `2019/37941-0007-Data.tsv` | 5,901 providers (4,231 listed, 1,670 unlisted) | `1a_nsece_processing.R:830-833` |
| DS5 workforce | `2019/37941-0005-Data.tsv` | 5,192 classroom staff | `1a_nsece_processing.R:836-839` |

File references below give the file name and line number. All files are under `src/` in the repository above: `1_processing/1a_nsece_processing.R`, `1_processing/1b_acs_processing.R`, `2_calibration/2a_calibration.R`, `3_simulation/3a_initialize_simulation.R`, `3_simulation/3b_run_simulation_year.R`, `3_simulation/3c_run_scenario.R`, and `shared_functions/*.R`.

Definitions and codes in the table come from the 2019 user guides (text extracted from the PDFs in the same folder). Counts marked "data" were tabulated from the .tsv files by reading only the needed columns.

Where to find what:

- Section 1 has one row for each variable or family of variables.
- Section 2 describes how children are assigned to care categories, and how each category's price is set.
- Section 3 describes how 2019 values are carried forward to 2024, 2025 and 2026.
- Section 4 lists hard-coded numbers that depend on 2019 survey facts.
- Section 5 lists other facts a second pass should check.

---

## 1. Variable table

The NSECE uses negative "reserve codes" for structural blanks and for missing answers. There is no common scheme across variables. The same number can mean "no such child", "valid skip", "don't know" or "not defined for this provider type", depending on the variable. The model has two general recoding helpers:

- `nsece_admin_classify` / `recode_admin_na` (`1a_nsece_processing.R:23-76`) set -1 and -8 to missing.
- The arrangement-level status loop (`1a_nsece_processing.R:589-619`) works as follows:
  - A value of 0 or above is kept.
  - A value from -1 up to just below 0 is set to missing. For HH9_REGCARE only, the cutoff is -2, so -1 and -2 are both set to missing.
  - Anything more negative is treated as "not in universe" and **set to 0**.

### 1a. Household survey (DS9)

| Exact name(s) | 2019 definition, universe and codes (user guide) | Model codes and recodes (file:line) | What it feeds | Sensitivity to a change in definition |
|---|---|---|---|---|
| `HH9_METH_CASEID` | Household case ID. | Renamed `hh_id` (`1a:165` and elsewhere). | Join key only. | Low. Only the name matters. |
| `HH9_METH_WEIGHT` | Household weight. Sums to US households with a child under 13 in 2019. Range 0 to 33,184. | Kept as `hh_weight` (`1a:166`). Written out but **never used** downstream. | Nothing. | Low. |
| `HHC9_METH_WEIGHT_1`..`_9` | Child weight. Sums to US children under 13 in 2019 (52.6 million). Range 0 to 33,184. Blank when the child does not exist. | Found by regex `^HHC9_METH_WEIGHT_[1-9]$` (`1a:181`) and becomes `child_weight`. | Weights for every household-side statistic. These include the sampling weight for the enrollment random forest (a machine-learning classifier fitted to the NSECE and used to predict care choices for ACS children; `1b:1155-1160`), the price-heterogeneity models (`1a:1386`), the 1st to 99th income-percentile cut points (`1a:1332-1336`, `1b:94-98`), the paid home-based price (`1a:1150`) and the "Other Paid" base price (`1a:1392`). | Medium. Every household-side average and share depends on it. A change in the universe (for example, children under 13 versus under 6) does not matter much, because the model keeps only children under 5. |
| `HHC9_AGE_1`..`_9` | Child's age **in months** at the time of questionnaire Section A, from birth month and year. Range 0 to 162. -2 means no Child X. | Regex `^HHC9_AGE_[1-9]$` (`1a:177`). Rows with age below 0 are dropped (`1a:216`). Age in years is `floor(months/12)` (`1a:220`). | Defines the **under-5 universe** (`age < 5`) everywhere: `1a:503, 1148, 1330, 1348`, `1b:92, 260`. Also a feature in the enrollment random forest and the price models. | High. If 2024 reports age in years, or measures it at a different date, the under-5 universe and the age features change. |
| `HHC9_AGE_AT_SEP1_18_1`..`_9` | Child's age in months before September 1, 2018. -3 and -2 are reserve codes. | Regex `^HHC9_AGE_AT_SEP1_[0-9]+_[1-9]$` (`1a:179`). Negative values set to missing (`1a:221-222`). | Written out only. **Not used.** | Low for results. **But the code stops with an error if no matching column exists**: the code at `1a:221` refers to `age_months_sep1` unconditionally. |
| `HHC9_GENDER_1`..`_9` | Child's sex: 1 = boy, 2 = girl, -1 = don't know/refused, -9 = no child. | 1 becomes male = 1, 2 becomes 0, anything else missing (`1a:210-214`). | Feature in the enrollment random forest (`1b:1127-1134`). Children with missing sex are dropped from training (`1b:412, 420`). | Low. |
| `HH9_HHM_AGE_1`..`_9` | Age in years of household member X. Range 1 to 80 (80 looks like a top code, but the guide does not say so). -9 = no member, -2 = valid skip, -1 = don't know/refused. | Regex `^HH9_HHM_AGE_[1-9]$` (`1a:229`). Rows kept if age > -9 (`1a:242`). Negative ages set to missing (`1a:245`). Member number taken from the **last character** of the name (`1a:244`). | Parent ages: `p.age1` for one-parent units; caregiver and partner ages for two-parent units (`1b:285-296, 337-338`). These are random-forest features. | Low to medium. |
| `HH9_PARCH_X_Y` (X = child 1-9, Y = member 1-9; 81 variables) | Flag that member Y is a parent of child X. 1 = parent, 0 = not a parent, -1 = don't know/refused, -2 = not asked, -7/-8/-9 = structural blanks. | Regex `^HH9_PARCH_[1-9]_[1-9]$` (`1a:260`). Keeps `is_parent == 1` (`1a:274`). Parents are ranked by member number. **Columns for a 3rd and 4th parent are dropped** (`1a:291`). The unit keeps its first two parents; the comment says these units are "dropped", but the code does not drop them. `n_parents` = 1 or 2 (`1a:298`). | Defines **parent units** (sets of children who share the same parent or parents), number of parents, the child-to-parent link, and family size by children under 5 (`1a:502-511`). | High (structure). Data: 20 children under 5 have 3 flagged parents and 3 have 4. **`select(-hhm_id3, -hhm_id4)` stops with an error if no child in a new file has 3 and 4 flagged parents.** |
| `HH9_RGENDER` | Respondent's sex: 1 = male, 2 = female, 3 = don't know/refused, -4 = valid skip. Data: 1,912 / 6,605 / 6 / 53. | `if_else(RGENDER != 3, as.integer(RGENDER == 1), NA)` (`1a:154, 307`). **Code -4 becomes female (0).** Applied only when parent 1 is member 1, the respondent (`1a:303-311`). | Parent 1's sex. This decides which parent is the "primary caregiver": the mother in a two-parent unit (`1b:35-55, 334`). The primary caregiver's employment state is a random-forest feature. For one-parent units, `p.male1` is a feature. | Medium. Parent units whose first parent is not the respondent have missing `male1`. Units of that kind with one parent are dropped from training (`1b:459, 464`). |
| `HH9_HHCOMP_NUMMOTHER`, `HH9_HHCOMP_NUMFATHER` | Number of members who are a mother (or father) to any child in the household. Range 0 to 3. | Used to infer parent 2's sex. If the respondent is the father, there is exactly 1 father and at least 1 mother, then parent 2 is a mother; the reverse case also applies. Otherwise missing (`1a:319-334`). | Parent 2's sex, which feeds the choice of primary caregiver (`1b:334`). | Medium. Exists because the public file gives only the respondent's sex. |
| `HH9_HHCOMP_MEMBERS` | Number of household members, all ages. Range 2 to 15. -1 = don't know/refused (18 households in the data). | Renamed `n_members` (`1a:167`). **-1 is not recoded** and enters the model as -1. | Random-forest feature (`1b:1127-1134`). | Low to medium. The ACS counterpart is `n()` persons (`1b:625`). |
| `HH9_RSPOUSEPARTNER_LOOP` | Roster number of the respondent's spouse or partner (2-8). -1 = none in household, -2 = no household members identified. | Becomes `resp_has_partner` and `resp_partner` (`1a:155-160`). | Written out only. **Not used.** | Low. |
| `HH9_REGION` | Census region: 1 = Northeast, 2 = Midwest, 3 = South, 4 = West. Based on the sampling state. | Becomes `region` (`1a:162`) and is turned into a factor (`1a:1362`, `1b:318`). The factor levels are stored with the price models (`1a:122`). | Feature in the enrollment random forest and in all four price models. | Low, if the codes stay 1-4. |
| `HH9_ECON_INCOME_ANNUAL` | Household income for calendar year **2018**, adjusted to a before-tax level. Top-coded: the top 1% are replaced by the median of the top 1%. Range 0 to 371,066.50. | Becomes `income` (`1a:168`). Converted to a child-weighted 1-100 percentile among children under 5 with income > 0, using a small random jitter (`1a:1328-1338`, `1b:76-101`). | The income-percentile feature in the enrollment random forest and the price models. The ACS side ranks ACS income (enrollment) or baseline AGI (price models, `compute_price_wedge_cache.R:79-90`) within the ACS. | Medium. Only ranks are used, so the dollar level and inflation do not matter. Changes in income concept, reference year or top-coding matter only through ranks. |
| `HH9_WST_PARCH_HI_1`..`_9`, `HH9_WST_PARCH_LO_1`..`_9` | Member number of the child's parent with the most (HI) and second-most (LO) hours in work-related activities (work, school, training, commuting) last week. -7 = no child, -8 = no parent in household, -9 = only one parent (LO only), -10 = a parent has a missing calendar block. | Regex (`1a:337-338`). Reshaped. Records with member number ≤ 0 or hours < 0 are dropped (`1a:364`); the comment says "12 records", which matches the 11 + 1 records coded -10. | Parent work hours by member (see next row). | High. See next row. |
| `HH9_WST_HRS_TOT_PHI_1`..`_9`, `HH9_WST_HRS_TOT_PLO_1`..`_9` | Total hours in work, school, training and commuting last week for the HI or LO parent, from the adult calendar (summer cases: equivalent items). Range 0-168 (HI) and 0-102 (LO). Same reserve codes as above. Guide names them `HH9_WST_HOURS_TOT_*` in one list and `HH9_WST_HRS_TOT_*` in the entries; the data use `HRS`. | Regex `^HH9_WST_HRS_TOT_PHI_[1-9]$` / `PLO` (`1a:339-340`). Becomes `hours1` / `hours2` (`1a:427-437`), after the D1a adjustment in the next row. | Primary caregiver's employment state: none if hours are 0 or missing; part-time below **35**; full-time at 35 or more (`1b:57-74`). This is a random-forest feature. The forest's predictions under all three employment states become each ACS family's care probabilities by employment state (`1b:1207`), which drive the calibration. | High. The care-by-employment pattern is identified from this variable. |
| `HH9_D1A_WORK_1`..`_9` | "Last week, did you/HHM do any work for pay?" 1 = yes, 2 = no, 3 = don't know/refused. Blank when there is no such member. | Regex (`1a:376`). Only codes 1 and 2 are kept (`1a:388`). Code 2 sets hours to 0. Code 1 with 0 calendar hours gets hours drawn at random from working parents with the same earner rank (HI or LO), using seed `random_seed_base + 7` (`1a:396-411`). Otherwise calendar hours are used. | Same as previous row. | High (same reason). The rule was added on this branch to match the ACS "at work in the reference week" concept. |
| `HH9_D3D_WAGE_TC_1`..`_9`, `HH9_D3D_UNIT_1`..`_9` | Pay at main job (top-coded by pay unit: top 1% or top 10 cases replaced by their median). -4 = don't know/refused. Unit: 1 hour, 2 day, 3 week, 4 two weeks, 5 month, 6 year, 7 other, 8 don't know/refused. Asked only of parents (or a parent's spouse) who answered "yes" at D1a. | Pay ≤ 0 set to missing. Units 7 and 8 set to missing. Converted to hourly pay: day ÷ 8, week ÷ 40, two weeks ÷ 80, **month ÷ (2000/12)** (the comment says 2080/12), year ÷ 2080 (`1a:440-484`). | Written to `nsece_parent_units_2019.csv` only. **Not used.** | Low. |
| `HH9_CHILDREN_X_Y` (child 1-9 × provider 1-14) | Whether provider Y cares for child X: 1 = yes, 0 = no, -9 = no such child or provider. | Regex `^HH9_CHILDREN_[1-9]_(?:[1-9]\|1[0-4])$` (`1a:522`). Keeps rows equal to 1 (`1a:571`). | Defines the set of arrangements (child-provider pairs). | High (structure). |
| `HH9_REGCARE_X_Y` | Whether provider Y has given child X at least 5 hours of care at some point. 1 = yes (regular), 0 = not regular (< 5 hours), -1 = don't know/refused, -2 = unable to determine, -6 to -9 = structural blanks. | Status loop with cutoff -2: -1 and -2 set to missing; below -2 set to 0 (`1a:592`). Only `regular == 1` counts toward a child's care category (`1a:691`). The price models also keep `regular == 1` (`1a:1342`). | Care-category assignment and price models. | High. A change in the 5-hour rule changes who counts as using care. |
| `HH9_TYPEOFCARE_AGG_X_Y` | Derived type of care for each child-provider pair (guide Exhibit 5.1, data codebook p.192). 1 = individual, no prior relationship, regular, paid. 2 = individual, prior relationship, regular, paid. 3 = individual, unpaid, regular. 4 = center-based ECE (Head Start, public pre-K, community child care; children under 72 months). 5 = other organizational ECE, regular. 6 = K-8 school. 7 = irregular ECE (< 5 hours a week). 8 = regular, setting unknown. -6 to -9 = structural blanks. | Becomes `type_raw` (`1a:579`). Code 6 = K-8, code 4 = center, codes 1-3 = individual (`1a:630-632`). Codes 5, 7 and 8 fall into "Other" (`1a:645`). | The main care-type classification (Section 2). | High. Any renumbering or regrouping (for example, a different split of Head Start or pre-K, or of the paid categories) changes every category. |
| `HH9_CARELOC_X_Y` | Whether provider Y cared for child X in the respondent's home. 1 = in home, 0 = not in home, -1 = don't know/refused, -2 = not defined for this provider type (organizations), -6 to -9 = blanks. | Status loop: -1 set to missing; **-2 and below set to 0** ("not in child's home") (`1a:597`). `in_child_home = careloc == 1` (`1a:641`). | Splits individual care (types 1-3) into "home-based" (outside the child's home) and "Other" (in the child's home). | High for the paid home-based sector. |
| `HH9_PRIORREL_1`..`_14` (provider level) | Whether provider Y had a prior relationship with the respondent. 1 = yes, 0 = no, 2 = inferred no relationship (individual who runs family day care), -1 = don't know/refused/skip, -9 = no provider. | Regex `^HH9_PRIORREL_(?:[1-9]\|1[0-4])$` (`1a:533`). Status loop (`1a:598`). `has_prior_rel = priorrel == 1` (`1a:642`), so code 2 counts as no relationship. | With the location variable, decides "home-based" versus "Other". | High. Data: every type 2 arrangement has PRIORREL = 1, so type 2 can never be paid home-based. |
| `HH9_HRS_WEEK_PROV{Y}_{X}` (provider first, then child; 126 variables) | Total hours child X spent with provider Y in the **reference-week calendar** (summer cases: a typical week in May). Range 0-168. **No reserve codes**: 0 covers both no care and no calendar entry. Providers who are "regular but not last week" have 0 hours. | Renamed to `HH9_HRS_WEEK_PROV_{child}_{provider}` (`1a:142-145`). A second rename for `HH9_HOURS_WEEK_PROV…` (`1a:146-149`) matches nothing in the 2019 file. Becomes `hours` (`1a:584`). | (a) An arrangement counts only if hours > 0 (`1a:693`). (b) Hours are summed by category (`1a:696`). (c) Child hours: 0 = none, under **20** = part-time, otherwise full-time (`1a:727-732`). (d) Hourly price = weekly payment ÷ hours (`1a:626`). | High. Data: hours are 0 for 2,333 of 5,138 type 3 arrangements and 138 of 1,667 type 4 arrangements. Those arrangements drop out of the classification. |
| `HH9_WEEKLY_PAYMENT_TC_X_Y` | "Top coded payments paid to Provider Y by household for care of Child X": direct charges paid by household members. 0 = free or no cost to the household; positive = dollars (data: up to 675). -1 = no amount (don't know/refused/missing), -2 = ineligible for Section J, -6 to -9 = blanks. A separate variable, `HH9_WEEKLY_COST_CARE_TC`, adds subsidies; the model does not use it. | Becomes `net_weekly_cost` (`1a:585`). Status loop: **-1 set to missing; -2 and below set to 0** (`1a:595`). `paid = net_weekly_cost > 0` (`1a:625`). Missing payment makes `is_paid` FALSE (`1a:635`). | Paid versus unpaid for every category. The hourly out-of-pocket price, which decides the **$4.75 low/high center split** (`1a:636-637`). The paid home-based price (`1a:1140-1151`). The "Other Paid" base price (`1a:1390-1393`). The price-wedge targets (`1a:1385`). | High. **Effects in the 2019 data:** (1) Types 6, 7 and 8 always carry -2, so the model always treats them as unpaid. (2) Arrangements with an unknown amount (-1) are treated as unpaid: 214 type 4, 107 type 1, 347 type 2 and 106 type 5 arrangements. The type 1 cases become "Unpaid Home-Based" although type 1 is by definition paid. |
| `HH9_SUBSIDY_X_Y` | Subsidy amount paid directly to the household for this arrangement. -4 = valid skip, -2 = ineligible, -1 = missing. | Status loop (`1a:596`). | Written out only. **Not used.** | Low. |
| `HH9_HEADSTART_X_Y`, `HH9_PUBPREK_X_Y` | Whether provider Y was a Head Start (or public pre-K) provider for child X. 1/0, -5 and others are reserve codes. | Status loop (`1a:593-594`). | Written out only. **Not used** in classification. | Low. |

### 1b. Center-based provider survey (DS6)

| Exact name(s) | 2019 definition, universe and codes | Model codes and recodes (file:line) | What it feeds | Sensitivity |
|---|---|---|---|---|
| `CB9_METH_CASEID` | Center ID. Also present in the workforce file, so staff can be linked to their center. | Becomes `id` (`1a:853, 958, 1050`). | Links centers, classrooms and workers. | Low (name only). |
| `CB9_METH_WEIGHT` | Center weight. Sums to US centers serving children 0-5 not yet in kindergarten. Range 1 to 461.6. | Becomes `center_weight` (`1a:854`). Averaged within age bin (`1a:898`). | The weight for the median split, which is center weight × FTE enrollment (`1a:936`). Sector prices (`1a:1109`). Sector FTE enrollment, which sets the 2019 labor stock L_0 (`1a:1110, 1240-1248`). | High. |
| `CB9_ENRL_{INF,1YR,2YR,3YR,4YR,5YR}` | Children enrolled by age: under 12 months, 1, 2, 3, 4, and 5 not yet in kindergarten. Age groups reported by the center are mapped to these ages, assuming enrollment is spread evenly across each group's age range. Rounded. **Top-coded**: the top 2% within each age are replaced by the median of the top group (data: 32 for infants, 31 for 5-year-olds). | Regex `^CB9_ENRL_(INF\|1YR\|2YR\|3YR\|4YR\|5YR)$` (`1a:844`). An enrollment of 0 marks that age's price as "not in universe" (`1a:870`). Summed into three age bins: infant and 1 into 0-1; 2 and 3 into 2-3; 4 and 5 into 4-5 (`1a:889-893, 901`). | Enrollment in full-time-equivalent (FTE) terms (next row). FTE enrollment weights the sector prices, and sector FTE enrollment builds the labor ratios and L_0. | High. |
| `CB9_ENRL_FT_{same ages}` | Percent enrolled full-time, 0-100. Capped at 100. -2 = age not served, -4 = not enough information. The center defines "full-time". | Regex (`1a:845`). Within each bin: if all values are negative, 0; otherwise the mean of the non-negative values ÷ 100 (`1a:904-908`). FTE = enrollment × share + **0.5** × enrollment × (1 − share) (`1a:921`). | FTE enrollment. | Medium. |
| `CB9_STDRATE_WKLY_{INF,2YR,3YR,4YR}` (no 1-year or 5-year version) | Weekly rate charged for **full-time** enrollment, excluding subsidies and discounts. Standardized to weekly: hourly × 40, half-day × 10, full day × 5, monthly ÷ 4.34524, and so on. Public file: the reported rate was **truncated at the 5th and 95th percentiles within each time unit** and those cases coded **-2 "Trimmed value"**. -9 = no rate for this age or families do not pay. -1 = not enough information. Range 0 to **146,000**. | Regex `^CB9_STDRATE_WKLY_(INF\|2YR\|3YR\|4YR)$` (`1a:846`). Status codes (`1a:868-876`): missing value or enrollment 0 = not in universe; rate flag -8 = free; rate flag 2 = no rate; -1 or -2 = missing; everything else OK. Hourly price = weekly ÷ **40** (`1a:880`). Free = $0. Within a bin, the mean of non-missing prices (`1a:912-915`), so the 1-year-old bin uses the infant price and the 5-year-old bin uses the 4-year-old price. | Center classification into unpaid, low-priced and high-priced (age-bin weighted median, `1a:927-952`). Sector **2019 supply prices** (`1a:1109, 1123, 1255-1257`). Per-unit residual delta_0 = price − labor cost (`1a:1133`). Base prices for the price wedges (`1a:1322-1323, 1385`). | High. **No trimming in the model.** In 2019, 32 center-age-bin records have posted rates above $50 an hour (over $2,000 a week; maximum $3,650 an hour). Recomputing the model's high-priced center price gives **$8.08 with them and $6.72 without them**. The low-priced price ($3.31) is unaffected. Public-file trimmed cases (-2) are treated as missing. |
| `CB9_B1_3A_AGERATE_{INF,2YR,3YR,4YR}_R` | "Does your program have a rate for full-time care for [age]?" 1 = yes, 2 = no, 3 = don't know/refused, **-8 = no families pay**. | Regex (`1a:847`). -8 = free (price 0). 2 = no rate (missing). 3 is not handled directly; it always comes with a rate of -1, so it ends up missing. | Defines the **unpaid center sector** on the supply side (price = 0). | High. |
| `CB9_F3_YOUNGMONTHS_R` | Age in months of the youngest child in the **randomly selected classroom group**. Top-coded at 157 or more. -1 and -8 (admin-only completion) are reserve codes. | -1 and -8 set to missing (`1a:969-972`). Age bin from `floor(months/12)`: 0-1, 2-3, 4-5; otherwise missing (`1a:981-986`). | Links each workforce respondent (who works in that classroom) to an age bin, and through it to the center's price category in that bin (`1a:1077-1094`). | Medium to high. This decides which sector's labor ratio a worker counts toward. |
| `CB9_WEIGHT_CLSM` | Classroom weight. | Becomes `classroom_weight` (`1a:959`). Joined at `1a:1080` but **never used** in a calculation. | Nothing. | Low. |
| `CB9_F3_ENROLL_TC`, `CB9_F3_TEACH_TC`, `CB9_F3_ASSIST_TC` | Children, lead teachers and assistants in the selected group. Top-coded at the 98th percentile. -1 and -8 are reserve codes. | -1 and -8 set to missing (`1a:969-972`). `child_per_staff` (`1a:978`). | `child_per_staff` is **never used**. | Low. |
| `CB9_F15_CHILD_FUND_HS_IND`, `CB9_F16_CHILD_FUND_PK_IND` | Any child in the classroom funded by Head Start (or public pre-K). 1/0, -1 = don't know/refused, -8 = admin-only. | -1 and -8 set to missing (`1a:969-972`). | **Not used.** | Low. |

### 1c. Workforce survey (DS5)

| Exact name(s) | 2019 definition, universe and codes | Model codes and recodes | What it feeds | Sensitivity |
|---|---|---|---|---|
| `WF9_METH_CASEID`, `CB9_METH_CASEID` | Worker ID and the ID of the center that produced the worker. Up to two workers per center. | `worker_id`, `id` (`1a:1050-1051`). | Link from workers to centers and classrooms. | Low (name only). |
| `WF9_METH_WEIGHT` | Worker weight. Sums to lead teachers, teachers, assistants and aides working at least 5 hours a week in centers serving children 0-5 in 2019. | Becomes `worker_weight` (`1a:1052`). | Weighted mean wages by education (`1a:1069`). Weighted FTE worker counts by sector (`1a:1097-1098`). | High. |
| `WF9_WORK_WAGE` | Hourly wage (pay amount, pay unit and hours combined; some parts imputed). **Top-coded** within urban/rural, auspice and education cells. Range $1.29 to $107.50. -1 = not enough information, -8 = admin-only center. | Wages above 0 kept; -1 and -8 set to missing (`1a:997-1002`). | **Base wages w_0 by education** (`1a:1061-1073`): $12.06 without a BA and $19.70 with a BA in run `202610161556`. These feed unit labor cost, delta_0, and the equilibrium wage path. | High. |
| `WF9_WORK_WAGE_IMP` | Imputation flag for the wage. 0 = not imputed, 1-7 = imputation methods, -8 = admin. | Capped at 1; -8 set to missing (`1a:1005-1010`). | **Not used.** Imputed wages are kept. | Low. |
| `WF9_WORK_ROLE` | Role reported by the center: 1 = aide or assistant teacher, 2 = teacher, instructor or lead teacher, 3 = other or undetermined. -1 and -8 are reserve codes. The 2019 codes merge 2012's five categories into three. | Keeps roles 1 and 2 (`1a:1013-1018, 1066, 1089`). | Who counts in wages and labor ratios. | Medium. |
| `WF9_CHAR_EDUC` | Highest education: 1 = less than high school (2012's codes 1 and 2 merged), 3 = GED, 4 = high school graduate, 5 = some college, 6 = associate degree, 7 = BA, 8 = graduate degree. -1 and -8 are reserve codes. | 7 or 8 = BA (1). 1, 3, 4, 5 or 6 = no BA (0) (`1a:1022-1032`). | The **two labor types** (BA and no BA). Base wages, labor requirements per child-hour, L_0. | High. |
| `WF9_WORK_FT` | Usually works full-time (35 hours or more) = 1, part-time = 2. -1 and -8 are reserve codes. | 1 counts as 1 FTE, 2 as **0.5** FTE (`1a:1035-1045`). | FTE worker counts in the labor ratios. | Medium. |

### 1d. Home-based provider survey (DS7)

| Exact name(s) | 2019 definition, universe and codes | Model codes and recodes | What it feeds | Sensitivity |
|---|---|---|---|---|
| `HB9_METH_CASEID` | Provider ID. | `hb_id` (`1a:1155`). | Nothing beyond the ID. | Low. |
| `HB9_METH_WEIGHT` | Provider weight. Sums to US home-based providers, listed and unlisted separately. | Kept as `hb_weight` (`1a:1156`) but **not used**. The totals at `1a:1189-1193` are **unweighted sums**. | Nothing as written. | Medium to high. The home-based labor ratios are unweighted sample ratios. Home-based enrollment enters L_0 as an unweighted count of **32,097 children**; the weighted figure would be about 710,000. |
| `HB9_METH_SAMPLE_SOURCE` | 1 = unlisted, 2 = listed (on state or national administrative lists). | Keeps `== 2` only (`1a:1178`). 3,855 listed providers survive the filters. | Defines the paid home-based supply sector as listed providers. | High. |
| `HB9_PRGM_PAID` | 1 = paid for at least one child, 0 = unpaid, -1 = don't know/refused. | Selected (`1a:1158`). **Not used** as a filter; the comment at `1a:1176-1177` explains why. | Nothing. | Low. |
| `HB9_ENRL_TOTAL` | Number of children of **all ages (0-13)** cared for regularly (5 or more hours a week). **Top-coded at 20 for listed providers** (8 for unlisted). Values of 4 or fewer are grouped together for listed providers. -1 = don't know/refused. | Keeps values of 0 or more (`1a:1167`). Treated as FTE enrollment directly (`1a:1198`). | Home-based labor requirements per child-hour (`1a:1201-1202`). Home-based share of L_0. | High. Note the mismatch with the center side, which counts children under 6 in FTE terms. |
| `HB9_CAREER_EDUC` | Respondent's education: 1 = less than high school (merged), 3 = high school or GED, 4 = some college, 5 = associate degree, 6 = BA, 7 = graduate degree. -1 = don't know/refused. | Keeps values above 0 (`1a:1168`). BA if **6 or higher** (`1a:1184`). | Home-based BA labor requirement. | High. |
| `HB9_H2_NUMHELP` | Number of people who helped last week. Range 0-9. -2 = valid skip (no outside help), -4 = don't know/refused. | Drops -4 (`1a:1169`). Sets -2 to 0 (`1a:1182`). Workers = 1 + helpers (`1a:1183`). | Home-based labor requirements. | High. |
| `HB9_STAFF_EDUC_PRCNT_4YR` | Percent of helpers with a 4-year degree (capped at 100). -2 = no helpers, -1 = don't know/refused, -3 = don't know whether anyone helps. | Drops -1 and -3 (`1a:1170`). BA helpers = helpers × percent ÷ 100 (`1a:1184`). | Home-based BA labor requirement. | Medium. |

---

## 2. Care-choice categories and their prices

### 2.1 Categories in the model

Each child under 5 in an ACS family is assigned one of 15 child care choices (`get_choice_catalog.R:28-46`):

- seven care types, each at part-time or full-time hours;
- "Parent Only", which has no hours.

The seven care types are high-priced center, low-priced center, unpaid center, paid home-based, other paid, unpaid home-based, and other unpaid.

The model then combines the child choices with three employment states for the primary caregiver: not working, part-time (under 35 hours), and full-time (`get_choice_catalog.R:48-52`). This gives 45 choices for one-child families and 675 for families with two or more children (only the two youngest children under 5 are modeled).

Only four care types are **market sectors** with prices set in equilibrium: unpaid center (price fixed at 0), low-priced center, high-priced center, and paid home-based (`get_choice_catalog.R:62-67`, `constants.R:43-47`).

In the model, part-time means **10 hours a week** and full-time **40 hours a week**, for **50 weeks** a year (`constants.R:28-40`). The NSECE hours themselves are used only to put a child into the part-time or full-time group.

### 2.2 How an NSECE child is assigned a category (household file)

This is done in `1a_nsece_processing.R:519-741`, one arrangement (child-provider pair) at a time:

1. **Arrangement universe.** Provider Y counts for child X when `HH9_CHILDREN_X_Y == 1` (`1a:571`).
2. **Missing-value recodes.** These follow the status loop described at the start of Section 1 (`1a:589-619`). The important consequences:
   - An unknown payment amount (-1) becomes missing.
   - "Ineligible for Section J" (-2) becomes 0, meaning unpaid.
   - "Location not defined for provider type" (-2) becomes 0, meaning not in the child's home.
3. **Paid or unpaid.** An arrangement is paid if the weekly payment is above 0 (`1a:625`). Both a 0 and a missing payment count as unpaid (`1a:635`).
4. **Hourly out-of-pocket price.** Weekly payment ÷ weekly calendar hours, when hours are above 0 (`1a:626`). If hours are 0 the price is marked missing (`1a:627`).
5. **Provider group from the type-of-care code** (`1a:630-645`):
   - Code 6 is **K-8** school.
   - Code 4 is a **center**.
   - Codes 1-3 are an **individual**.
   - An individual whose care is not in the child's home (`careloc != 1`) and who has no prior relationship with the respondent (`priorrel != 1`) is **home-based**.
   - Everything else that is neither a center nor K-8 is **other**: codes 5, 7 and 8, and codes 1-3 in the child's home or with a prior relationship.
6. **Arrangement category** (`1a:647-658`):
   - Center, not paid: **Unpaid Center-Based**.
   - Center, paid, hourly price **under $4.75**: **Low-Priced Center-Based** (`1a:636`).
   - Center, paid, hourly price **$4.75 or more**: **High-Priced Center-Based** (`1a:637`).
   - Center, paid, but no usable hourly price: missing.
   - Home-based, paid: **Paid Home-Based**. Home-based, not paid: **Unpaid Home-Based**.
   - Other, paid: **Other Paid**. Other, not paid: **Other Unpaid**.
   - K-8: **K-8**. Under-5 children never have this code, because the type of care reserves code 6 for children 60 months or older.
7. **Child's primary category** (`1a:676-724`):
   - Keep the arrangements with `regular == 1`, a known category, and hours above 0.
   - Add up hours by category.
   - Pick the highest-priority category in this order: K-8, High-Priced Center, Low-Priced Center, Unpaid Center, Paid Home-Based, Other Paid, Unpaid Home-Based, Other Unpaid (`1a:665-674`).
   - The comment says hours break ties. They never do, because there is already only one row per category after the hours are added up.
   - The child's hours are the total weekly hours in the chosen category.
   - A child with no provider rows is **Parent Only**.
   - A child whose provider rows all have a missing regularity flag or category is **missing** and is dropped from training (`1b:269`).
   - A child with provider rows but none usable (all irregular, or all with 0 hours) is **Parent Only**.
8. **Hours group** (`1a:727-732`): 0 hours is "none", **1-19.99 hours is part-time**, **20 or more is full-time**.

What these rules do with the 2019 codes (data tabulations, all ages):

- Type 7 (irregular) always has REGCARE = 0, so it never counts.
- Types 6, 7 and 8 always have payment -2, so type 8 arrangements (197 of them regular) always become **Other Unpaid**.
- Type 3 is never paid, so it is always unpaid home-based or other unpaid.
- Type 2 always has a prior relationship, so it is always other paid or other unpaid, never paid home-based.
- Type 5 (other organizational care such as after-school programs, drop-in care and lessons) is always "Other".
- Providers who are "regular but not last week" have 0 calendar hours, so they do not count. A child whose only provider is like that becomes Parent Only.

**How the NSECE categories reach the ACS.** A random forest (`ranger`, a classification-tree ensemble: 500 trees, minimum node size 10, 50,000 rows sampled with probability proportional to the child weight) is trained separately for:

- one-child and two-child families (the two youngest children under 5);
- families with zero, one or two parents.

It is trained on NSECE children under 5 (`1b:1085-1198`). The outcome is the joint "category_hours-group" class.

The features are (`1b:1125-1136`): child age and sex, income percentile, household size, region, parent ages, parent 1's sex (one-parent units only), the primary caregiver's employment state, and an indicator for three or more children.

The model then predicts each ACS child's class probabilities under each of the three employment states (`1b:1207`). Children beyond the second in families of three or more children get one-child predictions (`1b:1235-1340`). These probabilities, not the NSECE records themselves, enter calibration.

### 2.3 How a center is classified on the supply side (center file)

This is done in `1a_nsece_processing.R:843-953`, one center and age bin at a time:

- The posted weekly full-time rate (`CB9_STDRATE_WKLY_*`) is divided by 40 to give an hourly price.
- "No families pay" gives a price of $0.
- No rate for this age, an unknown rate, or a rate trimmed in the public file gives a missing price.
- Within each age bin (0-1, 2-3, 4-5), the model computes the **weighted median** of positive prices, weighted by center weight × FTE enrollment (`1a:927-939`).
- A price of $0 is **unpaid center**. A price below the bin's median is **low-priced**. A price at or above the median is **high-priced** (`1a:945-950`).

Recomputed from the 2019 file, the medians are **$5.00 an hour (ages 0-1), $4.44 (ages 2-3) and $4.00 (ages 4-5)**.

The household-side cut of **$4.75** is a separate fixed number. In the 2019 household data it sits near the child-weighted median of paid, regular center out-of-pocket hourly prices for children under 5 ($4.90; 48% of children are below $4.75).

So the two "low-priced" and "high-priced" labels are defined differently:

- On the household side, a family is low- or high-priced by **its own out-of-pocket payment per hour**.
- On the supply side, a center is low- or high-priced by its **posted full-time rate**, compared with other centers of the same age group.

### 2.4 The price of each category

| Category | 2019 base price | Source and rule | In simulation years |
|---|---|---|---|
| Unpaid Center-Based | $0 | Fixed (`2a:1822`; sector 1 in `constants.R:85`). | Stays $0. |
| Low-Priced Center-Based | $3.31 an hour (run `202610161556`; reproduced exactly from the 2019 center file) | **Center file.** Weighted mean of posted hourly rates (posted weekly rate ÷ 40) for center-age-bin records classified low-priced. Weights are center weight × FTE enrollment within each bin, then FTE across bins (`1a:1103-1123`). | Set in equilibrium: P = labor cost at equilibrium wages + delta_0 × CPI-U growth since 2019 (`3c:125-283`). |
| High-Priced Center-Based | $8.08 an hour ($6.72 if the 32 records above $50 an hour are excluded) | **Center file.** Same rule. | Set in equilibrium. Example: $10.50 in 2026 (run `202610151554`). |
| Paid Home-Based | $5.51 an hour | **Household file.** Child-weighted mean of the hourly out-of-pocket price (weekly payment ÷ calendar hours) for Paid Home-Based arrangements of children under 5 (`1a:1140-1151`). The home-based provider file supplies only labor inputs, not a price. | Set in equilibrium. Example: $7.22 in 2026. |
| Other Paid | $5.37 an hour | **Household file.** Child-weighted mean hourly out-of-pocket price for regular "Other Paid" arrangements of children under 5 (`1a:1390-1393`). Not a market sector. | Base price × CPI-U growth since 2019 (`3c:637-638`). |
| Other Unpaid, Unpaid Home-Based, Parent Only | $0 | Not priced. | $0. |

**Supply-side 2019 parameters.** Each supply sector's 2019 price is split into two parts (`1a:1129-1134, 1201-1209`):

- a labor cost: wages by education × workers per child-hour;
- a per-unit residual, delta_0, which is the price minus the labor cost. It is $0.48 for low-priced centers, $4.72 for high-priced centers and $2.83 for paid home-based care (missing for unpaid centers).

Calibration rebuilds the 2019 prices as labor cost + delta_0, which reproduces the NSECE price exactly (`2a:1799-1822`).

**Household-specific price.** The price an ACS family faces is a family-specific multiplier (the "price wedge") × the sector price × the annual hours for that choice (`2a:323-413`, `3c:554-660`). The wedge is built as follows:

- In the NSECE, the target is each child's out-of-pocket hourly price ÷ the sector's 2019 price (`1a:1366-1398`).
- Four quantile regression forests (`ranger` with quantile regression enabled) predict that ratio from income percentile, region, number of parents and child age (`1a:79-125, 1412-1443`).
- For each ACS parent unit, one random percentile (1-99) is drawn and applied to all four wedges. The wedges are clipped to the range 0.1 to 5.0 (`compute_price_wedge_cache.R:99-125`).
- The wedge is drawn once and held fixed across all simulation years.

---

## 3. From 2019 values to 2024, 2025 and 2026

The base year for both the NSECE (supply) and the ACS (demand) is hard-coded as 2019:

- `main.R:53` sets `acs_base_year <- 2019`;
- `prepare_year_context.R:70-71, 88, 93` hard-codes 2019 in the growth-factor calls;
- the supply parameter file is named `supply_2019.yaml` (`2a:1793`, `3a:221`).

All growth factors are ratios of target-year to 2019 values from the Macro-Projections files `historical.csv` and `projections.csv` in `/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026071916/baseline` (`3a:347-350`). The exception is the employment-rate targets, which use `resources/epop/epop_projections.csv` inside the repository.

### 3.1 What is grown, and by what

| Quantity | Origin | Grown by | Code |
|---|---|---|---|
| Base care-worker wages w_0 (no BA, BA) | NSECE workforce file | **Nominal hourly wage index** = `gdp_wages / agg_hours_index`, ratio to 2019. Equilibrium wage = w_0 × index × (labor demanded ÷ L_0)^(1/elasticity). The elasticity is 4 for workers without a BA and 2 for workers with a BA. | `get_hourly_wage_growth_factor.R:1-30`; `prepare_year_context.R:70`; `3c:166-187` |
| Per-unit residual delta_0 | NSECE center and household files | **CPI-U** (`cpiu`) ratio to 2019 | `prepare_year_context.R:71`; `3c:269` |
| Labor requirements per child-hour (by sector and education) | NSECE center, workforce and home-based files | **Not grown**. Fixed at 2019. | `3a:231-247` |
| 2019 labor stock L_0 (hours by education) | NSECE: sector FTE enrollment × 40 × 50 × requirements | **Not grown**. Fixed at 2019. Demand-side labor uses ACS weights, which do grow, so population growth raises equilibrium wages relative to the wage index. | `1a:1240-1248`; `3c:175-181` |
| Wage-floor policy amounts | Configuration file, in 2019 dollars | Nominal hourly wage index | `3c:198` |
| Dollar employer-subsidy amounts | Configuration file, base year 2019 by default | Nominal hourly wage index | `3c:1323-1333` |
| "Other Paid" base price | NSECE household file | **CPI-U** | `3c:637-638` |
| Price wedges | NSECE household file through the price models | Not grown. They are ratios applied to the current-year price. | `compute_price_wedge_cache.R`; `3a:383-387` |
| Household net income in utility | ACS | Deflated **to 2019 dollars by CPI-U** before utility is computed, because the preference parameters were calibrated in 2019 dollars | `3c:403-418` |
| ACS earnings and hourly earnings | ACS | Nominal hourly wage index | `3b:663, 676-683` |
| ACS adjusted gross income (AGI) and household income | ACS | Nominal GDP per capita (`gdp` ÷ sum of `married_*` and `unmarried_*` population) | `3b:11-40, 664-689` |
| ACS weights (households, persons, parent units, children, tax units) | ACS | Population ratio by single year of age × marital status | `3b:520-638` |
| Employment-rate targets | 2019 ACS primary-caregiver employment rates by sex × age group | Employment-to-population ratios from the CBO-based `epop_projections.csv`, weighted by 2019 population, ratio to 2019 | `3b:130-152`; `get_employment_rate_growth_factor.R` |
| Chained CPI factors (for policy indexing) | — | `ccpiu_irs` (falls back to `ccpiu`), ratio to 2019 | `prepare_year_context.R:72, 88` |

### 3.2 Growth factors, 2019 to target year (read from the Macro-Projections files without running the model)

| Factor | Series | 2019→2024 | 2019→2025 | 2019→2026 |
|---|---|---|---|---|
| Nominal hourly wage (care-worker wages, ACS earnings, wage floor) | `gdp_wages / agg_hours_index` | **1.2944** | **1.3469** | 1.3982 |
| CPI-U (delta_0, Other Paid price, utility deflator) | `cpiu` | **1.2270** | **1.2610** | 1.2979 |
| Chained CPI (policy indexing) | `ccpiu_irs` | 1.2082 | 1.2357 | 1.2683 |
| Nominal GDP per capita (ACS AGI and household income) | `gdp` ÷ population | 1.3166 | 1.3690 | 1.4357 |
| Population (all ages) | sum of `married_*` and `unmarried_*` | 1.0331 | 1.0412 | 1.0436 |

Levels behind these factors:

| Year | `gdp_wages` | `agg_hours_index` | `cpiu` | `ccpiu_irs` |
|---|---|---|---|---|
| 2019 | 9,325.15 | 118.54 | 0.7705 | 0.7885 |
| 2024 | 12,387.93 | 121.66 | 0.9454 | 0.9527 |
| 2025 | 13,000.39 | 122.70 | 0.9716 | 0.9743 |

The 2024 and 2025 rows come from `historical.csv`; projections start in 2026.

Employment-rate growth factors from 2019, as computed by `get_employment_rate_growth_factor` (age bins weighted by 2019 population):

| Year | F under 25 | F 25-34 | F 35-44 | F 45+ | M under 25 | M 25-34 | M 35-44 | M 45+ |
|---|---|---|---|---|---|---|---|---|
| 2024 | 1.0008 | 1.0132 | 1.0341 | 1.0190 | 0.9987 | 0.9947 | 0.9932 | 1.0023 |
| 2025 | 0.9817 | 1.0098 | 1.0258 | 1.0238 | 0.9832 | 0.9941 | 0.9932 | 1.0071 |

**Worked example from run `202610151554`, baseline 2026:**

- The no-BA wage is $16.43 = $12.06 × 1.3982 × (L/L_0)^(1/4), which implies that labor demanded is about 90% of L_0.
- The BA wage is $24.08 = $19.70 × 1.3982 × (L/L_0)^(1/2), which implies about 77%.
- The low-priced center price is $4.39 = labor cost of $3.77 + $0.479 × 1.2979.

So the ratio of ACS-based demand to the NSECE-based L_0 already matters in the base period. Any change in how L_0 is built from the NSECE changes the wage level directly.

---

## 4. Hard-coded numbers and names that depend on 2019 survey facts

**File, survey and naming facts**

- ICPSR study and data-set numbers, and the year folder, in file paths: `2019/37941-0009-Data.tsv`, `-0006-`, `-0007-`, `-0005-` (`1a:132, 825, 831, 837`).
- Every variable name carries the 2019 prefix `HH9_`, `HHC9_`, `CB9_`, `HB9_` or `WF9_` (the "9" marks 2019 in the NSECE naming convention).
- The child age-at-September-1 name embeds the year (`HHC9_AGE_AT_SEP1_18_X`). The regex `[0-9]+` copes with a different year, but the column must exist (`1a:179, 221`).
- Loop sizes are coded into the regular expressions:
  - children 1-9 and household members 1-9, single digit `[1-9]` (`1a:177-181, 229, 260, 337-340, 376, 440-441`);
  - providers 1-14, `(?:[1-9]|1[0-4])` (`1a:522-533`).
  - Child and member numbers are taken from the **last character** of the name (`1a:205, 244, 356`). A 10th child or member would be missed or mis-numbered.
- The hours variables are named provider-first (`HH9_HRS_WEEK_PROV{provider}_{child}`) and are renamed (`1a:142-149`).
- Center age categories: enrollment exists for INF, 1YR to 5YR; prices exist only for INF, 2YR, 3YR and 4YR. The 1-year-old bin takes the infant price and the 5-year-old bin takes the 4-year-old price (`1a:844-847, 910-915`).
- Columns `hhm_id3` and `hhm_id4` must exist. Some child must have 3 and 4 flagged parents, or `select(-hhm_id3, -hhm_id4)` stops with an error (`1a:291`).
- "remove records (12 of them) with nonresponse" (`1a:363`) matches the 2019 count of code -10.
- An "~4% in the reference 2019 data" share of children without an assignable parent unit; the code stops if the share exceeds **10%** (`1a:781-794`).
- Price models are cached to `./cache/price_models/price_qrf_models.rds` (`1a:1540-1543`), a path relative to where the code runs, not to the run folder. Calibration and simulation read the same file (`2a:1830-1831`, `3a:334, 379`). A run on 2024 data and a run on 2019 data would overwrite each other's cache.
- The year 2019 is written into the supply file (`year = 2019`, `supply_2019.yaml`, `1a:1261, 1274, 1307`), the NSECE output tables (`nsece_*_2019.csv`, `1a:813`; read at `1b:476, 484`), the price summary (`base_price_2019`, `1a:1461`) and console labels (`3c:1797, 1819`).

**Reserve codes and code values**

- Household arrangement status cutoffs: -1 is missing, below -1 is set to 0; for REGCARE, -1 and -2 are missing (`1a:591-610`).
- Respondent sex: 3 = don't know; -4 is not handled, so it becomes female (`1a:154, 307`).
- Spouse or partner loop: -1 = none (`1a:155-160`).
- Household member age: > -9 kept, < 0 set to missing (`1a:242, 245`).
- Child sex: 1 = boy, 2 = girl (`1a:210-214`).
- Parent-child flag = 1 (`1a:274`).
- Work last week: codes 1 and 2 (`1a:388, 397, 405-406`).
- Pay unit codes: 7 and 8 set to missing; 1-6 converted with 8, 40, 80, 2000/12 and 2080 hours (`1a:459-470`).
- Type of care: **4 = center, 6 = K-8, 1-3 = individual**; 5, 7 and 8 fall to "other" (`1a:630-632`).
- Location in the child's home: 1 (`1a:641`).
- Prior relationship: 1 (`1a:642`).
- Provider-cares-for-child flag: 1 (`1a:571`).
- Center price status: -8 = free, 2 = no rate, -1 and -2 = missing (`1a:868-876`).
- Center full-time share: negative = not served (`1a:904-908`).
- Center classroom variables: -1 and -8 set to missing (`1a:23-76, 969-972`).
- Workforce: wage > 0; -1 = missing; roles 1-3 valid and 1-2 kept; education 7 and 8 = BA, 1 and 3-6 = no BA; full-time 1 → 1, part-time 2 → 0.5 (`1a:997-1045, 1066, 1089`).
- Home-based: enrollment ≥ 0; education > 0 and BA if ≥ 6; helpers > -4 and -2 set to 0; helper-BA percent ≠ -1 and ≠ -3; sample source 2 = listed (`1a:1166-1184`).

**Thresholds and conversions**

- **$4.75 an hour** low/high cut for household out-of-pocket center prices, in 2019 dollars (`1a:636-637`).
- **20 hours a week** cut between part-time and full-time NSECE care (`1a:730`). The model itself uses 10 and 40 hours a week (`constants.R:28-29`).
- **35 hours a week** cut between part-time and full-time parental work (`1b:71, 1432`). This matches the NSECE workforce full-time definition.
- **Age under 5** (under 60 months) for every household-side use (`1a:503, 1148, 1330, 1348`; `1b:92, 260, 1236, 1349, 1404`).
- **Weekly rate ÷ 40** gives the hourly center price (`1a:880`). The NSECE itself converts an hourly rate to weekly with × 40.
- **Part-time enrollment = 0.5 FTE** (`1a:921`). **Part-time worker = 0.5 FTE** (`1a:1042`).
- **Center age bins**: infant and 1 → 0-1; 2 and 3 → 2-3; 4 and 5 → 4-5 (`1a:889-893`). Classroom bins use `floor(youngest months/12)` in 0:1, 2:3, 4:5 (`1a:981-986`).
- **Median split at the 0.5 quantile**, weighted by center weight × FTE (`1a:934-938`).
- **Labor stock hours** = FTE × 40 × 50 (`1a:1243`; `constants.R:28, 32`).
- **Labor supply elasticities** of 4 (no BA) and 2 (BA) (`1a:1251`). These do not come from the NSECE but are stored with the NSECE parameters.
- **Income percentiles** at 1-99 (`seq(0.01, 0.99, 0.01)`), with a jitter of up to $1 (`1a:1333-1335`; `1b:95-97`; `compute_price_wedge_cache.R:79-83`).
- **Price-wedge clip** to [0.1, 5.0] (`compute_price_wedge_cache.R:122-125`).
- **Sampling for model training**: 50,000 rows for each enrollment forest and each price model, drawn with replacement when the sample is smaller (`1a:102-106`; `1b:1152-1160`). Forest settings: 500 trees, minimum node 10 (`1a:111-119`; `1b:1163-1172`).
- **Random seeds**: `random_seed_base = 76` (`main.R:75`), plus 7 for the D1a hours imputation (`1a:399`) and plus 1 and 2 elsewhere.

**Counts implied by the 2019 coding** (not literals in the code, but facts the code assumes)

- Seven priced or unpriced care types plus Parent Only, giving 15 child choices (`get_choice_catalog.R:28-46`).
- Four market sectors (`constants.R:43-47`). These are tied to the four-row supply parameter structure (`1a:1221, 1276-1279`; `3a:227-247`).
- Three center age bins.
- Two labor types (BA and no BA).
- Four Census regions, used as factor levels in the price models (`1a:122`).

---

## 5. Points a second pass should check against the 2024 NSECE

1. **Variable names and loop sizes.**
   - Are the prefixes still `HH9_` and so on, or new ones?
   - Are there still at most 9 children, 9 members and 14 providers?
   - Are the hours still named provider-first?
   - Does the age-at-September-1 variable exist?
   - Do the center price ages still cover only infant, 2, 3 and 4?
2. **Type-of-care codes 1-8.** Check their definitions, especially:
   - whether center care still covers Head Start, pre-K and community-based care, and is limited to children under 72 months;
   - whether paid versus unpaid is still part of the code;
   - the 5-hour regularity rule;
   - the irregular and unknown codes.
3. **Payment variable.** Check whether it still records direct charges only, whether "ineligible for Section J" still covers K-8, irregular and unknown care, and how unknown amounts are coded. The model treats unknown amounts as unpaid.
4. **Calendar hours.** Check whether they still cover only the reference week, still use 0 rather than a reserve code for no entry, and still treat summer cases as a typical week in May.
5. **Location and prior-relationship codes**, including the "inferred no relationship" code 2 and the "not defined for provider type" code -2.
6. **Center posted rate.** Check the public-file trimming rule (5th and 95th percentiles within each time unit, coded -2), the weekly standardization, and the presence of large outliers. These outliers raise the 2019 high-priced center price by about 20%.
7. **Center enrollment and full-time share.** Check the top-codes and the age categories.
8. **Workforce wage, role, education and full-time definitions.** Check the top-coding cells and whether the role categories changed again (2019 already merged 2012's categories).
9. **Home-based provider file.** Check the listed/unlisted coding, the enrollment top-code (20 for listed providers), the education codes, and the helper variables. Note that the model currently ignores the home-based weight.
10. **Weights.** Check the universe and totals of the child, center, workforce and home-based weights.
11. **Household income.** Check the reference year (2018 for the 2019 survey) and the top-coding.
12. **Respondent sex, number of mothers and fathers, and the parent-child flags.** These are used to find the primary caregiver.
13. **Work-for-pay item D1a** and the parent work-hours variables. Check their codes and universe.
