# NSECE 2019 to 2024: changes in survey design, questionnaires, and public-use files

Prepared 2026-10-16 for the move of the ECEC simulator from the 2019 NSECE public-use files (ICPSR 37941) to the 2024 files (ICPSR 39466).

## How to read this note

Every statement carries a source in parentheses. Sources are listed in full at the end, with URLs or file paths. Page numbers are PDF page numbers (the page count of the PDF file), followed where useful by the page label printed on the page (for example "HH-7").

Two labels mark how firmly a 2024 fact is established:

- **Confirmed** means a 2024 document says it: the 2024 user's guides, the 2024 questionnaires, the 2024 introduction document, the ICPSR study record, the 2024 books of tables, or the column headers of the 2024 data files on disk.
- **Inferred** means I reasoned it from indirect evidence, such as the absence of a variable, a value label without explanation, or a draft questionnaire. These need checking against the microdata.

All 2024 variable names in the table in section 7 were checked against the column headers of the local 2024 files (`/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1/2024/39466-000{1,2,3,5}-Data.tsv`, first line only) and against the 2024 user's guides. The prefixes changed from `HH9_`, `HHC9_`, `CB9_`, `HB9_`, `WF9_` to `HH4_`, `HHC4_`, `CB4_`, `HB4_`, `WF4_`. Restricted-use variables carry an extra `L1_` prefix in both years.

---

## 1. The most consequential changes, in order of likely effect on the model

1. **Much of the household sample has no weekly care calendar.** The 2024 household fieldwork ran January to November 2024. Households interviewed from late May onward were not given the 15-minute care and work calendar. Instead they reported usual weekly hours per provider for "a typical week in May" (summer interviews) or "a typical week" (fall interviews). In the 2024 file, 4,660 of 6,403 households (73 percent) had the calendar, 970 (15 percent) had the summer version and 773 (12 percent) the fall version (2024 household user's guide, PDF p. 66, HH-62). In 2019, 8,089 of 8,576 households (94 percent) had the calendar and 487 (6 percent) the summer version (2019 household user's guide, PDF p. 59). Hours of care, parents' work hours, and type of care are measured differently for the non-calendar households, and NORC advises caution in comparing households interviewed after August 15, 2024 with 2019 (2024 household user's guide, PDF pp. 12–16, HH-7 to HH-11). **Confirmed.**
2. **"Worked for pay last week" and hours worked refer to different weeks for summer and fall interviews.** Item D1A (worked for pay) always refers to the week before the interview, while hours of work and care refer to the reference period above (2024 household user's guide, PDF p. 12, HH-7). For a household interviewed in July, employment status describes a July week and hours describe a typical May week. **Confirmed.**
3. **Head Start and public pre-K flags are no longer asked for children aged 54 to 71 months.** In the final 2024 household questionnaire, after the kindergarten question C8C, every answer skips past C8_2 (the Head Start and public pre-K question) (2024 household questionnaire, PDF pp. 56–57). The 2024 user's guide confirms that C8_2 was asked only when the child was under 54 months, and that `HH4_PUBPREK` and `HH4_HEADSTART` are coded −5 (not defined) for children 54 months or older (2024 household user's guide, PDF pp. 300–301). In 2019, children 54 to 71 months who were not in kindergarten went on to C8_2, and −5 applied only from 72 months (2019 household questionnaire, PDF p. 62; 2019 household user's guide, PDF p. 213). Arrangements flagged as public pre-K fell from 313 in 2019 to 104 in 2024, and Head Start from 232 to 101. The model's Head Start and pre-K flags for older four-year-olds and five-year-olds will be missing. **Confirmed.**
4. **California transitional kindergarten moved from "kindergarten" to "not kindergarten."** In 2019, California transitional kindergarten was counted as kindergarten in C8C; in 2024 it was counted as "not kindergarten (includes pre-kindergarten or California transitional kindergarten)" (2024 household user's guide, comparability exhibit, PDF p. 39; 2019 household questionnaire, PDF p. 62; 2024 household questionnaire, PDF p. 56). The 2024 center screener also tells California respondents to report transitional kindergarten as early care and education (draft 2024 center questionnaire, July 2024, screener item Q1; OMB change memo of September 28, 2023, item 1). Combined with change 3, California four-year-olds in transitional kindergarten will now appear as center-based care users without a pre-K flag. **Confirmed** for the question wording; the effect on counts is **inferred**.
5. **The type-of-care classification shifted between "center-based" and "other organizational" care.** A new item (C17) on the kind of single activity a provider offers moved some arrangements that would have been coded "other organizational care" in 2019 into "center-based care" or "K–8 schooling" in 2024 (2024 household user's guide, `HH4_TYPEOFCARE_AGG`, PDF p. 284). OPRE's published 2019 estimates were adjusted upward by factors of 1.072 (under age 3) and 1.087 (ages 3 to 5) for center-based care to make them comparable (2024 household book of tables, Appendix B, PDF p. 61). **Confirmed.**
6. **Administrative-data records grew from about 7 percent to 16 percent of the center file, and they also appear in the workforce file.** 1,141 of 7,283 center records (public-school-linked programs) carry only administrative data; they have no classroom data, no prices, and no single-year-of-age enrollment, and are coded −8 on those variables (2024 center user's guide, PDF pp. 10–11, CB-5 to CB-6; `CB4_STDRATE_WKLY_INF`, PDF p. 515). In 2019 there were 482 or 483 such records (same source; 2019 center user's guide, PDF p. 175). The workforce file adds one placeholder record for each of the 1,141 centers, with a positive weight and −8 on most variables; the file has 7,511 rows of which 6,370 are interviews (2024 workforce user's guide, `WF4_METH_DATA_SOURCE`, PDF p. 40). **Confirmed.**
7. **Several reserve codes (the negative codes for skips and missing values) changed meaning.** Examples that touch model code directly: in the home-based file, −3 now means "no helpers" in `HB4_STAFF_EDUC_PRCNT_4YR` (in 2019, −2 meant no helpers and −3 meant unknown denominator), and −3 is now the valid-skip code in `HB4_H2_NUMHELP` (2019 used −2). In the workforce file, −1 on `WF4_WORK_WAGE` now means "don't know or refused," while "not enough information" moved to −5 and a new −3 means "not paid." In the center file, the classroom Head Start and pre-K indicators gained −3 (valid skip, about 5,000 cases) and −5, −6, −7 ("don't know, but less than 50 / more than 50 / 75 percent or more"). The center price variables no longer use −2 ("trimmed value"). Details are in section 7. **Confirmed.**
8. **Loop sizes grew.** Household-member loops run to 13 (were 9) and provider loops to 15 (were 14) (2024 household user's guide, PDF pp. 9–11, HH-4 to HH-6). Model code that hard-codes `[1-9]` for household members or `1[0-4]` for providers will silently drop records. **Confirmed** from file headers.
9. **The respondent's gender is now two yes/no indicators, and a third category exists only in restricted data.** `HH9_RGENDER` (1 male, 2 female) became `HH4_RGENDER_1` (male, yes/no) and `HH4_RGENDER_2` (female, yes/no); respondents could select more than one answer, and "transgender, non-binary, or another gender" is in `L1_HH4_RGENDER_3` only (2024 household user's guide, PDF pp. 146–147). Counts of mothers and fathers (`HH4_HHCOMP_NUMMOTHER`, `HH4_HHCOMP_NUMFATHER`) are built from this item (same guide, PDF pp. 124–125). **Confirmed.**
10. **The field period overlaps the last year of pandemic-era federal child care money.** The 2019 survey was fielded November 2018 to July 2019, before the pandemic (ICPSR 37941 study record). The 2024 survey was fielded November or December 2023 to November 2024 (ICPSR 39466 study record; 2024 introduction, PDF p. 12). The American Rescue Plan stabilization grants had to be spent by September 30, 2023, before fieldwork began; the American Rescue Plan supplemental discretionary Child Care and Development Fund money had to be spent by September 30, 2024, which covers most of the fieldwork (ACF presentation, September 2021, slide 14; ACF stabilization fact sheet, June 2023, p. 1). **Confirmed** for dates; any effect on prices or subsidy receipt is not documented.

---

## 2. Design changes common to all four surveys

### 2.1 Field period and timing

| | 2019 | 2024 |
|---|---|---|
| Overall | November 2018 to July 2019; most interviews January to May, summer edits in June (ICPSR 37941 record, collection dates) | Household January to November 2024; providers December 2023 to November 2024; workforce March (first completes February) to November 2024 (ICPSR 39466 record; 2024 introduction, PDF pp. 13–20) |
| Reason for the long 2024 field period | — | Low early response and loss of field interviewers; OMB was asked in July 2024 to extend fieldwork into the fall and to adapt questionnaires for the new school year (OMB change memo, July 9, 2024, pp. 1–3) |

Share of 2024 completes by season, from the month-by-month exhibits (2024 introduction, PDF pp. 13–20):

| Survey | January–May 2024 | June–August 2024 | September–November 2024 |
|---|---|---|---|
| Household (6,403) | 4,514 (70%) | 1,265 (20%) | 624 (10%) |
| Center interviews (6,142) | 3,165 (52%) | 1,806 (29%) | 1,171 (19%) |
| Workforce interviews (6,370) | 1,305 (20%) | 2,491 (39%) | 2,574 (40%) |

The workforce survey in particular is now weighted toward summer and fall 2024. Workforce wages are reported "at the time of the interview" (2024 workforce book of tables, PDF p. 13), so 40 percent of 2024 wages describe fall 2024, the start of a new school year.

Provider questionnaires were also changed for late interviews. If a provider's school year had not yet started at the interview date, the interview asked about spring 2024; otherwise about the current period (OMB change memo, July 9, 2024, Table 6). In the center file, 4,762 records refer to "last week" and 1,380 to "spring of 2024" (`CB4_METH_REFPERIOD`, 2024 center user's guide, PDF p. 47). Centers interviewed after July 22, 2024 were asked how fall 2024 prices compared with spring 2024: 622 reported no change, 152 an increase of more than 5 percent, 223 a smaller increase, and 6 a decrease (`CB4_B_PRICE_CHG`, 2024 center user's guide, PDF p. 514).

### 2.2 Pandemic and post-pandemic funding context

- 2019 fieldwork ended in July 2019, before the pandemic (ICPSR 37941 record).
- Federal pandemic child care money: CARES Act and CRRSA Act supplements (spending deadline September 30, 2023), American Rescue Plan stabilization grants of $23.9 billion (obligation deadline September 30, 2022; spending deadline September 30, 2023), and American Rescue Plan supplemental discretionary funds of $14.9 billion (obligation deadline September 30, 2023; spending deadline September 30, 2024) (ACF presentation, September 2021, slide 14). ACF reports that stabilization funds ended September 30, 2023 (ACF stabilization fact sheet, June 2023, p. 1).
- The 2024 OMB submission names these three laws and post-pandemic inflation and remote work as reasons for the 2024 survey (2024 OMB Supporting Statement A, section A1).
- The draft 2024 center questionnaire added an item on "use of stabilization funds from a state or federal agency after the COVID-19 pandemic" (OMB Appendix G, item R13a). I found no variable with "stabilization" in its label in the ICPSR variable index for the 2024 public-use or Level 1 restricted files (ICPSR variable search, study 39466). **Inferred:** the item was dropped or is not released.

### 2.3 Sample frame and sampling design

| | 2019 | 2024 |
|---|---|---|
| First stage | 219 PSUs (primary sampling units: counties or county groups), allocated to states by population under 18 (ICPSR 37941 record, sampling) | 219 PSUs, allocated by population under 13, chosen to maximize overlap with 2012 and 2019 PSUs (2024 introduction, PDF p. 8) |
| Second stage | 747 SSUs (secondary sampling units: one or two census tracts); low-income oversample where at least **40** percent of households were under 250% of poverty; 508 such SSUs (ICPSR 37941 record) | 747 SSUs; low-income oversample where at least **35** percent of households were under 250% of poverty; 498 such SSUs (2024 introduction, PDF p. 8) |
| Household frame | USPS Delivery Sequence File | Same, plus administrative address lists in 32 SSUs where the postal list was incomplete (2024 introduction, PDF p. 10) |
| Provider sample area | "Provider cluster" around each SSU (tracts within 2 miles of its population center) | Same, plus a new "outer ring" (tracts within 5 miles) in non-metropolitan areas for center and listed home-based providers (2024 introduction, PDF pp. 8–10). 233 center records come from the outer ring (`L1_CB4_METH_OUTERRING`, 2024 center user's guide, PDF p. 44) |
| Household subsampling | Households whose children were all 72 months or older were subsampled at 70 percent (2019 household user's guide, PDF pp. 45–46) | No subsampling; all eligible households were interviewed. 2024 sample children are older on average (2024 household user's guide, PDF p. 15, HH-10) |
| Supplements | — | Minnesota state supplement; Bezos Family Foundation and Foundation for Child Development funded more outer-ring providers and a second worker from the same classroom (2024 introduction, PDF p. 11) |
| Provider frame lists | State and national lists | Same approach; lists collected April to June 2023 (2024 introduction, PDF p. 11) |

### 2.4 Mode

| | 2019 | 2024 |
|---|---|---|
| Household interview | All by field interviewer, in person or phone (`HH9_MODE`, 2019 household user's guide, PDF p. 57) | Mostly field interviewer; web self-completion added September 2024; 136 web completes (2024 introduction, PDF pp. 13–14; `HH4_METH_MODE_COMPLETED`) |
| Household screener | Mail, web, interviewer | Mail, web, interviewer (2024 household user's guide, PDF p. 7) |
| Center | Web or interviewer | 5,017 of 6,142 by web (2024 introduction, PDF p. 18) |
| Workforce | Web or interviewer | 5,743 of 6,370 by web (2024 introduction, PDF p. 20) |
| Honoraria | Center $25, workforce $10 | Raised in July 2024 to $50 (center) and $20 (workforce) (OMB change memo, July 9, 2024, Table 8) |
| Languages | English and Spanish | English and Spanish; no center respondent used Spanish (2024 introduction, PDF p. 17) |

### 2.5 Response rates and sample sizes

| Survey | 2019 records / overall weighted response rate | 2024 records / overall weighted response rate |
|---|---|---|
| Household | 8,576 / 55.2% | 6,403 / 35% |
| Listed home-based | 4,231 / 67.8% | 3,778 / 63% |
| Unlisted home-based | 1,670 / 45.5% | 1,067 / 41% |
| Center-based | 6,917 records, of which 482 or 483 are administrative-data cases (ICPSR counts all 6,917 as completes) / 62.4% | 7,283 records (6,142 interviews plus 1,141 administrative) / 57% |
| Workforce | 5,192 records (including 483 administrative placeholders) / 70.9% | 7,511 records (6,370 interviews plus 1,141 placeholders) / 66% |

Sources: ICPSR 37941 and 39466 study records (response rates); 2024 introduction, Exhibit 1.9, PDF p. 20; 2019 workforce user's guide, `WF9_WORK_FT`, PDF p. 130; 2024 workforce user's guide, PDF p. 40. The 2024 household screener completion rate fell from 85.7 to 62 percent.

### 2.6 Weighting

- Both years provide one weight per record, plus a child weight in the household file and a classroom weight in the center file, with stratum and cluster variables for variance estimation (2019 and 2024 user's guides, weight entries).
- The 2024 plan was to adjust household base weights for eligibility and nonresponse, calibrate child weights to American Community Survey totals by state, metropolitan status, race and ethnicity, age group (0–4, 5–12) and sex, and truncate large weights; unlisted home-based weights are not calibrated (2024 OMB Supporting Statement B, section B6). This is the planned method; the 2024 sampling and methodology report was not available to confirm it.
- Weight ranges changed: household weight 0 to 33,184 in 2019 and 81 to 73,893 in 2024; center weight 1 to 462 in 2019 and 1 to 153 in 2024; workforce weight 1.4 to 8,515 in 2019 and 1 to 5,154 in 2024 (2019 household, center, workforce user's guides, PDF pp. 52, 59, 60; 2024 household, center, workforce user's guides, PDF pp. 61, 38, 34).
- The 2024 household user's guide states that the household weight sums to 29,331,914 households and the child weights to 52,627,854 children (PDF p. 8). These are exactly the totals the 2019 guide gives for 2019 (2019 household user's guide, PDF p. 45). OPRE's 2024 tables show fewer children under 6 in 2024 than in 2019 (section 8). **Inferred:** the 2024 guide may have repeated the 2019 totals; summing the 2024 weights will settle it.

### 2.7 Public-use file structure

- 2024 public files: workforce (DS1, 173 columns), center (DS2, 342), home-based (DS3, 269), household calendar (DS4, 16,129 columns), household (DS5, 6,516 columns); restricted Level 1 files DS6–DS9 (ICPSR variable index for study 39466; local file headers). 2019 public files also included a separate household calendar file and quick-tabulation files (2019 household user's guide, PDF pp. 235, 248; CFData "The Data" page). The local 2019 folder holds only DS5, DS6, DS7 and DS9.
- The 2024 user's guides document both public and Level 1 restricted variables in one volume (NORC resource roadmap, June 2026, p. 2, note 3).
- Geography in the public files is still limited to Census region plus community poverty density and urbanicity categories (`*_REGION`, `*_COMM_POVERTY_DENSITY`, `*_COMM_URBAN_DENSITY`). State and county are in Level 2 restricted data only (2024 data files overview, NORC, June 2026).
- The urbanicity categories now use 2020 Census urban definitions. OPRE uses different cut points in 2019 (rural below 30 percent urban) and 2024 (rural below 50 percent urban) and says the public-use variables follow these year-specific definitions (2024 household book of tables, PDF pp. 9–10). Applying the 2024 rule to 2019 would move about 3 percent of young children from "moderate density urban" to "rural" (same source).

---

## 3. Household survey (children under 13)

### 3.1 Target population and respondent

Unchanged: households with at least one child under 13 where at least one adult speaks English or Spanish; the respondent is an adult knowledgeable about the youngest child's care (2024 household user's guide, PDF p. 7; 2019 household user's guide, PDF p. 45). The 2024 file has 11,745 children, 15,118 child–provider arrangements, 11,799 providers and 16,036 household members aged 13 or older (2024 household user's guide, PDF pp. 9–10).

### 3.2 Weekly care and work schedule

- **Calendar households (73 percent).** As in 2019, the respondent rostered every provider used last week and filled a 15-minute grid for each child (care) and each parent (work, school, training, including commuting) for the seven days before the interview (2024 household user's guide, PDF p. 11). The work-schedule question still tells respondents to include commuting time (2024 household questionnaire, D1D_1, PDF p. 61; 2019 household questionnaire, PDF p. 67). One change: children reported as using only parental care now also go through the grid loop, which NORC says is unlikely to change constructed variables (2024 household user's guide, comparability exhibit, PDF p. 27).
- **Summer households (15 percent)** reported each regular provider (5 or more hours per week) in "a typical week in May," the usual weekly hours, the hours between 8 a.m. and 6 p.m. on weekdays, and the hours parents were in work-related activities while the child was with that provider (2024 household user's guide, PDF pp. 14–16).
- **Fall households (12 percent)** answered the same short items for "a typical week" in late summer or fall 2024 (same source).
- Non-calendar households do not report irregular arrangements (under 5 hours a week) (same source, PDF p. 16).
- The constructed variables for hours in care and parents' work hours combine both pathways. NORC recommends using these constructed variables rather than the calendar file alone, because calendar data will not sum to the population (same source, PDF p. 11). `HH4_METH_QUEXVERSION` and `HH4_METH_REFPERIOD` identify the pathway; they replace `HH9_SUMMERQUEX` (same source, PDF pp. 66–67).
- The share of children whose parents' work hours could not be computed because of a missing calendar block rose sharply: code −10 on `HH4_WST_HRS_TOT_PLO` occurs 417 times in 2024 versus once in 2019 (2024 household user's guide, PDF p. 268; 2019 household user's guide, PDF p. 187).

### 3.3 How care arrangements are recorded and classified

- Arrangements are still child–provider pairs, classified into eight aggregated types of care (`TYPEOFCARE_AGG`, codes 1–8 unchanged) (2024 household user's guide, PDF p. 284; 2019, PDF pp. 199–200).
- New item C17 on the nature of single activities changes the split between code 5 (other organizational care) and codes 4 (center-based) and 6 (K–8 school) (section 1, item 5).
- Raw arrangements were deduplicated across the calendar and non-calendar pathways so that the provider index is constant within a household (2024 household user's guide, PDF p. 10).
- Unweighted arrangement counts by type, 2019 versus 2024: individual paid no prior relationship 555 vs 340; individual paid prior relationship 1,003 vs 644; individual unpaid 5,138 vs 3,713; center-based 1,667 vs 1,356; other organizational 1,187 vs 707; K–8 school 6,234 vs 5,584; irregular 2,959 vs 2,630 (2019 household user's guide, PDF p. 200; 2024 household user's guide, PDF p. 285).

### 3.4 Payments and subsidies

- The payment question (J2: amount paid, then time unit) is unchanged in substance. The 2024 questionnaire offers per hour, per day, per week, every two weeks, per month, or something else; 2019 had the same list plus codes added during processing for per year, school year, per X months and semester (2024 household questionnaire, J2A; 2019 household questionnaire, J2A_E7_A, PDF p. 80).
- `HH4_WEEKLY_PAYMENT_TC` keeps the 2019 definition (direct charges paid by the household, converted to weekly). The 2024 entry does not describe a top-coding rule and points users to the restricted version; the maximum is $1,153.85 and 1,774 arrangement-level values are positive (2024 household user's guide, PDF p. 377). In 2019 the top 1 percent within each age and type-of-care cell was replaced by the cell median of that top group (2019 household user's guide, PDF p. 272), and 2,560 arrangements had positive payments (PDF p. 273).
- Section J now screens out providers who are household members or a nonresident parent more explicitly (2024 household user's guide, comparability exhibit, PDF p. 52).
- Who pays the provider (J13) and who reimburses the household (J14) have a revised response category for government agencies that now mentions education departments and child care assistance agencies (same exhibit, PDF p. 53).
- Sliding-fee and co-payment items (J6, J7) are described as asked of families at or below 250 percent of poverty in 2024, versus 200 percent in 2019 (same exhibit, PDF p. 54). However, the final 2024 questionnaire uses the same nominal income cut-offs as 2019 (for example, below $52,000 for four people), now applied to 2023 income (2024 household questionnaire, J15, PDF p. 77; 2019 household questionnaire, J10_subelig, PDF p. 82). See open questions.
- `HH4_SUBSIDY` (payments made directly to the household) has 63 positive values in 2024, mean $85 per week (2024 household user's guide, PDF p. 379). In 2019 its inputs included the income screen item J10; the 2024 entry lists only J9, J9A, J9B and J11.
- New items: varying hours paid each week (J12), supply and other fees for subsidized families, subsidy receipt and application in the past five years (G-section items) (2024 household user's guide, comparability exhibit; OMB Appendix B).

### 3.5 Public pre-K, Head Start and kindergarten

See section 1, items 3 and 4. In short: in 2024, Head Start and public pre-K are identified only for children under 54 months in arrangements with an organization; California transitional kindergarten is no longer treated as kindergarten.

### 3.6 Parent employment, wages and income

- D1A ("Last week, did you do any work for pay?") has the same wording in both years and is rated directly comparable (2024 household user's guide, comparability exhibit, PDF p. 40; 2024 household questionnaire, PDF p. 60; 2019 household questionnaire, PDF p. 66). It is asked of the respondent, spouse or partner, other parents of children under 13, and guardians. Two changes: the "don't know or refused" code moved from 3 to −1, and 85 partners of the respondent who were not parents of children in the household were not asked because of a programming error (code −33) (2024 household user's guide, `HH4_D1A_WORK`, PDF p. 201).
- D1A always refers to "last week," even for summer and fall households (section 1, item 2).
- New work-from-home items (D16–D20): whether the person works only at home, only outside, or both; days and hours worked from home last week; child care used while working from home (2024 household questionnaire, D16–D20, PDF p. 64; OMB Appendix B).
- Wage at main job (D3D) is directly comparable; same units (per hour, day, week, two weeks, month, year, other) and same top-coding rule (top 1 percent or top 10 cases within each pay unit replaced by their median) (2024 household user's guide, PDF pp. 224–225; 2019, PDF p. 166). The guide labels it as asked of parents and parents' spouses or partners; guardians in households without parents were added to the D2 series (comparability exhibit, PDF p. 42).
- Annual income refers to calendar year 2023 (was 2018). Same construction: before-tax adjustment and top 1 percent replaced by the median of the top 1 percent (2024 household user's guide, PDF p. 181; 2019, PDF p. 139). Unweighted mean $96,441 and maximum $752,500 in 2024, versus $62,021 and $371,067 in 2019.
- Food and cash assistance items were restructured (separate SNAP, WIC, TANF and SSI items), so the 2019 combined items (`HH9_ECON_FOODSTAMP`, `HH9_ECON_WIC`, `HH9_WELFARE`, `HH9_PUBASSIST`) are not in the 2024 file (2024 household user's guide, comparability exhibit, PDF p. 49; comparison of file headers).

### 3.7 Household roster and relationships

- Relationship codes in B1D are unchanged: spouse, partner, parent or parent-in-law, child or child-in-law, sibling or sibling-in-law, other relative, non-relative (2024 household questionnaire, B1d, PDF p. 30; 2019 household questionnaire, B1d, PDF p. 36).
- Gender (now item B4) allows several answers and offers a third category (section 1, item 9). NORC rates it "compare with caution" (2024 household user's guide, comparability exhibit, PDF p. 26). Unweighted respondent counts: male 1,642 and female 4,718 of 6,403 in 2024; male 1,912 and female 6,605 of 8,576 in 2019.
- New items on nonresident parents: whether a child has a parent living elsewhere, nights spent with that parent last week, and that parent's spouse or partner (2024 household user's guide, comparability exhibit, PDF pp. 21–22).
- The guardian question (B1_CUST) now also applies when the child is the respondent's stepchild (same exhibit, PDF p. 23).
- Children-count questions accept up to 12 (was 10) (same exhibit, PDF p. 55).

---

## 4. Center-based provider survey

### 4.1 Frame, sample and reference period

- Target population unchanged: centers serving at least one child age 5 or under not yet in kindergarten, operating at least two hours a day and three days a week, not limited to drop-in, single-activity or before- and after-school care, and open during the school year (2024 introduction, PDF p. 17).
- Administrative-data cases rose from 482 to 1,141. In 2019 these were entered into the questionnaire with missing values where information was absent. In 2024 NORC built a fixed subset of variables (enrollment, staffing, revenue and sponsorship, schedule) and coded all other variables −8. Only total enrollment for ages 0–5 is available for these cases (2024 center user's guide, PDF pp. 10–11; 2024 center book of tables, PDF p. 11). NORC advises against analyzing interview cases alone, and suggests restricting to `CB4_METH_PUBSCHFLAG = 0` or imputing (2024 center user's guide, PDF p. 11).
- Reference period: "last week" for 4,762 records and "spring of 2024" for 1,380 (section 2.1).

### 4.2 Posted price ("standard rate") by age

- Question B1_5 is unchanged: "How much are you currently charging families for full-time enrollment for [age group]? Please do not include any subsidies or discounts," for infants, 2-, 3- and 4-year-olds (2024 center questionnaire, B1_5; 2019 center user's guide, PDF p. 325).
- 2024 drops the follow-up items needed to convert weekly, monthly and daily rates to hourly rates ("how many hours per week does that cover?"); only weekly prices can be constructed (2024 center user's guide, comparability exhibit, PDF p. 17; OMB Appendix G).
- The weekly standardization rules are the same in both years: hourly × 40, half-day × 10, full-day × 5, monthly ÷ 4.34524, term ÷ weeks (2024 center user's guide, PDF pp. 512–513; 2019 center user's guide, PDF pp. 324–325).
- **Disclosure treatment changed.** In 2019 values outside the 5th–95th percentile range for each time unit were removed from the public file and coded −2, "trimmed value" (1,048 cases across the four ages) (2019 center user's guide, PDF p. 325). In 2024, values outside that range are replaced by the 5th or 95th percentile value ("left- and right-censored"), so they stay in the data at the boundary (2024 center user's guide, PDF p. 513). The 2024 label still says "top coded."
- 2024 adds imputation of missing or outlying time units (z-score of 4 or more) and of weeks per term; the flags are in restricted data only (`L1_CB4_STDRATE_WKLY_FLAG_*`) (same source, PDF p. 513). **Inferred:** public-file users cannot tell which prices rest on an imputed time unit.
- Infant weekly price in 2024, unweighted: 2,479 valid values, mean $318.81, median $290 (2024 center user's guide, PDF p. 515).

### 4.3 Enrollment, staffing and classroom

- Enrollment by single year of age (`CB4_ENRL_*`) and percent full time (`CB4_ENRL_FT_*`) keep their 2019 definitions and top-coding (top 2 percent within each age category replaced by their median) (2024 center user's guide, PDF pp. 59, 75). Administrative cases are −8. The 2019 variables giving the bounds of each center's age groups (`CB9_A10_LO_*`, `CB9_A10_HI_*`) are not in the 2024 public file (header comparison).
- New: count of children enrolled on a regular drop-in basis (C1_3) (OMB Appendix G).
- Staff counts now cannot exceed totals reported earlier (2019 capped at 99) (2024 center user's guide, comparability exhibit, PDF p. 24).
- Classroom items F3 (youngest child's age, enrollment, teachers and aides in the last activity period) are unchanged in wording and top-coding (2024 center user's guide, PDF pp. 264–270).

### 4.4 Funding indicators

- Classroom-level indicators for Head Start (F15), public pre-K (F16) and subsidy (F14) keep their names but have new codes: −3 (valid skip; 4,961 cases for F15 and 5,057 for F16) and −5, −6, −7 for "don't know, but less than 50 percent / more than 50 percent / 75 percent or more" (2024 center user's guide, PDF pp. 279–280). In 2019 the skip was folded into −1 (5,410 for F15, 5,561 for F16) (2019 center user's guide, PDF pp. 175–176).
- Center-level items on blended child-level funding were dropped; new items ask about staff salaries paid by government sources (R13, R14 in the file) (OMB Appendix G; ICPSR variable index).
- Share of centers receiving each funding stream was similar in 2019 and 2024 (section 8).

---

## 5. Home-based provider survey (listed and unlisted)

- Definitions unchanged: individuals caring at least 5 hours a week for a child under 13 not their own in a home setting; listed providers come from state and national lists, unlisted ones from the household screener (2024 home-based book of tables, PDF pp. 8–10).
- 2024 cases: 3,778 listed and 1,067 unlisted (2019: 4,231 and 1,670). Unlisted paid cases fell from about 390 to 260 and unlisted unpaid from 1,290 to 810 (2024 home-based book of tables, PDF p. 8).
- Listed providers can also come from the new non-metropolitan outer ring (section 2.3).
- **Paid status (`HB4_PRGM_PAID`).** Same definition (paid for at least one child). New code −3 for small providers who skipped the payment item (22 listed, 14 unlisted). Listed unpaid providers are coded −1 in the public file for disclosure, as in 2019 (2024 home-based user's guide, PDF p. 60; 2019, PDF p. 67).
- **Helpers.** `HB4_H2_NUMHELP` valid skip moved from −2 to −3 and "don't know" from −4 to −1 (2024 home-based user's guide, PDF p. 413; 2019, PDF p. 366). `HB4_STAFF_EDUC_PRCNT_4YR` reserve codes were renumbered: 2024 uses −5 (not enough information), −3 (no helpers) and −1 (number of helpers unknown); 2019 used −3 (helper count unknown), −2 (no helpers) and −1 (numerator unknown) (2024 home-based user's guide, PDF p. 427; 2019, PDF p. 372).
- **Enrollment (`HB4_ENRL_TOTAL`).** Top-coded at the 98th percentile separately for listed and unlisted providers (maximum 19 listed, 10 unlisted); listed providers still bottom-coded at four or fewer. In 2019 the top codes were 20 (listed) and 8 (unlisted) (2024 home-based user's guide, PDF p. 149; 2019, PDF p. 151).
- **Education (`HB4_CAREER_EDUC`).** Same collapsed categories (codes 1, 3–7) (2024 home-based user's guide, PDF p. 532; 2019, PDF p. 427).
- **Prices.** The 2019 hourly price variables (`HB9_CHRG_RATE_*_TC`, "hourly price of care") are replaced by weekly price variables (`HB4_CHRG_WEEK_RATE_*_TC`, "weekly price of care") (2019 home-based user's guide, PDF p. 132; 2024 home-based user's guide, PDF p. 119). The model does not use these today.
- Subsidy-related items now go to more providers (small paid providers without a prior relationship are added to the subsidy enrollment item) (OMB Appendix E).
- The household roster of the provider uses fewer age categories (OMB Appendix E).

---

## 6. Center-based workforce survey

- Sampling unchanged in principle: one, and in some classrooms two, classroom-assigned teachers, assistants or aides from the randomly selected classroom of each interviewed center. The second-worker sample was expanded with foundation money (2024 introduction, PDF p. 11; 2024 workforce book of tables, PDF p. 8).
- The file has 7,511 records: 6,370 interviews plus 1,141 placeholder records for administrative-data centers that carry weights and −8 elsewhere. NORC says these must be kept for weighted totals of the workforce (2024 workforce user's guide, PDF p. 40). 2019 had 483 such placeholders (2019 workforce user's guide, PDF p. 130).
- **Hourly wage (`WF4_WORK_WAGE`).** Same construction (reported pay and unit, converted to hourly using hours per week, days open per week, weeks open per year) and same conditional top-coding by urbanicity, auspice and education (2024 workforce user's guide, PDF p. 105; 2019, PDF p. 133). The 2024 questionnaire added a confirmation item after the wage question to reduce implausible values (2024 workforce user's guide, comparability exhibit, PDF p. 16). Unweighted 2024: mean $21.61, median $17.50, minimum $0.11, maximum $230.06; 2019: mean $15.33, minimum $1.29, maximum $107.50. Reserve codes changed (section 1, item 7).
- **Imputation flag (`WF4_WORK_WAGE_IMP`).** Same codes 0–7 per the ICPSR value labels, plus a new −1 (don't know or refused; 655 cases). The summary statistics printed in the 2024 user's guide (N 5,715, mean 6,053) do not fit a 0–7 flag and look like a documentation error (2024 workforce user's guide, PDF p. 107).
- **Role (`WF4_WORK_ROLE`).** Same three categories (aide or assistant teacher; teacher, instructor or lead teacher; other or undetermined), still taken mainly from the center director's report (2024 workforce user's guide, PDF p. 113; 2019, PDF p. 131).
- **Education (`WF4_CHAR_EDUC`).** Same categories, with the two lowest combined (2024 workforce user's guide, PDF p. 63; 2019, PDF p. 255).
- **Full-time status (`WF4_WORK_FT`).** Same rule: 35 or more hours per week self-reported (2024 workforce user's guide, PDF p. 99; 2019, PDF p. 130).
- New 2024 content: extra payments such as bonuses, paid time off, raises for credentials, prior roles, intent to stay in early care and education, student debt, burnout, public assistance receipt, summer-specific items (temporary summer hire, summer hours different) (OMB Appendix I; header comparison). Removed: Parental Modernity Scale, Curbow job stress items, gender item (replaced by a gender identity item) (OMB Appendix I).

---

## 7. Variable-by-variable table

"Name confirmed" means the 2024 name appears in the local 2024 file header and in the 2024 user's guide. Page numbers are for the 2024 user's guide of the relevant survey unless stated.

### 7.1 Household (2019 `HH9_`/`HHC9_` → 2024 `HH4_`/`HHC4_`)

| 2019 variable | 2024 counterpart | Basis | Changes in definition, universe, codes or top-coding |
|---|---|---|---|
| `HH9_METH_WEIGHT` | `HH4_METH_WEIGHT` | Confirmed (p. 61) | Same role. Range 81–73,893 (2019: 0–33,184). Stated weighted total identical to 2019; see open questions. |
| `HH9_RSPOUSEPARTNER_LOOP` | `HH4_RSPOUSEPARTNER_LOOP` | Confirmed (p. 140) | Same codes (−2, −1, loop number), but loop numbers now run to 13. |
| `HH9_RGENDER` | `HH4_RGENDER_1` (male, 0/1) and `HH4_RGENDER_2` (female, 0/1) | Confirmed (pp. 146–147) | One categorical variable became two yes/no indicators. Multiple answers allowed. 0 means "no, don't know or refused." Third category only in `L1_HH4_RGENDER_3`. Model code `HH9_RGENDER != 3` / `== 1` must be rewritten. |
| `HH9_PARCH_X_Y` | `HH4_PARCH_X_Y` | Confirmed (p. 132) | Household-member index Y now 1–13 (was 1–9). Same codes. Model regex `[1-9]_[1-9]` will miss members 10–13. |
| `HH9_HRS_WEEK_PROVY_X` | `HH4_HRS_WEEK_PROVY_X` | Confirmed (p. 340) | Provider index now 1–15 (was 1–14). For summer and fall households the value is usual weekly hours in a typical week, not calendar hours. Zero for non-existent arrangements, as before. |
| `HH9_HOURS_WEEK_PROV` | none | Confirmed absent | Not in the 2019 public file either; the 2019 user's guide lists it once as a typo for `HRS_WEEK_PROV`. The model's fallback rename can be dropped. |
| `HH9_PRIORREL_Y` | `HH4_PRIORREL_Y` | Confirmed (p. 293) | Provider index 1–15. Codes −9, −1, 0, 1; 2019 code 2 ("inferred no relationship, individual with family day care") is not in the 2024 value labels (inferred from ICPSR labels). |
| `HH9_HHM_AGE_X` | `HH4_HHM_AGE_X` | Confirmed (p. 130) | Index 1–13. Valid skip moved from −2 to −3. |
| `HH9_HHCOMP_NUMMOTHER` | `HH4_HHCOMP_NUMMOTHER` | Confirmed (p. 125) | Built from the new multi-answer gender item B4. How a parent who selects both or the third category is counted is not stated. |
| `HH9_HHCOMP_NUMFATHER` | `HH4_HHCOMP_NUMFATHER` | Confirmed (p. 124) | As above. 2019 built it from `HH9_FATHER_X`; 2024 lists B4, B1D, A2F, B1E. |
| `HH9_HHCOMP_MEMBERS` | `HH4_HHCOMP_MEMBERS` | Confirmed (p. 123) | Same definition (all household members). |
| `HH9_D1A_WORK_X` | `HH4_D1A_WORK_X` | Confirmed (p. 201) | Same wording. Index 1–13. "Don't know or refused" moved from 3 to −1; new −3 (valid skip) and −33 (85 partners wrongly not asked). Refers to last week even when hours refer to May or a typical week. |
| `HH9_WST_PARCH_LO_X` | `HH4_WST_PARCH_LO_X` | Confirmed (p. 265) | Child index 1–9. For non-calendar households built from summary items DS_1 and DSY_1. Code −10 (missing calendar block) now 302 cases (2019: 5). |
| `HH9_WST_PARCH_HI_X` | `HH4_WST_PARCH_HI_X` | Confirmed (p. 263) | As above. |
| `HH9_WST_HRS_TOT_PLO_X` | `HH4_WST_HRS_TOT_PLO_X` | Confirmed (p. 268) | As above; maximum 150 hours. −10 now 417 cases (2019: 1). Non-calendar households report typical weekly work and commuting hours rather than a diary. |
| `HH9_WST_HRS_TOT_PHI_X` | `HH4_WST_HRS_TOT_PHI_X` | Confirmed (p. 267) | As above. |
| `HH9_WEEKLY_PAYMENT_TC_X_Y` | `HH4_WEEKLY_PAYMENT_TC_X_Y` | Confirmed (p. 377) | Provider index 1–15. Same reserve codes. Top-coding rule not described in 2024 (2019: top 1 percent by age and type-of-care cell). Maximum $1,153.85. |
| `HH9_TYPEOFCARE_AGG_X_Y` | `HH4_TYPEOFCARE_AGG_X_Y` | Confirmed (p. 284) | Same eight codes. New item C17 moves some code-5 arrangements to codes 4 or 6. |
| `HH9_SUBSIDY_X_Y` | `HH4_SUBSIDY_X_Y` | Confirmed (p. 379) | Provider index 1–15. Same codes. Only 63 positive values. |
| `HH9_REGION` | `HH4_REGION` | Confirmed (p. 69) | Same four Census regions. |
| `HH9_REGCARE_X_Y` | `HH4_REGCARE_X_Y` | Confirmed (p. 291) | Same codes. Source items now C9, C8A, C3, C4B; summer flag no longer an input. |
| `HH9_PUBPREK_X_Y` | `HH4_PUBPREK_X_Y` | Confirmed (p. 301) | **Universe narrowed:** defined only for organizations caring for children under 54 months (2019: under 72 months). −5 covers 54 months or older. California transitional kindergarten no longer counted as kindergarten. |
| `HH9_HEADSTART_X_Y` | `HH4_HEADSTART_X_Y` | Confirmed (p. 300) | Same narrowing as `PUBPREK`. |
| `HH9_ECON_INCOME_ANNUAL` | `HH4_ECON_INCOME_ANNUAL` | Confirmed (p. 181) | Calendar year 2023 (was 2018). Same before-tax adjustment and top-1-percent top-coding. |
| `HH9_D3D_WAGE_TC_X` | `HH4_D3D_WAGE_TC_X` | Confirmed (p. 225) | Index 1–13. Same top-coding by pay unit. Valid skip −3 (2019 listed −4 for don't know). |
| `HH9_D3D_UNIT_X` | `HH4_D3D_UNIT_X` | Confirmed (p. 224) | Index 1–13. Same unit codes 1–7; "don't know or refused" now −1 (2019: 8). |
| `HH9_D3D_` (raw wage) | `L1_` restricted only | Inferred | Only the top-coded `_TC` version is public in both years (header comparison). |
| `HH9_CHILDREN_X_Y` | `HH4_CHILDREN_X_Y` | Confirmed (p. 289) | Provider index 1–15. Codes −9, 0, 1. |
| `HH9_CARELOC_X_Y` | `HH4_CARELOC_X_Y` | Confirmed (p. 297) | Provider index 1–15. Same codes. |
| `HH9_SUMMERQUEX` (not in model list, but needed) | `HH4_METH_QUEXVERSION`, `HH4_METH_REFPERIOD` | Confirmed (pp. 66–67) | Three versions instead of two. |
| `HHC9_METH_WEIGHT_X` | `HHC4_METH_WEIGHT_X` | Confirmed (header) | Same role; school-age-only households no longer subsampled. |

### 7.2 Center-based provider (`CB9_` → `CB4_`)

| 2019 variable | 2024 counterpart | Basis | Changes |
|---|---|---|---|
| `CB9_METH_WEIGHT` | `CB4_METH_WEIGHT` | Confirmed (p. 38) | Same role; range 1–153 (2019: 1–462). Includes 1,141 administrative cases, which must be kept. |
| `CB9_WEIGHT_CLSM` | `CB4_WEIGHT_CLSM` | Confirmed (p. 44) | Same role; range 1–2,283. Administrative cases have no classroom. |
| `CB9_STDRATE_WKLY_X` (INF, 2YR, 3YR, 4YR) | `CB4_STDRATE_WKLY_X` | Confirmed (pp. 512–515) | Same question and weekly conversion. Extremes now censored at the 5th and 95th percentiles instead of removed (2019 code −2 no longer exists). New −8 for administrative cases. Time-unit imputation added; flags restricted. Some centers report spring 2024 prices. |
| `CB9_B1_3A_AGERATE_X_R` | `CB4_B1_3A_AGERATE_X_R` | Confirmed (p. 527) | Same item. New −8 and −3 codes. |
| `CB9_F3_YOUNGMONTHS_R` | `CB4_F3_YOUNGMONTHS_R` | Confirmed (p. 264) | Same; grouped at 157 months or more. −8 for administrative cases. |
| `CB9_F3_TEACH_TC` | `CB4_F3_TEACH_TC` | Confirmed (p. 269) | Same top-coding (top 2 percent). |
| `CB9_F3_ENROLL_TC` | `CB4_F3_ENROLL_TC` | Confirmed (p. 266) | Same. |
| `CB9_F3_ASSIST_TC` | `CB4_F3_ASSIST_TC` | Confirmed (p. 270) | Same. |
| `CB9_F16_CHILD_FUND_PK_IND` | `CB4_F16_CHILD_FUND_PK_IND` | Confirmed (p. 280) | New codes −3 (valid skip, 5,057 cases), −5, −6, −7 (partial knowledge). 2019 folded skips into −1. Model helper `recode_admin_na` handles only −1 and −8. |
| `CB9_F15_CHILD_FUND_HS_IND` | `CB4_F15_CHILD_FUND_HS_IND` | Confirmed (p. 279) | As for F16 (−3: 4,961 cases). |
| `CB9_ENRL_FT_X` | `CB4_ENRL_FT_X` | Confirmed (p. 75) | Same percentage definition; −8 for administrative cases. |
| `CB9_ENRL_X` | `CB4_ENRL_X` | Confirmed (p. 59) | Same; top 2 percent top-coded; −8 for administrative cases (only total 0–5 enrollment, `CB4_ENRL_0TO5`, for those). |

### 7.3 Home-based provider (`HB9_` → `HB4_`)

| 2019 variable | 2024 counterpart | Basis | Changes |
|---|---|---|---|
| `HB9_METH_SAMPLE_SOURCE` | `HB4_METH_SAMPLE_SOURCE` | Confirmed (p. 53) | Same codes (1 unlisted, 2 listed). |
| `HB9_STAFF_EDUC_PRCNT_4YR` | `HB4_STAFF_EDUC_PRCNT_4YR` | Confirmed (p. 427) | Reserve codes renumbered: −3 now means "no helpers" (1,890 listed), −5 "not enough information," −1 "number of helpers unknown." The model's filter `!= -1, != -3` would now drop all listed providers without helpers. |
| `HB9_PRGM_PAID` | `HB4_PRGM_PAID` | Confirmed (p. 60) | Same definition; new −3 valid skip; labels now "Unpaid/Paid." |
| `HB9_METH_WEIGHT` | `HB4_METH_WEIGHT` | Confirmed (p. 47) | Same role; listed range 1–178 (2019: 1–617). |
| `HB9_H2_NUMHELP` | `HB4_H2_NUMHELP` | Confirmed (p. 413) | Valid skip −2 → −3; don't know −4 → −1. The model's recode of −2 to zero helpers will no longer fire. |
| `HB9_ENRL_TOTAL` | `HB4_ENRL_TOTAL` | Confirmed (p. 149) | Top codes now at the 98th percentile (19 listed, 10 unlisted; 2019: 20 and 8). Bottom-coding of listed providers at four or fewer kept. |
| `HB9_CAREER_EDUC` | `HB4_CAREER_EDUC` | Confirmed (p. 532) | Same codes. |

### 7.4 Workforce (`WF9_` → `WF4_`)

| 2019 variable | 2024 counterpart | Basis | Changes |
|---|---|---|---|
| `WF9_WORK_FT` | `WF4_WORK_FT` | Confirmed (p. 99) | Same 35-hour rule. |
| `WF9_CHAR_EDUC` | `WF4_CHAR_EDUC` | Confirmed (p. 63) | Same categories. |
| `WF9_WORK_WAGE` | `WF4_WORK_WAGE` | Confirmed (p. 105) | Same construction and top-coding. Reserve codes: −1 now "don't know or refused" (2019: "not enough information"); −5 "not enough information"; −3 "not paid." The model maps only −1 to missing and everything else non-positive to "admin." |
| `WF9_WORK_WAGE_IMP` | `WF4_WORK_WAGE_IMP` | Confirmed (p. 107) | Codes 0–7 as before plus −1 (655 cases). Documentation statistics look wrong. |
| `WF9_WORK_ROLE` | `WF4_WORK_ROLE` | Confirmed (p. 113) | Same three categories, same source (director's report). |
| `WF9_METH_WEIGHT` | `WF4_METH_WEIGHT` | Confirmed (p. 34) | Same population definition; 1,141 placeholder records carry weights. |

---

## 8. Published 2019 and 2024 headline figures

All figures are from OPRE books of tables released in 2026. Shares are weighted. 2019 figures are as republished in these books (some differ slightly from earlier 2019 reports because of definition changes).

### 8.1 Children's participation (2024 household book of tables, PDF pp. 5, 11–15)

| Measure | 2019 | 2024 |
|---|---|---|
| Children under 3 | 11.4 million | 11.0 million |
| Children 3 to 5, not in kindergarten | 10.8 million | 10.1 million |
| Under 3 with any regular provider (5+ hours a week) | 48.6% | 53.2% (significant increase) |
| Under 3 in center-based care | 14.3% (adjusted; about 13.3% before adjustment) | 18.2% (significant increase) |
| Under 3 in individual unpaid care | 26.5% | 24.4% |
| Under 3 in individual paid care, no prior relationship | 6.1% | 7.8% |
| Ages 3–5 with any regular provider | 68.1% | 69.3% |
| Ages 3–5 in center-based care | 45.9% (adjusted; about 42.2% before adjustment) | 42.1% (count 4.95 million → 4.25 million, change significant only at 10 percent) |
| Ages 3–5 in individual unpaid care | 25.1% | 26.6% |

The 2019 center-based shares were multiplied by 1.072 (under 3) and 1.087 (3 to 5) to account for the new single-activity item (PDF p. 61). The "before adjustment" figures are my division of the published adjusted figures by these factors. The book uses age at the reference week and notes that 6 percent of 2019 children and 29 percent of 2024 children had summer or fall reference periods (PDF p. 7).

### 8.2 Center-based providers (2024 center book of tables, PDF pp. 6–7, 11)

| Measure | 2019 | 2024 |
|---|---|---|
| Centers serving at least one child 0–5 not in kindergarten | 121,000 | 117,000 |
| Children 0–13 enrolled in those centers | 9.8 million | 9.0 million |
| Enrolled children under 3 | 2.16 million (22.1%) | 2.36 million (26.1%) |
| Enrolled children 3–5 | 4.99 million | 4.78 million |
| Enrolled school-age children | 2.54 million (25.9%) | 1.85 million (20.4%) |
| Capacity use, 2-year-olds (enrollment ÷ enrollment plus vacancies) | 88.9% | 86.2% |
| Centers receiving any CCDF subsidy funding | 47.3% | 50.2% |
| Centers receiving Head Start funding | 25.4% | 24.9% |
| Centers receiving public pre-K funding | 41.6% | 42.4% |

No center price figures have been published for 2024. For orientation only, the unweighted 2024 infant weekly price in the public file has a median of $290 and mean of $319 (2024 center user's guide, PDF p. 515); the 2019 user's guide reports only a pooled distribution across ages, so the two are not comparable.

### 8.3 Home-based providers (2024 home-based book of tables, PDF pp. 6–8)

| Measure | 2019 | 2024 |
|---|---|---|
| Listed providers / children served | 91,200 / 782,000 | 81,600 / 668,000 |
| Unlisted paid providers / children served | 1,050,000 / 3,340,000 | 892,000 / 3,640,000 |
| Unlisted unpaid providers / children served | 4,030,000 / 7,380,000 | 3,650,000 / 6,830,000 |
| Listed providers age 60 or older | 19% | 27% |
| Listed providers caring for any subsidized child | 35% | 28% (significant decrease) |
| Mean children per listed provider | 8.9 | 8.4 |

### 8.4 Workforce (2024 workforce book of tables, PDF pp. 6–7, 35)

| Measure | 2019 | 2024 |
|---|---|---|
| Classroom teachers and aides, children not yet in kindergarten | 1.36 million | 1.55 million |
| Teachers and lead teachers | 853,000 | 1,020,000 |
| Aides and assistants | 507,000 | 531,000 |
| Mean hourly wage, aides and assistants (nominal) | $13.00 | $18.10 |
| Median hourly wage, aides and assistants | $11.00 | $15.00 |
| Mean hourly wage, teachers and lead teachers | $16.80 | $20.00 |
| Median hourly wage, teachers and lead teachers | $13.00 | $17.00 |
| 25th / 75th percentile, teachers | $10.00 / $17.00 | $14.00 / $21.30 |
| Wage missing (weighted) | 8.4% | 7.5% |

Wages are not adjusted for inflation and are measured at the interview date, which for 40 percent of 2024 interviews was September to November 2024 (section 2.1).

---

## 9. Open questions that only the 2024 microdata or full documentation can answer

1. **Weighted totals.** Do `HH4_METH_WEIGHT` and `HHC4_METH_WEIGHT_X` really sum to the 2019 totals (29,331,914 and 52,627,854), as the 2024 guide states? The OPRE tables imply fewer young children in 2024.
2. **Non-calendar households.** How different are hours in care, type of care, and parents' work hours for the 1,743 summer and fall households compared with calendar households of similar characteristics? Should the model weight, drop, or adjust them? Check `HH4_METH_QUEXVERSION` by month (`HH4_COMPLETE_DATE`).
3. **Employment and hours mismatch.** For summer and fall households, how often does `HH4_D1A_WORK` (last week) disagree with positive typical-week work hours in `HH4_WST_HRS_TOT_*`?
4. **Pre-K and Head Start for children 54–71 months.** Are there any non-reserve values of `HH4_PUBPREK`/`HH4_HEADSTART` at those ages, and can center-level data (`CB4_F15`, `CB4_F16`, center funding variables) or `HH4_C17_TYPEACTIV_R` be used to impute them?
5. **California transitional kindergarten.** How many children in the West region aged about 4 to 5 are now coded as center-based care rather than kindergarten? (State is not in the public file; region is the only handle.)
6. **Gender coding.** How many respondents have both `HH4_RGENDER_1` and `HH4_RGENDER_2` equal to 0, or both equal to 1, and how are they counted in `HH4_HHCOMP_NUMMOTHER` and `HH4_HHCOMP_NUMFATHER`?
7. **Weekly payment top-coding.** What rule produced `HH4_WEEKLY_PAYMENT_TC`? Compare its distribution by type of care and age with 2019.
8. **Subsidy income screen.** Was the J15 screen applied at the nominal 2019 dollar cut-offs (as the questionnaire shows) or at 250 percent of poverty (as the comparability exhibit says)? This affects which households were asked the co-payment and sliding-fee items.
9. **Center price censoring.** How many 2024 rates sit exactly at the 5th or 95th percentile values within each time unit, and how should they be treated to match the 2019 practice of dropping trimmed values?
10. **Spring versus fall prices.** For the 1,380 centers reporting spring 2024 and those reporting fall 2024, how large is the difference in weekly prices, and should the model deflate or align them to one date?
11. **Administrative cases.** Should the center-side model drop administrative cases (and use `CB4_METH_PUBSCHFLAG = 0`), impute their classroom and price data, or reweight? NORC warns that interview cases alone are not a valid population.
12. **Workforce placeholders.** Confirm that the 1,141 workforce placeholder records have −8 on all wage, role, education and full-time variables, and decide whether the wage model uses them only for totals.
13. **Very low wages.** `WF4_WORK_WAGE` has values as low as $0.11 per hour. How many values are under the federal minimum wage, and were they in the 2019 file? Check against `WF4_WORK_WAGE_IMP`.
14. **Wage imputation flag.** What is the actual distribution of `WF4_WORK_WAGE_IMP`, given that the user's guide statistics appear wrong?
15. **Reserve codes across all model variables.** Tabulate every negative value of each variable in section 7 to confirm the code meanings, since several were renumbered.
16. **Stabilization funds.** Is there any variable recording receipt of pandemic stabilization money (the draft item R13a)? None was found in the ICPSR variable index.
17. **Seasonal composition of the workforce.** Do wages, hours and full-time status differ between spring, summer and fall workforce interviews (`WF4_COMPLETE_DATE`, `WF4_METH_QUEXVERSION`, `WF4_A1_TEMP_SUM`)?
18. **Household weighting method.** Confirm calibration targets and weight truncation from the 2024 sampling and methodology report, which was not available.

---

## 10. Sources

Local documentation (read directly):

- 2019 household user's guide: `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1/2019/37941-0009-User_guide-MULTI.pdf`
- 2019 household questionnaire: `.../2019/37941-0009-Questionnaire-household_MULTI.pdf`
- 2019 center user's guide: `.../2019/37941-0006-User_guide.pdf`
- 2019 home-based user's guide: `.../2019/37941-0007-User_guide.pdf`
- 2019 workforce user's guide: `.../2019/37941-0005-User_guide.pdf`
- 2024 household user's guide: `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1/2024/39466-0005-User_guide-household_MULTI.pdf`
- 2024 household questionnaire: `.../2024/39466-0005-Questionnaire-household_MULTI.pdf`
- 2024 center user's guide: `.../2024/39466-0002-User_guide-centerbased_MULTI.pdf`
- 2024 center questionnaire: `.../2024/39466-0002-Questionnaire-centerbased_MULTI.pdf`
- 2024 home-based user's guide: `.../2024/39466-0003-User_guide-homebased_MULTI.pdf`
- 2024 workforce user's guide: `.../2024/39466-0001-User_guide-workforce_MULTI.pdf`
- 2024 introduction: `.../2024/39466-Documentation-Intro.pdf`
- 2024 file headers: first line of `.../2024/39466-000{1,2,3,5}-Data.tsv`; 2019 headers: first line of `.../2019/37941-000{5,6,7,9}-Data.tsv`

Web sources:

- ICPSR 39466 study record (2024): https://www.icpsr.umich.edu/web/ICPSR/studies/39466 (collection dates, response rates, sampling, questionnaire changes by survey)
- ICPSR 37941 study record (2019): https://www.icpsr.umich.edu/web/ICPSR/studies/37941
- ICPSR variable index for study 39466 (names, labels, value labels): https://www.icpsr.umich.edu/sites/api/solr/variable/query_dm?q=*:*&fq=STUDY_NO:39466
- NORC data users page: https://www.norc.org/research/projects/national-survey-of-early-care-and-education/data-users-group.html
- NORC 2024 data files overview (June 2026): https://www.norc.org/content/dam/norc-org/pdf2026/2024-nsece-data-files-overview_nrcec_06162026.pdf
- NORC resource roadmap (June 2026): https://www.norc.org/content/dam/norc-org/pdf2026/roadmap-nsece-resources-data-users_06172026.pdf
- OPRE 2024 NSECE project page: https://acf.gov/opre/project/national-survey-early-care-and-education-2024
- 2024 household book of tables, "Children's Participation in Child Care and Early Education: Counts and Characteristics," OPRE Report 2026-020: https://acf.gov/sites/default/files/documents/opre/HH-BoT-CCEE-Participation.pdf
- 2024 center book of tables: https://acf.gov/sites/default/files/documents/opre/2024-NSECE-CB-Book-of-Tables--2-_0.pdf
- 2024 home-based book of tables: https://acf.gov/sites/default/files/documents/opre/2024-HB-Providers-Book-of-Tables.pdf
- 2024 workforce book of tables: https://acf.gov/sites/default/files/documents/opre/2024-NSECE-WF-Book-of-Tables.pdf
- OMB clearance package 202305-0970-005 (Supporting Statements A and B; Appendices B, E, G, I comparing 2019 and 2024 questionnaire items): https://www.reginfo.gov/public/do/PRAViewDocument?ref_nbr=202305-0970-005
- OMB change memo, September 28, 2023 (California transitional kindergarten): package 202309-0970-011, https://www.reginfo.gov/public/do/PRAViewDocument?ref_nbr=202309-0970-011
- OMB change memo, July 9, 2024 (extension into fall 2024, reference periods, web household mode, honoraria) and July 2024 draft questionnaires: package 202407-0970-001, https://www.reginfo.gov/public/do/PRAViewDocument?ref_nbr=202407-0970-001
- ACF presentation, "ACF Perspectives on the Pandemic and the American Rescue Plan," September 2021, slide 14: https://peerta.acf.hhs.gov/sites/default/files/public/uploaded_files/ACF%20Perspectives%20on%20the%20Pandemic%20and%20the%20American%20Rescue%20Plan%20combined-508%20PPT.pdf
- ACF Office of Child Care, ARP Child Care Stabilization fact sheet, June 2023: https://acf.gov/sites/default/files/documents/occ/State_and_Territory_ARP_Child_Care_Stabilization_Fact_Sheet_June2023.pdf
- CFData "The Data" page (2019 file list): https://www.icpsr.umich.edu/sites/cfdata/nsece-data

Not obtained: the 2024 NSECE Sampling and Methodology Report (cited in the 2024 introduction but not found online), and any published 2024 price or out-of-pocket cost estimates (none found as of this date).
