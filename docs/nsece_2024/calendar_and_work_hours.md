2024 NSECE HOUSEHOLD FILE: THE WEEKLY CALENDAR, THE REFERENCE WEEK, AND MISSING WORK HOURS
(Markdown-formatted text; saved as .txt because writing a .md report file was blocked.)

Prepared 2026-10-16. Everything here was computed from the public-use household files (ICPSR 37941 for 2019, 39466 for 2024) and from the step 1a outputs of branch `review/nsece-2024` (commit `c05fefa`, seed 76) in `analysis/nsece2024/out/new_{2019,2024}`. No repository file was edited. The scripts re-implement the model's own classification rules and were checked against the 1a output: they reproduce every 1a care category, care-hours figure and parent work-hours figure exactly in both years (0 differences; `csv/wh_t0_check_against_1a.csv` and the job logs).

Terms used throughout:

- **Calendar households**: households that filled in the 15-minute weekly diary of care and work for the week before the interview.
- **Typical-week households**: households interviewed after the switch date, who were never given the diary and instead answered short questions about a typical week. The brief calls them non-completers; they did not decline the calendar, they were not offered it.
- **Regular-not-last-week provider**: a provider who usually gives at least 5 hours of care a week but did not happen to care for the child in the diary week.
- **Raking**: adjusting survey weights so that weighted totals match known totals on several characteristics at once.
- **Hot deck**: filling in a missing record by copying the answers of a randomly drawn similar record (the "donor").
- **D1a**: survey item "Last week, did you do any work for pay?", asked about the week before the interview in every version.
- All shares are percent of children under 5 with a care category, weighted by the child weight. Prices are the household's weekly out-of-pocket payment divided by weekly hours, for regular paid arrangements of children under 5. This is the definition of the earlier report's table C1 and the one behind the model's paid home-based and "Other Paid" base prices.

---

## The choices John has to make

### Choice 1. How to treat children in typical-week households

They are 29.0% of weighted children under 5 in 2024 and 6.0% in 2019. Options:

- **a, current**: keep them; the code reads their typical-week hours as if they were diary hours.
- **b, drop and reweight**: drop them and rake calendar households' child weights to the full sample on child age, poverty group, family type, whether the primary caregiver worked last week (D1a), and region. Interview month cannot be a raking variable, because almost no calendar household was interviewed after May (section 1).
- **c, typical-week answers mapped more faithfully**: as in a, except that regular typical-week arrangements with zero recorded hours (75 arrangements in 2024, 21 in 2019) still count as care, with hours drawn from other typical-week arrangements of the same kind.
- **d, impute from similar calendar households**: each typical-week child takes the care category, hours and paid arrangements of a calendar child drawn from the same cell of age, poverty group, family type, caregiver D1a answer and region (20 random draws averaged).

| Option | Parent Only 2019 / 2024 / change | High-priced center 2019 / 2024 | Other Unpaid 2019 / 2024 | Other Paid 2019 / 2024 | Mean price per hour, high-priced center 2019 / 2024 | Paid home-based price 2019 / 2024 | Other Paid price 2019 / 2024 |
|---|---|---|---|---|---|---|---|
| a current | 52.6 / 45.9 / −6.7 | 7.3 / 10.6 | 13.5 / 16.7 | 8.1 / 8.4 | $9.24 / $11.98 | $5.51 / $7.60 | $5.37 / $9.18 |
| b drop, reweight | 52.5 / 47.1 / −5.4 | 7.3 / 10.7 | 13.2 / 16.2 | 8.2 / 7.2 | $9.36 / $12.54 | $5.45 / $7.61 | $5.42 / $8.79 |
| c faithful mapping | 52.6 / 45.4 / −7.2 | 7.3 / 10.6 | 13.6 / 17.2 | 8.1 / 8.4 | $9.24 / $11.98 | $5.51 / $7.60 | $5.37 / $9.18 |
| d hot-deck imputation | 52.7 / 46.9 / −5.8 | 7.3 / 10.3 | 13.1 / 16.6 | 8.2 / 7.2 | $9.34 / $12.54 | $5.41 / $7.54 | $5.35 / $8.82 |

How much 2019 moves: under b, Parent Only −0.13 points and the paid home-based price $5.51 → $5.45; under c, −0.03 points and no price change; under d, +0.10 points and $5.51 → $5.41. The paid home-based and Other Paid figures are the model's 2019 base prices, so b and d would change `supply_2019.yaml` and the price models slightly. Weekly hours barely move under any option: the share of children with 20 or more hours, which is all the model takes from NSECE care hours, changes by at most 2.5 points in any category (section 3).

### Choice 2. Whether a regular provider who did not happen to give care in the reference week counts as care

The calendar records care only for the diary week. In calendar households only 13% (2019) and 23% (2024) of regular-not-last-week providers have any hours, so the model drops the rest; 4.9% (2019) and 3.4% (2024) of calendar children under 5 end up as Parent Only although they have such a provider. Typical-week households have no such category: they report their usual providers with usual hours. The option is:

- **e, usual-week concept for everyone**: option c, plus calendar households' regular-not-last-week providers with zero hours count as care, with hours drawn from typical-week arrangements of the same kind (689 arrangements in 2019, 308 in 2024).

| Option | Parent Only 2019 / 2024 / change | High-priced center 2019 / 2024 | Other Unpaid 2019 / 2024 | Paid home-based price 2019 / 2024 | High-priced center price 2019 / 2024 |
|---|---|---|---|---|---|
| current concept (a) | 52.6 / 45.9 / −6.7 | 7.3 / 10.6 | 13.5 / 16.7 | $5.51 / $7.60 | $9.24 / $11.98 |
| usual week (e) | 47.9 / 43.0 / −5.0 | 7.5 / 10.8 | 17.2 / 18.9 | $5.68 / $7.46 | $9.60 / $12.03 |

This option moves 2019 a great deal: Parent Only −4.7 points, Other Unpaid +3.7 points, and the paid home-based and Other Paid base prices up 3% and 4%.

### Choice 3. Parents whose work hours are missing

Hours are missing for 5.1% of two-parent primary caregivers, 5.7% of partners and 3.7% of single parents in 2024 (children under 5, child weights); the model counts them as not employed. Options:

- **a, current**.
- **b, extend the existing donor rule**: parents with a missing hours record who said yes to D1a get hours drawn from working parents of the same earner rank, as the current rule does for parents with zero hours; those who said no get zero. New draws use their own random stream so the existing draws are unchanged.
- **c, D1a decides employment for every parent, hours imputed**: as b, but hours for every D1a-yes parent without positive hours (including those the current rule already imputes) are drawn from working parents in the same cell of family type, sex and questionnaire version.

| Option | Single parent employed 2019 / 2024 | Single parent full-time 2019 / 2024 | Two-parent primary caregiver employed 2019 / 2024 | Primary caregiver full-time 2019 / 2024 | Partner employed 2019 / 2024 |
|---|---|---|---|---|---|
| a current | 56.3 / 64.5 | 39.9 / 46.0 | 51.5 / 58.6 | 31.0 / 36.8 | 89.4 / 84.9 |
| b extend donor rule | 57.0 / 68.0 | 40.6 / 49.4 | 51.8 / 62.2 | 31.3 / 38.7 | 89.8 / 89.8 |
| c D1a status, cell-imputed hours | 57.0 / 68.0 | 40.4 / 48.7 | 51.8 / 62.2 | 31.7 / 39.8 | 89.8 / 89.8 |
| for reference: said yes to D1a | 57.8 / 68.6 | | 51.9 / 62.6 | | 90.3 / 90.3 |

Option a is clearly broken in 2024: 3.8% of partners and 2.9% of two-parent primary caregivers said they worked last week but are coded not employed because their diary has a missing block, and the 2024 partner employment rate falls 4.5 points while the D1a answer does not move at all.

How much 2019 moves: under b, 30 children under 5 (0.48% of the weighted 2019 parent-unit sample) have a primary caregiver whose employment state (none, part-time, full-time) changes; employment rises 0.7 points for single parents, 0.4 for two-parent primary caregivers and 0.5 for partners. Under c, 102 children (1.83%) change, because the earlier imputed hours are redrawn from new cells. Either rule could be applied to 2024 only, which leaves the 2019 output byte-identical.

### Choice 4. Which week defines employment in typical-week households

The care data of typical-week households describe a typical week in May (summer interviews) or a typical fall week, but D1a, which the current code uses to override work hours, describes the week before the interview. In 2024 summer interviews, 6.1% of two-parent primary caregivers said they did not work last week but reported positive May hours (current rule: not employed), and 11.8% worked last week but reported zero May hours (current rule: donor hours, employed). Options:

- **D1a decides** (current rule, and b and c above).
- **d, reference-period consistent**: in typical-week households, employment and hours come from the typical-week hours; D1a is used only where hours are missing. Calendar households as in b.

| Option | Single parent employed 2019 / 2024 | Two-parent primary caregiver employed 2019 / 2024 | Primary caregiver full-time 2019 / 2024 | Partner employed 2019 / 2024 |
|---|---|---|---|---|
| b (D1a decides) | 57.0 / 68.0 | 51.8 / 62.2 | 31.3 / 38.7 | 89.8 / 89.8 |
| d (typical week decides) | 57.4 / 67.6 | 52.4 / 61.1 | 31.5 / 38.5 | 90.0 / 89.6 |

Within typical-week households the difference is larger: two-parent primary caregivers employed 67.4% under b against 63.5% under d in 2024, and 53.8% against 63.2% in 2019 (219 children). Under d, 61 children under 5 in 2019 (0.99% weighted) have a caregiver whose employment state changes, all in the 2019 summer households.

### Choice 5. Whether the 2019 baseline may change

| Option | What changes in 2019, against the current output |
|---|---|
| Care b | Parent Only −0.13 points; other categories within 0.4 points; paid home-based base price $5.51 → $5.45, Other Paid $5.37 → $5.42 |
| Care c | Parent Only −0.03, Other Unpaid +0.03, Unpaid Home-Based +0.01; prices unchanged |
| Care d | Parent Only +0.10; other categories within 0.4 points; paid home-based $5.51 → $5.41, Other Paid $5.37 → $5.35 |
| Care e | Parent Only −4.7, Other Unpaid +3.7; paid home-based $5.68, Other Paid $5.59, high-priced center household price $9.60 |
| Hours b | Employment +0.4 to +0.7 points; 0.48% of children change caregiver state |
| Hours c | Same employment rates as b; 1.83% of children change caregiver state |
| Hours d | Employment +0.6 to +1.1 points; 0.99% of children change caregiver state |

Any of these can instead be written as a rule that applies only when `nsece_year = 2024`, which keeps 2019 byte-identical but makes the two years' processing differ.

---

## 1. What typical-week households answered, how the code treats them, and who they are

### What they were asked

From the 2024 household questionnaire (PDF pp. 19, 46–49, 70–73) and user's guide (HH-6 to HH-10, HH-61, HH-62, HH-285, HH-335):

- **Version switch by date.** Item CHECK_S routes cases completed between May 31 and August 12, 2024 to the summer version ("a typical week in May"); CHECK_SY routes cases completed on or after August 12 to the "new school year" version ("a typical week", meaning late summer or fall). `HH4_METH_QUEXVERSION` (1 calendar, 2 summer, 3 fall) and `HH4_METH_REFPERIOD` (1 last week, 2 typical week in May, 3 typical week) record this; they disagree only for 10 fall cases that were asked about May.
- **Care (section C).** Instead of the provider roster and diary, items C3_S/C4_S (summer) or C3_SY/C4_SY (fall) ask "Did anyone regularly care for [child] in a typical week [in May]? By regularly I mean at least five hours each week," for up to four providers per child, then C4a (in the home or not), C4b ("How many hours per week did [provider] usually care for [child]?"), CS_1 (hours between 8 a.m. and 6 p.m. on weekdays) and CS_2 (hours the parents were in work-related activities while the child was with that provider). Irregular care (under 5 hours a week) is not asked. The type-of-care items and the payment section J are asked as for calendar households.
- **Work (section D).** D1a is asked of everyone, about last week. Instead of the adult diary, DS_1/DSY_1 ask "In a typical week [in May], how many hours did you spend working and commuting to and from work?" with follow-ups on standard and non-standard hours (DS_2 to DS_4). Unlike the diary, this excludes school and training.
- **What reaches the public file.** The raw typical-week items are not in the public file. The survey's constructed variables combine both routes: `HRS_WEEK_PROV` (hours per child and provider) holds the C4b usual hours for typical-week households, and `WST_HRS_TOT_PHI/PLO` (hours of the child's parent with the most and second-most work-related hours) hold the DS_1/DSY_1 hours. `MISSING_STATUS_CC` is 0 ("no calendar data") for every typical-week child.
- The guide says typical-week providers are coded as "last week" providers in `LASTWEEK`. The data disagree: 744 of 967 typical-week arrangements of children under 5 in 2024 carry `LASTWEEK = 0` (regular but not last week). The model does not read `LASTWEEK`, so this does not affect it.

2019 had the same design with one alternative version, the summer questionnaire ("a typical week in May"). Its stated cutoff was completion on or after May 29, 2019 (2019 questionnaire, CHECK_S), but in the data the summer version ran from June 27 to July 28, 2019 (487 households) and calendar interviews continued through June 27.

### How the current processing treats them

File `src/1_processing/1a_nsece_processing.R`, commit `c05fefa`:

- The code never reads `METH_QUEXVERSION`, `METH_REFPERIOD`, `SUMMERQUEX`, `LASTWEEK` or `COMPLETE_DATE` (none appears anywhere in `src/`). Typical-week and calendar households go through identical code.
- Hours per child and provider are renamed at lines 146–153, selected at line 533 and kept as `hours` at line 590. For typical-week households these are usual hours; for calendar households, diary hours.
- Price per hour is weekly payment ÷ hours (line 632), so typical-week prices use usual hours.
- An arrangement counts only if regular, categorised and with hours above 0 (lines 698–703); a child with arrangements but none usable becomes Parent Only (lines 721, 731); 20 hours splits part-time from full-time care (line 739).
- Work hours: the per-child records of the two parents with the most work-related hours are built at lines 343–369, and records with a negative code are dropped at line 370 (`filter(hhm_id > 0, hours >= 0)`). D1a is read at lines 383–394 and overrides the hours at lines 405–417 (did not work: 0; worked with 0 hours: donor hours). Hours reach the parent units at lines 430–442. In step 1b, `src/1_processing/1b_acs_processing.R` line 70 turns missing hours into "not employed".

So for typical-week households the model combines a typical-week care measure, typical-week work hours and a last-week employment status.

### Is interview month the whole story?

Almost entirely (`csv/ch_t1_households_by_version_and_month.csv`, `csv/ch_t1_date_range_by_version.csv`):

| | 2019 | 2024 |
|---|---|---|
| Calendar interviews | January 8 – June 27 (8,089 households) | December 29, 2023 – October 5, 2024 (4,660) |
| Typical-week interviews | Summer: June 27 – July 28 (487) | Summer: June 3 – August 19 (970); fall: August 19 – November 9 (773) |
| Households interviewed before June | all calendar (7,155) | all calendar (4,510) |
| Households interviewed June or later | 934 calendar (all in June), 487 summer | 150 calendar, 1,743 typical week |

Every interview before June was a calendar interview in both years. In 2024, 92% of interviews from June onward were typical-week interviews. The 150 late calendar households (117 children under 5, 2.2% of weighted children) are the only calendar households after May; they look unusual (73.8% Parent Only among the 100 interviewed June–August), so they cannot stand in for the summer and fall population. Interview timing, not a household's choice, decides who has a calendar.

### Who they are

Children under 5, weighted percent, calendar / typical-week households (`csv/ch_t1_composition_children_under5_by_version.csv`):

| Characteristic | 2019 calendar / summer | 2024 calendar / typical week |
|---|---|---|
| Primary caregiver worked for pay last week (D1a) | 51.0 / 55.4 | 60.1 / 68.2 |
| Primary caregiver employed, model rule | 50.9 / 49.1 | 56.4 / 65.1 |
| One-parent family | 29.6 / 26.6 | 25.2 / 26.8 |
| Under 100% of poverty | 24.2 / 20.9 | 21.3 / 25.7 |
| 100–199% of poverty | 23.8 / 15.5 | 22.7 / 18.3 |
| 400% of poverty or more | 23.7 / 28.9 | 31.4 / 31.8 |
| Midwest | 20.4 / 31.2 | 19.9 / 22.5 |
| West | 24.4 / 25.4 | 24.7 / 21.1 |
| Age 0; age 4 | 16.5, 21.3 / 19.1, 22.5 | 16.4, 20.0 / 16.2, 21.4 |
| Children under 5 (unweighted) | 5,927 / 363 | 3,050 / 1,166 |

The clearest difference in 2024 is employment: primary caregivers in typical-week households are 8 points more likely to have worked last week. Age mix, family type and income are close. Part of the employment difference may reflect the season of the work question itself, since D1a refers to a summer or fall week for these households.

---

## 2. Seasonality

2019 fieldwork ran January to July and was 94% calendar; 2024 ran January to November and was 71% calendar (children under 5). Care shares by interview season under the current treatment (`csv/ch_t4_care_shares_by_interview_month_and_season.csv`), percent:

| Year, season (version) | Children | Parent Only | High-priced center | Low-priced center | Unpaid center | Other Paid | Other Unpaid |
|---|---|---|---|---|---|---|---|
| 2019 Jan–Feb (calendar) | 1,869 | 53.4 | 7.5 | 6.3 | 8.1 | 8.7 | 12.2 |
| 2019 Mar–May (calendar) | 3,370 | 51.1 | 7.5 | 7.4 | 7.8 | 7.9 | 13.6 |
| 2019 June (calendar) | 688 | 58.9 | 5.1 | 4.9 | 6.2 | 7.7 | 15.0 |
| 2019 June–July (summer, typical week in May) | 363 | 52.0 | 8.1 | 7.2 | 2.2 | 7.7 | 17.1 |
| 2024 Jan–Feb (calendar) | 1,225 | 49.8 | 10.7 | 7.4 | 7.5 | 6.2 | 15.1 |
| 2024 Mar–May (calendar) | 1,710 | 45.9 | 10.4 | 7.3 | 7.4 | 8.0 | 17.0 |
| 2024 June–Aug (summer, typical week in May) | 643 | 42.3 | 11.3 | 7.8 | 5.3 | 13.6 | 16.3 |
| 2024 Sept–Nov (fall, typical week) | 397 | 38.6 | 11.2 | 6.6 | 7.8 | 7.7 | 23.5 |

- Within the calendar months there is no steady trend. By month, Parent Only runs 63.9 (January, 332 children), 51.4, 49.9, 48.3, 56.8 (May) in 2019 and 44.1, 52.1, 47.1, 41.4, 50.3 in 2024. May is higher than April in both years.
- The 2019 baseline already contains a seasonal piece: 10.6% of 2019 calendar children were interviewed in June, when the diary week falls after many programs have ended for the summer. Their Parent Only share is 58.9% and their paid center share 10.0%, against 51–53% and about 14–15% earlier in the year.
- In 2024 the typical-week seasons show less Parent Only care than the calendar seasons. Section 3 shows this comes from household composition and from the difference between "last week" and "usual" care, not obviously from the season itself.
- Comparing the same months in both years (calendar households, January–May only; `csv/ch_t4_care_shares_calendar_january_to_may_both_years.csv`): Parent Only 51.9 → 47.4 (−4.5), high-priced center 7.5 → 10.5 (+3.0), low-priced center 7.0 → 7.4, Other Unpaid 13.1 → 16.3 (+3.2), Other Paid 8.2 → 7.3.
- Weekly hours (`csv/ch_t4_weekly_hours_by_season.csv`): the share of non-parental care users with 20 or more hours is 71–73% in 2019 January–May and 74–75% in every 2024 season. Mean hours of home-based and other care are lower in the 2024 summer and fall seasons (29.0 and 30.5) than in 2024 calendar households (35.9 in January–February, 34.3 in March–May); center hours are similar throughout (31–34).

---

## 3. Treatments of typical-week households: method and full results

### Why typical-week households show less Parent Only care

`csv/ch_t2_parent_only_with_regular_provider.csv`, `csv/ch_t2_regular_arrangements_by_last_week_flag.csv`:

- In calendar households, regular providers who did not give care in the diary week have positive hours in only 13% (2019) and 23% (2024) of cases, and the hours-above-zero rule drops the rest. 4.9% (2019) and 3.4% (2024) of calendar children under 5 are Parent Only although they have such a provider.
- In typical-week households, 87–100% of regular arrangements have hours, because the question asks about usual providers. Only 0.6–2.3% of their children are Parent Only while having a regular provider.

An approximate accounting of the 2024 gap (Parent Only 48.1% in calendar households, 40.6% in typical-week households: 7.5 points). Imputing typical-week children from calendar children with the same age, poverty group, family type, caregiver employment and region (option d) gives them 44.0%, so differences in these characteristics account for about 4.1 points. Counting regular-not-last-week providers in calendar households (option e) lowers their Parent Only share by 3.4 points, to 44.7%. Together these account for roughly the whole gap. Each piece carries sampling error of a point or more.

### Method details

- **a** uses the 1a output as it stands.
- **b** drops typical-week children and rakes calendar children's weights to the full-sample totals (children with a care category) for child age, poverty group, family type, primary caregiver's D1a answer and region. Raking converged in both years; weights changed by factors of 0.95–1.14 (2019) and 1.08–2.00 (2024), with a negligible increase in weight variation (`csv/ch_t3_b_raking_diagnostics.csv`). Interview month was not used because calendar households interviewed after May are too few and atypical.
- **c** keeps a, except that regular arrangements in typical-week households with zero hours (75 arrangements of 74 children in 2024; 21 of 17 in 2019) get hours drawn, in proportion to child weight, from typical-week arrangements of the same kind (center, home-based, other). Arrangements are then re-classified with the model's rules, because the low/high center split depends on payment per hour. Twenty draws are averaged. These arrangements are almost all unpaid care by relatives or others, so c moves about half a point from Parent Only to Other Unpaid in 2024 and leaves prices unchanged.
- **d** gives each typical-week child the category, hours and paid arrangements of a calendar child drawn in proportion to child weight from the same cell. Cells are tried from finest (age × poverty group × family type × caregiver D1a answer × region) to coarsest (age only), requiring at least 5 donors. In 2024, 1,001 of 1,166 children found donors in the finest cell, 134 in the second, 18 in the third and 13 in the fourth (`csv/ch_t3_d_hotdeck_cell_levels.csv`). Twenty draws are averaged. The donor's paid arrangements enter the price table at the recipient's weight.
- **e** is c plus calendar households' regular-not-last-week arrangements with zero hours (689 arrangements of 580 children in 2019; 308 of 292 in 2024), with hours drawn from typical-week arrangements of the same kind in the same year (`csv/ch_t3_c_e_imputation_counts.csv`).

### Care shares (percent of children under 5), 2019 / 2024

From `csv/ch_t3_care_shares_by_treatment.csv`:

| Category | a current | b drop, reweight | c faithful mapping | d hot deck | e usual week |
|---|---|---|---|---|---|
| High-Priced Center-Based | 7.3 / 10.6 | 7.3 / 10.7 | 7.3 / 10.6 | 7.3 / 10.3 | 7.5 / 10.8 |
| Low-Priced Center-Based | 6.8 / 7.9 | 6.8 / 7.6 | 6.8 / 7.9 | 6.8 / 7.6 | 7.0 / 8.0 |
| Unpaid Center-Based | 7.4 / 6.9 | 7.8 / 7.5 | 7.4 / 6.9 | 7.7 / 7.8 | 7.6 / 7.0 |
| Paid Home-Based | 3.4 / 3.1 | 3.4 / 3.3 | 3.4 / 3.1 | 3.4 / 3.1 | 3.5 / 3.2 |
| Other Paid | 8.1 / 8.4 | 8.2 / 7.2 | 8.1 / 8.4 | 8.2 / 7.2 | 8.3 / 8.4 |
| Unpaid Home-Based | 0.8 / 0.7 | 0.8 / 0.5 | 0.8 / 0.7 | 0.8 / 0.5 | 0.9 / 0.7 |
| Other Unpaid | 13.5 / 16.7 | 13.2 / 16.2 | 13.6 / 17.2 | 13.1 / 16.6 | 17.2 / 18.9 |
| Parent Only | 52.6 / 45.9 | 52.5 / 47.1 | 52.6 / 45.4 | 52.7 / 46.9 | 47.9 / 43.0 |

For reference, the two groups separately under a: calendar households 52.7 / 48.1 Parent Only, typical-week households 52.0 / 40.6.

### Share of children with 20 or more hours, by category, 2019 / 2024

From `csv/ch_t3_weekly_hours_by_treatment.csv` (mean hours are in the same file):

| Category | a | b | d | e |
|---|---|---|---|---|
| High-Priced Center-Based | 63.2 / 73.2 | 62.9 / 72.8 | 62.9 / 70.9 | 62.9 / 73.2 |
| Low-Priced Center-Based | 89.4 / 82.6 | 89.7 / 82.2 | 89.7 / 84.2 | 89.6 / 82.9 |
| Unpaid Center-Based | 67.6 / 78.0 | 67.5 / 76.2 | 67.9 / 77.4 | 67.9 / 78.0 |
| Paid Home-Based | 92.3 / 77.9 | 91.5 / 78.3 | 90.8 / 79.6 | 91.8 / 77.7 |
| Other Paid | 71.4 / 80.9 | 71.7 / 83.4 | 72.3 / 83.0 | 71.7 / 80.7 |
| Unpaid Home-Based | 88.4 / 76.7 | 87.5 / 79.1 | 87.8 / 76.5 | 87.1 / 76.7 |
| Other Unpaid | 70.8 / 67.5 | 69.5 / 69.7 | 69.5 / 69.6 | 72.5 / 67.9 |

Option c equals a to within 0.5 points in every cell. Mean weekly hours under a, 2019 / 2024: high-priced center 27.5 / 30.5, low-priced center 40.4 / 36.8, unpaid center 28.7 / 31.1, paid home-based 38.4 / 34.7, Other Paid 31.1 / 31.7, Other Unpaid 38.6 / 33.2. Under the other options most means are within 2.5 hours of these (the largest gap is 3.5 hours, for the small Unpaid Home-Based group).

### Household-reported price per hour, 2019 / 2024

From `csv/ch_t3_household_price_per_hour_by_treatment.csv`. Mean (median), dollars:

| Category | a current | b drop, reweight | d hot deck | e usual week |
|---|---|---|---|---|
| High-Priced Center-Based | 9.24 (7.27) / 11.98 (10.20) | 9.36 (7.34) / 12.54 (10.88) | 9.34 (7.34) / 12.54 (10.88) | 9.60 (7.29) / 12.03 (10.20) |
| Low-Priced Center-Based | 2.69 (2.86) / 2.99 (3.33) | 2.69 (2.86) / 3.00 (3.32) | 2.69 (2.86) / 3.04 (3.32) | 2.69 (2.86) / 2.99 (3.33) |
| Paid Home-Based | 5.51 (4.50) / 7.60 (5.00) | 5.45 (4.25) / 7.61 (4.50) | 5.41 (4.25) / 7.54 (4.50) | 5.68 (4.62) / 7.46 (5.00) |
| Other Paid | 5.37 (3.50) / 9.18 (5.81) | 5.42 (3.50) / 8.79 (6.10) | 5.35 (3.50) / 8.82 (6.10) | 5.59 (3.52) / 9.22 (5.86) |

Option c gives the same prices as a. Typical-week households in 2024 report lower high-priced center prices than calendar households (mean $10.73 against $12.54; median $8.62 against $10.88), so the options that replace them with calendar households (b and d) raise the 2024 high-priced center price by about 5%. The 2019-to-2024 rise in the mean high-priced center price is 30% under a, 34% under b and d, and 25% under e.

---

## 4. Missing work hours

### Why hours are missing

`csv/wh_t1_reasons_hours_missing.csv` classifies every parent in a model parent unit whose hours are missing, using the survey's codes on the record of the parent unit's first child (the record the model uses). Percent of children under 5 in each role, 2024:

| Reason | Single parent | Two-parent primary caregiver | Partner |
|---|---|---|---|
| Calendar household, survey code −10 ("at least one parent of the child has at least one missing block in the calendar"); both parents' records blanked | 2.2 | 4.0 | 4.0 |
| Typical-week household, hours coded −10 although the parent is identified (hours question not answered) | 1.5 | 1.1 | 1.4 |
| Parent not among the two parents the survey's work-hours variables cover | 0 | 0 | 0.4 |
| Total missing | 3.7 | 5.1 | 5.7 |
| Of which said yes to D1a | 3.5 | 3.6 | 4.9 |

The calendar code −10 occurs in 211 calendar households; 135 of them are also flagged by `HH4_ADCAL_QUALITY_FLAG` (adult calendars with identical days but no start or end times; `csv/wh_t5_2024_adult_calendar_flag_vs_minus10.csv`). In 2019 the code −10 occurs in 2 households. The 2019 missing hours (2.5% of single parents, 2.7% of primary caregivers) come almost entirely from the summer households: when parents did not work, the survey codes the child as having no parent in the household (−8) or only one parent (−9), so 41% of 2019 summer primary caregivers have no hours record. Most of these said no to D1a, so counting them as not employed is correct for them.

### Why the branch's donor imputation does not reach them

The donor rule (lines 405–417) operates on `parent_hours`, which is built from `parent_labor_supply` after line 370 has removed every record with a negative member number or negative hours. A parent whose record carries −10, −8 or −9 never enters `parent_hours`, so the rule `worked %in% 1L & hours == 0 ~ donor_hours` never sees them. Their hours stay missing after the joins at lines 430–442, and step 1b (line 70) codes them as not employed. The rule covers only parents with a valid record showing zero hours.

### Employment by treatment

Percent of children under 5 in a parent unit (child weights), 2019 / 2024 (`csv/wh_t3_employment_by_treatment.csv`; split by questionnaire version in `csv/wh_t3_employment_by_treatment_long.csv`):

| Role and measure | a current | b extend donor rule | c D1a status, cell-imputed hours | d typical week decides | Said yes to D1a |
|---|---|---|---|---|---|
| Single parent, employed | 56.3 / 64.5 | 57.0 / 68.0 | 57.0 / 68.0 | 57.4 / 67.6 | 57.8 / 68.6 |
| Single parent, full-time (35+ hours) | 39.9 / 46.0 | 40.6 / 49.4 | 40.4 / 48.7 | 40.7 / 49.0 | |
| Two-parent primary caregiver, employed | 51.5 / 58.6 | 51.8 / 62.2 | 51.8 / 62.2 | 52.4 / 61.1 | 51.9 / 62.6 |
| Two-parent primary caregiver, full-time | 31.0 / 36.8 | 31.3 / 38.7 | 31.7 / 39.8 | 31.5 / 38.5 | |
| Partner, employed | 89.4 / 84.9 | 89.8 / 89.8 | 89.8 / 89.8 | 90.0 / 89.6 | 90.3 / 90.3 |
| Partner, full-time | 79.1 / 72.8 | 79.6 / 76.8 | 80.5 / 78.1 | 79.6 / 76.9 | |
| Hours still missing (two-parent primary caregiver) | 2.7 / 5.1 | 0.0 / 0.3 | 0.0 / 0.3 | 0.0 / 0.3 | |

The remaining 0.3% after b, c and d are parents with no usable D1a answer (don't know, or among the 85 partners skipped in error, code −33). The model uses only the primary caregiver's state (none, part-time, full-time) as a feature in the care-choice forest; partner hours do not enter training.

### How much each treatment moves 2019

`csv/wh_t4_changes_relative_to_current.csv`: parents and children whose employment state changes relative to the current output. The parent counts cover all parent units, including those whose children are all 5 or older; the child counts cover children under 5.

| Option | 2019 parents changed | 2019 primary caregivers changed | 2019 children under 5 affected (weighted %) | 2024 children under 5 affected (weighted %) |
|---|---|---|---|---|
| b | 67 | 39 | 30 (0.48%) | 119 (3.57%) |
| c | 240 | 132 | 102 (1.83%) | 164 (4.77%) |
| d | 131 | 91 | 61 (0.99%) | 235 (7.50%) |

Under b, 25 of the 30 affected 2019 children are in summer-questionnaire households whose caregiver has no hours record but said yes to D1a; the other 5 are calendar cases (code −10, or a parent the work-hours variables do not cover). These caregivers gain donor hours. The existing donor draws are untouched because the new draws use a separate random stream (seed 76 + 8).

### D1a against reference-period hours

`csv/wh_t2_d1a_vs_reference_period_hours.csv`, two-parent primary caregivers, percent of children:

| Group | Worked last week, positive reference-period hours | Worked last week, zero hours | Did not work last week, positive hours | Did not work, zero hours |
|---|---|---|---|---|
| 2024 calendar | 56.9 | 2.4 | 2.1 | 38.0 |
| 2024 summer (May hours) | 57.9 | 11.8 | 6.1 | 24.2 |
| 2024 fall (typical-week hours) | 60.5 | 3.5 | 2.0 | 34.0 |
| 2019 summer (May hours; parents with no record not shown) | 84.1 | — | 15.9 | — |

The disagreement in 2024 summer households (18% of primary caregivers) is four times that in calendar households (4.5%), which is what a mismatch of reference weeks would produce. In calendar households the disagreement mostly reflects school and training hours, which the diary counts and D1a does not.

---

## 5. Files

Folder: `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/nsece2024_options/calendar_hours/`

Scripts (`scripts/`), each run through Slurm with `scripts/run_r.sh` (account pi_nrs36, partition day, 32 GB, R 4.4.1):

- `common.R`: paths, raw-file readers (reuses the model's `read_nsece_file()`), helpers.
- `01_explore.R`: diagnostics printed to the log (version by month, last-week flags, reserve codes).
- `02_calendar_treatments.R`: tasks 1–3 (timing, composition, mechanism, care treatments a–e, seasonality).
- `03_work_hours.R`: task 4 (copies the 1a work-hours block verbatim, checks it against 1a, classifies reasons, treatments a–d).
- `04_checks.R`: adult-calendar quality flag against code −10.

Logs: `logs/`. Tables (`csv/`):

- `ch_t1_households_by_version_and_month.csv`, `ch_t1_date_range_by_version.csv`, `ch_t1_composition_children_under5_by_version.csv`, `ch_t1_2024_children_june_october_by_version.csv`
- `ch_t2_parent_only_with_regular_provider.csv`, `ch_t2_regular_arrangements_by_last_week_flag.csv`
- `ch_t3_care_shares_by_treatment.csv` (and `_long`), `ch_t3_weekly_hours_by_treatment.csv` (and `_long`), `ch_t3_household_price_per_hour_by_treatment.csv` (and `_long`), `ch_t3_b_raking_diagnostics.csv`, `ch_t3_c_e_imputation_counts.csv`, `ch_t3_d_hotdeck_cell_levels.csv`
- `ch_t4_care_shares_by_interview_month_and_season.csv`, `ch_t4_weekly_hours_by_season.csv`, `ch_t4_care_shares_calendar_january_to_may_both_years.csv`
- `wh_t0_check_against_1a.csv`, `wh_t1_reasons_hours_missing.csv`, `wh_t2_d1a_vs_reference_period_hours.csv`, `wh_t3_employment_by_treatment.csv` (and `_long`), `wh_t4_changes_relative_to_current.csv`, `wh_t5_2024_adult_calendar_flag_vs_minus10.csv`

Limits: the treatments are applied to the 1a outputs only. Calibration and the care-choice forest were not re-run, so the effect on simulated outcomes is not measured. Options c, d and e use random draws (20 averaged; seeds fixed in the scripts). There are 363 (2019) and 1,166 (2024) typical-week children under 5, so differences of about a point between options are within sampling error.
