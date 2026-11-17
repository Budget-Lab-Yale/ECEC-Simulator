# Moving the ECEC Simulator to the 2024 NSECE: decisions and evidence

John Ricco's review of 16 and 17 October 2026, kept current by Anna through 17 November 2026. Each decision starts with its status. The full record of the code and the open items is `docs/nsece_2019_to_2024_crosswalk.md`. The option studies in this directory (listed at the end) record the review runs of 16 and 17 October. Those runs paired the 2024 NSECE with the 2023 ACS and used a $5.76 price cutoff for 2024. The code now does neither. Figures marked "review runs" use the review's rules, not the current ones.


## Background

The 2024 public-use files (ICPSR 39466) contain every variable the model uses, under the 2019 names with the year digit changed from 9 to 4. The household survey is now split into a main file and a separate weekly calendar file. Step 1a reads either year. Its 2019 output now differs from `main`. The crosswalk lists each change (don't-know costs, home-based weights, and administrative-data centers).

The 2024 household sample is smaller (6,403 households against 8,576). Fieldwork ran from January to November 2024, after the pandemic stabilization grants ended but during the last months in which other American Rescue Plan child care funds could be spent.


## Decision 1: whether to reweight the 2024 household sample

Status: open (crosswalk Open item 4). The `weights` option is off by default.

The 2024 survey reports far more maternal employment than outside sources. Among children under 5 living with a parent, the share whose mother (or the one parent) worked last week was:

| Source | 2019 | 2024 | Change |
|---|---|---|---|
| NSECE | 53.6% | 64.3% | +10.7 points |
| March CPS | 57.1% | 59.6% | +2.5 points |
| ACS | 61.1% | 62.9% | +1.8 points |

The review had only the 2023 ACS (62.6%). The 2024 ACS figure comes from check `11_acs_mother_at_work.R`, which reproduces the review's 2019 and 2023 figures. The 2024 NSECE is 1.4 points above it. The 2024 ACS raking targets were built on 29 October from the review's code (factor range 0.805 to 1.802).

Fathers agree across all three surveys at about 90 percent. The 2019 survey is below both benchmarks and the 2024 survey is above both. Interview timing does not explain the difference, and reweighting on income, family type, and education does not remove it.

Reweighting each year's child weights to match the CPS or ACS on mothers' employment by family type shrinks most of the 2019-to-2024 change in care shares (review runs):

| Change from 2019 to 2024 | Survey weights | Reweighted |
|---|---|---|
| Parent-only care | −6.7 points | −2.2 to −3.5 |
| All center care | +3.8 | +1.8 to +2.4 |
| Any paid care | +4.2 | +0.1 to +1.3 |
| High-price center care | +3.3 | +2.2 to +2.5 |

The options are: use the survey weights as published, reweight to the CPS on employment only, reweight to the ACS on employment only, reweight on employment and demographics (with or without income), or reweight on demographics only. Under the rules of 29 October, `weights` cuts the fall in Parent Only from 5.6 to 3.1 points with CPS targets and to 4.8 with ACS targets, and it changes no supply parameter except the 2024 home-based residual (crosswalk Open item 4).

Points for the decision (17 November). The repo holds 2024 factors only, so the option treats the two years differently, while the two surveys miss the benchmarks in opposite directions. The care-choice forest trains on a weighted resample (`sample_n(..., weight = model_weight)` in `1b`), so the option reaches the forest, but the forest already conditions on the caregiver's employment. Its effect on model outputs is not measured. That needs a 2024 run, which needs a tax-unit crosswalk.

Report: `employment_and_reweighting.md`.


## Decision 2: households without the weekly calendar

Status: open. No option is in the code.

Only 73 percent of 2024 households completed the weekly care and work calendar, against 94 percent in 2019. The interview date decides it: every interview before June used the calendar, and from June 2024 on, 92 percent used questions about a typical week instead. Only 150 calendar households were interviewed after May, so calendar households cannot be reweighted to stand in for the others. In the review runs, typical-week households report about 7.5 points less Parent Only care. About 4 points of that come from their characteristics, mainly more working caregivers. About 3.4 points come from the questions themselves: the calendar records zero hours for a regular provider who did not happen to give care that week, and the model then codes the child as Parent Only care.

The options, rerun on current code on 17 November (cluster `agr55/nsece_evidence/cal1117`, job 33682914; the review's scripts reproduce every current 1a care category in both years). Children under 5 with a care category, child weights:

| Option | Parent Only 2019 / 2024 | Change |
|---|---|---|
| a, current | 54.3% / 47.1% | −7.2 points |
| b, drop typical-week households and rake | 54.3% / 48.0% | −6.2 |
| c, map typical-week answers more faithfully | 54.3% / 46.5% | −7.7 |
| d, care of similar calendar children | 54.5% / 47.8% | −6.7 |
| e, count usual providers in every household | 49.5% / 44.1% | −5.3 |

In 2024, calendar households show 49.0% Parent Only care and typical-week households 42.4%. Only option e moves 2019 materially (−4.9 points). With children who have no care category in the denominator, Parent Only is 50.91% in 2019 and 45.31% in 2024 under the rules of 29 October (crosswalk Open item 4). The review's own figures for options a to e were 52.6%, 52.5%, 52.6%, 52.7%, and 47.9% in 2019 and 45.9%, 47.1%, 45.4%, 46.9%, and 43.0% in 2024.

For option d, rules were tested on calendar children reweighted to look like typical-week children (10 folds, repeated 20 times; 5,792 children in 2019 and 2,983 in 2024, unweighted). A random forest on the review's five traits with child weights misses the 8 care shares by 0.5 points (2019) and 1.4 points (2024), against 1.8 and 3.1 for the review's single-draw donor rule and 1.8 for a cell average in 2024.

Report: `calendar_and_work_hours.md`.


## Decision 3: parents with missing work hours

Status: decided and in the code (28 October, revised by Anna on 6 November; crosswalk mismatch item 3). The fill is always on, in both years. A 2024 parent with no hours record but an own adult calendar takes the calendar's hours. Every other parent who worked last week without hours draws a donor of the same earner rank, child's weekly care hours, household income quintile, wage band, region, and urban density. The 28 October rule (earner rank and wage band only) reversed the link between parents' full-time work and their child's care hours. The fill covers parents' work hours only, not children's care (decision 2).

Before the fill, parents whose calendar had a missing block (survey code −10) were coded as not working. The fill gives (review runs):

| Group | Without fill, 2024 | With fill, 2024 | "Worked last week" answer, 2024 |
|---|---|---|---|
| Two-parent primary caregivers | 58.6% | 62.2% | 62.6% |
| Single parents | 64.5% | 68.0% | 68.6% |
| Partners | 84.9% | 89.8% | 90.3% |

The fill applies to 2019 too (36 parents with no hours record who worked last week). A related choice is which week defines employment for typical-week households: taking it from typical-week hours rather than the "worked last week" answer gives 61.1 percent instead of 62.2 for primary caregivers.

Report: `calendar_and_work_hours.md`.


## Decision 4: how to build center prices

Status: decided and in the code. Weekly rates above $2,000 are set to missing in both years (28 October; crosswalk mismatch item 2). Censored rates are filled in both years with the lognormal mean beyond the cut (Anna, 6 November; mismatch item 4). This replaces the first part of the `supply` option, which deleted the 2024 caps.

Both public files censor posted rates at the 5th and 95th percentiles within each reported time unit. The 2019 file deletes the censored rates. The 2024 file sets each one to the cap value. The review found 40 posted 2019 rates above $50 an hour left in the data (38 of them at an age with enrollment, in 32 of the model's center-by-age-bin records). Treating both years the same way (review runs):

| High-price center price per child-hour | 2019 | 2024 | Change |
|---|---|---|---|
| Public files as released | $8.08 | $9.04 | +12% |
| Consistent rules (range across rules) | $6.70–$6.92 | $9.01 | +26% to +40% |

The low-price center price moves from $3.15–$3.31 to $4.30–$4.61 under the same rules.

Posted center rates rose about 31 percent, against 21 percent for the CPI for day care and preschool. Between a third and a half of the gap comes from a different mix of centers: more for-profit chains and fewer public-school and nonprofit centers. Most of the rest comes from how the CPI is built. It includes family child care homes, whose prices rose only 10 to 14 percent, and it follows the same providers over time. The CPI for day care is therefore a poor index for carrying center prices forward.

Report: `prices.md`.


## Decision 5: the cut-off between low- and high-price centers

Status: decided on 27 October (crosswalk, Low/high center price cutoff). The cutoff is the within-year weighted median hourly price of paid center care for the youngest child under 5: $4.75 in 2019 and $6.86 in 2024. Under the current rules, 7.01% of children under 5 are in high-price centers in 2019 and 9.03% in 2024 (child weights, children with no category in the denominator).

The review compared these rules for 2024 (review runs):

| Rule | 2024 cut-off | High-price share, 2024 |
|---|---|---|
| Within-year weighted median, all paid center care | $6.68 | 9.2% |
| $4.75 grown by CPI day care (review setting, not used) | $5.76 | 10.6% |
| $4.75 not grown | $4.75 | 11.8% |

A fixed cut-off grown by an index assumes prices rose with that index, which they did not (+36 percent against +21). A within-year percentile makes the split independent of the price level, but it fixes the share of families on each side of the cut by construction.

Report: `prices.md`.


## Decision 6: home-based and "other paid" prices

Status: open. Options tested, none in the code.

Home-based posted prices are hourly in 2019 and weekly in 2024. Tested on current code on 17 November (cluster `agr55/nsece_evidence/price1117`, job 33683742; children under 5, child weights, standard errors from 200 bootstrap resamples of households). Benchmarks 2019 to 2024: CPI-U +22.7%, average wage index +29.1% (Macro-Projections v3 `2026071916`).
- Paid home-based, household side (208 and 125 arrangements, unweighted): the current mean of hourly prices rises 38.0% ($5.51 to $7.60, standard error 25.1 points), driven by high hourly prices on short part-time arrangements. The median rises from $4.50 to $5.00 (the review had $4.56). Total paid divided by total hours rises 18.2% ($4.86 to $5.74, standard error 15.6). Trimming arrangements under 5 hours or capping at the 99th percentile barely changes the mean.
- Posted home-based prices (listed providers, provider weights, weighted medians): converted with each provider's own hours open, 2024 weekly prices rise 12% to 14% over 2019 hourly prices (standard errors about 4). Dividing by 40 gives 48% to 51%, because listed providers are open a median 51 hours a week.
- Other Paid (522 and 375 regular paid arrangements): the current mean rises 70.4% ($5.39 to $9.18, standard error 21.0; the review had $5.37). At the 2019 mix of subtypes it rises 55.1% (standard error 16.2), and without nanny-type care 42.1%. Nanny-type care (in the child's home, no prior relationship) went from 17.1% to 26.2% of the category, at a mean of $8.76 and $16.29 an hour. The review, with a broader definition of in-home care by paid non-relatives, had a share of 14 to 26 percent and a median of $7.75 to $18.75.
- Proposed: total paid divided by total hours for the home-based household price, in both years; each provider's own hours open for the 2024 posted price; the current Other Paid mean, with the 2019-mix figure as a check.

Report: `prices.md`.


## Decision 7: the supply side

Negative non-labor cost at low-price centers. Status: decided (16 November). The model keeps one wage pair for all centers and accepts the negative residual, which carries the differences between sectors into model results. Sector wages are not used (crosswalk Open item 2). In 2024 the low-price sector's labor cost exceeds its price. The residual was −$0.47 per child-hour when the options were measured and is −$0.45 with the admin-data programs in supply. Options measured in the review:

| Option | Low-price non-labor cost, 2024 |
|---|---|
| Current method (then) | −$0.47 |
| Pool low- and high-price staffing | −$0.30 |
| Drop 2024 rates beyond the 5th and 95th percentiles of all units (the review's version, replaced on 29 October) | −$0.26 |
| Value each sector's staff at its own wages | +$0.26 |
| Staff from directors' reports, same centers as prices | +$0.53 |
| Floor at zero | $0 (model price $4.77, 11% above posted) |

Deleting only the 2024 cap values, the 2019 rule, gives −$0.25 (crosswalk mismatch item 4). With a negative residual, labor exceeds 100 percent of the low-price price, so the existing 25 percent payroll credit would cut that price by 28 percent.

Home-based wages. Status: decided for now (Anna, 17 November). The model keeps pricing home-based labor at the center wage pair (2024: $16.80 an hour without a BA, $24.22 with one). The option reviewed was the 2024 ACS median wage of self-employed child care workers in the child day care industry ($11.54 and $14.52, from 1,481 and 282 unweighted records). Listed providers' 2023 income in the 2024 NSECE ($10.68 per adult-hour open, median) is revenue before expenses and gives no wage.

Home-based workers. Status: decided and in the code. Home-based enrollment and workers carry provider weights (`hb_weight`) in both years (crosswalk, Home-based labor totals).

Administrative-data centers. Status: decided and in the code. Since 12 November, admin centers are in supply with their own staff count: the free ones in Unpaid, the paying ones split 72% Low-priced and 28% High-priced in 2024, with no price. Since 17 November, the 623 staff counts NORC imputed in 2024 are refit by a random forest on log size, funding, poverty, and urban density (crosswalk mismatch item 7 and *Administrative-data centers*). From 6 to 12 November, admin centers were dropped from supply. A check on 9 November found that neither center size nor age mix explains why admin centers report fewer staff per child than interviewed public-school centers.

Children with no parent. Status: decided and in the code (Anna, 17 November). Children under 5 with no parent in the household have no parent unit, so calibration and demand already left them out, and their care used no provider slots. They stay out of the choice model, and their care now enters demand as a fixed amount: NSECE care shares of no-parent children by single year of age, applied to aged ACS child weights (`compute_no_parent_demand()`). A holdout test (cluster `agr55/nsece_evidence/npcv1117`, 10 folds repeated 50 times, 239 children in 2019 and 79 in 2024, unweighted) compared rules. Shares by age came within 2% of held-out market child-hours in both years. The `1b` care forest overstated 2024 market child-hours by 32%, and random donors or cell averages matched on income or region understated them by 9% to 20%.

Base-year labor (`L_0`). Status: open (crosswalk Open item 1). Calibration fits demand at survey wages and never reads `L_0`. In the 2019 base year, labor demanded is 80.7% (no BA) and 72.8% (BA) of `L_0`, so the solved wage is 5.2% and 14.7% below the survey wage. Proposed: set `L_0` to labor demanded at survey prices after calibration.

Market response. Under the review's 2024 parameters, 10 percent more paid center hours raises care wages about 2 to 3 percent and the low-price price 2.4 percent, against 1.6 percent in 2019. No option broke the equilibrium solver.

Report: `supply_side.md`.


## Decision 8: the calibration year and ACS pairing

Status: the pairing is decided (27 October): the model pairs the 2024 NSECE with the 2024 ACS. The calibration year is a setting (`--nsece-year`; see the README section "Calibration year"). The 2024 ACS file is `raw_data/ACS/v1/2024/ipums_usa.csv` (config key `ACS-2024`). IPUMS has not released `TAXID`, `ADJGINC`, or the SPM variables for 2024, so `1b` stops for 2024 unless a tax-unit crosswalk file sits beside the ACS file.

Tax units and AGI. On 9 November Anna chose the Affordability-Index tax units and AGI (John Iselin's code, full chain run for 2019 and 2024) for 2024 runs. `src/misc/make_tax_unit_crosswalk.R` converts his files to the crosswalk, and `--check` reports when they change. Their AGI averages 14% above `ADJGINC` on 2019. Since 16 November, `1b` uses a converted file for 2019 too whenever one sits in the 2019 ACS folder, in place of the Census crosswalk. On 17 November Anna decided not to run with Affordability-Index units until John Iselin's new files arrive (crosswalk Open item 3).

An earlier stand-in, built the same day from ACS income (commit `d89994c`), used all income items (IPUMS extracts 37 and 38) and the school test for students. On 2019 its units match the Census crosswalk for 91.5% of parents of children under 5, and 98.3% fall in the same AGI decile as IPUMS `ADJGINC` (crosswalk Open item 3 has the validation).

Also on 9 November: `1b` counts armed forces at work as working (crosswalk, ACS employment: armed forces at work).


## Decision 9: parents who are not the respondent

Status: decided (10 November, revised 13 November). The public file gives the sex of the respondent only. When a child's lone parent is another adult, `1a` left the parent's sex missing and `1b` dropped the household from care-choice training. These are 13% (2019) and 16% (2024) of single-parent child weight. In about half of them the respondent is a grandparent. They are larger (5.2 to 5.5 members against 4.0 to 4.1), have higher income, and use more relative care and less low-price center care than the single parents kept. Anna chose to keep them.

Since 13 November, after Harris noted that the rules did not match, `1a` uses one rule for every parent whose sex the file does not give:

1. The respondent keeps the respondent's sex.
2. Any other first parent is male if the household counts only fathers (`HHCOMP_NUMMOTHER`, `HHCOMP_NUMFATHER`), and female otherwise. A respondent whose sex is other or refused is treated the same way.
3. The second parent is the opposite sex of the first if the household counts a parent of that sex, or if the counts are missing. Otherwise the second parent is the same sex as the first.

The 10 November rule covered lone parents only: it took the sex from the counts when all counted parents were one sex (67% in 2019 and 71% in 2024 of the dropped child weight) and set the others to mother, the sex of 80% to 84% of the single parents kept. It left two-parent units with no respondent parent without a sex, and `1b` dropped them, and it gave a respondent's partner a sex only when the counts were exact. The 13 November rule keeps those two-parent units, 1.1% (2019) and 1.6% (2024) of children under 5, weighted. It also gives a sex to the partners whose counts were not exact (1.8% and 1.9%) and to respondents whose sex is other or refused (0.1% and 0.2%). Parents with no age stay dropped. The checks are on the cluster under `agr55/nsece_evidence/defs1110/` (`taskc.R`) and `agr55/nsece_evidence/mtg1113/` (`checks_1113.R`, section B).


## How much care choices changed with the population held fixed

The model's care-choice forest was trained separately on the 2019 and 2024 surveys and applied to the same population: the 2019 ACS children in the setup run, with their observed employment. The difference measures the change in care choices that is not due to who has young children. Changes are in percentage points of all children under 5 (review runs, original forest settings, which the model uses):

| Care category | Raw survey change | Population held fixed |
|---|---|---|
| Parent only | −6.7 | −2.9 |
| High-price center | +3.3 | +3.0 |
| Unpaid relative and other | +3.2 | +2.8 |
| Other paid | — | −1.6 |
| Paid home-based | — | −1.3 |
| All paid care | +4.2 | −0.4 |

The direction holds under every treatment tried: CPS or ACS reweighting, the missing-hours fill, calendar households only, and the usual-week care concept. Measuring income in real dollars instead of as a within-survey rank shrinks the change further (parent-only −1.1 to −1.5, high-price center +1.4 to +2.5). The changes are concentrated in families where the caregiver does not work (parent-only care down 6 to 7 points, paid care up about 5) and in the top quarter of income (parent-only care down 6 to 8 points, high-price centers up 7 to 8.5). The review did not adjust for the 2024 recoding of some "other" care into center care, which probably overstates the center rise. Since 12 November the 2019 runs recode 2019 care to the 2024 rules (crosswalk, care types).

The change in preferences is modest: a move from parent and other paid care toward high-price centers and unpaid care, with little change in total paid care. Most of the raw change in paid care comes from the different 2024 sample.

The review also tried other forest settings. That was an experiment. Anna kept the original settings on 28 October.

Report: `care_choice_shift.md`.


## Documents in this directory

- `decisions.md`: this memo.
- `survey_design_changes.md`: what changed in the 2024 survey's design, questionnaire, and public-use files, variable by variable.
- `employment_and_reweighting.md`, `calendar_and_work_hours.md`, `prices.md`, `supply_side.md`, `care_choice_shift.md`: the option studies of 16 and 17 October.

The scripts and tables behind them are on the cluster under `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/` (`nsece2024*` and `nsece2024_options/`), which the studies refer to by path.
