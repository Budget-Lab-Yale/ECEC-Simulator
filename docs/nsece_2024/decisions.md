# Moving the ECEC Simulator to the 2024 NSECE: decisions and evidence

John Ricco's review of 16 and 17 October 2026, updated on 28 October 2026. Each decision below starts with its status. The current record of the code, the decisions made on 27 and 28 October, and the open items is `docs/nsece_2019_to_2024_crosswalk.md`. The option studies in this directory (listed at the end) record the review runs of 16 and 17 October. Those runs paired the 2024 NSECE with the 2023 ACS and used a $5.76 price cutoff for 2024. The code now does neither.


## Background

The 2024 public-use files (ICPSR 39466) contain every variable the model uses, under the 2019 names with the year digit changed from 9 to 4. The household survey is now split into a main file and a separate weekly calendar file. Step 1a reads either year. Its 2019 output now differs from `main`; the crosswalk lists each change (don't-know costs, home-based weights, and administrative-data centers).

The 2024 household sample is smaller (6,403 households against 8,576). Fieldwork ran from January to November 2024, after the pandemic stabilization grants ended but during the last months in which other American Rescue Plan child care funds could be spent.


## Decision 1: whether to reweight the 2024 household sample

Status: open (crosswalk Open item 4). The `weights` option is off by default.

The 2024 survey reports far more maternal employment than outside sources. Among children under 5 living with a parent, the share whose mother (or the one parent) worked last week was:

| Source | 2019 | 2024 | Change |
|---|---|---|---|
| NSECE | 53.6% | 64.3% | +10.7 points |
| March CPS | 57.1% | 59.6% | +2.5 points |
| ACS | 61.1% | 62.9% | +1.8 points |

The review had only the 2023 ACS (62.6%). The 2024 ACS figure comes from check `11_acs_mother_at_work.R`, which reproduces the review's 2019 and 2023 figures. The 2024 NSECE is 1.4 points above it. The review's ACS raking targets are still the 2023 ones.

Fathers agree across all three surveys at about 90 percent. The 2019 survey is below both benchmarks and the 2024 survey is above both. Interview timing does not explain the difference, and reweighting on income, family type, and education does not remove it.

Reweighting each year's child weights to match the CPS or ACS on mothers' employment by family type shrinks most of the 2019-to-2024 change in care shares (review runs; care shares under the review's rules, not the current ones):

| Change from 2019 to 2024 | Survey weights | Reweighted |
|---|---|---|
| Parent-only care | −6.7 points | −2.2 to −3.5 |
| All center care | +3.8 | +1.8 to +2.4 |
| Any paid care | +4.2 | +0.1 to +1.3 |
| High-price center care | +3.3 | +2.2 to +2.5 |

The options are: use the survey weights as published; reweight to the CPS on employment only; reweight to the ACS on employment only; reweight on employment and demographics, with or without income; reweight on demographics only. Under the current rules, `weights` cuts the fall in Parent Only from 5.6 to 3.1 points (crosswalk Open item 4).

Report: `employment_and_reweighting.md`.


## Decision 2: households without the weekly calendar

Status: open. No option exists.

Only 73 percent of 2024 households completed the weekly care and work calendar, against 94 percent in 2019. The interview date decides it: every interview before June used the calendar, and from June 2024 on, 92 percent used questions about a typical week instead. Because only 150 calendar households were interviewed after May, calendar households cannot be reweighted to stand in for the others.

Typical-week households report about 7.5 points less parent-only care. About 4 points of that come from their characteristics, mainly more working caregivers. About 3.4 points come from the questions themselves: the calendar records zero hours for a regular provider who did not happen to give care that week, and the model then codes the child as parent-only care.

Share of children under 5 in parent-only care, 2019 and 2024, under each treatment (review runs, review rules):

| Treatment | 2019 | 2024 |
|---|---|---|
| Review rules | 52.6% | 45.9% |
| Drop typical-week households and reweight | 52.5% | 47.1% |
| Map typical-week answers more faithfully | 52.6% | 45.4% |
| Impute from similar calendar households | 52.7% | 46.9% |
| Count usual providers in both years | 47.9% | 43.0% |

Only the last option moves 2019 materially. Under the current rules (don't-know costs missing, children with no category in the denominator), Parent Only is 50.91% in 2019 and 45.31% in 2024 (crosswalk Open item 4).

Report: `calendar_and_work_hours.md`.


## Decision 3: parents with missing work hours

Status: decided on 28 October (crosswalk mismatch item 3). The fill is always on for 2024; it is no longer an option. It draws donors by earner rank and hourly wage band.

Parents whose calendar has a missing block (survey code −10) are dropped before the step that fills in hours from similar workers, so they are coded as not working. Extending that step to them gives (review runs):

| Group | Without `hours`, 2024 | With `hours`, 2024 | "Worked last week" answer, 2024 |
|---|---|---|---|
| Two-parent primary caregivers | 58.6% | 62.2% | 62.6% |
| Single parents | 64.5% | 68.0% | 68.6% |
| Partners | 84.9% | 89.8% | 90.3% |

The same fix would move 2019 employment by 0.4 to 0.7 points; the option applies to 2024 only. A related choice is which week defines employment for typical-week households: taking it from typical-week hours rather than the "worked last week" answer gives 61.1 percent instead of 62.2 for primary caregivers.

Report: `calendar_and_work_hours.md`.


## Decision 4: how to build center prices

Status: open. Two fixes are measured as test variants (crosswalk mismatch items 2 and 4).

Both public files censor posted rates at the 5th and 95th percentiles within each reported time unit. The 2019 file deletes the censored rates; the 2024 file sets each one to the cap value. The model does no trimming of its own, so 40 posted 2019 rates above $50 an hour stay in the data (38 of them at an age with enrollment, in 32 of the model's center-by-age-bin records). Treating both years the same way (review runs):

| High-price center price per child-hour | 2019 | 2024 | Change |
|---|---|---|---|
| Public files as released | $8.08 | $9.04 | +12% |
| Consistent rules (range across rules) | $6.70–$6.92 | $9.01 | +26% to +40% |

The low-price center price moves from $3.15–$3.31 to $4.30–$4.61 under the same rules.

Posted center rates rose about 31 percent, against 21 percent for the CPI for day care and preschool. Between a third and a half of the gap comes from a different mix of centers: more for-profit chains and fewer public-school and nonprofit centers. Most of the rest comes from how the CPI is built: it includes family child care homes, whose prices rose only 10 to 14 percent, and it follows the same providers over time. The CPI for day care is therefore a poor index for carrying center prices forward.

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

Status: open. No option exists.

Home-based posted prices are hourly in 2019 and weekly in 2024. Converted with each provider's own hours open, they rose 13 to 14 percent; dividing by 40 instead gives about 50 percent. The model's household-based home-based price rose 38 percent (mean $5.51 to $7.60), driven by more part-time and higher-income arrangements; the median rose only from $4.56 to $5.00.

The "other paid" price rose from $5.37 to $9.18 an hour. Most of the rise is in-home care by paid non-relatives (nanny-type care), whose share of the category went from 14 to 26 percent and whose median price went from $7.75 to $18.75 an hour. About a quarter of the rise is the shift between care types.

Report: `prices.md`.


## Decision 7: the supply side

Negative non-labor cost at low-price centers. Status: open (crosswalk Open item 2, sector wages). In 2024 the low-price sector's labor cost exceeds its price; the residual is −$0.47 per child-hour. Options measured in the review:

| Option | Low-price non-labor cost, 2024 |
|---|---|
| Current method | −$0.47 |
| Pool low- and high-price staffing | −$0.30 |
| Drop 2024 rates beyond the 5th and 95th percentiles of all units | −$0.26 |
| Value each sector's staff at its own wages | +$0.26 |
| Staff from directors' reports, same centers as prices | +$0.53 |
| Floor at zero | $0 (model price $4.77, 11% above posted) |

Deleting only the 2024 cap values, the 2019 rule, gives −$0.25 (crosswalk mismatch item 4). With a negative residual, labor exceeds 100 percent of the low-price price, so the existing 25 percent payroll credit would cut that price by 28 percent.

Home-based workers. Status: decided and in the code. Home-based enrollment and workers now carry provider weights (`hb_weight`) in both years (crosswalk, Home-based labor totals).

Administrative-data centers. Status: decided for free centers, open for paying ones. Free admin centers join Unpaid Center-Based with their own staff count, in both years. Paying admin centers are in no sector (crosswalk mismatch item 5).

Market response. Under the review's 2024 parameters, 10 percent more paid center hours raises care wages about 2 to 3 percent and the low-price price 2.4 percent, against 1.6 percent in 2019. No option broke the equilibrium solver.

Report: `supply_side.md`.


## Decision 8: the calibration year and ACS pairing

Status: decided on 27 October. The model pairs the 2024 NSECE with the 2024 ACS, never the 2023 ACS. The 2024 ACS file is `raw_data/ACS/v1/2024/ipums_usa.csv` (config key `ACS-2024`). It has no `TAXID` or `ADJGINC`, so `1b` stops for 2024 until a source of 2024 tax units and AGI exists; SPM variables are also missing (crosswalk Open item 3). The calibration year is a setting (`--nsece-year`; see the README section "Calibration year").


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

The direction holds under every treatment tried: CPS or ACS reweighting, the missing-hours fix, calendar households only, and the usual-week care concept. Measuring income in real dollars instead of as a within-survey rank shrinks the change further (parent-only −1.1 to −1.5, high-price center +1.4 to +2.5). The changes are concentrated in families where the caregiver does not work (parent-only care down 6 to 7 points, paid care up about 5) and in the top quarter of income (parent-only care down 6 to 8 points, high-price centers up 7 to 8.5). The 2024 recoding of some "other" care into center care was not adjusted for and probably overstates the center rise.

So the change in preferences is real but modest: a move from parent and other paid care toward high-price centers and unpaid care, with little change in total paid care. Most of the raw change in paid care comes from the different 2024 sample.

The review also tried other forest settings. That was an experiment; Anna kept the original settings on 28 October.

Report: `care_choice_shift.md`.


## Documents in this directory

- `decisions.md`: this memo.
- `survey_design_changes.md`: what changed in the 2024 survey's design, questionnaire, and public-use files, variable by variable.
- `employment_and_reweighting.md`, `calendar_and_work_hours.md`, `prices.md`, `supply_side.md`, `care_choice_shift.md`: the option studies of 16 and 17 October.

The scripts and tables behind them are on the cluster under `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/` (`nsece2024*` and `nsece2024_options/`), which the studies refer to by path.
