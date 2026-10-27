# Moving the ECEC Simulator to the 2024 NSECE: decisions and evidence

Draft of 2026-10-27. This memo collects the choices that have to be made before the model can be recalibrated on the 2024 National Survey of Early Care and Education, and what the experiments of 16 and 17 October show about each. It describes the options and their consequences; it does not choose among them. The supporting reports are in this directory, listed at the end, together with the state of the code and the work that can start before the decisions are made.

The calibration runs made on 17 October (a trial 2024 calibration, four 2024 variants, and four 2019 cleanup runs) all finished, but their results have not yet been compared with the 2019 reference calibration. The sections that depend on them say so.


## Background

The 2024 public-use files (ICPSR 39466) contain every variable the model uses, under the 2019 names with the year digit changed from 9 to 4. The household survey is now split into a main file and a separate weekly calendar file. Step 1a of the model now reads either year (commit `c05fefa`), and its 2019 output is unchanged.

The 2024 household sample is smaller (6,403 households against 8,576). Fieldwork ran from January to November 2024, after the pandemic stabilization grants ended but during the last months in which other American Rescue Plan child care funds could be spent.

The model as it stands carries 2019 forward with general indices: care-worker wages by economy-wide pay per hour (+29.4 percent to 2024), the non-labor part of prices by CPI-U (+22.7 percent), and household income by nominal GDP per person (+31.7 percent). Its care shares barely move from 2019 to 2026.


## Decision 1: whether to reweight the 2024 household sample

The 2024 survey reports far more maternal employment than outside sources. Among children under 5 living with a parent, the share whose mother worked last week was:

| Source | 2019 | 2024 | Change |
|---|---|---|---|
| NSECE | 53.6% | 64.3% | +10.7 points |
| March CPS | 57.1% | 59.6% | +2.5 points |
| ACS | 61.1% | 62.6% (2023) | +1.5 points |

Fathers agree across all three surveys at about 90 percent. The 2019 survey is below both benchmarks and the 2024 survey is above both. Interview timing does not explain the difference, and reweighting on income, family type and education does not remove it.

Reweighting each year's child weights to match the CPS or ACS on mothers' employment by family type shrinks most of the 2019-to-2024 change in care shares:

| Change from 2019 to 2024 | Survey weights | Reweighted |
|---|---|---|
| Parent-only care | −6.7 points | −2.2 to −3.5 |
| All center care | +3.8 | +1.8 to +2.4 |
| Any paid care | +4.2 | +0.1 to +1.3 |
| High-price center care | +3.3 | +2.2 to +2.5 |

After reweighting, no combined change exceeds 1.7 standard errors. The rise in high-price center care is the most robust.

The options are: use the survey weights as published; reweight to the CPS on employment only; reweight to the ACS on employment only; reweight on employment and demographics, with or without income; reweight on demographics only. The employment report gives care shares and employment rates for each.

This also explains the gap between the model and the 2019 survey that the caregiver review found. Measured the same way, 2019 mothers' employment is 51.7 percent in the NSECE and 59.3 percent in the ACS, a real difference between the surveys rather than a matter of units or definitions. The earlier figure of 50.9 against 53.7 percent compared two different measures.

Report: `employment_and_reweighting.md`.


## Decision 2: households without the weekly calendar

Only 73 percent of 2024 households completed the weekly care and work calendar, against 94 percent in 2019. The interview date decides it: every interview before June used the calendar, and from June 2024 on, 92 percent used questions about a typical week instead. Because only 150 calendar households were interviewed after May, calendar households cannot be reweighted to stand in for the others.

Typical-week households report about 7.5 points less parent-only care. About 4 points of that come from their characteristics, mainly more working caregivers. About 3.4 points come from the questions themselves: the calendar records zero hours for a regular provider who did not happen to give care that week, and the model then codes the child as parent-only care.

Share of children under 5 in parent-only care, 2019 and 2024, under each treatment:

| Treatment | 2019 | 2024 |
|---|---|---|
| Current | 52.6% | 45.9% |
| Drop typical-week households and reweight | 52.5% | 47.1% |
| Map typical-week answers more faithfully | 52.6% | 45.4% |
| Impute from similar calendar households | 52.7% | 46.9% |
| Count usual providers in both years | 47.9% | 43.0% |

Only the last option moves 2019 materially. Hours by category barely move under any option. Dropping or imputing raises the 2024 price households report for high-price center care by about 5 percent.

Report: `calendar_and_work_hours.md`.


## Decision 3: parents with missing work hours

The current treatment is broken for 2024. Parents whose calendar has a missing block (survey code −10) are dropped before the step that fills in hours from similar workers, so they are coded as not working. Extending that step to them gives:

| Group | Current, 2024 | Extended donor step, 2024 | "Worked last week" answer, 2024 |
|---|---|---|---|
| Two-parent primary caregivers | 58.6% | 62.2% | 62.6% |
| Single parents | 64.5% | 68.0% | 68.6% |
| Partners | 84.9% | 89.8% | 90.3% |

The fix moves 2019 employment by 0.4 to 0.7 points. It can be applied to 2024 only if 2019 results must stay identical. A related choice is which week defines employment for typical-week households: taking it from typical-week hours rather than the "worked last week" answer gives 61.1 percent instead of 62.2 for primary caregivers.

Report: `calendar_and_work_hours.md`.


## Decision 4: how to build center prices

The 2019 public file deleted posted rates outside the 5th and 95th percentiles; the 2024 file caps them at those values. The model does no trimming of its own, so 40 impossible 2019 rates above $50 an hour remain in the data. Treating both years the same way:

| High-price center price per child-hour | 2019 | 2024 | Change |
|---|---|---|---|
| Model as it stands | $8.08 | $9.04 | +12% |
| Consistent rules (range across rules) | $6.70–$6.92 | $9.01 | +26% to +40% |

The low-price center price moves from $3.15–$3.31 to $4.30–$4.61 under the same rules.

Posted center rates rose about 31 percent, against 21 percent for the CPI for day care and preschool. Between a third and a half of the gap comes from a different mix of centers: more for-profit chains and fewer public-school and nonprofit centers. Most of the rest comes from how the CPI is built: it includes family child care homes, whose prices rose only 10 to 14 percent, and it follows the same providers over time. The CPI for day care is therefore a poor index for carrying center prices forward.

Report: `prices.md`.


## Decision 5: the cut-off between low- and high-price centers

Families are split at $4.75 an hour in 2019 dollars. The 2024 share of children in high-price centers depends on how the cut-off is carried forward:

| Rule | 2024 cut-off | High-price share, 2024 |
|---|---|---|
| Within-year weighted median | $6.68 | 9.2% |
| $4.75 grown by CPI day care (current trial default) | $5.76 | 10.6% |
| $4.75 not grown | $4.75 | 11.8% |

The 2019 share is 7.3 percent. A fixed cut-off grown by an index assumes prices rose with that index, which they did not (+36 percent against +21). A within-year percentile makes the split independent of the price level, but it fixes the share of families on each side of the cut by construction.

Report: `prices.md`.


## Decision 6: home-based and "other paid" prices

Home-based posted prices are hourly in 2019 and weekly in 2024. Converted with each provider's own hours open, they rose 13 to 14 percent; dividing by 40 instead gives about 50 percent. The model's household-based home-based price rose 38 percent (mean $5.51 to $7.60), driven by more part-time and higher-income arrangements; the median rose only from $4.56 to $5.00.

The "other paid" price rose from $5.37 to $9.18 an hour. Most of the rise is in-home care by paid non-relatives (nanny-type care), whose share of the category went from 14 to 26 percent and whose median price went from $7.75 to $18.75 an hour. About a quarter of the rise is the shift between care types.

Report: `prices.md`.


## Decision 7: the supply side

**Negative non-labor cost at low-price centers.** In 2024 the low-price sector's labor cost ($4.77 per child-hour) exceeds its price ($4.30). Wages alone would leave it positive; the measured rise in staff per child-hour, especially staff with a bachelor's degree (16 to 22 percent of staff), flips it. Part of the rise is how the files are built: the 2024 file keeps extreme posted rates that 2019 dropped ($0.21 to $0.35 of the −$0.47); the model counts a larger share of workers in 2024 (+5 percent on every ratio); and directors' own staff counts show low-price staffing up 5 percent against 22 percent in the workforce-based measure. Options:

| Option | Low-price non-labor cost, 2024 |
|---|---|
| Current method | −$0.47 |
| Pool low- and high-price staffing | −$0.30 |
| Drop extreme 2024 rates as in 2019 | −$0.26 |
| Value each sector's staff at its own wages | +$0.26 |
| Staff from directors' reports, same centers as prices | +$0.53 |
| Floor at zero | $0 (model price $4.77, 11% above posted) |

With a negative residual, labor exceeds 100 percent of the low-price price, so the existing 25 percent payroll credit would cut that price by 28 percent.

**Unweighted home-based workers.** Home-based workers enter the labor stock without survey weights in both years. The 2019 match between labor demand and the labor stock (91 and 78 percent) is two offsetting errors: center demand is 72 percent of the stock and home-based demand is 24 times it. Weighting home-based workers fixes the second and exposes the first, so first-year wages would start 5 percent (no degree) and 15 percent (degree) below survey wages. This is a 2019 problem as well.

**Market response.** Under 2024 parameters, 10 percent more paid center hours raises care wages about 2 to 3 percent and the low-price price 2.4 percent, against 1.6 percent in 2019. No option breaks the equilibrium solver.

Report: `supply_side.md`.


## Decision 8: the calibration year and ACS pairing

The model pairs the NSECE with the ACS of the same year. There is no 2024 ACS on disk, so the trial pairs the 2024 survey with the 2023 ACS and converts 2024 dollars to 2023 dollars. The calibration year is a setting (`--nsece-year`; see the README section "Calibration year"). Two further points came out of the trial. The 2023 ACS file uses the IPUMS tax-unit identifier, because the corrected Census crosswalk exists only for 2019; how well that identifier keeps married couples together in 2023 has not been settled (see `docs/ecec_team_taxid_memo_2026-08-17.md` for the 2019 problem). And the code that sets the base year for employment targeting read a setting that never existed and so always used 2019; that was correct for 2019 runs by coincidence.

The trial 2024 calibration finished (run `202610161837`) but has not been compared with the 2019 reference (`202610150828`), and the list of every place the code assumes 2019 was not written.


## How much care choices changed with the population held fixed

The model's care-choice forest was trained separately on the 2019 and 2024 surveys and applied to the same population: the 2019 ACS children in the setup run, with their observed employment. The difference measures the change in care choices that is not due to who has young children. Changes are in percentage points of all children under 5; the range in brackets is the middle 95 percent of 60 bootstrap draws over NSECE households.

| Care category | Raw survey change | Population held fixed, model forest | Population held fixed, regularized forest |
|---|---|---|---|
| Parent only | −6.7 | −2.9 | −3.0 [about −5 to −0.5] |
| High-price center | +3.3 | +3.0 | +3.6 [about +2 to +5] |
| Unpaid relative and other | +3.2 | +2.8 | +3.0 |
| Other paid | — | −1.6 | −1.9 |
| Paid home-based | — | −1.3 | −0.9 |
| All paid care | +4.2 | −0.4 | −0.1 |

The direction holds under every treatment tried: CPS or ACS reweighting, the missing-hours fix, calendar households only, the usual-week care concept, and both forest settings. Across them, parent-only care falls 1.6 to 3.0 points and high-price center care rises 2.5 to 3.7 points. Measuring income in real dollars instead of as a within-survey rank shrinks the change further (parent-only −1.1 to −1.5, high-price center +1.4 to +2.5). The changes are concentrated in families where the caregiver does not work (parent-only care down 6 to 7 points, paid care up about 5) and in the top quarter of income (parent-only care down 6 to 8 points, high-price centers up 7 to 8.5). The 2024 recoding of some "other" care into center care was not adjusted for and probably overstates the center rise.

So the change in preferences is real but modest: a move from parent and other paid care toward high-price centers and unpaid care, with little change in total paid care. Most of the raw change in paid care comes from the different 2024 sample.

The model's forest settings overfit. On held-out NSECE children its log loss (a measure of forecast error; lower is better) is 2.66 in 2019 and 2.94 in 2024, worse than a forecast that ignores family characteristics (1.55 and 1.62), and it gives near-zero probability to the actual category of 15 to 18 percent of children. A regularized forest scores 1.35 and 1.42. The two forests give nearly the same 2019-to-2024 change, so the overfitting affects levels rather than the change.

Report: `care_choice_shift.md`.


## Calibration runs not yet compared

All of these setup runs finished (output folders under `/nfs/roberts/project/pi_nrs36/shared/model_data/ECEC-Simulator/`). None has been compared with the 2019 reference yet.

| Run | What it is |
|---|---|
| `202610150828` | 2019 reference calibration (beta 0.000218, rho 0.0315, paid care elasticity −0.92) |
| `202610161837` | 2024 survey, 2023 ACS, no options |
| `202610170153` | Same as `202610161837` with a different random seed, to show how much results move by chance |
| `202610170133` | 2024 with `--nsece-2024-options hours` |
| `202610170135` | 2024 with `weights` |
| `202610170140` | 2024 with `supply` |
| `202610170142` | 2024 with all three |
| `202610170130`, `0132`, `0139`, `0136` | 2019 with the cleanup fixes (home-based weighting, posted-rate trimming, missing hours) one at a time and all together. These come from a separate branch, `fix/nsece-2019-cleanup`, which is not part of this branch because the fixes change current 2019 results. |

The repository's comparison script, `analysis/compare_setup.R` in the review folder, reads the ACS files by their 2019 names and stops on 2024 runs. A multi-run version that handles both years is `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/nsece2024_options/variants2024/scripts/compare_variants.R`.


## State of the code on this branch

This branch (`feat/nsece-2024`) sits on top of three commits that are not yet in main: the NSECE primary-caregiver fix (PR #7), the caregiver-first ordering of parent ages, and point-in-time employment in both surveys. The 2024 work adds three commits:

- `c05fefa`: step 1a reads either survey year (`run_nsece_processing(nsece_year)`); 2019 output is byte-for-byte unchanged.
- `f14f0a2` and `afa1164`: the `--nsece-year` flag runs the whole setup on 2024, paired with the 2023 ACS, with a separate price-model cache for each year.
- `fb56186`: the `--nsece-2024-options` flag (`hours`, `weights`, `supply`), all off by default. The `weights` option reads `config/nsece_reweight/nsece2024_child_weight_factors_cps2024.csv`, built by `variants2024/scripts/build_v2_weights.R` in the analysis folder.

Run without the new flags, the branch reproduces the 2019 model. The code was written and tested by AI agents with reproduction checks (identical 2019 output; option results matching the studies above), but has not been reviewed line by line.

Work that does not depend on the decisions above:

- Review the four commits above.
- Make the setup comparison script work on 2024 runs, and compare the runs in the table above with the 2019 reference.
- List every place the code still assumes 2019 (dollar base year, growth factors, employment targeting, tax donors, caches).
- Check how the 2023 ACS tax units keep married couples together, and whether a 2024 ACS extract can be obtained.


## Documents in this directory

- `decisions.md`: this memo.
- `variable_inventory.md`: every 2019 NSECE variable the model uses, its codes, and the code that reads it.
- `survey_design_changes.md`: what changed in the 2024 survey's design, questionnaire and public-use files, variable by variable.
- `employment_and_reweighting.md`, `calendar_and_work_hours.md`, `prices.md`, `supply_side.md`, `care_choice_shift.md`: the option studies summarized above.

These were written on 16 and 17 October. The scripts and tables behind them are on the cluster under `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/` (`nsece2024*` and `nsece2024_options/`), which the studies refer to by path. That folder also holds three further first-day reports: the 2019-to-2024 growth in child care prices and wages against the model's indices, the first run of the model's processing on both years, and the model's own 2024–2026 projection.
