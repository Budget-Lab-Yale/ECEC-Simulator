# Care choices in the 2019 and 2024 NSECE, holding the population fixed

Prepared 2026-10-17. (Written as .txt because writing a .md report was blocked; the content is Markdown.) All numbers come from Slurm jobs; scripts, logs and CSV files are listed at the end. No repository file was edited and nothing was pushed. The model code was read from the worktree `ECEC-Simulator-caregiver-review/nsece2024` (branch `review/nsece-2024`, commit `c05fefa`) and sourced, not changed.

## Answer

**Holding the population fixed, care choices changed by roughly half as much as the raw NSECE comparison suggests.** The model's care-choice forest was trained once on the 2019 NSECE and once on the 2024 NSECE, and both forests were applied to the same 2019 ACS children under 5, each at the employment state their primary caregiver actually had. The difference between the two sets of predictions is the change in care choices for an unchanged population. Results for all children under 5 (2019 forest / 2024 forest / change, in percent of children; the range in brackets is the middle 95 percent of 60 bootstrap draws over NSECE households):

| Care category | Raw NSECE, 2019 / 2024 / change | Fixed population, model's forest settings | Fixed population, regularized forest |
|---|---|---|---|
| Parent Only | 52.6 / 45.9 / −6.7 | 45.6 / 42.7 / **−2.9** [−4.4 to −0.5] | 49.0 / 46.0 / **−3.0** [−5.0 to −1.1] |
| High-Priced Center-Based | 7.3 / 10.6 / +3.3 | 6.7 / 9.7 / **+3.0** [+2.0 to +4.4] | 6.3 / 9.9 / **+3.6** [+1.8 to +5.1] |
| Low-Priced Center-Based | 6.8 / 7.9 / +1.0 | 8.4 / 7.9 / −0.5 [−1.7 to +0.9] | 8.1 / 7.2 / −0.9 [−2.0 to +0.4] |
| Unpaid Center-Based | 7.4 / 6.9 / −0.5 | 7.5 / 7.9 / +0.4 [−1.1 to +1.2] | 7.3 / 7.3 / 0.0 [−1.3 to +0.9] |
| Paid Home-Based | 3.4 / 3.1 / −0.4 | 4.5 / 3.3 / **−1.3** [−2.4 to −0.3] | 3.9 / 3.1 / −0.9 [−1.9 to 0.0] |
| Other Paid | 8.1 / 8.3 / +0.3 | 11.0 / 9.4 / **−1.6** [−3.2 to −0.2] | 10.2 / 8.3 / **−1.9** [−3.2 to −0.9] |
| Unpaid Home-Based | 0.8 / 0.7 / −0.1 | 0.8 / 0.9 / +0.1 | 0.7 / 0.8 / +0.2 |
| Other Unpaid | 13.5 / 16.7 / +3.1 | 15.4 / 18.2 / **+2.8** [+0.8 to +4.5] | 14.5 / 17.4 / **+3.0** [+1.6 to +4.9] |
| All center-based | 21.5 / 25.3 / +3.8 | 22.7 / 25.6 / +2.9 [+0.8 to +4.3] | 21.8 / 24.4 / +2.7 [+0.7 to +4.7] |
| All paid care | 25.7 / 29.9 / +4.2 | 30.6 / 30.2 / −0.4 [−2.4 to +1.7] | 28.6 / 28.5 / −0.1 [−1.4 to +2.8] |
| Full-time non-parental care (20+ hours) | 34.9 / 40.5 / +5.6 | 41.3 / 41.3 / 0.0 [−2.9 to +2.3] | 38.7 / 39.7 / +1.0 [−0.8 to +3.2] |

In words:

- **Parent-only care fell about 3 points** (from about 46–49 percent to 43–46 percent), against 6.7 points in the raw survey comparison. The rest of the raw fall reflects who is in the two samples (more working mothers and more high-income families in the 2024 NSECE), not how a given family chooses.
- **The fall in parent-only care went into high-priced centers (+3.0 to +3.6 points) and unpaid care by relatives and others (+3 points).** Other Paid care (−1.6 to −1.9) and paid home-based care (−0.9 to −1.3) fell, and low-priced centers changed little. Total paid care did not change (−0.4 to −0.1 points), and neither did full-time non-parental care. For a fixed population, then, the shift is a move within paid care toward high-priced centers, plus a move from parent-only care to unpaid care, rather than a move into paid care.
- **The changes are concentrated in two groups:** children whose caregiver is not employed (parent-only care −6 to −7 points; paid care +4 to +5) and children in the top quarter of the income distribution (parent-only −6 to −8; high-priced centers +7 to +8.5). Among children whose caregiver works full time, parent-only care did not fall (+1 point), but high-priced centers rose about 5 points at the expense of other paid arrangements.

**Robustness.** The direction and rough size of the main changes survive every variation tried. Across employment reweighting, the treatment of missing work hours, the two treatments of households without the weekly calendar, and both forest settings, the fall in parent-only care ranges from −1.6 to −3.0 points, the rise in high-priced centers from +2.5 to +3.7, the rise in Other Unpaid from +1.8 to +3.2, and the fall in Other Paid from −1.6 to −2.8. Three things shrink the change:

- Reweighting the NSECE to CPS employment and filling in missing work hours together (the most corrected version) gives a parent-only fall of about 2.0 points (regularized forest: −1.9, bootstrap range −3.5 to −0.3).
- Counting regular providers who did not happen to give care in the diary week (the "usual-week" care concept) gives a parent-only fall of 1.6 to 1.7 points, because it raises non-parental care in 2019 more than in 2024.
- Measuring income in real dollars rather than as a rank within each survey gives a parent-only fall of 1.1 to 1.5 points and a high-priced center rise of 1.4 to 2.5. At every income rank the 2024 NSECE family is richer in real terms than the 2019 one, partly because real incomes grew and partly because the NSECE's high-income share rose more than the CPS or ACS shows (employment report). With income as a rank, as the model uses it, part of what looks like a change in behavior is a change in real income.

**Forest settings.** The model's forest predicts held-out NSECE children worse than a forecast that ignores all family characteristics: its held-out log loss (defined in section 1) is 2.66 in 2019 and 2.94 in 2024, against 1.55 and 1.62 for the no-feature forecast, and it gives a near-zero probability to the actual care category of 15 to 18 percent of held-out children. A regularized forest (fewer candidate features per split, larger leaves, no 50,000-record resample) beats the no-feature forecast (1.35 and 1.42) and was the best of nine settings tried. Even so, the two forests give nearly the same 2019-to-2024 change. The overfitting moves the level of the predicted shares (the model's forest predicts 3.4 points less parent-only care in 2019 than the regularized one) but not the size of the change.

**Pooled forest.** A single forest trained on both years with the survey year as a feature gives smaller changes when the year is switched from 2019 to 2024: parent-only −1.4 (regularized) and −1.9 (model settings), high-priced centers +1.3 and +1.7, Other Unpaid +1.2 and +2.3, paid care flat. The year feature ranks in the lower half of feature importance in the large groups, and adding it does not measurably improve held-out accuracy. That is expected for a 3-point shift in a 50 percent share, which is small relative to the uncertainty about any one child's care. A random forest also tends to understate the effect of a single yes/no feature, so the pooled figures are best read as a lower bound.

## 1. What was done

### The model's care-choice forest

The care-choice step is in `src/1_processing/1b_acs_processing.R` (the `ecec_result` block, from line 1085 of commit `c05fefa`). Children under 5 are grouped into families. Families with three or more children under 5 keep their two youngest for training (the dropped children's weight is spread over the rest; in the ACS the extra children are predicted with the one-child forest). Six forests are trained: one-child and two-child families, each split by whether the family has no identified parent, one parent, or two parents. One-child forests predict the child's care class; two-child forests predict the pair's joint class, which is then split into each child's own probabilities. A care class is one of the seven non-parental categories crossed with part-time or full-time hours (under or at least 20 hours a week), plus Parent Only: 15 classes.

Features: child age and sex (both children in two-child families), household income as a percentile within the survey, household size, Census region, the caregiver's and partner's ages (or the single parent's age and sex), the primary caregiver's employment (none, part time under 35 hours, full time), and in two-child families whether there were originally three or more children under 5.

Model settings: 50,000 records drawn with replacement in proportion to the child weight, then 500 trees with every feature considered at every split (`mtry` = all features) and a minimum leaf of 10 records.

This analysis extracts the model's own data-preparation functions (`prepare_data_for_rf()`, `compute_income_pctiles()`, `get_primary_caregiver()`, `derive_employment_choice()`) from that file by parsing it, and repeats the training and prediction code with the same features and settings.

**Check against the setup run.** The 2019 forest trained here and applied to the 2019 ACS reproduces the setup run `202610150828` to within 0.25 points in every category (parent-only 45.4 against 45.6; high-priced centers 7.0 against 7.0; `csv/validation_against_setup_run.csv`). Forest randomness alone moves the model-setting change by about 0.2 points (standard deviation across four seeds: 0.20 for parent-only care, 0.19 for high-priced centers); for the regularized forest it is under 0.05.

### The fixed population

The 2019 ACS parent units, households and children from the setup run (`/nfs/roberts/project/pi_nrs36/shared/model_data/ECEC-Simulator/202610150828/estimation/data/`): 158,444 children under 5 (19.3 million weighted), the universe the model uses. The saved parent-unit file holds each unit three times, once per employment state; the row for the observed state was kept, and its hours reproduce the observed state for every unit. Every child is predicted at the caregiver's observed employment state. Income percentiles are those of the ACS, computed the model's way.

Reporting groups: child age; quarter of the ACS child-weighted income distribution (the model's income percentile; poverty ratios were not computed for the ACS); family type (one parent, two parents, no parent unit); caregiver employment as observed in the ACS. The 6,350 ACS children with no parent unit (about 4 percent) are predicted by forests trained on very few NSECE records (8 two-child families in 2024), so their changes (parent-only −17 to −22 points) are noise and are not discussed further.

### Dollar features

The forest has no dollar feature other than income, and income enters as a percentile within each survey's own child-weighted distribution, so the model's processing needs no deflation. As a sensitivity ("2024 income on the 2019 real-income scale"), 2024 household income (which refers to calendar 2023) was divided by the ratio of the 2023 to the 2018 CPI-U annual averages (304.702 / 251.107 = 1.213) and placed on the 2019 NSECE percentile cutpoints. On that scale the 2024 NSECE child-weighted median is $60,300 against $54,800 in 2019, and 35 percent of 2024 children are above the 2019 75th percentile.

### Data treatments (experiment 2)

Each treatment was applied to both years, so the comparison stays like for like. Training tables are in `work/nsece_<treatment>_<year>.rds`; `csv/inputs_summary_by_variant.csv` summarizes each, and each reproduces the shares in the earlier reports.

| Treatment | What it does | Parent Only in the NSECE, 2019 / 2024 | Caregiver employed (model rule), 2019 / 2024 |
|---|---|---|---|
| Current | Step 1a output, survey child weights | 52.6 / 45.9 | 52.6 / 60.1 |
| CPS reweighting | Child weights of children under 5 raked to the March CPS share of children by caregiver work status and family type (employment report, option B) | 50.9 / 48.5 | 56.1 / 55.6 |
| ACS reweighting | The same, raked to the ACS (option C; 2024 targets from ACS 2023) | 49.2 / 47.0 | 60.0 / 58.5 |
| Extended donor hours | Parents with a missing work-hours record get donor hours if they said they worked last week, zero otherwise (calendar report, hours option b) | 52.6 / 45.9 | 53.1 / 63.6 |
| CPS reweighting plus extended donor hours | Both of the above | 50.9 / 48.5 | 56.6 / 59.0 |
| Calendar households only | Households without the weekly calendar dropped; calendar children reweighted to the full sample by age, poverty group, family type, caregiver's answer to "worked last week" and region (calendar report, care option b) | 52.5 / 47.1 | 52.9 / 59.8 |
| Usual-week care concept | Regular providers with zero recorded hours (in typical-week households, and regular providers not used in the diary week in calendar households) count as care, with imputed hours; one random draw (calendar report, care option e) | 47.9 / 43.0 | 52.6 / 60.1 |

The treatment functions were taken from the calendar-hours scripts (`calendar_hours/scripts/02_calendar_treatments.R`, `03_work_hours.R`) and checked against the 1a output (zero differences); the raking factors use the employment report's targets (`employment/out/raking_targets.csv`).

### Regularized forest and held-out accuracy (experiment 3)

Held-out accuracy was measured by five-fold cross-validation with folds formed by household, so resampled copies of a child cannot appear on both sides. For each held-out child, the forest's probability for the category the child is actually in was recorded (two-child forests collapsed to each child's own category, as the model uses them). **Log loss** is the weighted average of minus the natural log of that probability, with probabilities below 0.0001 set to 0.0001; lower is better, and a forecast that always gives the true category probability 1 scores 0. The **no-feature forecast** gives every child its family group's weighted class shares from the training folds.

| Setting | Log loss, 2019 | Log loss, 2024 | Held-out children whose actual category got near-zero probability, 2019 / 2024 (%) |
|---|---|---|---|
| No-feature forecast | 1.548 | 1.620 | 0.2 / 0.1 |
| Model settings (resample, all features per split, leaf 10) | 2.658 | 2.944 | 15.3 / 17.6 |
| Original records, all features per split, leaf 10 | 1.475 | 1.520 | 1.2 / 0.8 |
| All features per split, leaf 50 | 1.384 | 1.456 | 0.4 / 0.5 |
| All features per split, leaf 150 | 1.366 | 1.456 | 0.4 / 0.3 |
| Square root of features per split, leaf 10 | 1.378 | 1.427 | 0.4 / 0.3 |
| **Square root of features per split, leaf 50 (the regularized forest used here)** | **1.350** | **1.421** | 0.2 / 0.2 |
| Square root of features per split, leaf 150 | 1.369 | 1.454 | 0.2 / 0.1 |
| Square root of features per split, leaf 400 | 1.436 | 1.550 | 0.2 / 0.1 |

Log loss is for the eight categories; the 15-class version ranks the settings the same way (`csv/heldout_log_loss_by_setting.csv`). Most of the model forest's poor score comes from the 50,000-record resample: with only about 1,000 to 2,000 distinct families in a group, each family is copied 25 to 50 times, so a leaf of 10 records can be a single family and the forest effectively memorizes families. Without the resample, the same `mtry` and leaf size already beat the no-feature forecast. The regularized setting, chosen in advance (the square root of the number of features per split, rounded down; leaf 50; 63 percent of records drawn without replacement for each tree; child weights as case weights), turned out to be the best of the grid.

On one measure the model's forest does well: the average probability it gives the actual category (0.41 in 2019) is as high as the regularized forest's (0.40). Its predictions are right on average but overconfident, which is why its aggregate shares are close to the regularized forest's while its log loss is poor.

### Bootstrap

Each draw resamples, with replacement and separately in each year, the NSECE households that have a child under 5, retrains both years' forests under both settings, and predicts the fixed ACS population. 60 draws were run with the current processing and 30 with CPS reweighting plus extended donor hours. The draws ignore the survey's strata and clusters. As a check, the same household bootstrap applied to the raw NSECE change in parent-only care gives a standard deviation of 1.8 points, close to the design-based standard error of 1.9 in the employment report, so ignoring the design does not make the bands much too narrow. The fixed-population change is less noisy than the raw change (standard deviation 1.05 to 1.10 points for parent-only care) because the population no longer varies from draw to draw.

## 2. Results by group

Change from the 2019 forest to the 2024 forest, percentage points, regularized forest, current processing; middle 95 percent of 60 bootstrap draws in brackets. Model-setting results are in `csv/bootstrap_bands_current.csv` and are similar.

| Group (unweighted ACS children) | Parent Only | High-Priced Center | Low-Priced Center | Paid Home-Based | Other Paid | Other Unpaid | All paid care |
|---|---|---|---|---|---|---|---|
| Age 0 (29,080) | −2.6 [−6.4, 0.0] | +2.7 [+1.0, +4.7] | −0.8 | −1.1 | −0.9 | +1.8 | −0.1 |
| Age 1 (30,665) | −4.1 [−7.2, −1.0] | +3.6 [+1.8, +5.2] | −0.3 | −0.9 | −1.4 | +2.1 | +1.0 |
| Age 2 (32,073) | −4.7 [−6.8, −2.4] | +3.8 [+2.1, +5.3] | −0.4 | −0.6 | −1.8 | +2.5 | +1.1 |
| Age 3 (33,035) | −1.8 [−4.8, −0.1] | +3.7 [+2.1, +5.5] | −1.5 | −0.8 | −2.5 | +3.2 | −1.1 |
| Age 4 (33,591) | −1.8 [−3.8, +1.6] | +4.1 [+1.9, +5.9] | −1.4 | −1.0 | −2.8 | +4.9 | −1.2 |
| Bottom quarter of income (35,391) | −2.6 [−4.6, +0.2] | +1.1 [+0.5, +1.5] | −0.3 | −0.4 | −1.3 | +2.9 | −0.8 |
| Second quarter (37,874) | −1.5 [−3.1, +0.9] | +1.7 [+0.4, +2.2] | −0.2 | −0.7 | −1.9 | +2.4 | −1.0 |
| Third quarter (41,294) | −1.5 [−5.2, +0.3] | +3.3 [+1.1, +5.6] | −0.9 | −1.3 | −2.5 | +3.0 | −1.5 |
| Top quarter of income (43,885) | −6.4 [−9.2, −3.6] | +8.5 [+4.7, +12.2] | −2.3 | −1.1 | −2.0 | +3.6 | +3.1 |
| One parent (30,306) | −3.9 [−7.8, −0.7] | +2.5 [+1.3, +4.3] | −1.1 | −2.0 | −2.5 | +6.5 | −3.1 |
| Two parents (121,788) | −2.0 [−4.3, +0.3] | +4.2 [+2.1, +6.2] | −0.8 | −0.6 | −1.9 | +1.7 | +0.9 |
| Caregiver not employed (58,320) | −6.7 [−9.7, −4.6] | +2.2 [+1.0, +3.4] | +1.6 | +0.2 | +0.7 | +2.9 | +4.7 [+3.0, +6.8] |
| Caregiver part time (25,477) | −1.2 [−5.0, +3.1] | +4.0 [+1.9, +5.8] | −1.6 | −0.9 | −3.3 | +3.0 | −1.8 |
| Caregiver full time (68,297) | +0.8 [−1.7, +3.0] | +5.1 [+2.6, +7.3] | −2.8 | −1.9 | −3.9 | +2.7 | −3.5 [−6.0, +0.3] |

Levels for each group and year are in `csv/fixed_population_shares_all_runs.csv`.

What the table shows:

- **Age.** The rise in high-priced centers is similar at every age (+2.7 to +4.1). The fall in parent-only care is largest at ages 1 and 2. At ages 3 and 4 the parent-only fall is smaller and the rise in unpaid care by others larger (+3.2, +4.9).
- **Income.** The high-priced center rise grows with income, from +1 point in the bottom quarter to +8.5 in the top quarter. The top quarter also has the largest fall in parent-only care. With income measured in real dollars instead of rank, the top-quarter changes shrink to −3.5 to −5.2 (parent-only) and +3.4 to +5.7 (high-priced centers) but remain the largest of any income group.
- **Family type.** One-parent families show a larger fall in parent-only care and a large rise in unpaid care by others (+6.5); two-parent families show a larger rise in high-priced centers.
- **Employment.** Among children whose caregiver does not work, parent-only care fell about 6 to 7 points and paid care rose about 5. Among children whose caregiver works full time, parent-only care did not fall; high-priced centers rose about 5 points while low-priced centers, paid home-based care and Other Paid fell, so total paid care fell somewhat. The fall for children of non-employed caregivers holds under every treatment (−4.7 to −7.4 points), including extended donor hours, which moves 2024 parents with missing hours out of the not-employed group.

## 3. Robustness

Change in percentage points, all children under 5, 2024 forest minus 2019 forest. "Model" is the model's forest settings; "reg." is the regularized forest. Full results in `csv/robustness_all_children.csv`.

| Treatment | Parent Only, model / reg. | High-Priced Center | Low-Priced Center | Paid Home-Based | Other Paid | Other Unpaid | All paid care |
|---|---|---|---|---|---|---|---|
| Current | −2.9 / −3.0 | +3.0 / +3.6 | −0.5 / −0.9 | −1.3 / −0.9 | −1.6 / −1.9 | +2.8 / +3.0 | −0.4 / −0.1 |
| CPS reweighting | −2.4 / −2.5 | +2.5 / +3.5 | −0.9 / −1.0 | −1.2 / −1.0 | −1.8 / −2.1 | +3.2 / +3.0 | −1.4 / −0.6 |
| ACS reweighting | −2.4 / −2.4 | +2.6 / +3.5 | −1.1 / −1.0 | −1.4 / −1.0 | −1.6 / −2.1 | +3.2 / +2.9 | −1.4 / −0.6 |
| Extended donor hours | −2.8 / −2.4 | +3.1 / +3.5 | −0.5 / −1.0 | −1.2 / −0.9 | −1.9 / −2.0 | +2.6 / +2.8 | −0.5 / −0.4 |
| CPS reweighting plus extended donor hours | −2.0 / −1.9 | +2.7 / +3.4 | −0.7 / −1.1 | −1.2 / −1.0 | −2.1 / −2.2 | +2.7 / +2.8 | −1.4 / −1.0 |
| Calendar households only | −2.9 / −2.7 | +3.4 / +3.7 | +0.5 / −0.4 | −1.3 / −1.1 | −2.5 / −2.8 | +2.1 / +2.8 | +0.2 / −0.6 |
| Usual-week care concept | −1.7 / −1.6 | +3.1 / +3.6 | +0.1 / −0.7 | −1.2 / −0.8 | −2.3 / −2.2 | +1.9 / +1.8 | −0.3 / −0.2 |
| 2024 income on the 2019 real-income scale | −1.1 / −1.5 | +1.4 / +2.5 | −1.1 / −1.1 | −1.6 / −1.0 | −1.7 / −2.2 | +3.0 / +2.9 | −3.1 / −1.8 |

For CPS reweighting plus extended donor hours, 30 bootstrap draws give parent-only −1.9 [−3.5 to −0.3] (regularized) and −2.0 [−3.0 to +0.6] (model settings); high-priced centers +3.4 [+2.0 to +4.7] and +2.7 [+1.6 to +4.2]; Other Unpaid +2.8 [+1.7 to +4.0] and +2.7 [+0.7 to +4.0] (`csv/bootstrap_bands_w_cps_hours_b.csv`).

Notes on the treatments:

- **Reweighting** changes the forest's training mix only slightly within employment groups, because the forest already conditions on the caregiver's employment and the population's employment is held at the ACS level. It removes about half a point of the parent-only fall and about 1 point of paid care.
- **Missing work hours.** Filling in missing hours moves some 2024 children from the not-employed group to the employed groups. On its own it changes little; combined with reweighting it brings the parent-only fall to about 2 points.
- **Calendar households only** leaves the results nearly unchanged. Typical-week households differ mainly in characteristics the forest already conditions on (employment above all), so dropping them and reweighting changes the conditional choices little.
- **Usual-week care concept** raises non-parental care in both years (by 4.7 points in the 2019 NSECE and 2.9 in 2024), so the gap between the years narrows to about 1.7 points. This treatment also changes the 2019 levels the most; the calendar report covers that choice.
- **Real income.** The rank version (the model's) gives a 2019 family the 2024 behavior of the 2024 family at the same income rank, who is richer in real terms; the real-income version compares families with the same real income. The second gives the smaller change, especially for high-priced centers (+1.4 to +2.5 instead of +3.0 to +3.6). The NSECE share of children above 400 percent of poverty rose 7.5 points, against 3.7 in the CPS and 1.8 in the ACS (employment report), so part of the 2024 sample's real-income gain is sampling, and neither version cleanly separates real income growth from sample composition.

## 4. Pooled forest with a year feature (experiment 4)

One set of six forests was trained on both years together (each year's weights scaled to the same total; income percentiles computed within each year, as the model does), with the survey year as an added feature. The ACS children were then predicted with year set to 2019 and to 2024 (`csv/pooled_forest_year_effect.csv`).

| Measure, all children | Model settings: year = 2019 / 2024 / change | Regularized: year = 2019 / 2024 / change |
|---|---|---|
| Parent Only | 44.8 / 42.8 / −1.9 | 48.1 / 46.7 / −1.4 |
| High-Priced Center-Based | 7.5 / 9.2 / +1.7 | 7.5 / 8.8 / +1.3 |
| Low-Priced Center-Based | 8.1 / 7.5 / −0.6 | 7.9 / 7.6 / −0.3 |
| Paid Home-Based | 4.3 / 3.7 / −0.6 | 3.8 / 3.4 / −0.3 |
| Other Paid | 10.6 / 10.0 / −0.6 | 9.6 / 9.0 / −0.6 |
| Other Unpaid | 15.9 / 18.2 / +2.3 | 15.2 / 16.4 / +1.2 |
| All paid care | 30.6 / 30.5 / −0.1 | 28.8 / 28.9 / +0.1 |

The pattern matches the separate forests (parent-only down; high-priced centers and unpaid care by others up; paid care flat), at about half to two-thirds of the size.

How much the year matters:

- **Permutation importance** (the rise in prediction error when the year values are shuffled): with the regularized forest, year ranks 2nd of 6 features in the no-parent one-child group and 2nd of 12 in the one-parent two-child group, but 6th to 8th in the large groups; with model settings it ranks 5th to 9th in every group (`csv/pooled_feature_importance_*.csv`).
- **Held-out accuracy:** adding year to the regularized pooled forest leaves the eight-category log loss unchanged (1.363 with and without) and lowers the 15-class log loss from 1.643 to 1.640 (`csv/heldout_log_loss_pooled_year_feature.csv`).

A 3-point shift between the years is small relative to the uncertainty about any one child's category, so a held-out test of this size cannot detect it; the bootstrap bands are the better test of whether the shift is real. Forests also tend to understate the effect of a single yes/no feature: with only some features considered at each split, year is often not available to split on, and the predictions average over leaves that mix the two years. The pooled changes are therefore a lower bound on the year effect.

## 5. Limits

- **The 2019 ACS population is held fixed**, including its employment. The results describe how a 2019 population would choose under 2024 behavior, not 2024 outcomes.
- **Type-of-care coding.** The 2024 survey moved some "other organizational" care into center care (OPRE scales 2019 center shares up 7 to 9 percent to compare them). No adjustment was made. This probably overstates the rise in center care by roughly 1.5 to 2 points of the raw comparison and understates the fall in Other Paid and Other Unpaid (`reports/04_model_processing_2019_vs_2024.md`, comparability problem 4). The rise in Other Unpaid runs against the direction of this coding change.
- **The low/high center price cut** is $4.75 in 2019 and $5.76 in 2024 (grown by the CPI day care index). The split between the two paid center categories depends on that choice; their sum does not.
- **Bootstrap bands** ignore the survey's strata and clusters and the uncertainty in the raking targets. The check against the design-based standard error suggests this matters little for the overall shares; group-level bands are rougher.
- **Small groups.** Children with no parent unit and some two-child groups rest on a handful of 2024 records.
- **The usual-week treatment** uses one random draw of the imputed hours, not an average over draws.
- **No calibration or simulation was run.** The effect on calibrated model output, which re-fits demand parameters to these shares, is not measured here.

## 6. Files

Folder: `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/nsece2024_options/forest_shift/`

Scripts (`scripts/`; run with `sbatch --job-name=<name> scripts/run_r.sh <script> [arguments]`; logs in `logs/`):

- `00_common.R`: paths; extracts the model's forest functions from `1b_acs_processing.R`; training under model or regularized settings; ACS preparation and prediction; summaries.
- `01_build_inputs.R`: the seven NSECE training treatments for both years.
- `02_point.R <treatment> <setting> [seed]`: experiments 1–3 point estimates and the check against the setup run.
- `03_cv.R single <year>` and `03_cv.R pooled`: held-out accuracy.
- `04_pooled.R <setting>`: experiment 4.
- `05_bootstrap.R <draw> [treatment]` (array jobs via `run_boot.sh` and `run_boot_comb.sh`): bootstrap draws.
- `06_collect.R`: builds the CSV tables.

CSV tables (`csv/`):

- `fixed_population_shares_all_runs.csv`: every run (treatment, setting, seed), every group and measure: 2019 forest, 2024 forest, change.
- `robustness_all_children.csv`: all children, by treatment and setting, with the raw NSECE shares alongside.
- `bootstrap_bands_current.csv`, `bootstrap_bands_w_cps_hours_b.csv`: point change, bootstrap mean, standard deviation, and 2.5th and 97.5th percentiles, by group.
- `heldout_log_loss_by_setting.csv`, `heldout_log_loss_pooled_year_feature.csv`: cross-validation.
- `pooled_forest_year_effect.csv`, `pooled_feature_importance_model_current.csv`, `pooled_feature_importance_reg_current.csv`: experiment 4.
- `nsece_raw_shares_by_variant.csv`, `inputs_summary_by_variant.csv`: raw NSECE shares and employment under each treatment.
- `validation_against_setup_run.csv`: the 2019 reproduction check.

Intermediate files (`work/`): training tables by treatment and year, per-run results and bootstrap draws (`.rds`). `logs/old/` holds the logs of four runs that were repeated because a script was edited while they were running.
