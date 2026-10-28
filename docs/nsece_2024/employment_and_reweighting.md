# Maternal employment and family mix in the 2019 and 2024 NSECE, against the CPS and ACS

Status, 2026-10-28: this report records the review runs of 2026-10-16 on commit `c05fefa`. Words like "current", "today", and "the model" refer to that commit, not to the current code. The current record is `docs/nsece_2019_to_2024_crosswalk.md`. The review used the 2023 ACS because the model's file ended in 2023. A 2024 ACS file now exists (`raw_data/ACS/v1/2024`, config key `ACS-2024`), and the model pairs the 2024 NSECE with it. On the per-child measure below, the 2024 ACS gives 62.9% (check `src/misc/nsece_2024_checks/11_acs_mother_at_work.R`, which reproduces this report's 61.1% for 2019 and 62.6% for 2023). The 2024 NSECE's 64.3% is 1.4 points above it, and the ACS change from 2019 is +1.8 points. The ACS raking targets below (options C, E, and G) are still the 2023 ones; they have not been rebuilt from the 2024 ACS. The care shares used a $5.76 cutoff for 2024; the code now uses $6.86.

Prepared 2026-10-16. All numbers were computed from the microdata in Slurm jobs; scripts, logs and CSV outputs are listed at the end. No model code was changed and no model run was made. (Written as .txt because writing a .md report was blocked; the content is Markdown.)

## Short answer

**The 2024 NSECE's rise in maternal employment is not believable as a change in the population.** Among children under 5 who live with a parent, the share whose mother (or the one parent) worked last week rises from 53.6 percent in the 2019 NSECE to 64.3 percent in the 2024 NSECE, a rise of 10.7 points. Over the same period the March CPS shows 57.1 to 59.6 percent (+2.5 points), and the ACS shows 61.1 percent in 2019 and 62.6 percent in 2023 (+1.5 points). Most of the NSECE rise, roughly three-quarters to seven-eighths of it, reflects the samples drawn in the two years rather than a real change.

**Neither NSECE year matches the benchmarks on its own.** The 2019 NSECE is 3.5 points below the CPS and 7.5 points below the ACS. The 2024 NSECE is 4.7 points above the CPS and 1.7 points above the 2023 ACS. Against the ACS, the source the model uses for its population, the 2024 NSECE is the closer of the two years. Against the CPS, the two years miss by similar amounts in opposite directions.

**The family mix is mixed.** On some measures the 2024 NSECE moved toward the benchmarks: the share of two-parent families, the share of children at or above 400 percent of poverty, and mothers' education. On others it moved away: the 200-399 percent poverty group, families with only one child, and the split between two-parent families with and without a working mother. In both years the NSECE has about 8 points more children below the poverty line than the CPS or ACS. That gap is the same size in both years, which points to a difference in how income is measured rather than to sampling.

**Reweighting removes a large part of the 2019-to-2024 change in care shares.** When each year's NSECE child weights are raked (adjusted step by step until the weighted sample matches a set of outside totals) to that year's benchmark share of children by mother's work status and family type, the fall in parent-only care (model categories) shrinks from 6.7 points to between 2.2 and 3.5 points. The rise in all center-based care shrinks from 3.8 to about 2 points, and the rise in all paid care shrinks from 4.2 points to between 0.1 and 1.3 points. The rise in high-priced center care is the most robust: it shrinks only from 3.3 to between 2.2 and 2.5 points. After reweighting, none of the changes in the three combined measures (parent only, all center-based, all paid) is larger than 1.7 standard errors (before reweighting, all three were 2.3 to 3.6 standard errors). Reweighting on demographics alone (income, family type, mother's education, region, child age, race) leaves the employment gap and the care changes almost untouched. Employment is the margin that matters.

Whether to reweight before calibration is John's decision. Section 7 lists the options and what each one produces.

## 1. Sources and definitions

| Source | Years | Employment measure | Weights | Notes |
|---|---|---|---|---|
| NSECE household survey (public use) | 2019 (fielded Jan–Jul 2019), 2024 (Jan–Oct 2024) | Item D1a, "Last week, did you do any work for pay?" | Child weight for children; household weight for families | Mother identified the way `nsece2024_raw` does: the respondent if she is a female parent, otherwise the other parent; one-parent families use that parent. |
| CPS, March Annual Social and Economic Supplement (ASEC) | March 2019, 2023, 2024, 2025 | "At work" in the survey week (EMPSTAT 10) | ASEC person weight for children; ASEC household weight for families | Mother = MOMLOC; otherwise father (POPLOC). |
| ACS (the file the model reads, `ACS/v1/2023/ipums_usa.csv`) | 2019 and 2023, the only two years in the file | "At work" (EMPSTATD 10), the model's rule | Person weight; household weight | Mother = MOMLOC or MOMLOC2; otherwise father. Group quarters excluded (GQ 1–2 kept), as in the model. |

"Per child" means children are counted, each weighted by its child weight, so a family with two young children counts twice. "Per family" means one record per household: the youngest child in the age range and that child's mother or primary caregiver, weighted by the household weight. "Two parents" means the child has two parents in the household (biological, adoptive or step); a parent's partner who is not the child's parent does not count. This is the model's definition.

**Why the March CPS and not the monthly CPS.** The monthly CPS extracts on disk cannot be used for this. The file named in the request (`CPS-Monthly/2026101314`) contains only September 2026. None of the monthly vintages covers 2019 (the longest covers January 2020 to June 2026). None of them includes the family relationship variables (MOMLOC, POPLOC, RELATE, NCHILD) needed to find mothers. The March ASEC files (`CPS-ASEC/cps_asec_common`) do have those variables. The monthly files were used only to measure how the at-work rate changes from month to month (section 3).

**Income relative to poverty.** For the NSECE this is the survey's own ratio `POV_RATIO_TC`: household income in the prior calendar year (2018 or 2023) divided by the Census poverty threshold. For the CPS it is ASEC household income for the prior year divided by the official threshold for the household's size and number of children; the threshold table was read from the CPS family thresholds (the four-person, two-child threshold is $25,465 for 2018 and $30,900 for 2023, matching the Census tables). For the ACS it is household income over the past 12 months divided by the same table for the survey year.

## 2. Maternal employment, 2019 and 2024

Share of children (or families) whose mother, or the one parent, was at work or did any work for pay last week, in percent. Standard errors are in parentheses for the under-5, per-child rows. NSECE standard errors use the survey's strata and clusters. CPS and ACS standard errors treat households as clusters.

### Children under 5

| Unit and group | NSECE 2019 | NSECE 2024 | NSECE change | CPS Mar 2019 | CPS Mar 2023 | CPS Mar 2024 | CPS Mar 2025 | CPS change 2019–24 | ACS 2019 | ACS 2023 | ACS change 2019–23 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Per child, all with a parent | 53.6 (1.2) | 64.3 (1.5) | +10.7 | 57.1 (0.7) | 58.8 | 59.6 (0.8) | 59.3 | +2.5 | 61.1 (0.2) | 62.6 | +1.5 |
| Per child, two parents | 51.7 (1.5) | 62.7 (1.9) | +11.0 | 55.7 (0.7) | 58.2 | 58.5 (0.9) | 57.9 | +2.8 | 59.3 | 61.4 | +2.1 |
| Per child, one parent | 57.8 (1.7) | 68.6 (2.5) | +10.8 | 62.4 (1.5) | 60.9 | 64.0 (1.9) | 64.6 | +1.6 | 66.6 | 66.6 | 0.0 |
| Per child, mother in household | 52.0 | 62.8 | +10.8 | 56.7 | 58.5 | 59.3 | 58.8 | +2.6 | 60.5 | 62.0 | +1.5 |
| Per family, all with a parent | 55.7 | 65.5 | +9.8 | 59.4 | 61.0 | 61.5 | 61.5 | +2.1 | 62.9 | 64.3 | +1.4 |
| Per family, two parents | 54.0 | 64.0 | +10.0 | 57.9 | 60.3 | 60.5 | 60.3 | +2.6 | 61.3 | 63.1 | +1.8 |
| Per family, one parent | 59.2 | 69.6 | +10.4 | 64.6 | 64.0 | 65.7 | 66.2 | +1.1 | 68.0 | 68.4 | +0.4 |

### Children under 13

| Unit and group | NSECE 2019 | NSECE 2024 | NSECE change | CPS Mar 2019 | CPS Mar 2024 | CPS change | ACS 2019 | ACS 2023 | ACS change |
|---|---|---|---|---|---|---|---|---|---|
| Per child, all with a parent | 59.2 | 67.3 | +8.1 | 62.4 | 64.4 | +2.0 | 65.4 | 66.8 | +1.4 |
| Per child, two parents | 57.5 | 65.6 | +8.1 | 60.2 | 62.5 | +2.3 | 63.1 | 64.9 | +1.8 |
| Per child, one parent | 62.1 | 71.1 | +9.0 | 69.3 | 70.8 | +1.5 | 71.9 | 72.4 | +0.5 |
| Per family, all with a parent | 62.9 | 69.6 | +6.7 | 65.5 | 67.3 | +1.8 | 68.5 | 69.6 | +1.1 |
| Per family, two parents | 61.0 | 67.5 | +6.5 | 63.4 | 65.3 | +1.9 | 66.5 | 67.8 | +1.3 |
| Per family, one parent | 66.1 | 73.9 | +7.8 | 71.9 | 73.2 | +1.3 | 73.9 | 74.6 | +0.7 |

The pattern is the same in every row. Both benchmarks show a rise of 1 to 3 points. The NSECE shows a rise of 7 to 11 points. The 2019 NSECE is below both benchmarks, and the 2024 NSECE sits between the CPS and the ACS or above both. The two benchmarks also disagree with each other: the ACS is about 4 points above the March CPS in both years. That gap is outside the scope of this note, but it means "the right level" is uncertain by about 4 points before any NSECE question arises.

### Fathers and both parents (two-parent families, children under 5, per child)

| Measure | NSECE 2019 | NSECE 2024 | CPS Mar 2019 | CPS Mar 2024 | ACS 2019 | ACS 2023 |
|---|---|---|---|---|---|---|
| Mother at work | 51.7 | 62.7 | 55.7 | 58.5 | 59.3 | 61.4 |
| Father at work | 90.5 | 90.2 | 88.0 | 87.7 | 90.0 | 89.8 |
| Both at work | 47.4 | 56.7 | 50.3 | 52.4 | 53.7 | 55.7 |
| Mother at work, father not | 4.2 | 6.0 | 5.4 | 6.1 | 5.6 | 5.8 |

Fathers' employment agrees across all three surveys and both years. The NSECE differences are in mothers only.

## 3. Season

The NSECE asks about the week before the interview, and its interviews are spread over several months; the ASEC is a March snapshot. Two checks were made with the monthly CPS.

- **A stand-in measured every month:** women aged 18 to 54 living in a household with a child aged 0 to 4. In the March ASEC this stand-in tracks actual mothers closely (59.2 against 58.9 percent in 2019, 60.8 against 61.0 percent in 2024). In 2024 it is within 1 point of March from January to May. It is 2.2 points lower in June, 4.9 lower in July and 1.9 lower in August.
- **The same mothers in other months:** mothers of children under 5 in the March 2024 ASEC, linked by person identifier to their monthly interviews from December 2023 to June 2024. Compared with their own March status, they are 1.8 points lower in January, 0.3 lower in February, and 0.8, 1.1 and 0.5 points higher in April, May and June.

Weighting the stand-in's monthly differences by each NSECE year's mix of interview months gives a shift from March of −0.5 points for the 2019 field period and −0.7 points for 2024 (2024 monthly pattern), or −0.4 points in both years (2022–2024 average pattern). Season therefore lowers both benchmark levels by about half a point and leaves the 2019-to-2024 comparison unchanged. The raking below uses the March figures without this adjustment. File: `out/seasonal_shift.csv`.

Interview months, children under 5, weighted: 2019 — January 8 percent, February 24, March 27, April 13, May 12, June 11, July 5. 2024 — January 8, February 19, March 18, April 15, May 10, June 9, July 6, August 7, September 7, October 3.

## 4. Family mix

Percent of children under 5 in each group. "Gap" is the NSECE minus the March CPS of the same year. NSECE standard errors for 2024 are roughly 1 to 2 points, and the standard error of a gap is similar. Shares exclude unknown values: these are at most 1.2 percent of NSECE children on any variable, and the CPS and ACS have none because their missing values are imputed. Full tables for children under 5 and for households with children under 13 are in `out/composition_gaps.csv`.

| Variable | Group | NSECE 2019 | CPS 2019 | ACS 2019 | NSECE 2024 | CPS 2024 | ACS 2023 | Gap 2019 | Gap 2024 |
|---|---|---|---|---|---|---|---|---|---|
| Mother's work by family type | Two parents, mother at work | 34.5 | 42.5 | 43.2 | 45.4 | 45.9 | 45.9 | −8.0 | −0.5 |
| | Two parents, mother not at work | 32.2 | 33.8 | 29.6 | 27.0 | 32.5 | 28.8 | −1.6 | **−5.6** |
| | One parent, at work | 16.9 | 13.1 | 15.6 | 17.6 | 12.2 | 14.6 | +3.8 | **+5.4** |
| | One parent, not at work | 12.3 | 7.9 | 7.8 | 8.1 | 6.9 | 7.3 | +4.5 | +1.2 |
| | No parent in household | 4.1 | 2.7 | 3.8 | 2.0 | 2.5 | 3.5 | +1.4 | −0.5 |
| Family type | Two parents | 66.5 | 76.3 | 72.8 | 72.3 | 78.4 | 74.7 | −9.9 | −6.1 |
| | One parent | 29.4 | 21.0 | 23.4 | 25.7 | 19.1 | 21.9 | +8.5 | +6.6 |
| Income relative to poverty | Under 100% | 24.0 | 15.7 | 16.1 | 22.6 | 14.2 | 14.8 | +8.3 | +8.3 |
| | 100–199% | 23.4 | 21.1 | 22.0 | 21.4 | 20.3 | 20.7 | +2.3 | +1.1 |
| | 200–399% | 28.6 | 30.5 | 31.4 | 24.5 | 29.0 | 32.1 | −1.9 | **−4.5** |
| | 400% and above | 24.0 | 32.7 | 30.5 | 31.5 | 36.4 | 32.3 | −8.7 | −4.9 |
| Mother's education | Less than high school | 8.0 | 9.0 | 9.2 | 8.5 | 7.3 | 7.8 | −1.0 | +1.3 |
| | High school or GED | 20.2 | 21.6 | 19.7 | 21.3 | 24.5 | 20.2 | −1.4 | −3.3 |
| | Some college or associate | 27.6 | 25.1 | 28.1 | 22.9 | 22.6 | 25.9 | +2.4 | +0.3 |
| | Bachelor's | 22.0 | 24.3 | 21.5 | 24.2 | 24.1 | 23.2 | −2.3 | +0.1 |
| | Graduate degree | 13.2 | 15.0 | 14.0 | 15.8 | 16.9 | 15.7 | −1.8 | −1.1 |
| | No mother in household | 9.1 | 5.0 | 7.6 | 7.4 | 4.6 | 7.2 | +4.1 | +2.7 |
| Child race and ethnicity | Hispanic | 25.4 | 26.2 | 25.9 | 27.9 | 27.3 | 26.9 | −0.8 | +0.6 |
| | White, non-Hispanic | 50.5 | 48.7 | 49.8 | 46.5 | 46.1 | 46.0 | +1.9 | +0.4 |
| | Black, non-Hispanic | 12.6 | 13.9 | 12.9 | 11.4 | 14.7 | 12.5 | −1.3 | −3.3 |
| | Asian, non-Hispanic | 4.4 | 5.3 | 4.6 | 4.5 | 5.6 | 4.9 | −0.9 | −1.2 |
| | Other or multiple, non-Hispanic | 7.2 | 6.0 | 6.8 | 9.8 | 6.3 | 9.8 | +1.2 | +3.5 |
| Region | Northeast / Midwest / South / West | 16.0 / 21.0 / 38.6 / 24.4 | 15.3 / 20.7 / 39.9 / 24.1 | 15.7 / 21.0 / 39.0 / 24.3 | 16.2 / 20.7 / 39.5 / 23.7 | 15.4 / 21.7 / 40.5 / 22.4 | 16.0 / 20.7 / 40.2 / 23.1 | within 1.3 | within 1.2 |
| Children under 13 in household | One | 27.7 | 30.2 | 29.2 | 27.2 | 32.2 | 30.6 | −2.4 | **−5.1** |
| | Two | 40.2 | 40.5 | 40.4 | 43.1 | 39.3 | 40.5 | −0.3 | +3.8 |
| Mother's age | Under 25 / 25–29 / 30–34 / 35–39 / 40+ | 9.6 / 21.4 / 30.1 / 20.9 / 9.0 | 10.2 / 21.3 / 29.0 / 22.7 / 11.9 | 10.0 / 21.5 / 28.9 / 21.9 / 10.1 | 8.9 / 18.6 / 27.6 / 23.4 / 14.1 | 7.5 / 18.8 / 29.9 / 25.0 / 14.1 | 8.3 / 19.6 / 29.6 / 24.2 / 11.1 | 40+: −3.0 | 40+: 0.0 |
| Child age | 0 | 16.7 | 18.7 | 18.1 | 16.3 | 18.9 | 19.1 | −2.0 | −2.6 |

What the table shows:

- **Moved toward the benchmarks in 2024:** two-parent families (gap −9.9 to −6.1), the 400 percent and above group (−8.7 to −4.9), bachelor's and graduate degrees, mothers aged 40 and over, and two-parent families with a working mother (−8.0 to −0.5).
- **Moved away in 2024:** two-parent families with a mother not at work (−1.6 to −5.6), one parent at work (+3.8 to +5.4), the 200–399 percent group (−1.9 to −4.5), households with one child under 13 (−2.4 to −5.1), and Black children against the CPS (−1.3 to −3.3; against the ACS only −1.1). The rise in "other or multiple race" children matches the ACS 2023 (9.8 percent) and probably reflects changes in how race is reported, not sampling.
- **Unchanged, large gaps in both years:** below the poverty line (+8.3 points both years) and one-parent families (+8.5 and +6.6). Even after the NSECE is raked to the benchmark's family type and employment, its income distribution stays far below the benchmarks. That is consistent with the NSECE's single household income question giving lower incomes than the CPS and ACS income detail. It is a measurement difference, not a sampling difference.
- **The rise in the high-income share is partly real.** The share at or above 400 percent of poverty rises 7.5 points in the NSECE, 3.7 in the CPS and 1.8 in the ACS.
- **Region** is within about 1 point in every year, as expected, since the NSECE weights are calibrated by geography.

Households with children under 13 (household weights, youngest child's mother) show the same pattern. Two-parent households: NSECE 59.7 percent in 2019 and 66.2 percent in 2024, against the CPS's 71.9 and 73.3. The 200–399 percent group: NSECE 30.7 and 23.9, against 31.1 and 30.3.

**Does the family mix explain the employment gap?** No. Raking the NSECE to the CPS or ACS on family type, income, mother's education, region, child age and race, but not on employment, moves the share of children with a working mother only from 53.6 to 54.2–54.4 percent in 2019 and from 64.3 to 64.7–64.8 percent in 2024. The employment gaps lie within these groups, not across them.

## 5. The 50.9-versus-53.7 and 53-versus-61 discrepancy

The two comparisons measured different things. Once each is measured the same way in both surveys, they tell the same story: the 2019 NSECE has 6 to 9 points less maternal employment than the ACS.

**The 53.7 percent (ACS 2019) is the share of two-parent children under 5 whose mother and father were both at work.** The 2026-10-15 script (`analysis/acs_hours_detail.R`) set each parent's hours to zero if not at work and took the lower of the two; "lower-hours parent at work" therefore means both parents at work. Rerun here: both parents at work, 53.7 percent; mother at work, 59.3 percent; father at work, 90.0 percent.

**The 50.9 percent (NSECE 2019) is a looser measure on a slightly different universe.** It is the share of children whose parent with fewer hours in the survey's work-schedule variables (`WST_PARCH_LO`, `WST_HRS_TOT_PLO`) said yes to "worked for pay last week." Those schedule hours include school, training and commuting. If the father has more schedule hours (for example, from school) but did not work, and the mother worked, the child counts as "yes" even though only one parent worked. In the same universe, both parents worked for 48.6 percent of children (reproduced by `scripts/06_check_509.R`).

So the 2026-10-15 comparison set a measure close to "at least the lower-hours parent worked" (NSECE, 50.9) against a strict "both worked" (ACS, 53.7). Measured consistently:

| Two-parent children under 5, 2019 | NSECE | ACS | Gap |
|---|---|---|---|
| Both parents worked last week | 47.4 (48.6 in the work-schedule universe) | 53.7 | 5 to 6 points |
| Mother worked last week | 51.7 | 59.3 | 7.6 points |
| Father worked last week | 90.5 | 90.0 | −0.5 points |

The dual-earner measure also hides part of the mothers' gap, because the fathers agree and in most two-parent families with a working mother the father also works.

**The 53-versus-61 gap in the setup run is the mother's own status, and it is not caused by units, universe or the model's definition.** From the setup run `202610150828` (`out/reconciliation_model_2019.csv`):

| Children under 5 in a parent unit, 2019 | Value |
|---|---|
| Model ACS, primary caregiver at work (EMPSTATD 10), observed | 61.2 |
| Model ACS, same, predicted probability used in calibration | 61.4 |
| Model ACS, observed, two-parent / one-parent units | 59.3 / 66.9 |
| Model ACS, per family (family weight), observed | 62.9 |
| Model NSECE, primary caregiver has positive adjusted work hours (the branch's rule) | 52.0 |
| Model NSECE, two-parent / one-parent units | 50.1 / 56.3 |
| Raw NSECE, D1a "worked for pay last week" (this note) | 53.6 |
| Raw ACS, EMPSTATD 10 (this note) | 61.1 |

- **Units:** the gap is 7.5 points per child (53.6 against 61.1) and 7.2 points per family (55.7 against 62.9).
- **Universe:** the model's ACS parent units give 61.2 percent, and the raw ACS gives 61.1 percent.
- **Model definition:** the model's ACS rule is exactly EMPSTATD 10. Its NSECE rule (positive adjusted hours) gives 52.0 percent, 1.6 points below the D1a answer, because parents with missing calendar hours are counted as not working.
- **Something else:** what remains is a difference between the surveys in mothers' reported employment. The March CPS sits between them (57.1 percent). The 2019 NSECE is the outlier on the low side.

## 6. Reweighting the NSECE

### Method

Each year's NSECE child weights for children under 5 were raked to that year's benchmark distribution of children under 5. The benchmarks are the March 2019 or March 2024 CPS, or the 2019 or 2023 ACS (2023 is the latest ACS year in the model's file). Total weight was kept fixed. Unknown NSECE categories (for example, a mother's work status that is unknown, 0.7 percent of children) were held at their own share. Four sets of margins were used:

1. **Employment by family type:** five groups — two parents with the mother at work, two parents with the mother not at work, one parent at work, one parent not at work, and no parent in the household.
2. **Employment and demographics:** set 1 plus income relative to poverty (4 groups), mother's education (5 groups plus no mother), region (4), child's single year of age (0–4), and child race and ethnicity (5).
3. **Employment and demographics except income:** set 2 without the income margin. Because the NSECE measures income differently (section 4), forcing its poverty distribution onto the CPS or ACS moves weight between NSECE income groups that are not defined the same way.
4. **Demographics only:** set 2 with family type in place of employment by family type. This is a diagnostic: it shows what happens when employment is left alone.

Care shares use the model's eight care categories from the step 1a runs on branch `review/nsece-2024` (`analysis/nsece2024/out/orig_2019` and `new_2024`; 2024 uses the $5.76 low/high cut). These categories are defined for 96 percent of 2019 and 99 percent of 2024 children under 5. The survey's own primary type of care gives the same picture (`out/care_shares_by_option.csv`).

**Weight dispersion stays moderate.** The design effect from unequal weights (1 plus the squared coefficient of variation of the weights) rises from 2.25 to at most 2.62 in 2019 and from 2.72 to at most 3.02 in 2024. With the employment margin alone, the ratio of new to old weight stays between 0.64 and 1.71 for every child. With the full set, the ratio ranges from 0.12 to 3.72, and the 1st-to-99th percentile range is 0.27 to 2.30 (`out/raking_weight_diagnostics.csv`).

### Results: children under 5, percent

| Weighting (benchmark: margins) | Mother at work, 2019 → 2024 | Parent Only | All center-based | High-priced center | All paid care | Other Unpaid |
|---|---|---|---|---|---|---|
| A. Survey weights (as is) | 53.6 → 64.3 | 52.6 → 45.9 (−6.7) | 21.5 → 25.3 (+3.8) | 7.3 → 10.6 (+3.3) | 25.7 → 29.9 (+4.2) | 13.5 → 16.7 (+3.1) |
| B. CPS: employment by family type | 57.1 → 59.6 | 50.9 → 48.5 (−2.4) | 22.5 → 24.3 (+1.8) | 8.3 → 10.5 (+2.2) | 27.8 → 28.9 (+1.1) | 13.4 → 15.5 (+2.1) |
| C. ACS: employment by family type | 61.1 → 62.6 | 49.2 → 47.0 (−2.2) | 23.1 → 25.0 (+1.9) | 8.4 → 10.6 (+2.2) | 28.8 → 29.4 (+0.6) | 14.0 → 16.1 (+2.1) |
| D. CPS: employment and demographics | 57.1 → 59.6 | 51.0 → 47.6 (−3.5) | 22.6 → 24.7 (+2.1) | 8.7 → 11.0 (+2.3) | 28.6 → 29.5 (+1.0) | 12.8 → 15.7 (+2.9) |
| E. ACS: employment and demographics | 61.1 → 62.6 | 49.4 → 46.8 (−2.5) | 22.8 → 25.0 (+2.2) | 8.4 → 10.5 (+2.2) | 29.1 → 29.2 (+0.1) | 13.7 → 16.3 (+2.6) |
| F. CPS: employment and demographics except income | 57.1 → 59.6 | 51.5 → 48.6 (−2.8) | 22.0 → 24.3 (+2.2) | 8.0 → 10.3 (+2.4) | 27.2 → 28.5 (+1.3) | 13.5 → 15.5 (+2.0) |
| G. ACS: employment and demographics except income | 61.1 → 62.6 | 49.8 → 47.2 (−2.6) | 22.4 → 24.8 (+2.4) | 7.8 → 10.3 (+2.5) | 27.9 → 28.8 (+0.9) | 14.3 → 16.3 (+2.0) |
| H. CPS: demographics only (diagnostic) | 54.2 → 64.8 | 52.3 → 45.6 (−6.7) | 22.2 → 25.4 (+3.3) | 8.6 → 11.3 (+2.7) | 27.8 → 30.6 (+2.8) | 12.4 → 16.4 (+4.0) |
| I. ACS: demographics only (diagnostic) | 54.4 → 64.7 | 52.3 → 46.1 (−6.2) | 21.9 → 25.3 (+3.4) | 8.1 → 10.6 (+2.6) | 27.2 → 29.6 (+2.4) | 12.8 → 16.6 (+3.8) |

"Mother at work" is the share of children with a parent whose mother, or one parent, worked last week. The full set of eight categories for every option is in `out/care_shares_by_option.csv`, with a column giving each change as a percent of the change under survey weights. Under options B to G, low-priced center care changes by +0.1 to +0.3 points (survey weights: +1.0). Paid home-based changes by −0.8 to −1.3 (survey weights: −0.4), other paid by −0.4 to −0.8 (+0.3), and unpaid center-based by −0.6 to −0.1 (−0.5).

**How much of the change survives reweighting.** Under the six options that include employment (B–G):

- The parent-only fall keeps 33 to 52 percent of its size.
- The all-center rise keeps 48 to 64 percent.
- The high-priced center rise keeps 66 to 75 percent.
- The all-paid rise keeps 2 to 30 percent.

Standard errors of the 2019-to-2024 change, using the survey's design and treating the raked weights as fixed (`out/care_change_standard_errors.csv`):

| Change | Survey weights | Options B–G |
|---|---|---|
| Parent Only | −6.7 (1.9) | −2.2 to −3.5 (1.9 to 2.0) |
| All center-based | +3.8 (1.5) | +1.8 to +2.4 (1.5 to 1.6) |
| All paid care | +4.2 (1.6) | +0.1 to +1.3 (1.6 to 1.7) |

**Levels in 2019.** Raking the 2019 NSECE to the ACS lowers its parent-only share from 52.6 to 49.2–49.8 percent. That closes about half of the 7-point gap between the NSECE (52.6) and the model's 2019 calibration targets (45.5, from `06_model_projection.md`), consistent with that report's estimate that the employment mix accounts for about 4 of the 7 points.

## 7. The choices

Each option is described by what it does and what it produces. The care shares are NSECE shares for children under 5 in the model's categories. They are not model output: the model trains its care-choice forest on the NSECE and predicts on its own ACS population, so the effect on calibrated model results would be smaller and would need a setup run to measure. The forest conditions on the caregiver's employment state, income rank, region and family members' ages, and it draws its training sample in proportion to the NSECE child weights. Reweighting therefore changes the model mainly through the mix of training records and through the within-NSECE income ranks, not directly through the aggregate shares below.

1. **Use the 2024 weights as they are.** The 2024 NSECE has 64.3 percent of young children with a working mother, against 59.6 (CPS) and 62.6 (ACS 2023). Care shares: parent only 45.9, all center-based 25.3, high-priced center 10.6, all paid 29.9. Comparisons with the 2019 NSECE would show changes of −6.7, +3.8, +3.3 and +4.2 points, of which most is sample difference. The 2019 NSECE, which the current model uses, was 3.5 to 7.5 points too low on the same measure, so the as-is 2024 file is not further from the benchmarks than the current one; against the ACS it is closer.

2. **Rake to the CPS on employment by family type only (option B).** The smallest change to the weights: every child's weight moves by a factor between 0.70 and 1.23 in 2024. It fixes the employment level at the CPS March value (59.6 percent; 59.0 after the seasonal shift). 2024 care shares: parent only 48.5, center 24.3, high-priced center 10.5, paid 28.9. The CPS measure is the closest in wording to the NSECE item, but the CPS level is about 3 points below what the model's own ACS population has, so the training sample and the prediction sample would disagree on employment by about that much.

3. **Rake to the ACS on employment by family type only (option C).** Matches the employment level of the population the model predicts on (62.6 percent in 2023, by family type). 2024 care shares: parent only 47.0, center 25.0, high-priced center 10.6, paid 29.4. Weights move by a factor between 0.83 and 1.71. The ACS file has 2023, not 2024, so the target is one year early; the CPS shows employment of mothers of young children 0.8 points higher in March 2024 than in March 2023.

4. **Rake on employment plus demographics (options D–G).** Adds mother's education, region, child age and race, with or without income. Gives 2024 parent-only shares of 46.8 to 48.6 and paid shares of 28.5 to 29.5. It adds little beyond options 2 and 3 for care shares, and widens the weights (largest ratio 2.2 to 3.7). Including the income margin forces the NSECE's poverty distribution, which is measured differently and differs by a stable 8 points in both years, onto the benchmark's. Since the model uses NSECE income only as a rank within the NSECE, the income margin mainly reshuffles which NSECE records are treated as poor.

5. **Rake on demographics only (options H, I).** Shown for completeness. It does not correct the employment level (64.7 to 64.8 percent) and leaves the care changes nearly as large as with the survey weights. It is not a way to address the employment problem.

A related choice: if the 2019 NSECE stays in use for anything (for example as the starting point of a 2019-to-2024 comparison or as a second calibration year), the same raking applied to 2019 raises its employment from 53.6 to 57.1 (CPS) or 61.1 percent (ACS) and lowers its parent-only share by 1.7 to 3.4 points. Reweighting only one of the two years would leave the comparison between them inconsistent.

## 8. Limits

- **Benchmark level.** The CPS and ACS disagree by about 4 points on mothers' employment in both years. The choice between them sets the level.
- **Season.** The CPS benchmark is March. The seasonal shift to the NSECE field months is about −0.5 points in both years and is not applied in the raking.
- **ACS year.** The ACS file ends in 2023; the 2024 targets from the ACS are one year early.
- **Standard errors.** NSECE standard errors use the survey's strata and clusters. CPS and ACS standard errors cluster on household only, because the public files have no design variables; they are understated somewhat. The standard errors after raking treat the raked weights as fixed and ignore the uncertainty in the targets.
- **Mother identification.** The NSECE public file gives the sex of the respondent only. 4 to 6 percent of children under 5 have a caregiver whose sex is inferred (two non-respondent parents, or a respondent of unknown sex). Their "mother" may be the father. This is the same method as `nsece2024_raw`.
- **Work hours.** Only "worked last week" was benchmarked, not hours or full-time status. The model's NSECE employment rule (positive adjusted hours) runs 1.6 points below D1a in 2019 and more in 2024 because of missing calendar hours (`04_model_processing_2019_vs_2024.md`, comparability problem 3). Raking to D1a-based targets does not fix that separate problem.
- **What was not run.** No calibration was run, so the effect of each option on model output is not measured here.

## Files

All paths are under `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/nsece2024_options/employment/`.

Scripts (`scripts/`, run with `sbatch --job-name=<name> scripts/run_r.sh <script>`; logs in `logs/`):

- `00_helpers.R`: fixed-width readers, category recodes, raking routine.
- `01_nsece_build.R`: child-level NSECE 2019 and 2024 with covariates and the 1a care categories.
- `02_cps_asec_build.R`: child-level March CPS 2019, 2023, 2024 and 2025; poverty threshold table.
- `03_cps_monthly_season.R`: monthly stand-in and linked mothers, for the seasonal shift.
- `04_acs_build.R`: child-level ACS 2019 and 2023; reproduces the 53.7 percent figure.
- `05_analysis.R`: all tables and the raking.
- `06_check_509.R`: reproduces the 50.9 percent figure.

CSV outputs (`out/`):

- `employment_benchmark.csv`: every employment rate by source, year, universe, unit and group, with standard errors and sample counts.
- `seasonal_shift.csv`, `cps_monthly_proxy.csv`, `cps_linked_mothers_by_month.csv`, `cps_asec_proxy_check.csv`: the season checks.
- `composition_long.csv`, `composition_gaps.csv`: family mix, children under 5 and households with children under 13, with NSECE standard errors and gaps.
- `reconciliation_model_2019.csv`: the model setup run's employment rates.
- `raking_targets.csv`, `raking_weight_diagnostics.csv`, `care_shares_by_option.csv`, `care_change_standard_errors.csv`: the reweighting.
- `pov_thresholds.csv`: poverty threshold table by income year.
- The `.rds` files are the child-level analysis files built from the microdata.
