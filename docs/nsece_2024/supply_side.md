# Supply-side parameters for the move to the 2024 NSECE: choices, options and their effects

Status, 2026-10-28: this report records the review runs of 2026-10-16 on commit `c05fefa`. Words like "current", "today", and "the model" and every line number refer to that commit, not to the current code. The current record is `docs/nsece_2019_to_2024_crosswalk.md`. Since then, home-based enrollment and staff carry provider weights in both years (option b1), and free administrative-data centers join Unpaid Center-Based with their own staff count (crosswalk, Administrative-data centers). Current supply parameters are in the crosswalk (Open item 4).

Prepared 2026-10-16 for John Ricco. Nothing in the repository was edited. All numbers come from standalone scripts that read the review branch `review/nsece-2024` (worktree `ECEC-Simulator-caregiver-review/nsece2024`, commit `c05fefa`) and the NSECE public files. The rebuild of the model's supply step reproduces `supply_2019.yaml` and `supply_2024.yaml` exactly (largest difference in any staffing ratio 5e-8). Scripts, logs and CSV files are in `analysis/nsece2024_options/supply/` (list at the end). This file is plain text in Markdown layout; it is named .txt because writing a .md file was blocked.

Terms used throughout:

- Staff per child-hour (the model's `L_req`): full-time-equivalent classroom staff divided by full-time-equivalent enrolled children, by sector and by whether the staff member has a bachelor's degree (BA). The model treats one full-time staff member and one full-time child as the same number of hours, so this is staff hours per child-hour.
- Labor cost per child-hour: staff per child-hour times the wage, summed over the two education groups.
- delta_0: the non-labor part of the sector price, defined as the posted price minus labor cost per child-hour. In simulation it is held fixed (grown with CPI-U), and the equilibrium price is labor cost at market wages plus delta_0.
- L_0: the baseline stock of care-worker hours per year, by education, built as staff per child-hour times provider-side child-hours, summed over sectors. Market wages are w = w_0 x nominal wage factor x (labor demanded / L_0)^(1/elasticity), with elasticities 4 (no BA) and 2 (BA).
- Gap ratio: labor demanded at the calibration year's care hours divided by L_0. A ratio below 1 means the first simulated year starts with wages below the survey wage.
- Price cell: one center and one age group (0-1, 2-3, 4-5). Each cell is classified as unpaid, low-priced or high-priced from the center's posted full-time rate for that age group.
- Labor share: labor cost per child-hour divided by the price. With a negative delta_0 it exceeds 1.

"2024 demand" below means ACS-implied care hours for 2024. No 2024 calibration exists, so these are the 2019 ACS calibration hours scaled, sector by sector, by the change in NSECE household-reported hours from 2019 to 2024 (an assumption; see section 3.1).

---

## The choices and what each option does

Six decisions on the supply side can be made separately. Each table shows the options side by side. Unless stated, 2024 figures use 2024 data throughout, and "current" is the model's existing method applied to that year. Full detail for every option and both years is in `tables/choices_summary.csv` and `tables/opt_supply_parameters_long.csv`.

### Choice 1. How to measure staff per child-hour in centers

| Option | What it does | Low-priced staff per child-hour, no BA / BA (2019 -> 2024) | Unpaid-center staff per child-hour, no BA / BA (2024) | Low-priced delta_0, 2024 | L_0 2024, no BA / BA (billion hours) | Gap ratio 2024, no BA / BA |
|---|---|---|---|---|---|---|
| (a) Current | Weighted workforce records, each assigned to the price sector of its sampled classroom's age group, over weighted FTE enrollment of all price cells in that sector | 0.178 / 0.035 -> 0.203 / 0.056 | 0.086 / 0.063 | -$0.47 | 1.65 / 0.70 | 0.89 / 0.83 |
| (c1) Leave administrative-data centers out of the ratio | These public-school programs have enrollment but no interviewed staff; dropping them from the denominator removes the understatement this causes | 0.179 / 0.035 -> 0.203 / 0.056 | 0.106 / 0.078 | -$0.47 | 1.71 / 0.75 | 0.88 / 0.82 |
| (c2) Also leave publicly funded centers out of the paid-sector ratios | (c1), plus the low- and high-priced ratios exclude centers with any Head Start or public pre-K children or a public-school address | 0.183 / 0.033 -> 0.206 / 0.052 | 0.106 / 0.078 | -$0.41 | 1.71 / 0.72 | 0.88 / 0.82 |
| (c3) Director-reported staff on the same price cells | The center director's count of teachers and aides for children 0-5 (full-time counted as 1, part-time as 0.5), spread over the center's age groups in proportion to FTE enrollment, over FTE enrollment of the same cells; BA split from the workforce file's BA share in each sector | 0.163 / 0.032 -> 0.161 / 0.044 | 0.125 / 0.092 | +$0.53 | 1.60 / 0.74 | 0.88 / 0.82 |

Under (c3) the 2024 low-priced labor cost is $3.77 against a posted price of $4.30, and the 2019 delta_0 is $0.71 (current: $0.48). Staff per child under (c3) rises 5% in low-priced centers from 2019 to 2024, against 22% under the current method (section 1.4).

### Choice 2. What to do about the negative low-priced delta_0

All rows use 2024 data with the current staffing and wages unless stated. "Price used" is the low-priced sector price the model would calibrate to. The last two columns show how strongly the low-priced price responds: to 10% more paid center hours (section 3.3), and to the existing 25% payroll credit (`config/employer_subsidy/employer_tax_credit.yaml`) at base-year wages and quantities.

| Option | Economic meaning | Price used | delta_0 | Labor share | Low-priced price after +10% paid center hours | Low-priced price under the 25% payroll credit |
|---|---|---|---|---|---|---|
| (a) Keep it negative | Low-priced centers recover less than their measured payroll from posted prices; the shortfall is a fixed amount per child-hour that grows with CPI-U | $4.30 | -$0.47 | 1.12 | +2.4% (+$0.10) | -27.9% |
| (e1) Floor delta_0 at zero (equivalently, price = labor cost) | Treats the posted rate as understating revenue per child-hour; the model price becomes the cost-covering price, 11% above the posted rate, with no non-labor margin | $4.77 | $0 | 1.00 | +2.2% (+$0.10) | -25.0% |
| (e2) Pool low- and high-priced centers for staffing | One staffing ratio for all paid centers (0.179 no BA, 0.066 BA in 2024); the two sectors differ only in delta_0 | $4.30 | -$0.30 | 1.07 | +2.5% (+$0.10) | -26.9% |
| (e3) Price keeps the 2019 non-labor share | Price = labor cost / (1 - 0.145), the low-priced sector's 2019 non-labor share; delta_0 = 14.5% of the price | $5.58 | +$0.81 | 0.85 | +1.9% (+$0.10) | -21.2% |
| (e4) Scale low-priced staffing so the 2019 non-labor share holds at the posted price | Treats the 2024 low-priced staffing ratio as overstated; multiplies it by 0.77 (to 0.157 no BA, 0.043 BA) | $4.30 | +$0.62 | 0.85 | +1.8% (+$0.07) | -21.2% |
| (e5) Value each sector's staff at that sector's own wages | Low-priced centers pay less than the all-center average ($14.26 and $20.40 against $16.80 and $24.22 in 2024); staffing is restated in "average-wage hours" so unit cost equals the sector's own payroll; L_0 is restated in the same units | $4.30 | +$0.26 | 0.94 | +1.9% (+$0.08) | -23.4% |

Pooling (e2) does not make delta_0 non-negative in 2024: pooled labor cost in paid centers ($4.60) is still above the low-priced posted price. Under (e1) and (e3) the household price wedges must be rebuilt against the new supply price (1a sets each family's wedge to its out-of-pocket price divided by the supply price), or calibration-year family costs in this sector would rise by 11% and 30%. Once the wedges are rebuilt, family costs move by the percentage change in the supply price, so the practical difference among these options is the labor share: how much of a wage change or a payroll subsidy passes through to the price. In 2019 the low-priced labor share was 0.85 under the current method.

### Choice 3. Extreme posted rates in the 2024 file

The 2019 public file removed posted rates outside the 5th-95th percentile of each pay unit (coded "trimmed", which the model treats as missing). The 2024 file instead sets them to the 5th or 95th percentile value, so they stay in. Option (c4) drops 2024 weekly rates at or beyond the 5th and 95th percentiles of each age's rates (1,402 of 13,361 positive rates, close to the 2019 share), then rebuilds the sectors.

| 2024 | (a) Keep censored values | (c4) Drop them, as 2019 did |
|---|---|---|
| Low-priced price / delta_0 | $4.30 / -$0.47 | $4.64 / -$0.26 |
| High-priced price / delta_0 | $9.04 / $4.60 | $8.42 / $4.09 |
| Low-priced staff per child-hour, no BA / BA | 0.203 / 0.056 | 0.213 / 0.054 |
| L_0, no BA / BA (billion hours) | 1.65 / 0.70 | 1.54 / 0.63 |
| Gap ratio, no BA / BA | 0.89 / 0.83 | 0.97 / 0.90 |

Dropping only the low tail gives a low-priced price of $4.72 and delta_0 of -$0.12. L_0 falls under (c4) because the model counts only enrollment in price cells with a usable price, so dropped rates also drop their enrollment from L_0, as happened in 2019.

### Choice 4. Base wages by education

| Option | 2024 wage, no BA / BA | Low-priced delta_0 | High-priced delta_0 | Home-based delta_0 |
|---|---|---|---|---|
| (d1) NSECE workforce mean (current) | $16.80 / $24.22 | -$0.47 | $4.60 | $3.73 |
| (d2) NSECE mean, wages from $7.25 to $139 (the 2019 top value grown by the model's wage factor) | $16.97 / $22.27 | -$0.40 | $4.72 | $3.79 |
| (d5) NSECE mean, interviews before August 2024 | $16.81 / $21.94 | -$0.34 | $4.77 | $3.83 |
| (d3) 2019 NSECE x CES child care hourly earnings growth (1.325) | $15.98 / $26.09 | -$0.41 | $4.59 | $3.79 |
| (d4) 2019 NSECE x OEWS growth by occupation (childcare workers 1.298 for no BA; preschool teachers 1.196 for BA) | $15.66 / $23.56 | -$0.20 | $4.83 | $3.95 |
| For reference: 2019 NSECE x model wage factor (1.2944) | $15.61 / $25.49 | | | |

No wage choice alone makes the low-priced delta_0 positive; at 2024 wages and 2019 staffing it would be +$0.47 (section 1.1). The BA mean is the sensitive figure. BA staff interviewed in August-November 2024 report a mean of $26.52, against $21.94 for those interviewed earlier, while the no-BA mean is the same in both periods ($16.79 and $16.81). Part of this is who was interviewed in the fall: 21% of fall BA respondents work in public-school-linked centers, against 7% in the spring; outside public-school and government centers the fall-spring difference is $2.30 rather than $4.58 (`tables/opt_d_wage_timing_2024.csv`). The survey weights are built for the whole field period, so a spring-only mean under-represents those public-school staff. The model's single wage per education group applies to every sector and is the all-center mean, which public programs pull up (unpaid-center BA staff earn $30.39 in 2024; high-priced center BA staff $22.06).

### Choice 5. Home-based totals (the October code-review problem)

The model sums listed home-based providers' enrollment and staff without weights, and counts children of all ages (0-13) as full-time.

| Option | Enrollment counted, 2019 / 2024 | Home-based child-hours, 2019 / 2024 (billion) | Home-based part of L_0, no BA (billion hours), 2019 / 2024 | L_0 total, no BA / BA, 2019 | L_0 total, no BA / BA, 2024 | Gap ratio no BA / BA, 2019 | Gap ratio 2024 |
|---|---|---|---|---|---|---|---|
| (a) Unweighted, all ages (current) | 32,097 / 29,889 | 0.06 / 0.06 | 0.010 / 0.010 | 1.27 / 0.55 | 1.65 / 0.70 | 0.91 / 0.78 | 0.89 / 0.83 |
| (b1) Weighted, all ages | 710,400 / 640,300 | 1.42 / 1.28 | 0.231 / 0.210 | 1.49 / 0.59 | 1.85 / 0.75 | 0.78 / 0.71 | 0.79 / 0.77 |
| (b2) Weighted, children under 6 not in kindergarten (staff scaled by each provider's under-6 share) | 483,100 / 458,700 | 0.97 / 0.92 | 0.149 / 0.149 | 1.41 / 0.58 | 1.79 / 0.74 | 0.82 / 0.73 | 0.82 / 0.79 |

For comparison, household-side paid home-based hours for children under 5 are 1.54 billion (ACS calibration, 2019) and 1.24 billion (NSECE household survey, 2019). Weighting brings the home-based provider side close to the household side; under the current method the household side is 24 times the provider side. Weighting barely changes the no-BA home-based staffing ratio but lowers the BA ratio by 12.4% (2019: 0.160 / 0.038 unweighted, 0.163 / 0.033 weighted, all ages) or delta_0 (+$0.06 to +$0.17). Its effect is on L_0, and through L_0 on the first-year wage level (Choice 6).

### Choice 6. What L_0 is anchored to

| Option | 2019 L_0, no BA / BA | First-year wage relative to the survey wage, 2019 (no BA / BA) | 2024 L_0 | First-year wage relative to the survey wage, 2024 |
|---|---|---|---|---|
| Provider side, current home-based | 1.27 / 0.55 | -2.3% / -11.8% | 1.65 / 0.70 | -2.9% / -8.9% |
| Provider side, weighted home-based under 6 (b2) | 1.41 / 0.58 | -5.0% / -14.9% | 1.79 / 0.74 | -5.0% / -11.2% |
| Provider side, (b2) plus the administrative-data fix (c1), option (f1) | 1.42 / 0.59 | -5.0% / -14.9% | 1.85 / 0.79 | -5.1% / -11.5% |
| Set L_0 equal to labor demanded at calibration hours | 1.16 / 0.43 | 0 / 0 | 1.46 / 0.58 (current staffing) | 0 / 0 |

"First-year wage" is the wage the solver finds when the survey year is simulated with nominal factors of 1. Under the current 2019 setup this is the $11.79 and $17.38 in report 06. Anchoring L_0 to demand removes that first-year drop by construction. It does not change how wages respond to later changes in demand, because the wage response depends only on the percentage change in labor demanded (section 3.3).

### Two combinations

| Option | Contents | 2024 low-priced delta_0 | 2024 L_0, no BA / BA | 2024 gap ratio | First-year wage 2024, no BA / BA |
|---|---|---|---|---|---|
| (f1) Fix the two known 2019 problems only | (b2) weighted home-based under 6, (c1) administrative-data centers out of the ratio | -$0.47 | 1.85 / 0.79 | 0.81 / 0.78 | -5.1% / -11.5% |
| (f2) Measurement-consistent version | (b2) home-based, (c3) director staffing, (c4) 2024 extreme rates dropped | +$0.86 (price $4.64) | 1.64 / 0.71 | 0.87 / 0.83 | -3.4% / -9.0% |

In 2019, (f2) gives a low-priced delta_0 of $0.71 and a gap ratio of 0.81 / 0.72.

### Solver failures

No option produces parameters the solver rejects. Every wage, staffing ratio and L_0 is positive and finite, every sector has staff of both education groups, and every base-year equilibrium price is at least $3.07, far above the optimizer's $0.01 lower bound. A negative delta_0 would only stop the solver from finding a price if a policy cut the low-priced unit labor cost by about 90% or more (a payroll subsidy rate of 0.90 under the current 2024 parameters, 0.94 under (c4), never under the options with a non-negative delta_0), or if a supplier-side subsidy per child-hour exceeded the base-year price of about $4.08. The only payroll subsidy in the repository is 25%.

---

## 1. Why the 2024 low-priced delta_0 is negative

### 1.1 Arithmetic of the sign change

| Step | Low-priced labor cost | Low-priced delta_0 |
|---|---|---|
| 2019: price $3.31, 2019 staffing, 2019 wages | $2.84 | +$0.48 |
| 2024 price ($4.30) and 2024 wages, 2019 staffing | $3.84 | +$0.47 |
| ... plus the 2024 no-BA staffing ratio (0.178 -> 0.203) | $4.26 | +$0.04 |
| ... plus the 2024 BA staffing ratio (0.035 -> 0.056) | $4.77 | -$0.47 |

Prices and wages rose by similar proportions; the sign changes because measured staffing rose. The BA ratio alone adds $0.51 per child-hour, more than the whole 2024 residual. The price itself is in line with the model's own projection: 2019 grown the model's way gives $4.26.

### 1.2 Which centers are low-priced

Shares of FTE enrollment in the low-priced sector (center weight x FTE enrollment; `tables/diag_A_center_sector_composition.csv`):

| | 2019 | 2024 |
|---|---|---|
| FTE enrollment (millions) | 1.62 | 1.88 |
| Administrative-data (public-school) centers | 0.3% | 0% |
| Public-school address | 9.6% | 5.3% |
| For-profit / nonprofit / government / other | 50.0 / 43.8 / 5.4 / 0.9% | 45.3 / 47.7 / 2.4 / 4.6% |
| Any Head Start or public pre-K child in the center | 42.8% | 40.0% |
| Age group 0-1 / 2-3 / 4-5 | 21.3 / 47.8 / 30.9% | 22.3 / 48.1 / 29.6% |
| Full-time share of enrollment | 79.3% | 83.1% |

The low-priced sector did not take in more public programs. Public-school-linked and government centers fell as a share of its enrollment and of its counted staff (public-school share of counted staff 7.5% -> 3.2%). The age mix is unchanged. The higher full-time share raises FTE enrollment, which lowers the ratio, so it works against the measured rise.

All 1,141 administrative-data centers in 2024 fall in the unpaid sector (930 centers, 298,000 FTE children) or have no sector (211); none are low-priced. In 2019, 7 of 483 were low-priced (5,500 FTE children).

### 1.3 How the staffing ratio is built

- Numerator. Each workforce respondent (lead teachers and assistants with known education and full-time status) carries a weight that scales the sample to all classroom staff in the country: 1.36 million in 2019 and 1.55 million in 2024. A respondent counts 1 if usually working 35 hours or more, otherwise 0.5. The respondent is assigned to the sector of the price cell that matches the age of the youngest child in the sampled classroom. If that cell has no price sector, the respondent is dropped.
- Denominator. For each price cell, enrolled children are converted to FTE (full-time counts 1, part-time 0.5, using the center's reported full-time share), multiplied by the center weight, and summed over all cells in the sector, whether or not that center produced a counted worker.
- The ratio therefore divides a national staff total by a national enrollment total, sector by sector. The two totals come from different files with different coverage. Workers whose classroom age group has no price sector are dropped, while the same center's other age groups still count in the denominator.

### 1.4 Tests of file and definition changes

| Question | Finding |
|---|---|
| Do the workforce placeholder rows enter wages or staffing? | No. The 483 (2019) and 1,141 (2024) placeholder records carry weights (16,731 and 27,054 staff, 1.2% and 1.7% of the total) but code -8 on wage, role, education and full-time status, so none pass the model's filters. Their centers' enrollment does count in the unpaid-sector denominator, which understates the unpaid ratio (these centers are 18.8% of 2024 unpaid FTE enrollment, 8.0% in 2019). Option (c1) addresses this. |
| Did a larger share of staff get counted in 2024? | Yes. The model counts 73.8% of workforce weight in 2019 and 82.6% in 2024; FTE enrollment with a price sector is 81.8% and 87.4%. Relative to the denominator, the numerator's coverage is 4.7% better in 2024. Most staff dropped in 2019 work in classrooms whose age group had a trimmed or missing price (20.7% of weight; 12.3% in 2024). This raises every 2024 ratio by roughly 5% for reasons unrelated to staffing. |
| Did the 2024 price censoring move staff into the low-priced sector? | Little. Dropping the 2024 extreme rates (Choice 3) lowers the low-priced BA ratio from 0.056 to 0.054 and raises the no-BA ratio slightly. Its main effect is on the price: the censored low rates pull the 2024 low-priced price down by $0.34 to $0.41. |
| Do the classroom funding flags matter? | The model does not use them. The 2024 classroom Head Start and pre-K flags add new codes (-3 valid skip for about 5,000 centers; -5 to -7 partial knowledge), so they cannot be compared with 2019. The center-level shares of children funded by Head Start or pre-K were used instead. |
| Is the BA rise a shift in the kind of low-priced center or staff? | No. The BA share of counted low-priced staff rises from 16.4% to 21.6%, and it rises in every subgroup: lead teachers 20.7 -> 25.8%, assistants 8.8 -> 12.2%; for-profit 11.6 -> 14.9%, nonprofit 20.1 -> 28.4%; centers with Head Start or pre-K children 18.5 -> 23.8%, without 15.0 -> 19.7%; classrooms for ages 2-3 17.1 -> 25.4%. Holding the 2019 mix of roles fixed, the 2024 BA share would be 20.9% instead of 21.6%; holding the 2019 auspice, funding or age mix fixed changes it by less (`tables/diag_F_low_price_ba_share_by_subgroup.csv`). Nationally the BA share of classroom staff barely moved (30.3% -> 31.5%); in high-priced centers it was flat (32.8% -> 32.5%); in unpaid centers it fell (52.2% -> 42.3%), mainly because public-school staff moved into administrative-data centers that have no interviewed workers (public-school share of counted unpaid staff 37% -> 18%). |
| Do directors report the same rise in staffing? | No. Director-reported teachers and aides per FTE child, on the same price cells: low-priced 0.195 -> 0.205 (+5%), high-priced 0.217 -> 0.225 (+4%), unpaid 0.171 -> 0.217 (+27%). The model's measure: low-priced 0.213 -> 0.259 (+22%), high-priced 0.230 -> 0.231 (0%), unpaid 0.108 -> 0.149 (+38%). Nationally both sources show about 10% more staff per FTE child (total workforce weight rose 13.9% and total FTE enrollment 3.6%). They differ in where they put the increase. Director counts are top-coded for the largest 2% of centers in both years, which pulls them down somewhat. In 2024 directors also report a BA share of 27.3% for low-priced centers, above the workforce file's 21.6%; 2019 has no such item. |
| How precise is the workforce-based change? | A bootstrap that resamples sampling units within strata (500 draws; it ignores some parts of the design, so it understates uncertainty) gives a 2024/2019 ratio for low-priced BA staffing of 1.60, with a 95% range of 1.23 to 2.16, and for no-BA staffing 1.14 (1.01 to 1.29). The 2024 low-priced delta_0 lies between -$0.75 and -$0.22 in 95% of draws and is negative in all of them (`tables/diag_H_bootstrap_ratio_uncertainty.csv`). |

### 1.5 Summary of the causes

The negative 2024 delta_0 comes from the staffing ratios, not from prices or wages. Administrative-data centers, workforce placeholders and public programs do not explain the low-priced change. The censoring of extreme 2024 rates accounts for $0.21 to $0.35 of the -$0.47, through the price rather than through staffing.

Of the 22% rise in the model's low-priced staff per child: about 5 points come from the better coverage of the 2024 numerator, which is a by-product of the changed treatment of missing and extreme prices and affects every sector. Both the workforce file and center directors show about 10% more staff per enrolled child nationally, but directors put most of that increase in unpaid centers and little in low-priced ones (+5%), while the workforce measure puts it in low-priced and unpaid centers. The public files cannot settle which allocation is right; the workforce allocation rests on 1,265 (2019) and 1,816 (2024) worker records. The 61% rise in the BA ratio also reflects a rise in the BA share of low-priced staff from 16% to 22%, which appears in every subgroup, is statistically distinguishable from zero in the bootstrap, and is not contradicted by the 2024 director reports.

---

## 2. Supply parameters under each option, both years

Each option was computed for 2019 and 2024 with the same rule. Options (c4), (d3), (d4) and (d5) differ from (a) only in 2024, because they correct a 2024 file feature or replace 2024 wages; for 2019 they equal (a). Options (e1), (e3) and (e4) act only when delta_0 is negative, so their 2019 values also equal (a). Staff per child-hour, no BA / BA, in each sector:

| Option | Year | Unpaid center | Low-priced | High-priced | Home-based | Wages | delta_0 low / high / home | L_0 no BA / BA (bn) |
|---|---|---|---|---|---|---|---|---|
| (a) current | 2019 | 0.052 / 0.056 | 0.178 / 0.035 | 0.155 / 0.076 | 0.160 / 0.038 | 12.06 / 19.70 | 0.48 / 4.72 / 2.83 | 1.27 / 0.55 |
| | 2024 | 0.086 / 0.063 | 0.203 / 0.056 | 0.156 / 0.075 | 0.167 / 0.044 | 16.80 / 24.22 | -0.47 / 4.60 / 3.73 | 1.65 / 0.70 |
| (b1) home weighted, all ages | 2019 | as (a) | as (a) | as (a) | 0.163 / 0.033 | as (a) | 0.48 / 4.72 / 2.89 | 1.49 / 0.59 |
| | 2024 | as (a) | as (a) | as (a) | 0.164 / 0.041 | as (a) | -0.47 / 4.60 / 3.87 | 1.85 / 0.75 |
| (b2) home weighted, under 6 | 2019 | as (a) | as (a) | as (a) | 0.154 / 0.033 | as (a) | 0.48 / 4.72 / 3.00 | 1.41 / 0.58 |
| | 2024 | as (a) | as (a) | as (a) | 0.162 / 0.043 | as (a) | -0.47 / 4.60 / 3.83 | 1.79 / 0.74 |
| (c1) admin-data centers out | 2019 | 0.056 / 0.061 | 0.179 / 0.035 | 0.155 / 0.076 | as (a) | as (a) | 0.47 / 4.72 / 2.83 | 1.28 / 0.56 |
| | 2024 | 0.106 / 0.078 | 0.203 / 0.056 | 0.156 / 0.075 | as (a) | as (a) | -0.47 / 4.60 / 3.73 | 1.71 / 0.75 |
| (c2) public programs out of paid ratios | 2019 | 0.056 / 0.061 | 0.183 / 0.033 | 0.159 / 0.075 | as (a) | as (a) | 0.46 / 4.68 / 2.83 | 1.31 / 0.55 |
| | 2024 | 0.106 / 0.078 | 0.206 / 0.052 | 0.154 / 0.071 | as (a) | as (a) | -0.41 / 4.72 / 3.73 | 1.71 / 0.72 |
| (c3) director staffing | 2019 | 0.082 / 0.089 | 0.163 / 0.032 | 0.146 / 0.071 | as (a) | as (a) | 0.71 / 4.92 / 2.83 | 1.29 / 0.63 |
| | 2024 | 0.125 / 0.092 | 0.161 / 0.044 | 0.152 / 0.073 | as (a) | as (a) | 0.53 / 4.71 / 3.73 | 1.60 / 0.74 |
| (c4) 2024 extreme rates dropped | 2024 | 0.086 / 0.063 | 0.213 / 0.054 | 0.157 / 0.069 | as (a) | as (a) | -0.26 / 4.09 / 3.73 | 1.54 / 0.63 |
| (d2) wages $7.25-$139 | 2019 | as (a) | as (a) | as (a) | as (a) | 12.34 / 19.85 | 0.43 / 4.67 / 2.78 | as (a) |
| | 2024 | as (a) | as (a) | as (a) | as (a) | 16.97 / 22.27 | -0.40 / 4.72 / 3.79 | as (a) |
| (d5) spring 2024 interviews | 2024 | as (a) | as (a) | as (a) | as (a) | 16.81 / 21.94 | -0.34 / 4.77 / 3.83 | as (a) |
| (d3) CES growth | 2024 | as (a) | as (a) | as (a) | as (a) | 15.98 / 26.09 | -0.41 / 4.59 / 3.79 | as (a) |
| (d4) OEWS growth | 2024 | as (a) | as (a) | as (a) | as (a) | 15.66 / 23.56 | -0.20 / 4.83 / 3.95 | as (a) |
| (e1) floor at zero | 2024 | as (a) | as (a) | as (a) | as (a) | as (a) | 0 (price 4.77) / 4.60 / 3.73 | as (a) |
| (e2) pooled paid staffing | 2019 | as (a) | 0.166 / 0.056 | 0.166 / 0.056 | as (a) | as (a) | 0.22 / 4.98 / 2.83 | as (a) |
| | 2024 | as (a) | 0.179 / 0.066 | 0.179 / 0.066 | as (a) | as (a) | -0.30 / 4.43 / 3.73 | as (a) |
| (e3) 2019 non-labor share | 2024 | as (a) | as (a) | as (a) | as (a) | as (a) | 0.81 (price 5.58) / 4.60 / 3.73 | as (a) |
| (e4) scaled low staffing | 2024 | as (a) | 0.157 / 0.043 | as (a) | as (a) | as (a) | 0.62 / 4.60 / 3.73 | 1.47 / 0.66 |
| (e5) own-sector wages (average-wage hours) | 2019 | 0.059 / 0.075 | 0.153 / 0.026 | 0.166 / 0.059 | as (a) | as (a) | 0.95 / 4.91 / 2.83 | 1.25 / 0.52 |
| | 2024 | 0.106 / 0.079 | 0.173 / 0.047 | 0.164 / 0.068 | as (a) | as (a) | 0.26 / 4.63 / 3.73 | 1.63 / 0.70 |
| (f1) = (b2) + (c1) | 2019 | 0.056 / 0.061 | 0.179 / 0.035 | 0.155 / 0.076 | 0.154 / 0.033 | as (a) | 0.47 / 4.72 / 3.00 | 1.42 / 0.59 |
| | 2024 | 0.106 / 0.078 | 0.203 / 0.056 | 0.156 / 0.075 | 0.162 / 0.043 | as (a) | -0.47 / 4.60 / 3.83 | 1.85 / 0.79 |
| (f2) = (b2) + (c3) + (c4) | 2019 | 0.082 / 0.089 | 0.163 / 0.032 | 0.146 / 0.071 | 0.154 / 0.033 | as (a) | 0.71 / 4.92 / 3.00 | 1.42 / 0.66 |
| | 2024 | 0.125 / 0.092 | 0.164 / 0.042 | 0.156 / 0.069 | 0.162 / 0.043 | as (a) | 0.86 / 4.14 / 3.83 | 1.64 / 0.71 |

L_0 by sector and education (each sector's contribution) is in `tables/opt_supply_parameters_long.csv` under "L_0 contribution". Under (e5), 2019 wages relative to the all-center mean are 0.86 (no BA) and 0.76 (BA) in low-priced centers, 1.07 and 0.79 in high-priced centers, and 1.13 and 1.33 in unpaid centers.

What each option means economically:

- (b1), (b2) count the national number of children with listed home-based providers instead of the sample count. (b1) includes school-age children, whom the household side never counts; (b2) keeps children under 6. Both still treat every enrolled child as full-time.
- (c1) assumes administrative-data centers are staffed like interviewed unpaid centers. (c2) assumes publicly funded centers staff differently from others and that the paid-sector ratios should describe the rest. (c3) takes the director's count as the measure of staff and uses the workforce file only for the BA split, so that numerator and denominator come from the same centers.
- (c4) restores the 2019 treatment of extreme posted rates, so that both years' prices and sector boundaries are built the same way.
- The (d) options change the price of labor, not its quantity. (d3) and (d4) carry the 2019 survey levels forward with published industry or occupation growth, which gives up the 2024 survey's own levels.
- The (e) options are described under Choice 2. (e1) and (e3) change the price the model calibrates to; (e4) and (e5) change measured staffing; (e2) imposes one staffing technology on all paid centers.

---

## 3. Equilibrium mechanics

### 3.1 Equations used

From `compute_supply_residuals` in `src/3_simulation/3c_run_scenario.R` (lines 128-286):

- Labor demanded by education l: L_l = sum over sectors s of L_req[s,l] x Q_s, where Q_s is child-hours in sector s, including unpaid centers (whose price is fixed at 0 but whose staff count).
- Market wage: w_l = w_0,l x nominal wage factor x (L_l / L_0,l)^(1/e_l), with e = 4 (no BA) and 2 (BA).
- Unit cost: c_s = sum over l of L_req[s,l] x w_l (with a payroll subsidy, w_l is multiplied by 1 minus the rate).
- Equilibrium: in each paid sector, price - c_s = delta_0,s x CPI factor. The optimizer searches prices between $0.01 and $100 and accepts a solution when every gap is under $0.01.

Here the survey year itself is the simulated year, so both nominal factors are 1. Demand hours Q_s for 2019 are the ACS calibration targets (`model_projection_long.csv` from report 06). For 2024, Q_s = ACS 2019 x (NSECE household hours 2024 / 2019) in each sector, which assumes the ACS-to-NSECE ratio in each sector stays at its 2019 level. The same calculation against NSECE household hours directly (provider side against household side of the same survey) is in `choices_summary.csv` (columns `ratio_nsece_hh_*`): 0.82 / 0.74 in 2019 and 0.82 / 0.79 in 2024 under the current method.

### 3.2 Where the 91% / 78% gap comes from

Child-hours per year, billions (`tables/mech_gap_decomposition_child_hours.csv`, `tables/mech_gap_by_sector_current_method.csv`):

| | 2019 | 2024 |
|---|---|---|
| Provider-side center hours, all price cells with a sector (enters L_0) | 9.74 | 10.79 |
| of which 5-year-olds not in kindergarten | 0.48 | 0.47 |
| of which part-time children counted at 1,000 hours rather than the household side's 500 | 0.61 | 0.50 |
| Provider-side center hours, under 5, household-side hour conventions | 8.67 | 9.84 |
| NSECE household center hours, under 5 | 6.53 | 7.84 |
| ACS center hours used for demand | 6.93 | 8.20 |
| Provider-side center hours in cells with no price sector (left out of L_0) | 2.17 | 1.56 |
| Home-based: provider side as counted (unweighted) | 0.06 | 0.06 |
| Home-based: household side (NSECE / ACS) | 1.24 / 1.54 | 0.95 / 1.19 |

By sector in 2019 (no-BA staff hours, billions, L_0 against demand): unpaid centers 0.16 against 0.12, low-priced 0.58 against 0.50, high-priced 0.52 against 0.29, home-based 0.01 against 0.25. For centers alone, demand is 72% of L_0. For home-based care, demand is 24 times L_0. The overall 91% (no BA) is the net of these two errors in opposite directions. The BA ratio is lower (78%) because centers, where the provider side runs ahead of demand, hold a larger share of BA staff than home-based care.

In centers the provider side runs ahead of the household side for three reasons that can be measured (5-year-olds and the part-time hour convention, about 1.1 billion hours together) and one that cannot be separated here: enrollment counts children on the rolls, while the household side counts children who used the center in the reference week under the model's priority rules. Leaving price cells without a usable price out of L_0 offsets part of the difference. The 2024 high-priced household hours rise 57% while provider-side high-priced FTE enrollment rises 15%; the household and provider sides split centers into low- and high-priced by different rules (household out-of-pocket price against a fixed cut, provider posted rate against the age-group median), so only the total across center sectors is comparable.

Any option that weights home-based totals (b1, b2, f1) removes the home-based error and leaves the center error, so the gap widens: to 0.82 / 0.73 in 2019 and 0.82 / 0.79 in 2024 under (b2). Options that reduce center staffing or center enrollment in L_0 (c4, e4) narrow it. Options that change only delta_0 or wages leave it unchanged.

### 3.3 Response to 10% more paid center hours

Low- and high-priced center hours are raised 10%, other sectors held fixed, and wages and prices recomputed from the supply equations. This traces the supply side only; with demand feedback the quantity increase would be smaller.

| Option | Year | Labor demanded, no BA / BA | Wage, no BA / BA | Low-priced price | High-priced price | Home-based price |
|---|---|---|---|---|---|---|
| (a) current | 2019 | +6.9% / +5.7% | +1.7% / +2.8% | +1.6% (+$0.05) | +0.9% (+$0.07) | +0.9% |
| (a) current | 2024 | +7.3% / +6.7% | +1.8% / +3.3% | +2.4% (+$0.10) | +1.1% (+$0.10) | +1.1% |
| (b2) home weighted under 6 | 2024 | +7.4% / +6.7% | +1.8% / +3.3% | +2.5% | +1.1% | +1.0% |
| (c3) director staffing | 2024 | +6.6% / +5.8% | +1.6% / +2.8% | +1.7% (+$0.07) | +1.0% | +1.0% |
| (c4) extreme rates dropped | 2024 | +7.4% / +6.6% | +1.8% / +3.2% | +2.3% | +1.2% | +1.1% |
| (e1) floor at zero | 2024 | as (a) | as (a) | +2.2% (+$0.10) | +1.1% | +1.1% |
| (e3) 2019 non-labor share | 2024 | as (a) | as (a) | +1.9% (+$0.10) | +1.1% | +1.1% |
| (e4) scaled low staffing | 2024 | +7.1% / +6.5% | +1.7% / +3.2% | +1.8% (+$0.07) | +1.1% | +1.0% |
| (e5) own-sector wages | 2024 | +7.0% / +6.0% | +1.7% / +3.0% | +1.9% (+$0.08) | +1.0% | +1.0% |
| (f2) | 2024 | +6.7% / +5.6% | +1.6% / +2.8% | +1.6% | +1.0% | +0.9% |

Three features follow from the equations:

- The wage response depends only on the percentage change in labor demanded, which depends on staffing ratios and the sector mix of hours. L_0 does not enter it. Options that change only L_0 (b1, b2, c1, the demand-anchored L_0) set the level of first-year wages but leave this response unchanged.
- The dollar price response is staffing times the wage change. Options that change only delta_0 (e1, e3) leave it unchanged (+$0.10 in 2024); options that lower measured staffing (c3, e4, e5) reduce it.
- The percentage price response, which is what reaches families through the fixed price wedges, equals the labor share times the weighted wage change. A negative delta_0 makes the labor share exceed 1, so the low-priced price moves proportionally more than wages do. The same holds for wage-side policies: the 25% payroll credit lowers the 2024 low-priced price by 27.9% under the current parameters, by 25.0% with delta_0 floored at zero, and by 21% to 22% under (c3), (e3) or (e4); in 2019 it was 21.2%.

### 3.4 Checks for solver failure

See "Solver failures" at the end of the opening section. In brief: no option gives a non-positive wage, a zero or negative L_0, a sector with no staff, or a base-year price near the $0.01 bound (`check_*` and `payroll_subsidy_rate_that_breaks_*` columns in `tables/choices_summary.csv`).

---

## 4. Caveats

- The 2024 demand hours are an assumption (section 3.1). A 2024 calibration with the ACS would replace them, so the 2024 gap ratios are indicative.
- Director-reported staff counts are top-coded for the largest centers and exclude specialists; the workforce file covers classroom-assigned staff working at least 5 hours a week. Neither measures hours worked; both use 35 hours as the full-time line and count part-time staff as half.
- The bootstrap ignores parts of the survey design, so the true sampling uncertainty is larger than shown.
- Option (c4) approximates the 2019 trimming rule. The 2024 public file does not identify the pay unit behind each weekly rate, so percentiles are taken within age rather than within pay unit.
- Home-based enrollment is still treated as full-time in every option, and the household side's paid home-based category includes unlisted non-relative caregivers, whom the supply side does not cover.
- The household-side paid home-based price ($5.51 in 2019, $7.60 in 2024) was taken from the 1a outputs, not recomputed.

---

## Files

All under `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/nsece2024_options/supply/`.

Scripts (`scripts/`):

- `00_common.R`: reads the provider files with the branch's own reading and harmonization functions and rebuilds the model's supply step with switches for each option.
- `01_replicate.R`: reproduces `supply_2019.yaml` and `supply_2024.yaml`; caches processed provider data in `cache/`.
- `02_diagnose_low_price.R`, `03_explore_codes.R`, `04_low_price_ba_tests.R`: task 1 diagnostics (sector composition, dropped workforce records, price-status codes, censoring test, director counts, subgroup BA shares).
- `05_household_hours.R` with `run_household_hours.sh`: NSECE household child-hours by category (loads processed household files; run through Slurm, job 29782174).
- `06_bootstrap.R`: sampling uncertainty of the staffing ratios and delta_0.
- `07_options.R`: all options for both years and the equilibrium mechanics.
- `08_gap_decomposition.R`: provider-side against household-side child-hours.
- `09_wage_timing_check.R`: 2024 wages by interview period.
- `10_choices_summary.R`: the one-row-per-option summary.

Tables (`tables/`):

- `choices_summary.csv`: every option and year, with parameters, gap ratios, first-year wages and prices, responses to the demand increase, payroll-credit effects and solver checks.
- `opt_supply_parameters_long.csv` and `opt_supply_parameters_wide.csv`: staffing, wages, labor cost, prices, delta_0, provider-side hours, L_0 and each sector's L_0 contribution.
- `opt_d_wage_variants.csv`, `opt_d_wage_timing_2024.csv`, `opt_b_home_based_detail.csv`.
- `mech_gap_shock_checks.csv`, `mech_demand_child_hours.csv`, `mech_gap_decomposition_child_hours.csv`, `mech_gap_by_sector_current_method.csv`.
- `diag_A_center_sector_composition.csv` through `diag_H_bootstrap_ratio_uncertainty.csv`, `diag_E2_2024_center_reported_ba_share.csv`, `nsece_household_child_hours_by_category.csv`.

Logs are in `logs/`.
