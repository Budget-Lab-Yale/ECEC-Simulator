# Care prices for the 2024 NSECE: options and their consequences

Prepared 2026-10-16. (Saved as REPORT.txt because writing REPORT.md was blocked; the text is Markdown.) Nothing in any git worktree was changed. All numbers come from the public-use NSECE files (2019: ICPSR 37941; 2024: ICPSR 39466) processed with the model's own step 1a code on branch `review/nsece-2024` (commit `c05fefa`), plus the scripts in `prices/scripts/`. Dollar figures are nominal and per hour of care unless stated. "CPI day care" means the CPI-U index for day care and preschool (series CUUR0000SEEB03), which rose by a factor of 1.212 between the 2019 and 2024 annual averages.

## Choices John has to make

Each choice below lists the options and what they do to the numbers. Section numbers point to the detail.

### Choice 1. How to treat extreme posted center rates (section 1)

The 2019 public file deleted posted rates outside the 5th to 95th percentile of each pay unit (coded −2). The 2024 file keeps them but sets them equal to the 5th or 95th percentile. The 2019 file also keeps some impossible values (up to $3,650 an hour) of the kind that a new 2024 outlier step would have caught. Model sector prices (the model's formula: weighted mean of the posted weekly rate ÷ 40):

| Rule | Low-price center 2019 → 2024 | Ratio | High-price center 2019 → 2024 | Ratio |
|---|---|---|---|---|
| A. Public files as released (model today) | $3.31 → $4.30 | 1.30 | $8.08 → $9.04 | 1.12 |
| B. A, dropping rates above $50 an hour in 2019 dollars in both years | $3.31 → $4.30 | 1.30 | $6.70 → $9.01 | 1.35 |
| C1. B, and drop 2024 records at detected censoring points (both years "trimmed") | $3.31 → $4.38 | 1.32 | $6.70 → $8.71 | 1.30 |
| C2. B, and drop the same share of the 2024 tails that 2019 trimmed | $3.31 → $4.61 | 1.39 | $6.70 → $8.44 | 1.26 |
| D. B, and put the 2019 trimmed records back at estimated cap values (both years "capped") | $3.15 → $4.30 | 1.37 | $6.92 → $9.01 | 1.30 |
| E. Weighted median within each sector instead of the mean (B data) | $3.42 → $4.37 | 1.28 | $5.86 → $8.23 | 1.40 |

Rule A is the only rule under which the two years are treated differently in a way that matters: its 2019 high-price figure rests on the 2019 rates above $50 an hour (40 age-specific rates, which are 32 of the model's center-by-age-bin records). Every rule that treats the years alike puts 2019-to-2024 growth in the high-price center price at 26 to 40 percent, not 12 percent. The paid home-based price comes from household payments, so these rules do not touch it; the household-side equivalents (a common real cap on weekly payments, the 2019 top-coding rule applied to both years, trimming the top and bottom 1 percent) leave it at $5.50–5.51 in 2019 and $7.58–7.60 in 2024. The median gives $4.56 and $5.00 (choice 3).

### Choice 2. Where to draw the line between low- and high-price centers for families (section 3)

Today a family's center arrangement is "high-price" if its out-of-pocket payment per hour is at least $4.75 in 2019 dollars; the 2024 branch grows that by CPI day care to $5.76. Shares of all children under 5 (2019: high 7.3%, low 6.8%; high-price share of paid center children 52%):

| Rule for 2024 | 2024 cut ($/hour) | High-price children | Low-price children | High share of paid center | Mean household price, high / low |
|---|---|---|---|---|---|
| $4.75 × CPI day care (branch now) | 5.76 | 10.6% | 7.9% | 57% | $11.98 / $2.99 |
| $4.75 × CPI-U | 5.83 | 10.4% | 8.1% | 56% | $12.11 / $3.06 |
| $4.75 × model wage factor | 6.15 | 10.0% | 8.5% | 54% | $12.36 / $3.20 |
| $4.75, not grown | 4.75 | 11.8% | 6.7% | 64% | $11.31 / $2.61 |
| Within-year weighted median of household price (2019: $4.90) | 6.68 | 9.2% | 9.2% | 50% | $12.84 / $3.46 |
| Same percentile that $4.75 held in 2019 (48th) | 6.43 | 9.6% | 8.9% | 52% | $12.62 / $3.34 |
| Supply-side posted-rate median for the child's age (2019: $5.00 / $4.44 / $4.00) | 7.00 / 5.94 / 5.25 | 10.0% | 8.5% | 54% | $12.32 / $3.22 |

None of these changes the supply-side sector prices, which come from a separate split of centers at the weighted median posted rate by age bin. If that split is changed too, the sector prices move as shown in section 3.2 (for example, 2019 bin medians grown by CPI day care give $3.99 and $8.57 in 2024 instead of $4.30 and $9.04).

### Choice 3. Mean or median for household-based prices, and which hours basis for home-based care (sections 4 and 5)

| Price | Model today (mean of each arrangement's payment ÷ hours) | Median | Total payments ÷ total hours |
|---|---|---|---|
| Paid home-based, 2019 → 2024 | $5.51 → $7.60 (+38%) | $4.56 → $5.00 (+10%) | $4.86 → $5.74 (+18%) |
| Other Paid, 2019 → 2024 | $5.37 → $9.18 (+71%) | $3.51 → $5.86 (+67%) | $4.28 → $8.60 (+101%) |

Posted prices of listed home-based providers, put on a common basis, rose 13 to 14 percent at the median when 2024 weekly prices are converted with each provider's own weekly hours open (2019 hourly $3.33–3.73; 2024 $3.75–4.17). They rose 18 to 21 percent at an assumed 50 hours a week, and 48 to 51 percent if 2024 weekly prices are simply divided by 40. The hours-open conversion is the one that matches how NORC built the 2024 figure.

### Choice 4. Whether posted or household-reported prices anchor 2024 center prices (section 6)

The median household payment per hour for paid center care and the median posted rate ÷ 40 both rose 36 percent (household $4.90 → $6.68; posted, weighted to the same child ages, $4.43 → $6.00). They diverge for full-time families paying full price (household weekly median $165 → $270, +64%; posted median $177 → $240, +36%). The posted file has 12,000–13,000 rate records a year; the household file has about 600 paid center arrangements a year.

### Choice 5. Whether to treat the 2024 NSECE price level as real or partly a sample effect (section 2)

Posted center rates rose 31 percent in geometric mean (center-weighted, impossible values dropped), against 21 percent for CPI day care. Reweighting 2024 centers to the 2019 mix of auspice, public funding, public-school address, region, center size and age served accounts for 31 percent of the gap (50 percent with enrollment weights). Adding community poverty density and urbanicity, whose definitions changed between the surveys, raises this to 52 percent (63 percent). The growth that remains, 2.4 to 5.3 percentage points above the CPI, is about the size one would expect from two features of the CPI: it includes family child care homes, whose posted prices rose only about 10–14 percent, and it follows the same providers over time, so openings and closings of higher- or lower-priced centers do not enter it.

---

## 1. Posted center rates under comparable censoring

### 1.1 What differs between the files

- 2019: rates outside the 5th–95th percentile of each pay unit (hourly, daily, weekly, monthly and so on) were deleted and coded −2, "trimmed value" (2019 center user's guide, `CB9_STDRATE_WKLY_X`, PDF p. 317). 1,269 age-specific rates were trimmed (221 infant, 315 two-year-old, 365 three-year-old, 368 four-year-old), about 9 percent of the rates reported.
- 2024: rates outside the same range were set equal to the 5th or 95th percentile ("left- and right-censored"), separately for each pay unit and age (2024 center user's guide, CB-508). 2024 also re-imputed the pay unit for rates 4 or more standard deviations from their unit's mean. 2019 had no such step, which is why values such as $146,000 a week remain in the 2019 file.
- The pay unit is not in either public file, so the 2024 censoring points have to be inferred.

### 1.2 How the 2024 censored records were found

Censored records pile up on single values. Examples: 86 infant rates exactly $490 a week; 31 at $690.41 ($3,000 a month ÷ 4.34524); 106 three-year-old rates at $390; 63 at $69.04 ($300 a month). None of these values appears for the same age in 2019. A value is flagged as a censoring point if it has at least 8 records, lies in the bottom or top 15 percent of that age's 2024 distribution, and is at least three times as common (as a share of records) as in 2019. For a flagged value, the records in excess of the 2019 frequency are marked. This marks 1,120 records, 8 percent of 2024 rates, against 9 percent trimmed in 2019 (`p1_2024_censoring_points_detected.csv`, `p1_cap_levels_and_counts.csv`).

### 1.3 The rules

- **A. As released.** The model today.
- **B. Common rule for impossible values.** Rates above $50 an hour ($2,000 a week) in 2019 dollars, $60.60 in 2024, are set to missing in both years: 40 age-specific rates in 2019 and 8 in 2024. B stands in for the 2024 outlier step that 2019 lacked. (Dropping these records before the median split gives $6.70; the earlier report dropped them after the split and got $6.72.)
- **C1. Both years trimmed, spike rule.** B, plus the 2024 records marked in 1.2 set to missing.
- **C2. Both years trimmed, tail-share rule.** B, plus, in 2024, the lowest and highest records in each age dropped in the shares 2019 trimmed (4.5 to 4.7 percent at each end). This treats the pooled distribution as if it were one pay unit, so it removes more cheap records than the per-unit rule did.
- **D. Both years capped.** B, plus each 2019 trimmed record put back at a cap value. The cap values are the 2024 censoring points scaled by the change in that age's median (for example, the 2019 infant top cap is $409 a week and the bottom cap $111). Whether a trimmed record was low or high is inferred from the same center's other ages: 353 of the 622 centers with a trimmed rate have a valid rate at another age, and 44 percent of those rank above the median. The other 269 centers are assigned a tail at random, and results are averaged over 25 draws.
- **E. Medians.** The sector price is the weighted median within each sector and age bin instead of the weighted mean. Medians are almost unaffected by whether extreme values are dropped or capped.

### 1.4 Results

Full table: `p1_censoring_rules_sector_prices.csv`, which also gives the bin medians used for the split and each sector's share of enrollment. Distributions of rates by rule and age: `p1_censoring_rules_rate_distributions.csv`.

| Rule | Statistic | Low 2019 | Low 2024 | Ratio | High 2019 | High 2024 | Ratio |
|---|---|---|---|---|---|---|---|
| A | mean (model) | 3.31 | 4.30 | 1.30 | 8.08 | 9.04 | 1.12 |
| A | median | 3.42 | 4.37 | 1.28 | 5.88 | 8.23 | 1.40 |
| B | mean | 3.31 | 4.30 | 1.30 | 6.70 | 9.01 | 1.35 |
| C1 | mean | 3.31 | 4.38 | 1.32 | 6.70 | 8.71 | 1.30 |
| C2 | mean | 3.31 | 4.61 | 1.39 | 6.70 | 8.44 | 1.26 |
| D | mean | 3.15 | 4.30 | 1.37 | 6.92 | 9.01 | 1.30 |
| C1 | median | 3.42 | 4.45 | 1.30 | 5.86 | 7.97 | 1.36 |
| D | median | 3.24 | 4.37 | 1.35 | 6.07 | 8.23 | 1.36 |

Consequence for the model's cost split. The model sets each sector's non-labor amount per child-hour (delta_0) to price minus labor cost. With 2019 labor costs of $2.84 (low) and $3.36 (high), the 2019 high-price amount is $4.72 under rule A and $3.34 under B or C1. With 2024 labor costs of $4.77 and $4.44, the 2024 low-price amount is negative under every rule (from −$0.47 under A to −$0.16 under C2), and the high-price amount ranges from $4.00 (C2) to $4.60 (A).

The split points (weighted medians by age bin: $5.00 / $4.44 / $4.00 in 2019, $7.00 / $5.94 / $5.25 in 2024) barely move under any rule, so the shares of center enrollment in the free, low- and high-price sectors stay at about one third each.

## 2. Why posted center rates rose faster than the CPI for day care

### 2.1 The gap to be explained

The sample is every center-age record with a posted rate above $0, after rule B (12,287 records in 2019, 13,353 in 2024). "Center-weighted" uses the center weight; "enrollment-weighted" multiplies it by the center's enrollment at that age, which is closer to the price the average child faces.

| Measure, 2019 → 2024 | Center-weighted | Enrollment-weighted |
|---|---|---|
| Change in mean log rate (geometric mean) | 0.269 (+30.9%) | 0.284 (+32.8%) |
| Median ratio | 1.32 | 1.35 |
| CPI day care, annual averages | 0.192 (+21.2%) | same |
| Gap, log points | 0.077 | 0.092 |

Timing does not narrow the gap. The median 2019 center interview was in May 2019 and the median 2024 price date April 2024 (centers whose answers refer to spring 2024 are dated April 2024). CPI day care averaged over each year's interview months rose by 1.207, slightly less than the 1.212 between annual averages (`p2_interview_timing_cpi.csv`). The 11 percent of 2024 centers that reported fall 2024 prices do not explain it either: without them the change is 0.276; using only them, 0.218 (`p2_change_by_2024_price_date.csv`).

### 2.2 Composition: reweighting 2024 centers to the 2019 mix

Method. A weighted logit of survey year on center characteristics, pooled over both years, gives each 2024 record a weight proportional to the odds that a center with its characteristics was in the 2019 sample rather than the 2024 sample (DiNardo-Fortin-Lemieux reweighting; weights capped at the 99th percentile). The reweighted 2024 distribution then has the 2019 mix of those characteristics. The difference between the raw and the reweighted 2024 mean log rate is the composition effect. Characteristics used:

- age served (infant, 2, 3, 4);
- auspice and sponsorship (`PRGM_AUSPICE_EXP`: for-profit independent, for-profit chain, nonprofit independent, nonprofit sponsored, government independent, government sponsored, other);
- public funding mix (`RVNU_CENTER_FUND_COMBO`: combinations of Head Start, public pre-K and CCDF subsidy funding, or none);
- public-school address (`METH_PUBSCHFLAG`). This also captures the rise in administrative-only public-school records. Those records never have prices, so their growth shows up as fewer public-school centers among priced records: 5.7 percent of center-weighted priced records in 2019, 2.6 percent in 2024. Administrative-only records are 3.3 percent of weighted centers in 2019 and 8.8 percent in 2024 (`p2_center_file_composition.csv`);
- Census region;
- enrollment under age 6 (1–25, 26–50, 51–100, over 100);
- community poverty density and urbanicity. These are built from different data in the two surveys. Poverty density uses the same cut points but the 2013–17 ACS in 2019 and the 2018–22 ACS in 2024; the national ACS poverty rate fell from about 14.6 to about 12.5 percent between those periods, so part of the shift toward "low poverty density" is a change in the places rather than in which centers were sampled. Urbanicity cut points changed (2019: 0.85 and 0.30; 2024: 0.898 and 0.50) along with the Census urban definitions.

Main results (all priced centers; the full table with 12 combinations and three samples is `p2_reweighting_decomposition.csv`):

| Characteristics held at the 2019 mix | Reweighted 2024 change, center-weighted | Share of gap that is composition | Reweighted change, enrollment-weighted | Share of gap |
|---|---|---|---|---|
| Age only | 0.268 | 2% | 0.282 | 2% |
| Age + auspice | 0.254 | 19% | 0.255 | 31% |
| Age + public-school address | 0.262 | 10% | 0.265 | 21% |
| Age + community poverty density | 0.248 | 27% | 0.265 | 21% |
| Age + auspice + funding + public-school address | 0.253 | 21% | 0.245 | 42% |
| Variables with unchanged definitions (age, auspice, funding, public school, region, size) | 0.245 | 31% | 0.238 | 50% |
| All, including poverty density and urbanicity | 0.229 | 52% | 0.226 | 63% |

The share is (raw change − reweighted change) ÷ (raw change − 0.192). Against the interview-month CPI (0.188) the shares are 2 to 3 points lower. A regression of the log rate on a 2024 indicator and the same characteristics gives a within-group change of 0.242 (unchanged-definition variables) and 0.232 (all variables), with standard errors of about 0.005 that ignore the survey design (`p2_regression_year_effect.csv`).

Which characteristics matter:

- **Auspice.** For-profit chains rose from 6.7 to 10.1 percent of priced records and charge the most ($340 a week median in 2024). Nonprofit sponsored centers fell from 12.3 to 8.8 percent. "Other or unknown" auspice rose from 1.5 to 3.5 percent, partly a coding change.
- **Public schools.** Restricting both years to centers not at a public-school address lowers the raw change from 0.269 to 0.258 (14 percent of the gap). This is the combined effect of the administrative-only records and of fewer interviewed public-school programs charging fees.
- **Funding mix and region** add little once age and auspice are held fixed.
- **Poverty density** has a large apparent effect for the reason given above; treat it as an upper bound.

### 2.3 The same groups in both years

Geometric-mean ratio, 2024 to 2019, by group, center-weighted (`p2_same_groups_across_years.csv`): for-profit independent 1.35, for-profit chain 1.32, nonprofit independent 1.23, nonprofit sponsored 1.15; Northeast 1.32, Midwest 1.31, South 1.26, West 1.40; centers with over 100 children under 6 1.41, smaller centers 1.27–1.31; infants 1.30, 2-year-olds 1.33, 3-year-olds 1.28, 4-year-olds 1.31. For-profit centers, 56 to 58 percent of priced records, rose faster than the CPI in every group.

### 2.4 What the CPI measures, and how much it could explain

From BLS documentation:

- Scope. The item "Day care and preschool" (EB031) "includes charges for the care of pre-elementary school children," including pre-kindergarten programs and co-operative schools, and "individuals whose occupation is to regularly care for pre-elementary school children." It excludes babysitting and nannies, and "camp, kindergarten and any programs (such as extended before and after school day care) for grade school students" ([BLS entry-level item descriptions](https://www.bls.gov/cpi/additional-resources/entry-level-item-descriptions.htm)). So it includes family child care homes and part-day preschool tuition, neither of which is in the NSECE center posted-rate measure, and it excludes school-age care.
- Same providers over time. Price change is computed from the same item at the same outlet in two periods: "It is important that the price of each quote be collected (or estimated) in both periods in order to measure price change." When an item can no longer be priced, the replacement is either judged comparable, adjusted for quality, or treated as missing and imputed from the price change of other quotes in the same class ([BLS Handbook of Methods, CPI calculation](https://www.bls.gov/opub/hom/cpi/calculation.htm)). No quality adjustment specific to day care is documented. The consequence is that differences in price level between centers that open and centers that close never enter the CPI, whereas a comparison of two cross-sections such as the NSECE includes them.
- Sample rotation. Each area's outlet sample is refreshed on a standard 4-year cycle ([BLS notice, 2019](https://www.bls.gov/cpi/notices/2019/methodology-changes-2019.htm)), and outlets are selected from Consumer Expenditure Survey data ([BLS Handbook, CPI data sources](https://www.bls.gov/opub/hom/cpi/data.htm)). New outlets enter without changing the index level.
- Price concept and population. The CPI uses "the consumer's out-of-pocket price"; means-tested subsidies are "treated as changes to the recipient's income and are out of scope." The CPI-U covers urban consumers only ([BLS Handbook, CPI concepts](https://www.bls.gov/opub/hom/cpi/concepts.htm)). Dropping rural centers changes the NSECE raw change only from 0.269 to 0.262.

A rough reconciliation. Among children under 5 in the NSECE household file, centers took 56 percent of out-of-pocket care spending in 2019 and paid home-based providers 16 percent (`p2_spending_shares_under5.csv`). Home-based care is therefore about 22 percent of the spending the CPI item covers, leaving out relatives, nannies and other organizational care. Posted home-based prices rose about 10–14 percent at the median on a comparable basis (section 4), about 0.11 in logs. Blending 78 percent of a composition-adjusted center change of 0.229–0.245 with 22 percent of 0.11 gives 0.20–0.22, against the CPI's 0.192. This is an illustration with stated weights, not an estimate. It shows that the remaining gap is the size that the CPI's coverage of home-based care and its same-provider design would produce. The NSECE cannot measure the effect of center openings and closings directly because it does not follow the same centers.

## 3. The low/high price split

### 3.1 Household side

The cut is used only at `1a_nsece_processing.R:640–646` (branch), where each paid center arrangement is labelled low- or high-price by the family's out-of-pocket payment per hour. The labels feed (a) the random forest that predicts each ACS child's care category, so they set the base-year shares, and (b) the price wedges, which are each family's out-of-pocket price divided by the sector price. Nothing in calibration or simulation applies the cut again.

Full table, including the cut at each age and medians: `p3_household_split_rules.csv`. The 2019 row is the same under every rule except the within-year median ($4.90: high 7.1%, low 7.1%) and the supply-side median by age ($5.00 / $4.44 / $4.00: high 7.6%, low 6.6%).

| Rule | 2024 cut | High 2024 | Low 2024 | Household mean high / low 2024 | Household median high / low 2024 |
|---|---|---|---|---|---|
| (2019, $4.75) | 4.75 | 7.3% (2019) | 6.8% (2019) | $9.24 / $2.69 (2019) | $7.27 / $2.86 (2019) |
| a1. $4.75 × CPI day care | 5.76 | 10.6% | 7.9% | $11.98 / $2.99 | $10.20 / $3.34 |
| a2. $4.75 × CPI-U | 5.83 | 10.4% | 8.1% | $12.11 / $3.06 | $10.29 / $3.43 |
| a3. $4.75 × wage factor | 6.15 | 10.0% | 8.5% | $12.36 / $3.20 | $10.56 / $3.55 |
| a4. $4.75 not grown | 4.75 | 11.8% | 6.7% | $11.31 / $2.61 | $9.77 / $3.01 |
| a5. $4.75 × growth of the posted-rate median (1.32) | 6.27 | 9.8% | 8.7% | $12.48 / $3.27 | $10.70 / $3.60 |
| b1. within-year median of household price | 6.68 | 9.2% | 9.2% | $12.84 / $3.46 | $10.98 / $3.76 |
| b2. 2024 price at the percentile $4.75 held in 2019 (48.3rd) | 6.43 | 9.6% | 8.9% | $12.62 / $3.34 | $10.86 / $3.60 |
| c1. supply-side posted median for the child's age bin | 7.00 / 5.94 / 5.25 | 10.0% | 8.5% | $12.32 / $3.22 | $10.56 / $3.55 |

Paid center care as a whole rises from 14.1 to 18.5 percent of children under every rule; the rules only divide it differently. The high-price share of paid center children is 52 percent in 2019 and between 50 and 64 percent in 2024.

### 3.2 Supply side

The supply-side split does not depend on the household cut. Alternatives (`p3_supply_splits_sector_prices.csv`; rule A data shown, rule B in the same file):

| Supply-side split | Low 2019 | Low 2024 | High 2019 | High 2024 | High-price share of center enrollment, 2024 |
|---|---|---|---|---|---|
| Within-year weighted median by age bin (model) | 3.31 | 4.30 | 8.08 | 9.04 | 36% |
| Within-year 40th percentile | 3.10 | 3.96 | 7.44 | 8.47 | 43% |
| Within-year 60th percentile | 3.55 | 4.67 | 8.94 | 9.74 | 28% |
| 2019 bin medians × CPI day care | 3.31 | 3.99 | 8.08 | 8.57 | 42% |
| 2019 bin medians × CPI-U | 3.31 | 4.03 | 8.08 | 8.64 | 41% |
| 2019 bin medians × wage factor | 3.31 | 4.17 | 8.08 | 8.87 | 38% |
| Single $4.75 posted cut × CPI day care | 3.43 | 4.17 | 8.57 | 8.94 | 37% |

Under the model's split the high-price sector holds 34 percent of 2019 center enrollment; free centers hold about a third.

### 3.3 How each rule interacts with policies stated in nominal dollars

- The cut is applied once, to the base-year survey. In simulation years the sector prices move with the equilibrium, and families do not change category because a price crossed a dollar line. The indexing choice therefore sets the base-year composition and price levels; it does not cause drift in later years.
- A within-year percentile rule (b1, b2, c1) makes the classification independent of the price level. The boundary then means "below or above the typical price that year," which matches the supply side's within-year median split. A fixed real cut (a1–a5) builds in an inflation assumption. Because center prices rose about 36 percent while CPI day care rose 21 percent, rule a1 puts more paid center children in the high-price group (57 percent against 52 percent in 2019) and lowers the average price within that group.
- Policy amounts that are fixed in dollars (for example the CDCTC expense limits) or indexed to a general index (chained CPI, wages) interact with each sector's price level and with how many families are in each sector. Under a4 the high-price group is largest and cheapest on average ($11.31); under b1 it is smallest and most expensive ($12.84). A fixed-dollar benefit therefore covers a larger share of high-price costs under a4 than under b1. The copayment schedules in `config/policy_demand/` (a share of income, applied to the full cost of paid care) depend on sector price levels, not on the cut itself.
- The wedges change with the cut. The household mean divided by the sector price is 0.81 (low) and 1.14 (high) in 2019, and 0.69 and 1.33 in 2024 under a1 (`price_summary.csv` in the 1a output). A rule that raises the household cut (b1) raises both household means and so both wedges.

## 4. Home-based prices on a common hourly basis

### 4.1 How each year's posted price was built

- 2019 (`HB9_CHRG_RATE_X_TC`): an hourly price. Providers quoting a weekly or other rate were asked how many hours a week it covers, and the hourly price is the rate divided by those hours (2019 home-based user's guide, PDF pp. 124–126).
- 2024 (`HB4_CHRG_WEEK_RATE_X_TC`): a weekly price. Hourly rates were multiplied by the provider's weekly hours open, daily rates by days open, and monthly rates by 12 ÷ 52. The question on hours covered was dropped (2024 home-based user's guide, HB-112).
- Both years are top-coded with the same rule (top 2 percent within listed/unlisted and community poverty groups).

The conversion that matches both constructions is the provider's own weekly hours open (`HRSOPEN_R_MON` to `FRI`, available in both years; median 53.2 hours in 2019 and 50.5 in 2024 among listed providers open at least 10 hours). For providers quoting a weekly rate, NORC's 2019 hourly price used hours covered rather than hours open, so dividing 2024 weekly prices by hours open understates the 2024 hourly price when the two differ. That biases the growth figure down somewhat.

### 4.2 Results (listed providers, provider weights, medians; `p4_home_based_posted_common_basis.csv`)

| Age group | 2019 hourly | 2024 weekly | 2024 ÷ hours open | Growth | 2024 ÷ 50 | Growth | 2024 ÷ 40 | Growth |
|---|---|---|---|---|---|---|---|---|
| Under 1 | 3.73 (3.70 among providers with hours open) | 225 | 4.17 | +13% | 4.50 | +21% | 5.63 | +51% |
| 1–2 | 3.50 | 207 | 4.00 | +14% | 4.14 | +18% | 5.18 | +48% |
| 3–4 | 3.33 | 200 | 3.75 | +13% | 4.00 | +20% | 5.00 | +50% |

On a weekly basis (2019 hourly × hours open against the 2024 weekly price): $200 → $225, $187 → $205, $180 → $200, growth of 9 to 13 percent.

### 4.3 Household payments for paid home-based care (`p4_home_based_household_payments.csv`, `p1_p4_home_based_household_price_rules.csv`)

| | 2019 | 2024 |
|---|---|---|
| Arrangements (children under 5) | 208 | 125 |
| Mean hourly (model price) | $5.51 | $7.60 |
| Median hourly | $4.56 | $5.00 |
| Total payments ÷ total hours | $4.86 | $5.74 |
| Median weekly payment | $150 | $150 |
| Median weekly hours | 40 | 35 |
| Share at 35+ hours | 64% | 52% |
| Median weekly payment, 35+ hours | $175 | $151 |
| Median weekly payment, 35+ hours, no subsidy or third-party payment | $195 | $175 |
| Share of children above 400% of poverty | 53% | 67% |

The 38 percent rise in the model's paid home-based price comes from taking the mean of each arrangement's hourly price. Part-time arrangements, which have high hourly prices, became more common, and the 2024 sample has more high-income families. Among families below 400 percent of poverty the mean was flat ($4.91 → $4.81); above 400 percent it rose from $6.05 to $8.99 (`p5_in_home_care_diagnostics.csv`). Compared with posted prices, the household median hourly payment ($4.56, $5.00) is somewhat above the providers' posted median on the hours-open basis ($3.33–3.73 in 2019, $3.75–4.17 in 2024), and full-time weekly payments ($175, $151) are below posted weekly prices ($180–200, $200–225). The household sample is small (63 full-time arrangements in 2024), so none of the household medians is precise.

## 5. The "Other Paid" price

### 5.1 How it is built

In `1a_nsece_processing.R` (branch, around line 1390) the price is the child-weighted mean of out-of-pocket weekly payment ÷ weekly hours, over regular "Other Paid" arrangements of children under 5 with a positive payment. "Other Paid" collects paid individual care that is in the child's home or given by someone with a prior relationship to the family (type of care 1 or 2), plus other organizational care (type 5: drop-in care, single-activity programs and similar). It has no supply sector; in simulation years the base price is grown by CPI-U.

### 5.2 Why it rose 71 percent (`p5_other_paid_by_subtype.csv`, `p5_other_paid_shift_share.csv`)

| Subtype | Share of Other Paid, 2019 | 2024 | Mean hourly 2019 | 2024 | Median hourly 2019 | 2024 |
|---|---|---|---|---|---|---|
| Paid, no prior relationship, in the child's home (nanny-type) | 14% | 26% | 8.76 | 16.29 | 7.75 | 18.75 |
| Paid, prior relationship, in the child's home | 20% | 19% | 6.20 | 6.96 | 4.29 | 4.00 |
| Paid, prior relationship, outside the child's home (paid relatives and friends) | 30% | 35% | 3.38 | 6.03 | 2.74 | 4.27 |
| Other organizational care (type 5) | 35% | 20% | 5.17 | 7.51 | 3.52 | 6.72 |
| All Other Paid | | | 5.37 | 9.18 | 3.51 | 5.86 |

- At the 2019 subtype mix, the 2024 mean would be $8.23; at the 2024 mix, the 2019 mean would be $5.67. Roughly a quarter of the rise is the shift between subtypes and three quarters is price growth within subtypes.
- The nanny-type group drives both parts. Its share nearly doubled, its median hourly price more than doubled, and 95 percent of its 2024 children are above 400 percent of poverty (56 percent in 2019). For nanny-type arrangements of 30 or more hours, the median weekly payment went from $200 to $736.
- The 2024 change that moved some type 5 care into center care (new item C17) shows up as type 5 falling from 35 to 20 percent of Other Paid, and from 4.6 to 3.6 percent of children. It moves the mean only through the subtype shift above.
- Payment top-coding does not explain the rise. The 2019 public file caps weekly payments at $675; capping 2024 at the same real value ($818) gives $8.99 instead of $9.18 (`p1_p5_common_payment_cap.csv`). The mean weekly payment itself doubled ($124 → $258) while mean hours barely moved (29 → 30), so the rise is in what families pay, not in how hours are measured. Calendar households alone give $5.41 → $8.80.
- The rule that routes individual care to Other Paid or to paid home-based care is the same in both years. `p5_individual_care_routing.csv` tabulates every combination of type, location and prior relationship in both years.

For reference, carrying 2019 forward the model's way gives $6.59 for 2024 (2019 × CPI-U).

## 6. Household-reported versus posted center prices

Files: `p6_household_vs_posted_centers.csv`, `p6_household_vs_posted_by_age.csv`, `p6_household_prices_by_model_sector.csv`. Posted rates are weighted by center weight × enrollment at the child's age and reweighted to the age mix of children in paid center care. Household figures cover regular paid center arrangements (type 4) of children under 5.

| | 2019 | 2024 | Ratio |
|---|---|---|---|
| Posted weekly rate, median | $177 | $240 | 1.36 |
| Posted rate ÷ 40, median | $4.43 | $6.00 | 1.36 |
| Household payment per hour, median (all paid center care) | $4.90 | $6.68 | 1.36 |
| Household payment per hour, mean | $6.08 | $8.14 | 1.34 |
| Household weekly payment, median, 35+ hours | $156 | $236 | 1.51 |
| Household weekly payment, median, 35+ hours, no subsidy or third-party payment | $165 | $270 | 1.64 |
| That figure ÷ posted median | 0.93 | 1.13 | |
| Share of paid center children with a subsidy, copayment or third-party payment | 18% | 20% | |

By model sector, household means are $2.69 → $2.99 (low) and $9.24 → $11.98 (high), against supply-side sector prices of $3.31 → $4.30 and $8.08 → $9.04 ($6.70 → $9.01 under rule B). In the low-price group the share with a subsidy or third-party payment rose from 24 to 37 percent, which holds down its out-of-pocket mean.

At the median the two sources agree on growth (36 percent). Full-time families paying full price paid less than the posted median in 2019 and more in 2024. That fits a 2024 household sample with more high-income families using more expensive centers (the share of children above 400 percent of poverty rose from 24 to 31 percent). The posted file is about 20 times larger and is weighted to the population of centers. The household file measures net out-of-pocket payments and their spread across families, which the model needs for the price wedges.

## Caveats

- No survey-design standard errors were computed, and the regression standard errors ignore clustering. Household cells are small: 125 paid home-based arrangements and 58 nanny-type arrangements in 2024.
- The 2024 censoring points are inferred from spikes. Some genuine round-number rates may be marked and some censored records at round numbers missed; rules C1 and C2 bracket this.
- Reweighting can hold fixed only what the public files record. It cannot capture changes in quality, hours or staffing not reflected in these variables, and the poverty-density and urbanicity variables are not defined the same way in both years.
- The CPI reconciliation in 2.4 uses rough spending weights from the NSECE household file and a home-based price change from posted prices with an imperfect hours conversion.

## Files

All under `/nfs/roberts/project/pi_nrs36/jar335/Repositories/ECEC-Simulator-caregiver-review/analysis/nsece2024_options/prices/`.

- Scripts (`scripts/`): `centers.R` (tasks 1, 2 and the supply side of 3; reads only the center files and ran on the login node), `households.R` (tasks 3–6, run through Slurm with `run_households.sh`), `other_paid_diag.R`, `common_cap.R`, `spending_shares.R` (Slurm, with matching `run_*.sh`), `rr.sh` (runs R with the project module). Logs are in `logs/`.
- `downloads/bls_cpi_monthly.csv`: monthly CPI series from the BLS API (day care and preschool, all items, tuition and childcare), 2018–2025.
- CSVs (`csv/`), by task: `p1_*` censoring rules; `p2_*` decomposition, timing, group comparisons and spending shares; `p3_*` split rules, household and supply side; `p4_*` home-based common basis and household payments; `p5_*` Other Paid; `p6_*` household versus posted.
