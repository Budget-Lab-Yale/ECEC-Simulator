# ECEC married tax-unit identifier investigation

August 17, 2026

## Decision summary

The 2019 IPUMS `TAXID` used by the ECEC Simulator does not reliably place married spouses in the same constructed tax unit. The Census Bureau released corrected 2019 `Tax_unit` values in April 2024, but ECEC reads the uncorrected identifier directly from its legacy IPUMS extract. Among married two-parent units with a child under age 5, the share with a common tax-unit identifier rises from 0.134% in the ECEC input to 99.352% in the corrected Census file.

This input error causes ECEC to represent nearly all married couples as two separate tax units, even though it labels both units married filing jointly. The main established consequence is incorrect tax-donor matching. For roughly 57% of affected units by weight, ECEC prices the caregiver's earnings change using a separate zero-AGI donor record instead of the couple's joint tax context. Because taxes enter household choices and calibration, the error can affect employment, childcare demand, equilibrium prices, fiscal results, and poverty estimates.

The direction and aggregate size of those effects are not yet known. Refundable-credit phase-ins and phaseouts mean that a zero-AGI donor does not necessarily produce a lower marginal tax rate. The corrected tax units should therefore be incorporated before the model is locked, and the tax calculations should be rerun before drawing conclusions about the sign or materiality of changes to published results.

Four findings govern the repair:

1. The official corrected `Tax_unit` values join exactly to all 3,087,291 people in ECEC's 2019 ACS universe.
2. The correction changes tax-unit identifiers, not adjusted gross income (AGI), except for rounding of no more than $0.50.
3. The corrected file leaves a small number of married couples in separate constructed tax units, so ECEC should use the official identifiers rather than merge every married spouse pair.
4. Replacing `TAXID` alone would leave a second error: when parent 2 is the caregiver in a joint unit, ECEC routes the earnings change to the joint tax unit but selects the marginal-rate schedule using parent 1's earnings.

## The methods define joint filing units; the code trusts the raw identifier

The [ECEC methods](../../references/childcare_methods.pdf) describe a coherent distinction among households, parent units, and tax units. A parent unit is the behavioral unit and may contain a single parent or a married or cohabiting couple. Married joint filers contribute both parents' earnings to one joint AGI. Unmarried cohabitants retain separate AGIs and tax liabilities, which ECEC aggregates to the parent unit. Each ACS tax unit is matched separately to a tax donor. The [ECEC report](../../references/childcare_report.pdf) uses the same concept when it defines a family as people who would file together, with unmarried cohabitants as the exception.

The code assumes that raw `TAXID` equality implements this conceptual distinction. It does not independently construct or validate married tax units:

- The processing script filters the IPUMS file to 2019 and renames `TAXID` and `ADJGINC` directly to `tax_unit_id` and `agi` ([`src/1_processing/1b_acs_processing.R:492-511`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/1_processing/1b_acs_processing.R#L492-L511)).
- `SPLOC`, the IPUMS spouse or partner pointer, is selected but never used after ingestion. Parent units instead use `MOMLOC`, `POPLOC`, `MOMLOC2`, and `POPLOC2`.
- Tax records are built by grouping on `(hh_id, tax_unit_id)`, and any group containing a person with `MARST == 1` receives married-filing-jointly status ([`1b_acs_processing.R:697-710`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/1_processing/1b_acs_processing.R#L697-L710)). A married spouse alone in a separate raw-ID group therefore becomes a separate married-filing-jointly tax unit.
- The second parent's tax record is suppressed, collapsing the couple to one joint filer, only when the two raw tax IDs are equal ([`1b_acs_processing.R:1479-1531`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/1_processing/1b_acs_processing.R#L1479-L1531)).
- The CDCTC config defines a couple as married when its tax IDs are equal and cohabiting when they differ ([`config/cdctc/baseline.R:125-136`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/config/cdctc/baseline.R#L125-L136)).
- Each resulting tax record is matched separately to a donor on AGI, filing status, and dependents; donor effective rates then apply to baseline AGI and donor marginal rates to earnings changes ([`src/3_simulation/3b_run_simulation_year.R:940-1005`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/3_simulation/3b_run_simulation_year.R#L940-L1005)).

The release code therefore uses three related but non-equivalent definitions. General household fields define `married` from `MARST`; tax groups define joint filing status from `MARST` within each raw-ID group; and the joint-AGI and CDCTC logic infer marriage from raw-ID equality. The identifier error is most consequential in the last two uses.

The [April 14, 2026 initial public commit](https://github.com/Budget-Lab-Yale/ECEC-Simulator/commit/be79743c251ea3ee879af8d996d6c1b99a09b4c4) contains this implementation. The relevant ACS-processing and CDCTC files are unchanged on public `main` as of August 17, 2026. The stored March 2026 run does not record a git commit, but its parent tax IDs match the audited input file exactly.

## The source file contains the legacy identifiers

The problem originates in the ACS-SPM tax-unit field rather than in ECEC's joins. The [IPUMS documentation](https://usa.ipums.org/usa-action/variables/TAXID) defines `TAXID` as an identifier for a TAXSIM tax unit and warns that IPUMS has not incorporated the Census Bureau's correction to the 2009-2019 ACS-SPM identifiers. The [Census ACS-SPM documentation](https://www2.census.gov/programs-surveys/supplemental-poverty-measure/datasets/spm/spm-asc-readme.pdf) says that its 2009-2019 research extracts were updated to correct `Tax_unit`. The corrected files became available in April 2024 through the [Census ACS-SPM dataset page](https://www.census.gov/data/datasets/time-series/demo/supplemental-poverty-measure/acs-research-files.html): every 2009-2019 and 2021 extract carries an April 9, 2024 file timestamp, and the page revision announcing them is April 10, 2024. The Fall 2021 release named in the readme is the vintage that introduced the flawed `TAX_ID` (the one IPUMS still carries), and the page's current April 16, 2025 release date reflects an unrelated re-release of the 2022 and 2023 files.

ECEC does not read or crosswalk those corrected values. Its configured input, `/nfs/roberts/project/pi_nrs36/shared/raw_data/ACS/v1/2023/ipums_usa.csv`, contains the legacy IPUMS `TAXID`. The stored March 2026 run preserves those values exactly for both parent slots.

The tax-unit identifier is also a constructed input rather than an observed tax return. The ACS does not observe whether a household member files a return or whether married spouses file jointly. Consequently, a corrected `Tax_unit` is the Census tax model's filing-unit assignment, not proof of actual filing behavior.

## The corrected Census file resolves nearly all married-spouse splits

The corrected 2019 Census ACS-SPM file joins to the ECEC input on:

```text
Census SERIALNO = IPUMS CBSERIAL - 2019000000000
Census SPORDER  = IPUMS PERNUM
```

All 3,087,291 IPUMS people in ECEC's `YEAR == 2019` and `GQ %in% c(1, 2)` universe match exactly. Age, sex, and person weight agree for every match. Neither the legacy nor corrected identifier collides across IPUMS households.

### Married-Couple Tax-Unit Agreement

| Population | Units or pairs | Legacy ID agreement | Corrected ID agreement |
|---|---:|---:|---:|
| Reciprocal married spouse pairs in the ECEC ACS universe | 669,263 | 0.1303% unweighted; 0.1326% weighted | 99.3733% unweighted; 99.0530% weighted |
| Stored married two-parent units linked to a child age 12 or younger | 169,945 | 0.1200% unweighted; 0.1166% weighted | 99.4033% unweighted; 99.1403% weighted |
| Married two-parent units with at least one child under age 5 | 82,740 | 0.1342% unweighted; 0.1383% weighted | 99.3522% unweighted; 99.0686% weighted |

Weighted spouse-pair statistics use the average of the spouses' person weights; parent-unit statistics use parent 1's person weight, consistent with ECEC's unit weighting.

The corrected file reduces the number of distinct tax units in the ECEC universe from 2,347,451 to 1,689,811. It changes the identifier for 664,356 people, or 21.52% of the ECEC universe. Among reciprocal married pairs, 664,198 move from different legacy IDs to one corrected ID.

The mapping is not a simple spouse-union crosswalk. The corrected file merges two legacy IDs in 664,354 corrected units, but 6,578 legacy IDs also split across multiple corrected units. A rule that merges all reciprocal married spouses reproduces the corrected partition with 98.34% pairwise precision and 99.75% recall. The official corrected identifiers are therefore preferable to a locally inferred spouse merge.

## Some married couples remain separate in the corrected file

The official correction does not assign every reciprocal married pair to one tax unit. It leaves 4,194 pairs split, including 4,193 that were also split under the legacy identifier and one that moves from a shared legacy ID to separate corrected IDs.

These remaining pairs do not appear to be one simple coding residual. Same-sex couples account for 19.03% unweighted and 18.24% weighted. Of the 4,194 pairs, 2,292 have two unequal positive AGIs, 1,309 have exactly one zero AGI, 447 have both AGIs at zero, and 146 have equal positive AGIs. They also span several household and subfamily relationship patterns.

Because the corrected identifier is a constructed filing unit, these pairs may include married people assigned to separate tax units as well as remaining anomalies. ECEC should preserve the corrected partition and audit these observations separately. It should not assume that reciprocal `SPLOC` plus `MARST == 1` always implies one tax unit. Conversely, `SPLOC` alone is insufficient because it also links unmarried partners.

## AGI remains attached to legacy tax-unit fragments

The Census correction does not substantively change person-level AGI. The corrected Census `AGI` equals IPUMS `ADJGINC` exactly for 99.28% of people and differs by no more than $0.50 for every person. The small differences reflect numeric storage and rounding.

The corrected file also does not repeat one tax-unit AGI on every member record. Only 61.97% of corrected tax units have identical AGI values across all members. Among the 664,198 married pairs that move from separate legacy IDs to one corrected ID, 642,426 have exactly one spouse with zero AGI, 21,765 have both spouses at zero, and seven have two unequal positive AGIs. The dominant pattern therefore remains a joint or tax-unit AGI carried on one legacy fragment and zero on the other.

These records rule out a blanket `max(AGI)` repair. Among all 1,689,811 corrected tax units, `first(AGI)` differs from `max(AGI)` in 363 cases. In every one of those cases, `first(AGI)` matches the AGI attached to the legacy ID selected as the corrected ID, while `max(AGI)` does not. The seven positive-positive married pairs create a different problem: either `first` or `max` would omit a positive component, while the sum is economically plausible in the inspected cases.

A production repair should therefore retain both legacy and corrected identifiers long enough to reconstruct AGI explicitly. One candidate is to deduplicate AGI within each legacy tax-unit fragment and then aggregate the fragments assigned to a corrected tax unit. That rule handles the dominant positive-plus-zero pattern and retains both components in the seven positive-positive cases. However, the legacy units that split across corrected units require separate treatment, and the full rule should be validated against the Census tax-model construction before a production rerun.

## The main established error is tax-donor matching

The legacy split preserves combined parent-pair AGI in aggregate, but it places that AGI and the children on one parent's nominal tax unit. The other married spouse usually receives a separate zero-AGI tax unit that ECEC also labels married filing jointly.

Among the under-5 married units whose legacy IDs differ (82,629 of the 82,740), 99.15% have exactly one zero-AGI record. The primary caregiver sits on the zero-AGI record in 56.66% unweighted and 57.24% weighted. For those units, ECEC matches the caregiver's earnings change to a zero-AGI, generally no-dependent donor instead of the couple's joint AGI and dependent context.

This mismatch directly changes the tax calculation used in the caregiver's employment alternatives. It can therefore affect:

- calibration through after-tax household income;
- employment and childcare choices through scenario taxes and net income;
- childcare demand, allocations, and equilibrium prices;
- subsidy and tax-revenue estimates; and
- poverty and distributional measures.

The mismatch does not establish a uniform direction. Donor schedules include refundable-credit phase-ins and phaseouts, so a donor near zero AGI may have a higher, lower, or negative effective marginal tax rate relative to a joint-income donor. Claims that the current model necessarily understates marginal rates, overstates work incentives, or biases fiscal results in one direction require a corrected comparison.

Means-tested eligibility and baseline-income measures may be less exposed than the marginal-rate calculation because the model generally recombines the positive and zero parent records. That protection is approximate and should be tested at the record level.

## CDCTC exposure is real but concentrated in rare cases

The Child and Dependent Care Tax Credit (CDCTC) code treats different parent IDs as evidence that a couple is cohabiting. This classification is wrong for nearly all married couples in the legacy input, but its mechanical effect is narrower than the classification rate suggests.

Almost every young child in the affected married population shares the parent 1 ID that also carries the joint AGI. Only six model-relevant split units have young children divided across both parent IDs, the main route through which two credits could stack within one parent unit. Among linked under-5 children in adult, two-parent units where both parents have ECEC's married flag, 150 match neither legacy parent ID; the corrected identifier reduces that count to 134. The remaining mismatch is primarily a separate child-claiming or parent-link issue rather than evidence that the married-unit correction failed.

The corrected rerun should still recalculate CDCTC from the tax-unit level because rare observations can receive the wrong expense assignment or separate credit. It should not assume that the raw split classification produces a broadly higher credit.

## Correcting the identifier would expose a caregiver-slot error

A direct `TAXID` replacement would not fully correct the tax calculation. When parents share a tax-unit ID, `1b_acs_processing.R:1495-1530` suppresses parent 2's AGI and routes either caregiver's earnings change to tax unit 1. However, `3b_run_simulation_year.R:1270-1287` selects tax unit 1's marginal-rate schedule with `lookup_emtr(donor_id1, earnings1)`.

When parent 2 is the caregiver, the joint donor is the appropriate donor, but `earnings1` belongs to the noncaregiver. The stored data make this path common: the corrected surviving ID is always in parent slot 1, while the caregiver is attached to the other legacy fragment in roughly 56% of affected units. The repair must route the earnings change to the joint tax unit and select the marginal-rate schedule using the caregiver's earnings or another explicitly documented joint-return rule.

## Established findings and unresolved outcome effects

The evidence establishes that:

- ECEC uses an IPUMS vintage with a documented `TAXID` correction problem;
- ECEC does not use or reproduce the corrected Census tax units;
- the corrected Census file joins exactly to the model input;
- nearly all model-relevant married couples are currently represented as two married-filing-jointly tax records;
- separate donor matching places many caregiver earnings changes in the wrong AGI and dependent context;
- person-level AGI was not substantively corrected and requires explicit aggregation after the identifier crosswalk; and
- a shared-ID repair also requires a caregiver-slot correction.

The evidence does not yet establish:

- whether the current model's marginal rates are systematically too high or too low;
- the direction or size of changes in employment and childcare choices;
- the direction or size of changes in subsidy spending, tax revenue, or poverty; or
- whether any published policy estimate changes materially.

These unresolved outcome effects require model comparison, not additional inspection of the legacy identifiers.

## Required repair and validation sequence

1. **Use the official corrected identifiers.** Crosswalk Census `Tax_unit` on `CBSERIAL` and `PERNUM`, retain household scope, and preserve both legacy and corrected IDs through validation.
2. **Define one AGI per corrected unit.** Reconstruct AGI from deduplicated legacy fragments, resolve legacy-unit splits, and validate edge cases rather than taking `first`, `max`, or an undeduplicated person sum by default.
3. **Represent joint units once.** Construct one filing status, dependent assignment, AGI, and donor match for each corrected joint tax unit. Continue to key tax units by `(hh_id, tax_unit_id)` and assert uniqueness.
4. **Fix caregiver earnings routing.** Route the earnings change to the joint unit while selecting the marginal-rate schedule from the actual caregiver's earnings under a documented rule.
5. **Add construction checks.** Require complete crosswalk coverage, stable person and parent-unit counts, reciprocal spouse diagnostics, unique household-tax-unit keys, traceable child expense assignments, and explicit parent-2 caregiver tests.
6. **Recalculate taxes before equilibrium.** Holding choices fixed, compare donor matches, effective and marginal tax rates, baseline liabilities, earnings-induced liability changes, and CDCTC amounts.
7. **Rerun calibration and policy scenarios.** With identical seeds, decompose changes in choices, employment, childcare demand, prices, subsidies, tax revenue, budget effects, poverty, and distributional results.
8. **Record code provenance.** Store the ECEC commit, corrected-file version, crosswalk diagnostics, and run configuration with every output used downstream.

The fixed-choice tax comparison should determine whether the issue is quantitatively material before the team interprets a full equilibrium rerun. Until that comparison is complete, ECEC-derived fiscal and behavioral results remain exposed to a confirmed input and tax-matching error of unknown aggregate magnitude.

## Adjacent documentation issues

Two implementation differences are separate from the identifier problem but should be recorded for later review. The methods say that ECEC linearly interpolates donor effective marginal tax rates across wage levels, while the release code selects the nearest wage threshold. The methods also describe a match on marital status, while the code matches on a joint-filing indicator constructed from the tax record. Neither difference causes the married-spouse split.

The other ECEC materials in `references/` do not define this microsimulation pipeline. `macro_consequences.pdf` describes a distinct overlapping-generations model, and `ECEC Outcomes Lit Review.docx` summarizes evidence on childcare outcomes and parental labor supply.

## Reproducible evidence

All corrected-file analysis is stored on Bouchet under the user's project space:

```text
/home/ag3377/scratch_pi_nrs36/ag3377/taxid_audit_20260817/
```

Primary summaries are:

```text
output/audit_summary.txt
output_followup/followup_summary.txt
output_stored/stored_parent_summary.txt
output_child_u5/child_u5_summary.txt
```

Supporting tables include the corrected spouse cross-tab, corrected-split married pairs, AGI edge cases, parent units joined to corrected IDs, candidate partition scores, and child-to-parent matching results. The analysis did not modify shared ECEC code, raw data, or stored model results.
