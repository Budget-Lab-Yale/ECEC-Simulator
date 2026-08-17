# Corrected ACS tax units are needed in the ECEC Simulator

August 17, 2026

Prepared for the ECEC microsimulation team by the macro team.

## Summary

In the 2019 ACS file that the ECEC Simulator reads, married spouses almost never share a tax-unit identifier. Among married two-parent units with a child under age 5, 0.134% have a common `TAXID`; under the identifier values the Census Bureau has since corrected, that share is 99.352%. Because ECEC builds tax units directly from the legacy `TAXID`, it represents nearly every married couple as two separate tax units, each labeled married filing jointly.

The split reaches the tax calculation through the AGI attached to those units. The legacy data carry the couple's AGI on one spouse's record and zero on the other, and the primary caregiver sits on the zero-AGI record for roughly 57% of affected units by weight. ECEC therefore matches those caregivers' earnings changes to a zero-AGI, generally no-dependent tax donor instead of the couple's joint tax context. Because after-tax income enters the household choice model, the error can reach employment, childcare demand, equilibrium prices, subsidy spending, and poverty results, not only reported tax totals.

The error is confirmed, but its aggregate effect is not yet known. Refundable-credit phase-ins and phaseouts mean that the current treatment does not necessarily understate marginal rates or push results in one direction. The corrected Census file is available now and joins exactly to every person in ECEC's 2019 universe. We recommend incorporating the corrected identifiers, fixing the related caregiver-rate lookup, and comparing tax results first with household choices held fixed. If that comparison is material, a full equilibrium rerun can measure the behavioral and fiscal effects.

## The corrected file confirms the identifier problem

`TAXID` is the Census Bureau's constructed identifier for a TAXSIM filing unit in the ACS-SPM research files. The ACS does not observe actual tax filing, so the identifier is a tax-model assignment, and the [IPUMS documentation](https://usa.ipums.org/usa-action/variables/TAXID) defines it in those terms. The [Census ACS-SPM documentation](https://www2.census.gov/programs-surveys/supplemental-poverty-measure/datasets/spm/spm-asc-readme.pdf) says the 2009-2019 research files were updated to correct `Tax_unit`, and Census made the corrected files available in April 2024 through its [ACS-SPM dataset page](https://www.census.gov/data/datasets/time-series/demo/supplemental-poverty-measure/acs-research-files.html); the corrected extracts carry April 9, 2024 file timestamps, and the page's current April 16, 2025 release date reflects a later re-release of the 2022 and 2023 files only. IPUMS has not incorporated the correction, so extracts drawn from IPUMS USA, including ECEC's configured input, still carry the legacy values.

We joined the corrected 2019 file to the IPUMS input using:

```text
Census SERIALNO = IPUMS CBSERIAL - 2019000000000
Census SPORDER  = IPUMS PERNUM
```

All 3,087,291 people in ECEC's `YEAR == 2019` and `GQ %in% c(1, 2)` universe match. Age, sex, and person weight agree for every record, and neither the legacy nor corrected identifiers collide across households.

### Married-couple tax-unit agreement

| Population | Units or pairs | Legacy ID agreement | Corrected ID agreement |
|---|---:|---:|---:|
| Reciprocal married spouse pairs | 669,263 | 0.1303% unweighted; 0.1326% weighted | 99.3733% unweighted; 99.0530% weighted |
| Stored married two-parent units linked to a child age 12 or younger | 169,945 | 0.1200% unweighted; 0.1166% weighted | 99.4033% unweighted; 99.1403% weighted |
| Married two-parent units with at least one child under age 5 | 82,740 | 0.1342% unweighted; 0.1383% weighted | 99.3522% unweighted; 99.0686% weighted |

Weighted spouse-pair statistics use the average of the spouses' person weights. Parent-unit statistics use parent 1's person weight, consistent with ECEC's unit weighting.

The correction changes the identifier for 664,356 people and reduces the number of distinct tax units in the ECEC universe from 2,347,451 to 1,689,811. Among reciprocal married pairs, 664,198 move from different legacy IDs to one corrected ID.

The corrected file does not assign every married pair to one tax unit. It leaves 4,194 reciprocal married pairs split, and these pairs span several AGI and household-relationship patterns. Because the identifier is a tax-model assignment rather than observed filing, some of those splits are presumably intended. A local repair that simply merged every married spouse pair would erase them, so the production repair should use the official corrected identifiers.

## ECEC turns the legacy split into separate tax records

The ECEC methods documentation describes married joint filers as one tax unit with one joint adjusted gross income (AGI). Unmarried cohabitants retain separate AGIs and liabilities. The code relies on raw `TAXID` equality to implement that distinction:

- The ACS processing script renames `TAXID` and `ADJGINC` directly to `tax_unit_id` and `agi` ([`src/1_processing/1b_acs_processing.R:492-511`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/1_processing/1b_acs_processing.R#L492-L511)).
- Tax records are grouped on `(hh_id, tax_unit_id)`, and each group containing a married person receives married-filing-jointly status ([`1b_acs_processing.R:697-710`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/1_processing/1b_acs_processing.R#L697-L710)).
- The second parent's record is suppressed only when the two raw IDs match ([`1b_acs_processing.R:1479-1531`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/1_processing/1b_acs_processing.R#L1479-L1531)).
- Each remaining tax record is matched separately to a donor on AGI, filing status, and dependents ([`src/3_simulation/3b_run_simulation_year.R:940-1005`](https://github.com/Budget-Lab-Yale/ECEC-Simulator/blob/be79743c251ea3ee879af8d996d6c1b99a09b4c4/src/3_simulation/3b_run_simulation_year.R#L940-L1005)).

Among the married under-5 parent units whose legacy IDs differ (82,629 of the 82,740), 99.15% have exactly one zero-AGI record. The primary caregiver is on that record in 56.66% of units unweighted and 57.24% weighted. ECEC therefore matches many caregiver earnings changes to a zero-AGI, generally no-dependent donor rather than to the couple's joint AGI and dependent context.

The mismatch directly changes the tax calculation used in employment alternatives. Because after-tax income enters calibration and the household choice model, the mismatch can affect employment, childcare demand, equilibrium prices, subsidy spending, tax revenue, poverty, and distributional results. Combined parent-pair AGI is approximately preserved in aggregate (the weighted sum of the two records' AGIs exceeds the sum of couple-level maxima by 0.12% across split married pairs), so the most direct concern is tax-donor and marginal-rate assignment rather than broad income double-counting.

## The corrected implementation must also address AGI and caregiver routing

The Census correction is effectively an identifier correction, not an AGI replacement. Corrected person-level `AGI` equals IPUMS `ADJGINC` exactly for 99.28% of people and differs by no more than $0.50 for every person. The corrected file does not repeat one AGI on every tax-unit member; only 61.97% of corrected units have identical AGI values across all member records.

For the married pairs that the correction merges, 642,426 have one zero AGI, 21,765 have both AGIs at zero, and seven have two unequal positive AGIs. In addition, 6,578 legacy IDs split across multiple corrected IDs. Within a legacy unit, members carry that unit's AGI value, so a person-level sum double counts whenever a corrected unit absorbs a multi-person fragment, and `max(AGI)` picks the wrong value in the 363 corrected units whose proper AGI is not the largest member value. A production repair should therefore retain both identifiers long enough to deduplicate AGI within each legacy fragment, aggregate the fragments assigned to each corrected unit, and validate the edge cases.

The corrected identifiers also promote an existing caregiver-slot issue from rare to universal. When parents share an ID, ECEC suppresses parent 2's AGI and routes either caregiver's earnings change to tax unit 1, yet the marginal-rate lookup for that unit uses parent 1's earnings (`lookup_emtr(donor_id1, earnings1)`). When parent 2 is the caregiver, the model has the joint donor but selects the rate schedule using the noncaregiver's earnings. Today this path covers only the 0.13% of married couples that share a legacy ID; after the identifier repair it covers all of them, and in the stored data the caregiver is the parent-2 spouse in roughly 56% of affected married units. The repair should use the actual caregiver's earnings or another documented joint-return rule.

## CDCTC exposure appears concentrated rather than broad

The Child and Dependent Care Tax Credit (CDCTC) code defines couples with different tax IDs as cohabiting, so the legacy identifier misclassifies nearly all married couples. The mechanical effect is narrower than that classification rate, because the credit is computed within each tax unit from the children assigned to it, and almost every young child in the affected population is assigned to the parent 1 ID that also carries the joint AGI. The second spouse's unit typically has no children and no expenses, so it generates no credit.

Only six model-relevant split units have young children divided across both parent IDs, the main route to calculating two credits within one parent unit. Among linked under-5 children in adult two-parent units where both parents have ECEC's married flag, 150 match neither legacy parent ID; the correction reduces that count to 134. The corrected run should recalculate CDCTC from the tax-unit level, but the current evidence does not support a presumption that the legacy split broadly raises or lowers the credit.

## Proposed implementation and validation sequence

1. **Crosswalk the official identifier.** Join Census `Tax_unit` on `CBSERIAL` and `PERNUM`, preserve household scope, and retain both legacy and corrected IDs during validation.
2. **Define one AGI per corrected unit.** Deduplicate legacy tax-unit fragments, resolve legacy units that split under the corrected partition, and validate the rare positive-positive married pairs.
3. **Construct each corrected unit once.** Assign one filing status, AGI, dependent count, and donor match to each corrected tax unit.
4. **Fix caregiver-rate selection.** Route earnings changes to the joint unit while selecting the marginal-rate schedule using the caregiver's earnings under a documented rule.
5. **Run a fixed-choice comparison.** Holding household choices constant, compare donor matches, effective and marginal tax rates, liabilities, earnings-induced tax changes, and CDCTC amounts with the stored run.
6. **Run a targeted equilibrium comparison if warranted.** If the fixed-choice difference is material, rerun one year and one policy with identical seeds to measure changes in employment, childcare demand, prices, subsidies, tax revenue, and poverty before undertaking a complete production rerun.

The fixed-choice comparison is the fastest way to determine whether the confirmed construction error materially affects ECEC's published and downstream uses. The macro team can share the crosswalk and verification scripts and help with either validation stage.

## Reproducible evidence

The corrected-file analysis and supporting tables are stored on Bouchet at:

```text
/home/ag3377/scratch_pi_nrs36/ag3377/taxid_audit_20260817/
```

The stored March 2026 ECEC run does not record a repository commit, but its parent IDs match the audited legacy input exactly. The relevant ACS-processing and CDCTC files are unchanged between the April 2026 initial public release and public `main` as of August 17, 2026. No shared ECEC code, raw data, or stored results were modified for this analysis.
