# Caregiver EMTR lookup under corrected tax units: mechanics and an open question

August 17, 2026

Prepared as part of the tax-unit identifier repair (see `ecec_team_taxid_memo_2026-08-17.md` and `extended_taxid_married_units_investigation_2026-08-17.md`). Both memos flag a "caregiver-slot" issue that the identifier fix promotes from rare to universal, and both leave the repair rule open ("the actual caregiver's earnings or another explicitly documented joint-return rule"). This note documents the current mechanics precisely and poses the one question that determines the correct rule.

## How the tax calculation works today

Each parent's tax unit is matched to a donor record from Tax-Simulator output, on AGI percentile bin, joint-filing status, and dependent count (`match_tax_donors()`, `src/3_simulation/3b_run_simulation_year.R:963`). The donor carries two things:

1. **An effective tax rate (ETR)**, used once: baseline income tax = `etr * baseline_agi`.
2. **A 31-point marginal-rate schedule** (`mtr_wages_p0` ... `mtr_wages_p100`): the donor's marginal tax rate evaluated at 31 wage levels — the dollar amounts corresponding to economy-wide wage percentiles, read from `static/supplemental/percentiles_<year>.csv` (e.g. p5 ≈ $2,433 in 2019).

When the model simulates the primary caregiver's three employment options (none/pt/ft), the income-tax consequence of each option is `EMTR * earnings_delta` (`calculate_taxes_with_donors()`, `3b:1290`). The EMTR is read off the donor's schedule at the wage-percentile threshold nearest to the unit's earnings (`lookup_emtr()`, `3b:1314`). The schedule answers: *at this wage level, what is the marginal rate on additional wages for a taxpayer like this donor?*

### Split-unit couples (~99.9% of married couples today)

Under the legacy `TAXID`, nearly all married couples are represented as two separate tax units. The system is then internally consistent, even though the donor context is wrong: if parent 2 is the caregiver, the earnings delta is routed to unit 2 (`earnings_delta2`, `src/1_processing/1b_acs_processing.R:1516-1520`) and priced at `emtr2` looked up at parent 2's own counterfactual earnings (`3b:1278`), which move with the employment choice. The problem in this regime is the *donor* — typically a zero-AGI, no-dependent unit — not the lookup.

### Shared-unit couples (0.13% today; essentially all married couples after the identifier fix)

When the two parents share a tax unit, parent 2's tax record is suppressed (`1b:1495-1499`) and the caregiver's earnings delta is routed into unit 1 regardless of which parent is the caregiver (`1b:1511-1526`):

```r
earnings_delta1 = case_when(
  is.na(agi2)            ~ earnings_delta,   # shared unit: delta always to unit 1
  primary_caregiver == 1 ~ earnings_delta,
  primary_caregiver == 2 ~ 0
)
```

That is the right unit for a joint return. But the EMTR lookup uses `earnings1` (`3b:1275`), and `earnings1` only varies with the employment choice when parent 1 *is* the caregiver (`1b:1461-1465`):

```r
earnings1 = case_when(
  is.na(baseline_earnings2) ~ earnings_choice,
  primary_caregiver == 1    ~ earnings_choice,
  primary_caregiver == 2    ~ baseline_earnings1   # fixed across choices
)
```

So when parent 2 is the caregiver — roughly 56% of affected married units by weight — the model prices parent 2's earnings change using the marginal rate at **parent 1's fixed baseline earnings**. The lookup point is both the wrong person's wage level and invariant across the none/pt/ft alternatives. Today this path covers only the rare shared-ID couples; after the corrected identifiers are incorporated, it covers essentially all married couples.

## The decision: at what wage level do we read the joint donor's schedule?

After the identifier fix, the joint unit has the right donor (matched on joint AGI, joint filing status, and the unit's dependents) and the right delta dollars (the caregiver's earnings change). The only open choice is the wage level at which the joint donor's MTR schedule is evaluated. Two candidate rules:

- **(a) The caregiver's own counterfactual earnings.** Mirrors the existing cohabiting-couple convention (each person's own earnings index their own schedule), and the lookup point correctly moves across the three employment alternatives. However, for a couple where the noncaregiver earns most of the wages, it evaluates the schedule at a wage point well below the unit's actual position in the wage distribution.

- **(b) The unit's combined wages** (noncaregiver's baseline earnings + caregiver's counterfactual earnings). Treats the schedule as "the unit's marginal rate when its total wage income is X." Also moves with the employment choice, and arguably better represents a joint return, where the rate on the caregiver's marginal dollar depends on the couple's total taxable income.

Which rule is correct is not a matter of taste — it depends on how the schedule was constructed.

## The question

**When Tax-Simulator generated the `mtr_wages_pX` columns in the detail files ECEC consumes (vintage `202512180813`, `baseline_ex_cdctc`), whose wages were set to percentile X's dollar level before recomputing liability — the tax unit's total wages, or a single earner's (and if so, which earner)?**

- If the schedule varies the **unit's total wages**, rule (b) is the correct lookup: index by combined wages.
- If it varies a **single earner's wages** (holding the spouse's wages fixed), rule (a) — the caregiver's own earnings — is the better match, possibly with the caveat that the donor's own spouse-earnings composition then matters.

We could not settle this from the code available to us. The Tax-Simulator clone we inspected (`main` at `6bde1646d`, plus the `minor_fixes` and `tw` remote branches) contains `calc_mtrs()` (`src/calc/do_taxes.R:435`) for next-dollar and extensive-margin MTRs — including composite handling for `wages1`/`wages2`/`wages` — but nothing that generates the percentile-schedule `mtr_wages_p*` columns or the `percentiles_<year>.csv` thresholds. Those presumably came from a custom runscript or a version not checked out locally. A pointer to the generating code or runscript would let us verify directly.

## Secondary notes for the same review

1. Whatever rule is chosen, it prices a potentially large inframarginal change (an earnings delta of tens of thousands of dollars) at a single MTR point. That is existing methodology, unchanged by this repair — noted only so the rule is evaluated in that context.
2. The ECEC methods documentation describes linear interpolation of EMTRs across wage levels; `lookup_emtr()` uses nearest-threshold selection instead. This difference predates the identifier issue (it is flagged in the extended investigation memo as an adjacent documentation issue) and is untouched by the repair, but if a change is ever intended, this is the same code path.
