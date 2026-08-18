# Corrected tax units: full-sample paired comparison

August 18, 2026

Results of the repair and validation sequence proposed in
`ecec_team_taxid_memo_2026-08-17.md`. Three full-sample `report_runs` arms
(identical seeds, sample, Monte Carlo draws, and NSECE inputs) isolate the
two fixes, and a fourth run rescores report8 under all fixes.

| Arm | Code | Commit | Interface |
|---|---|---|---|
| Legacy | `main` | `dbfae80` | `202608172056` |
| ID fix | `taxid_082026` | `04e9951` | `202608172154` |
| ID + EMTR fix | `taxid_082026_emtr` | `a612eb0` | `202608172156` |
| Report8, all fixes | `taxid_082026_emtr` | `a612eb0` | `202608172158` |

All arms: `report_runs`, 2026-2055, full sample, `-n 1 -f`, seed 0
(report8 arm uses the `report8` runscript, `-n 1 -f`, 48G). The legacy arm
reproduces the stored March 2026 production run (`202603111322`) to within
0.005% on every scenario's FY2026-35 budget effect, so differences across
arms are attributable to the fixes alone and comparisons against published
March numbers are direct.

## The fixes

1. **Corrected tax-unit identifiers** (`cc69994`, `92a6f0c`): ACS processing
   joins the Census Bureau's April 2024 corrected `Tax_unit` (crosswalk under
   `raw_data/Census-ACS-SPM/v1/2019/`), reconstructs one AGI per corrected
   unit from deduplicated legacy fragments with survivor handling for split
   legacy units (`src/shared_functions/reconstruct_tax_unit_agi.R`), and
   retains legacy IDs for validation. Construction was validated
   record-exactly against the macro team's full-file audit: 1,689,811
   corrected units, 100% crosswalk coverage, all 363 first!=max edge cases
   resolve to the survivor AGI. One documented judgment call: the audit's 7
   positive-positive married pairs are themselves split-legacy cases, and the
   rule prefers no-double-counting (survivor AGI) over summing (~$1.1M AGI
   nationally).

2. **Combined-wage EMTR lookup** (`1b02367`, John Ricco): for couples sharing
   a tax unit, the donor's marginal-rate schedule is evaluated at the unit's
   combined counterfactual wages instead of parent 1's earnings. This
   corresponds to option (b) in `caregiver_emtr_lookup_question_2026-08-17.md`;
   the schedule is indexed by unit-total wages, so the lookup level now moves
   with the caregiver's employment alternatives.

3. **Calibration optimizer polish** (`04e9951`): required enabler discovered
   during the reruns. A single Nelder-Mead run reports convergence with its
   simplex collapsed along the curved beta-rho valley; on corrected-unit data
   this left a 1.23% income-target miss that tripped the 1% post-fit
   assertion (legacy data stalls too, but inside tolerance at 0.22%).
   Restarting from the terminus until no improvement reaches the optimum to
   machine precision. No targets, tolerances, or assertions changed.

## What the identifier fix does to the data

| | Legacy | Corrected |
|---|---:|---:|
| Distinct tax units (2019) | 2,347,451 | 1,689,811 |
| Share filing MFJ | 0.567 | 0.398 |
| Share with AGI == 0 | 0.363 | 0.115 |
| Total weighted AGI ($B) | 11,014 | 10,976 (-0.34%) |
| Married spouse pairs sharing a unit | 0.13% | 99.37% |

## Calibration

All arms hit both targets (cost -0.15, income -0.05); corrected arms hit
them to machine precision under the polish.

| Arm | beta | rho | wage elasticity (achieved) |
|---|---:|---:|---:|
| Legacy | 0.0003421 | 0.02842 | 1.664 |
| ID fix | 0.0003312 | 0.02563 | 1.588 |
| ID + EMTR | 0.0003337 | 0.02624 | 1.535 |

Caveat: the legacy arm ran without the polish commit (its 0.22% residual
miss passes the assertion), so a sliver of the legacy-vs-corrected parameter
difference is optimizer residual rather than data. It is an order of
magnitude smaller than the data effect.

## Fixed-choice taxes (baseline, 2026, record-level; join coverage 1.0000)

Weighted means; "wedge" = total tax at full-time minus at not-working for
the primary caregiver's alternatives.

| | Legacy | ID fix | ID + EMTR |
|---|---:|---:|---:|
| c1 tax at none | $7,700 | $5,565 | $4,632 |
| c1 tax at ft | $15,448 | $15,836 | $16,461 |
| c1 ft-vs-none wedge | $7,747 | $10,271 (+32.6%) | $11,829 (+15.2% more) |
| c2plus wedge | $8,753 | $11,568 (+32.2%) | $13,574 (+17.3% more) |

Combined, the work-incentive tax wedge for parents rises ~53%: caregiver
earnings now stack on joint income (ID fix) and the marginal-rate lookup
tracks the couple's combined wage position (EMTR fix). 39-46% of parent
units see their wedge change by more than $100. Baseline equilibrium prices
move by ~0.1% and the aggregate parental employment rate by -0.04pp
(employment targeting holds group rates by construction; composition
shifts).

## Policy scores, report_runs (total budget effect, $B)

| Scenario | Legacy FY26-35 | ID fix | ID+EMTR | Legacy 30-yr | ID+EMTR 30-yr |
|---|---:|---:|---:|---:|---:|
| universal | -769.51 | -758.61 | -750.67 (+2.4%) | -3,175.80 | -3,117.03 (+1.9%) |
| means_tested | -728.12 | -715.61 | -707.61 (+2.8%) | -2,998.71 | -2,934.78 (+2.1%) |
| arpa_cdctc | -103.15 | -96.00 | -93.19 (+9.7%) | -367.76 | -329.27 (+10.5%) |
| child_ubi_1k | -194.00 | -194.03 | -194.03 (0.0%) | -714.64 | -714.49 (0.0%) |
| wage_subsidy | -89.75 | -89.96 | -89.82 (0.0%) | -383.22 | -382.95 (0.0%) |

Interpretation: care-subsidy policies get modestly cheaper (higher caregiver
marginal rates mean more tax clawback of induced earnings and correct joint
income in means tests); the CDCTC policy moves most in relative terms
because nearly all married couples were previously classified as cohabiting
in the credit calculation; employment-invariant cash (child UBI) and the
supply-side wage subsidy are essentially unaffected.

## Report8 rescored under all fixes (vs production 202608132032)

| Scenario | Prod FY26-35 | Fixed | Prod 30-yr | Fixed |
|---|---:|---:|---:|---:|
| a1_universal | -777.88 | -758.66 (+2.5%) | -3,215.62 | -3,155.36 (+1.9%) |
| a2_smooth | -776.12 | -754.35 (+2.8%) | -3,228.92 | -3,160.27 (+2.1%) |
| b1_smooth_plus_10k | -2,071.91 | -2,066.04 (+0.3%) | -9,068.77 | -9,050.63 (+0.2%) |
| b2_smooth_plus_5k | -1,297.63 | -1,285.13 (+1.0%) | -5,500.03 | -5,462.81 (+0.7%) |
| b3_matched | -777.64 | -764.84 (+1.6%) | -3,181.87 | -3,139.34 (+1.3%) |
| c1_child_ubi_10k | -2,065.41 | -2,065.05 (0.0%) | -8,934.76 | -8,933.91 (0.0%) |
| c2_child_ubi_age_dep | -2,038.49 | -2,038.30 (0.0%) | -8,804.04 | -8,803.40 (0.0%) |
| c3_ubi_matched | -777.87 | -777.76 (0.0%) | -3,365.26 | -3,365.03 (0.0%) |

Note for the cost-matched arms: A2's phase-out rate, B3's transfer, and
C3's UBI amount were tuned to match A1's FY26-35 score under legacy tax
units. Under the fixes the matched arms drift apart (A1 -758.66 vs A2
-754.35, B3 -764.84, C3 -777.76); if the fixes are adopted for report8,
the matching parameters need re-tuning.

## Infrastructure notes

- `slurm/phase_{setup,year,finalize}.sh` hard-code `cd` to the primary repo
  path, so launches from git worktrees silently run the primary repo's code.
  This invalidated the first overnight attempt. Worktrees used for these
  runs carry local (uncommitted) patches; a proper fix (pass the project dir
  from `launch_array.sh`) is agreed as a separate later change.
- `-a N` requires `--sim-sample N` alongside it (contrary to the CLAUDE.md
  smoke-test recipe).
- Comparison and diagnostic scripts:
  `/nfs/roberts/project/pi_nrs36/hre2/taxid_diag/` (three-way comparison,
  extracted-estimation calibration diagnostics, NM-polish test).

## Deferred / open items

- Report8 cost-matching re-tune (above), if fixes adopted for report8.
- `phase_*.sh` working-directory fix (separate PR).
- The 4,194 married pairs the Census correction leaves split are preserved
  as-is per the memo; the residual 134 under-5 children matching neither
  parent's tax unit are a separate child-claiming issue.
- Legacy arm ran without the optimizer polish (0.22% residual target miss);
  a purist rerun with the polish would move legacy beta/rho in the fourth
  digit.
