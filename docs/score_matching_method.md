# Finding Policy Parameters to Hit an Aggregate Fiscal Score

*Reference memo — ECEC-Simulator. First applied July 2026 to calibrate the
`smooth_phaseout` demand policy to the `universal` policy's ten-year score.*

## The problem

We often want a policy parameterized by some knob θ — a transfer amount, a
copay rate, a phase-out slope — chosen so that its aggregate fiscal score
(e.g., the FY sum of `total_budget_effect` over a budget window) equals a
pre-specified target T, usually the score of a reference policy. Formally:

```
find θ such that Score(θ) = T
```

This is one-dimensional root finding. The difficulty is never the root
finder — secant iteration converges in a handful of steps for any smooth,
monotone cost curve. The difficulty is that each evaluation of `Score(θ)` is
a full model run: equilibrium solves for every year in the window, at full
sample. The method below is about making evaluations cheap without
compromising the final answer.

## Conceptual foundations

### 1. The target is a fixed number

The reference policy is scored once and never re-run. All work goes into
evaluating the *candidate* policy. Corollary: the target must be measured
under the **same conditions** the candidate runs will use — same estimation
data, sample, draws, seed, and (critically) the same upstream data vintages.
A target computed months earlier is stale the moment a dependency pin
changes (see "Gotchas").

### 2. Anatomy of a score: mechanical vs behavioral

The model decomposes every score into a *mechanical* component (policy rules
applied to baseline behavior) and a *behavioral* component (everything that
changes because choices and prices respond). Write the ratio
`r(θ) = Total(θ) / Mechanical(θ) = 1 + b(θ)`.

Two facts make this decomposition useful:

- For **scale-knob policies**, Mechanical(θ) is exactly (or nearly) linear
  in θ. If b is small (tax-type policies: child UBI has b ≈ 0.003), the
  whole score is effectively linear and one secant through two existing
  points solves the problem outright.
- For **behavioral-heavy policies** (demand subsidies: b ≈ 5–6), r moves
  slowly in θ. Formally, an iteration that solves the problem on a cheap
  surrogate and corrects with measured r has per-step error multiplier
  `ε = (b/(1+b)) · η_b`, where `η_b` is the elasticity of b with respect
  to θ. Near θ = 0, η_b ≈ 1 but b ≈ 0; at scale, take-up saturates so
  η_b → 0 while b/(1+b) < 1. Either way ε stays well below 1 and each
  correction step kills most of the remaining error. Empirically,
  structurally different demand policies (universal vs means-tested vs
  smooth-phaseout) have r within a few percent of each other, so r borrowed
  from *any* nearby run pins the first guess well.

### 3. The cheap-evaluation hierarchy

From cheapest to most expensive:

1. **Existing runs** (free). If prior runs contain the candidate family at
   one or two parameter values, interpolate. Exact for near-linear policies.
2. **Small-sample full runs** (~minutes). Run the search at a reduced ACS
   sample (`-a 1 --sim-sample 1`) against a small-sample target *measured
   at the same sample*. Sampling error largely cancels in the ratio: the θ
   that matches the reference at 1% transfers to full sample with small
   error (0.56% in the smooth-phaseout case).
3. **Baseline reuse** (`--baseline-interface` / `-b`). The baseline is
   policy-invariant, so a completed run's converged baseline (equilibrium
   prices, employment shifts, collapsed choices) is loaded from disk instead
   of re-solved. This roughly halves every run — and the baseline is the
   expensive half, since it nests the employment-targeting root-find inside
   every optimizer step. Search iterations at 1% take ~4–5 minutes each.
4. **Full-sample runs** (~25–55 min per year as a SLURM array job). Needed
   twice: once to measure the target and once to verify (occasionally a
   third time for a final secant step).

### 4. Exactness requirements

Score differences are paired comparisons at the epsilon-draw level, so the
candidate runs must reproduce the reference conditions exactly:

- same `--calib-sample` / `--sim-sample`, `--seed`, `-n` (draws per record);
- same estimation data (`-C` pointing at the same run);
- same `phase_in_years` (the behavioral component is scaled by phase-in);
- same fiscal-year convention (scores are 0.25/0.75 calendar-year blends);
- same upstream data vintages (`config/default_paths.yaml` pins).

`-b` runs enforce most of this automatically: startup validation checks
metadata, and per-year integrity assertions compare the loaded baseline
against freshly prepared data (key sets, row counts, weighted values), so a
vintage mismatch fails loudly rather than silently contaminating the score.

## The algorithm

Using the smooth-phaseout calibration as the running example. Reference
policy: `universal`. Candidate: copay = X × (income − 0.75 × median),
matched on the FY2026–2035 sum of `total_budget_effect`.

**Step 0 — Set up runscripts.** Two runscripts spanning only the scoring
window (here `2026:2035`):

- `<name>_ref.csv`: baseline + reference policy + candidate (initial guess),
  used once to measure the target and generate a reusable baseline;
- `<name>_iter.csv`: baseline + candidate only, used for every iteration.

Match `phase_in_years` to the reference scenario.

**Step 1 — Reference run at search scale.** One fresh run of the ref
runscript at 1% sample:

```
Rscript src/main.R -r <name>_ref -a 1 --sim-sample 1 -n 1
```

This yields (a) the 1%-scale target T₁ for the reference policy, (b) the
first candidate point, and (c) a stored baseline for reuse. ~15 min.
Example: universal = −863.7, smooth(X=0.10) = −935.6 → X too generous.

**Step 2 — Secant iterations at search scale.** Repeat until within ~0.2%:
edit θ in the policy file, then

```
Rscript src/main.R -r <name>_iter -a 1 --sim-sample 1 -n 1 -C <ref_ts> -b <ref_ts>
```

(~4–5 min each). Update θ by the secant rule
`θ_{k+1} = θ_k + (T₁ − S_k) · (θ_k − θ_{k−1}) / (S_k − S_{k−1})`.
Example trajectory: X = 0.10 → 0.13 → 0.119 → 0.1175, landing 0.13% from
T₁ in three iterations.

**Step 3 — Full-sample reference.** One full-sample run of the ref
runscript (SLURM array over years; `launch_array.sh`), reusing estimation
data via `-C` where a compatible interface exists:

```
bash slurm/launch_array.sh -r <name>_ref -C <estimation_ts> -n 1 --mem 96G
```

This measures the full-sample target T and the candidate's full-sample
score at the 1%-calibrated θ, and — importantly — produces a full-sample
baseline with exact RDS sidecars that all future iterations can `-b`
against. Example: universal = −818.5, smooth(0.1175) = −813.9 (0.56% off).

**Step 4 — Full-sample secant step (if needed).** One iteration run with
`-b` against the step-3 baseline, stepping θ by the slope measured at
search scale (rescaled by T/T₁). Year workers skip the baseline solve, so
wall clock is roughly half of step 3. Example: X = 0.1157 → −819.5 vs
target −818.5 (0.12%). Done.

**Step 5 — Record provenance.** Write the target, run timestamps, achieved
score, and any interpolated exact crossing into the policy file header, so
the calibration is auditable and reproducible.

Total cost in the example: one 15-minute 1% run, three 5-minute iterations,
one full-sample array run, one half-cost full-sample iteration.

## Gotchas

- **Stale targets.** A dependency repin invalidates every previously
  measured score. In July 2026 a Macro-Projections pin change moved the
  universal 10-year score by ~6% (population and income-aging paths both
  moved). Never match against a target measured under different pins; the
  `-b` integrity assertions are the tripwire, but only for runs that reuse
  a baseline — standalone scores have no such guard, so check pins by hand.
- **Draw noise.** At `-n 1` the score carries Monte Carlo noise; paired
  seeds cancel most of it in deltas. Don't chase agreement much below ~0.1%
  — that's inside the noise floor and below any policy-relevant precision.
- **Nonlinearity and cliffs in θ.** Secant assumes local smoothness. If θ
  itself moves households across eligibility cliffs, bracket with bisection
  first. (Smooth phase-out designs avoid this by construction.)
- **Ratio correction as a fallback.** If search-scale results transfer
  poorly (large sample-to-full drift), fall back to the two-level loop:
  solve `Mechanical(θ) = T / r` with r measured from the last full run,
  and iterate — convergence is governed by ε above. After two full-sample
  points exist, plain secant on the full-sample totals needs no assumptions
  at all.
- **Multi-parameter policies.** Cost matching is one equation; fix all but
  one parameter (or define a single scale factor) and search on that.

## Assets this method leaves behind

- A full-sample, current-pins baseline run with RDS sidecars: point
  `-C <ts> -b <ts>` at it for any future candidate in the same window.
- The `_ref`/`_iter` runscripts, reusable for re-calibration after any data
  or model change.
- The reference policy's score under current pins — the standing target for
  other policies in the same matching family.
