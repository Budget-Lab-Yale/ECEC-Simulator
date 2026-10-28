# NSECE 2024 checks

Standalone checks behind the 2019 → 2024 NSECE migration, written up in
[`docs/nsece_2019_to_2024_crosswalk.md`](../../../docs/nsece_2019_to_2024_crosswalk.md).
They read the raw public-use files directly and don't touch the simulator's pipeline or outputs.

| Script | Question it answers |
|---|---|
| `01_variable_crosswalk.py` | Does every raw variable `1a_nsece_processing.R` reads exist in 2024, with which loop indices and which reserve codes? |
| `02_care_types.py` | How does the household care-type classification (the 15 options) come out on each year, and how sensitive is it to the low/high center price cutoff? |
| `03_dk_costs_and_calendar_gaps.py` | Do "don't know" center costs look like free care or missing amounts? How many parent hours records does the −10 calendar-gap code blank? |
| `04_admin_centers.py` | What are the administrative-data center cases, and how do they enter the sector labor ratios in each year? |
| `05_price_cutoff.py` | What recipe reproduces the $4.75 low/high center cutoff on 2019, and what does it give on 2024 ($6.86, now set per year in `1a`)? |
| `06_supply_checks.py` | Do negative posted-rate codes reach a price? How do the admin-center treatments, wages, and home-based weights change the sector labor ratios and `L_0`? |
| `07_admin_family_pay.py` | How many administrative-data centers fall in each family-pay group, and how much enrollment do they hold? |
| `08_admin_staffing.py` | Do administrative-data centers carry director staff counts, and how many staff per child do they report against interviewed centers? |
| `09_definition_checks.py` | Do the 2019 and 2024 free-center rules agree? How are posted rates censored, and how many rates above $50 per hour remain? Do imputed and reported admin staff counts differ? How much enrollment do admin centers outside every sector hold? |
| `10_fix_evidence.py` | Do don't-know payment centers look paying or free? Are rates above $2,000 per week unit errors? Which price sector do paying public-school programs fall in? How do parents with missing work hours compare with donors? How much priced enrollment do deleted, capped, and missing rates remove? How do NORC's imputed admin staff counts compare with the reported ones? |
| `11_acs_mother_at_work.R` | What share of ACS children under 5 have a mother (or one parent) at work last week in 2019, 2023, and 2024? This is the benchmark for the NSECE's 64.3% in 2024. R, not Python: run it through `sbatch` with about 96 GB. |

## Running

On Bouchet, from the repo root. Python 3.9+ and the standard library are all they need (the system
`python3` on the login nodes works):

```bash
mkdir -p nsece_check_output
for s in src/misc/nsece_2024_checks/[01]*.py; do
  python3 -I "$s" --out "nsece_check_output/$(basename "$s" .py).txt"
done
```

Every script takes `--raw-root` (default `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1`,
with `2019/` and `2024/` underneath) and `--out` (default: print to the terminal). `02_care_types.py`
also takes `--cutoff`, the low/high center price cutoff in $/hour (default 4.75, the 2019 value in
`1a`).

## Caveats

- `02`–`05` re-implement parts of `1a` in Python to test changes before the R rewrite. They follow
  `1a`'s rules as they were before the 2024 rewrite on `feat/nsece-2024`, so expect small
  differences from the current R output.
- Counts are unweighted unless a line says "weighted"; weighted figures use the survey's household,
  child, center or worker weight as appropriate.
