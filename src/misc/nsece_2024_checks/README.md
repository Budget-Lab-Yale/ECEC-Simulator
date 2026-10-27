# NSECE 2024 checks

Standalone checks behind the 2019 → 2024 NSECE migration, written up in
[`docs/nsece_2019_to_2024_crosswalk.md`](../../../docs/nsece_2019_to_2024_crosswalk.md).
They read the raw public-use files directly and don't touch the simulator's pipeline or outputs.

| Script | Question it answers |
|---|---|
| `01_variable_crosswalk.py` | Does every raw variable `1a_nsece_processing.R` reads exist in 2024, with which loop indices and which reserve codes? |
| `02_care_types.py` | How does the household care-type classification (the 15 options) come out on each year, and how sensitive is it to the low/high center price cutoff? |
| `03_dk_costs_and_calendar_gaps.py` | Do "don't know" center costs look like free care or missing amounts? How many children does the −10 calendar-gap code drop? |
| `04_admin_centers.py` | What are the administrative-data center cases, and how do they enter the sector labor ratios in each year? |
| `05_price_cutoff.py` | What recipe reproduces 1a's $4.75 low/high center cutoff on 2019, and what does it give on 2024? |
| `06_supply_checks.py` | Do negative posted-rate codes reach a price? How do the admin-center treatments, wages, and home-based weights change the sector labor ratios and `L_0`? |
| `07_admin_family_pay.py` | How many administrative-data centers fall in each family-pay group, and how much enrollment do they hold? |

## Running

On Bouchet, from the repo root. Python 3.9+ and the standard library are all they need (the system
`python3` on the login nodes works):

```bash
mkdir -p nsece_check_output
for s in src/misc/nsece_2024_checks/0*.py; do
  python3 -I "$s" --out "nsece_check_output/$(basename "$s" .py).txt"
done
```

Every script takes `--raw-root` (default `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1`,
with `2019/` and `2024/` underneath) and `--out` (default: print to the terminal). `02_care_types.py`
also takes `--cutoff`, the low/high center price cutoff in $/hour (default 4.75, the value `1a`
hard-codes).

## Caveats

- `02`–`05` re-implement parts of `1a` in Python to test changes before the R rewrite. They follow
  `1a`'s rules as written on the `2024-nsece` branch, so expect small differences from the R output;
  rerun the comparisons from the R pipeline once `1a` is rewritten.
- Counts are unweighted unless a line says "weighted"; weighted figures use the survey's household,
  child, center or worker weight as appropriate.
