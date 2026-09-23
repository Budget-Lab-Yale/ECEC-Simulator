# Sources

Every input the build reads, where it comes from, and what it feeds. Nothing
downloads at build time: each file is fetched by hand and held in `raw/`, so a
build is reproducible offline. `SOURCES.csv` is the machine-readable register —
one row per source and geography, with its URL, vintage and retrieval date.

## File naming

```
raw/                  US_<SOURCE_KEY>__<content>__<vintage>.<ext>
raw/documentation/    reports and codebooks behind a hand-extracted number
```

- `content` is lowercase snake_case.
- **`vintage` is the year the data describes, never the download date** — ISO
  where a month matters, a school year as `2018-19`.
- Double underscores separate the three fields, so the name parses back apart.

Base year is **2019**. Where a source's nearest vintage differs, the table says
so.

## Inputs

| Input | Source | Held | Vintage | Feeds |
|---|---|---|---|---|
| Population 0–4 | Census Population Estimates, county by age | `raw/US_PEP__…` | 2019 | step 1 |
| Participation rate | NSCH topical file, question `k6q20` | `raw/US_NSCH__…` (2018, 2019, 2020) | 2019 | step 1 |
| NSECE constants | NSECE household file, ICPSR 37941 dataset 0009 | cluster only (licensed) | 2019 | steps 2 to 6 |
| Public pre-K | NCES Common Core of Data, pre-K membership by county | `raw/US_CCD__…` | 2018–19 | step 3 check |
| Head Start | NIEER, *State of Preschool 2019* | constant | 2018–19 | step 3 check |
| Early Head Start | NIEER, *The State(s) of Head Start and Early Head Start* (2023) | constant | 2018–19 | step 3 check |
| NYC Head Start share | ACF Head Start service locations, funded slots by site | `raw/US_HSSL__…` | 2026 | step 3 check, NYC |
| Head Start / pre-K overlap | NIEER, *State of Preschool 2019*, Table 4 and state pages | constant | 2018–19 | step 3 check |
| California public pre-K | NIEER, *State of Preschool 2019*, California pages | constant | 2018–19 | step 3 check, CA |
| SIPP care-type counts | SIPP annual public-use files, pooled | `raw/US_SIPP__…` | 2018–2023 | replacement register |

A **file** is read by the code. A **constant** is read by hand once and written
into `states/<GEO>.R` next to its provenance comment.

## NSCH — the participation rate

`k6q20` asks whether the child receives at least 10 hours a week of care from
someone other than a parent or guardian. The prompt names every arrangement
type and never asks which one applies. Wording is in
`raw/documentation/US_NSCH__topical_variable_list__2019.pdf`.

`01_nsch_rate.R` derives each state's rate and standard error and writes
`out/nsch_rate.csv`; `--check` compares the constants in `states/<GEO>.R` back
to it. The error uses the **survey design** — strata `FIPSST × STRATUM`, PSU
`HHID`, weight `FWC`, each state read as a domain of the national design.

The 2019 file is the Census re-release (`nsch_2019e_topical`, 2024), all 51
states. An earlier copy held only six states; its 3,545 records match the
re-release on every column the build reads, so no rate or error moved.

The build uses the 2019 wave alone. The 2018 and 2020 files are held so that
the pooled comparison in `01_nsch_rate.R` can run: NSCH 2020 was fielded
through the pandemic, and its care rates fall 5 to 18 points in KY, NJ, and VA.

State cells run 101–147 records. NSCH has no sub-state identifier, so NYC uses
the New York State rate and standard error.

## NSECE — the regional constants

The formal share, the unpaid-centre share, the centre-home split, the informal
mix, and the weekly hours all come from the NSECE 2019 household file, child-weighted, by Census region.
NSECE carries no state, so region is the finest geography it offers.

`02_nsece_constants.R` derives them over two universes:

- **Universe A** — every child 0–4 with a regular non-parental arrangement,
  NSECE's own five-hour screener. It reproduces NSECE's published figures
  exactly, which validates the code. **This is the universe the build uses**,
  because the simulator sets no hours threshold of its own.
- **Universe B** — the same children whose regular non-parental care totals at
  least 10 hours a week, school excluded. This matches the NSCH threshold.
  `NSECE_UNIVERSE_RATIO` carries the NSCH rate from B up to A; B is otherwise
  used only for sensitivity.

Each constant carries its unweighted record count. A regional cell under 30
records takes the national value; only Unpaid Home-Based falls below it (8
records in the Northeast, 4 in the South). Full precision is in
`out/nsece_universe.csv`.

**NSECE is not in this repository.** ICPSR 37941 is licensed and lives on the
cluster at `/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1`. Run the
script there with `sbatch`.

## SIPP — evaluated, and rejected as an NSECE replacement

NSECE carries no state. SIPP does, for all 51, and it asks about child care, so
it is the obvious candidate to replace the regional constants with state ones.
It cannot, and the reason is worth keeping because it will come up again.

Tested against the 2018–2023 annual public-use files on the cluster
(`shared/raw_data/SIPP/<year>/pu<year>.csv`), pooled. Three findings, in order
of how badly each one binds:

1. **No hours variable.** SIPP's child care battery is fourteen yes/no flags,
   one per arrangement — `EDAYCARE`, `ENUR`, `EFAM`, `ENREL`, `EGRAN` and the
   rest, with child-level recodes `RDAYCARE` and so on. Nothing asks how many
   hours. The only quantity in the whole battery is `TPAYWK`. Step 6 needs
   hours, so this alone rules SIPP out for the hours constants.
2. **Price does not attach to a child.** `TPAYWK` sits on the reference
   parent's record and covers all that parent's children together. Of 6,307
   children 0–4 with a non-parental arrangement, 2,891 are in a household
   reporting an amount — and none of those amounts can be split between two
   children or between two arrangements.
3. **Cells are too thin.** Pooling gives 8,319 **distinct** children 0–4 —
   distinct, not person-years: 2,911 appear in more than one panel year, so
   naive pooling double-counts them. Split by state and care type, 50 of the
   108 state × type cells for the 18 whole states reach 30 records, the bar the
   build already applies to a thin NSECE regional cell. No state clears all
   six types; Vermont clears none.

SIPP also never asks whether home-based care is paid, so it has no question
corresponding to Unpaid Home-Based, and its `EPAY` is one household answer
rather than a per-arrangement one.

The counts are held as `raw/US_SIPP__…` and the register that reads them is
`05_replacement_register.R`.

## CCD — public pre-K

Pre-K membership, school by school, summed by county, for 2018–19. It counts a
**grade**, not an age, so it includes some five-year-olds, and it counts more
than state pre-K programmes alone — in Kentucky 28,465 against the 21,351 NIEER
reports for the Kentucky Preschool Program, because the grade also holds
district-funded places and preschool special education.

## NIEER — Head Start and Early Head Start

Both figures are **funded enrollment slots**, which NIEER takes from ACF. They
are not counts of unique children, and they are not observed by age. NIEER's
methodology, held at `raw/documentation/US_NIEER__head_start_methodology__2023.pdf`:

> Funded enrollment was not provided by single year of age. Therefore,
> federally-funded enrollment by age was estimated based on the proportion of
> the cumulative enrollment that was either under 1-, 1-, 2-, 3-, 4- or
> 5-years-old. [...] Because HS is intended to serve 3- and 4-year-olds, we
> counted 2-year-olds in HS as 3-year-olds; and we counted 5-year-olds as
> 4-year-olds. Similarly, since EHS is intended to serve infants and toddlers,
> we considered 3-year-olds enrolled in EHS to be under 3 years old.

So the Head Start count includes some five-year-olds by construction, and
nothing held here can size them.

**Head Start and pre-K can count the same children.** A state can fund pre-K
places inside Head Start, and a district can run Head Start in a school.
Step 3 therefore does not use the counts; the build prints them as a check, net
of NIEER's count of that overlap. *State of Preschool 2019*, Table 4
(p.28), gives 3- and 4-year-olds in pre-K plus special education, without and
with Head Start, each child counted once; NIEER's methodology (p.193) says it
subtracts Head Start children in state pre-K where a state reports them. The
difference between the two columns is Head Start net of the overlap. Special
education cancels: Virginia reports no overlap, and its difference is exactly
its gross Head Start, 11,579.

The overlap is with state pre-K; CCD counts public schools only. The check
scales the overlap by `min(1, CCD pre-K / NIEER state pre-K)`, the state's
"Total state pre-K enrollment" on its profile page. Seven states report an
overlap: CT, IL, KY, MA, NJ, NY, WV. NIEER marks the others "some Head Start
children may also be counted in state pre-K"; they deduct nothing. A gap of two
children or fewer is rounding in the table (five states read ±1).

**California has no CCD pre-K rows** for 2018–19. Its pre-K count is NIEER's:
State Preschool Program 140,920 + transitional kindergarten 104,694 = 245,614.
It excludes preschool special education (52,307), which CCD counts elsewhere.

## Head Start service locations — the NYC share

ACF's service-location file gives funded slots by site. NYC's Head Start count
is the city's share of New York State slots in that file, 7,194 of 20,285,
applied to the 2018–19 NIEER state figure. The file is a 2026 export, so this
assumes the city's share was the same in 2019.

## Reference documents

`raw/documentation/` holds the reports behind hand-extracted numbers and the
evidence for choices the method makes:

| File | What it is |
|---|---|
| `US_NIEER__head_start_methodology__2023.pdf` | NIEER's method for Head Start funded slots by age |
| `US_NIEER__state_of_preschool_yearbook__2019.pdf` | NIEER yearbook 2019: Head Start ages 3–4, Table 4 overlap, state pre-K totals, CA pre-K |
| `US_NIEER__states_of_head_start_and_early_head_start__2023.pdf` | NIEER State(s) of Head Start: Early Head Start 2018–19 by state |
| `US_NSCH__topical_variable_list__2019.pdf` | NSCH 2019 variable list, incl. `k6q20` wording |
| `US_NSECE__home_based_provider_chartbook__2021-05.pdf` | NSECE home-based provider chartbook |
| `US_NSECE__measuring_supply_capacity_centers__2023-12.pdf` | ACF report on centre enrollment and capacity, NSECE 2019 |
| `US_PWBM__data_processing_for_tax_module__2020-01.pdf` | PWBM tax-module data processing |
| `VA_EDWP__authorized_vs_current_capacity__2024.pdf` | Virginia provider survey on authorized versus current capacity |
