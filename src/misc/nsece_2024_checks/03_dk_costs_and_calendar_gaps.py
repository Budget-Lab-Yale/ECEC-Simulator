"""
03_dk_costs_and_calendar_gaps.py -- evidence for two household-side decisions

1. "Don't know" center costs. Profiles regular center arrangements of children under 5
   by reported weekly payment (DK = -1, free = 0, paid > 0): Head Start / public pre-K
   flags, subsidy item, child age, hours, household income. Used to decide whether DK
   costs look like unpaid care or like missing amounts.
2. Calendar gaps. Counts children under 5 whose weekly-hours summaries carry -10 ("at
   least one parent has a missing block in the calendar"), which 1a drops, and profiles
   them (income, number of parents, parents' point-in-time work status D1A_WORK).

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/03_dk_costs_and_calendar_gaps.py [--raw-root PATH] [--out FILE]

Runtime: a few minutes. Python 3.9+, standard library only.
Results are summarized in docs/nsece_2019_to_2024_crosswalk.md ("Decisions").
"""
import argparse
import collections
import csv
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {
    '2019': dict(hh='2019/37941-0009-Data.tsv', p='HH9', c='HHC9', n_prov=14, n_hhm=9),
    '2024': dict(hh='2024/39466-0005-Data.tsv', p='HH4', c='HHC4', n_prov=15, n_hhm=13),
}
PROFILE = ['Head Start', 'public pre-K', 'Head Start or pre-K', 'HS/pre-K DK', 'subsidy > 0',
           'subsidy DK', 'age 0-2', 'age 3-4']


def num(x):
    try:
        return float(x.strip('"'))
    except ValueError:
        return None


def wmedian(pairs):
    pairs = sorted((x, w) for x, w in pairs if x is not None and w)
    if not pairs:
        return None
    half, acc = sum(w for _, w in pairs) / 2, 0.0
    for x, w in pairs:
        acc += w
        if acc >= half:
            return x


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out')
    args = ap.parse_args()
    csv.field_size_limit(10**9)
    out = open(args.out, 'w') if args.out else sys.stdout
    P = lambda *a: print(*a, file=out)

    for year, cfg in YEARS.items():
        p, c = cfg['p'], cfg['c']
        # calendar gaps
        tot_w = gap_w = 0.0; gap_n = 0; gap_hh = set()
        income = {'complete': [], 'gap': []}; n_parents = collections.Counter(); d1a = collections.Counter()
        # DK costs
        grp = {k: collections.Counter() for k in ('DK', 'free', 'paid')}
        grp_income = {k: [] for k in grp}; grp_hours = {k: [] for k in grp}
        with open(f"{args.raw_root}/{cfg['hh']}") as fh:
            r = csv.reader(fh, delimiter='\t')
            ix = {name.strip('"'): i for i, name in enumerate(next(r))}
            for row in r:
                g = lambda name: num(row[ix[name]]) if name in ix else None
                hh_income = g(f'{p}_ECON_INCOME_ANNUAL')
                for x in range(1, 10):
                    age_m, w = g(f'{c}_AGE_{x}'), g(f'{c}_METH_WEIGHT_{x}')
                    if age_m is None or age_m < 0 or not w or age_m >= 60:
                        continue
                    # --- calendar gaps (children with parents in the household) ---
                    wst = [g(f'{p}_WST_{v}_{x}') for v in ('PARCH_HI', 'HRS_TOT_PHI', 'PARCH_LO', 'HRS_TOT_PLO')]
                    if wst[0] not in (-8, None):
                        tot_w += w
                        has_gap = -10 in wst
                        income['gap' if has_gap else 'complete'].append((hh_income, w))
                        if has_gap:
                            gap_w += w; gap_n += 1; gap_hh.add(row[ix[f'{p}_METH_CASEID']])
                            parents = [y for y in range(1, cfg['n_hhm'] + 1) if g(f'{p}_PARCH_{x}_{y}') == 1]
                            n_parents[len(parents)] += 1
                            for y in parents:
                                d1a[g(f'{p}_D1A_WORK_{y}')] += 1
                    # --- regular center arrangements by reported payment ---
                    for y in range(1, cfg['n_prov'] + 1):
                        if g(f'{p}_CHILDREN_{x}_{y}') != 1 or g(f'{p}_TYPEOFCARE_AGG_{x}_{y}') != 4:
                            continue
                        hours = g(f'{p}_HRS_WEEK_PROV{y}_{x}')
                        if g(f'{p}_REGCARE_{x}_{y}') != 1 or not hours or hours <= 0:
                            continue
                        pay = g(f'{p}_WEEKLY_PAYMENT_TC_{x}_{y}')
                        k = 'DK' if pay == -1 else 'free' if pay == 0 else 'paid' if pay and pay > 0 else None
                        if not k:
                            continue
                        hs, pk, sub = g(f'{p}_HEADSTART_{x}_{y}'), g(f'{p}_PUBPREK_{x}_{y}'), g(f'{p}_SUBSIDY_{x}_{y}')
                        G = grp[k]; G['n'] += 1; G['w'] += w
                        G['Head Start'] += w * (hs == 1); G['public pre-K'] += w * (pk == 1)
                        G['Head Start or pre-K'] += w * (hs == 1 or pk == 1); G['HS/pre-K DK'] += w * (hs == -1 or pk == -1)
                        G['subsidy > 0'] += w * (sub is not None and sub > 0); G['subsidy DK'] += w * (sub == -1)
                        G['age 0-2'] += w * (age_m < 36); G['age 3-4'] += w * (age_m >= 36)
                        grp_income[k].append((hh_income, w)); grp_hours[k].append((hours, w))

        P(f'## {year} calendar gaps: children <5 with parents in HH, weighted {tot_w:,.0f}')
        P(f'  with -10: {gap_n} children in {len(gap_hh)} households = {100 * gap_w / tot_w:.1f}% of weight')
        P(f'  median HH income: complete ${wmedian(income["complete"]):,.0f} vs gap ${wmedian(income["gap"]):,.0f}' if gap_n else '  (none)')
        P(f'  gap children by number of parents: {dict(n_parents)}; their parents\' D1A_WORK codes: {dict(d1a)}')
        P(f'## {year} regular center arrangements, children <5, by reported weekly payment')
        for k, G in grp.items():
            w = G['w'] or 1
            P(f'  {k:4s} n={G["n"]:4d} weighted {G["w"]:11,.0f} | '
              + ' '.join(f'{m}: {100 * G[m] / w:.0f}%' for m in PROFILE)
              + f' | median hours {wmedian(grp_hours[k])} | median HH income ${wmedian(grp_income[k]):,.0f}')
    if args.out:
        out.close()


if __name__ == '__main__':
    main()
