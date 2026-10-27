"""
07_admin_family_pay.py -- weighted enrollment of administrative-data centers by family-pay item

Admin centers (CB*_METH_DATA_SOURCE = 2) with FAMPAY = 2 (no family pays) land in the Unpaid
Center sector. Those with FAMPAY = 1 (families pay) or -1 (don't know) get no price in 1a, so
they fall in no sector. This script sizes each group: number of centers and weighted
enrollment under age 6 (2019: sum of ENRL_INF..5YR; 2024: ENRL_0TO5), headcount not FTE.

Usage (on Bouchet):
    python3 -I src/misc/nsece_2024_checks/07_admin_family_pay.py [--raw-root PATH]

Runtime: seconds. Python 3.9+, standard library only.
Results are summarized in docs/nsece_2019_to_2024_crosswalk.md ("Administrative-data centers").
"""
import argparse
import collections
import csv

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {'2019': ('2019/37941-0006-Data.tsv', 'CB9'), '2024': ('2024/39466-0002-Data.tsv', 'CB4')}
AGES = ['INF', '1YR', '2YR', '3YR', '4YR', '5YR']


def num(x):
    x = x.strip('"')
    try:
        return float(x)
    except ValueError:
        return None


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    args = ap.parse_args()
    csv.field_size_limit(10**9)
    for year, (path, cp) in YEARS.items():
        with open(f'{args.raw_root}/{path}') as fh:
            r = csv.reader(fh, delimiter='\t')
            header = [h.strip('"') for h in next(r)]
            ix = {n: i for i, n in enumerate(header)}
            g = lambda row, k: num(row[ix[k]]) if k in ix else None
            enrl, n = collections.Counter(), collections.Counter()
            for row in r:
                if g(row, f'{cp}_METH_DATA_SOURCE') != 2:
                    continue
                fp, w = g(row, f'{cp}_B1_3_FAMPAY'), g(row, f'{cp}_METH_WEIGHT') or 0
                if year == '2024':
                    e = max(g(row, f'{cp}_ENRL_0TO5') or 0, 0)
                else:
                    e = sum(max(g(row, f'{cp}_ENRL_{a}') or 0, 0) for a in AGES)
                enrl[fp] += w * e
                n[fp] += 1
        print(f'{year} admin centers by family-pay item (1 = families pay, 2 = none pay, -1 = DK):')
        for k in sorted(enrl, key=lambda z: (z is None, z or 0)):
            print(f'  {k}: {n[k]} centers, weighted enrollment under 6 = {enrl[k]:,.0f}')


if __name__ == '__main__':
    main()
