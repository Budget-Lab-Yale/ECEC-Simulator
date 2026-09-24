#!/usr/bin/env python3
"""
Derives the NHES-ECPP figures the build checks NSECE against, and writes them
as one small CSV. Reads the committed public-use file
raw/US_NHES__ecpp_public_use_file__2019.csv (19 MB); codebook at
raw/documentation/US_NHES__ecpp_public_use_codebook__2019.pdf.

    python state_data/cluster/nhes_summary.py \
        state_data/raw/US_NHES__ecpp_children_0_4_by_hours_universe__2019.csv

WHY NHES. It is the only source that measures hours of care and can therefore
be cut at an arbitrary threshold. NSECE universe A is its five-hour screener
and universe B is total hours >= 10; NSECE_UNIVERSE_RATIO is A over B. NHES can
produce the same ratio independently, which nothing else can.

Weighted by FEWT, the ECPP full-sample weight. FEWT1..FEWT80 are replicate
weights for variance estimation, not alternative point weights.

Ages 0-4 only. ECPP covers birth through age 5 and not yet in kindergarten, so
it holds children the build's 0-4 population does not.
"""

import csv
import os
import sys


def repo_root():
    return os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..')

PATH = os.path.join(repo_root(), 'state_data', 'raw',
                    'US_NHES__ecpp_public_use_file__2019.csv')
OUT = sys.argv[1] if len(sys.argv) > 1 else 'nhes_ecpp_universes.csv'

REGION = {'1': 'northeast', '2': 'south', '3': 'midwest', '4': 'west'}
PT_CUT = 20.0
MAX_AGE = 4
WEIGHT = 'FEWT'

UNIVERSES = [
    ('any',  lambda h: h > 0,   'any positive hours'),
    ('ge5',  lambda h: h >= 5,  '5+ hrs/week -- NSECE universe A analogue'),
    ('ge10', lambda h: h >= 10, '10+ hrs/week -- NSECE universe B, NSCH cut'),
]


def load():
    with open(PATH, newline='') as f:
        r = csv.reader(f)
        hdr = next(r)
        for k in ('CAREHOURX', 'CENREG', WEIGHT, 'AGE2018'):
            if k not in hdr:
                raise SystemExit('missing column %s' % k)
        i = {k: hdr.index(k) for k in ('CAREHOURX', 'CENREG', WEIGHT, 'AGE2018')}
        rows = []
        for row in r:
            try:
                h = float(row[i['CAREHOURX']])
                w = float(row[i[WEIGHT]])
                a = int(row[i['AGE2018']])
            except ValueError:
                continue
            if a > MAX_AGE:
                continue
            rows.append((REGION.get(row[i['CENREG']], 'unknown'), h, w))
        return rows


def main():
    rows = load()
    out = []
    for key, keep, label in UNIVERSES:
        sel = [r for r in rows if keep(r[1])]
        for reg in ('northeast', 'midwest', 'south', 'west', 'national'):
            g = sel if reg == 'national' else [r for r in sel if r[0] == reg]
            if not g:
                continue
            w = sum(r[2] for r in g)
            out.append({
                'universe': key,
                'universe_label': label,
                'region': reg,
                'n': len(g),
                'weighted_children': round(w, 4),
                'mean_weekly_hours': round(
                    sum(r[1] * r[2] for r in g) / w, 10),
                'share_under_20_hrs': round(
                    sum(r[2] for r in g if r[1] < PT_CUT) / w, 10),
            })

    with open(OUT, 'w', newline='') as f:
        wr = csv.DictWriter(f, fieldnames=[
            'universe', 'universe_label', 'region', 'n', 'weighted_children',
            'mean_weekly_hours', 'share_under_20_hrs'])
        wr.writeheader()
        wr.writerows(out)

    print('wrote %s, %d rows' % (OUT, len(out)))
    nat = {r['universe']: r for r in out if r['region'] == 'national'}
    print('ratio ge5/ge10  = %.10f'
          % (nat['ge5']['weighted_children'] / nat['ge10']['weighted_children']))
    print('ratio any/ge10  = %.10f'
          % (nat['any']['weighted_children'] / nat['ge10']['weighted_children']))


if __name__ == '__main__':
    main()
