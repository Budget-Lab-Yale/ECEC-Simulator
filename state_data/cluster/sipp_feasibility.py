#!/usr/bin/env python3
"""
Phase 4 feasibility count: SIPP children 0-4 with a non-parental care
arrangement, by state, pooled over the 2018+ panel.

Streams each pu<year>.csv once. Pipe-delimited, thousands of columns, so it
maps the header to indices and keeps only the columns it needs. One person-year
is one observation; the care battery is annual ("fall of the reference year"),
so a person counts once per year if ANY month row carries the flag.

Writes sipp_feasibility_<year>.csv per year and a pooled summary.
"""

import csv
import gzip
import os
import re
import sys
from collections import defaultdict


def shared_root():
    """roots.input from config/default_paths.yaml -- same root simulator reads."""
    cfg = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       '..', '..', 'config', 'default_paths.yaml')
    with open(cfg) as f:
        for line in f:
            m = re.match(r"\s+input:\s*'([^']+)'", line)
            if m:
                return m.group(1)
    raise SystemExit('no roots.input in %s' % cfg)

SIPP_ROOT = os.path.join(shared_root(), 'raw_data', 'SIPP')
OUT_DIR = sys.argv[1] if len(sys.argv) > 1 else '.'
YEARS = [2018, 2019, 2020, 2021, 2022, 2023]

# Child-level care recodes. The build's analogue of a NSECE regular
# non-parental arrangement -- excludes RSELF (reference parent), RPAR (other
# parent), RKIDSELF (self-care).
NONPARENTAL = ['RDAYCARE', 'RNUR', 'RFAM', 'RNREL', 'RGRAN', 'RSIB15',
               'ROTHR', 'RHEADST', 'RPROG']
# Head Start sub-flags, tracked but not added -- they overlap RDAYCARE/RNUR.
HS_SUB = ['RDAYHS', 'RNURHS']
PARENTAL = ['RSELF', 'RPAR', 'RKIDSELF']
# Price and hours. TPAYWK is the ONLY quantity in the battery.
MONEY = ['TPAYWK', 'EPAY']
KEYS = ['SSUID', 'PNUM', 'MONTHCODE', 'TAGE', 'TAGE_EHC', 'TST_INTV',
        'WPFINWGT']

WANT = KEYS + NONPARENTAL + HS_SUB + PARENTAL + MONEY

FIPS = {
    '1': 'AL', '2': 'AK', '4': 'AZ', '5': 'AR', '6': 'CA', '8': 'CO',
    '9': 'CT', '10': 'DE', '11': 'DC', '12': 'FL', '13': 'GA', '15': 'HI',
    '16': 'ID', '17': 'IL', '18': 'IN', '19': 'IA', '20': 'KS', '21': 'KY',
    '22': 'LA', '23': 'ME', '24': 'MD', '25': 'MA', '26': 'MI', '27': 'MN',
    '28': 'MS', '29': 'MO', '30': 'MT', '31': 'NE', '32': 'NV', '33': 'NH',
    '34': 'NJ', '35': 'NM', '36': 'NY', '37': 'NC', '38': 'ND', '39': 'OH',
    '40': 'OK', '41': 'OR', '42': 'PA', '44': 'RI', '45': 'SC', '46': 'SD',
    '47': 'TN', '48': 'TX', '49': 'UT', '50': 'VT', '51': 'VA', '53': 'WA',
    '54': 'WV', '55': 'WI', '56': 'WY',
}


def open_any(path):
    if path.endswith('.gz'):
        return gzip.open(path, 'rt', newline='')
    return open(path, 'rt', newline='')


def find_file(year):
    for name in ('pu%d.csv' % year, 'pu%d.csv.gz' % year):
        p = os.path.join(SIPP_ROOT, str(year), name)
        if os.path.exists(p):
            return p
    return None


def run_year(year):
    path = find_file(year)
    if path is None:
        print('%d: NO FILE' % year, flush=True)
        return None

    fh = open_any(path)
    header = fh.readline().rstrip('\r\n').split('|')
    idx = {}
    missing = []
    for v in WANT:
        if v in header:
            idx[v] = header.index(v)
        else:
            missing.append(v)
    print('%d: %s | %d cols | missing: %s'
          % (year, os.path.basename(path), len(header),
             ','.join(missing) if missing else 'none'), flush=True)

    if 'TAGE' not in idx or 'TST_INTV' not in idx:
        fh.close()
        return None

    care_vars = [v for v in NONPARENTAL if v in idx]
    if not care_vars:
        print('%d: NO CARE VARIABLES -- module absent this year' % year,
              flush=True)
        fh.close()
        return None

    maxi = max(idx.values())

    # person-year -> record. Children 0-4 only, so this stays small.
    kids = {}
    # which monthcodes carry a non-blank care value
    month_filled = defaultdict(int)
    nrow = 0

    for line in fh:
        nrow += 1
        f = line.rstrip('\r\n').split('|')
        if len(f) <= maxi:
            continue
        a = f[idx['TAGE']]
        if not a or not a.isdigit() or int(a) > 4:
            continue

        pid = (f[idx['SSUID']], f[idx['PNUM']])
        r = kids.get(pid)
        if r is None:
            r = {'state': f[idx['TST_INTV']], 'w': 0.0, 'care': 0,
                 'types': set(), 'paywk': '', 'age': int(a)}
            kids[pid] = r

        mc = f[idx['MONTHCODE']] if 'MONTHCODE' in idx else ''
        any_filled = False
        for v in care_vars:
            val = f[idx[v]]
            if val:
                any_filled = True
            if val == '1':
                r['care'] = 1
                r['types'].add(v)
        if any_filled:
            month_filled[mc] += 1

        # weight: take the max over months (months out of sample are 0)
        try:
            w = float(f[idx['WPFINWGT']])
            if w > r['w']:
                r['w'] = w
        except (ValueError, KeyError):
            pass

        if 'TPAYWK' in idx and f[idx['TPAYWK']] and not r['paywk']:
            r['paywk'] = f[idx['TPAYWK']]

    fh.close()

    by_state = defaultdict(lambda: {'kids': 0, 'care': 0, 'care_w': 0.0,
                                    'paywk': 0})
    for r in kids.values():
        st = FIPS.get(r['state'].lstrip('0'), r['state'])
        s = by_state[st]
        s['kids'] += 1
        if r['care']:
            s['care'] += 1
            s['care_w'] += r['w']
            if r['paywk']:
                s['paywk'] += 1

    out = os.path.join(OUT_DIR, 'sipp_feasibility_%d.csv' % year)
    with open(out, 'w', newline='') as g:
        w = csv.writer(g)
        w.writerow(['year', 'state', 'children_0_4', 'with_care',
                    'with_care_weighted', 'with_care_and_paywk'])
        for st in sorted(by_state):
            s = by_state[st]
            w.writerow([year, st, s['kids'], s['care'],
                        round(s['care_w'], 1), s['paywk']])

    tot_k = sum(s['kids'] for s in by_state.values())
    tot_c = sum(s['care'] for s in by_state.values())
    tot_p = sum(s['paywk'] for s in by_state.values())
    print('%d: rows %d | children 0-4 %d | with care %d | w/ TPAYWK %d'
          % (year, nrow, tot_k, tot_c, tot_p), flush=True)
    print('%d: monthcodes carrying a care value: %s'
          % (year, dict(sorted(month_filled.items()))), flush=True)
    return by_state


def main():
    pooled = defaultdict(lambda: {'kids': 0, 'care': 0, 'paywk': 0})
    years_ok = []
    for y in YEARS:
        r = run_year(y)
        if r is None:
            continue
        years_ok.append(y)
        for st, s in r.items():
            p = pooled[st]
            p['kids'] += s['kids']
            p['care'] += s['care']
            p['paywk'] += s['paywk']

    out = os.path.join(OUT_DIR, 'sipp_feasibility_pooled.csv')
    with open(out, 'w', newline='') as g:
        w = csv.writer(g)
        w.writerow(['state', 'children_0_4', 'with_care', 'with_care_and_paywk',
                    'years'])
        for st in sorted(pooled):
            p = pooled[st]
            w.writerow([st, p['kids'], p['care'], p['paywk'],
                        ' '.join(str(y) for y in years_ok)])

    print()
    print('POOLED over %s' % years_ok)
    print('%-6s %10s %10s %10s' % ('state', 'kids0_4', 'with_care', 'w/price'))
    for st in sorted(pooled):
        p = pooled[st]
        print('%-6s %10d %10d %10d' % (st, p['kids'], p['care'], p['paywk']))


if __name__ == '__main__':
    main()
