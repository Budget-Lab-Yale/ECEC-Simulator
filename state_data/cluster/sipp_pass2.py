#!/usr/bin/env python3
"""
Phase 4 second pass. Three things the first pass could not answer:

  1. DISTINCT children. SIPP 2018+ panels overlap, so a child appears in
     several year files. Pooled person-years overstate the sample.
  2. PRICE COVERAGE. TPAYWK sits on the reference parent's record, not the
     child's, so pass 1 found zero. Link each child to its own household's
     TPAYWK and count again.
  3. CARE TYPE DETAIL. How many children fall in each of the build's six
     types, by state -- the cell sizes a state-level split would rest on.

Writes sipp_pass2_*.csv.
"""

import csv
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

# The build's six types, from SIPP's flags. Priority order matches the
# simulator's own rule: paid center > unpaid center > paid home > other paid >
# unpaid home > other unpaid. SIPP cannot see payment per arrangement, so
# "paid" here is the household's EPAY answer -- an approximation, and one more
# reason SIPP is not a drop-in.
CARE = ['RDAYCARE', 'RNUR', 'RHEADST', 'RFAM', 'RNREL', 'RGRAN', 'RSIB15',
        'ROTHR', 'RPROG']
KEYS = ['SSUID', 'PNUM', 'ERELRPE', 'MONTHCODE', 'TAGE', 'TST_INTV',
        'WPFINWGT', 'TPAYWK', 'EPAY']

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

# child -> set of years seen; child -> state; child -> care flags
child_years = defaultdict(set)
child_state = {}
child_care = defaultdict(set)
child_has_care = {}
# household-year -> TPAYWK present
hh_paywk = {}
child_hh = {}


def main():
    for year in YEARS:
        path = os.path.join(SIPP_ROOT, str(year), 'pu%d.csv' % year)
        if not os.path.exists(path):
            print('%d: no file' % year, flush=True)
            continue
        fh = open(path, 'rt', newline='')
        header = fh.readline().rstrip('\r\n').split('|')
        idx = {v: header.index(v) for v in KEYS + CARE if v in header}
        maxi = max(idx.values())
        miss = [v for v in KEYS + CARE if v not in idx]
        print('%d: missing %s' % (year, miss if miss else 'none'), flush=True)

        for line in fh:
            f = line.rstrip('\r\n').split('|')
            if len(f) <= maxi:
                continue
            ssuid = f[idx['SSUID']]

            # household-level: does ANY person report paying for care?
            if 'TPAYWK' in idx and f[idx['TPAYWK']]:
                hh_paywk[(ssuid, year)] = f[idx['TPAYWK']]

            a = f[idx['TAGE']]
            if not a or not a.isdigit() or int(a) > 4:
                continue
            cid = (ssuid, f[idx['PNUM']])
            child_years[cid].add(year)
            child_state[cid] = f[idx['TST_INTV']]
            child_hh[cid] = (ssuid, year)
            for v in CARE:
                if v in idx and f[idx[v]] == '1':
                    child_care[cid].add(v)
                    child_has_care[cid] = 1
        fh.close()
        print('%d: cumulative distinct children 0-4 = %d'
              % (year, len(child_years)), flush=True)

    # --- 1 + 2: distinct children, and price coverage -----------------------
    by_state = defaultdict(lambda: {'kids': 0, 'care': 0, 'price': 0,
                                    'multi_year': 0})
    for cid, yrs in child_years.items():
        st = FIPS.get(child_state[cid].lstrip('0'), child_state[cid] or 'NA')
        s = by_state[st]
        s['kids'] += 1
        if len(yrs) > 1:
            s['multi_year'] += 1
        if child_has_care.get(cid):
            s['care'] += 1
            if child_hh[cid] in hh_paywk:
                s['price'] += 1

    with open(os.path.join(OUT_DIR, 'sipp_pass2_by_state.csv'), 'w',
              newline='') as g:
        w = csv.writer(g)
        w.writerow(['state', 'distinct_children_0_4', 'with_care',
                    'with_care_and_hh_price', 'seen_in_multiple_years'])
        for st in sorted(by_state):
            s = by_state[st]
            w.writerow([st, s['kids'], s['care'], s['price'],
                        s['multi_year']])

    tot = sum(s['kids'] for s in by_state.values())
    totc = sum(s['care'] for s in by_state.values())
    totp = sum(s['price'] for s in by_state.values())
    totm = sum(s['multi_year'] for s in by_state.values())
    print()
    print('DISTINCT children 0-4 pooled 2018-2023: %d' % tot)
    print('  with a non-parental arrangement:      %d' % totc)
    print('  of those, household reports TPAYWK:   %d' % totp)
    print('  children appearing in >1 year:        %d' % totm)

    # --- 3: care-type cell sizes by state -----------------------------------
    cell = defaultdict(int)
    for cid, flags in child_care.items():
        st = FIPS.get(child_state[cid].lstrip('0'), child_state[cid] or 'NA')
        for v in flags:
            cell[(st, v)] += 1

    with open(os.path.join(OUT_DIR, 'sipp_pass2_cells.csv'), 'w',
              newline='') as g:
        w = csv.writer(g)
        w.writerow(['state', 'care_flag', 'distinct_children'])
        for (st, v) in sorted(cell):
            w.writerow([st, v, cell[(st, v)]])

    print()
    print('CELL SIZES for the 19 build geographies (distinct children)')
    geos = ['AZ', 'CA', 'CO', 'CT', 'IL', 'IN', 'KY', 'MA', 'MN', 'NJ', 'NM',
            'NY', 'TX', 'VA', 'VT', 'WA', 'WI', 'WV']
    print('%-4s %6s %6s %6s %6s %6s %6s' %
          ('st', 'centre', 'nurs', 'hdst', 'famdc', 'nonrel', 'grand'))
    for st in geos:
        print('%-4s %6d %6d %6d %6d %6d %6d' %
              (st, cell[(st, 'RDAYCARE')], cell[(st, 'RNUR')],
               cell[(st, 'RHEADST')], cell[(st, 'RFAM')],
               cell[(st, 'RNREL')], cell[(st, 'RGRAN')]))


if __name__ == '__main__':
    main()
