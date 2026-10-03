#!/usr/bin/env python3
"""
State employment-population ratios by sex and age bin, from IPUMS ACS.

resources/epop/epop_projections.csv is national: 26 columns, 12 age bins +
16plus x two sexes, 1994-2100. config/state/employment/<ST>.csv takes same
schema; state_epop_projections.R builds it from this file's output.

ACS, not CPS: CPS-Monthly extract on cluster carries no STATEFIP, CPS-ASEC
sample too thin for state x sex x 12 age bins. ACS gives ~3M persons a year,
2006-2024.

IPUMS ships fixed-width .dat.gz w/ column positions in DDI .xml -- layout read
from DDI, not hard-coded; hard-coded offset would silently misread if extract
ever rebuilt w/ different variables.

Writes one long CSV: year, state_fips, sex, age_bin, employed_wt, pop_wt, n.
"""

import csv
import gzip
import os
import re
import sys
import xml.etree.ElementTree as ET
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


ACS_ROOT = os.path.join(shared_root(), 'raw_data', 'ACS', 'acs_common_v2')
NEED = ['STATEFIP', 'AGE', 'SEX', 'EMPSTAT', 'PERWT']

# National file's bins, in its column order
BINS = [(16, 17, '16_17'), (18, 19, '18_19'), (20, 24, '20_24'),
        (25, 29, '25_29'), (30, 34, '30_34'), (35, 39, '35_39'),
        (40, 44, '40_44'), (45, 49, '45_49'), (50, 54, '50_54'),
        (55, 59, '55_59'), (60, 64, '60_64'), (65, 200, '65plus')]


def bin_of(age):
    """BINS name holding age, or None (under 16, over 200)."""
    for lo, hi, name in BINS:
        if lo <= age <= hi:
            return name
    return None


def layout(xml_path):
    """StartPos and width for each needed variable, from the IPUMS DDI."""
    root = ET.parse(xml_path).getroot()
    out = {}
    local = lambda e: e.tag.rsplit('}', 1)[-1]   # tag w/out namespace
    for v in root.iter():
        name = v.get('ID')
        if local(v) != 'var' or name not in NEED:
            continue
        for loc in v:
            if local(loc) == 'location':
                s = int(loc.get('StartPos'))
                w = int(loc.get('width'))
                out[name] = (s - 1, s - 1 + w)   # 0-based slice
    return out


def run_year(year, out_rows):
    """Append one year's state x sex x age-bin cells to out_rows; skip year
    if extract or DDI missing."""
    d = os.path.join(ACS_ROOT, 'us%da' % year)
    dat = os.path.join(d, 'usa_%da.dat.gz' % year)
    xml = os.path.join(d, 'usa_%da.xml' % year)
    if not (os.path.exists(dat) and os.path.exists(xml)):
        print('%d: missing' % year, flush=True)
        return
    lay = layout(xml)
    missing = [v for v in NEED if v not in lay]
    if missing:
        print('%d: DDI lacks %s -- skipped' % (year, missing), flush=True)
        return

    sl = {v: slice(*lay[v]) for v in NEED}
    acc = defaultdict(lambda: [0.0, 0.0, 0])   # employed_wt, pop_wt, n
    nrow = 0
    with gzip.open(dat, 'rt', errors='replace') as fh:
        for line in fh:
            nrow += 1
            try:
                age, st, sx, es, pw = (int(line[sl[v]]) for v in
                                       ('AGE', 'STATEFIP', 'SEX', 'EMPSTAT', 'PERWT'))
            except ValueError:
                continue
            b = bin_of(age)
            w = pw / 100.0   # PERWT: 2 implied decimals
            if b is None or sx not in (1, 2) or w <= 0:
                continue
            k = (st, 'm' if sx == 1 else 'f', b)
            a = acc[k]
            a[1] += w                      # population 16+
            if es == 1:                    # 1 = employed
                a[0] += w
            a[2] += 1

    for (st, sx, b), (e, p, n) in acc.items():
        out_rows.append([year, st, sx, b, round(e, 2), round(p, 2), n])
    print('%d: rows %d, cells %d' % (year, nrow, len(acc)), flush=True)


def main():
    """argv: [year ...] out.csv -- no years = 2006-2024. out.csv required:
    w/out it argv[-1] is this script's own path, overwritten."""
    if len(sys.argv) < 2 or sys.argv[-1].endswith('.py') or sys.argv[-1].isdigit():
        raise SystemExit('usage: acs_epop.py [year ...] <out.csv>')
    years = [int(x) for x in sys.argv[1:-1]] or list(range(2006, 2025))
    out = sys.argv[-1]
    rows = []
    for y in years:
        run_year(y, rows)
    with open(out, 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['year', 'state_fips', 'sex', 'age_bin',
                    'employed_wt', 'pop_wt', 'n'])
        w.writerows(rows)
    print('wrote %s, %d rows' % (out, len(rows)))


if __name__ == '__main__':
    main()
