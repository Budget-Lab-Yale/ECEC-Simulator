#!/usr/bin/env python3
"""
State employment-population ratios by sex and age bin, from IPUMS ACS.

resources/epop/epop_projections.csv is national: 26 columns, 13 age bins x two
sexes, 1994-2100. config/state/employment/<ST>.csv takes the same schema and
nothing has ever been written to it. This produces the state series the
simulator would read.

ACS is the right instrument rather than CPS: the CPS-Monthly extract on the
cluster carries no STATEFIP, and CPS-ASEC's sample is too thin for
state x sex x 13 age bins. ACS gives ~3M persons a year, 2006-2024.

IPUMS ships fixed-width .dat.gz with column positions in the DDI .xml, so the
layout is read from the DDI rather than hard-coded -- a hard-coded offset would
silently misread if the extract were ever rebuilt with different variables.

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

# The national file's bins, in its column order.
BINS = [(16, 17, '16_17'), (18, 19, '18_19'), (20, 24, '20_24'),
        (25, 29, '25_29'), (30, 34, '30_34'), (35, 39, '35_39'),
        (40, 44, '40_44'), (45, 49, '45_49'), (50, 54, '50_54'),
        (55, 59, '55_59'), (60, 64, '60_64'), (65, 200, '65plus')]


def bin_of(age):
    if age < 16:
        return None
    for lo, hi, name in BINS:
        if lo <= age <= hi:
            return name
    return None


def layout(xml_path):
    """StartPos and width for each needed variable, from the IPUMS DDI."""
    ns = {'d': 'ddi:codebook:2_5'}
    root = ET.parse(xml_path).getroot()
    out = {}
    for v in root.iter():
        if not v.tag.endswith('}var') and v.tag != 'var':
            continue
        name = v.get('ID')
        if name not in NEED:
            continue
        for loc in v:
            if loc.tag.endswith('}location') or loc.tag == 'location':
                s = int(loc.get('StartPos'))
                w = int(loc.get('width'))
                out[name] = (s - 1, s - 1 + w)   # 0-based slice
    return out


def run_year(year, out_rows):
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

    acc = defaultdict(lambda: [0.0, 0.0, 0])   # employed_wt, pop_wt, n
    nrow = 0
    with gzip.open(dat, 'rt', errors='replace') as fh:
        for line in fh:
            nrow += 1
            try:
                age = int(line[lay['AGE'][0]:lay['AGE'][1]])
            except ValueError:
                continue
            b = bin_of(age)
            if b is None:
                continue
            try:
                st = int(line[lay['STATEFIP'][0]:lay['STATEFIP'][1]])
                sx = int(line[lay['SEX'][0]:lay['SEX'][1]])
                es = int(line[lay['EMPSTAT'][0]:lay['EMPSTAT'][1]])
                # PERWT carries 2 implied decimals in IPUMS ACS
                w = int(line[lay['PERWT'][0]:lay['PERWT'][1]]) / 100.0
            except ValueError:
                continue
            if sx not in (1, 2) or w <= 0:
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
    years = [int(x) for x in sys.argv[1:-1]] if len(sys.argv) > 2 else []
    out = sys.argv[-1]
    if not years:
        years = list(range(2006, 2025))
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
