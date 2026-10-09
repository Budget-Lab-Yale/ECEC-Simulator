#!/usr/bin/env python3
"""
National occupation mix of childcare workers by education, IPUMS ACS -- the
weights w(o, g) of oes_childcare_wage_ratios_2019.R.

OES gives state wages by occupation, no education; ACS gives education by
occupation. w(o, g) = national person-weighted share of education group g's
childcare workers in occupation o. National only -- large sample, no small
state cells. State ratio then = OES state wages mixed at national weights.

POPULATION -- same as the former ACS wage ratios (acs_childcare_wage_by_
education_2019.py, git history): household residents (GQ 1, 2), positive
earnings (INCWAGE + INCBUS00, 999999 -> 0) and usual hours, ACS occupations
    4600  childcare_worker    39-9011, any industry but private households
                              (IND 9290: nannies -- simulator's Other Paid)
    2300  preschool_teacher   25-2010, preschool + kindergarten
    2330  sped_teacher        25-2050, all levels
    230   administrator       11-9030, all levels
last three kept only in child day care services (IND 8470, NAICS 6244) --
in that industry 2300 is preschool, not kindergarten, teaching.
Education: EDUC >= 10 (4+ years college) = ba.

SAMPLE -- us2019c (2015-2019 ACS 5-year, SAMPLE 201903), all five MULTYEAR
years pooled: shares carry no dollars, so no deflation, and five years
steady small cells (sped teachers, administrators). Also writes the
2018-19-only shares for comparison (sample column).

    python resources/supply/acs_childcare_occupation_weights_2019.py <usa34_dir> <out.csv>

Output: sample, educ_group, occupation, soc, n (unweighted records),
workers_wt, weight (share of group, sums to 1 per sample x group).
"""

import csv
import glob
import gzip
import os
import re
import sys
from collections import defaultdict

SAMPLE_CODE = 201903
SAMPLES = {'acs5_2015_2019': {2015, 2016, 2017, 2018, 2019},
           'acs5_2018_2019': {2018, 2019}}
HH_GQ = {1, 2}
TOPCODE_NA = 999999
OCC_GROUP = {4600: ('childcare_worker', '39-9011'),
             2300: ('preschool_teacher', '25-2011'),
             2330: ('sped_teacher', '25-2051'),
             230:  ('administrator', '11-9031')}
IND_CHILD_DAY_CARE = {8470}
IND_PRIVATE_HOUSEHOLDS = {9290}
NEED = ['SAMPLE', 'MULTYEAR', 'GQ', 'OCC', 'IND', 'UHRSWORK', 'INCWAGE',
        'INCBUS00', 'EDUC', 'PERWT']


def ddi(d):
    """Layout {var: (a, b, dcml)}, .dat.gz path."""
    xml = sorted(glob.glob(os.path.join(d, '*.xml')))
    dat = sorted(glob.glob(os.path.join(d, '*.dat.gz')))
    if len(xml) != 1 or len(dat) != 1:
        raise SystemExit('want one .xml and one .dat.gz in %s' % d)
    t = open(xml[0], encoding='utf-8', errors='replace').read()
    lay = {}
    for m in re.finditer(r'<var\b[^>]*\bID="([^"]+)"[^>]*>(.*?)</var>', t, re.S):
        head = m.group(0)[:m.group(0).find('>') + 1]
        loc = re.search(r'<location\b[^>]*StartPos="(\d+)"[^>]*width="(\d+)"', m.group(2))
        if not loc:
            continue
        dc = re.search(r'\bdcml="(\d+)"', head)
        s, w = int(loc.group(1)), int(loc.group(2))
        lay[m.group(1)] = (s - 1, s - 1 + w, int(dc.group(1)) if dc else 0)
    return lay, dat[0]


def val(line, lay, v):
    a, b, dc = lay[v]
    s = line[a:b].strip()
    if not s:
        return None
    x = float(s) if '.' in s else int(s)
    return x / (10 ** dc) if dc and '.' not in s else x


def main(d, out_path):
    lay, dat = ddi(d)
    missing = [v for v in NEED if v not in lay]
    if missing:
        raise SystemExit('DDI lacks %s' % missing)
    oa, ob = lay['OCC'][:2]

    W = defaultdict(float)
    N = defaultdict(int)
    with gzip.open(dat, 'rt', errors='replace') as f:
        for nrow, line in enumerate(f, 1):
            if nrow % 5000000 == 0:
                print('  rows %d' % nrow, flush=True)
            occ = line[oa:ob].strip()
            g = OCC_GROUP.get(int(occ) if occ else 0)
            if g is None:
                continue
            if val(line, lay, 'SAMPLE') != SAMPLE_CODE or val(line, lay, 'GQ') not in HH_GQ:
                continue
            ind = val(line, lay, 'IND')
            if not ((g[0] == 'childcare_worker' and ind not in IND_PRIVATE_HOUSEHOLDS)
                    or ind in IND_CHILD_DAY_CARE):
                continue
            wage = val(line, lay, 'INCWAGE') or 0
            bus = val(line, lay, 'INCBUS00') or 0
            earn = (0 if wage == TOPCODE_NA else wage) + (0 if bus == TOPCODE_NA else bus)
            hrs = val(line, lay, 'UHRSWORK')
            if earn <= 0 or not hrs or hrs <= 0:
                continue
            educ = 'ba' if val(line, lay, 'EDUC') >= 10 else 'no_ba'
            yr = val(line, lay, 'MULTYEAR')
            w = val(line, lay, 'PERWT')
            for sname, years in SAMPLES.items():
                if yr in years:
                    W[(sname, educ, g)] += w
                    N[(sname, educ, g)] += 1

    rows = []
    for sname in SAMPLES:
        for educ in ('no_ba', 'ba'):
            tot = sum(v for (s, e, _), v in W.items() if s == sname and e == educ)
            if tot <= 0:
                raise SystemExit('no %s workers in %s' % (educ, sname))
            for code in sorted(OCC_GROUP):
                g = OCC_GROUP[code]
                rows.append({'sample': sname, 'educ_group': educ, 'occupation': g[0],
                             'soc': g[1], 'n': N[(sname, educ, g)],
                             'workers_wt': round(W[(sname, educ, g)]),
                             'weight': round(W[(sname, educ, g)] / tot, 6)})
    with open(out_path, 'w', newline='') as f:
        wr = csv.DictWriter(f, fieldnames=list(rows[0]))
        wr.writeheader()
        wr.writerows(rows)
    for r in rows:
        print(r, flush=True)
    print('wrote %s' % out_path)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    main(sys.argv[1], sys.argv[2])
