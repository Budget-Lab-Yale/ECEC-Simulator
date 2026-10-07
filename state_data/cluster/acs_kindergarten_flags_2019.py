#!/usr/bin/env python3
"""
acs_kindergarten_flags_2019.py -- ACS 2019 children 0-4 already in kindergarten.

NHES-ECPP leaves out children already in kindergarten (NHES:2019 Data File
User's Manual, ch. 2). Simulator's ACS children 0-4 include them. Flags let
07_compare_nhes.R drop them from model side so both cover same children.

Reads IPUMS USA extract usa:34 (resources/supply/acs_ipums_extract_2019.R),
sample us2019a (SAMPLE 201901) -- same 2019 1-year sample as simulator's raw
pull, so SERIAL = simulator hh_id, PERNUM = child_id. Keeps AGE <= 4,
GRADEATT = 2 (kindergarten).

Usage (cluster):
    python state_data/cluster/acs_kindergarten_flags_2019.py <usa34_dir> <out.csv>
Writes SERIAL, PERNUM, AGE, PERWT -- one row per flagged child.
"""

import csv
import glob
import gzip
import os
import re
import sys

SAMPLE_2019_1YR = 201901
GRADEATT_KINDERGARTEN = 2
NEED = ['SAMPLE', 'SERIAL', 'PERNUM', 'AGE', 'GRADEATT', 'PERWT']


def layout(d):
    """{var: (start, end, dcml)} from DDI xml, path of .dat.gz."""
    xml = sorted(glob.glob(os.path.join(d, '*.xml')))
    dat = sorted(glob.glob(os.path.join(d, '*.dat.gz')))
    if len(xml) != 1 or len(dat) != 1:
        raise SystemExit('want one .xml and one .dat.gz in %s' % d)
    t = open(xml[0], encoding='utf-8', errors='replace').read()
    lay = {}
    for m in re.finditer(r'<var\b[^>]*\bID="([^"]+)"[^>]*>(.*?)</var>', t, re.S):
        loc = re.search(r'<location\b[^>]*StartPos="(\d+)"[^>]*width="(\d+)"', m.group(2))
        if not loc:
            continue
        head = m.group(0)[:m.group(0).find('>') + 1]
        dc = re.search(r'\bdcml="(\d+)"', head)
        s, w = int(loc.group(1)), int(loc.group(2))
        lay[m.group(1)] = (s - 1, s - 1 + w, int(dc.group(1)) if dc else 0)
    missing = [v for v in NEED if v not in lay]
    if missing:
        raise SystemExit('DDI lacks %s' % missing)
    return lay, dat[0]


def main(d, out_path):
    lay, dat = layout(d)

    def num(line, v):
        a, b, dc = lay[v]
        s = line[a:b].strip()
        x = int(s)
        return x / (10 ** dc) if dc else x

    n_kids = n_flag = 0
    with gzip.open(dat, 'rt', errors='replace') as f, open(out_path, 'w', newline='') as g:
        w = csv.writer(g)
        w.writerow(['SERIAL', 'PERNUM', 'AGE', 'PERWT'])
        for line in f:
            if num(line, 'SAMPLE') != SAMPLE_2019_1YR or num(line, 'AGE') > 4:
                continue
            n_kids += 1
            if num(line, 'GRADEATT') == GRADEATT_KINDERGARTEN:
                n_flag += 1
                w.writerow([num(line, 'SERIAL'), num(line, 'PERNUM'),
                            num(line, 'AGE'), num(line, 'PERWT')])
    print('children 0-4 (us2019a records): %d; in kindergarten: %d' % (n_kids, n_flag))
    print('wrote', out_path)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    main(sys.argv[1], sys.argv[2])
