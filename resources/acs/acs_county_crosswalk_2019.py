"""
acs_county_crosswalk_2019.py -- ACS 2019 1-year household -> county and PUMA.

The simulator's raw ACS pull (shared/raw_data/ACS/v1/2023/ipums_usa.csv)
carries STATEFIP but no county, so it cannot select a sub-state geography
(SUBSTATE_GEOGRAPHIES in src/shared_functions/constants.R -- NYC). IPUMS
extract usa:33 (resources/supply/acs_ipums_extract_2019.R) holds the same
2019 1-year sample (us2019a, SAMPLE 201901) with COUNTYFIP and PUMA. IPUMS
SERIAL is stable within a sample, so one row per household joins onto the
simulator's hh_id (= SERIAL).

  build     usa:33 fixed-width + DDI -> one row per SAMPLE 201901 household:
            SERIAL, CBSERIAL, STATEFIP, COUNTYFIP, PUMA  (gzip CSV)
  validate  every household in the simulator's ipums_usa.csv must find its
            SERIAL in the crosswalk w/ the same CBSERIAL and STATEFIP --
            CBSERIAL is the Census id, so agreement on it proves the join

COUNTYFIP is IPUMS's 3-digit within-state code; 0 = county not identified.
Every NYC PUMA nests in one borough, so all five NYC counties are identified.

Usage (cluster; usa:33 is too large for the laptop):
  python acs_county_crosswalk_2019.py build    <usa33_dir> <out.csv.gz>
  python acs_county_crosswalk_2019.py validate <out.csv.gz> <ipums_usa.csv>
"""

import csv
import glob
import gzip
import os
import re
import sys

SAMPLE_2019_1YR = 201901
FIELDS = ['SERIAL', 'CBSERIAL', 'STATEFIP', 'COUNTYFIP', 'PUMA']


def substate_counties(geo):
    """(statefip, {countyfip}) for geo from config/substate_geographies.csv --
    one definition shared w/ SUBSTATE_GEOGRAPHIES in constants.R. Looks
    beside repo checkout holding this script, then in cwd (repo root)."""
    rel = os.path.join('config', 'substate_geographies.csv')
    here = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', rel)
    path = here if os.path.exists(here) else rel
    with open(path, newline='') as f:
        rows = [r for r in csv.DictReader(f) if r['geo'] == geo]
    fips = {int(r['statefip']) for r in rows}
    if len(fips) != 1:
        raise SystemExit('%s: %s needs rows w/ one statefip, found %s'
                         % (path, geo, sorted(fips)))
    return fips.pop(), {int(r['countyfip']) for r in rows}


# Bronx, Kings, New York, Queens, Richmond
NYC_STATEFIP, NYC = substate_counties('NYC')


def ddi_layout(d):
    """{var: (start0, end0)} from the one DDI .xml in d, and the .dat.gz path."""
    xml = sorted(glob.glob(os.path.join(d, '*.xml')))
    dat = sorted(glob.glob(os.path.join(d, '*.dat.gz')))
    if len(xml) != 1 or len(dat) != 1:
        raise SystemExit('want one .xml and one .dat.gz in %s, found %s %s'
                         % (d, xml, dat))
    with open(xml[0], encoding='utf-8', errors='replace') as f:
        t = f.read()
    lay = {}
    for m in re.finditer(r'<var\b[^>]*\bID="([^"]+)"[^>]*>(.*?)</var>', t, re.S):
        loc = re.search(r'<location\b[^>]*StartPos="(\d+)"[^>]*width="(\d+)"',
                        m.group(2))
        if loc:
            s, w = int(loc.group(1)), int(loc.group(2))
            lay[m.group(1)] = (s - 1, s - 1 + w)
    missing = [v for v in ['SAMPLE'] + FIELDS if v not in lay]
    if missing:
        raise SystemExit('DDI lacks %s -- extract cannot give a county crosswalk'
                         % missing)
    return lay, dat[0]


def build(d, out_path):
    """Write one row per SAMPLE 201901 household (FIELDS) to out_path; stop
    if a SERIAL carries two geographies."""
    lay, dat = ddi_layout(d)
    a, b = lay['SAMPLE']
    seen = {}
    n_lines = 0
    with gzip.open(dat, 'rt', errors='replace') as f:
        for line in f:
            n_lines += 1
            if int(line[a:b]) != SAMPLE_2019_1YR:
                continue
            row = {v: int(line[slice(*lay[v])]) for v in FIELDS}
            prev = seen.get(row['SERIAL'])
            if prev is None:
                seen[row['SERIAL']] = row
            elif prev != row:
                raise SystemExit('SERIAL %d carries two geographies: %s vs %s'
                                 % (row['SERIAL'], prev, row))
    with gzip.open(out_path, 'wt', newline='') as f:
        w = csv.DictWriter(f, fieldnames=FIELDS)
        w.writeheader()
        for k in sorted(seen):
            w.writerow(seen[k])
    nyc = sum(1 for r in seen.values()
              if r['STATEFIP'] == NYC_STATEFIP and r['COUNTYFIP'] in NYC)
    ided = sum(1 for r in seen.values() if r['COUNTYFIP'] != 0)
    print('read %d person records; %d households in SAMPLE %d'
          % (n_lines, len(seen), SAMPLE_2019_1YR))
    print('county identified: %d households (%.1f%%); NYC: %d'
          % (ided, 100 * ided / len(seen), nyc))
    print('wrote', out_path)


def validate(xwalk_path, sim_path):
    """Check every YEAR 2019 simulator household joins crosswalk on SERIAL w/
    same CBSERIAL, STATEFIP; exit nonzero otherwise."""
    xw = {}
    with gzip.open(xwalk_path, 'rt') as f:
        for r in csv.DictReader(f):
            xw[int(r['SERIAL'])] = (int(r['CBSERIAL']), int(r['STATEFIP']),
                                    int(r['COUNTYFIP']))
    hh = {}
    with open(sim_path, newline='') as f:
        rd = csv.reader(f)
        head = next(rd)
        i = {v: head.index(v) for v in ['YEAR', 'SERIAL', 'CBSERIAL', 'STATEFIP']}
        for r in rd:
            if r[i['YEAR']] != '2019':
                continue
            s = int(r[i['SERIAL']])
            if s not in hh:
                hh[s] = (int(r[i['CBSERIAL']]), int(r[i['STATEFIP']]))
    n = len(hh)
    absent = [s for s in hh if s not in xw]
    cb_bad = [s for s in hh if s in xw and xw[s][0] != hh[s][0]]
    st_bad = [s for s in hh if s in xw and xw[s][1] != hh[s][1]]
    nyc = sum(1 for s in hh if s in xw and xw[s][1] == NYC_STATEFIP and xw[s][2] in NYC)
    print('simulator households (YEAR 2019): %d' % n)
    print('  SERIAL absent from crosswalk:   %d' % len(absent))
    print('  CBSERIAL disagrees:             %d' % len(cb_bad))
    print('  STATEFIP disagrees:             %d' % len(st_bad))
    print('  NYC households:                 %d' % nyc)
    for lab, bad in (('absent', absent), ('cbserial', cb_bad), ('statefip', st_bad)):
        if bad:
            print('  first %s: %s' % (lab, bad[:5]))
    if absent or cb_bad or st_bad:
        raise SystemExit('VALIDATION FAILED')
    print('VALIDATION PASSED -- every simulator household joins on SERIAL')


if __name__ == '__main__':
    if len(sys.argv) != 4 or sys.argv[1] not in ('build', 'validate'):
        raise SystemExit(__doc__)
    (build if sys.argv[1] == 'build' else validate)(sys.argv[2], sys.argv[3])
