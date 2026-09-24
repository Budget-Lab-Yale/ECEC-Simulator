#!/usr/bin/env python3
"""
Hourly earnings of childcare workers by state and education, IPUMS ACS 2019.

Replaces 2018 derivation (acs_ccwage.py, in git history). Reads IPUMS USA
extract usa:33 -- defined in acs_ipums_extract_2019.R -- two samples, one file:

    us2019a   2019 ACS 1-year, base year exactly -- cross-check
    us2019c   2015-2019 ACS 5-year -- ONLY its 2018, 2019 records (MULTYEAR)
              kept, as sample acs5_2018_2019: source of state ratios. 2015-2017
              records dropped -- no pre-base-year wages.

WHAT CHANGED FROM 2018, AND WHY
  Earnings    INCEARN (wage + business + farm income), not INCWAGE. INCWAGE > 0
              dropped unincorporated self-employed -- 13.6% of weighted
              childcare workers in 2018, mostly home-based providers.
  Weeks       WKSWORK1 continuous weeks where ACS asks it (2019 records);
              WKSWORK2 interval midpoints only for 2018 records of 5-year
              sample.
  Dollars     IPUMS states multi-year income in final-year (2019) dollars;
              extract carries no ADJUST. Checked in run output: mean hourly by
              survey year (MULTYEAR) within 5-year sample.
  Sector      CLASSWKR splits self-employed from wage/salary: diagnostic rows
              only -- simulator carries one wage per education group, not per
              sector.
  Occupation  childcare worker (OCC 4600, SOC 39-9011) vs preschool/kindergarten
              teacher (OCC 2300, SOC 25-2011/12): diagnostic rows only.
  Precision   80 replicate weights (REPWTP1-80): successive-difference SE on
              every mean and on every state/national ratio.
  GQ          group-quarters residents (GQ 3, 4) excluded.

POPULATION (unchanged from 2018 -- continuity with committed wage ratios)
  IND 8470 child day care services, OR OCC 4600 / 2300; age 16+; household
  residents. Hourly = earnings / (weeks x usual hours). Excluded: earnings
  <= 0, usual hours <= 0, annual hours < 100, hourly outside 2-200.
  BA = EDUC >= 10 (4+ years of college).

    python resources/supply/acs_ccwage_2019.py --inspect <extract_dir>
    python resources/supply/acs_ccwage_2019.py <extract_dir> <out.csv>

Output long table, one row per sample x state x education x sector x occupation:
  sample, state_fips, educ_group, sector, occ_group, n, workers_wt,
  mean_hourly, se_mean, median_hourly, ratio_to_national, se_ratio
state_fips 0 = national. `all` in sector/occ_group = no split.
"""

import csv
import glob
import gzip
import math
import os
import re
import sys
from collections import defaultdict

IND_CHILDCARE = {8470}
OCC_CHILDCARE = {4600: 'childcare_worker', 2300: 'preschool_teacher'}
WKS_MID = {1: 7.0, 2: 20.0, 3: 33.0, 4: 43.5, 5: 48.5, 6: 51.0}
N_REP = 80

BASE = ['SAMPLE', 'STATEFIP', 'GQ', 'AGE', 'CLASSWKR', 'OCC', 'IND',
        'UHRSWORK', 'WKSWORK1', 'WKSWORK2', 'INCEARN', 'INCWAGE', 'EDUC',
        'PERWT', 'MULTYEAR']
OPTIONAL = ['ADJUST', 'OCCSOC', 'CLASSWKRD']
# 5-year records kept -- base year and year before only.
KEEP_YEARS = {2018, 2019}
LABELLED = ['SAMPLE', 'CLASSWKR', 'CLASSWKRD', 'EDUC', 'GQ', 'WKSWORK2',
            'INCEARN', 'INCWAGE', 'WKSWORK1', 'UHRSWORK', 'ADJUST']


def ddi_files(d):
    xml = sorted(glob.glob(os.path.join(d, '*.xml')))
    dat = sorted(glob.glob(os.path.join(d, '*.dat.gz')))
    if len(xml) != 1 or len(dat) != 1:
        raise SystemExit('want one .xml and one .dat.gz in %s, found %s %s'
                         % (d, xml, dat))
    return xml[0], dat[0]


def ddi(xml):
    """{var: (start0, end0, decimals)} and {var: {code: label}} from DDI."""
    t = open(xml, encoding='utf-8', errors='replace').read()
    lay, labels = {}, {}
    for m in re.finditer(r'<var\b[^>]*\bID="([^"]+)"[^>]*>(.*?)</var>', t, re.S):
        name, body = m.group(1), m.group(2)
        head = m.group(0)[:m.group(0).find('>') + 1]
        loc = re.search(r'<location\b[^>]*StartPos="(\d+)"[^>]*width="(\d+)"', body)
        if not loc:
            continue
        dc = re.search(r'\bdcml="(\d+)"', head)
        s, w = int(loc.group(1)), int(loc.group(2))
        lay[name] = (s - 1, s - 1 + w, int(dc.group(1)) if dc else 0)
        cats = re.findall(r'<catValu>\s*([^<]+?)\s*</catValu>\s*<labl>\s*([^<]*?)\s*</labl>',
                          body, re.S)
        if cats:
            labels[name] = dict(cats)
    return lay, labels


def inspect(d):
    xml, dat = ddi_files(d)
    lay, labels = ddi(xml)
    print('xml', xml, '\ndat', dat, os.path.getsize(dat), 'bytes')
    reps = sorted(v for v in lay if re.fullmatch(r'REPWTP\d+', v))
    print('variables:', len(lay), ' replicate weights:', len(reps))
    for v in BASE + OPTIONAL:
        print('  %-10s %s' % (v, lay.get(v, 'ABSENT')))
    for v in LABELLED:
        lab = labels.get(v, {})
        items = list(lab.items())
        print('\n%s: %d labelled codes' % (v, len(items)))
        for k, l in items[:40]:
            print('    %s = %s' % (k, l))
    with gzip.open(dat, 'rt', errors='replace') as f:
        for i, line in enumerate(f):
            if i >= 2:
                break
            print('\nrecord %d length %d:' % (i, len(line.rstrip('\n'))),
                  {v: line[a:b] for v, (a, b, _) in lay.items() if v in BASE + OPTIONAL})


def val(line, lay, v):
    a, b, dc = lay[v]
    s = line[a:b].strip()
    if not s:
        return None
    x = float(s) if ('.' in s or dc) else int(s)
    return x / (10 ** dc) if dc and '.' not in s else x


def weighted_median(pairs):
    pairs.sort()
    tot = sum(w for _, w in pairs)
    c = 0.0
    for h, w in pairs:
        c += w
        if c >= tot / 2:
            return h
    return float('nan')


def run(d, out_path):
    xml, dat = ddi_files(d)
    lay, labels = ddi(xml)
    missing = [v for v in BASE if v not in lay]
    reps = ['REPWTP%d' % i for i in range(1, N_REP + 1)]
    missing += [v for v in reps if v not in lay]
    if missing:
        raise SystemExit('DDI lacks %s' % missing)
    has_adjust = 'ADJUST' in lay
    print('ADJUST in extract:', has_adjust, flush=True)

    # IPUMS SAMPLE codes: 201901 = 2019 ACS, 201903 = 2015-2019 ACS 5-year
    # (2018, 2019 records only -- see KEEP_YEARS).
    samp = {201901: 'acs1_2019', 201903: 'acs5_2018_2019'}
    lab = labels.get('SAMPLE', {})
    for code in samp:
        print('sample %d = %s' % (code, lab.get(str(code), 'UNLABELLED')), flush=True)

    # key -> [sum_w_h, sum_w, n] for main weight + each replicate
    S = defaultdict(lambda: [[0.0] * (N_REP + 1), [0.0] * (N_REP + 1), 0])
    M = defaultdict(list)          # key -> [(hourly, perwt)] for the median
    DIAG = defaultdict(lambda: [0.0, 0.0, 0, 0])   # (sample, year) check
    ia, ib = lay['IND'][:2]
    oa, ob = lay['OCC'][:2]
    kept = dropped = 0
    drop = defaultdict(int)
    with gzip.open(dat, 'rt', errors='replace') as f:
        for nrow, line in enumerate(f, 1):
            if nrow % 2000000 == 0:
                print('  rows %d kept %d' % (nrow, kept), flush=True)
            ind = line[ia:ib].strip()
            occ = line[oa:ob].strip()
            ind = int(ind) if ind else 0
            occ = int(occ) if occ else 0
            if ind not in IND_CHILDCARE and occ not in OCC_CHILDCARE:
                continue
            if val(line, lay, 'AGE') < 16 or val(line, lay, 'GQ') in (3, 4):
                continue
            earn = val(line, lay, 'INCEARN')
            hrs = val(line, lay, 'UHRSWORK')
            w1 = val(line, lay, 'WKSWORK1')
            w2 = val(line, lay, 'WKSWORK2')
            weeks = w1 if (w1 is not None and w1 > 0) else WKS_MID.get(w2)
            if earn is None or earn <= 0:
                drop['earnings <= 0'] += 1; continue
            if not hrs or hrs <= 0 or not weeks:
                drop['no hours or weeks'] += 1; continue
            if has_adjust:
                earn *= val(line, lay, 'ADJUST')
            annual_hours = weeks * hrs
            if annual_hours < 100:
                drop['annual hours < 100'] += 1; continue
            hourly = earn / annual_hours
            if not (2 <= hourly <= 200):
                drop['hourly outside 2-200'] += 1; continue

            scode = val(line, lay, 'SAMPLE')
            sname = samp.get(scode)
            if sname is None:
                drop['unknown sample'] += 1; continue
            my = val(line, lay, 'MULTYEAR')
            if scode == 201903 and my not in KEEP_YEARS:
                drop['5-year record before 2018'] += 1; continue
            st = val(line, lay, 'STATEFIP')
            educ = 'ba' if val(line, lay, 'EDUC') >= 10 else 'no_ba'
            cw = val(line, lay, 'CLASSWKR')
            sector = {1: 'self_employed', 2: 'wage_salary'}.get(cw, 'other')
            occg = OCC_CHILDCARE.get(occ, 'other_in_industry')
            wts = [val(line, lay, 'PERWT')] + [val(line, lay, r) for r in reps]
            kept += 1
            dg = DIAG[(sname, my or 2019)]
            dg[0] += wts[0] * hourly; dg[1] += wts[0]; dg[2] += 1
            dg[3] += 1 if (w1 is not None and w1 > 0) else 0

            for geo in (st, 0):
                for e in ('all', educ):
                    for sec, og in (('all', 'all'), (sector, 'all'), ('all', occg)):
                        k = (sname, geo, e, sec, og)
                        c = S[k]
                        for j, w in enumerate(wts):
                            c[0][j] += w * hourly
                            c[1][j] += w
                        c[2] += 1
                        M[k].append((hourly, wts[0]))

    print('kept %d; dropped %s' % (kept, dict(drop)), flush=True)
    print('by survey year: sample, year, n, mean hourly, share with WKSWORK1 > 0')
    for (sn, y), (swh, sw, n, n1) in sorted(DIAG.items()):
        print('  %-15s %s %7d %8.3f %6.3f' % (sn, y, n, swh / sw, n1 / n))

    def means(k):
        swh, sw, _ = S[k]
        return [swh[j] / sw[j] if sw[j] > 0 else float('nan') for j in range(N_REP + 1)]

    def sdr_se(vals):
        # successive-difference replication: var = 4/80 * sum (r - full)^2
        full = vals[0]
        return math.sqrt(4.0 / N_REP * sum((v - full) ** 2 for v in vals[1:]))

    rows = []
    for k in sorted(S, key=lambda x: (x[0], x[1], x[2], x[3], x[4])):
        sname, geo, e, sec, og = k
        m = means(k)
        nat = (sname, 0, e, sec, og)
        mn = means(nat) if nat in S else None
        if mn:
            r = [a / b for a, b in zip(m, mn)]
            ratio, se_r = r[0], sdr_se(r)
        else:
            ratio = se_r = float('nan')
        rows.append([sname, geo, e, sec, og, S[k][2], round(S[k][1][0], 1),
                     round(m[0], 4), round(sdr_se(m), 4),
                     round(weighted_median(M[k]), 4),
                     round(ratio, 5), round(se_r, 5)])
    with open(out_path, 'w', newline='') as f:
        w = csv.writer(f, lineterminator='\n')
        w.writerow(['sample', 'state_fips', 'educ_group', 'sector', 'occ_group',
                    'n', 'workers_wt', 'mean_hourly', 'se_mean',
                    'median_hourly', 'ratio_to_national', 'se_ratio'])
        w.writerows(rows)
    print('wrote %s, %d rows' % (out_path, len(rows)))


if __name__ == '__main__':
    if len(sys.argv) == 3 and sys.argv[1] == '--inspect':
        inspect(sys.argv[2])
    elif len(sys.argv) == 3:
        run(sys.argv[1], sys.argv[2])
    else:
        raise SystemExit(__doc__)
