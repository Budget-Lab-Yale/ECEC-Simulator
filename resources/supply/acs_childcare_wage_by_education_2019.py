#!/usr/bin/env python3
"""
State childcare wage ratios and labour shares by MEASURED education, IPUMS ACS.

BLS OEWS gives wages by occupation, no education -- an occupation mapping
(39-9011 -> no_ba; 25-2011, 25-2051, 11-9031 -> ba) mixes groups: BLS EP
Table 5.3 (2023-24) has 22.6% of childcare workers BA+, 46.2% of preschool
teachers not. Here education is read off
each worker (EDUC >= 10, 4+ years college = ba), so group = degree, not job.

Reads IPUMS USA extract usa:33 (acs_ipums_extract_2019.R), two samples:
    us2019a   2019 ACS 1-year -- cross-check
    us2019c   2015-2019 ACS 5-year, 2018 + 2019 records only (MULTYEAR) --
              source of state ratios: about 2x 1-year records, no pre-2018
              wages

POPULATION -- ACS occupations of four SOC codes (ACS pools detailed SOC):
    childcare_worker    Childcare workers (39-9011), any industry but
                        private households (IND 9290: nannies, babysitters --
                        simulator's Other Paid, not a center or home provider)
    preschool_teacher   Preschool and kindergarten teachers (25-2010)
    sped_teacher        Special education teachers (25-2050)
    administrator       Education and childcare administrators (11-9030)
Last three pool school-age levels, so main spec keeps them only in child day
care services (IND, NAICS 6244). Spec `all_industries` keeps preschool and
kindergarten teachers in every industry -- adds public-school pre-K, and
kindergarten. Spec `incl_private_households` = main + nannies. OCCSOC under each code printed at run.
Age 16+, household residents (GQ not 3, 4). Hourly = INCEARN / (weeks x usual
hours). Excluded: earnings <= 0, no hours, annual hours < 100, hourly outside
2-200. Weeks = WKSWORK1, else WKSWORK2 interval midpoint.

GEOGRAPHY  50 states + DC (STATEFIP), NYC (STATEFIP 36, COUNTYFIP 5, 47, 61,
81, 85 -- read from config/substate_geographies.csv), national = all.

    python resources/supply/acs_childcare_wage_by_education_2019.py run <usa33_dir> <out.csv>
    python resources/supply/acs_childcare_wage_by_education_2019.py summarise <out.csv>         resources/supply/state_childcare_wage_ratios_acs.csv

`summarise` (laptop) keeps spec main, sample acs5_2018_2019, writes the
schema state_supply_params.R reads.
Cell under MIN_N unweighted workers -> ratio 1 (national wage).

Output long table, one row per spec x sample x geo x educ_group:
  spec, sample, geo, educ_group, n, workers_wt, mean_hourly, se_mean,
  ratio_to_national, se_ratio, worker_share_of_national
plus occupation rows (geo US) giving ba share per occupation: educ_group
'ba_share', mean_hourly = weighted BA+ share -- compare BLS Table 5.3.
SE: successive-difference replication over 80 REPWTP.
"""

import csv
import glob
import gzip
import math
import os
import re
import sys
from collections import defaultdict

N_REP = 80
WKS_MID = {1: 7.0, 2: 20.0, 3: 33.0, 4: 43.5, 5: 48.5, 6: 51.0}
KEEP_YEARS = {2018, 2019}
SAMPLES = {201901: 'acs1_2019', 201903: 'acs5_2018_2019'}
FIPS_TO_USPS = {
    1: 'AL', 2: 'AK', 4: 'AZ', 5: 'AR', 6: 'CA', 8: 'CO', 9: 'CT', 10: 'DE',
    11: 'DC', 12: 'FL', 13: 'GA', 15: 'HI', 16: 'ID', 17: 'IL', 18: 'IN',
    19: 'IA', 20: 'KS', 21: 'KY', 22: 'LA', 23: 'ME', 24: 'MD', 25: 'MA',
    26: 'MI', 27: 'MN', 28: 'MS', 29: 'MO', 30: 'MT', 31: 'NE', 32: 'NV',
    33: 'NH', 34: 'NJ', 35: 'NM', 36: 'NY', 37: 'NC', 38: 'ND', 39: 'OH',
    40: 'OK', 41: 'OR', 42: 'PA', 44: 'RI', 45: 'SC', 46: 'SD', 47: 'TN',
    48: 'TX', 49: 'UT', 50: 'VT', 51: 'VA', 53: 'WA', 54: 'WV', 55: 'WI',
    56: 'WY'}

# ACS occupation codes (2018+ Census OCC) -- DDI carries no OCC labels, so
# fixed here; OCCSOC under each printed at run to confirm
OCC_GROUP = {4600: 'childcare_worker',     # 39-9011
             2300: 'preschool_teacher',    # 25-2010, preschool + kindergarten
             2330: 'sped_teacher',         # 25-2050, all levels
             230:  'administrator'}        # 11-9030, all levels
IND_CHILD_DAY_CARE = {8470}               # NAICS 6244
IND_PRIVATE_HOUSEHOLDS = {9290}           # nannies -- Other Paid, not provider
NEED = ['SAMPLE', 'MULTYEAR', 'STATEFIP', 'COUNTYFIP', 'GQ', 'AGE', 'OCC',
        'IND', 'UHRSWORK', 'WKSWORK1', 'WKSWORK2', 'INCEARN', 'EDUC', 'PERWT']


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


NYC_STATEFIP, NYC_COUNTIES = substate_counties('NYC')


def ddi(d):
    """Layout {var: (a, b, dcml)}, labels {var: {code: label}}, .dat.gz path."""
    xml = sorted(glob.glob(os.path.join(d, '*.xml')))
    dat = sorted(glob.glob(os.path.join(d, '*.dat.gz')))
    if len(xml) != 1 or len(dat) != 1:
        raise SystemExit('want one .xml and one .dat.gz in %s' % d)
    t = open(xml[0], encoding='utf-8', errors='replace').read()
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
            labels[name] = {int(k): v for k, v in cats if k.strip().lstrip('-').isdigit()}
    return lay, labels, dat[0]


def val(line, lay, v):
    a, b, dc = lay[v]
    s = line[a:b].strip()
    if not s:
        return None
    x = float(s) if '.' in s else int(s)
    return x / (10 ** dc) if dc and '.' not in s else x


def sdr_se(vals):
    full = vals[0]
    return math.sqrt(4.0 / N_REP * sum((v - full) ** 2 for v in vals[1:]))


def run(d, out_path):
    lay, labels, dat = ddi(d)
    reps = ['REPWTP%d' % i for i in range(1, N_REP + 1)]
    missing = [v for v in NEED + reps if v not in lay]
    if missing:
        raise SystemExit('DDI lacks %s' % missing)

    occ_group = OCC_GROUP
    ind_cc = IND_CHILD_DAY_CARE
    has_soc = 'OCCSOC' in lay
    SOC = defaultdict(float)
    # key (spec, sample, geo, educ) -> [sum_wh per rep], [sum_w per rep], n
    S = defaultdict(lambda: [[0.0] * (N_REP + 1), [0.0] * (N_REP + 1), 0])
    # occupation BA share, national: (spec, sample, occ) -> [w_ba, w_all, n]
    O = defaultdict(lambda: [0.0, 0.0, 0])
    drop = defaultdict(int)
    oa, ob = lay['OCC'][:2]
    with gzip.open(dat, 'rt', errors='replace') as f:
        for nrow, line in enumerate(f, 1):
            if nrow % 5000000 == 0:
                print('  rows %d' % nrow, flush=True)
            occ = line[oa:ob].strip()
            occ = int(occ) if occ else 0
            g = occ_group.get(occ)
            if g is None:
                continue
            if val(line, lay, 'AGE') < 16 or val(line, lay, 'GQ') in (3, 4):
                continue
            scode = val(line, lay, 'SAMPLE')
            sname = SAMPLES.get(scode)
            if sname is None:
                drop['unknown sample'] += 1; continue
            if scode == 201903 and val(line, lay, 'MULTYEAR') not in KEEP_YEARS:
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
            if weeks * hrs < 100:
                drop['annual hours < 100'] += 1; continue
            hourly = earn / (weeks * hrs)
            if not (2 <= hourly <= 200):
                drop['hourly outside 2-200'] += 1; continue

            if has_soc:
                a, b, _ = lay['OCCSOC']
                SOC[(occ, line[a:b].strip())] += val(line, lay, 'PERWT')
            ind = val(line, lay, 'IND')
            in_cc = ind in ind_cc
            in_ph = ind in IND_PRIVATE_HOUSEHOLDS
            specs = []
            if (g == 'childcare_worker' and not in_ph) or in_cc:
                specs.append('main')
            if g == 'childcare_worker' or in_cc:
                specs.append('incl_private_households')
            if (g == 'childcare_worker' and not in_ph) or g == 'preschool_teacher' or in_cc:
                specs.append('all_industries')
            if not specs:
                continue
            educ = 'ba' if val(line, lay, 'EDUC') >= 10 else 'no_ba'
            st = val(line, lay, 'STATEFIP')
            geos = ['US', FIPS_TO_USPS.get(st, 'ST%d' % st)]
            if st == NYC_STATEFIP and val(line, lay, 'COUNTYFIP') in NYC_COUNTIES:
                geos.append('NYC')
            wts = [val(line, lay, 'PERWT')] + [val(line, lay, r) for r in reps]
            for sp in specs:
                o = O[(sp, sname, g)]
                o[0] += wts[0] if educ == 'ba' else 0.0
                o[1] += wts[0]; o[2] += 1
                for geo in geos:
                    for e in (educ, 'all'):
                        c = S[(sp, sname, geo, e)]
                        for j, w in enumerate(wts):
                            c[0][j] += w * hourly
                            c[1][j] += w
                        c[2] += 1
    print('dropped', dict(drop), flush=True)
    print('OCC x OCCSOC, weighted workers kept:')
    for (o, soc), w in sorted(SOC.items()):
        print('  %5d %-8s %12.0f' % (o, soc, w))

    rows = []
    for k in sorted(S):
        sp, sname, geo, e = k
        swh, sw, n = S[k]
        nat = S[(sp, sname, 'US', e)]
        m = [swh[j] / sw[j] if sw[j] > 0 else float('nan') for j in range(N_REP + 1)]
        mn = [nat[0][j] / nat[1][j] for j in range(N_REP + 1)]
        r = [a / b for a, b in zip(m, mn)]
        rows.append({'spec': sp, 'sample': sname, 'geo': geo, 'educ_group': e,
                     'n': n, 'workers_wt': round(sw[0]),
                     'mean_hourly': round(m[0], 4), 'se_mean': round(sdr_se(m), 4),
                     'ratio_to_national': round(r[0], 4),
                     'se_ratio': round(sdr_se(r), 4),
                     'worker_share_of_national': round(sw[0] / nat[1][0], 6)})
    for (sp, sname, g), (wba, wall, n) in sorted(O.items()):
        rows.append({'spec': sp, 'sample': sname, 'geo': 'US:' + g,
                     'educ_group': 'ba_share', 'n': n, 'workers_wt': round(wall),
                     'mean_hourly': round(wba / wall, 4), 'se_mean': '',
                     'ratio_to_national': '', 'se_ratio': '',
                     'worker_share_of_national': ''})
    with open(out_path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print('wrote %d rows to %s' % (len(rows), out_path), flush=True)


MIN_N = 30


def summarise(long_path, out_path):
    """Long run output -> one row per geo x educ_group, state_supply_params.R schema."""
    rows = [r for r in csv.DictReader(open(long_path, newline=''))
            if r['spec'] == 'main' and r['sample'] == 'acs5_2018_2019'
            and r['educ_group'] in ('no_ba', 'ba')]
    nat = {r['educ_group']: r for r in rows if r['geo'] == 'US'}
    out, thin = [], []
    for r in rows:
        if r['geo'] == 'US':
            continue
        n = int(r['n'])
        ratio = float(r['ratio_to_national'])
        src = 'acs'
        if n < MIN_N:
            ratio, src = 1.0, 'acs_thin_national'
            thin.append('%s/%s n=%d' % (r['geo'], r['educ_group'], n))
        e = r['educ_group']
        out.append({'state': r['geo'], 'educ_group': e,
                    'workers': r['workers_wt'], 'n': n,
                    'wage': r['mean_hourly'],
                    'natl_wage': nat[e]['mean_hourly'],
                    'natl_workers': nat[e]['workers_wt'],
                    'wage_ratio': round(ratio, 4), 'se_ratio': r['se_ratio'],
                    'source': src,
                    'emp_share': r['worker_share_of_national']})
    out.sort(key=lambda x: (x['state'], x['educ_group'] != 'no_ba'))
    with open(out_path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(out[0]))
        w.writeheader()
        w.writerows(out)
    print('wrote %d rows to %s; %d cells under n=%d -> national: %s'
          % (len(out), out_path, len(thin), MIN_N, ', '.join(thin) or 'none'))


if __name__ == '__main__':
    if len(sys.argv) != 4 or sys.argv[1] not in ('run', 'summarise'):
        raise SystemExit(__doc__)
    (run if sys.argv[1] == 'run' else summarise)(sys.argv[2], sys.argv[3])
