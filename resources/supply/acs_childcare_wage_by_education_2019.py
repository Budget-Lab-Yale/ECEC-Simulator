#!/usr/bin/env python3
"""
State childcare wage ratios and labour shares by MEASURED education, IPUMS ACS.

BLS OEWS gives wages by occupation, no education -- an occupation mapping
(39-9011 -> no_ba; 25-2011, 25-2051, 11-9031 -> ba) mixes groups: BLS EP
Table 5.3 (2023-24) has 22.6% of childcare workers BA+, 46.2% of preschool
teachers not. Here education is read off each worker (EDUC >= 10, 4+ years
college = ba), so group = degree, not job.

Reads IPUMS USA extract usa:34 (acs_ipums_extract_2019.R), one sample:
    us2019c   2015-2019 ACS 5-year, 2018 + 2019 records (MULTYEAR), 2019 dollars
Extract also holds us2020c (2016-2020 file); skipped -- one sample, one ratio,
whole record counts, no 2020 pandemic-year records (2026-10-06).
Ratio = weighted MEDIAN hourly earnings, local / national. Median, not mean: a
few high-weight records swing small cells.

POPULATION -- ACS occupations of four SOC codes (ACS pools detailed SOC):
    childcare_worker    Childcare workers (39-9011), any industry but
                        private households (IND 9290: nannies, babysitters --
                        simulator's Other Paid, not a center or home provider)
    preschool_teacher   Preschool and kindergarten teachers (25-2010)
    sped_teacher        Special education teachers (25-2050)
    administrator       Education and childcare administrators (11-9030)
Last three pool school-age levels, so kept only in child day care services
(IND 8470, NAICS 6244).

EARNINGS -- same rule as simulator (src/1_processing/1b_acs_processing.R):
household residents (GQ 1, 2); earnings = INCWAGE + INCBUS00 (999999 -> 0);
hourly = earnings / (weeks x UHRSWORK); kept when earnings, weeks, hours > 0.
Weeks = WKSWORK1 (continuous). Records w/ interval weeks only (2018 records,
WKSWORK2) take weighted mean WKSWORK1 of workers in same interval among
records w/ both -- pass 1.

SMALL CELLS -- no national fallback. Cell w/ n >= MIN_N keeps own childcare
ratio; smaller cell takes state's all-worker median hourly ratio (every
occupation, same education group, large samples). n = unweighted records.
2026-10-06: 9 of 104 cells below MIN_N, all BA (AK, DC, DE, ID, MT, ND, NM,
WV, WY); no no-BA cell.

GEOGRAPHY  50 states + DC (STATEFIP), NYC (STATEFIP 36, COUNTYFIP 5, 47, 61,
81, 85 -- read from config/substate_geographies.csv), national = all.

    python resources/supply/acs_childcare_wage_by_education_2019.py run <usa34_dir> <out.csv>
    python resources/supply/acs_childcare_wage_by_education_2019.py summarise <out.csv> resources/supply/state_childcare_wage_ratios_acs.csv

Long output, one row per group x sample x geo x educ_group: group
(childcare | all_workers), sample, geo, educ_group, n, workers_wt,
median_hourly, ratio_to_national, worker_share_of_national.
"""

import csv
import glob
import gzip
import os
import re
import sys
from collections import defaultdict

SAMPLES = {201903: 'acs5_2018_2019'}
# 5-year sample -> MULTYEAR records kept
KEEP_YEARS = {201903: {2018, 2019}}
RATIO_SAMPLE = 'acs5_2018_2019'
HH_GQ = {1, 2}             # household residents, as simulator
TOPCODE_NA = 999999        # IPUMS N/A code for INCWAGE, INCBUS00
BIN = 0.10                 # all-worker median: $0.10 bins
MAX_BIN = 2000             # $200; higher hourly -> top bin (median unaffected)
MIN_N = 30                 # fewer records -> all-worker ratio replaces own ratio
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
# fixed here
OCC_GROUP = {4600: 'childcare_worker',     # 39-9011
             2300: 'preschool_teacher',    # 25-2010, preschool + kindergarten
             2330: 'sped_teacher',         # 25-2050, all levels
             230:  'administrator'}        # 11-9030, all levels
IND_CHILD_DAY_CARE = {8470}               # NAICS 6244
IND_PRIVATE_HOUSEHOLDS = {9290}           # nannies -- Other Paid, not provider
NEED = ['SAMPLE', 'MULTYEAR', 'STATEFIP', 'COUNTYFIP', 'GQ', 'OCC', 'IND',
        'UHRSWORK', 'WKSWORK1', 'WKSWORK2', 'INCWAGE', 'INCBUS00', 'EDUC', 'PERWT']


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


def weighted_median(recs):
    """Weighted median of [(value, weight)] -- first value where cumulative
    weight reaches half the total."""
    if not recs:
        return float('nan')
    recs = sorted(recs)
    half = sum(w for _, w in recs) / 2.0
    cum = 0.0
    for v, w in recs:
        cum += w
        if cum >= half:
            return v
    return recs[-1][0]


def binned_median(h):
    """Weighted median from $BIN histogram (bin midpoint)."""
    half = sum(h) / 2.0
    cum = 0.0
    for i, w in enumerate(h):
        cum += w
        if cum >= half:
            return (i + 0.5) * BIN
    return (len(h) - 0.5) * BIN


def records(dat, lay):
    """Kept household-resident records w/ positive earnings and hours:
    yields (sname, line, earnings, hours, wks1, wks2, weight)."""
    with gzip.open(dat, 'rt', errors='replace') as f:
        for nrow, line in enumerate(f, 1):
            if nrow % 5000000 == 0:
                print('  rows %d' % nrow, flush=True)
            if val(line, lay, 'GQ') not in HH_GQ:
                continue
            scode = val(line, lay, 'SAMPLE')
            sname = SAMPLES.get(scode)
            if sname is None or val(line, lay, 'MULTYEAR') not in KEEP_YEARS[scode]:
                continue
            wage = val(line, lay, 'INCWAGE') or 0
            bus = val(line, lay, 'INCBUS00') or 0
            earn = (0 if wage == TOPCODE_NA else wage) + (0 if bus == TOPCODE_NA else bus)
            hrs = val(line, lay, 'UHRSWORK')
            if earn <= 0 or not hrs or hrs <= 0:
                continue
            yield (sname, line, earn, hrs, val(line, lay, 'WKSWORK1'),
                   val(line, lay, 'WKSWORK2'), val(line, lay, 'PERWT'))


def run(d, out_path):
    lay, dat = ddi(d)
    missing = [v for v in NEED if v not in lay]
    if missing:
        raise SystemExit('DDI lacks %s' % missing)

    # Pass 1 -- weighted mean continuous weeks per interval, records w/ both
    wk = defaultdict(lambda: [0.0, 0.0])
    for _, _, _, _, w1, w2, w in records(dat, lay):
        if w1 and w1 > 0 and w2:
            wk[w2][0] += w * w1
            wk[w2][1] += w
    mean_wks = {k: s / t for k, (s, t) in wk.items() if t > 0}
    print('mean WKSWORK1 by WKSWORK2 interval:',
          {k: round(v, 2) for k, v in sorted(mean_wks.items())}, flush=True)

    # Pass 2 -- hourly earnings; childcare exact, all workers binned
    S = defaultdict(list)                        # childcare: [(hourly, w)]
    H = defaultdict(lambda: [0.0] * MAX_BIN)      # all workers: histogram
    N = defaultdict(int)
    n_interval = 0
    oa, ob = lay['OCC'][:2]
    for sname, line, earn, hrs, w1, w2, w in records(dat, lay):
        if w1 and w1 > 0:
            weeks = w1
        else:
            weeks = mean_wks.get(w2)
            n_interval += 1
        if not weeks:
            continue
        hourly = earn / (weeks * hrs)
        educ = 'ba' if val(line, lay, 'EDUC') >= 10 else 'no_ba'
        st = val(line, lay, 'STATEFIP')
        geos = ['US', FIPS_TO_USPS.get(st, 'ST%d' % st)]
        if st == NYC_STATEFIP and val(line, lay, 'COUNTYFIP') in NYC_COUNTIES:
            geos.append('NYC')
        b = min(int(hourly / BIN), MAX_BIN - 1)
        for geo in geos:
            H[(sname, geo, educ)][b] += w
            N[(sname, geo, educ)] += 1

        occ = line[oa:ob].strip()
        g = OCC_GROUP.get(int(occ) if occ else 0)
        if g is None:
            continue
        ind = val(line, lay, 'IND')
        if not ((g == 'childcare_worker' and ind not in IND_PRIVATE_HOUSEHOLDS)
                or ind in IND_CHILD_DAY_CARE):
            continue
        for geo in geos:
            S[(sname, geo, educ)].append((hourly, w))
    print('records w/ interval weeks only: %d' % n_interval, flush=True)

    rows = []
    for k in sorted(S):
        sname, geo, e = k
        recs, nat = S[k], S[(sname, 'US', e)]
        m, mn = weighted_median(recs), weighted_median(nat)
        rows.append({'group': 'childcare', 'sample': sname, 'geo': geo, 'educ_group': e,
                     'n': len(recs), 'workers_wt': round(sum(w for _, w in recs)),
                     'median_hourly': round(m, 4), 'ratio_to_national': round(m / mn, 6),
                     'worker_share_of_national':
                         round(sum(w for _, w in recs) / sum(w for _, w in nat), 6)})
    for k in sorted(H):
        sname, geo, e = k
        m, mn = binned_median(H[k]), binned_median(H[(sname, 'US', e)])
        rows.append({'group': 'all_workers', 'sample': sname, 'geo': geo, 'educ_group': e,
                     'n': N[k], 'workers_wt': round(sum(H[k])),
                     'median_hourly': round(m, 4), 'ratio_to_national': round(m / mn, 6),
                     'worker_share_of_national': ''})
    with open(out_path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print('wrote %d rows to %s' % (len(rows), out_path), flush=True)


def summarise(long_path, out_path):
    """Long run output -> one row per geo x educ_group, state_supply_params.R
    schema, RATIO_SAMPLE only. Childcare ratio = own, or all-worker ratio
    below MIN_N (header, SMALL CELLS); wage, natl_wage in 2019 dollars."""
    rows = [r for r in csv.DictReader(open(long_path, newline=''))
            if r['sample'] == RATIO_SAMPLE and r['educ_group'] in ('no_ba', 'ba')]
    by = {(r['group'], r['geo'], r['educ_group']): r for r in rows}

    def row(group, geo, e):
        if (group, geo, e) not in by:
            raise SystemExit('%s %s/%s: missing from %s' % (group, geo, e, RATIO_SAMPLE))
        return by[(group, geo, e)]

    geos = sorted({g for (grp, g, _) in by if grp == 'childcare' and g != 'US'})
    out = []
    for e in ('no_ba', 'ba'):
        small = [g for g in geos if int(row('childcare', g, e)['n']) < MIN_N]
        print('%s: %d cells < %d records take all-worker ratio: %s'
              % (e, len(small), MIN_N, ' '.join(small) or '-'))
        nat = row('childcare', 'US', e)
        for g in geos:
            cc = row('childcare', g, e)
            own = float(cc['ratio_to_national'])
            allw = float(row('all_workers', g, e)['ratio_to_national'])
            n = int(cc['n'])
            ratio = own if n >= MIN_N else allw
            out.append({'state': g, 'educ_group': e,
                        'workers': cc['workers_wt'], 'n': n,
                        'wage': cc['median_hourly'],
                        'natl_wage': nat['median_hourly'],
                        'natl_workers': nat['workers_wt'],
                        'own_ratio': round(own, 4), 'all_worker_ratio': round(allw, 4),
                        'wage_ratio': round(ratio, 4),
                        'emp_share': round(float(cc['worker_share_of_national']), 6)})
    out.sort(key=lambda x: (x['state'], x['educ_group'] != 'no_ba'))
    with open(out_path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(out[0]))
        w.writeheader()
        w.writerows(out)
    print('wrote %d rows to %s' % (len(out), out_path))


if __name__ == '__main__':
    if len(sys.argv) != 4 or sys.argv[1] not in ('run', 'summarise'):
        raise SystemExit(__doc__)
    (run if sys.argv[1] == 'run' else summarise)(sys.argv[2], sys.argv[3])
