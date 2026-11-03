#!/usr/bin/env python3
"""
Observed care-worker hours by state, IPUMS ACS -- cluster half of observed
staffing diagnostic (analyze_observed_staffing.R reads output). Diagnostic
only, nothing simulator or build reads.

POPULATION -- same as acs_childcare_occupation_weights_2019.py teaching staff:
household residents (GQ 1, 2), positive earnings + usual hours, ACS occupations
    4600  childcare_worker    any industry but private households (IND 9290)
    2300  preschool_teacher   child day care services only (IND 8470)
Administrators, special-education teachers dropped -- NSECE staff ratios count
teachers + assistants (roles 1-2) only.

SECTOR -- CLASSWKR 1 (self-employed) = home, 2 (works for wages) = centre.
Rough split: employees of homes count as centre, self-employed centre owners
as home.

HOURS -- annual = UHRSWORK x weeks, weeks = WKSWORK2 interval midpoint (ACS
reports intervals only through 2018). 2015-2019 1-year samples pooled (shared
acs_common_v2 us2015a..us2019a = 5-year sample), PERWT / number of years --
totals are one average year.

GEOGRAPHY -- place of work (PWSTATE2, PWCOUNTY) -- staff counted where they
work, e.g. DC staff living in MD, VA; residence (STATEFIP, COUNTYFIP) where
work state missing. NYC also as own geo (county 5, 47, 61, 81, 85 in NY),
inside NY too.

    python state_data/code/analyze_observed_staffing_acs.py <out.csv> <year_dir> [<year_dir> ...]

WAGE -- hourly = (INCWAGE + INCBUS00) / annual hours, CPI-U to 2019 dollars,
kept in $2-$100; wage_mean = person-weighted mean (NSECE wages are
worker-weighted means), wage_wt = weight behind it.

Output: geo (state FIPS, or NYC), sector, educ_group, n (unweighted records),
workers_wt, hours_wt (person-weighted annual hours), wage_wt, wage_mean.
"""

import csv
import gzip
import os
import sys
from collections import defaultdict

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'resources', 'supply'))
from acs_childcare_occupation_weights_2019 import ddi, val  # noqa: E402

HH_GQ = {1, 2}
TOPCODE_NA = 999999
OCC_CHILDCARE = 4600
OCC_PRESCHOOL = 2300
IND_CHILD_DAY_CARE = 8470
IND_PRIVATE_HOUSEHOLDS = 9290
# WKSWORK2 interval -> midpoint weeks
WEEKS_MID = {1: 7.0, 2: 20.0, 3: 33.0, 4: 43.5, 5: 48.5, 6: 51.0}
SECTOR = {1: 'home', 2: 'centre'}
NYC_COUNTIES = {5, 47, 61, 81, 85}
# 50 states + DC FIPS (PWSTATE2 0 = N/A, 72+ = PR / abroad / not identified)
STATE_FIPS = {1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21,
              22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
              38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56}
NEED = ['YEAR', 'GQ', 'STATEFIP', 'COUNTYFIP', 'PWSTATE2', 'PWCOUNTY', 'OCC', 'IND', 'CLASSWKR',
        'UHRSWORK', 'WKSWORK2', 'INCWAGE', 'INCBUS00', 'EDUC', 'PERWT']
# Hourly wage = earnings / annual hours, kept in [WAGE_MIN, WAGE_MAX] (2019 $);
# CPI-U annual averages -> 2019 dollars
WAGE_MIN, WAGE_MAX = 2.0, 100.0
CPI = {2015: 237.017, 2016: 240.007, 2017: 245.120, 2018: 251.107, 2019: 255.657}
DEFLATE = {y: CPI[2019] / c for y, c in CPI.items()}


def read_year(d, N, W, H, WW, WS, scale):
    """Adds one ACS year's care-worker records to N, W, H, wage sums WW, WS; weights x scale."""
    lay, dat = ddi(d)
    missing = [v for v in NEED if v not in lay]
    if missing:
        raise SystemExit('DDI %s lacks %s' % (d, missing))
    oa, ob = lay['OCC'][:2]
    with gzip.open(dat, 'rt', errors='replace') as f:
        for nrow, line in enumerate(f, 1):
            if nrow % 1000000 == 0:
                print('  rows %d' % nrow, flush=True)
            occ = line[oa:ob].strip()
            occ = int(occ) if occ else 0
            if occ not in (OCC_CHILDCARE, OCC_PRESCHOOL):
                continue
            if val(line, lay, 'GQ') not in HH_GQ:
                continue
            ind = val(line, lay, 'IND')
            if occ == OCC_CHILDCARE and ind == IND_PRIVATE_HOUSEHOLDS:
                continue
            if occ == OCC_PRESCHOOL and ind != IND_CHILD_DAY_CARE:
                continue
            sector = SECTOR.get(val(line, lay, 'CLASSWKR'))
            if sector is None:
                continue
            wage = val(line, lay, 'INCWAGE') or 0
            bus = val(line, lay, 'INCBUS00') or 0
            earn = (0 if wage == TOPCODE_NA else wage) + (0 if bus == TOPCODE_NA else bus)
            hrs = val(line, lay, 'UHRSWORK')
            wks = WEEKS_MID.get(val(line, lay, 'WKSWORK2'))
            if earn <= 0 or not hrs or hrs <= 0 or wks is None:
                continue
            educ = 'ba' if val(line, lay, 'EDUC') >= 10 else 'no_ba'
            # Place of work; residence where work state missing / abroad
            st, co = val(line, lay, 'PWSTATE2'), val(line, lay, 'PWCOUNTY')
            if st not in STATE_FIPS:
                st, co = val(line, lay, 'STATEFIP'), val(line, lay, 'COUNTYFIP')
            geos = [str(st)]
            if st == 36 and co in NYC_COUNTIES:
                geos.append('NYC')
            w = val(line, lay, 'PERWT') * scale
            year = val(line, lay, 'YEAR')
            hourly = earn / (hrs * wks) * DEFLATE[year]
            ok_wage = WAGE_MIN <= hourly <= WAGE_MAX
            for g in geos:
                k = (g, sector, educ)
                N[k] += 1
                W[k] += w
                H[k] += w * hrs * wks
                if ok_wage:
                    WW[k] += w
                    WS[k] += w * hourly


def main(out_path, dirs):
    N = defaultdict(int)
    W = defaultdict(float)
    H = defaultdict(float)
    WW = defaultdict(float)
    WS = defaultdict(float)
    for d in dirs:
        print('reading %s' % d, flush=True)
        read_year(d, N, W, H, WW, WS, 1.0 / len(dirs))

    rows = [{'geo': k[0], 'sector': k[1], 'educ_group': k[2], 'n': N[k],
             'workers_wt': round(W[k]), 'hours_wt': round(H[k]),
             'wage_wt': round(WW[k]), 'wage_mean': round(WS[k] / WW[k], 4) if WW[k] > 0 else ''}
            for k in sorted(N)]
    geos = {r['geo'] for r in rows}
    if len(geos) != 52:
        raise SystemExit('want 51 states + DC + NYC, got %d' % len(geos))
    with open(out_path, 'w', newline='') as f:
        wr = csv.DictWriter(f, fieldnames=list(rows[0]))
        wr.writeheader()
        wr.writerows(rows)
    print('wrote %s (%d rows)' % (out_path, len(rows)))


if __name__ == '__main__':
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    main(sys.argv[1], sys.argv[2:])
