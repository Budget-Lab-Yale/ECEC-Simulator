"""
10_fix_evidence.py -- evidence behind the fixes in the crosswalk's mismatch list, both years

Reports (weighted figures use the survey weights; counts are unweighted):
  E. Centers by family-pay answer (B1_3_FAMPAY: pay, none pay, don't know), interviewed
     centers: public-school flag, Head Start and public pre-K funding, tuition revenue,
     for-profit status, subsidy receipt, and enrollment 0-5. Do don't-know centers look
     free or paying?
  F. Every posted weekly rate above $2,000 ($50 per hour at 40 hours): the rate, the same
     center's other age rates, family-pay answer, enrollment, weight, and the rate divided
     by 4.345 / 52 (what it would be if it were a monthly or yearly amount).
  G. Price sector of public-school programs where families pay (interviewed centers with
     METH_PUBSCHFLAG = 1 and a positive rate), against 1a's age-bin weighted medians:
     weighted FTE share Low and High, and mean hourly price. 2019 admin centers with a
     rate are shown too. This is what an imputed price for admin centers would look like.
  H. Parents of children under 5 who said they worked last week (D1A = 1): those with
     calendar hours (the donors 1a draws from) against those with no hours record (2024
     code -10 and other negative codes; filled by option 'hours') and those with zero
     hours (filled by the base rule). Pay unit, hourly wage, school or training, and the
     donor mean hours by pay unit, to see whether an unconditional draw fits.
  I. Priced enrollment lost to missing rates: interviewed centers where families pay, each
     1a age-bin cell with FTE > 0 classed priced (a positive rate), trimmed (code -2, or a
     2024 cap value), no time unit (code -1), or no rate / other. Weighted FTE and share
     by class. 2024 also detects the top/bottom-code caps per rate age and repeats the
     shares with cap-valued rates deleted.
  J. 2024 only. Free admin centers (data source 2, no family pays): NORC-imputed against
     reported total staff counts (CB9_ADMIN_STAFF_IMP). Staff per FTE child and per child,
     composition, ratio by FTE quartile, imputed-center staff totals under NORC / flat /
     cell / log-log ratios, and the same restricted to the reported centers' FTE range.
     J6 checks region: reported ratio by region and FTE half, log-log WLS with region
     dummies (and full-time share), NORC's per-child ratio by region.

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/10_fix_evidence.py [--raw-root PATH] [--out FILE]

Runtime: about a minute. Python 3.9+, standard library only.
"""
import argparse
import collections
import contextlib
import csv
import math
import statistics
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {
    '2019': dict(cb='2019/37941-0006-Data.tsv', hh='2019/37941-0009-Data.tsv', d='9', dk=3),
    '2024': dict(cb='2024/39466-0002-Data.tsv', hh='2024/39466-0005-Data.tsv', d='4', dk=-1),
}
AGES = ['INF', '1YR', '2YR', '3YR', '4YR', '5YR']
RATE_AGES = ['INF', '2YR', '3YR', '4YR']
RATE_BIN = {'INF': '0-1', '2YR': '2-3', '3YR': '2-3', '4YR': '4-5'}
RATE_ENRL_AGES = {'INF': ['INF', '1YR'], '2YR': ['2YR'], '3YR': ['3YR'], '4YR': ['4YR', '5YR']}   # as in 09
BIN_AGES = {'0-1': ['INF', '1YR'], '2-3': ['2YR', '3YR'], '4-5': ['4YR', '5YR']}
PAY_UNIT = {1: 'hour', 2: 'day', 3: 'week', 4: 'two weeks', 5: 'month', 6: 'year'}
PAY_HOURS = {1: 1, 2: 8, 3: 40, 4: 80, 5: 2000 / 12, 6: 2080}   # 1a's conversion to hourly
TRIM_CODE, NO_UNIT_CODE = -2, -1   # rate codes: trimmed outlier, no time unit
WEEKS_PER_MONTH = 4.34524           # monthly-looking rate: weekly x this within 0.05 of whole dollars
CLASSES = ['priced', 'trimmed', 'no time unit', 'no rate / other']


def num(x):
    try:
        return float(x.strip('"'))
    except ValueError:
        return None


def load(raw_root, path, d, prefixes):
    """Rows as dicts with 2019-style names (year digit 9), only columns with the given prefixes."""
    csv.field_size_limit(10**9)
    with open(f'{raw_root}/{path}') as fh:
        r = csv.reader(fh, delimiter='\t')
        header = [h.strip('"') for h in next(r)]
        names = []
        for h in header:
            for p in ('HHC', 'HH', 'CB'):
                if h.startswith(f'{p}{d}_'):
                    h = f'{p}9_' + h[len(p) + 2:]
                    break
            names.append(h)
        keep = [(i, n) for i, n in enumerate(names) if n.startswith(prefixes)]
        return [{n: num(row[i]) for i, n in keep} for row in r]


def wmean(pairs):
    pairs = [(x, w) for x, w in pairs if x is not None and w]
    s = sum(w for _, w in pairs)
    return sum(x * w for x, w in pairs) / s if s else None


def wmedian(pairs):
    pairs = sorted((x, w) for x, w in pairs if x is not None and w and w > 0)
    half, acc = sum(w for _, w in pairs) / 2, 0.0
    for x, w in pairs:
        acc += w
        if acc >= half:
            return x


def fte_bins(c):
    """FTE enrollment by 1a age bin (full time 1, part time 0.5; unknown share counts 0 as 1a does)."""
    out = {}
    for b, ages in BIN_AGES.items():
        e = sum(max(c.get(f'CB9_ENRL_{a}') or 0, 0) for a in ages)
        fts = [c.get(f'CB9_ENRL_FT_{a}') for a in ages]
        fts = [f for f in fts if f is not None]
        s = 0 if all(f < 0 for f in fts) else statistics.mean([f for f in fts if f >= 0]) / 100
        out[b] = e * s + 0.5 * e * (1 - s)
    return out


def bin_prices(c):
    """Hourly posted price by bin, 1a rule (FREE -> 0, NORATE/MISS -> none, mean within bin)."""
    out = {}
    for b in BIN_AGES:
        ps = []
        for a in [x for x in RATE_AGES if RATE_BIN[x] == b]:
            flag, rate = c.get(f'CB9_B1_3A_AGERATE_{a}_R'), c.get(f'CB9_STDRATE_WKLY_{a}')
            if flag == -8:
                ps.append(0.0)
            elif flag == 2 or rate is None or rate < 0:
                continue
            else:
                ps.append(rate / 40)
        out[b] = statistics.mean(ps) if ps else None
    return out


def pctile(xs, p):
    """Sorted-index percentile, no interpolation: element int(p * (n - 1)) of sorted values."""
    xs = sorted(xs)
    return xs[int(p * (len(xs) - 1))]


def ratio(xs):
    """Weighted staff per FTE child."""
    f = sum(x['w'] * x['fte'] for x in xs)
    return sum(x['w'] * x['staff'] for x in xs) / f if f else None


def detect_caps(cb):
    """2024 caps per rate age (rule of R evidence run): positive rates of interviewed centers in cents,
    most frequent value below q15 / above q85, non-monthly then monthly-looking values."""
    caps, shares = {}, {}
    for a in RATE_AGES:
        vs = [round(c[f'CB9_STDRATE_WKLY_{a}'], 2) for c in cb
              if c['CB9_METH_DATA_SOURCE'] == 1 and (c.get(f'CB9_STDRATE_WKLY_{a}') or 0) > 0]
        q15, q85 = pctile(vs, 0.15), pctile(vs, 0.85)
        caps[a] = []
        for monthly in (False, True):
            m = [v for v in vs if (abs(v * WEEKS_PER_MONTH - round(v * WEEKS_PER_MONTH)) < 0.05) == monthly]
            for side in (lambda v: v < q15, lambda v: v > q85):
                cnt = collections.Counter(v for v in m if side(v))
                caps[a].append(cnt.most_common(1)[0][0] if cnt else None)
        shares[a] = (sum(v in caps[a] for v in vs) / len(vs), q15, q85, len(vs))
    return caps, shares


def wls(X, y, w):
    """Weighted least squares coefficients: normal equations, Gaussian elimination w/ partial pivoting."""
    k = len(X[0])
    A = [[sum(wi * xi[r] * xi[c] for xi, wi in zip(X, w)) for c in range(k)]
         + [sum(wi * xi[r] * yi for xi, yi, wi in zip(X, y, w))] for r in range(k)]
    for c in range(k):
        p = max(range(c, k), key=lambda r: abs(A[r][c]))
        A[c], A[p] = A[p], A[c]
        for r in range(k):
            if r != c:
                f = A[r][c] / A[c][c]
                A[r] = [a - f * b for a, b in zip(A[r], A[c])]
    return [A[r][k] / A[r][r] for r in range(k)]


def rate_class(rates, caps=None):
    """Class of an age-bin cell from its (age, rate) pairs; with caps, cap-valued rates count as deleted."""
    is_cap = lambda a, r: bool(caps) and round(r, 2) in caps[a]
    pos = [r for a, r in rates if r is not None and r > 0 and not is_cap(a, r)]
    capped = [r for a, r in rates if r is not None and r > 0 and is_cap(a, r)]
    codes = [r for a, r in rates if r is not None and r <= 0]
    if pos:
        return 'priced'
    if TRIM_CODE in codes or capped:
        return 'trimmed'
    if NO_UNIT_CODE in codes:
        return 'no time unit'
    return 'no rate / other'


def admin_ft_share(c):
    """Full-time share s from ADMIN_ care items, 1a rule (full only 1, part only 0, else 0.5)."""
    full, part = c.get('CB9_ADMIN_FULLTIME_CARE'), c.get('CB9_ADMIN_PARTTIME_CARE')
    return 1.0 if full == 1 and part == 2 else 0.0 if full == 2 and part == 1 else 0.5


def admin_fte(c):
    """FTE under 6 for admin records, 1a rule: by age where file has it, else total 0-5 x (s + 0.5(1 - s))."""
    by_age = 0.0
    for a in AGES:
        e, ft = c.get(f'CB9_ENRL_{a}'), c.get(f'CB9_ENRL_FT_{a}')
        e = e if e is not None and e > 0 else 0
        s = ft / 100 if ft is not None and ft >= 0 else 0
        by_age += e * (s + 0.5 * (1 - s))
    s, total = admin_ft_share(c), c.get('CB9_ENRL_0TO5')
    if by_age > 0:
        return by_age
    return total * (s + 0.5 * (1 - s)) if total is not None and total > 0 else 0.0


def section_e(cb, dk):
    print('\n  E. Interviewed centers by family-pay answer (center weights)')
    groups = {1: 'families pay', 2: 'none pay', dk: "don't know"}
    items = [('public-school flag', 'CB9_METH_PUBSCHFLAG', lambda v: v == 1),
             ('Head Start funded children', 'CB9_F15_CHILD_FUND_HS_IND', lambda v: v == 1),
             ('public pre-K funded children', 'CB9_F16_CHILD_FUND_PK_IND', lambda v: v == 1),
             ('revenue from parent tuition', 'CB9_RVNU_CHRG', lambda v: v == 1),
             ('for profit', 'CB9_A8_PROFIT_R2', lambda v: v == 1),
             ('subsidy children past year', 'CB9_RVNU_SUBS_PAST_YR', lambda v: v == 1)]
    rows = [c for c in cb if c['CB9_METH_DATA_SOURCE'] == 1]
    print(f'    {"":30s}' + ''.join(f'{g:>16s}' for g in groups.values()))
    print(f'    {"centers":30s}' + ''.join(f'{sum(c.get("CB9_B1_3_FAMPAY") == k for c in rows):16d}' for k in groups))
    for lab, col, f in items:
        cells = []
        for k in groups:
            g = [c for c in rows if c.get('CB9_B1_3_FAMPAY') == k and c.get(col) is not None and c[col] >= 0]
            w = sum(c['CB9_METH_WEIGHT'] for c in g)
            cells.append(f'{sum(c["CB9_METH_WEIGHT"] for c in g if f(c[col])) / w:16.1%}' if w else f'{"--":>16s}')
        print(f'    {lab:30s}' + ''.join(cells))
    cells = []
    for k in groups:
        g = [c for c in rows if c.get('CB9_B1_3_FAMPAY') == k]
        cells.append(f'{wmedian([(sum(max(c.get(f"CB9_ENRL_{a}") or 0, 0) for a in AGES), c["CB9_METH_WEIGHT"]) for c in g]) or 0:16.0f}')
    print(f'    {"median enrollment 0-5":30s}' + ''.join(cells))
    print('    (shares among centers with a valid answer to each item; 1 = yes)')


def section_f(cb):
    print('\n  F. Posted weekly rates above $2,000 (cells with enrollment in the age group)')
    print(f'    {"age":4s} {"rate/wk":>9s} {"$/hr":>7s} {"/4.345":>8s} {"/52":>7s} {"center other ages":28s} {"FAMPAY":>6s} {"enrl":>5s} {"weight":>7s}')
    n = 0
    for c in cb:
        if c['CB9_METH_DATA_SOURCE'] != 1:
            continue
        for a in RATE_AGES:
            r = c.get(f'CB9_STDRATE_WKLY_{a}')
            e = sum(max(c.get(f'CB9_ENRL_{x}') or 0, 0) for x in RATE_ENRL_AGES[a])
            if r is None or r <= 2000 or e <= 0:
                continue
            n += 1
            others = ' '.join(f'{x}:{c.get(f"CB9_STDRATE_WKLY_{x}") or 0:.0f}' for x in RATE_AGES if x != a)
            print(f'    {a:4s} {r:9,.0f} {r / 40:7,.0f} {r / 4.345:8,.0f} {r / 52:7,.0f} {others:28s} '
                  f'{c.get("CB9_B1_3_FAMPAY") or 0:6.0f} {e:5.0f} {c["CB9_METH_WEIGHT"]:7.1f}')
    print(f'    cells: {n}')


def section_g(cb):
    print('\n  G. Price sector of public-school programs where families pay (1a age-bin medians)')
    cells = []
    for c in cb:
        fte, pr = fte_bins(c), bin_prices(c)
        for b in BIN_AGES:
            if fte[b] > 0:
                cells.append(dict(c=c, b=b, fte=fte[b], p=pr[b], w=c['CB9_METH_WEIGHT']))
    interviewed = [x for x in cells if x['c']['CB9_METH_DATA_SOURCE'] == 1]
    med = {b: wmedian([(x['p'], x['w'] * x['fte']) for x in interviewed if x['b'] == b and x['p'] and x['p'] > 0])
           for b in BIN_AGES}
    print('    medians $/hr:', {b: round(m, 2) for b, m in med.items()})
    groups = {
        'interviewed public school, families pay': [x for x in interviewed if x['c'].get('CB9_METH_PUBSCHFLAG') == 1
                                                    and x['c'].get('CB9_B1_3_FAMPAY') == 1],
        'interviewed not public school, families pay': [x for x in interviewed if x['c'].get('CB9_METH_PUBSCHFLAG') != 1
                                                        and x['c'].get('CB9_B1_3_FAMPAY') == 1],
        'admin, families pay': [x for x in cells if x['c']['CB9_METH_DATA_SOURCE'] == 2
                                and x['c'].get('CB9_B1_3_FAMPAY') == 1],
    }
    print(f'    {"group":44s} {"cells":>6s} {"priced":>7s} {"wtd FTE priced":>15s} {"Low":>6s} {"High":>6s} {"mean $/hr":>10s}')
    for lab, xs in groups.items():
        pr = [x for x in xs if x['p'] is not None and x['p'] > 0]
        wf = sum(x['w'] * x['fte'] for x in pr)
        low = sum(x['w'] * x['fte'] for x in pr if x['p'] < med[x['b']])
        mp = wmean([(x['p'], x['w'] * x['fte']) for x in pr])
        if wf:
            print(f'    {lab:44s} {len(xs):6d} {len(pr):7d} {wf:15,.0f} {low / wf:6.1%} {1 - low / wf:6.1%} {mp:10.2f}')
        else:
            print(f'    {lab:44s} {len(xs):6d} {len(pr):7d}  no posted rate')


def section_h(hh):
    print('\n  H. Parents of children under 5 who worked last week (D1A = 1), household weights')
    seen, groups = set(), collections.defaultdict(list)
    for h in hh:
        for k in range(1, 10):
            age = h.get(f'HHC9_AGE_{k}')
            if age is None or age < 0 or age >= 60:
                continue
            for rank in ('HI', 'LO'):
                m = h.get(f'HH9_WST_PARCH_{rank}_{k}')
                if m is None or m <= 0 or (h['HH9_METH_CASEID'], m) in seen:
                    continue
                seen.add((h['HH9_METH_CASEID'], m))
                m = int(m)
                if h.get(f'HH9_D1A_WORK_{m}') != 1:
                    continue
                hrs = h.get(f'HH9_WST_HRS_TOT_P{rank}_{k}')
                g = 'donor (hours > 0)' if hrs is not None and hrs > 0 else \
                    'zero hours' if hrs == 0 else f'no hours record ({hrs:.0f})' if hrs is not None else 'no hours record (blank)'
                unit, pay = h.get(f'HH9_D3D_UNIT_{m}'), h.get(f'HH9_D3D_WAGE_TC_{m}')
                groups[g].append(dict(w=h['HH9_METH_WEIGHT'], rank=rank, hrs=hrs, unit=unit if unit in PAY_UNIT else None,
                                      wage=pay / PAY_HOURS[unit] if unit in PAY_UNIT and pay and pay > 0 else None,
                                      school=h.get(f'HH9_D1B_SCHOOL_{m}') == 1 or h.get(f'HH9_D1C_TRAINING_{m}') == 1))
    for g in sorted(groups):
        xs = groups[g]
        W = sum(x['w'] for x in xs)
        units = collections.Counter()
        for x in xs:
            units[PAY_UNIT.get(x['unit'], 'none')] += x['w'] / W
        print(f'    {g:28s} n={len(xs):5d} second earner {sum(x["w"] for x in xs if x["rank"] == "LO") / W:5.1%} '
              f'school/training {sum(x["w"] for x in xs if x["school"]) / W:5.1%} '
              f'median $/hr {wmedian([(x["wage"], x["w"]) for x in xs]) or 0:6.2f}')
        print(f'      pay unit: ' + ', '.join(f'{u} {s:.1%}' for u, s in sorted(units.items(), key=lambda z: -z[1])))
    donors = groups.get('donor (hours > 0)', [])
    for rank in ('HI', 'LO'):
        d = [x for x in donors if x['rank'] == rank]
        by_unit = {u: wmean([(x['hrs'], x['w']) for x in d if x['unit'] == u]) for u in list(PAY_UNIT) + [None]}
        print(f'    donor mean weekly hours, {"first" if rank == "HI" else "second"} earners: all {wmean([(x["hrs"], x["w"]) for x in d]):.1f}; '
              + ', '.join(f'{PAY_UNIT.get(u, "none")} {v:.1f}' for u, v in by_unit.items() if v))
        for g, xs in groups.items():
            if g.startswith('donor'):
                continue
            xr = [x for x in xs if x['rank'] == rank]
            if not xr:
                continue
            W = sum(x['w'] for x in xr)
            pred = sum(x['w'] * (by_unit.get(x['unit']) or by_unit[None] or 0) for x in xr) / W
            print(f'      {g:28s} n={len(xr):4d}: donor hours given its pay-unit mix {pred:.1f} (unconditional draw gives {wmean([(x["hrs"], x["w"]) for x in d]):.1f})')


def section_i(cb, year):
    print('\n  I. Priced enrollment lost to missing rates, interviewed centers where families pay (FTE x center weight)')
    caps = None
    if year == '2024':
        caps, shares = detect_caps(cb)
        print('    caps per rate age (percentiles: sorted-index, no interpolation; order: low, high, monthly low, monthly high)')
        for a in RATE_AGES:
            sh, q15, q85, n = shares[a]
            print(f'      {a:4s} ' + ' / '.join('--' if v is None else f'{v:g}' for v in caps[a])
                  + f'   q15 {q15:g}, q85 {q85:g}, positive rates {n}, held by caps {sh:.1%}')
    cells = []
    for c in cb:
        if c['CB9_METH_DATA_SOURCE'] != 1 or c.get('CB9_B1_3_FAMPAY') != 1:
            continue
        fte = fte_bins(c)
        for b in BIN_AGES:
            if fte[b] <= 0:
                continue
            rates = [(a, c.get(f'CB9_STDRATE_WKLY_{a}')) for a in RATE_AGES if RATE_BIN[a] == b]
            cells.append((rate_class(rates), rate_class(rates, caps) if caps else None, fte[b] * c['CB9_METH_WEIGHT']))
    tot = sum(x[2] for x in cells)
    hdr = f'    {"class":18s} {"cells":>6s} {"wtd FTE":>11s} {"share":>7s}'
    print(hdr + (f'   {"caps deleted: cells":>20s} {"wtd FTE":>11s} {"share":>7s}' if caps else ''))
    for k in CLASSES + ['all']:
        cols = [0, 1] if caps else [0]
        line = f'    {k:18s}'
        for j in cols:
            xs = [x for x in cells if k == 'all' or x[j] == k]
            wf = sum(x[2] for x in xs)
            line += (f'   {len(xs):20d}' if j else f' {len(xs):6d}') + f' {wf:11,.0f} {wf / tot:7.1%}'
        print(line)
    print('    (trimmed: no positive rate and a code -2 rate in the bin, or a cap-valued rate when caps are deleted)')


def section_j(cb):
    print('\n  J. Free admin centers (DATA_SOURCE 2, FAMPAY 2): NORC-imputed vs reported total staff counts')
    xs = []
    for c in cb:
        if c['CB9_METH_DATA_SOURCE'] != 2 or c.get('CB9_B1_3_FAMPAY') != 2:
            continue
        st = c.get('CB9_STAFF_TYPE_TOTAL_NUM')
        xs.append(dict(w=c['CB9_METH_WEIGHT'], imp=c.get('CB9_ADMIN_STAFF_IMP'), fte=admin_fte(c),
                       staff=st if st is not None and st >= 0 else None, enrl=c.get('CB9_ENRL_0TO5') or 0,
                       s=admin_ft_share(c), region=c.get('CB9_REGION')))
    print(f'    centers {len(xs)}; CB9_ADMIN_STAFF_IMP values: '
          + ', '.join(f'{k}: {n}' for k, n in sorted(collections.Counter(x['imp'] for x in xs).items(), key=str)))
    grp = {'imputed': [x for x in xs if x['imp'] == 1], 'reported': [x for x in xs if x['imp'] == 0]}

    # J1 counts and ratios
    print('    J1 counts and ratios (shares of centers unweighted)')
    print(f'      {"group":9s} {"n":>5s} {"staff ok":>8s} {"wtd staff":>10s} {"staff/FTE":>10s} {"staff/child":>11s} '
          f'{"ratio 0.110":>11s} {"non-integer":>11s}')
    for g, ys in grp.items():
        v = [y for y in ys if y['staff'] is not None]
        ve = [y for y in v if y['enrl'] > 0]
        spe = sum(y['w'] * y['staff'] for y in ve) / sum(y['w'] * y['enrl'] for y in ve) if ve else 0
        r110 = sum(round(y['staff'] / y['enrl'], 3) == 0.11 for y in ve) / len(ve) if ve else 0
        nonint = sum(y['staff'] != int(y['staff']) for y in v) / len(v) if v else 0
        print(f'      {g:9s} {len(ys):5d} {len(v):8d} {sum(y["w"] * y["staff"] for y in v):10,.0f} '
              f'{ratio([y for y in v if y["fte"] > 0]) or 0:10.4f} {spe:11.4f} {r110:11.1%} {nonint:11.1%}')

    # J2 composition
    regions = sorted({x['region'] for x in xs if x['region'] is not None})
    print('    J2 composition (center weights, FTE > 0)')
    print(f'      {"group":9s} {"mean FTE":>9s} {"median FTE":>10s} {"mean s":>7s} '
          + ''.join(f'{"region " + str(int(r)):>10s}' for r in regions))
    for g, ys in grp.items():
        ys = [y for y in ys if y['fte'] > 0]
        W = sum(y['w'] for y in ys)
        print(f'      {g:9s} {wmean([(y["fte"], y["w"]) for y in ys]):9.1f} {wmedian([(y["fte"], y["w"]) for y in ys]):10.1f} '
              f'{wmean([(y["s"], y["w"]) for y in ys]):7.2f} '
              + ''.join(f'{sum(y["w"] for y in ys if y["region"] == r) / W:10.1%}' for r in regions))

    # J3 by FTE quartile of reported centers
    imp_v = [y for y in grp['imputed'] if y['staff'] is not None and y['fte'] > 0]
    rep_v = [y for y in grp['reported'] if y['staff'] is not None and y['fte'] > 0]
    cuts = [pctile([y['fte'] for y in rep_v], q) for q in (0.25, 0.5, 0.75)]
    quart = lambda f: sum(f > k for k in cuts)
    print(f'    J3 staff per FTE child by FTE quartile (cuts from reported, unweighted, sorted-index: '
          + ', '.join(f'{k:.1f}' for k in cuts) + ')')
    print(f'      {"quartile":9s} {"imputed n":>9s} {"staff/FTE":>10s} {"reported n":>10s} {"staff/FTE":>10s}')
    rq = {}
    for q in range(4):
        iq, rr = [y for y in imp_v if quart(y['fte']) == q], [y for y in rep_v if quart(y['fte']) == q]
        rq[q] = (ratio(rr), len(rr))
        print(f'      Q{q + 1:<8d} {len(iq):9d} {ratio(iq) or 0:10.4f} {len(rr):10d} {ratio(rr) or 0:10.4f}')

    # J4 imputed-center staff totals under four rules
    r0 = ratio(rep_v)
    rc = {}
    for q in range(4):
        for s in (0.0, 0.5, 1.0):
            rr = [y for y in rep_v if quart(y['fte']) == q and y['s'] == s]
            rc[(q, s)] = ratio(rr) if len(rr) >= 10 else rq[q][0] if rq[q][1] >= 10 else r0
    lx = [(math.log(y['fte']), math.log(y['staff']), y['w']) for y in rep_v if y['staff'] > 0]
    W = sum(w for _, _, w in lx)
    xb, yb = sum(x * w for x, _, w in lx) / W, sum(y * w for _, y, w in lx) / W
    b = sum(w * (x - xb) * (y - yb) for x, y, w in lx) / sum(w * (x - xb) ** 2 for x, _, w in lx)
    a = yb - b * xb
    smear = sum(w * math.exp(y - a - b * x) for x, y, w in lx) / W
    hi = max(y['fte'] for y in rep_v)
    rules = {
        'NORC': lambda y: y['staff'],
        'flat': lambda y: r0 * y['fte'],
        'cell': lambda y: rc[(quart(y['fte']), y['s'])] * y['fte'],
        'log-log': lambda y: smear * math.exp(a + b * math.log(y['fte'])),
    }
    rep_tot = sum(y['w'] * y['staff'] for y in rep_v)
    print(f'    J4 weighted staff totals (FTE > 0, valid staff: imputed n={len(imp_v)}, reported n={len(rep_v)})')
    print(f'      flat staff/FTE {r0:.4f}; log-log slope {b:.3f}, intercept {a:.3f}, smearing {smear:.3f} '
          f'(n={len(lx)}); imputed above largest reported FTE ({hi:.1f}): '
          f'{sum(y["fte"] > hi for y in imp_v) / len(imp_v):.1%}')
    print(f'      {"rule":8s} {"imputed":>10s} {"vs NORC":>8s} {"all free admin":>15s} {"vs NORC":>8s}')
    base = {}
    for k, f in rules.items():
        t = sum(y['w'] * f(y) for y in imp_v)
        base.setdefault('imp', t)
        base.setdefault('all', t + rep_tot)
        print(f'      {k:8s} {t:10,.0f} {t / base["imp"] - 1:+8.1%} {t + rep_tot:15,.0f} {(t + rep_tot) / base["all"] - 1:+8.1%}')

    # J5 overlap only, no extrapolation
    lo = min(y['fte'] for y in rep_v)
    imp_o = [y for y in imp_v if lo <= y['fte'] <= hi]
    print(f'    J5 imputed centers within reported FTE range [{lo:.1f}, {hi:.1f}]: n={len(imp_o)} of {len(imp_v)}')
    t0 = sum(y['w'] * y['staff'] for y in imp_o)
    for k in ('NORC', 'flat', 'cell'):
        t = sum(y['w'] * rules[k](y) for y in imp_o)
        print(f'      {k:8s} {t:10,.0f} {t / t0 - 1:+8.1%}')

    # J6 region: NORC imputed from district ratios, imputed/reported region mix differs
    print('    J6 region (reported staff per FTE child, center weights)')
    med = pctile([y['fte'] for y in rep_v], 0.5)
    print(f'      (a) {"region":8s} {"n":>5s} {"staff/FTE":>10s}   {"FTE <= " + format(med, ".1f"):>12s} {"n":>5s}'
          f'   {"FTE > " + format(med, ".1f"):>12s} {"n":>5s}')
    for r in regions:
        rr = [y for y in rep_v if y['region'] == r]
        lo_h, hi_h = [y for y in rr if y['fte'] <= med], [y for y in rr if y['fte'] > med]
        print(f'          {int(r):<8d} {len(rr):5d} {ratio(rr) or 0:10.4f}   {ratio(lo_h) or 0:12.4f} {len(lo_h):5d}'
              f'   {ratio(hi_h) or 0:12.4f} {len(hi_h):5d}')
    fit = [y for y in rep_v if y['staff'] > 0]
    dums = regions[1:]
    print(f'      (b) WLS ln(staff) on ln(FTE) + region dummies (base region {int(regions[0])}), reported staff > 0, n={len(fit)}')
    print(f'          {"model":14s} {"slope":>6s} ' + ''.join(f'{"reg " + str(int(r)):>7s}' for r in dums)
          + f' {"s":>7s} {"smear":>6s} {"imputed":>9s} {"vs NORC":>8s} {"all":>8s} {"vs NORC":>8s}')
    for lab, use_s in (('region', False), ('region + s', True)):
        feats = lambda y: [1.0, math.log(y['fte'])] + [float(y['region'] == r) for r in dums] + ([y['s']] if use_s else [])
        beta = wls([feats(y) for y in fit], [math.log(y['staff']) for y in fit], [y['w'] for y in fit])
        lin = lambda y: sum(bk * xk for bk, xk in zip(beta, feats(y)))
        sm = sum(y['w'] * math.exp(math.log(y['staff']) - lin(y)) for y in fit) / sum(y['w'] for y in fit)
        t = sum(y['w'] * sm * math.exp(lin(y)) for y in imp_v)
        print(f'          {lab:14s} {beta[1]:6.3f} ' + ''.join(f'{bk:7.3f}' for bk in beta[2:2 + len(dums)])
              + (f' {beta[-1]:7.3f}' if use_s else f' {"--":>7s}')
              + f' {sm:6.3f} {t:9,.0f} {t / base["imp"] - 1:+8.1%} {t + rep_tot:8,.0f} {(t + rep_tot) / base["all"] - 1:+8.1%}')
    print('      (c) NORC imputed centers by region: staff per enrolled child (weighted), share at 0.110 (unweighted)')
    print(f'          {"region":8s} {"n":>5s} {"staff/child":>11s} {"at 0.110":>9s}')
    for r in regions:
        ve = [y for y in grp['imputed'] if y['region'] == r and y['staff'] is not None and y['enrl'] > 0]
        if not ve:
            continue
        spe = sum(y['w'] * y['staff'] for y in ve) / sum(y['w'] * y['enrl'] for y in ve)
        print(f'          {int(r):<8d} {len(ve):5d} {spe:11.4f} {sum(round(y["staff"] / y["enrl"], 3) == 0.11 for y in ve) / len(ve):9.1%}')


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out', default=None)
    args = ap.parse_args()
    with (open(args.out, 'w') if args.out else contextlib.nullcontext(sys.stdout)) as out, \
            contextlib.redirect_stdout(out):
        for year, cfg in YEARS.items():
            cb = load(args.raw_root, cfg['cb'], cfg['d'], ('CB9_',))
            hh = load(args.raw_root, cfg['hh'], cfg['d'],
                      ('HH9_METH_', 'HHC9_AGE_', 'HH9_WST_', 'HH9_D1A_', 'HH9_D1B_', 'HH9_D1C_', 'HH9_D3D_'))
            print(f'\n===== {year}')
            section_e(cb, cfg['dk'])
            section_f(cb)
            section_g(cb)
            section_h(hh)
            section_i(cb, year)
            if year == '2024':
                section_j(cb)


if __name__ == '__main__':
    main()
