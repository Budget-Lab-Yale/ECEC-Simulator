"""
06_supply_checks.py -- supply-side checks on 1a as written on feat/nsece-2024, both years

Re-implements 1a's center and home-based supply inputs (including the 2024 harmonize
functions) and reports:
  A. Unguarded reserve codes: posted rates whose code reaches 1a's price_status 'OK'
     with a negative value (e.g. 2024 STDRATE -9), and where those cells would land.
  B. Administrative-data centers: their share of each sector's FTE enrollment and of
     the workforce, and the sector labor ratios with 1a's admin treatment
     ('impute': enrollment split across ages, as harmonize_nsece_2024_centers does)
     against dropping admin cases from both sides ('drop').
  C. Center supply inputs by sector (price, labor ratios, labor cost, residual), and
     the low-price residual under alternatives: drop admin, trim 2024 posted rates at
     the 5th/95th percentiles as 2019 did, sector-specific wages.
  D. Home-based labor inputs and the labor stock L_0, unweighted (as 1a) and weighted.

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/06_supply_checks.py [--raw-root PATH] [--out FILE]

Runtime: about a minute. Python 3.9+, standard library only. Figures re-implement 1a in
Python, so expect small differences from the R output (weighted medians differ slightly
from Hmisc::wtd.quantile).
"""
import argparse
import collections
import csv
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {
    '2019': dict(cb='2019/37941-0006-Data.tsv', wf='2019/37941-0005-Data.tsv', hb='2019/37941-0007-Data.tsv', d='9'),
    '2024': dict(cb='2024/39466-0002-Data.tsv', wf='2024/39466-0001-Data.tsv', hb='2024/39466-0003-Data.tsv', d='4'),
}
AGES = ['INF', '1YR', '2YR', '3YR', '4YR', '5YR']
RATE_AGES = ['INF', '2YR', '3YR', '4YR']
BIN = {'INF': '0-1', '1YR': '0-1', '2YR': '2-3', '3YR': '2-3', '4YR': '4-5', '5YR': '4-5'}
SECTORS = ['Unpaid', 'Low', 'High']
FT_CARE_HOURS, WEEKS_PER_YEAR = 40, 52
KEEP = ('CB9_METH_', 'CB9_ENRL_', 'CB9_STDRATE_WKLY_', 'CB9_B1_3', 'CB9_ADMIN_', 'CB9_F3_YOUNGMONTHS_R',
        'WF9_METH_', 'WF9_WORK_', 'WF9_CHAR_EDUC', 'HB9_METH_', 'HB9_ENRL_TOTAL', 'HB9_CAREER_EDUC',
        'HB9_H2_NUMHELP', 'HB9_STAFF_EDUC_PRCNT_4YR')


def num(x):
    try:
        return float(x.strip('"'))
    except ValueError:
        return None


def wmedian(pairs):
    pairs = sorted((x, w) for x, w in pairs if x is not None and w and w > 0)
    half, acc = sum(w for _, w in pairs) / 2, 0.0
    for x, w in pairs:
        acc += w
        if acc >= half:
            return x


def quantile(xs, q):
    xs = sorted(xs)
    pos = (len(xs) - 1) * q
    lo = int(pos)
    return xs[lo] + (xs[min(lo + 1, len(xs) - 1)] - xs[lo]) * (pos - lo)


def load(raw_root, path, d):
    """Rows as dicts with 2019-style names (year digit 9) and numeric values."""
    with open(f'{raw_root}/{path}') as fh:
        r = csv.reader(fh, delimiter='\t')
        header = [h.strip('"') for h in next(r)]
        names = []
        for h in header:
            for p in ('HHC', 'HH', 'CB', 'HB', 'WF'):
                if h.startswith(f'{p}{d}_'):
                    h = f'{p}9_' + h[len(p) + 2:]
                    break
            names.append(h)
        keep = [(i, n) for i, n in enumerate(names) if n.startswith(KEEP)]
        return [{n: num(row[i]) for i, n in keep} for row in r]


def harmonize_centers_2024(cb, admin):
    """harmonize_nsece_2024_centers() from 1a; admin = 'impute' (as 1a) or 'drop'."""
    is_admin = lambda c: c['CB9_METH_DATA_SOURCE'] == 2
    donors = [c for c in cb if c['CB9_METH_DATA_SOURCE'] == 1 and c['CB9_METH_PUBSCHFLAG'] == 1
              and c['CB9_B1_3_FAMPAY'] == 2 and all(c[f'CB9_ENRL_{a}'] >= 0 for a in AGES)]
    tot = {a: sum(c[f'CB9_ENRL_{a}'] * c['CB9_METH_WEIGHT'] for c in donors) for a in AGES}
    share = {a: tot[a] / sum(tot.values()) for a in AGES}
    out = []
    for c in cb:
        c = dict(c)
        adm = is_admin(c)
        if adm and admin == 'drop':
            for a in AGES:
                c[f'CB9_ENRL_{a}'] = 0
        ft_share = 100 if (c.get('CB9_ADMIN_FULLTIME_CARE') == 1 and c.get('CB9_ADMIN_PARTTIME_CARE') == 2) else \
            0 if (c.get('CB9_ADMIN_FULLTIME_CARE') == 2 and c.get('CB9_ADMIN_PARTTIME_CARE') == 1) else 50
        for a in AGES:
            e, ft = c[f'CB9_ENRL_{a}'], c[f'CB9_ENRL_FT_{a}']
            if adm and admin == 'impute':
                e = max(c['CB9_ENRL_0TO5'], 0) * share[a]
                ft = ft_share if e > 0 else -2
            c[f'CB9_ENRL_{a}'], c[f'CB9_ENRL_FT_{a}'] = max(e, 0), ft
        out.append(c)
    for a in AGES:   # full-time share unknown (-4) -> enrollment-weighted mean
        rep = [(c[f'CB9_ENRL_FT_{a}'], c['CB9_METH_WEIGHT'] * c[f'CB9_ENRL_{a}']) for c in out
               if not is_admin(c) and c[f'CB9_ENRL_{a}'] > 0 and c[f'CB9_ENRL_FT_{a}'] >= 0]
        m = sum(x * w for x, w in rep) / sum(w for _, w in rep)
        for c in out:
            if c[f'CB9_ENRL_FT_{a}'] == -4:
                c[f'CB9_ENRL_FT_{a}'] = m if c[f'CB9_ENRL_{a}'] > 0 else -2
    for c in out:
        for a in RATE_AGES:
            f = c[f'CB9_B1_3A_AGERATE_{a}_R']
            c[f'CB9_B1_3A_AGERATE_{a}_R'] = -8 if c['CB9_B1_3_FAMPAY'] == 2 else f if f in (1, 2) else 3
            if c[f'CB9_STDRATE_WKLY_{a}'] == -8:
                c[f'CB9_STDRATE_WKLY_{a}'] = -1
    return out


def trim_rates(cb):
    """Option 'supply' part 1: positive rates at/beyond each age's 5th/95th pct -> -2."""
    for a in RATE_AGES:
        pos = [c[f'CB9_STDRATE_WKLY_{a}'] for c in cb if c[f'CB9_STDRATE_WKLY_{a}'] and c[f'CB9_STDRATE_WKLY_{a}'] > 0]
        lo, hi = quantile(pos, 0.05), quantile(pos, 0.95)
        for c in cb:
            r = c[f'CB9_STDRATE_WKLY_{a}']
            if r and r > 0 and (r <= lo or r >= hi):
                c[f'CB9_STDRATE_WKLY_{a}'] = -2
    return cb


def centers(cb, guard_negative=False, unguarded=None):
    """processed_centers from 1a: (id, bin) -> weight, fte, price, sector, is_admin."""
    cells = {}
    for c in cb:
        bins = collections.defaultdict(lambda: dict(e=0.0, ft=[], p=[]))
        for a in AGES:
            e, ft = c[f'CB9_ENRL_{a}'], c[f'CB9_ENRL_FT_{a}']
            b = bins[BIN[a]]
            b['e'] += e
            b['ft'].append(ft)
            if a not in RATE_AGES:
                continue
            rate, flag = c[f'CB9_STDRATE_WKLY_{a}'], c[f'CB9_B1_3A_AGERATE_{a}_R']
            if e == 0:
                st = 'NIU'
            elif flag == -8:
                st = 'FREE'
            elif flag == 2:
                st = 'NORATE'
            elif rate in (-1, -2):
                st = 'MISS'
            else:
                st = 'OK'
            if st == 'OK' and rate < 0:
                if unguarded is not None:
                    unguarded[(rate, flag, c['CB9_B1_3_FAMPAY'])] += c['CB9_METH_WEIGHT'] * e
                    unguarded[('n', rate)] += 1
                if guard_negative:
                    st = 'MISS'
            if st == 'OK':
                b['p'].append(rate / 40)
            elif st == 'FREE':
                b['p'].append(0.0)
        for bn, b in bins.items():
            fts = b['ft']
            s = 0 if all(x < 0 for x in fts) else \
                (sum(x for x in fts if x >= 0) / len([x for x in fts if x >= 0]) / 100)
            price = sum(b['p']) / len(b['p']) if b['p'] else None
            cells[(c['CB9_METH_CASEID'], bn)] = dict(
                w=c['CB9_METH_WEIGHT'], fte=b['e'] * s + 0.5 * b['e'] * (1 - s), price=price,
                admin=c['CB9_METH_DATA_SOURCE'] == 2, fampay=c['CB9_B1_3_FAMPAY'])
    meds = {bn: wmedian([(v['price'], v['w'] * v['fte']) for (i, b), v in cells.items()
                         if b == bn and v['price'] is not None and v['price'] > 0]) for bn in ('0-1', '2-3', '4-5')}
    for (i, bn), v in cells.items():
        p = v['price']
        v['sector'] = None if p is None else 'Unpaid' if p == 0 else 'Low' if p < meds[bn] else 'High'
    return cells, meds


def workers(wf, cb):
    """processed_wf joined to the center's sampled classroom age bin."""
    cls_bin = {}
    for c in cb:
        m = c['CB9_F3_YOUNGMONTHS_R']
        y = None if m is None or m in (-1, -8) else int(m // 12)
        cls_bin[c['CB9_METH_CASEID']] = {0: '0-1', 1: '0-1', 2: '2-3', 3: '2-3', 4: '4-5', 5: '4-5'}.get(y)
    out = []
    for r in wf:
        ed, ft, role, wage = r['WF9_CHAR_EDUC'], r['WF9_WORK_FT'], r['WF9_WORK_ROLE'], r['WF9_WORK_WAGE']
        out.append(dict(
            id=r['CB9_METH_CASEID'], w=r['WF9_METH_WEIGHT'], bin=cls_bin.get(r['CB9_METH_CASEID']),
            college=1 if ed in (7, 8) else 0 if ed in (1, 3, 4, 5, 6) else None,
            ft=1 if ft == 1 else 0.5 if ft == 2 else None, role=role if role in (1, 2, 3) else None,
            wage=wage if wage and wage > 0 else None))
    return out


def center_inputs(cells, wk, sector_wages=False):
    sel = [x for x in wk if x['role'] in (1, 2) and x['college'] is not None and x['wage'] is not None]
    wages = {k: sum(x['wage'] * x['w'] for x in sel if x['college'] == k) / sum(x['w'] for x in sel if x['college'] == k)
             for k in (0, 1)}
    lab = collections.Counter(); swage = collections.defaultdict(lambda: [0.0, 0.0])
    for x in wk:
        cell = cells.get((x['id'], x['bin']))
        if x['role'] not in (1, 2) or x['bin'] is None or not cell or cell['sector'] is None \
                or x['college'] is None or x['ft'] is None:
            continue
        s = cell['sector']
        lab[(s, 'ba')] += x['college'] * x['ft'] * x['w']
        lab[(s, 'no_ba')] += (1 - x['college']) * x['ft'] * x['w']
        if x['wage'] is not None:
            swage[(s, x['college'])][0] += x['wage'] * x['w']; swage[(s, x['college'])][1] += x['w']
    res = {}
    for s in SECTORS:
        by_bin = {}
        for (i, bn), v in cells.items():
            if v['sector'] != s:
                continue
            d = by_bin.setdefault(bn, [0.0, 0.0, 0.0])
            ww = v['w'] * v['fte']
            d[0] += v['price'] * ww; d[1] += ww; d[2] += ww
        fte = sum(d[2] for d in by_bin.values())
        price = sum((d[0] / d[1]) * d[2] for d in by_bin.values() if d[1] > 0) / fte
        ba, no_ba = lab[(s, 'ba')] / fte, lab[(s, 'no_ba')] / fte
        wv = {k: (swage[(s, k)][0] / swage[(s, k)][1]) if sector_wages and swage[(s, k)][1] else wages[k] for k in (0, 1)}
        cost = wv[1] * ba + wv[0] * no_ba
        res[s] = dict(price=price, fte=fte, ba=ba, no_ba=no_ba, cost=cost,
                      resid=price - cost if price > 0 else None, w_ba=wv[1], w_no=wv[0])
    return res, wages


def home_inputs(hb, wages, weighted):
    tot = collections.Counter()
    for h in hb:
        if not (h['HB9_ENRL_TOTAL'] >= 0 and h['HB9_CAREER_EDUC'] > 0 and h['HB9_H2_NUMHELP'] > -4
                and h['HB9_STAFF_EDUC_PRCNT_4YR'] not in (-1, -3) and h['HB9_METH_SAMPLE_SOURCE'] == 2):
            continue
        w = h['HB9_METH_WEIGHT'] if weighted else 1.0
        nh = 0 if h['HB9_H2_NUMHELP'] == -2 else h['HB9_H2_NUMHELP']
        n_ba = (1 if h['HB9_CAREER_EDUC'] >= 6 else 0) + nh * h['HB9_STAFF_EDUC_PRCNT_4YR'] / 100
        tot['enrl'] += w * h['HB9_ENRL_TOTAL']; tot['ba'] += w * n_ba; tot['no_ba'] += w * (1 + nh - n_ba)
        tot['n'] += 1
    ba, no_ba = tot['ba'] / tot['enrl'], tot['no_ba'] / tot['enrl']
    return dict(n=tot['n'], fte=tot['enrl'], ba=ba, no_ba=no_ba, cost=wages[1] * ba + wages[0] * no_ba)


def harmonize_home_2024(hb):
    for h in hb:
        h['HB9_H2_NUMHELP'] = {-3: -2, -1: -4}.get(h['HB9_H2_NUMHELP'], h['HB9_H2_NUMHELP'])
        h['HB9_STAFF_EDUC_PRCNT_4YR'] = {-3: -2, -5: -3}.get(h['HB9_STAFF_EDUC_PRCNT_4YR'], h['HB9_STAFF_EDUC_PRCNT_4YR'])
    return hb


def fmt(res):
    return '  '.join(f"{s}: price {r['price']:.2f} ba {r['ba']:.4f} no_ba {r['no_ba']:.4f} "
                     f"cost {r['cost']:.2f} resid {r['resid'] if r['resid'] is None else round(r['resid'], 2)}"
                     for s, r in res.items())


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out')
    args = ap.parse_args()
    csv.field_size_limit(10**9)
    out = open(args.out, 'w') if args.out else sys.stdout
    P = lambda *a: print(*a, file=out)

    for year, cfg in YEARS.items():
        cb_raw = load(args.raw_root, cfg['cb'], cfg['d'])
        wf = load(args.raw_root, cfg['wf'], cfg['d'])
        hb = load(args.raw_root, cfg['hb'], cfg['d'])
        if year == '2024':
            hb = harmonize_home_2024(hb)
        prep = (lambda admin: harmonize_centers_2024(cb_raw, admin)) if year == '2024' else \
            (lambda admin: [dict(c, **({f'CB9_ENRL_{a}': 0 for a in AGES} if admin == 'drop' and c['CB9_METH_DATA_SOURCE'] == 2 else {}))
                            for c in cb_raw])
        wk = workers(wf, cb_raw)

        P(f'\n######## {year} ########')
        # A. unguarded codes
        ung = collections.Counter()
        cells, meds = centers(prep('impute'), unguarded=ung)
        P('## A. posted rates reaching price_status OK with a negative value')
        if not ung:
            P('  none')
        for k, v in sorted(ung.items(), key=str):
            if k[0] == 'n':
                P(f'  rate {k[1]:.0f}: {v} center-age cells')
            else:
                P(f'  rate {k[0]:.0f}, flag {k[1]:.0f}, FAMPAY {k[2]}: weighted enrollment {v:,.0f}')
        neg_cells = [v for v in cells.values() if v['price'] is not None and v['price'] < 0]
        P(f'  center-age-bin cells with a negative mean price: {len(neg_cells)}, sectors '
          f'{dict(collections.Counter(v["sector"] for v in neg_cells))}, weighted FTE {sum(v["w"] * v["fte"] for v in neg_cells):,.0f}')
        P(f'  sector medians by age bin: ' + ' '.join(f'{b} ${m:.2f}' for b, m in meds.items()))

        # B. admin centers
        P('## B. administrative-data centers')
        fte = collections.Counter()
        for v in cells.values():
            if v['sector']:
                fte[(v['sector'], v['admin'])] += v['w'] * v['fte']
        for s in SECTORS:
            a, i = fte[(s, True)], fte[(s, False)]
            P(f'  {s:6s} FTE enrollment: interviewed {i:12,.0f}  admin {a:10,.0f}  ({100 * a / (a + i):4.1f}% admin)')
        adm_ids = {c['CB9_METH_CASEID'] for c in cb_raw if c['CB9_METH_DATA_SOURCE'] == 2}
        adm_w = [x for x in wk if x['id'] in adm_ids]
        P(f'  workforce records at admin centers: {len(adm_w)}; with role, education and FT usable: '
          f'{sum(1 for x in adm_w if x["role"] in (1, 2) and x["college"] is not None and x["ft"] is not None)}; '
          f'with a wage: {sum(1 for x in adm_w if x["wage"] is not None)}; with a classroom age bin: '
          f'{sum(1 for x in adm_w if x["bin"])}')
        fam = collections.Counter(c['CB9_B1_3_FAMPAY'] for c in cb_raw if c['CB9_METH_DATA_SOURCE'] == 2)
        P(f'  admin centers by family-pay item: {dict(sorted(fam.items(), key=lambda kv: (kv[0] is None, kv[0])))}')

        # C. center inputs
        P('## C. center supply inputs (hourly $, staff per FTE child)')
        variants = [('1a as written (admin impute in 2024)', prep('impute'), False),
                    ('admin dropped from both sides', prep('drop'), False)]
        if year == '2024':
            variants.append(('admin dropped + 2019-style rate trim', trim_rates(prep('drop')), False))
        variants.append(('admin dropped + sector-specific wages', prep('drop'), True))
        for label, cbv, sw in variants:
            cl, _ = centers(cbv, guard_negative=True)
            res, wages = center_inputs(cl, wk, sector_wages=sw)
            P(f'  [{label}]')
            P(f'    wages (all centers): no BA ${wages[0]:.2f}  BA ${wages[1]:.2f}')
            P('    ' + fmt(res))
            if sw:
                P('    sector wages: ' + '  '.join(f"{s} noBA ${r['w_no']:.2f} BA ${r['w_ba']:.2f}" for s, r in res.items()))

        # D. home-based
        cl, _ = centers(prep('drop'), guard_negative=True)
        res, wages = center_inputs(cl, wk)
        P('## D. home-based labor inputs and the labor stock L_0')
        for weighted in (False, True):
            h = home_inputs(hb, wages, weighted)
            hrs_home = h['fte'] * FT_CARE_HOURS * WEEKS_PER_YEAR
            l0 = collections.Counter()
            for s, r in res.items():
                l0['no_ba_center'] += r['no_ba'] * r['fte'] * FT_CARE_HOURS * WEEKS_PER_YEAR
                l0['ba_center'] += r['ba'] * r['fte'] * FT_CARE_HOURS * WEEKS_PER_YEAR
            l0['no_ba_home'] = h['no_ba'] * hrs_home; l0['ba_home'] = h['ba'] * hrs_home
            P(f'  {"weighted  " if weighted else "unweighted"} n={h["n"]} enrollment {h["fte"]:,.0f}  '
              f'ba {h["ba"]:.4f} no_ba {h["no_ba"]:.4f} labor cost ${h["cost"]:.2f}/child-hour | '
              f'home share of L_0: no BA {100 * l0["no_ba_home"] / (l0["no_ba_home"] + l0["no_ba_center"]):.1f}% '
              f'BA {100 * l0["ba_home"] / (l0["ba_home"] + l0["ba_center"]):.1f}%')
        P(f'  center FTE enrollment (weighted, admin dropped): {sum(r["fte"] for r in res.values()):,.0f}')
    if args.out:
        out.close()


if __name__ == '__main__':
    main()
