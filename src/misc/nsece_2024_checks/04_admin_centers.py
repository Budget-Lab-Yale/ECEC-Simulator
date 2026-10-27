"""
04_admin_centers.py -- center-survey cases built from administrative data, both years

The center-based survey includes public-school programs whose records were built from
administrative data rather than an interview (CB*_METH_DATA_SOURCE = 2; 483 cases in
2019, 1,141 in 2024). This script reports, by data source x public-school flag:
  - share of weighted centers, enrollment by age, family-pay status, 4-year-old price status;
  - which variables are populated for the admin cases;
  - the admin cases' share of each center sector's FTE enrollment (Unpaid / Low / High,
    using 1a's FTE-weighted median price by age bin) and the workforce weight attached to
    admin centers -- i.e. how much admin cases enter the denominator vs the numerator of
    1a's sector labor ratios (workforce FTE / enrollment FTE).

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/04_admin_centers.py [--raw-root PATH] [--out FILE]

Runtime: under a minute. Python 3.9+, standard library only.
Results are summarized in docs/nsece_2019_to_2024_crosswalk.md ("Admin-data centers").
"""
import argparse
import collections
import csv
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {
    '2019': dict(cb='2019/37941-0006-Data.tsv', wf='2019/37941-0005-Data.tsv', cp='CB9', wp='WF9'),
    '2024': dict(cb='2024/39466-0002-Data.tsv', wf='2024/39466-0001-Data.tsv', cp='CB4', wp='WF4'),
}
AGES = ['INF', '1YR', '2YR', '3YR', '4YR', '5YR']
AGE_BINS = {'0-1': ['INF', '1YR'], '2-3': ['2YR', '3YR'], '4-5': ['4YR', '5YR']}


def num(x):
    try:
        return float(x.strip('"'))
    except ValueError:
        return None


def wmedian(pairs):
    pairs = sorted((x, w) for x, w in pairs if x is not None and w and w > 0)
    if not pairs:
        return None
    half, acc = sum(w for _, w in pairs) / 2, 0.0
    for x, w in pairs:
        acc += w
        if acc >= half:
            return x


def load(raw_root, path):
    with open(f'{raw_root}/{path}') as fh:
        r = csv.reader(fh, delimiter='\t')
        header = [h.strip('"') for h in next(r)]
        return header, [row for row in r]


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out')
    args = ap.parse_args()
    csv.field_size_limit(10**9)
    out = open(args.out, 'w') if args.out else sys.stdout
    P = lambda *a: print(*a, file=out)

    for year, cfg in YEARS.items():
        cp, wp = cfg['cp'], cfg['wp']
        h, rows = load(args.raw_root, cfg['cb']); ix = {n: i for i, n in enumerate(h)}
        g = lambda row, n: num(row[ix[n]]) if n in ix else None
        is_admin = lambda row: g(row, f'{cp}_METH_DATA_SOURCE') == 2
        group = lambda row: ('admin' if is_admin(row) else 'interview') + f'|pubsch={int(g(row, f"{cp}_METH_PUBSCHFLAG"))}'
        admin_ids = {row[ix[f'{cp}_METH_CASEID']].strip('"') for row in rows if is_admin(row)}

        # --- profile by data source x public-school flag ---
        W = collections.Counter(); N = collections.Counter()
        enrl = collections.defaultdict(collections.Counter); fam = collections.defaultdict(collections.Counter)
        p4 = collections.defaultdict(collections.Counter)
        for row in rows:
            k = group(row); w = g(row, f'{cp}_METH_WEIGHT') or 0; W[k] += w; N[k] += 1
            for a in AGES:
                e = g(row, f'{cp}_ENRL_{a}')
                if e is not None and e >= 0: enrl[k][a] += w * e
            fp = g(row, f'{cp}_B1_3_FAMPAY'); fam[k][fp] += w
            e4 = g(row, f'{cp}_ENRL_4YR')
            if e4 and e4 > 0:
                flag, rate = g(row, f'{cp}_B1_3A_AGERATE_4YR_R'), g(row, f'{cp}_STDRATE_WKLY_4YR')
                free = (flag == -8) if year == '2019' else (flag == -3 and fp == 2)
                p4[k]['free' if free else 'rate > 0' if rate and rate > 0 else 'no rate' if flag == 2 else 'missing'] += w * e4
        P(f'## {year} center survey by data source x public-school flag')
        for k in sorted(W):
            t = sum(enrl[k].values())
            P(f'  {k:20s} n={N[k]:5d}  {100 * W[k] / sum(W.values()):4.1f}% of weighted centers | '
              f'age-specific enrollment {t:11,.0f}' + ('  mix ' + ' '.join(f'{a}:{100 * enrl[k][a] / t:.0f}%' for a in AGES) if t else ''))
            fw = sum(fam[k].values())
            P('      family-pay item (weighted %): ' + ' '.join(f'{int(x) if x is not None else "NA"}:{100 * v / fw:.0f}'
                                                         for x, v in sorted(fam[k].items(), key=lambda kv: (kv[0] is None, kv[0] or 0))))
            pt = sum(p4[k].values())
            if pt: P('      4-year-old price status (enrollment-weighted %): ' + ' '.join(f'{s}:{100 * v / pt:.0f}' for s, v in sorted(p4[k].items())))
        admin_rows = [row for row in rows if is_admin(row)]
        if admin_rows:
            valid = [n for n in h if sum(1 for row in admin_rows if (g(row, n) if g(row, n) is not None else -1) >= 0) >= 0.5 * len(admin_rows)]
            P(f'  admin cases: {len(valid)} variables populated for >=50% of cases, incl. ' + ' '.join(v for v in valid if 'ENRL' in v or 'FAMPAY' in v or 'STAFF_TYPE' in v))

        # --- sector FTE enrollment: admin share of the labor-ratio denominator ---
        acc = collections.defaultdict(list)
        for row in rows:
            w, fp = g(row, f'{cp}_METH_WEIGHT'), g(row, f'{cp}_B1_3_FAMPAY')
            for b, ages in AGE_BINS.items():
                e_sum, fts, prices = 0.0, [], []
                for a in ages:
                    e, ft = g(row, f'{cp}_ENRL_{a}'), g(row, f'{cp}_ENRL_FT_{a}')
                    rate, flag = g(row, f'{cp}_STDRATE_WKLY_{a}'), g(row, f'{cp}_B1_3A_AGERATE_{a}_R')
                    if e is not None and e > 0: e_sum += e
                    if ft is not None and ft >= 0: fts.append(ft)
                    if rate is None or e == 0: continue
                    free = (flag == -8) if year == '2019' else (flag == -3 and fp == 2)
                    if free: prices.append(0.0)
                    elif flag != 2 and rate >= 0: prices.append(rate / 40)
                if not prices or e_sum <= 0: continue
                s = (sum(fts) / len(fts) / 100) if fts else 0.0
                acc[b].append((sum(prices) / len(prices), w * (e_sum * s + 0.5 * e_sum * (1 - s)), is_admin(row)))
        fte = collections.Counter()
        for b, L in acc.items():
            med = wmedian([(x, ww) for x, ww, _ in L if x > 0])
            for x, ww, adm in L:
                sector = 'Unpaid' if x == 0 else 'Low' if x < med else 'High'
                fte[(sector, adm)] += ww
        P(f'## {year} share of each center sector\'s FTE enrollment from admin cases')
        for sector in ('Unpaid', 'Low', 'High'):
            a, i = fte[(sector, True)], fte[(sector, False)]
            P(f'  {sector:6s}: interview {i:12,.0f}  admin {a:10,.0f}  admin share {100 * a / (a + i):5.1f}%')

        # --- workforce attached to admin centers: the labor-ratio numerator ---
        h, wrows = load(args.raw_root, cfg['wf']); wix = {n: i for i, n in enumerate(h)}
        gw = lambda row, n: num(row[wix[n]]) if n in wix else None
        wt = collections.Counter(); n_w = collections.Counter(); usable = collections.Counter()
        for row in wrows:
            k = 'admin-linked' if row[wix[f'{cp}_METH_CASEID']].strip('"') in admin_ids else 'interview'
            w = gw(row, f'{wp}_METH_WEIGHT') or 0; ft = gw(row, f'{wp}_WORK_FT')
            wt[k] += w; n_w[k] += 1
            if ft in (1, 2) and gw(row, f'{wp}_CHAR_EDUC') not in (None, -1, -8) and gw(row, f'{wp}_WORK_ROLE') in (1, 2):
                usable[k] += w * (1 if ft == 1 else 0.5)
        P(f'## {year} workforce records by center data source')
        for k in sorted(wt):
            P(f'  {k:12s} n={n_w[k]:5d}  {100 * wt[k] / sum(wt.values()):5.1f}% of workforce weight; usable FT-equivalent weight {usable[k]:,.0f}')
    if args.out:
        out.close()


if __name__ == '__main__':
    main()
