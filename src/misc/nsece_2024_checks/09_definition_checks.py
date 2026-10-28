"""
09_definition_checks.py -- center-file definitions that differ between 2019 and 2024, both years

Reports, from the center file only (weighted figures use CB9_METH_WEIGHT):
  A. Free-center flag. 1a calls a center-age cell free when its rate flag
     CB9_B1_3A_AGERATE_<age>_R is -8. In 2019, -8 means "no families pay" and is set for
     every age of a center with B1_3_FAMPAY = 2 (none pay) or 3 (don't know). In 2024, -8
     marks admin cases, so harmonize_nsece_2024_centers() sets the flag from FAMPAY = 2 only.
     On 2019, where both exist, cross-tab the two rules: cells and weighted FTE enrollment.
  B. Posted-rate censoring. Reserve-code mix of CB9_STDRATE_WKLY_<age>, the positive-rate
     5th/95th percentiles, the number of positive rates exactly at the lowest and highest
     value (a mass point marks top/bottom coding), and rates above $50 per hour
     (weekly rate / 40), with how many of those sit in cells that get a price in 1a.
  C. Administrative-data centers, staff count by imputation flag (2024 CB9_ADMIN_STAFF_IMP):
     CB9_STAFF_TYPE_TOTAL_NUM per FTE child among free admin centers (the ones 1a adds to
     Unpaid), imputed against reported.
  D. Administrative-data centers that 1a leaves out of every sector (families pay or don't
     know): centers, weighted headcount and FTE enrollment 0-5, and their FTE as a share
     of interviewed Low- plus High-priced FTE enrollment.

FTE counts full-time children as 1 and part-time children as 0.5, as 1a does. Admin FTE uses
1a's rule: enrollment by age where present, else ENRL_0TO5 with the full-time share from
ADMIN_FULLTIME_CARE / ADMIN_PARTTIME_CARE (full time only 100%, part time only 0%, else 50%).

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/09_definition_checks.py [--raw-root PATH] [--out FILE]

Runtime: under a minute. Python 3.9+, standard library only.
"""
import argparse
import collections
import contextlib
import csv
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {
    '2019': dict(cb='2019/37941-0006-Data.tsv', d='9'),
    '2024': dict(cb='2024/39466-0002-Data.tsv', d='4'),
}
AGES = ['INF', '1YR', '2YR', '3YR', '4YR', '5YR']
RATE_AGES = ['INF', '2YR', '3YR', '4YR']
RATE_BIN_AGES = {'INF': ['INF', '1YR'], '2YR': ['2YR'], '3YR': ['3YR'], '4YR': ['4YR', '5YR']}
FAMPAY_LABEL = {1: 'families pay', 2: 'no family pays', 3: "don't know", -1: "don't know"}


def num(x):
    try:
        return float(x.strip('"'))
    except ValueError:
        return None


def load(raw_root, path, d):
    """Rows as dicts with 2019-style names (CB9_) and numeric values."""
    csv.field_size_limit(10**9)
    with open(f'{raw_root}/{path}') as fh:
        r = csv.reader(fh, delimiter='\t')
        header = [h.strip('"') for h in next(r)]
        names = ['CB9_' + h[4:] if h.startswith(f'CB{d}_') else h for h in header]
        return names, [{n: num(v) for n, v in zip(names, row)} for row in r]


def quantile(xs, q):
    """Type-7 quantile (R default), as option 'supply' trims with."""
    xs = sorted(xs)
    pos = (len(xs) - 1) * q
    lo = int(pos)
    return xs[lo] + (xs[min(lo + 1, len(xs) - 1)] - xs[lo]) * (pos - lo)


def fte_by_age(c):
    """FTE enrollment by age; admin records without ages get 1a's ENRL_0TO5 rule (key 'ALL')."""
    out = {}
    if c['CB9_METH_DATA_SOURCE'] == 2 and all((c.get(f'CB9_ENRL_{a}') or -8) < 0 for a in AGES):
        head = max(c.get('CB9_ENRL_0TO5') or 0, 0)
        ft, pt = c.get('CB9_ADMIN_FULLTIME_CARE'), c.get('CB9_ADMIN_PARTTIME_CARE')
        s = 100 if (ft == 1 and pt == 2) else 0 if (ft == 2 and pt == 1) else 50
        out['ALL'] = (head, head * (s / 100 + 0.5 * (1 - s / 100)))
        return out
    for a in AGES:
        e, f = c.get(f'CB9_ENRL_{a}'), c.get(f'CB9_ENRL_FT_{a}')
        if e is None or e <= 0:
            continue
        f = f if f is not None and f >= 0 else 50
        out[a] = (e, e * (f / 100 + 0.5 * (1 - f / 100)))
    return out


def section_a(cb):
    print('\n  A. Free flag: any rate flag -8 (1a rule in 2019) against FAMPAY = 2 (1a rule in 2024)')
    print('     Unit: center-age cells with enrollment, by rate age group; weighted FTE enrollment')
    acc = collections.defaultdict(lambda: [0, 0.0])
    for c in cb:
        fte = fte_by_age(c)
        fam2 = c.get('CB9_B1_3_FAMPAY') == 2
        for ra, ages in RATE_BIN_AGES.items():
            f = sum(fte[a][1] for a in ages if a in fte)
            if f <= 0:
                continue
            flag8 = c.get(f'CB9_B1_3A_AGERATE_{ra}_R') == -8
            src = 'admin' if c['CB9_METH_DATA_SOURCE'] == 2 else 'interviewed'
            k = (src, flag8, fam2)
            acc[k][0] += 1
            acc[k][1] += c['CB9_METH_WEIGHT'] * f
    print(f'    {"source":12s} {"flag -8":>8s} {"FAMPAY=2":>9s} {"cells":>7s} {"weighted FTE":>14s}')
    for k in sorted(acc):
        print(f'    {k[0]:12s} {str(k[1]):>8s} {str(k[2]):>9s} {acc[k][0]:7d} {acc[k][1]:14,.0f}')
    for src in ('interviewed', 'admin'):
        tot = sum(v[1] for k, v in acc.items() if k[0] == src)
        agree = sum(v[1] for k, v in acc.items() if k[0] == src and k[1] == k[2])
        if tot:
            print(f'    {src}: rules agree on {agree / tot:.1%} of weighted FTE')


def section_b(cb):
    print('\n  B. Posted weekly rates (CB9_STDRATE_WKLY_<age>), cells with enrollment in the age group')
    for ra, ages in RATE_BIN_AGES.items():
        col = f'CB9_STDRATE_WKLY_{ra}'
        codes = collections.Counter()
        pos = []
        hi_cells = collections.Counter()
        for c in cb:
            if c['CB9_METH_DATA_SOURCE'] == 2:
                continue
            e = sum(max(c.get(f'CB9_ENRL_{a}') or 0, 0) for a in ages)
            if e <= 0:
                continue
            r = c.get(col)
            if r is None:
                codes['blank'] += 1
            elif r < 0:
                codes[int(r)] += 1
            else:
                codes['>=0'] += 1
                if r > 0:
                    pos.append(r)
                    if r / 40 > 50:
                        flag = c.get(f'CB9_B1_3A_AGERATE_{ra}_R')
                        hi_cells['gets a price' if flag not in (-8, 2) else 'free or no rate'] += 1
        lo, hi = quantile(pos, 0.05), quantile(pos, 0.95)
        n_min, n_max = sum(x == min(pos) for x in pos), sum(x == max(pos) for x in pos)
        n_le, n_ge = sum(x <= lo for x in pos), sum(x >= hi for x in pos)
        print(f'    {ra}: codes {dict(sorted(codes.items(), key=str))}')
        print(f'         positive {len(pos)}; min ${min(pos):,.0f} (n={n_min}), p5 ${lo:,.0f}, '
              f'p95 ${hi:,.0f}, max ${max(pos):,.0f} (n={n_max}); at or beyond p5 {n_le}, p95 {n_ge}')
        print(f'         rates above $50/hour: {sum(hi_cells.values())} {dict(hi_cells)}')


def staff_per_fte(rows):
    num_, den, n = 0.0, 0.0, 0
    for c in rows:
        t = c.get('CB9_STAFF_TYPE_TOTAL_NUM')
        f = sum(v[1] for v in fte_by_age(c).values())
        if t is None or t < 0 or f <= 0:
            continue
        n += 1
        num_ += c['CB9_METH_WEIGHT'] * t
        den += c['CB9_METH_WEIGHT'] * f
    return n, (num_ / den if den else None), den


def section_c(admin, names):
    print('\n  C. Free admin centers (FAMPAY = 2): total staff count per FTE child, by imputation flag')
    free = [c for c in admin if c.get('CB9_B1_3_FAMPAY') == 2]
    if 'CB9_ADMIN_STAFF_IMP' not in names:
        n, r, den = staff_per_fte(free)
        print(f'    no CB9_ADMIN_STAFF_IMP in file; all free admin: used {n}, staff per FTE child {r:.4f}')
        return
    groups = collections.defaultdict(list)
    for c in free:
        groups[c.get('CB9_ADMIN_STAFF_IMP')].append(c)
    print(f'    {"IMP flag":>9s} {"records":>8s} {"used":>6s} {"weighted FTE":>14s} {"staff per FTE child":>20s}')
    for k in sorted(groups, key=lambda z: (z is None, z or 0)):
        n, r, den = staff_per_fte(groups[k])
        rs = f'{r:20.4f}' if r is not None else f'{"--":>20s}'
        print(f'    {str(k):>9s} {len(groups[k]):8d} {n:6d} {den:14,.0f} {rs}')
    n, r, den = staff_per_fte(free)
    print(f'    {"all":>9s} {len(free):8d} {n:6d} {den:14,.0f} {r:20.4f}')


def section_d(admin, interviewed_low_high_fte):
    print('\n  D. Admin centers by family-pay item: centers, weighted headcount and FTE 0-5')
    acc = collections.defaultdict(lambda: [0, 0.0, 0.0])
    for c in admin:
        k = c.get('CB9_B1_3_FAMPAY')
        fte = fte_by_age(c)
        acc[k][0] += 1
        acc[k][1] += c['CB9_METH_WEIGHT'] * sum(v[0] for v in fte.values())
        acc[k][2] += c['CB9_METH_WEIGHT'] * sum(v[1] for v in fte.values())
    for k in sorted(acc, key=lambda z: (z is None, z or 0)):
        lab = FAMPAY_LABEL.get(k, str(k))
        share = f', {acc[k][2] / interviewed_low_high_fte:.1%} of interviewed Low+High FTE' if k != 2 else ''
        print(f'    {lab:15s} {acc[k][0]:5d} centers, headcount {acc[k][1]:11,.0f}, FTE {acc[k][2]:11,.0f}{share}')


# Interviewed Low- plus High-priced FTE enrollment from the R pipeline (crosswalk doc table)
LOW_HIGH_FTE = {'2019': 1_613_967 + 1_670_810, '2024': 1_878_676 + 1_929_731}


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out', default=None)
    args = ap.parse_args()
    with (open(args.out, 'w') if args.out else contextlib.nullcontext(sys.stdout)) as out, \
            contextlib.redirect_stdout(out):
        for year, cfg in YEARS.items():
            names, cb = load(args.raw_root, cfg['cb'], cfg['d'])
            admin = [c for c in cb if c['CB9_METH_DATA_SOURCE'] == 2]
            print(f'\n===== {year}: {len(cb)} center records, {len(admin)} administrative')
            if year == '2019':
                section_a(cb)
            else:
                print('\n  A. Free flag: 2024 has no -8 flag for interviewed centers; 1a uses FAMPAY = 2 only')
            section_b(cb)
            section_c(admin, names)
            section_d(admin, LOW_HIGH_FTE[year])


if __name__ == '__main__':
    main()
