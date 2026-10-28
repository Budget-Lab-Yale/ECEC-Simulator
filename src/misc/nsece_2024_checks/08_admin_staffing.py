"""
08_admin_staffing.py -- do administrative-data centers carry director staff counts, and how
do they staff compared with interviewed centers? Both years.

Reports, from the center file only:
  A. Reserve-code mix of the director staff items (teachers and aides 0-5, count and percent
     full time) for administrative (CB9_METH_DATA_SOURCE = 2) against interviewed centers.
  B. Which other center variables administrative records fill (share of records >= 0).
  C. Staff per child enrolled 0-5, by group: administrative; interviewed public-school
     (METH_PUBSCHFLAG = 1); interviewed not public-school with no family paying
     (B1_3_FAMPAY = 2, close to 1a's Unpaid sector); interviewed families pay.
     Ratios use director staff FTE (full time 1, part time 0.5, as 1a option 'supply')
     over enrollment 0-5, per headcount and per FTE child. Weighted by CB9_METH_WEIGHT.
  D. Total staff count (CB9_STAFF_TYPE_TOTAL_NUM, the only staff item 2024 admin records
     carry) per child, by the same groups, and the 0-5 classroom share of it.

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/08_admin_staffing.py [--raw-root PATH]

Runtime: under a minute. Python 3.9+, standard library only.
"""
import argparse
import collections
import csv
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {
    '2019': dict(cb='2019/37941-0006-Data.tsv', d='9'),
    '2024': dict(cb='2024/39466-0002-Data.tsv', d='4'),
}
AGES = ['INF', '1YR', '2YR', '3YR', '4YR', '5YR']
STAFF = dict(n_teach='CB9_STAFF_TYPE_TCHR_NUM_0TO5', n_aide='CB9_STAFF_TYPE_AIDEASST_NUM0TO5',
             pft_teach='CB9_STAFF_TYPE_TCHR_PRCNTFT_0TO5', pft_aide='CB9_STAFF_TYPE_AIDEASST_PFT0TO5')


def num(x):
    try:
        return float(x.strip('"'))
    except ValueError:
        return None


def load(raw_root, path, d):
    """Rows as dicts with 2019-style names (CB9_) and numeric values; all columns."""
    with open(f'{raw_root}/{path}') as fh:
        r = csv.reader(fh, delimiter='\t')
        header = [h.strip('"') for h in next(r)]
        names = ['CB9_' + h[4:] if h.startswith(f'CB{d}_') else h for h in header]
        return names, [{n: num(v) for n, v in zip(names, row)} for row in r]


def staff_fte(c):
    v = {k: c.get(col) for k, col in STAFF.items()}
    if any(x is None or x < 0 for x in v.values()):
        return None
    return (v['n_teach'] * (v['pft_teach'] / 100 + 0.5 * (1 - v['pft_teach'] / 100)) +
            v['n_aide'] * (v['pft_aide'] / 100 + 0.5 * (1 - v['pft_aide'] / 100)))


def enrollment(c, admin):
    """(headcount 0-5, FTE 0-5); FTE counts full time 1, part time 0.5."""
    head = c.get('CB9_ENRL_0TO5')
    if head is None or head < 0:
        head = sum(max(c.get(f'CB9_ENRL_{a}') or 0, 0) for a in AGES)
    if admin and all((c.get(f'CB9_ENRL_{a}') or -8) < 0 for a in AGES):
        # 1a harmonize rule for administrative records: full-time share from ADMIN_ items
        ft, pt = c.get('CB9_ADMIN_FULLTIME_CARE'), c.get('CB9_ADMIN_PARTTIME_CARE')
        s = 100 if (ft == 1 and pt == 2) else 0 if (ft == 2 and pt == 1) else 50
        return head, head * (s / 100 + 0.5 * (1 - s / 100))
    fte = 0.0
    for a in AGES:
        e, f = c.get(f'CB9_ENRL_{a}'), c.get(f'CB9_ENRL_FT_{a}')
        if e is None or e <= 0:
            continue
        f = f if f is not None and f >= 0 else 50
        fte += e * (f / 100 + 0.5 * (1 - f / 100))
    return head, fte


def group(c):
    if c['CB9_METH_DATA_SOURCE'] == 2:
        return 'admin'
    if c.get('CB9_METH_PUBSCHFLAG') == 1:
        return 'interviewed, public school'
    if c.get('CB9_B1_3_FAMPAY') == 2:
        return 'interviewed, not public school, no family pays'
    return 'interviewed, not public school, families pay'


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out', help='write report here instead of the terminal')
    args = ap.parse_args()
    if args.out:
        sys.stdout = open(args.out, 'w')
    for year, cfg in YEARS.items():
        names, cb = load(args.raw_root, cfg['cb'], cfg['d'])
        admin = [c for c in cb if c['CB9_METH_DATA_SOURCE'] == 2]
        inter = [c for c in cb if c['CB9_METH_DATA_SOURCE'] == 1]
        print(f'\n===== {year}: {len(cb)} center records, {len(admin)} administrative, {len(inter)} interviewed')
        missing = [col for col in STAFF.values() if col not in names]
        if missing:
            print('  staff items not in file:', missing)
            continue

        print('\n  A. Director staff items, reserve-code mix (unweighted record counts)')
        for col in STAFF.values():
            for lab, rows in (('admin', admin), ('interviewed', inter)):
                codes = collections.Counter('>=0' if (c[col] is not None and c[col] >= 0) else c[col] for c in rows)
                print(f'    {col:36s} {lab:12s} {dict(codes)}')
        ok = sum(staff_fte(c) is not None for c in admin)
        print(f'    administrative records with all four staff items valid: {ok} of {len(admin)}')

        print('\n  B. Variables administrative records fill (share of records >= 0, items above 50%)')
        filled = []
        for n in names:
            if not n.startswith('CB9_') or n.startswith('CB9_METH_'):
                continue
            vals = [c[n] for c in admin if c[n] is not None]
            share = sum(v >= 0 for v in vals) / len(admin) if admin else 0
            if share > 0.5:
                filled.append(f'{n} {share:.0%}')
        print('    ' + '\n    '.join(filled) if filled else '    none')

        print('\n  C. Director staff FTE per child enrolled 0-5 (weighted by CB9_METH_WEIGHT)')
        acc = collections.defaultdict(lambda: dict(n=0, s=0.0, h=0.0, f=0.0, w=0.0, nall=0))
        for c in cb:
            g = group(c)
            a = acc[g]
            a['nall'] += 1
            s = staff_fte(c)
            head, fte = enrollment(c, g == 'admin')
            if s is None or not head or head <= 0 or fte <= 0:
                continue
            w = c['CB9_METH_WEIGHT']
            a['n'] += 1
            a['s'] += s * w
            a['h'] += head * w
            a['f'] += fte * w
            a['w'] += w
        print(f'    {"group":48s} {"records":>8s} {"used":>6s} {"wtd centers":>12s} {"per child":>10s} {"per FTE child":>14s}')
        for g in ('admin', 'interviewed, public school', 'interviewed, not public school, no family pays',
                  'interviewed, not public school, families pay'):
            a = acc[g]
            if a['h'] > 0:
                print(f'    {g:48s} {a["nall"]:8d} {a["n"]:6d} {a["w"]:12,.0f} {a["s"] / a["h"]:10.4f} {a["s"] / a["f"]:14.4f}')
            else:
                print(f'    {g:48s} {a["nall"]:8d} {a["n"]:6d}  no usable records')

        print('\n  D. Total staff count (CB9_STAFF_TYPE_TOTAL_NUM) per child enrolled 0-5, and the')
        print('     0-5 classroom staff FTE share of it (interviewed only; weighted by CB9_METH_WEIGHT)')
        if 'CB9_ADMIN_STAFF_IMP' in names:
            print('    CB9_ADMIN_STAFF_IMP among admin:', dict(collections.Counter(c['CB9_ADMIN_STAFF_IMP'] for c in admin)))
        acc = collections.defaultdict(lambda: dict(n=0, t=0.0, h=0.0, f=0.0, n5=0, s5=0.0, t5=0.0))
        for c in cb:
            g = group(c)
            tot = c.get('CB9_STAFF_TYPE_TOTAL_NUM')
            head, fte = enrollment(c, g == 'admin')
            if tot is None or tot < 0 or not head or head <= 0 or fte <= 0:
                continue
            w = c['CB9_METH_WEIGHT']
            a = acc[g]
            a['n'] += 1
            a['t'] += tot * w
            a['h'] += head * w
            a['f'] += fte * w
            s = staff_fte(c)
            if s is not None and tot > 0:
                a['n5'] += 1
                a['s5'] += s * w
                a['t5'] += tot * w
        print(f'    {"group":48s} {"used":>6s} {"total/child":>12s} {"total/FTE child":>16s} {"used":>6s} {"0-5 FTE / total":>16s}')
        for g in ('admin', 'interviewed, public school', 'interviewed, not public school, no family pays',
                  'interviewed, not public school, families pay'):
            a = acc[g]
            if a['h'] <= 0:
                print(f'    {g:48s} {a["n"]:6d}  no usable records')
                continue
            share = f'{a["s5"] / a["t5"]:16.3f}' if a['t5'] > 0 else f'{"--":>16s}'
            print(f'    {g:48s} {a["n"]:6d} {a["t"] / a["h"]:12.4f} {a["t"] / a["f"]:16.4f} {a["n5"]:6d} {share}')


if __name__ == '__main__':
    main()
