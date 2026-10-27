"""
05_price_cutoff.py -- recompute the low/high center price cutoff ($4.75/hr in 1a) on both years

1a_nsece_processing.R hard-codes $4.75/hr as the line between low- and high-priced center care.
Borowsky et al. (2022, NBER w30140) define it as the median hourly expenditure on the youngest
child in the 2019 NSECE: total fees paid / total hours of paid care, over providers with positive
fees. Some details are not pinned down in the paper, so this script computes the plausible
variants on 2019, to see which reproduces $4.75, and the same variants on 2024:

  providers   all paid providers, or paid center care only (TYPEOFCARE_AGG = 4)
  regular     regular providers only (REGCARE = 1, >= 5 hrs/wk), or all
  weight      child weight (HHC*_METH_WEIGHT_X) or household weight (HH*_METH_WEIGHT)

The youngest child is the youngest child under 5 (age in months < 60) in the household. Payments
with a reserve code (e.g. -1 DK) are excluded, as Borowsky et al. drop households that report
paying but lack an amount. Medians are weighted (the smallest value at which cumulative weight
reaches half).

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/05_price_cutoff.py [--raw-root PATH] [--out FILE]

Runtime: a couple of minutes. Python 3.9+, standard library only.
"""
import argparse
import csv
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'
YEARS = {
    '2019': dict(hh='2019/37941-0009-Data.tsv', p='HH9', c='HHC9', n_prov=14),
    '2024': dict(hh='2024/39466-0005-Data.tsv', p='HH4', c='HHC4', n_prov=15),
}


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


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out')
    args = ap.parse_args()
    csv.field_size_limit(10**9)
    out = open(args.out, 'w') if args.out else sys.stdout
    P = lambda *a: print(*a, file=out)

    for year, cfg in YEARS.items():
        p, c = cfg['p'], cfg['c']
        samples = {}   # (providers, regular) -> list of (hourly cost, child weight, household weight)
        with open(f"{args.raw_root}/{cfg['hh']}") as fh:
            r = csv.reader(fh, delimiter='\t')
            ix = {name.strip('"'): i for i, name in enumerate(next(r))}
            for row in r:
                g = lambda name: num(row[ix[name]]) if name in ix else None
                kids = [(g(f'{c}_AGE_{x}'), x) for x in range(1, 10)]
                kids = [(a, x) for a, x in kids if a is not None and 0 <= a < 60 and g(f'{c}_METH_WEIGHT_{x}')]
                if not kids:
                    continue
                _, x = min(kids)
                w_child, w_hh = g(f'{c}_METH_WEIGHT_{x}'), g(f'{p}_METH_WEIGHT')
                for providers in ('all paid', 'center only'):
                    for regular_only in (True, False):
                        fees = hours = 0.0
                        for y in range(1, cfg['n_prov'] + 1):
                            if g(f'{p}_CHILDREN_{x}_{y}') != 1:
                                continue
                            if providers == 'center only' and g(f'{p}_TYPEOFCARE_AGG_{x}_{y}') != 4:
                                continue
                            if regular_only and g(f'{p}_REGCARE_{x}_{y}') != 1:
                                continue
                            pay, hrs = g(f'{p}_WEEKLY_PAYMENT_TC_{x}_{y}'), g(f'{p}_HRS_WEEK_PROV{y}_{x}')
                            if pay is None or pay <= 0 or not hrs or hrs <= 0:
                                continue   # positive fees only; reserve codes (DK etc.) excluded
                            fees += pay; hours += hrs
                        if hours > 0:
                            samples.setdefault((providers, regular_only), []).append((fees / hours, w_child, w_hh))
        P(f'## {year}: median hourly expenditure on the youngest child under 5 (paid care only)')
        for (providers, regular_only), L in sorted(samples.items()):
            m_child = wmedian([(v, wc) for v, wc, _ in L])
            m_hh = wmedian([(v, wh) for v, _, wh in L])
            P(f'  {providers:11s} | {"regular only" if regular_only else "all providers":13s} | n={len(L):5d} | '
              f'child-weighted ${m_child:.2f} | household-weighted ${m_hh:.2f}')
    if args.out:
        out.close()


if __name__ == '__main__':
    main()
