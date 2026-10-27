"""
02_care_types.py -- the household care-type classification on both NSECE years

Re-implements, in Python, the provider classification and primary-care selection in
src/1_processing/1a_nsece_processing.R (ecec_choice, category_priority, pt/ft split at
20 hours) and applies it to the 2019 and 2024 household files, children 0-4, weighted by
the child weight. Also reports:
  - the weighted median net hourly cost of paid regular center care (household side),
    the quantity the hard-coded $4.75 low/high cutoff stands in for;
  - the supply-side FTE-weighted median center price by age bin, as 1a computes it;
  - the TYPEOFCARE_AGG mix of regular arrangements and the K-8 share of under-5s.

This is a replication for planning, not the production code: it follows 1a's rules as
written on the 2024-nsece branch (including treating a DK payment as unpaid), so small
differences from the R output are expected.

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/02_care_types.py [--cutoff 4.75] [--raw-root PATH] [--out FILE]

Runtime: a few minutes. Python 3.9+, standard library only.
Results are summarized in docs/nsece_2019_to_2024_crosswalk.md ("Care types").
"""
import argparse
import collections
import csv
import math
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'

YEARS = {
    '2019': dict(hh='2019/37941-0009-Data.tsv', cb='2019/37941-0006-Data.tsv', p='HH9', c='HHC9', cp='CB9', n_prov=14),
    '2024': dict(hh='2024/39466-0005-Data.tsv', cb='2024/39466-0002-Data.tsv', p='HH4', c='HHC4', cp='CB4', n_prov=15),
}
PRIORITY = ['K-8', 'High-Priced Center-Based', 'Low-Priced Center-Based', 'Unpaid Center-Based',
            'Paid Home-Based', 'Other Paid', 'Unpaid Home-Based', 'Other Unpaid']
AGE_BINS = {'0-1': ['INF', '1YR'], '2-3': ['2YR', '3YR'], '4-5': ['4YR', '5YR']}


def num(x):
    try:
        return float(x.strip('"'))
    except ValueError:
        return None


def status(v, miss_threshold=-1):
    """1a's status recode: >=0 OK; >= miss_threshold -> NA (MISS); otherwise 0 (NIU)."""
    if v is None:
        return None
    if v >= 0:
        return v
    if v >= miss_threshold:
        return None
    return 0.0


def wmedian(pairs):
    pairs = sorted((x, w) for x, w in pairs if x is not None and w and w > 0)
    if not pairs:
        return None
    half, acc = sum(w for _, w in pairs) / 2, 0.0
    for x, w in pairs:
        acc += w
        if acc >= half:
            return x


# R's three-valued logic for the home-based / other split
def r_not(a): return None if a is None else (not a)
def r_or(*a): return True if any(v is True for v in a) else (None if any(v is None for v in a) else False)
def r_and(*a): return False if any(v is False for v in a) else (None if any(v is None for v in a) else True)


def classify(toc, careloc, priorrel, pay, hours, cutoff):
    """One child-provider arrangement -> (ecec_choice, net hourly cost)."""
    paid = pay is not None and pay > 0                      # DK pay -> NA -> unpaid, as in 1a today
    nhc = pay / hours if (pay is not None and hours and hours > 0) else None
    is_k8, is_center, is_home = toc == 6, toc == 4, toc in (1, 2, 3)
    in_home = None if careloc is None else careloc == 1
    prior = None if priorrel is None else priorrel == 1
    home_based = r_and(is_home, r_not(in_home), r_not(prior))
    other = r_and(not is_center and not is_k8, r_or(not is_home, in_home, prior))
    if is_k8:
        return 'K-8', nhc
    if is_center and not paid:
        return 'Unpaid Center-Based', nhc
    if is_center and nhc is not None:
        return ('Low-Priced Center-Based' if nhc < cutoff else 'High-Priced Center-Based'), nhc
    if is_center:
        return None, nhc
    if home_based is True:
        return ('Paid Home-Based' if paid else 'Unpaid Home-Based'), nhc
    if other is True:
        return ('Other Paid' if paid else 'Other Unpaid'), nhc
    return None, nhc


def household_side(raw_root, cfg, cutoff):
    p, c = cfg['p'], cfg['c']
    share = collections.Counter(); n = collections.Counter()
    toc_mix = collections.Counter(); k8 = collections.Counter(); by_age = collections.Counter()
    center_costs = []
    with open(f"{raw_root}/{cfg['hh']}") as fh:
        r = csv.reader(fh, delimiter='\t')
        ix = {name.strip('"'): i for i, name in enumerate(next(r))}
        for row in r:
            g = lambda name: num(row[ix[name]]) if name in ix else None
            for x in range(1, 10):
                age_m, w = g(f'{c}_AGE_{x}'), g(f'{c}_METH_WEIGHT_{x}')
                if age_m is None or age_m < 0 or not w:
                    continue
                age = math.floor(age_m / 12)
                if age >= 5:
                    continue
                by_age[age] += w
                arrangements = []
                for y in range(1, cfg['n_prov'] + 1):
                    if g(f'{p}_CHILDREN_{x}_{y}') != 1:
                        continue
                    regular = status(g(f'{p}_REGCARE_{x}_{y}'), -2)
                    toc = g(f'{p}_TYPEOFCARE_AGG_{x}_{y}')
                    hours = g(f'{p}_HRS_WEEK_PROV{y}_{x}')
                    choice, nhc = classify(toc, status(g(f'{p}_CARELOC_{x}_{y}')), status(g(f'{p}_PRIORREL_{y}')),
                                           status(g(f'{p}_WEEKLY_PAYMENT_TC_{x}_{y}')), hours, cutoff)
                    if regular == 1 and hours and hours > 0:
                        toc_mix[toc] += w
                        if toc == 4 and nhc is not None and nhc > 0:
                            center_costs.append((nhc, w))
                    arrangements.append((regular, choice, hours))
                usable = collections.Counter()
                for regular, choice, hours in arrangements:
                    if regular == 1 and choice is not None and hours and hours > 0:
                        usable[choice] += hours
                if usable:
                    choice, hours = sorted(usable.items(), key=lambda kv: (PRIORITY.index(kv[0]), -kv[1]))[0]
                    intensity = 'pt' if hours < 20 else 'ft'
                elif not arrangements:
                    choice, intensity = 'Parent Only', 'none'
                elif all(reg is None or ch is None for reg, ch, _ in arrangements):
                    choice, intensity = 'NA', 'NA'
                else:
                    choice, intensity = 'Parent Only', 'none'
                share[(choice, intensity)] += w; n[(choice, intensity)] += 1
                if choice == 'K-8':
                    k8[age] += w
    return dict(share=share, n=n, toc_mix=toc_mix, k8=k8, by_age=by_age, center_median=wmedian(center_costs))


def supply_side_medians(raw_root, year, cfg):
    """FTE-weighted median hourly price of paid center care by age bin, as in 1a, with the
    2024 free-care rule (AGERATE -3 & FAMPAY 2) in place of 2019's AGERATE -8."""
    cp = cfg['cp']; acc = collections.defaultdict(list)
    with open(f"{raw_root}/{cfg['cb']}") as fh:
        r = csv.reader(fh, delimiter='\t')
        ix = {name.strip('"'): i for i, name in enumerate(next(r))}
        for row in r:
            g = lambda name: num(row[ix[name]]) if name in ix else None
            w, fampay = g(f'{cp}_METH_WEIGHT'), g(f'{cp}_B1_3_FAMPAY')
            for b, ages in AGE_BINS.items():
                enrl, ft_shares, prices = 0.0, [], []
                for a in ages:
                    e, ft = g(f'{cp}_ENRL_{a}'), g(f'{cp}_ENRL_FT_{a}')
                    rate, flag = g(f'{cp}_STDRATE_WKLY_{a}'), g(f'{cp}_B1_3A_AGERATE_{a}_R')
                    if e is not None and e > 0: enrl += e
                    if ft is not None and ft >= 0: ft_shares.append(ft)
                    if rate is None or e == 0:
                        continue
                    free = (flag == -8) if year == '2019' else (flag == -3 and fampay == 2)
                    if free:
                        prices.append(0.0)
                    elif flag != 2 and rate >= 0:
                        prices.append(rate / 40)
                if not prices or enrl <= 0:
                    continue
                ft_share = (sum(ft_shares) / len(ft_shares) / 100) if ft_shares else 0.0
                fte = enrl * ft_share + 0.5 * enrl * (1 - ft_share)
                acc[b].append((sum(prices) / len(prices), w * fte))
    out = {}
    for b in AGE_BINS:
        total = sum(ww for _, ww in acc[b])
        out[b] = (wmedian([(x, ww) for x, ww in acc[b] if x > 0]),
                  100 * sum(ww for x, ww in acc[b] if x == 0) / total if total else None)
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--cutoff', type=float, default=4.75, help='low/high center cutoff, $/hour (1a hard-codes 4.75)')
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out')
    args = ap.parse_args()
    csv.field_size_limit(10**9)
    out = open(args.out, 'w') if args.out else sys.stdout
    P = lambda *a: print(*a, file=out)
    for year, cfg in YEARS.items():
        hh = household_side(args.raw_root, cfg, args.cutoff)
        tot = sum(hh['share'].values())
        P(f'## {year}: children 0-4, n={sum(hh["n"].values())}, weighted {tot:,.0f}, cutoff ${args.cutoff:.2f}/hr')
        for k in sorted(hh['share'], key=lambda k: (PRIORITY.index(k[0]) if k[0] in PRIORITY else 99, k[1])):
            P(f'  {k[0]:26s} {k[1]:4s} n={hh["n"][k]:5d}  {100 * hh["share"][k] / tot:5.1f}%')
        P('  K-8 share by age (%):', {a: round(100 * hh['k8'][a] / hh['by_age'][a], 1) for a in sorted(hh['by_age'])})
        tm = sum(hh['toc_mix'].values())
        P('  regular arrangements by TYPEOFCARE_AGG (%):', {int(k): round(100 * v / tm, 1) for k, v in sorted(hh['toc_mix'].items())})
        P(f'  household-side weighted median net hourly cost, paid regular center care: ${hh["center_median"]:.2f}')
        for b, (med, free) in supply_side_medians(args.raw_root, year, cfg).items():
            P(f'  supply-side FTE-weighted median price {b}: ${med:.2f}/hr (free share of FTE {free:.1f}%)')
    if args.out:
        out.close()


if __name__ == '__main__':
    main()
