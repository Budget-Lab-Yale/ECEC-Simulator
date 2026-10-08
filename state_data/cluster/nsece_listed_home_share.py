#!/usr/bin/env python3
"""
Listed-home share of Paid Home-Based children -- NSECE 2019 home-based file.

Register check (04_compare_licensing_registers.R) compares register homes --
licensed or registered only -- w/ build Paid Home-Based: paid, no prior
relationship, outside child's home, licensed or not. Different universes.
Factor t puts build on register's universe:

  t = children under school age in LISTED homes (all children, as register
      holds them)
      / non-relationship paid children under school age in listed + unlisted
      paid homes, care in provider's own home (= Paid Home-Based)

  build licensed-home children = build Paid Home-Based x t

NSECE listed stratum = providers drawn from state administrative lists, what
register holds (HB9_PRGM_TYPE 1); unlisted paid = household-screener
providers paid for care (type 2). HB9_PRGM_LOCATION 1 = provider's own home.
Under school age = under 3 + 3 to 5 (holds five-year-olds, as REG fill rates
in 04). Per provider, non-relationship share = 1 - FAMILY_RLTNSHP_NUMCH /
ENRL_TOTAL; unlisted paid: min(PAID_NUMCH, TOTAL - FAMILY) / TOTAL.

SUPPRESSION. NSECE withholds age counts (-2) for listed providers w/ 4 or
fewer children; dropping them drops small homes from numerator. Imputed:
under school age = ENRL_TOTAL x listed stratum's own under-school-age share,
from providers w/ every count. Missing TOTAL or FAMILY -> dropped, counted.
No replicate weights in public file -- n reported, no SE. Regions printed for
context; unlisted-paid n too thin (Northeast 26) to use by region.

NSECE is ICPSR 37941, licensed, cluster only.

    python state_data/cluster/nsece_listed_home_share.py <nsece_2019_dir> state_data/out/nsece_listed_home_share.csv
"""

import collections
import csv
import os
import sys

REGIONS = {'US': 'US', '1': 'Northeast', '2': 'Midwest', '3': 'South', '4': 'West'}

# State licensing thresholds, 2019 (raw/US/US_STATELAW__fcc_home_licensing_
# threshold_register_states__2019.csv): count basis x children at which a home
# must be licensed. KY unrelated 4, NY all 3, VA unrelated 5; neighbours kept
# for context.
THRESHOLDS = [(b, k) for b in ('all', 'unrelated') for k in (1, 3, 4, 5)]


def num(v):
    try:
        z = float(v)
        return z if z >= 0 else None
    except (TypeError, ValueError):
        return None


def main(d, out_path):
    f = os.path.join(d, '37941-0007-Data.tsv')
    rows = list(csv.DictReader(open(f, newline='', encoding='latin-1'), delimiter='\t'))
    print('home-based providers:', len(rows))

    # listed stratum under-school-age share, providers w/ every count
    nu = de = n35 = 0.0
    for x in rows:
        if x['HB9_PRGM_TYPE'] != '1':
            continue
        w, u3, m35, sa = (num(x[k]) for k in ('HB9_METH_WEIGHT', 'HB9_ENRL_NUMCH_UNDER3',
                                               'HB9_ENRL_NUMCH_3TO5', 'HB9_ENRL_NUMCH_SA'))
        if None in (w, u3, m35, sa) or u3 + m35 + sa <= 0:
            continue
        nu += w * (u3 + m35)
        de += w * (u3 + m35 + sa)
        n35 += w * m35
    u5_share = nu / de
    # 3-5 share of under-school-age children -- 04_ takes 5-year-olds out of
    # register home capacity w/ it (national, listed stratum)
    share_3to5 = n35 / nu
    print('listed under-school-age share (imputation): %.4f' % u5_share)
    print('listed 3-5 share of under-school-age: %.4f' % share_3to5)

    A = collections.defaultdict(lambda: [0.0, 0.0, 0.0, 0, 0])
    TK = collections.defaultdict(lambda: [0.0, 0])
    drop, imputed = collections.Counter(), 0
    for x in rows:
        typ, loc = x['HB9_PRGM_TYPE'], x['HB9_PRGM_LOCATION']
        if typ not in ('1', '2'):
            continue
        w, tot, fam = (num(x[k]) for k in ('HB9_METH_WEIGHT', 'HB9_ENRL_TOTAL',
                                           'HB9_ENRL_FAMILY_RLTNSHP_NUMCH'))
        if w is None or tot is None or tot <= 0:
            drop['type %s: no total' % typ] += 1
            continue
        if fam is None:
            drop['type %s: no family count' % typ] += 1
            continue
        u3, m35 = num(x['HB9_ENRL_NUMCH_UNDER3']), num(x['HB9_ENRL_NUMCH_3TO5'])
        if u3 is None or m35 is None:
            u5 = tot * u5_share
            imputed += 1
        else:
            u5 = u3 + m35
        nrel = 1 - min(fam, tot) / tot
        size = {'all': tot, 'unrelated': tot - min(fam, tot)}   # children, all ages
        for g in ('US', x['HB9_REGION']):
            a = A[g]
            if typ == '1':
                a[0] += w * u5
                if loc == '1':
                    a[1] += w * u5 * nrel
                a[3] += 1
                in_home = True
            elif loc == '1':
                paid = num(x['HB9_ENRL_PAID_NUMCH'])
                if paid is None:
                    if g == 'US':
                        drop['type 2: no paid count'] += 1
                    continue
                a[2] += w * u5 * min(paid, tot - min(fam, tot)) / tot
                a[4] += 1
                in_home = paid > 0
            else:
                in_home = False
            # Listed homes a state's rule covers (size >= threshold): t on
            # that state's register universe -- listed homes under threshold
            # hold only voluntary registrants
            if g == 'US' and typ == '1':
                for basis, k in THRESHOLDS:
                    if size[basis] >= k:
                        TK[(basis, k)][0] += w * u5
                        TK[(basis, k)][1] += 1
    print('imputed age counts: %d; dropped %s' % (imputed, dict(drop)))

    out = []
    print('%-10s %12s %12s %13s %6s %6s %7s' % ('region', 'listed_all', 'listed_nrel',
                                                 'unl_paid_nrel', 'n_l', 'n_u', 't'))
    for g in ('US', '1', '2', '3', '4'):
        n, dl, du, nl, nuu = A[g]
        t = n / (dl + du)
        print('%-10s %12.0f %12.0f %13.0f %6d %6d %7.4f' % (REGIONS[g], n, dl, du, nl, nuu, t))
        out.append({'region': REGIONS[g], 'listed_children': round(n),
                    'listed_nonrel_children': round(dl),
                    'unlisted_paid_nonrel_children': round(du),
                    'n_listed': nl, 'n_unlisted_paid': nuu, 't': round(t, 4),
                    'listed_3to5_share': round(share_3to5, 4)})
    with open(out_path, 'w', newline='') as fh:
        wr = csv.DictWriter(fh, fieldnames=list(out[0]))
        wr.writeheader()
        wr.writerows(out)
    print('wrote', out_path)

    # Licensed share by state rule, national: children under school age in
    # homes at or over threshold / same denominator as t
    _, dl, du, _, _ = A['US']
    p = os.path.join(os.path.dirname(out_path) or '.', 'nsece_home_threshold_share.csv')
    with open(p, 'w', newline='') as fh:
        wr = csv.writer(fh)
        wr.writerow(['basis', 'threshold', 'share', 'n_providers'])
        for (basis, k) in THRESHOLDS:
            v, n = TK[(basis, k)]
            print('threshold %-9s %d: share %.4f (n %d)' % (basis, k, v / (dl + du), n))
            wr.writerow([basis, k, round(v / (dl + du), 4), n])
    print('wrote', p)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    main(sys.argv[1], sys.argv[2])
