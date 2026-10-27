"""
01_variable_crosswalk.py -- NSECE 2019 -> 2024 variable crosswalk for 1a_nsece_processing.R

For every raw variable stem that src/1_processing/1a_nsece_processing.R reads from the
2019 NSECE, finds the 2024 counterpart (prefix 9 -> 4, e.g. HH9_ -> HH4_), reports loop
indices that were added or dropped, and tabulates the values of the first loop instance
in both years so changes in reserve (missing-value) codes are visible.

Usage (on Bouchet, from the repo root):
    python3 -I src/misc/nsece_2024_checks/01_variable_crosswalk.py [--raw-root PATH] [--out FILE]

Runtime: about a minute (reads the four 2019 and five 2024 public-use TSVs).
Python 3.9+, standard library only.
Results are summarized in docs/nsece_2019_to_2024_crosswalk.md.
"""
import argparse
import collections
import csv
import re
import sys

DEFAULT_RAW_ROOT = '/nfs/roberts/project/pi_nrs36/shared/raw_data/NSECE/v1'

FILES_2019 = {'HH': '2019/37941-0009-Data.tsv', 'CB': '2019/37941-0006-Data.tsv',
              'HB': '2019/37941-0007-Data.tsv', 'WF': '2019/37941-0005-Data.tsv'}
# 2024 household is split into the main file (0005) and the calendar file (0004)
FILES_2024 = {'HH': ['2024/39466-0005-Data.tsv', '2024/39466-0004-Data.tsv'],
              'CB': ['2024/39466-0002-Data.tsv'], 'HB': ['2024/39466-0003-Data.tsv'],
              'WF': ['2024/39466-0001-Data.tsv']}

# Raw 2019 stems referenced in 1a_nsece_processing.R (loop indices stripped)
STEMS_2019 = '''
HH9_METH_CASEID HH9_METH_WEIGHT HH9_HHCOMP_MEMBERS HH9_ECON_INCOME_ANNUAL HH9_REGION HH9_RGENDER
HH9_RSPOUSEPARTNER_LOOP HH9_HHCOMP_NUMMOTHER HH9_HHCOMP_NUMFATHER
HHC9_AGE HHC9_AGE_AT_SEP1 HHC9_GENDER HHC9_METH_WEIGHT
HH9_HHM_AGE HH9_PARCH HH9_WST_PARCH_HI HH9_WST_PARCH_LO HH9_WST_HRS_TOT_PHI HH9_WST_HRS_TOT_PLO
HH9_HRS_WEEK_PROV HH9_HOURS_WEEK_PROV HH9_CARELOC HH9_CHILDREN HH9_D1A_WORK HH9_D3D_UNIT HH9_D3D_WAGE_TC
HH9_HEADSTART HH9_PRIORREL HH9_PUBPREK HH9_REGCARE HH9_SUBSIDY HH9_TYPEOFCARE_AGG HH9_WEEKLY_PAYMENT_TC
CB9_METH_CASEID CB9_METH_WEIGHT CB9_WEIGHT_CLSM CB9_ENRL CB9_ENRL_FT CB9_STDRATE_WKLY CB9_B1_3A_AGERATE
CB9_F15_CHILD_FUND_HS_IND CB9_F16_CHILD_FUND_PK_IND CB9_F3_ASSIST_TC CB9_F3_ENROLL_TC CB9_F3_TEACH_TC
CB9_F3_YOUNGMONTHS_R
HB9_METH_CASEID HB9_METH_WEIGHT HB9_METH_SAMPLE_SOURCE HB9_CAREER_EDUC HB9_ENRL_TOTAL HB9_H2_NUMHELP
HB9_PRGM_PAID HB9_STAFF_EDUC_PRCNT_4YR
WF9_METH_CASEID WF9_METH_WEIGHT WF9_CHAR_EDUC WF9_WORK_FT WF9_WORK_ROLE WF9_WORK_WAGE WF9_WORK_WAGE_IMP
'''.split()


def to_2024(name):
    """HH9_X -> HH4_X, HHC9_X -> HHC4_X, etc."""
    return re.sub(r'^([A-Z]+)9_', r'\g<1>4_', name)


def read_header(raw_root, path):
    with open(f'{raw_root}/{path}') as f:
        return [h.strip('"\r') for h in f.readline().rstrip('\n').split('\t')]


def matches(names, stem, all_stems):
    """Variables belonging to `stem`: the stem itself, stem_<suffix>, or stem<digit>
    (the hours grid is PROV<y>_<x> with no separator). A longer stem in the list
    (e.g. HHC9_AGE_AT_SEP1 vs HHC9_AGE) claims its own variables."""
    longer = [t for t in all_stems if t != stem and t.startswith(stem + '_')]
    out = []
    for n in names:
        if not (n == stem or n.startswith(stem + '_') or re.match(re.escape(stem) + r'[0-9]', n)):
            continue
        if any(n == t or n.startswith(t + '_') for t in longer):
            continue
        out.append(n)
    return out


def value_summary(counter):
    if not counter:
        return 'not read'
    if len(counter) > 25:
        neg = sorted({k for k in counter if k.startswith('-')}, key=float)
        vals = [float(k) for k in counter if re.fullmatch(r'-?[0-9.]+', k) and float(k) >= 0]
        rng = f'{min(vals):g}..{max(vals):g}' if vals else 'none'
        return f'{len(counter)} distinct; reserve codes {neg} (n={sum(counter[k] for k in neg)}); valid range {rng}'
    return ' '.join(f'{k}:{n}' for k, n in sorted(counter.items(), key=lambda kv: (len(kv[0]), kv[0])))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--raw-root', default=DEFAULT_RAW_ROOT)
    ap.add_argument('--out', help='write the report here instead of stdout')
    args = ap.parse_args()
    csv.field_size_limit(10**9)
    out = open(args.out, 'w') if args.out else sys.stdout
    P = lambda *a: print(*a, file=out)

    stems_2024 = [to_2024(s) for s in STEMS_2019]
    h19 = {sv: read_header(args.raw_root, f) for sv, f in FILES_2019.items()}
    h24 = {sv: {f: read_header(args.raw_root, f) for f in fs} for sv, fs in FILES_2024.items()}

    P('# Variable crosswalk: 2019 stem | n vars 2019 | n vars 2024 | 2024 file(s) | missing in 2024 | new in 2024')
    first_instance = {}
    for s in STEMS_2019:
        sv = 'HH' if s.startswith('HH') else s[:2]
        s24 = to_2024(s)
        a = matches(h19[sv], s, STEMS_2019)
        b = {n: f for f, h in h24[sv].items() for n in matches(h, s24, stems_2024)}
        miss = sorted({to_2024(n) for n in a} - set(b))
        new = sorted(set(b) - {to_2024(n) for n in a})
        clip = lambda L: ' '.join(L[:8]) + (f' (+{len(L) - 8} more)' if len(L) > 8 else '')
        files = ','.join(sorted({b[n].split('/')[1][:10] for n in b}))
        P(f'{s} | {len(a)} | {len(b)} | {files} | {clip(miss)} | {clip(new)}')
        if not b:
            key = s24.split('_')[-1]
            cands = [n for h in h24[sv].values() for n in h if key in n][:12]
            P(f'    no 2024 match; candidates containing "{key}": {" ".join(cands)}')
        if a:
            v19 = sorted(a)[0]
            v24 = to_2024(v19) if to_2024(v19) in b else (sorted(b)[0] if b else None)
            first_instance[s] = (sv, v19, v24)

    # Value tabulations for the first loop instance of each stem
    need = collections.defaultdict(set)
    for sv, v19, v24 in first_instance.values():
        need[FILES_2019[sv]].add(v19)
        if v24:
            for f, h in h24[sv].items():
                if v24 in h:
                    need[f].add(v24)
    counts = {}
    for f, cols in need.items():
        with open(f'{args.raw_root}/{f}') as fh:
            r = csv.reader(fh, delimiter='\t')
            h = [x.strip('"') for x in next(r)]
            idx = {c: h.index(c) for c in cols}
            c = {col: collections.Counter() for col in cols}
            for row in r:
                for col, i in idx.items():
                    c[col][row[i].strip('"')] += 1
            counts.update(c)

    P('\n# Values of the first loop instance (unweighted counts)')
    for s, (sv, v19, v24) in first_instance.items():
        P(f'{s}\n   2019 {v19}: {value_summary(counts.get(v19))}\n   2024 {v24}: {value_summary(counts.get(v24))}')
    if args.out:
        out.close()


if __name__ == '__main__':
    main()
