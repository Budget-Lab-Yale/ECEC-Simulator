"""Scan the R here for one specific defect: a name shadowed inside summarise().

dplyr evaluates summarise() arguments in order, so

    summarise(slots = sum(slots), matched = sum(slots[m]))

replaces the column `slots` with the scalar it just computed, and the second
expression indexes that scalar. m[1] TRUE gives the scalar back, so the rate
reads 100%. m[1] FALSE gives numeric(0), so it reads 0%. Nothing errors.

Run it from anywhere:  python state_data/tests/check_summarise_shadowing.py
Exit status is 1 if anything is found, so it can gate a commit.
"""
import io
import os
import re
import glob
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REPO = os.path.dirname(ROOT)
# code/ and cluster/ hold the build since the September reorganisation; the
# list once stopped at states/ and tests/, so the build itself went unscanned.
# resources/ holds the supply and employment builders the state files use.
files = sorted(glob.glob(os.path.join(ROOT, '*.R')) +
               glob.glob(os.path.join(ROOT, 'code', '*.R')) +
               glob.glob(os.path.join(ROOT, 'cluster', '*.R')) +
               glob.glob(os.path.join(ROOT, 'states', '*.R')) +
               glob.glob(os.path.join(ROOT, 'tests', '*.R')) +
               glob.glob(os.path.join(REPO, 'resources', '**', '*.R'), recursive=True) +
               sys.argv[1:])   # extra files, e.g. to test the scanner itself

print('scanning %d R files for summarise()/mutate() name shadowing\n' % len(files))

bad = 0
for f in files:
    s = io.open(f, encoding='utf-8').read()

    # Strip comments first. These scripts document the defect they guard
    # against, and the worked example in the docstring is not code.
    lines = []
    for ln in s.split('\n'):
        q = ln.find('#')
        if q >= 0 and ln.count('"', 0, q) % 2 == 0 and ln.count("'", 0, q) % 2 == 0:
            ln = ln[:q]
        lines.append(ln)
    s = '\n'.join(lines)

    for m in re.finditer(r'\b(summarise|summarize|mutate)\s*\(', s):
        start = m.end()
        depth, i = 1, start
        while i < len(s) and depth:
            if s[i] == '(':
                depth += 1
            elif s[i] == ')':
                depth -= 1
            i += 1
        block = s[start:i - 1]

        for name in re.findall(r'(?:^|,)\s*([A-Za-z_][A-Za-z0-9_.]*)\s*=(?!=)', block):
            pos = re.search(r'\b%s\s*=(?!=)' % re.escape(name), block)
            if not pos:
                continue
            # rest = everything after this argument's own expression
            nxt = re.search(r',\s*[A-Za-z_][A-Za-z0-9_.]*\s*=(?!=)', block[pos.end():])
            rest = block[pos.end() + nxt.start():] if nxt else ''
            ln = s[:start].count('\n') + 1
            if re.search(r'\b%s\s*\[' % re.escape(name), block[pos.end():]):
                print('  %s:%d  %s() reassigns %r then indexes it'
                      % (os.path.basename(f), ln, m.group(1), name))
                bad += 1
            # summarise() only: reassigned name inside a later aggregate --
            # sum(h * w) / sum(w) after w = sum(w) divides by the scalar.
            # mutate() reuse is normal, so not flagged there.
            elif m.group(1) != 'mutate' and re.search(
                    r'\b(sum|mean|median|min|max|weighted\.mean)\s*\([^()]*\b%s\b'
                    % re.escape(name), rest):
                print('  %s:%d  %s() reassigns %r then aggregates it again'
                      % (os.path.basename(f), ln, m.group(1), name))
                bad += 1

if bad:
    print('\n%d occurrence(s) to fix' % bad)
    sys.exit(1)
print('CLEAN -- no shadowing found')
