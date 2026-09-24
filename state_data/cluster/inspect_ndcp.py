#!/usr/bin/env python3
"""
What is in the DOL National Database of Childcare Prices, and does it mark
part-time against full-time?

The build places every child at 10 or 40 hrs/week on the simulator's grid. A
price with no part-time / full-time marker cannot be put on that grid without
an assumption, so this reports the answer explicitly.

STREAMS the xlsx. An earlier version called ET.fromstring on the sheet XML and
was OOM-killed at 8 GB on the 92 MB workbook -- the decompressed sheet is far
larger than the file. Two streaming passes: read the header row's shared-string
indices, then pull just those strings.
"""

import re
import sys
import zipfile
from xml.etree import ElementTree as ET

NS = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'

PT_PAT = re.compile(
    r'part[\s_-]?time|full[\s_-]?time|\bpt\b|\bft\b|hour|week|day|month|'
    r'annual|year|full[\s_-]?day|half[\s_-]?day', re.I)


def first_row_cells(z, sheet_path):
    """(type, value) per cell of the first row, streaming."""
    out = []
    with z.open(sheet_path) as fh:
        for event, el in ET.iterparse(fh, events=('end',)):
            if el.tag == NS + 'c':
                v = el.find(NS + 'v')
                out.append((el.get('t'), v.text if v is not None else None))
                el.clear()
            elif el.tag == NS + 'row':
                break
    return out


def pull_strings(z, wanted):
    """Only the shared strings at the given indices, streaming."""
    got = {}
    if not wanted:
        return got
    hi = max(wanted)
    with z.open('xl/sharedStrings.xml') as fh:
        i = 0
        for event, el in ET.iterparse(fh, events=('end',)):
            if el.tag != NS + 'si':
                continue
            if i in wanted:
                got[i] = ''.join(t.text or '' for t in el.iter(NS + 't'))
            el.clear()
            i += 1
            if i > hi:
                break
    return got


def sheet_names(z):
    with z.open('xl/workbook.xml') as fh:
        root = ET.parse(fh).getroot()
    return [s.get('name') for s in root.iter(NS + 'sheet')]


def report(path):
    print('=' * 70, flush=True)
    print(path, flush=True)
    print('=' * 70, flush=True)
    z = zipfile.ZipFile(path)
    names = sheet_names(z)
    print('sheets: %s' % names, flush=True)

    sheets = sorted(n for n in z.namelist()
                    if n.startswith('xl/worksheets/sheet'))
    for i, sp in enumerate(sheets):
        cells = first_row_cells(z, sp)
        want = set()
        for t, v in cells:
            if t == 's' and v is not None:
                try:
                    want.add(int(v))
                except ValueError:
                    pass
        strs = pull_strings(z, want)

        hdr = []
        for t, v in cells:
            if v is None:
                continue
            if t == 's':
                try:
                    hdr.append(strs.get(int(v), ''))
                except ValueError:
                    hdr.append('')
            else:
                hdr.append(v)
        hdr = [h for h in hdr if h]

        label = names[i] if i < len(names) else sp
        print('\n--- sheet %d (%s): %d header cells'
              % (i + 1, label, len(hdr)), flush=True)
        for h in hdr:
            print('      %s' % h, flush=True)
        hits = [h for h in hdr if PT_PAT.search(h)]
        print('    TIME / DURATION COLUMNS: %s'
              % (hits if hits else 'NONE'), flush=True)
    z.close()


if __name__ == '__main__':
    for p in sys.argv[1:]:
        try:
            report(p)
        except Exception as e:
            print('%s: FAILED %r' % (p, e), flush=True)
        print(flush=True)
