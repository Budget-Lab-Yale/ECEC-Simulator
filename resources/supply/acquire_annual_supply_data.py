"""Acquire annual BEA parities and BLS occupation extracts; preserve source hashes.

Usage: python resources/supply/acquire_annual_supply_data.py [--rpp-only]
Raw archives live in resources/bea and resources/bls_oews.
Download URLs and hashes live beside this script; existing archives are reused.
Requires lxml for streaming BLS XLSX archives.
"""
import argparse
import csv
import hashlib
import io
import json
import re
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RAW = ROOT / 'resources'
OUT = ROOT / 'resources/supply'


def download(url, name, manifest):
    """Reuse downloaded bytes; record source URL, retrieval time, SHA256."""
    if name in ('SARPP.zip', 'MARPP.zip'):
        kind = 'state' if name == 'SARPP.zip' else 'metro'
        matches = sorted((RAW / 'bea').glob(f'US_BEA__regional_price_parities_{kind}__*.zip'))
        path = matches[-1] if matches else RAW / 'bea' / name
    else:
        year = re.search(r'oesm(\d{2})all', name)
        path = RAW / 'bls_oews' / (f'US_BLS_OEWS__all_data__20{year.group(1)}.zip' if year else name)
    for folder in [RAW.parent, ROOT, ROOT.parent]:
        candidate = folder / name
        if not path.exists() and candidate.is_file():
            path = candidate
    if not path.exists():
        request = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        data = urllib.request.urlopen(request, timeout=45).read()
        zipfile.ZipFile(io.BytesIO(data)).testzip()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
    data = path.read_bytes()
    manifest[name] = dict(url=url, sha256=hashlib.sha256(data).hexdigest(),
                          checked_at=datetime.now(timezone.utc).isoformat())
    return zipfile.ZipFile(io.BytesIO(data))


def write_csv(name, rows):
    """Write deterministic extract; reject empty output."""
    if not rows:
        raise ValueError(f'No rows for {name}')
    folder = RAW / ('bea' if name == 'regional_price_parities_annual.csv' else 'bls_oews')
    with (folder / name).open('w', newline='', encoding='utf-8') as handle:
        writer = csv.DictWriter(handle, fieldnames=rows[0].keys())
        writer.writeheader()
        writer.writerows(rows)


def rpp_extract(manifest):
    """All-items parity, all available years; states and NYC MSA."""
    baseline = list(csv.DictReader((RAW / 'bea' / 'US_BEA__regional_price_parities_all_items__2019.csv')
                                   .open(encoding='utf-8-sig')))
    names = {r['bea_name']: r['geo'] for r in baseline if r['bea_geography'] == 'state'}
    records = {}
    for table in ['SARPP', 'MARPP']:
        archive = download(f'https://apps.bea.gov/regional/zip/{table}.zip', table + '.zip', manifest)
        filename = next(n for n in archive.namelist() if n.startswith(table + '_') and n.endswith('.csv'))
        rows = csv.DictReader(io.StringIO(archive.read(filename).decode('utf-8-sig')))
        for row in rows:
            if not row.get('GeoName') or not row.get('Description'):
                continue
            name = row['GeoName'].strip()
            fips = row['GeoFIPS'].strip().strip('"')
            geo = names.get(name) if table == 'SARPP' else ('NYC' if fips == '35620' else None)
            description = row['Description'].strip()
            metric = {'RPPs: All items': 'all_items'}.get(description)
            if not geo or not metric:
                continue
            for year, value in row.items():
                if not re.fullmatch(r'\d{4}', year or ''):
                    continue
                try:
                    number = float(value)
                except ValueError:
                    continue
                if number <= 0:
                    raise ValueError('Nonpositive BEA parity')
                records.setdefault((geo, int(year)), dict(geo=geo, year=int(year)))[metric] = number
    panel = [records[key] for key in sorted(records)]
    if any(set(r) != {'geo', 'year', 'all_items'} for r in panel):
        raise ValueError('Incomplete annual BEA all-items parity')
    for year in sorted({r['year'] for r in panel}):
        if sum(r['year'] == year for r in panel) != 52:
            raise ValueError(f'Incomplete BEA geography coverage in {year}')
    write_csv('regional_price_parities_annual.csv', panel)
    print(f'BEA: {len(panel)} geography/year rows, {min(r["year"] for r in panel)}--'
          f'{max(r["year"] for r in panel)}', flush=True)


def workbook_rows(data):
    """Stream XLSX XML; avoid constructing millions of Python cell objects."""
    from lxml import etree
    ns = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'
    with zipfile.ZipFile(io.BytesIO(data)) as book:
        shared = []
        if 'xl/sharedStrings.xml' in book.namelist():
            with book.open('xl/sharedStrings.xml') as handle:
                for _, elem in etree.iterparse(handle, events=('end',), tag=ns + 'si'):
                    shared.append(''.join(elem.itertext()))
                    elem.clear()
                    while elem.getprevious() is not None:
                        del elem.getparent()[0]
        for sheet in book.namelist():
            if not re.fullmatch(r'xl/worksheets/sheet\d+\.xml', sheet):
                continue
            header = None
            with book.open(sheet) as handle:
                for _, elem in etree.iterparse(handle, events=('end',), tag=ns + 'row'):
                    values = {}
                    for cell in elem:
                        column = re.sub(r'\d+', '', cell.get('r', ''))
                        value = cell.find(ns + 'v')
                        text = value.text if value is not None else ''
                        if cell.get('t') == 's' and text:
                            text = shared[int(text)]
                        elif cell.get('t') == 'inlineStr':
                            text = ''.join(cell.itertext())
                        values[column] = text
                    if header is None:
                        cols = {k: str(v).strip().lower() for k, v in values.items()}
                        if 'occ_code' in cols.values() and 'h_median' in cols.values():
                            header = cols
                    else:
                        yield {key: values.get(col, '') for col, key in header.items()}
                    elem.clear()
                    while elem.getprevious() is not None:
                        del elem.getparent()[0]


def wage_extract(manifest, end_year):
    """Comparable occupation codes, national/state/MSA hourly medians; preserve suppression."""
    state_names = {r['bea_name']: r['geo'] for r in csv.DictReader(
        (RAW / 'bea' / 'US_BEA__regional_price_parities_all_items__2019.csv').open(encoding='utf-8-sig'))
                   if r['bea_geography'] == 'state'}
    occupations = {'00-0000', '39-9011', '25-2011', '11-9031'}
    result = []
    for year in range(2019, end_year + 1):
        name = f'oesm{year % 100:02d}all.zip'
        archive = download('https://www.bls.gov/oes/special-requests/' + name, name, manifest)
        for filename in archive.namelist():
            if not filename.lower().endswith('.xlsx'):
                continue
            for row in workbook_rows(archive.read(filename)):
                code = str(row.get('occ_code', '')).strip()
                if code not in occupations:
                    continue
                naics = str(row.get('naics', '000000')).split('.')[0].zfill(6)
                owner = str(row.get('own_code', '1235')).split('.')[0]
                if naics != '000000' or owner != '1235':
                    continue
                area = str(row.get('area', '')).split('.')[0].zfill(5)
                title = str(row.get('area_title', '')).strip()
                geo = state_names.get(title)
                if str(row.get('area_type', '')).split('.')[0] == '1' or area == '00000':
                    geo = 'US'
                elif area == '35620':
                    geo = 'NYC'
                if geo:
                    result.append(dict(year=year, geo=geo, occ_code=code,
                                       h_median=row['h_median'], area_title=title,
                                       method='MB3' if year >= 2021 else 'legacy'))
        print(f'BLS {year}: extracted', flush=True)
    keys = [(r['year'], r['geo'], r['occ_code']) for r in result]
    if len(keys) != len(set(keys)):
        raise ValueError('Duplicate BLS occupation cells; inspect workbook aggregation')
    for year in range(2019, end_year + 1):
        if len({r['geo'] for r in result if r['year'] == year}) != 53:
            raise ValueError(f'Incomplete BLS geography coverage in {year}')
    write_csv('childcare_occupation_wages_annual.csv', sorted(result, key=lambda r: (
        r['year'], r['geo'], r['occ_code'])))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rpp-only', action='store_true')
    parser.add_argument('--end-year', type=int, default=2025)
    parser.add_argument('--raw-dir', type=Path, help='Resource root containing bea/ and bls_oews/.')
    args = parser.parse_args()
    if args.raw_dir is not None:
        RAW = args.raw_dir.resolve()
    RAW.mkdir(parents=True, exist_ok=True)
    manifest_path = OUT / 'annual_supply_sources.json'
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
    try:
        rpp_extract(manifest)
        if not args.rpp_only:
            wage_extract(manifest, args.end_year)
    finally:
        manifest_path.write_text(json.dumps(manifest, indent=2), encoding='utf-8')
