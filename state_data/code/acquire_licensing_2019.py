"""Retrieve registered public source candidates; no automatic rule verification.

Usage: python state_data/code/acquire_licensing_2019.py
Requires pypdf for PDF extraction. Existing source files are never overwritten.
"""

import csv
import hashlib
import io
import json
import re
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from datetime import date
from pathlib import Path

from pypdf import PdfReader


ROOT = Path(__file__).resolve().parents[2]
WORK = ROOT / 'state_data/work/licensing'


def acquire(row):
    """Save original PDF bytes and page-delimited text, reporting failures."""
    folder = ROOT / 'state_data/raw' / row['state']
    if row['state'] == 'NY' and (ROOT / 'state_data/raw/NYS').is_dir():
        folder = ROOT / 'state_data/raw/NYS'
    folder.mkdir(parents=True, exist_ok=True)
    destination = folder / (row['source_id'] + '.pdf')
    record = {**row, 'checked_on': date.today().isoformat()}
    try:
        if destination.exists():
            record['retrieval_mode'] = 'existing_file'
            content = destination.read_bytes()
            final_url = row['url']
        else:
            record['retrieval_mode'] = 'http_download'
            request = urllib.request.Request(row['url'], headers={
                'User-Agent': 'Mozilla/5.0 (historical child care policy research)'})
            with urllib.request.urlopen(request, timeout=35) as response:
                content = response.read()
                final_url = response.url
            record['retrieved_on'] = date.today().isoformat()
        if not content.lstrip().startswith(b'%PDF'):
            raise ValueError('Response is not a PDF; inspect source landing page')
        reader = PdfReader(io.BytesIO(content))
        pages = [page.extract_text() or '' for page in reader.pages]
        if not destination.exists():
            destination.write_bytes(content)
        text = '\n'.join(f'\n=== PDF PAGE {i + 1} ===\n{page}'
                         for i, page in enumerate(pages))
        WORK.mkdir(parents=True, exist_ok=True)
        (WORK / (row['source_id'] + '.txt')).write_text(
            text, encoding='utf-8')
        # Candidate section only; full text remains available for date review.
        hits = [i for i, page in enumerate(pages)
                if re.search(r'5\.2\.1\b', page)
                and re.search(r'ratio|group size|caregiver', page, re.I)]
        selected = set()
        for start in hits:
            for i in range(start, min(start + 22, len(pages))):
                if i > start and re.search(r'5\.2\.2\b|5\.3\s', pages[i]):
                    break
                selected.add(i)
        excerpt = '\n'.join(f'\n=== PDF PAGE {i + 1} ===\n{pages[i]}'
                            for i in sorted(selected))
        (WORK / (row['source_id'] + '__staffing.txt')).write_text(
            excerpt, encoding='utf-8')
        record.update(local_file=destination.relative_to(ROOT).as_posix(),
                      final_url=final_url, sha256=hashlib.sha256(content).hexdigest(),
                      pages=len(pages), characters=len(text), staffing_pages=len(selected))
    except Exception as error:
        record['error'] = str(error)
    print(f"{row['state']}: " + (record.get('error') or
          f"{record['pages']} PDF pages, {record['staffing_pages']} candidate staffing pages"),
          flush=True)
    return record


if __name__ == '__main__':
    manifest = ROOT / 'resources/licensing/licensing_2019_candidates.csv'
    with manifest.open(encoding='utf-8-sig', newline='') as handle:
        candidates = list(csv.DictReader(handle))
    with ThreadPoolExecutor(max_workers=6) as pool:
        records = list(pool.map(acquire, candidates))
    WORK.mkdir(parents=True, exist_ok=True)
    (WORK / 'licensing_2019_acquisition.json').write_text(
        json.dumps(records, indent=2), encoding='utf-8')
    print(f"Retrieved {sum('error' not in row for row in records)}/{len(records)} candidates")
