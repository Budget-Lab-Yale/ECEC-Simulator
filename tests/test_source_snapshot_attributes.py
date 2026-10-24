"""Keep registered source bytes stable under Git checkout on Windows."""
import csv
from pathlib import Path
import subprocess

root = Path(__file__).resolve().parents[1]
with (root / 'resources/licensing/licensing_2019_sources.csv').open(
        encoding='utf-8-sig', newline='') as handle:
    paths = sorted({row['local_file'].replace('\\', '/') for row in csv.DictReader(handle)})
result = subprocess.run(
    ['git', 'check-attr', '-z', 'text', '--stdin'], cwd=root,
    input=b'\0'.join(path.encode() for path in paths) + b'\0',
    stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True)
fields = result.stdout.decode().rstrip('\0').split('\0')
assert len(fields) == 3 * len(paths), 'Incomplete Git attribute results'
unsafe = [fields[i] for i in range(0, len(fields), 3) if fields[i + 2] != 'unset']
assert not unsafe, 'SHA-256 sources need -text before commit: ' + ', '.join(unsafe)
print(f'PASS: all {len(paths)} registered source files protected from line-ending conversion.')
