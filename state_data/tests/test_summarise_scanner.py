"""Regression cases for nested keywords, self references, real shadowing."""
import subprocess
import sys
import tempfile
from pathlib import Path

scanner = Path(__file__).with_name('check_summarise_shadowing.py')
cases = [
    ('summarise(a = sum(x, na.rm = TRUE), b = sum(y, na.rm = TRUE))', False),
    ('summarise(x = sum(x[keep]), y = sum(z))', False),
    ('mutate(x = sum(x), y = sum(x))', False),
    ('summarise(x = sum(x), y = sum(x[keep]))', True),
    ('summarise(w = sum(w), y = sum(x * w) / sum(w))', True),
]
with tempfile.TemporaryDirectory() as folder:
    fixture = Path(folder) / 'fixture.R'
    for code, should_fail in cases:
        fixture.write_text(code, encoding='utf-8')
        result = subprocess.run([sys.executable, str(scanner), str(fixture)],
                                capture_output=True, text=True)
        if result.returncode != int(should_fail):
            raise AssertionError(f'{code}\n{result.stdout}\n{result.stderr}')
print('PASS: nested keyword arguments, own-expression indexing, actual later-argument shadowing')
