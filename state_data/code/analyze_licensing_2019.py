"""Build October 1, 2019 licensing audit; never write simulation inputs.

Usage from repository root: python state_data/code/analyze_licensing_2019.py
Python standard library only. Sources must already exist at registered paths.
"""

import argparse
import csv
import hashlib
import math
from collections import Counter
from datetime import date
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SNAPSHOT = date(2019, 10, 1)
KEY = ('state', 'setting', 'age_group')
POINTS = {'11_months', '18_months', '35_months', '3_years', '4_years'}
VERIFIED = {'verified_archive', 'verified_history', 'verified_conditional'}


def read_csv(path):
    """Read UTF-8 CSV, accepting optional BOM."""
    with path.open(encoding='utf-8-sig', newline='') as handle:
        return list(csv.DictReader(handle))


def keyed(rows, label):
    """Index rows; reject duplicate geography/setting/age keys."""
    result = {}
    for row in rows:
        key = tuple(row[field] for field in KEY)
        if key in result:
            raise ValueError(f'Duplicate {label} key: {key}')
        result[key] = row
    return result


def build_audit(require_verified=False):
    """Validate source bytes, join evidence, preserve missingness."""
    baseline = read_csv(ROOT / 'resources/licensing/'
                        'US_ICPSR37700__center_and_home_child_staff_ratios_by_age__2017.csv')
    base = keyed(baseline, 'baseline')
    geographies = {row['state'] for row in baseline}
    if len(geographies) != 52 or len(base) != 312:
        raise ValueError('Expected 52 geographies and 312 geography/setting/age rows')
    for state in geographies:
        expected = {(state, 'center', age) for age in POINTS}
        expected.add((state, 'home', 'all_under_school_age'))
        if {key for key in base if key[0] == state} != expected:
            raise ValueError(f'Unexpected age coverage: {state}')

    searches = read_csv(ROOT / 'resources/licensing/licensing_2019_search_log.csv')
    search_index = {row['state']: row for row in searches}
    if len(search_index) != len(searches) or set(search_index) != geographies:
        raise ValueError('Search log must cover each of 52 geographies exactly once')

    sources = read_csv(ROOT / 'resources/licensing/licensing_2019_sources.csv')
    registry = {row['source_id']: row for row in sources}
    if len(registry) != len(sources):
        raise ValueError('Duplicate source ID')
    for source in sources:
        path = ROOT / source['local_file']
        if not path.is_file():
            raise ValueError(f'Missing source: {path}; retrieve registered URL')
        if hashlib.sha256(path.read_bytes()).hexdigest() != source['sha256']:
            raise ValueError(f'Source hash changed: {path}; review before updating registry')

    configurations = read_csv(ROOT / 'resources/licensing/licensing_2019_home_configurations.csv')
    configuration_keys = set()
    for row in configurations:
        key = (row['state'], row['configuration'])
        if key in configuration_keys or row['state'] not in geographies:
            raise ValueError(f'Invalid/duplicate home configuration: {key}')
        configuration_keys.add(key)
        if row['source_id'] not in registry or not row['conditions']:
            raise ValueError(f'Home configuration lacks source/conditions: {key}')
        for field in ('max_total_children', 'max_preschool_children', 'required_adults'):
            value = row[field]
            if value and (not math.isfinite(float(value)) or float(value) < 0):
                raise ValueError(f'Invalid home configuration quantity: {key}, {field}')
        if row['max_preschool_children'] and row['max_total_children']:
            if float(row['max_preschool_children']) > float(row['max_total_children']):
                raise ValueError(f'Preschool capacity exceeds total capacity: {key}')

    evidence = keyed(read_csv(ROOT / 'resources/licensing/licensing_2019_evidence.csv'),
                     'evidence')
    if evidence.keys() - base.keys():
        raise ValueError('Evidence contains keys outside baseline coverage')
    if set(evidence) != set(base):
        raise ValueError('Every audit row needs explicit evidence or fallback disposition')
    allowed = VERIFIED | {'candidate_plan', 'candidate_annual_statute', 'candidate_archive',
               'blocked_conflict', 'blocked_definition', 'blocked_date',
               'blocked_access', 'blocked_incomplete', 'blocked_draft',
               'fallback_source_gap'}
    output = []
    for key, previous in base.items():
        item = evidence.get(key, {})
        status = item.get('status', 'not_reviewed')
        if item and status not in allowed:
            raise ValueError(f'Unknown status: {status}')
        source = registry.get(item.get('source_id'))
        if item and source is None:
            raise ValueError(f'Unregistered source: {key}')
        supporting = item.get('supporting_source_ids', '')
        for source_id in filter(None, supporting.split(';')):
            if source_id not in registry:
                raise ValueError(f'Unregistered supporting source: {key}, {source_id}')
        candidate = item.get('candidate_value', '')
        if candidate and (not math.isfinite(float(candidate)) or float(candidate) <= 0):
            raise ValueError(f'Nonpositive candidate: {key}')
        effective = item.get('effective_from', '')
        basis = item.get('verification_basis', '')
        if status in VERIFIED:
            if not candidate or (status == 'verified_archive' and not effective):
                raise ValueError(f'Verified row lacks applicable dated evidence: {key}')
            if effective and date.fromisoformat(effective) > SNAPSHOT:
                raise ValueError(f'Verified rule after snapshot: {key}')
            if status in {'verified_history', 'verified_conditional'} and not basis:
                raise ValueError(f'Historical verification lacks rule-history basis: {key}')
            if status == 'verified_conditional':
                alternatives = item.get('permitted_values', '').split(';')
                if not item.get('selection_basis') or candidate not in alternatives:
                    raise ValueError(f'Conditional mapping lacks explicit selection: {key}')
            if source['review_status'] != 'historical_rule_reviewed':
                raise ValueError(f'Verified row uses unverified source: {key}')
        if not item.get('notes'):
            raise ValueError(f'Missing evidence/fallback explanation: {key}')
        verified = status in VERIFIED
        fallback = ('none' if verified else 'retain_current_input_pending_verification')
        if key[1] == 'home' and not verified:
            fallback = ('retain_neutral_index_1' if previous['children_per_staff'] == 'NA'
                        else 'retain_capacity_proxy_pending_historical_verification')
        delta = ''
        if candidate and previous['children_per_staff'] != 'NA':
            delta = float(candidate) - float(previous['children_per_staff'])
        output.append({
            **{field: previous[field] for field in KEY},
            'snapshot_date': SNAPSHOT.isoformat(),
            'baseline_value': previous['children_per_staff'],
            'baseline_measure': ('children_per_staff' if key[1] == 'center'
                                 else 'home_capacity_proxy'),
            'baseline_source': previous['source'],
            'candidate_value': candidate,
            'candidate_measure': item.get('candidate_measure', ''),
            'verified_value': candidate if verified else '',
            'candidate_minus_baseline': delta,
            'status': status,
            'fallback_action': fallback,
            'fallback_value': previous['children_per_staff'] if not verified else '',
            'fallback_reason': '' if verified else item['notes'],
            'source_id': item.get('source_id', ''),
            'supporting_source_ids': supporting,
            'url': source['url'] if source else '',
            'locator': item.get('locator', ''),
            'effective_from': effective,
            'verification_basis': basis,
            'permitted_values': item.get('permitted_values', ''),
            'selection_basis': item.get('selection_basis', ''),
            'search_result': search_index[key[0]]['result'],
            'notes': item.get('notes', ''),
        })
    destination = ROOT / 'state_data/out/licensing_2019_audit.csv'
    with destination.open('w', encoding='utf-8', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=list(output[0]))
        writer.writeheader()
        writer.writerows(output)
    coverage = []
    for state in sorted(geographies):
        rows = [row for row in output if row['state'] == state]
        coverage.append({
            'state': state,
            'snapshot_date': SNAPSHOT.isoformat(),
            'center_points_verified': sum(row['setting'] == 'center'
                                          and row['status'] in VERIFIED for row in rows),
            'home_verified': any(row['setting'] == 'home'
                                 and row['status'] in VERIFIED for row in rows),
            'numeric_candidates': sum(bool(row['candidate_value']) for row in rows),
            'fallback_rows': sum(row['status'] not in VERIFIED for row in rows),
            'search_result': search_index[state]['result'],
            'next_step': search_index[state]['next_step'],
        })
    with (ROOT / 'state_data/out/licensing_2019_coverage.csv').open(
            'w', encoding='utf-8', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=list(coverage[0]))
        writer.writeheader()
        writer.writerows(coverage)
    counts = Counter(row['status'] for row in output)
    print(f'Snapshot {SNAPSHOT}: {len(output)} unweighted audit rows, '
          f'{len(geographies)} geographies; not child/population counts')
    for status, count in sorted(counts.items()):
        print(f'  {status}: {count} geography/setting/age rows')
    print(f'Wrote {destination.relative_to(ROOT)}; simulation inputs unchanged')
    unverified = sum(row['status'] not in VERIFIED for row in output)
    if require_verified and unverified:
        raise ValueError(f'{unverified} rows still lack verified, comparable 2019 values')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--require-verified', action='store_true',
                        help='Fail if any row needs a fallback; never promote candidates')
    arguments = parser.parse_args()
    build_audit(require_verified=arguments.require_verified)
