#!/bin/bash
#
# launch_array.sh - Submit ECEC Simulator as a SLURM job array
#
# Submits three phases with dependencies:
#   1. Setup:    processing + calibration + save context
#   2. Year array: one job per simulation year (parallel)
#   3. Finalize: combine results and write output
#
# Usage:
#   ./slurm/launch_array.sh -r <runscript_id> [options] [-- extra R flags]
#
# Options:
#   -r, --runscript     Runscript ID (required)
#   -a, --calib-sample  Calibration sample percentage (default: 100)
#   --sim-sample        Simulation sample percentage (default: 100, must be <= calib-sample)
#   -n, --n-draws       Monte Carlo draws per record (default: 10)
#   -N, --nsece         NSECE interface timestamp
#   -A, --acs           ACS interface timestamp
#   -C, --calibration   Calibration interface timestamp
#   -f, --fiscal-npv    Enable fiscal NPV calculations
#   --mem               Memory per job (default: 32G)
#   --time              Time limit per job (default: 4:00:00)
#   --partition         SLURM partition (default: day)
#
# Examples:
#   ./slurm/launch_array.sh -r report_runs -a 20 -n 20 -N 202602052017 -f
#   ./slurm/launch_array.sh -r report_runs_test -a 1 -n 1 --mem 8G --time 1:00:00

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "${SCRIPT_DIR}")"

# Defaults
RUNSCRIPT=""
MEM="32G"
TIME="4:00:00"
PARTITION="day"
R_FLAGS=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -r|--runscript)
      RUNSCRIPT="$2"; shift 2 ;;
    -a|--calib-sample)
      R_FLAGS="${R_FLAGS} -a $2"; shift 2 ;;
    --sim-sample)
      R_FLAGS="${R_FLAGS} --sim-sample $2"; shift 2 ;;
    -n|--n-draws)
      R_FLAGS="${R_FLAGS} -n $2"; shift 2 ;;
    -N|--nsece)
      R_FLAGS="${R_FLAGS} -N $2"; shift 2 ;;
    -A|--acs)
      R_FLAGS="${R_FLAGS} -A $2"; shift 2 ;;
    -C|--calibration)
      R_FLAGS="${R_FLAGS} -C $2"; shift 2 ;;
    -f|--fiscal-npv)
      R_FLAGS="${R_FLAGS} -f"; shift ;;
    -o|--overwrite-param-defaults)
      R_FLAGS="${R_FLAGS} -o"; shift ;;
    --calib-target1-name)
      R_FLAGS="${R_FLAGS} --calib-target1-name $2"; shift 2 ;;
    --calib-target1-value)
      R_FLAGS="${R_FLAGS} --calib-target1-value $2"; shift 2 ;;
    --calib-target2-name)
      R_FLAGS="${R_FLAGS} --calib-target2-name $2"; shift 2 ;;
    --calib-target2-value)
      R_FLAGS="${R_FLAGS} --calib-target2-value $2"; shift 2 ;;
    --no-price-wedge)
      R_FLAGS="${R_FLAGS} --no-price-wedge"; shift ;;
    --no-employment-targeting)
      R_FLAGS="${R_FLAGS} --no-employment-targeting"; shift ;;
    --mem)
      MEM="$2"; shift 2 ;;
    --time)
      TIME="$2"; shift 2 ;;
    --partition)
      PARTITION="$2"; shift 2 ;;
    --)
      shift; R_FLAGS="${R_FLAGS} $*"; break ;;
    *)
      echo "Error: unknown option: $1"
      exit 1 ;;
  esac
done

if [[ -z "${RUNSCRIPT}" ]]; then
  echo "Error: --runscript (-r) is required"
  echo "Usage: $0 -r <runscript_id> [options]"
  exit 1
fi

# Read runscript CSV to determine year range
RUNSCRIPT_CSV="${PROJECT_DIR}/config/runscripts/${RUNSCRIPT}.csv"
if [[ ! -f "${RUNSCRIPT_CSV}" ]]; then
  echo "Error: runscript not found: ${RUNSCRIPT_CSV}"
  exit 1
fi

# Extract year range from baseline row (first data row)
YEAR_RANGE=$(awk -F',' 'NR==2 { print $NF }' "${RUNSCRIPT_CSV}" | tr -d '[:space:]')

# Handle case where years isn't the last column - find it by header
YEARS_COL=$(head -1 "${RUNSCRIPT_CSV}" | tr ',' '\n' | grep -n '^years$' | cut -d: -f1)
if [[ -n "${YEARS_COL}" ]]; then
  YEAR_RANGE=$(awk -F',' -v col="${YEARS_COL}" 'NR==2 { print $col }' "${RUNSCRIPT_CSV}" | tr -d '[:space:]')
fi

YEAR_START=$(echo "${YEAR_RANGE}" | cut -d: -f1)
YEAR_END=$(echo "${YEAR_RANGE}" | cut -d: -f2)
N_YEARS=$(( YEAR_END - YEAR_START + 1 ))

echo "=== ECEC SLURM Job Array Launcher ==="
echo "  Runscript:  ${RUNSCRIPT}"
echo "  Years:      ${YEAR_START}-${YEAR_END} (${N_YEARS} years)"
echo "  Memory:     ${MEM}"
echo "  Time:       ${TIME}"
echo "  Partition:  ${PARTITION}"
echo "  R flags:    ${R_FLAGS}"
echo ""

# Create scratch directory
SCRATCH_DIR="${PROJECT_DIR}/slurm/scratch/${RUNSCRIPT}"
mkdir -p "${SCRATCH_DIR}/year_results"
echo "  Scratch dir: ${SCRATCH_DIR}"

# Ensure logs directory exists
mkdir -p "${PROJECT_DIR}/slurm/logs"

# Phase 1: Setup
SETUP_JOB=$(sbatch --parsable \
  --job-name="ecec-setup-${RUNSCRIPT}" \
  --partition="${PARTITION}" \
  --mem="${MEM}" \
  --time="${TIME}" \
  "${PROJECT_DIR}/slurm/phase_setup.sh" \
  "${RUNSCRIPT}" "${SCRATCH_DIR}" ${R_FLAGS})
echo "  Setup job:    ${SETUP_JOB}"

# Phase 2: Year array (depends on setup)
ARRAY_JOB=$(sbatch --parsable \
  --dependency="afterok:${SETUP_JOB}" \
  --array="1-${N_YEARS}" \
  --job-name="ecec-year-${RUNSCRIPT}" \
  --partition="${PARTITION}" \
  --mem="${MEM}" \
  --time="${TIME}" \
  "${PROJECT_DIR}/slurm/phase_year.sh" \
  "${RUNSCRIPT}" "${SCRATCH_DIR}")
echo "  Year array:   ${ARRAY_JOB} (array 1-${N_YEARS})"

# Phase 3: Finalize (depends on all year array tasks)
FINALIZE_JOB=$(sbatch --parsable \
  --dependency="afterok:${ARRAY_JOB}" \
  --job-name="ecec-finalize-${RUNSCRIPT}" \
  --partition="${PARTITION}" \
  --mem=16G \
  --time="${TIME}" \
  "${PROJECT_DIR}/slurm/phase_finalize.sh" \
  "${RUNSCRIPT}" "${SCRATCH_DIR}")
echo "  Finalize job: ${FINALIZE_JOB}"

echo ""
echo "Job chain submitted: ${SETUP_JOB} -> ${ARRAY_JOB}[1-${N_YEARS}] -> ${FINALIZE_JOB}"
echo "Monitor: squeue -u \$USER"
