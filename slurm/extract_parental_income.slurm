#!/bin/bash
#SBATCH --job-name=ecec-parent-income
#SBATCH --partition=devel
#SBATCH --account=pi_nrs36
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=00:15:00

set -euo pipefail

if [[ $# -lt 3 || $# -gt 4 ]]; then
  echo "Usage: sbatch [resource overrides] slurm/extract_parental_income.slurm RUN_ROOT OUTPUT_DIR FISCAL_YEARS [SCENARIOS]" >&2
  exit 2
fi

RUN_ROOT="$1"
OUTPUT_DIR="$2"
FISCAL_YEARS="$3"
SCENARIOS="${4:-}"
PROJECT_DIR="${ECEC_PROJECT_DIR:-${SLURM_SUBMIT_DIR:-$PWD}}"
WORKERS="${SLURM_CPUS_PER_TASK:-1}"

module load R/4.4.1-foss-2022b
export R_LIBS_USER="${R_LIBS_USER:-$HOME/R/libs}"

cd "${PROJECT_DIR}"

COMMAND=(
  Rscript src/4_output/extract_parental_income_archive.R
  "--run-root=${RUN_ROOT}"
  "--output-dir=${OUTPUT_DIR}"
  "--fiscal-years=${FISCAL_YEARS}"
  "--workers=${WORKERS}"
)
if [[ -n "${SCENARIOS}" ]]; then
  COMMAND+=("--scenarios=${SCENARIOS}")
fi

echo "Project dir: ${PROJECT_DIR}"
echo "Source run: ${RUN_ROOT}"
echo "Output dir: ${OUTPUT_DIR}"
echo "Fiscal years: ${FISCAL_YEARS}"
echo "Workers: ${WORKERS}"
echo "Started: $(date --iso-8601=seconds)"

/usr/bin/time -v "${COMMAND[@]}"

echo "Finished: $(date --iso-8601=seconds)"
