#!/bin/bash
#SBATCH --job-name=ecec-year
#SBATCH --partition=day
#SBATCH --account=sarin
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=4:00:00
#SBATCH --output=slurm/logs/year_%A_%a.out
#SBATCH --error=slurm/logs/year_%A_%a.err

# SLURM Phase 2: Run one simulation year per array task
#
# Usage: sbatch --array=1-N --dependency=afterok:<setup_job_id> \
#          slurm/phase_year.sh <runscript_id> <scratch_dir>
#
# SLURM_ARRAY_TASK_ID maps to --slurm-year-index (1-based index into years)

RUNSCRIPT="$1"
SCRATCH_DIR="$2"

module load R/4.4.1-foss-2022b
export R_LIBS_USER=~/R/libs

cd /gpfs/gibbs/project/sarin/hre2/repositories/ECEC-Simulator

echo "=== ECEC Year Phase ==="
echo "Runscript: ${RUNSCRIPT}"
echo "Scratch: ${SCRATCH_DIR}"
echo "Array task ID: ${SLURM_ARRAY_TASK_ID}"

Rscript src/main.R \
  -r "${RUNSCRIPT}" \
  --slurm-phase year \
  --slurm-scratch "${SCRATCH_DIR}" \
  --slurm-year-index "${SLURM_ARRAY_TASK_ID}"
