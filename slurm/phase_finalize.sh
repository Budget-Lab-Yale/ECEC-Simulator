#!/bin/bash
#SBATCH --job-name=ecec-finalize
#SBATCH --partition=day
#SBATCH --account=sarin
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=1:00:00
#SBATCH --output=slurm/logs/finalize_%j.out
#SBATCH --error=slurm/logs/finalize_%j.err

# SLURM Phase 3: Combine year results and write output
#
# Usage: sbatch --dependency=afterok:<array_job_id> \
#          slurm/phase_finalize.sh <runscript_id> <scratch_dir>

RUNSCRIPT="$1"
SCRATCH_DIR="$2"

module load R/4.4.1-foss-2022b
export R_LIBS_USER=~/R/libs

cd /gpfs/gibbs/project/sarin/hre2/repositories/ECEC-Simulator

echo "=== ECEC Finalize Phase ==="
echo "Runscript: ${RUNSCRIPT}"
echo "Scratch: ${SCRATCH_DIR}"

Rscript src/main.R \
  -r "${RUNSCRIPT}" \
  --slurm-phase finalize \
  --slurm-scratch "${SCRATCH_DIR}"
