#!/bin/bash
#SBATCH --job-name=ecec-setup
#SBATCH --partition=day
#SBATCH --account=sarin
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=4:00:00
#SBATCH --output=slurm/logs/setup_%j.out
#SBATCH --error=slurm/logs/setup_%j.err

# SLURM Phase 1: Processing + Calibration + Save context to scratch
#
# Usage: sbatch slurm/phase_setup.sh <runscript_id> <scratch_dir> [extra R flags...]
#   e.g. sbatch slurm/phase_setup.sh report_runs slurm/scratch/report_runs -N 202602052017 -a 20 -n 20

RUNSCRIPT="$1"
SCRATCH_DIR="$2"
shift 2
R_FLAGS="$@"

module load R/4.4.1-foss-2022b
export R_LIBS_USER=~/R/libs

cd /gpfs/gibbs/project/sarin/hre2/repositories/ECEC-Simulator

echo "=== ECEC Setup Phase ==="
echo "Runscript: ${RUNSCRIPT}"
echo "Scratch: ${SCRATCH_DIR}"
echo "R flags: ${R_FLAGS}"

Rscript src/main.R \
  -r "${RUNSCRIPT}" \
  ${R_FLAGS} \
  --slurm-phase setup \
  --slurm-scratch "${SCRATCH_DIR}"
