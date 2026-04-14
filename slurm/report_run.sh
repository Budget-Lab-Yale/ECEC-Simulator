#!/bin/bash
#SBATCH --job-name=ecec-report
#SBATCH --partition=day
#SBATCH --account=sarin
#SBATCH --cpus-per-task=5
#SBATCH --mem-per-cpu=28G
#SBATCH --time=12:00:00
#SBATCH --output=slurm/logs/report_run_%j.out
#SBATCH --error=slurm/logs/report_run_%j.err

module load R/4.4.1-foss-2022b
export R_LIBS_USER=~/R/libs

cd /gpfs/gibbs/project/sarin/jar335/Repositories/ECEC-Simulator

Rscript src/main.R \
  -r report_runs \
  -N 202602052017 \
  -a 10 \
  -n 10 \
  -P -w 5 \
  -f \
  --calib-target1-name paid_cost \
  --calib-target1-value -0.15
