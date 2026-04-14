#!/bin/bash
#SBATCH --job-name=ecec-verify
#SBATCH --partition=day
#SBATCH --account=sarin
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=28G
#SBATCH --time=4:00:00
#SBATCH --output=slurm/logs/verify_refactor_%j.out
#SBATCH --error=slurm/logs/verify_refactor_%j.err

module load R/4.4.1-foss-2022b
export R_LIBS_USER=~/R/libs

cd /gpfs/gibbs/project/sarin/hre2/repositories/ECEC-Simulator

Rscript src/main.R -r speed_test -a 1
