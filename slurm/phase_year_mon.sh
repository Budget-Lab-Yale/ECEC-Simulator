#!/bin/bash
#SBATCH --job-name=ecec-year-mon
#SBATCH --partition=day
#SBATCH --account=sarin
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=4:00:00
#SBATCH --output=slurm/logs/year_%A_%a.out
#SBATCH --error=slurm/logs/year_%A_%a.err

# SLURM Phase 2 with RSS monitoring

RUNSCRIPT="$1"
SCRATCH_DIR="$2"

module load R/4.4.1-foss-2022b
export R_LIBS_USER=~/R/libs
# Reduce R heap fragmentation: grow memory conservatively so GC compacts more often
export R_GC_MEM_GROW=0

cd /gpfs/gibbs/project/sarin/hre2/repositories/ECEC-Simulator

echo "=== ECEC Year Phase (monitored) ==="
echo "Runscript: ${RUNSCRIPT}"
echo "Scratch: ${SCRATCH_DIR}"
echo "Array task ID: ${SLURM_ARRAY_TASK_ID}"
echo "Start: $(date)"

Rscript src/main.R \
  -r "${RUNSCRIPT}" \
  --slurm-phase year \
  --slurm-scratch "${SCRATCH_DIR}" \
  --slurm-year-index "${SLURM_ARRAY_TASK_ID}" &

R_PID=$!

# Monitor RSS every 15 seconds
PEAK_RSS=0
while kill -0 $R_PID 2>/dev/null; do
  RSS=$(ps -o rss= -p $R_PID 2>/dev/null | tr -d ' ')
  if [[ -n "$RSS" ]]; then
    RSS_MB=$((RSS / 1024))
    if [[ $RSS_MB -gt $PEAK_RSS ]]; then
      PEAK_RSS=$RSS_MB
    fi
    echo "$(date +%H:%M:%S) RSS: ${RSS_MB} MB (peak: ${PEAK_RSS} MB)"
  fi
  sleep 15
done

wait $R_PID
EXIT_CODE=$?

echo ""
echo "Exit code: ${EXIT_CODE}"
echo "Peak RSS: ${PEAK_RSS} MB"
echo "End: $(date)"
exit $EXIT_CODE
