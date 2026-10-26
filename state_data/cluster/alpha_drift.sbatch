#!/bin/bash
#SBATCH -p day
#SBATCH -A pi_nrs36
#SBATCH -t 12:00:00
#SBATCH --mem=96G
# State baseline diagnostics on a finished 52-geography run.
# Usage: sbatch alpha_drift.sbatch <state|national> <run ts> <calib ts> <run dir> [script]
#   run dir = run copy holding run's code + config (copied to a scratch run dir here)
#   script  = alpha_drift (default) | baseline_consistency (state_data/cluster/<script>.R)
# Writes state_data/out-style files to $E/analysis/<script>/<run ts>/.
set -eu
MODE=$1; RUN=$2; CAL=$3; SRC=$4; SCRIPT=${5:-alpha_drift}
module load R/4.4.2-gfbf-2024a
export R_LIBS_USER=$HOME/R/4.4.2-gfbf-2024a
export ECEC_URBAN_DENSITY=1 ECEC_INCOME_RANK=national_rpp
export ECEC_STATE_ALPHA_PRICE=$MODE
E=/nfs/roberts/project/pi_nrs36/agr55/ecec-state
SHARED=/nfs/roberts/project/pi_nrs36/shared/model_data/ECEC-Simulator
D=$E/analysis/$SCRIPT/$RUN
W=$E/runs/$RUN-$SCRIPT-$MODE
ROOT=$D/root_$MODE
# init writes metadata under output root -- scratch root w/ calibration linked in
[ -d $W ] || rsync -a --exclude='*.log' $SRC/ $W/
mkdir -p $ROOT
[ -e $ROOT/$CAL ] || ln -s $SHARED/$CAL $ROOT/$CAL
sed -i "s#^  output: .*#  output: '$ROOT'#" $W/config/default_paths.yaml
grep -n "output:" $W/config/default_paths.yaml
cd $W
{
  echo "$SCRIPT provenance -- mode $MODE run $RUN calibration $CAL"
  echo "date: $(date)"
  echo "run dir: $SRC; code: $(cat code_sha.txt 2>/dev/null)"
  echo "script md5: $(md5sum state_data/cluster/$SCRIPT.R | cut -d' ' -f1)"
  echo "env: ECEC_URBAN_DENSITY=1 ECEC_INCOME_RANK=national_rpp ECEC_STATE_ALPHA_PRICE=$MODE"
  echo "R: $(Rscript --version 2>&1)"
} > $D/${SCRIPT}_${MODE}_provenance.txt
Rscript state_data/cluster/$SCRIPT.R -r state_targets_all -C $CAL \
  --drift-run $RUN --drift-src-root $SHARED --drift-out $D
echo "exit $? $(date)"
