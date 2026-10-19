#!/bin/bash
#SBATCH -p day
#SBATCH -A pi_nrs36
#SBATCH -t 12:00:00
#SBATCH --mem=96G
# 2026-10-19 state baseline diagnostics on a finished 52-geography run.
# Usage: sbatch alpha_drift.sbatch <state|national> <run ts> [script, default alpha_drift]
#   script = alpha_drift | baseline_consistency (state_data/cluster/<script>.R)
set -eu
MODE=$1; RUN=$2; SCRIPT=${3:-alpha_drift}
module load R/4.4.2-gfbf-2024a
export R_LIBS_USER=$HOME/R/4.4.2-gfbf-2024a
export ECEC_URBAN_DENSITY=1 ECEC_INCOME_RANK=national ECEC_STATE_AGING=national
export ECEC_STATE_ALPHA_PRICE=$MODE
E=/nfs/roberts/project/pi_nrs36/agr55/ecec-state
SHARED=/nfs/roberts/project/pi_nrs36/shared/model_data/ECEC-Simulator
D=$E/analysis/alpha_drift
W=$E/runs/1019-$SCRIPT-$MODE
ROOT=$D/root_${SCRIPT}_$MODE
CAL=202610151431
[ -d $W ] || cp -r $E/runs/1015-main $W
tar xzf $E/analysis/open_items/code_sim2.tar.gz -C $W
mkdir -p $W/state_data/cluster $ROOT
cp $D/$SCRIPT.R $W/state_data/cluster/$SCRIPT.R
[ -e $ROOT/$CAL ] || ln -s $SHARED/$CAL $ROOT/$CAL
sed -i "s#^  output: .*#  output: '$ROOT'#" $W/config/default_paths.yaml
grep -n "output:" $W/config/default_paths.yaml
cd $W
{
  echo "$SCRIPT provenance -- mode $MODE run $RUN calibration $CAL"
  echo "date: $(date)"
  echo "code: code_sim2.tar.gz md5 $(md5sum $E/analysis/open_items/code_sim2.tar.gz | cut -d' ' -f1); commit.txt: $(cat commit.txt 2>/dev/null)"
  echo "script md5: $(md5sum state_data/cluster/$SCRIPT.R | cut -d' ' -f1)"
  echo "env: ECEC_URBAN_DENSITY=1 ECEC_INCOME_RANK=national ECEC_STATE_AGING=national ECEC_STATE_ALPHA_PRICE=$MODE"
  echo "R: $(Rscript --version 2>&1)"
  echo "run log (checks): $(ls $E/logs/nomsim52_${MODE}_*.out)"
} > $D/${SCRIPT}_${MODE}_provenance.txt
Rscript state_data/cluster/$SCRIPT.R -r state_targets_all -C $CAL \
  --drift-run $RUN --drift-src-root $SHARED --drift-out $D
echo "exit $? $(date)"
