#!/bin/bash
#qselect -u $USER -s RQ | xargs --no-run-if-empty qdel
cd /lrde/home/jnewton/sw/regular-type-expression ; git pull --no-edit
cd
export CLUSTER_JOB_NUM=$$
if [ -d cluster.$$ ]; then
  mv cluster.$$ "cluster.$$.`date`"
fi
mkdir cluster.$$
cd cluster.$$
echo starting jobs in cluster.$$
BIN="$HOME/sw/regular-type-expression/bin"

qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="3",NUM-SAMPLES="256" $BIN/distribution-report.lisp
qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="4",NUM-SAMPLES="65536" $BIN/distribution-report.lisp


for i in $(seq 0 100) ; do
  qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="5",NUM-SAMPLES="5000" $BIN/distribution-report.lisp
  qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="6",NUM-SAMPLES="4000" $BIN/distribution-report.lisp
  qsub -q lrde -l walltime=10:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="7",NUM-SAMPLES="1000" $BIN/distribution-report.lisp
  qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="8",NUM-SAMPLES="100" $BIN/distribution-report.lisp
  for i in $(seq 0 4) ; do
    qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="9",NUM-SAMPLES="40" $BIN/distribution-report.lisp
    qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="10",NUM-SAMPLES="10" $BIN/distribution-report.lisp
  done
done
echo started jobs in cluster.$$






