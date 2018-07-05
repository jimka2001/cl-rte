#!/bin/bash
#qselect -u $USER -s RQ | xargs --no-run-if-empty qdel
cd /lrde/home/jnewton/sw/regular-type-expression ; git stash ; git pull --no-edit
cd
export CLUSTER_JOB_NUM=$$
if [ -d cluster.$$ ]; then
  mv cluster.$$ "cluster.$$.`date`"
fi
mkdir cluster.$$
cd cluster.$$
echo starting jobs in cluster.$$
BIN="$HOME/sw/regular-type-expression/bin"

numvars=$1
time=$2
numsamplesperjob=$3
numjobs=$4

for n in $(seq 1 $numjobs) ; do
qsub -l walltime=$time -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="$numvars",NUM-SAMPLES="$numsamplesperjob" $BIN/distribution-report.lisp
done

echo started `qselect -u $USER -s RQ | wc -l` jobs
echo starting jobs in cluster.$$

