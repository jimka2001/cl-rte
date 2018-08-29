#!/bin/bash
# qselect -u $USER -s RQ | xargs --no-run-if-empty qdel
# cd /lrde/home/jnewton/sw/regular-type-expression ; git pull --no-edit
cd
export CLUSTER_JOB_NUM=$$
if [ -d cluster.$$ ]; then
  mv cluster.$$ "cluster.$$.`date`"
fi
mkdir cluster.$$
cd cluster.$$
echo starting jobs in cluster.$$
BIN="$HOME/sw/regular-type-expression/bin/"

x=""
# there are 10 bucket-reports (0..9)
for bucket in $(seq 0 9) ; do
    x=$x:`qsub -q infinite -l walltime=30000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",BUCKET-INDEX="$bucket" $BIN/baker-report.lisp`
done


