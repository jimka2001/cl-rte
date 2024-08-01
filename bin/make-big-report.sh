#!/bin/bash
#qselect -u $USER -s RQ | xargs --no-run-if-empty qdel
cd /lrde/home/jnewton/sw/cl-rte ; git stash ; git pull --no-edit
cd
export CLUSTER_JOB_NUM=$$
if [ -d cluster.$$ ]; then
  mv cluster.$$ "cluster.$$.`date`"
fi
mkdir cluster.$$
cd cluster.$$
echo starting jobs in cluster.$$
BIN="$HOME/sw/cl-rte/bin/"

# there are 10 bucket-reports (0..9)
for bucket in $(seq 0 9) ; do
    qsub -q infinite -l walltime=40000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",BUCKET-INDEX="$bucket" $BIN/big-report.lisp
done


