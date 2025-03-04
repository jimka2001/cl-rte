#!/bin/bash
qselect -u $USER -s RQ | xargs --no-run-if-empty qdel
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

# there are 10 bucket-reports (0..9) and 15 (0..14) decomposition functions
# it is important that all the calls to mdtd-report-profile.lisp be done on the same architecture
for bucket in $(seq 0 9) ; do
    for decompose in $(seq 0 14) ; do
	qsub -q infinite -l walltime=80000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",DECOMPOSE-INDEX="$decompose",BUCKET-INDEX="$bucket"  $BIN/mdtd-report-profile.lisp
    done
done


