#!/bin/bash

export CLUSTER_JOB_NUM=$$
if [ -d cluster.$$ ]; then
  mv cluster.$$ "cluster.$$.`date`"
fi
cd
mkdir cluster.$$
cd cluster.$$
echo starting jobs in $PWD
BIN="$HOME/sw/regular-type-expression/bin/"


qsub -q infinite -l walltime=80000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",DECOMPOSE-INDEX="3",BUCKET-INDEX="7"  $BIN/test-lisp.lisp


exit 0
