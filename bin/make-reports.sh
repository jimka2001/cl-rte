#!/bin/bash
qselect -u $USER -s RQ | xargs --no-run-if-empty qdel
cd /lrde/home/jnewton/sw/regular-type-expression ; git pull --no-edit
cd
export CLUSTER_JOB_NUM=$$
mkdir cluster.$$
cd cluster.$$
echo starting jobs in cluster.$$
BIN="$HOME/sw/regular-type-expression/bin/"

x=""
# there are 10 bucket-reports (0..9) and 11 decomposition functions
for bucket in $(seq 0 9) ; do
    x=$x:`qsub -l walltime=20000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",BUCKET-INDEX="$bucket" $BIN/big-report.lisp`
    x=$x:`qsub -l walltime=10000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",BUCKET-INDEX="$bucket" $BIN/best-report.lisp`
    x=$x:`qsub -l walltime=16000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",BUCKET-INDEX="$bucket" $BIN/parameterization-report.lisp`
    x=$x:`qsub -l walltime=16000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",BUCKET-INDEX="$bucket" $BIN/bdd-report.lisp`
    for decompose in $(seq 0 10) ; do
	x=$x:`qsub -q lrde -l walltime=20000 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",DECOMPOSE-INDEX="$decompose",BUCKET-INDEX="$bucket"  $BIN/bdd-report-profile.lisp`
    done
done

qsub -W depend=afterok$x -l walltime=1:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM" $BIN/copy-reports.sh


