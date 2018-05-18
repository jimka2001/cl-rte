#!/bin/bash
qselect -u $USER -s RQ | xargs --no-run-if-empty qdel
cd /lrde/home/jnewton/sw/regular-type-expression ; git pull --no-edit
cd
export CLUSTER_JOB_NUM=$$
mkdir cluster.$$
cd cluster.$$
echo starting jobs in cluster.$$

x=""
x=$x:`qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="3",NUM-SAMPLES="256" $HOME/bin/distribution-report.lisp`
x=$x:`qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="4",NUM-SAMPLES="65536" $HOME/bin/distribution-report.lisp`
x=$x:`qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="5",NUM-SAMPLES="500000" $HOME/bin/distribution-report.lisp`
x=$x:`qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="6",NUM-SAMPLES="400000" $HOME/bin/distribution-report.lisp`
x=$x:`qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="7",NUM-SAMPLES="100000" $HOME/bin/distribution-report.lisp`
x=$x:`qsub -l walltime=24:00:00-v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="8",NUM-SAMPLES="21000" $HOME/bin/distribution-report.lisp`
x=$x:`qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="9",NUM-SAMPLES="4500" $HOME/bin/distribution-report.lisp`
x=$x:`qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="10",NUM-SAMPLES="1000" $HOME/bin/distribution-report.lisp`

qsub -W depend=afterok$x -l walltime=1:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM" $HOME/bin/copy-reports


