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

# qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="3",NUM-SAMPLES="256" $BIN/distribution-report.lisp
# qsub -l walltime=24:00:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="4",NUM-SAMPLES="65536" $BIN/distribution-report.lisp


# for i in $(seq 0 100) ; do
#   qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="5",NUM-SAMPLES="5000" $BIN/distribution-report.lisp
#   qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="6",NUM-SAMPLES="4000" $BIN/distribution-report.lisp
#   qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="7",NUM-SAMPLES="10000" $BIN/distribution-report.lisp
#   qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="8",NUM-SAMPLES="100" $BIN/distribution-report.lisp
#   for i in $(seq 0 4) ; do
#     qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="9",NUM-SAMPLES="150" $BIN/distribution-report.lisp
#     qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="10",NUM-SAMPLES="10" $BIN/distribution-report.lisp
#   done

# done


  # 10 var 1000 takes 66 sec
  #  => 30 minutes gives 40000
#  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="10",NUM-SAMPLES="40000" $BIN/distribution-report.lisp
for n in $(seq 1 2) ; do
  # 11 var 1000 takes 106 sec
  #  => 30 minutes gives 20000
#  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="11",NUM-SAMPLES="20000" $BIN/distribution-report.lisp
for n in $(seq 1 2) ; do
  # 12 var 1000 takes 193 sec
  #  => 30 minutes gives 10000
#  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="12",NUM-SAMPLES="10000" $BIN/distribution-report.lisp
for n in $(seq 1 2) ; do
  # 13 var 1000 takes 377 sec 
  #  => 30 minutes gives 4800
#  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="13",NUM-SAMPLES="5000" $BIN/distribution-report.lisp
for n in $(seq 1 2) ; do
  # 14 var 1000 takes (est) 754 = 12.5 minutes
  #  => 30 minutes gives 2400
#  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="14",NUM-SAMPLES="2400" $BIN/distribution-report.lisp
for n in $(seq 1 2) ; do
  # 15 var 1000 takes 1622 = 27 minutes
  #  => 30 minutes gives 1200
#  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="15",NUM-SAMPLES="1200" $BIN/distribution-report.lisp
for n in $(seq 1 2) ; do
  # 16 var 1000 takes (est) 3016 = 50 minutes = 8.84 hours
  #  => 30 minutes gives 600
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="16",NUM-SAMPLES="600" $BIN/distribution-report.lisp
for n in $(seq 1 6) ; do
  # 17 var 1000 takes (est) 6032 = 1.67 hours
  #  ==> 30 minutes gives 300
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="17",NUM-SAMPLES="125" $BIN/distribution-report.lisp
for n in $(seq 1 6) ; do
  # 18 var 1000 takes (est) 12064 = 3.35 hours
  #  ==> 30 mintues gives 149
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="18",NUM-SAMPLES="50" $BIN/distribution-report.lisp
for n in $(seq 1 8) ; do
  # 19 var 1000 takes (est) 24128 = 402 minutes = 6.7 hours
  #  ==> 30 minutes gives 74
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="19",NUM-SAMPLES="18" $BIN/distribution-report.lisp
done
done
done
done
done
done
done
done
done


echo started jobs in cluster.$$
