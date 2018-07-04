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
#   qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="5",NUM-SAMPLES="10965" $BIN/distribution-report.lisp
#   qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="6",NUM-SAMPLES="13968" $BIN/distribution-report.lisp
#   qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="7",NUM-SAMPLES="23756" $BIN/distribution-report.lisp
#   for i in $(seq 0 4) ; do
#     qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="8",NUM-SAMPLES="50000" $BIN/distribution-report.lisp
#     qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="9",NUM-SAMPLES="150" $BIN/distribution-report.lisp
#     qsub -q lrde -l walltime=30:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="10",NUM-SAMPLES="10" $BIN/distribution-report.lisp
#   done
# done



for n in $(seq 1 50) ; do
  echo -n
  # 10 var 1000 takes 66 sec
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="10",NUM-SAMPLES="1000" $BIN/distribution-report.lisp
  # 11 var 1000 takes 106 sec
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="12",NUM-SAMPLES="1000" $BIN/distribution-report.lisp
  # 12 var 1000 takes 193 sec
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="12",NUM-SAMPLES="1000" $BIN/distribution-report.lisp
  # 13 var 1000 takes 377 sec 
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="13",NUM-SAMPLES="1000" $BIN/distribution-report.lisp
done

for n in $(seq 1 75) ; do
  echo -n
  # 14 var 1000 takes (est) 754 = 12.5 minutes
  #  => 30 minutes gives 2400
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="14",NUM-SAMPLES="500" $BIN/distribution-report.lisp
done

for n in $(seq 1 125) ; do
  echo -n
  # 15 var 1000 takes 1622 = 27 minutes
  #  => 30 minutes gives 1200
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="15",NUM-SAMPLES="300" $BIN/distribution-report.lisp
done

for n in $(seq 1 250) ; do
  echo -n
  # 16 var 1000 takes (est) 3016 = 50 minutes = 8.84 hours
  #  => 30 minutes gives 600
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="16",NUM-SAMPLES="150" $BIN/distribution-report.lisp
done

for n in $(seq 1 500) ; do
  echo -n
  qsub -q lrde -l walltime=40:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="17",NUM-SAMPLES="80" $BIN/distribution-report.lisp
done
  # 17 var 1000 takes (est) 6032 = 1.67 hours
  #  ==> 30 minutes gives 300


for n in $(seq 1 1000) ; do
  qsub -q lrde -l walltime=45:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="18",NUM-SAMPLES="40" $BIN/distribution-report.lisp
done

# for n in $(seq 1 8) ; do
#   echo -n
#   # 19 var 1000 takes (est) 24128 = 402 minutes = 6.7 hours
#   #  ==> 30 minutes gives 74
#   # qsub -q lrde -l walltime=45:00 -v CLUSTER_JOB_NUM="$CLUSTER_JOB_NUM",NUM-VARS="19",NUM-SAMPLES="18" $BIN/distribution-report.lisp
# done



echo started jobs in cluster.$$
