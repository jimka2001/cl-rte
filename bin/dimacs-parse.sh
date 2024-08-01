#!/bin/bash
#PBS -m a
set verbose echo

BIN="$HOME/sw/cl-rte/bin/"
cd $BIN/..

initial=`qsub -l walltime=3000 $BIN/dimacs-compile.csh`


# inDir=/lrde/cluster/jnewton/SAT-benchmarks/NoLimits
# outDir=/lrde/cluster/jnewton/QM-reduce/NoLimits

# cd $inDir
# for cnf in *.cnf
# do
#   echo file = $cnf
#   cd $BIN/..
#   qsub -W depend=afterok:$initial -l nodes=1:ppn=4 -F  "$inDir $outDir $cnf"  $BIN/dimacs-parse-1.csh
# done


inDir=/lrde/cluster/jnewton/SAT-benchmarks/RandomSat
outDir=/lrde/cluster/jnewton/QM-reduce/RandomSat

cd $inDir
for cnf in *.cnf
do
  echo file = $cnf
  cd $BIN/..
  qsub -W depend=afterok:$initial -l nodes=1:ppn=4 -F  "$inDir $outDir $cnf"  $BIN/dimacs-parse-1.csh
done

