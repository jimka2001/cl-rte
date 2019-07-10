#!/bin/bash
#PBS -m a
set verbose echo

BIN="$HOME/sw/regular-type-expression/bin/"
inDir=/lrde/cluster/jnewton/SAT-benchmarks/NoLimits
cd $BIN/..

initial=`qsub -l walltime=3000 $BIN/dimacs-compile.csh`

cd $inDir
for cnf in *.cnf
do
  echo file = $cnf
  cd $BIN/..
  qsub -W depend=afterok:$initial -F  "$inDir $cnf"  $BIN/dimacs-parse-1.csh
done

