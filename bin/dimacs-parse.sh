#!/bin/bash
set verbose echo

BIN="$HOME/sw/regular-type-expression/bin/"
inDir=/lrde/cluster/jnewton/SAT-benchmarks/NoLimits
cd $BIN/..

initial=`qsub -l walltime=3000 $BIN/dimacs-compile.csh`

cd $inDir
for cnf in g2-ACG-*.cnf
do
  qsub -W depend=afterok:$initial $BIN/dimacs-parse-1.csh $inDir $cnf
done

