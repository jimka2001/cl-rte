#!/bin/csh -f

set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)
set inDir = /lrde/cluster/jnewton/SAT-benchmarks/NoLimits
cd $BIN/..

set initial = `qsub -l walltime=30 $BIN/dimacs-compile.csh`

cd $inDir
foreach cnf (g2-ACG-*.cnf)
  qsub -W depend=afterok:$x $BIN/dimacs-parse-1.csh $cnf
end
