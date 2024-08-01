#!/bin/csh -f
#PBS -m a

set BIN="$HOME/sw/cl-rte/bin/"
set path = ($path $BIN)
set PROJECT=$BIN/../cl-robdd/src/cl-robdd-scala
cd $PROJECT

sbt -Dsbt.log.noformat=true "runMain dimacs.dimacsParse $argv"
