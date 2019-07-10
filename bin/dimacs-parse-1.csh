#!/bin/csh -f
set verbose echo

set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)
set PROJECT=$BIN/../cl-robdd/src/cl-robdd-scala
cd $PROJECT

sbt -Dsbt.log.noformat=true "runMain dimacs.dimacsParse $argv"
