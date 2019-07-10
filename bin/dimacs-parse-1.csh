#!/bin/csh -f

set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)
set PROJECT=$BIN/../cl-robdd/src/cl-robdd-scala
cd $PROJECT

printenv

sbt "runMain dimacs.dimacsParse"
