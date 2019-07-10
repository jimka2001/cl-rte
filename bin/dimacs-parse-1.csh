#!/bin/csh -f

set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)
set PROJECT=$BIN/../cl-robdd/src/cl-robdd-scala
cd $PROJECT

sbt "runMain dimacs.dimacsParse"
