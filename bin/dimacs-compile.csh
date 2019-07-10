#!/bin/csh -f
set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)

printenv

cd $BIN
git pull
sbt compile
