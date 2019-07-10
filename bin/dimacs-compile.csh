#!/bin/csh -f
set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)

cd $BIN
git pull
sbt compile
