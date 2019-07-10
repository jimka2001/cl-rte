#!/bin/csh -f
set echo verbose
set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)

cd $BIN
git stash
git pull
sbt -Dsbt.log.noformat=true compile
