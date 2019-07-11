#!/bin/csh -f
#PBS -m a

set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)

set PROJECT=$BIN/../cl-robdd/src/cl-robdd-scala
cd $PROJECT

git stash 
git pull
sbt -Dsbt.log.noformat=true compile
