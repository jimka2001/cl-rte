#!/bin/csh -f
set BIN="$HOME/sw/regular-type-expression/bin/"
set path = ($path $BIN)

printenv
echo "ls /usr/bin"
ls -l /usr/bin

cd $BIN
git pull
sbt compile
