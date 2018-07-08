#!/bin/bash

destdir=$1
cd ~/analysis
for n in $(seq 7 19) ; do
      cp -v bdd-distribution-"$n".ltxdat $destdir
done      
cp -v *.ltxdat \
   *.sexp \
   bdd-sizes-*2-columns* \
   $destdir
