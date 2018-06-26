#!/bin/bash

cd ~/analysis
for n in $(seq 7 19) ; do
      cp bdd-distribution-"$n".ltxdat bdd-distribution-data-"$n".sexp   ~/newton.16.edtchs/src/
done      
cp bdd-efficiency-sample.ltxdat bdd-distribution.ltxdat bdd-distribution-expected.ltxdat bdd-distribution-sigma.ltxdat bdd-samples-table.ltx ~/newton.16.edtchs/src/
