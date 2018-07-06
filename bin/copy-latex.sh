#!/bin/bash

cd ~/analysis
for n in $(seq 7 19) ; do
      cp -v bdd-distribution-"$n".ltxdat ~/newton.16.edtchs/src/
done      
cp -v bdd-efficiency-sample.ltxdat \
   bdd-distribution-data-*.sexp \
   bdd-distribution.ltxdat \
   bdd-distribution-2-8.ltxdat \
   bdd-distribution-expected.ltxdat \
   bdd-distribution-expected-2-8.ltxdat \
   bdd-distribution-sigma.ltxdat \
   bdd-distribution-sigma-2-8.ltxdat \
   sigma-kolmogorov-*.ltxdat \
   average-kolmogorov-*.ltxdat \
   excursion-summary.ltxdat \
   bdd-distribution-kolmogorov-*-*.ltxdat \
   delta-N=*-exp=*.ltxdat \
   bdd-samples-table.ltx \
   ~/newton.16.edtchs/src/
