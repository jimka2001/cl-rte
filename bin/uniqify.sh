#!/bin/bash


cd /lrde/cluster/jnewton/bdd-sizes
rm -f tmp

for new in *new ; do
    file="${new%%.*}"
    touch $file
    mv $new tmp
    sort -u -T $PWD tmp > $new
    mv $file tmp
    sort -m -u -T $PWD tmp $new > $file
    rm tmp $new
    wc -l $file
   cat $file | sed -e 's/ [^ ]*$/  -1/' > $file.2-columns
   scp $file.2-columns johan:/Users/jnewton/analysis/.
done
