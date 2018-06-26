#!/bin/bash

cd /lrde/cluster/jnewton/bdd-sizes
rm -f tmp
for file in * ; do
    mv $file tmp
    cat tmp | sort -u > $file
    wc -l $file
    git add $file
    cat $file | sed -e 's/ [^ ]*$/  -1/' > $file.2-columns
    git add $file.2-columns
    scp $file.2-columns johan:/Users/jnewton/analysis/.
done
rm -f tmp
git commit -m'automatic update'
