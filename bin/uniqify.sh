#!/bin/bash

cd /lrde/cluster/jnewton/bdd-sizes
rm -ftmp
for file in * ; do
    echo -n before " " 
    wc -l $file
    mv $file tmp
    cat tmp | sort -u > $file
    echo -n after "  " 
    wc -l $file
    git add $file
    cat $file | sed -e 's/ [^ ]*$//' > $file.2-columns
    git add $file.2-columns
done
git commit -m'automatic update'
