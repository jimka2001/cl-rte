#!/bin/bash

cd /lrde/cluster/jnewton/bdd-sizes
rm -ftmp
for file in * ; do
    mv $file tmp
    cat tmp | sort -u > $file
    git add $file
done
git commit -m'automatic update'
