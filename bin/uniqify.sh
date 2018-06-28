#!/bin/bash

cd /lrde/cluster/jnewton/bdd-sizes
rm -f tmp
for file in bdd-sizes-unique-[0-9]? ; do
    if [ -f $file.new ] ; then
        mv $file.new tmp
        sort -u -T $PWD tmp > $file.new
        mv $file tmp
        sort -m -u -T $PWD tmp $file.new > $file
        rm tmp $file.new
        wc -l $file
        git add $file
        cat $file | sed -e 's/ [^ ]*$/  -1/' > $file.2-columns
        git add $file.2-columns
        scp $file.2-columns johan:/Users/jnewton/analysis/.
    fi
done
rm -f tmp
git commit -q -m'automatic update'
