#!/bin/bash
#PBS -m bea

scp -r /lrde/home/jnewton/analysis/. johan:/Users/jnewton/analysis/.
cat /lrde/home/jnewton/cluster.*/bdd-sizes.* | sort -u > /lrde/home/jnewton/bdd-sizes-unique
scp /lrde/home/jnewton/bdd-sizes-unique johan:/Users/jnewton/analysis/bdd-sizes-unique
