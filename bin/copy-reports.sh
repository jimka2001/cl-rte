#!/bin/bash
#PBS -m bea

scp -r /lrde/home/jnewton/analysis/*.{sorted,sexp,dat,gnu,ltxdat} johan:/Users/jnewton/analysis/.

