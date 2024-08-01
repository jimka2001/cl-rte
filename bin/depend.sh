#!/bin/bash
BIN="$HOME/sw/cl-rte/bin/"
x=""
x=$x:`qsub -l walltime=30 $BIN/print-error.sh`
qsub -l walltime=30 -W depend=afterok$x $BIN/print-message.sh
