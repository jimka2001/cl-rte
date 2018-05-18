#!/bin/bash

x=""
x=$x:`qsub -l walltime=30 $HOME/bin/print-error.sh`
qsub -l walltime=30 -W depend=afterok$x $HOME/bin/print-message.sh
