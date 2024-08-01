#!/bin/csh -f

foreach f ($argv)
  foreach b (0 1 2 3 4 5 7 8 9 6)
    /Volumes/Disk2/jimka/sw/common-lisp/cl-rte/bin/mdtd-report-profile-local.lisp $b $f
  end
end
