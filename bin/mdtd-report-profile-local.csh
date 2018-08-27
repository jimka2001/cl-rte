#!/bin/csh -f

foreach f ($argv)
  foreach b (0 1 2 3 4 5 6 7 8 9)
    /Volumes/Disk2/jimka/sw/common-lisp/regular-type-expression/bin/mdtd-report-profile-local.lisp $b $f
  end
end
