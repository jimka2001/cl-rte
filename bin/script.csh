#!/bin/csh -f

foreach f (0 1)
  setenv DECOMPOSE_INDEX $f
  foreach b (0 1 2 3 4 5 6 7 8 9)
    setenv BUCKET_INDEX $b 
    /Volumes/Disk2/jimka/sw/common-lisp/regular-type-expression/bin/mdtd-report-profile-local.lisp
  end
end
