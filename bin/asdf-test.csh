#!/bin/csh -f
set verbose
set echo

cd ~/sw/cl-rte
foreach path (`find * -type f -name '*.asd' -print`)
   rm -rf ~/.cache/common-lisp/sbcl-*
   set system = $path:t:r
   set cmd = "(asdf:load-system :$system)"
   echo =======================
   echo ========= $cmd =========
   echo =======================
   echo sbcl --non-interactive --no-userinit --disable-debugger --eval '(require :asdf)' --eval "$cmd" >> /tmp/script
end
