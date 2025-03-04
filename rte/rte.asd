;; Copyright (c) 2016 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(asdf:defsystem :rte
  :version (:read-file-form "../version.lisp")
  :author "Jim Newton"
  :description "Regular type expressions implementation"
  :license "MIT"
  :depends-on (:ndfa
	       :adjuvant
	       :lisp-types
	       )
  :components
  ((:module "src"
    :components
    ((:file "rte")
     (:file "asdf" :depends-on ("rte"))
     (:file "expand-typedef"  :depends-on ("rte"))
     (:file "list-of" :depends-on ("rte" "expand-typedef"))
     (:file "dependents" :depends-on ("rte"))
     (:file "derivative" :depends-on ("rte"))
     (:file "rte-state-machine" :depends-on ("rte"))
     (:file "dump-code" :depends-on ("rte-state-machine" "rte"))
     (:file "strategy-inline" :depends-on ("rte" "dump-code"))
     (:file "strategy-goto" :depends-on ("rte" "dump-code" "strategy-inline"))
     (:file "strategy-tail-call" :depends-on ("rte" "dump-code" "strategy-inline"))
     (:file "strategy-trampoline" :depends-on ("rte" "dump-code" "strategy-inline"))
     (:file "strategy-jump-table" :depends-on ("rte" "dump-code"))
     (:file "re-pattern" :depends-on ("rte" "rte-state-machine" "dump-code" "expand-typedef" "dependents" "derivative"))
     (:file "rte-case" :depends-on ("rte"))
     (:file "destructuring-case" :depends-on ("re-pattern"))
     (:file "prolog" :depends-on ("rte"))
     ))))
