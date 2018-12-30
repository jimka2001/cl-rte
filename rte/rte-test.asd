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

(asdf:defsystem :rte-test
  :version (:read-file-form "../version.lisp")
  :author "Jim Newton"
  :description "Test cases for rte package/system"
  :license "MIT"
  :defsystem-depends-on (:rte)
  :depends-on (;;:rte
	       :adjuvant
	       :rte-regexp-test
	       (:feature :sbcl :2d-array)
	       :scrutiny
	       (:feature :sbcl :2d-array-test)
	       :ndfa-test
	       :lisp-types-test)
  :components
  ((:module "src"
    :components
    ((:file "test-rte")
     (:file "test-list-of")
     (:rte-file "test-re-pattern")
     (:rte-file "test-destructuring-case-1")
     (:rte-file "test-destructuring-case-2")
     (:rte-file "test-destructuring-case")
     (:rte-file "test-ordinary-lambda-list")
     (:rte-file "test-rte-typecase")))))
