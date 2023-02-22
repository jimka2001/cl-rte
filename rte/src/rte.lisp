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


(defpackage :regular-type-expression
  (:use :cl :ndfa :lisp-types :adjuvant)
  (:nicknames "RTE")
  (:export
   "CANONICALIZE-PATTERN"
   "DEFRTE"
   "DESTRUCTURING-CASE"
   "RTE-CASE"
   "RTE-ECASE"
   "DESTRUCTURING-METHODS"
   "LIST-OF"
   "RTE"
   "RTE-RESET"
   "RTE-SIMULATE"
   ))

(in-package :rte)

(deftype rte-keyword ()
  '(member :cat :1
    :0-* :* :0-or-more
    :1-* :+ :1-or-more
    :0-1 :?
    :and :or
    :permute))

(defvar *type-functions* (make-hash-table)
  "Hash tble mapping parameterized type name to function which can be used with (SATISFIES ...)")
(defvar *state-machines* (make-hash-table :test #'equal)
  "Hash table mapping rte pattern to state-machine object.")
(defvar *rte-types* (make-hash-table :test #'equal)
  "Hash table mapping rte-pattern to lisp-type:  
E.g., ((1-* SYMBOL NUMBER)) --> (AND SEQUENCE (SATISFIES \" \"((:1-* SYMBOL NUMBER))))")
