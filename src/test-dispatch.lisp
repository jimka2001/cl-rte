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

(defpackage :dispatch.test
  (:use :cl :dispatch :lisp-unit))

(in-package :dispatch.test)

(defun test ()
  (let ((*print-summary* t)
	(*print-failures* t)
	(*summarize-results* t)
	(*print-errors* t))
    (run-tests :all (list :dispatch.test))))

(defclass T1 () ())
(defclass T2 () ())
(defclass T3 (T1 T2) ())
(defclass T4 (T1 T2) ())
(defclass T5 (T2) ())
(defclass T6 (T3) ())
(defclass T7 (T3 T4) ())
(defclass T8 (T1 T5) ())

(closer-mop:finalize-inheritance (find-class 'T8))

(define-test dispatch/intersections
  (assert-false (set-exclusive-or (dispatch::specializer-intersections (find-class 'T1) (find-class 'T2))
				  (mapcar #'find-class '(T3 T4 T8)))))


(defgeneric foo1 (a b))
(defmethod foo1 ((a number) b))
(defmethod foo1 (b (a number)))

(define-test dispatch/find-method-ambiguities
  (destructuring-bind ((&key qualifiers methods arg-types)) (dispatch::find-method-ambiguities 'foo1)
    (assert-true (equal arg-types (list (find-class 'number)
					(find-class 'number))))
    (assert-true (equal qualifiers nil))
    (assert-true (equal 2 (length methods)))))


(defgeneric foo2 (a b))
(defmethod foo2 ((a number) b))
(defmethod foo2 (b (a number)))
(defmethod foo2 ((a list) b))
(defmethod foo2 (b (a sequence)))

(define-test dispatch/find-method-ambiguities
  (let ((ambig (dispatch::find-method-ambiguities 'foo2)))
    (flet ((getter (key)
	     (lambda (x)
	       (getf x key))))
      (assert-true (equal 4 (length ambig)))

      (dolist (pair '((number number)
		      (number sequence)
		      (list number)
		      (list sequence)))
      
	(assert-true (find (mapcar #'find-class pair)
			   ambig
			   :test #'equal
			   :key (getter :arg-types)))))))
			     
