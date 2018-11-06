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

(in-package :rte-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :rte :package-into :rte-test))

(define-test test/rte-typecase-5
  (assert-true (macroexpand-1 '(rte-typecase data
				(t 42)))))

(define-test test/rte-typecase-3
  (let ((data '(:x 3.4)))
    (assert-true (eq :yes
		     (rte-typecase data
		       ((:cat (:* fixnum) number)
			:no)
		       ((:cat (:or number symbol) number)
			:yes))))))




(define-test test/rte-typecase-1
  (let ((data '(1 2 3 4 )))
    (assert-true (eq :yes
		    (rte-typecase data
		      ((:cat number)
		       :no)
		      ((:cat number number)
		       :no)
		      ((:cat number number number)
		       :no)
		      ((:cat number number number number)
		       :yes)
		      ((:cat number number number number number)
		       :no))))))

(define-test test/rte-typecase-2
  (let ((data '(1 2 3 4 )))
    (assert-true (eq :yes
		     (rte-typecase data
		       ((:cat fixnum)
			:no)
		       ((:cat number number)
			:no)
		       ((:cat number number real)
			:no)
		       ((:cat number number number unsigned-byte)
			:yes)
		       ((:cat fixnum number real unsigned-byte number)
			:no))))))

(define-test test/rte-to-dfa
  (dolist (rte '((:CAT (:* FIXNUM) NUMBER)
		 (:AND (:CAT (:* FIXNUM) NUMBER) (:NOT (:OR)))
		 (:CAT (:OR NUMBER SYMBOL) NUMBER)
		 (:AND (:CAT (:OR NUMBER SYMBOL) NUMBER)
		  (:NOT (:OR (:CAT (:* FIXNUM) NUMBER))))))
    (assert-true (rte-to-dfa rte :trim nil :reduce nil))
    (assert-true (rte-to-dfa rte :trim t :reduce nil))
    (assert-true (rte-to-dfa rte :reduce t))))

(define-test test/rte-typecase-4
  (assert-true (macroexpand-1 '(rte-typecase data
				((:cat (:* fixnum) number)
				 :no)
				((:cat (:or number symbol) number)
				 :yes)))))

