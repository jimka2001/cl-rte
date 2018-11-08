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

(define-test test/rte-typecase-3
  (let ((data '(:x 3.4)))
    (assert-true (eq :yes
		     (rte-typecase data
		       ((:cat (:* fixnum) number)
			:no)
		       ((:cat (:or number symbol) number)
			:yes))))))

(define-test test/rte-typecase-4
  (assert-true (macroexpand-1 '(rte-typecase data
				(t 42)))))

(define-test test/rte-typecase-5
  (assert-true (macroexpand-1 '(rte-typecase data
				((:cat (:* fixnum) number)
				 :no)
				((:cat (:or number symbol) number)
				 :yes)))))

(define-test test/rte-typecase-6
  (let ((data '(1)))
    (assert-true (eql :here
		      (block block
			(tagbody
			   (go 2)
			 1
			   (return-from block
			     :here)
			 2
			   (rte-typecase data
			     ((:cat fixnum)
			      (go 1))
			     )
			 :no-here))))))

(define-test test/rte-typecase-7
  (let ((data '(1 2 3 )))
    (assert-true (eql :yes
		     (rte-typecase data
		       ((:cat fixnum)
			:no)
		       ((:cat number number string)
			:no)
		       ((:cat string number number )
			:no)
		       ((:cat number number fixnum)
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

(define-test test/synchronized-product
  (let ((rte-t (rte-to-dfa t)))
    (assert-true (eq rte-t
		     (rte-synchronized-product (list rte-t)))))
  (let ((rte-number (rte-to-dfa 'number))
	(rte-string (rte-to-dfa 'string)))
    (assert-true (= 2 (length (states rte-number))))
    (assert-true (= 2 (length (states rte-string))))
    (let ((rte-or (synchronized-product rte-number rte-string :boolean-function (lambda (a b) (or a b)))))
      (assert-true (= 2 (length (states rte-or))))
      (dolist (st-i (get-initial-states rte-or))
	(dolist (trans (transitions st-i))
	  (assert-true (member (transition-label trans)
			       '((or string number)
				 (or number string)) :test #'equal)))))
		  
    (assert-true (= 2 (length (states (synchronized-product rte-string rte-number :boolean-function (lambda (a b) (or a b)))))))
    (assert-true (= 0 (length (states (synchronized-product rte-string rte-number :boolean-function (lambda (a b) (and a b)))))))

    (let ((dfa-or (rte-synchronized-product (list rte-number rte-string))))
      (assert-true (= 2 (length (states dfa-or))))
      (dolist (st-i (get-initial-states dfa-or))
	(dolist (trans (transitions st-i))
	  (assert-true (member (transition-label trans)
			       '((or string number)
				 (or number string)) :test #'equal)))))))
    
    
    
