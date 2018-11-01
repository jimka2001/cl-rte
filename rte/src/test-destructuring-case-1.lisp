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

(define-test test/destructuring-case-alt-1-a
  (assert-true
   (equal 3 (destructuring-case-alt '(x y z) 
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      ((a b c) ()
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-alt-1-b
  (assert-true
   (equal 2 (destructuring-case-alt '(x y)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      ((a b c) ()
	       (declare (ignore a b c))
	       3))))  )

(define-test test/destructuring-case-alt-1-c
  (assert-true
   (equal 1 (destructuring-case-alt '(x)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      ((a b c) ()
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-alt-2-a
  (assert-true
   (equal 3 (destructuring-case-alt '((x) y z)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a b) ()
	       (declare (ignore a b))
	       2)
	      (((a) b c) ()
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-alt-2-b  
  (assert-true
   (equal 2 (destructuring-case-alt '(x (y))
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a (b)) ()
	       (declare (ignore a b))
	       2)
	      (((a) b) ()
	       (declare (ignore a b))
	       3))))  )


(define-test test/destructuring-case-alt-2-c
  (assert-true
   (equal 3 (destructuring-case-alt '((x) y)
	      ((a) ()
	       (declare (ignore a))
	       1)
	      ((a (b)) ()
	       (declare (ignore a b))
	       2)
	      (((a) b) ()
	       (declare (ignore a b))
	       3))))
)
   
(define-test test/destructuring-case-alt-3
  (let ((n 0))
    (dolist (x '((1)
		 (2 (3))
		 (1 ((2)) (3 4))))
      (destructuring-case-alt x
	((a) ()
	 (incf n)
	 (assert-true (= a 1)))
	((a (b)) ()
	 (incf n)
	 (assert-true (= a 2))
	 (assert-true (= b 3)))
	((a ((b)) (c d)) ()
	 (incf n)
	 (assert-true (= a 1))
	 (assert-true (= b 2))
	 (assert-true (= c 3))
	 (assert-true (= d 4)))))

    (assert-true (= n 3))))



;; ==============


(define-test test/destructuring-case-1-a
  (assert-true
   (equal 3 (destructuring-case '(x y z) 
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      ((a b c)
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-1-b
  (assert-true
   (equal 2 (destructuring-case '(x y)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      ((a b c)
	       (declare (ignore a b c))
	       3))))  )

(define-test test/destructuring-case-1-c
  (assert-true
   (equal 1 (destructuring-case '(x)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      ((a b c)
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-2-a
  (assert-true
   (equal 3 (destructuring-case '((x) y z)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a b)
	       (declare (ignore a b))
	       2)
	      (((a) b c)
	       (declare (ignore a b c))
	       3)))))

(define-test test/destructuring-case-2-b  
  (assert-true
   (equal 2 (destructuring-case '(x (y))
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a (b))
	       (declare (ignore a b))
	       2)
	      (((a) b)
	       (declare (ignore a b))
	       3))))  )


(define-test test/destructuring-case-2-c
  (assert-true
   (equal 3 (destructuring-case '((x) y)
	      ((a)
	       (declare (ignore a))
	       1)
	      ((a (b))
	       (declare (ignore a b))
	       2)
	      (((a) b)
	       (declare (ignore a b))
	       3))))
)
   
(define-test test/destructuring-case-3
  (let ((n 0))
    (dolist (x '((1)
		 (2 (3))
		 (1 ((2)) (3 4))))
      (destructuring-case x
	((a)
	 (incf n)
	 (assert-true (= a 1)))
	((a (b))
	 (incf n)
	 (assert-true (= a 2))
	 (assert-true (= b 3)))
	((a ((b)) (c d))
	 (incf n)
	 (assert-true (= a 1))
	 (assert-true (= b 2))
	 (assert-true (= c 3))
	 (assert-true (= d 4)))))

    (assert-true (= n 3))))

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

(define-test ndfa/test-reduce-1
  (let* ((dfa (make-ndfa '((:label i :initial-p t
			    :transitions ((:next-label 1 :transition-label fixnum)
					  (:next-label 2 :transition-label (and number (not fixnum)))))
			   (:label f :final-p t)
			   (:label 1
			    :transitions ((:next-label f :transition-label number)))
			   (:label 2
			    :transitions ((:next-label f :transition-label number))))))
	 (reduced-dfa (reduce-state-machine dfa
					    :equal-labels (lambda (a b)
							    (and (subtypep a b)
								 (subtypep b a)))
					    :combine (lambda (a b)
						       (type-to-dnf-bottom-up `(or ,a ,b))))))
    (assert-true (= 3 (length (states reduced-dfa))))))

