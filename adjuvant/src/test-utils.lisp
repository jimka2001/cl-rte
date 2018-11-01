;; Copyright (c) 2018 EPITA Research and Development Laboratory
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



(defpackage :adjuvant-test
  (:use :cl :adjuvant :scrutiny))

(in-package :adjuvant-test)

(define-test test/tconc
  (let ((buf (list nil)))
    (tconc buf 1)
    ;; test that we can add to a tconc struct while iterating over it
    (dolist-tconc (item buf)
      (when (= 1 item)
	(tconc buf 2)))
    (assert-true (equal (car buf) '(1 2)))))

(define-test test/dolist-tconc
  (let ((buf (list nil)))
    (tconc buf 10)
    (dolist-tconc (item buf)
      (when (and (not (member (1- item) (car buf)))
		 (plusp (1- item)))
	(tconc buf (1- item))))
    (assert-true (equal (car buf)
			'(10 9 8 7 6 5 4 3 2 1)))))



(define-test test/group-by
  (assert-true (null (group-by nil)))
  (assert-false (set-exclusive-or '((1 (1 1 1)) (2 (2 2 2)) (3 (3 3)) (4 (4)))
				  (group-by '(1 2 1 2 3 1 2 3 4))
				  :test #'equal))
  (assert-false (set-exclusive-or (group-by '((1 2) (1) (1 2) (2)) :test #'equal)
				  '(((1 2) ((1 2) (1 2))) ((1) ((1))) ((2) ((2))))
				  :test #'equal))
				  
  (assert-false (set-exclusive-or (group-by '((3 1 2) (1) (1 2) (2) (3 4)) :test #'equal)
				  '(((3 1 2) ((3 1 2)))
				    ((1) ((1)))
				    ((1 2) ((1 2)))
				    ((2) ((2)))
				    ((3 4) ((3 4))))
				  :test #'equal))
  (assert-false (set-exclusive-or '((1 ("a")) (2 ("ab" "ba")))
				  (group-by '("ba" "a" "ab") :key #'length)
				  :test #'equal))
  (assert-false (set-exclusive-or '((#\e (#\e))
				    (#\d (#\d))
				    (#\c (#\c #\c))
				    (#\b (#\b #\b))
				    (#\a (#\a #\a)))
				  (group-by "abcabcde" :test #'char=)
				  :test #'equal)))

(define-test test/exists
  (assert (exists x '(1 3 2 7) (evenp x)))
  (assert (not (exists x '(1 3 5 7) (evenp x))))
  (assert (exists (&key (x 0)) '((:x 1) (:x 3) (:x 2) (:x 7)) (evenp x)))
  (assert (not (exists (&key (x 0) &allow-other-keys) '((:y 0 :x 1) (:x 3 :z 1) (:x 3 :x 2) (:x 7)) (evenp x)))))

(define-test test/forall
  (assert (forall x '(1 3 5 7) (oddp x)))
  (assert (not (forall x '(1 3 2 7) (oddp x))))
  (assert (forall (&key (x 0)) '((:x 1) (:x 3) (:x 5) (:x 7)) (oddp x)))
  (assert (not (forall (&key (x 0) &allow-other-keys) '((:y 0 :x 1) (:x 3 :z 1) (:x 2 :x 3) (:x 7)) (oddp x)))))

(define-test test/setof
  (assert-false (set-exclusive-or '(1 3 5)
				  (setof x '( 1 2 3 5 2 8)
				    (oddp x))))

  (assert-false (set-exclusive-or '((:y 0 :x 1) (:x 3 :z 1) (:x 7))
				 (setof (&key (x 0) &allow-other-keys) '((:y 0 :x 1) (:x 3 :z 1) (:x 2 :x 3) (:x 7))
				   (oddp x))
				 :test #'equal)))

(define-test test/tree-reduce2
  (flet ((sqr (x)
	   (* x x)))
    ;; sum of the squares of 1 ... 8
    (assert-true (= (tree-reduce #'+ '(1 2 3 4 5 6 7 8)
				 :initial-value 0
				 :key #'sqr)
		    (reduce #'+ '(1 2 3 4 5 6 7 8) :initial-value 0 :key #'sqr)))))

(define-test test/tree-reduce3
  (assert-true (equal '(0)
		      (tree-reduce #'+ nil :initial-value '(0))))
  (assert-true (equal '1
		      ;; make sure KEY is apoplied to first element of singleton list
		      (tree-reduce #'+ '((1)) :initial-value 0 :key #'car))))

(define-test test/tree-reduce
  (assert-true (= (+ 1 2 3 4 5 6 7 8 9 )
		  (tree-reduce #'+ '(1 2 3 4 5 6 7 8 9)
				      :initial-value 0)))
  (assert-true (= (* 1 2 3 4 5 6 7 8 9 )
		  (tree-reduce #'* '(1 2 3 4 5 6 7 8 9)
				      :initial-value 1)))
  (assert-true (= (* 1 2 3 4 5 6 7 8 )
		  (tree-reduce #'* '(1 2 3 4 5 6 7 8)
				      :initial-value 1)))
  (assert-true (= (* 1 2 3 4 5 6 7)
		  (tree-reduce #'* '(1 2 3 4 5 6 7)
				      :initial-value 1)))
  (assert-true (= 0
		  (tree-reduce #'+ nil
				      :initial-value 0)))
  (assert-true (= 3
		  (tree-reduce #'+ '(3)
				      :initial-value 0)))
  (let (nums)
    (loop :for i :from 1 :to 100
	  :do (push i nums)
	  :do (assert-true (= (reduce #'+ nums :initial-value 0)
			      (tree-reduce #'+ nums :initial-value 0)))
	  :do (assert-true (= (reduce #'* nums :initial-value 1)
			      (tree-reduce #'* nums :initial-value 1)))))
  )

(define-test test/group-by-equivalence
  (assert-true (equal
		(sort (group-by-equivalence '(1 4 4 4 2 3 2 3 4 3 4)) #'< :key #'car)
		'((1) (2 2) (3 3 3) (4 4 4 4 4))))
  (let ((grouped (group-by-equivalence '((1 2 3) ; 6
					      (3 4 -1) ; 6
					      (6 0) ; 6
					      (1 -1) ; 0
					      (2 2 -4) ;0
					      )
					    :key (lambda (numbers)
						   (reduce #'+ numbers :initial-value 0)))))
    ;; e.g., (((1 -1) (2 2 -4))
    ;;        ((1 2 3) (6 0) (3 4 -1)))
    (dolist (group grouped)
      (dolist (numbers (cdr group))
	(assert-true (= (reduce #'+ numbers :initial-value 0)
			(reduce #'+ (car group) :initial-value 0)))))))
		
