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


(defpackage :2d-array-test
  (:use :cl :2d-array :jimka-test))

(in-package :2d-array-test)

(defun test ()
  (run-package-tests :2d-array-test))

(define-test 2d-array/test1
  (let* ((arr (make-array '(3 2) :initial-contents '((1 2)
						     (3 4)
						     (5 6))))
	 (row-vector (make-instance '2d-array:row-vector
				    :2d-array arr :row 2))
	 (column-vector (make-instance '2d-array:column-vector
				       :2d-array arr :column 1))
	 (vector-of-rows (make-instance '2d-array:vector-of-rows
					:2d-array arr))
	 (vector-of-columns (make-instance '2d-array:vector-of-columns
					   :2d-array arr)))
	 
    ;; length
    (assert-true (= 2 (sequence:length vector-of-columns)))
    (assert-true (= 3 (sequence:length vector-of-rows)))
    (assert-true (= 2 (sequence:length row-vector)))
    (assert-true (= 3 (sequence:length column-vector)))


    ;; elt
    (assert-true (= 5 (sequence:elt row-vector 0)))
    (assert-true (= 6 (sequence:elt row-vector 1)))

    (assert-true (= 2 (sequence:elt column-vector 0)))
    (assert-true (= 4 (sequence:elt column-vector 1)))
    (assert-true (= 6 (sequence:elt column-vector 2)))

    (assert-true (= 1 (sequence:elt (sequence:elt vector-of-rows 0)
				    0)))
    (assert-true (= 2 (sequence:elt (sequence:elt vector-of-rows 0)
				    1)))
    (assert-true (= 3 (sequence:elt (sequence:elt vector-of-rows 1)
				    0)))
    (assert-true (= 4 (sequence:elt (sequence:elt vector-of-rows 1)
				    1)))
    (assert-true (= 5 (sequence:elt (sequence:elt vector-of-rows 2)
				    0)))
    (assert-true (= 6 (sequence:elt (sequence:elt vector-of-rows 2)
				    1)))

    (assert-true (= 1 (sequence:elt (sequence:elt vector-of-columns 0)
				    0)))
    (assert-true (= 3 (sequence:elt (sequence:elt vector-of-columns 0)
				    1)))
    (assert-true (= 5 (sequence:elt (sequence:elt vector-of-columns 0)
				    2)))
    (assert-true (= 2 (sequence:elt (sequence:elt vector-of-columns 1)
				    0)))
    (assert-true (= 4 (sequence:elt (sequence:elt vector-of-columns 1)
				    1)))
    (assert-true (= 6 (sequence:elt (sequence:elt vector-of-columns 1)
				    2)))

    ;; setf elt
    (assert-true (= 42 (setf (sequence:elt row-vector 0) 42)))
    (assert-true (= 42 (sequence:elt row-vector 0)))
    (assert-true (= 42 (aref arr 2 0)))

    (assert-true (= 43 (setf (sequence:elt column-vector 2) 43)))
    (assert-true (= 43 (sequence:elt column-vector 2)))
    (assert-true (= 43 (aref arr 2 1)))

    (assert-true (equal '(100 200 300) (setf (sequence:elt vector-of-columns 1)
					'(100 200 300))))
    (assert-true (= 100 (aref arr 0 1)))
    (assert-true (= 200 (aref arr 1 1)))
    (assert-true (= 300 (aref arr 2 1)))
    
    (assert-true (equal '(-1 -2) (setf (sequence:elt vector-of-rows 2)
				       '(-1 -2))))
    (assert-true (= -1 (aref arr 2 0)))
    (assert-true (= -2 (aref arr 2 1)))
    

    (assert (= 2 (count-if #'numberp row-vector)))
    (assert (= 3 (count-if #'numberp column-vector)))
    (assert (= 2 (count-if #'(lambda (obj) (typep obj 'sequence)) vector-of-columns)))
    (assert (= 3 (count-if #'(lambda (obj) (typep obj 'sequence)) vector-of-rows)))


    ))
