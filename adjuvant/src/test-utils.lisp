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


(define-test test-prog1-let
  (let (b)
    (assert-true (equal 4 (prog1-let (a 0)
                            1
                            2
                            3
                            (setf a 4)
                            5
                            6
                            7
                            (setf b 8))))
    (assert-true (equal 0 (prog1-let (a 0)
                            1
                            2
                            3
                            4
                            5
                            6
                            7
                            (setf b 8))))
    
    (assert-true (equal 8 b))))

(define-test test/exists-tail
  (assert-true (equal (exists-tail x '(1 3 5 2 x x x)
                        (evenp (car x)))
                      '(2 x x x)))
  (assert-true (null (exists-tail x '(1 2 3 4 5 6)
                       (stringp (car x))))))

(define-test test/env-var
  (let ((env-var (format nil "var0"))
        (index 0))
    (loop :while (getenv env-var)
          :do (setf env-var (format nil "var~D" (incf index))))
    (assert-false (getenv env-var))
    (assert-error error (demand-env-var env-var))
    (assert-true (demand-env-var "USER"))))
          


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

(define-test test/tree-reduce4
  ;; assure we get the same result from CL:REDUCE and TREE-REDUCE
  (assert-true (equal 0 (tree-reduce #'* '(1 0 2 3 4))))
  (assert-true (equal 0 (tree-reduce #'* '(1 0 2 3 4) :stop-when 0)))
  (let ((side-effect 0))
    (assert-true (equal 0 (tree-reduce #'* (list (lambda ()
                                                   1)
                                                 (lambda ()
                                                   2)
                                                 (lambda ()
                                                   0)
                                                 (lambda ()
                                                   (incf side-effect)))
                                       :key #'funcall )))
    (assert-true (equal 1 side-effect)))
  (let ((side-effect 0))
    (assert-true (equal 0 (tree-reduce #'* (list (lambda ()
                                                   1)
                                                 (lambda ()
                                                   2)
                                                 (lambda ()
                                                   0)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   3))
                                       :stop-when 0
                                       :key #'funcall )))
    (assert-true (equal 0 side-effect)))
  (let ((side-effect 0))
    (assert-true (equal 0 (tree-reduce #'* (list (lambda ()
                                                   1)
                                                 (lambda ()
                                                   2)
                                                 (lambda ()
                                                   0)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   3)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   4))
                                       :stop-when 0
                                       :key #'funcall )))
    (assert-true (equal 0 side-effect)))
  (let ((side-effect 0))
    (assert-true (equal 0 (tree-reduce #'* (list (lambda ()
                                                   1)
                                                 (lambda ()
                                                   2)
                                                 (lambda ()
                                                   0)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   3)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   4)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   5))
                                       :stop-when 0
                                       :key #'funcall )))
    (assert-true (equal 0 side-effect)))
  (let ((side-effect 0))
    (assert-true (equal 0 (tree-reduce #'* (list (lambda ()
                                                   1)
                                                 (lambda ()
                                                   2)
                                                 (lambda ()
                                                   0)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   3)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   4)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   5)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   6))
                                       :stop-when 0
                                       :key #'funcall )))
    (assert-true (equal 0 side-effect)))
  (let ((side-effect 0))
    (assert-true (equal 0 (tree-reduce #'* (list (lambda ()
                                                   1)
                                                 (lambda ()
                                                   2)
                                                 (lambda ()
                                                   0)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   3)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   4)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   5)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   6)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   7))
                                       :stop-when 0
                                       :key #'funcall )))
    (assert-true (equal 0 side-effect)))

  (let ((side-effect 0))
    (assert-true (equal 0 (tree-reduce #'logand (list (lambda ()
                                                        #b1)
                                                 (lambda ()
                                                   #b10)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   #b101)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   #b110)
                                                 (lambda ()
                                                   (incf side-effect)
                                                   #b11))
                                       :stop-when 0
                                       :key #'funcall )))
    (assert-true (equal 0 side-effect)))
  )

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
		      ;; make sure KEY is applied to first element of singleton list
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
		

(define-test type/fixed-point
  (assert-true (< (fixed-point #'(lambda (obj)
				   (/ obj 2.0))
			       10
			       :test #'(lambda (a b)
					 (< (abs (- a b)) 0.01)))
		  0.01))
  (let ((data (list 1 2 3)))
    (assert-true (eq data
		     (fixed-point #'copy-list
				  data
				  :test #'equal)))))

(define-test types/find-duplicates
  (assert-true (equal '(a b) (find-duplicates '(a b a b)))))


(define-test test-diff-files
  (assert-false (diff-files "/bin/ls" "/bin/ls"))
  (assert-true (diff-files "/bin/ls" "/dev/null"))
  (assert-true (diff-files "/dev/null" "/bin/ls"))
  (assert-false (diff-files "/dev/null" "/dev/null"))
  (assert-true (diff-files "/bin/ls" "/bin/cp"))
  (assert-true (diff-files "/bin/cp" "/bin/ls")))

(define-test test-destructuring-lambda
  (let ((executed nil))
    (funcall (destructuring-lambda (a (b) (&key c))
               (assert-true (equal 1 a))
               (assert-true (equal 2 b))
               (assert-true (equal 3 c))
               (setf executed t))
             1 '(2) '(:C 3))
    (assert-true executed))

  (let ((count 0))
    
    (apply (destructuring-lambda (a (b) &key c d &allow-other-keys)
             (incf count)
             (assert-true (equal 1 a))
             (assert-true (equal 2 b))
             (assert-true (equal 4 c))
             (assert-true (equal 1 d))
             )
           '(1 (2) :d 1 :a 2 :b 3 :c 4))
    
    (assert-true (eql count 1)))

  (let ((count 0))
    
    (apply (destructuring-lambda (a (b) (&key c d &allow-other-keys))
             (incf count)
             (assert-true (equal 1 a))
             (assert-true (equal 2 b))
             (assert-true (equal 4 c))
             (assert-true (equal 1 d))
             )
           '(1 (2) (:d 1 :a 2 :b 3 :c 4)))

    (assert-true (eql count 1)))

  (let ((count 0))
    
    (mapcar (destructuring-lambda ((a (b) (&key c d &allow-other-keys)))
              (incf count)
              (assert-true (equal 1 a))
              (assert-true (equal 2 b))
              (assert-true (equal 4 c))
              (assert-true (equal 1 d))
              )
            '((1 (2) (:d 1 :a 2 :b 3 :c 4))
              (1 (2) (:d 1 :a 2 :b 3 :c 4))
              (1 (2) (:d 1 :a 2 :b 3 :c 4))))

    (assert-true (eql count 3)))

  (let ((count 0))
    
    (mapcar (destructuring-lambda (a (b) (&key c d &allow-other-keys))
              (incf count)
              (assert-true (equal 1 a))
              (assert-true (equal 2 b))
              (assert-true (equal 4 c))
              (assert-true (equal 1 d))
              )
            '(1 1 1)
            '((2) (2) (2))
            
            '((:d 1 :a 2 :b 3 :c 4)
              (:d 1 :a 2 :b 3 :c 4)
              (:d 1 :a 2 :b 3 :c 4)))

    (assert-true (eql count 3))))

(define-test test-destructuring-let
  (let ((executed nil))
    (destructuring-let ((a 1)
                        ((b) '(2))
                        ((&key c) '(:c 3))
                        )
      (assert-true (equal 1 a))
      (assert-true (equal 2 b))
      (assert-true (equal 3 c))
      (setf executed t)
      )
    (assert-true executed)
  ))

(define-test test-topological-sort
  (let* ((dependency-graph '((des-system-lib   std synopsys std-cell-lib des-system-lib dw02 dw01 ramlib ieee)
                             (dw01             ieee dw01 dware gtech)
                             (dw02             ieee dw02 dware)
                             (dw03             std synopsys dware dw03 dw02 dw01 ieee gtech)
                             (dw04             dw04 ieee dw01 dware gtech)
                             (dw05             dw05 ieee dware)
                             (dw06             dw06 ieee dware)
                             (dw07             ieee dware)
                             (dware            ieee dware)
                             (gtech            ieee gtech)
                             (ramlib           std ieee)
                             (std-cell-lib     ieee std-cell-lib)
                             (synopsys)))
         (sorted (topological-sort dependency-graph)))
    (dolist (node dependency-graph)
      (destructuring-bind (later &rest earlier) node
        (dolist (early earlier)
          (assert-true (member later (member early sorted))))))))

(define-test test/bfs-graph
  (let* ((edge-list '((a b)
                      (b c)
                      (c d)
                      (d a)))
         (adj-hash (edges-to-adjacency-hash edge-list :test #'eq)))
    (assert-true (hash-table-p adj-hash))
    (let ((edges nil))
      (bfs-graph 'a
                 adj-hash
                 (lambda (to from)
                   (push (list to from) edges)))
      (assert-true (= 4 (length edges)))
      (assert-true (member '(a a) edges :test #'equal))
      (assert-true (member '(b a) edges :test #'equal))
      (assert-true (member '(c b) edges :test #'equal))
      (assert-true (member '(d c) edges :test #'equal))
      (assert-false (member '(a d) edges :test #'equal)))))
        
(define-test test/empty-file-p
  (assert-true (empty-file-p "/dev/null"))
  (assert-false (empty-file-p "/etc/hosts")))

(define-test test/count-1-bits
  (assert-true (eql 0 (count-1-bits 0)))
  (assert-true (eql 1 (count-1-bits 8)))
  (assert-true (eql 3 (count-1-bits(+  8 4 2)))))


(define-test test/count-bit-diffs
  (assert-true (eql 1 (count-bit-diffs 0 1)))
  (assert-true (eql 1 (count-bit-diffs 0 8)))
  (assert-true (eql 1 (count-bit-diffs 8 0)))
  (assert-true (eql 4 (count-bit-diffs 8 7)))
  (assert-true (eql 3 (count-bit-diffs (+    8 4   1)
                                       (+ 16 8 4 2 )))))


(define-test test/sort-unique
  (assert-true (equal '(1 2 3 4 5)
                      (sort-unique '(1 2 3 4 5) #'< #'=)))
  (assert-true (equal '(1 2 3 4 5)
                      (sort-unique '(1 2 3 3 4 3 5) #'< #'=)))
  (assert-true (equal '(1 2 3 4 5)
                      (sort-unique '(1 2 1 3 1 4 1 2 3 3 4 3 5) #'< #'=))))

(define-test test/gnu-plot
  (let ((name (make-temp-file-name "plot" :extension "gnu")))
    (adjuvant:gnu-plot name :data '((:title "first"
                            :xys ((1 1.1)
                                  (2 2.11)
                                  (3 3.111)
                                  (4 4.1111)))
                           (:title "second"

                            :xys ((1.1 1.1)
                                  (2.2 2.11)
                                  (3.3 3.111)
                                  (4.4 4.1111)))
                           (:title "second"
                            :xys ((1.11 1.1)
                                  (2.21 2.11)
                                  (2.7  2.88)
                                  (3.31 3.111)
                                  (3.8  3.76
                                  (4.41 4.1111))))))))
                           
                           
