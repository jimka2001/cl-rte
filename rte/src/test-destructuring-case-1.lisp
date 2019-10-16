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

(garbage-collect)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :rte :package-into :rte-test))

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

(define-test test-destructuring-lambda-list-to-rte
  (assert-true (equal :empty-set
                      (canonicalize-pattern '(:and t (:cat t t)))))
  (assert-true (equal :empty-set
                      (canonicalize-pattern '(:and symbol (:cat symbol t)))))
  (assert-true (equal :empty-set
                      (canonicalize-pattern '(:and :empty-word symbol))))
  (assert-true (equal 'symbol
                      (canonicalize-pattern '(:and symbol (:* t)))))
  (assert-true (equivalent-patterns '(:cat t t)
                                    (destructuring-lambda-list-to-rte '(a b))))
  (assert-true (equivalent-patterns '(:cat t t)
                                    (canonicalize-pattern (destructuring-lambda-list-to-rte '(a b)))))
  (assert-true (equivalent-patterns '(:cat t)
                                    (destructuring-lambda-list-to-rte '(a))))
  (assert-true (equivalent-patterns '(:cat t)
                                    (canonicalize-pattern (destructuring-lambda-list-to-rte '(a)))))
  (assert-true (equivalent-patterns '(:cat)
                                    (destructuring-lambda-list-to-rte '())))
  (assert-true (equivalent-patterns '(:cat)
                                    (canonicalize-pattern (destructuring-lambda-list-to-rte '())))))
  


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

(define-test test-231
  (assert-true (equal 3 (RTE-CASE '((X) Y)
                     ((:CAT (:AND LIST (RTE T)) T)
                      3))))
  (assert-true (equal 3 (RTE-CASE '((X) Y)
                          ((:CAT T (:AND LIST (RTE T)))
                           2)
                          ((:CAT (:AND LIST (RTE T)) T)
                           3))))
  (assert-true (equal 3 (RTE-CASE '((X) Y)
                     (T 1)
                     ((:CAT T (:AND LIST (RTE T)))
                      2)
                     ((:CAT (:AND LIST (RTE T)) T)
                      3)))))

;; (defrte (:AND LIST (RTE T)))

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




(define-test ndfa/test-trim-0
  (let ((dfa (rte-to-dfa '(:AND (:CAT (:OR NUMBER SYMBOL) NUMBER)
                           (:NOT (:OR (:CAT (:* FIXNUM) NUMBER)))) :trim t :reduce nil)))
    ;;(ndfa-to-dot dfa t :view nil :transition-legend t :state-legend t)
    ;;(ndfa-to-dot dfa nil :view t :transition-legend t :state-legend t)
    ;; assert that all transitions point to a state which is actually
    ;; in the state list of the dfa
    (dolist (state (states dfa))
      (dolist (transition (transitions state))
        (assert-true (member (next-state transition) (states dfa) :test #'eq)))
      (assert-false (member nil (transitions state))))))

(define-test ndfa/test-reduce-0
  (let* ((pattern '(:AND (:CAT (:OR NUMBER SYMBOL) NUMBER)
                    (:NOT (:OR (:CAT (:* FIXNUM) NUMBER)))))
         (dfa (rte-to-dfa pattern :trim nil :reduce nil))
         (dfa-trim (rte-to-dfa pattern :trim t :reduce nil))
         (dfa-reduce (rte-to-dfa pattern :trim t :reduce t))
         )
    
    (assert-true (= 7 (length (states dfa))))
    (assert-true (= 4 (length (states dfa-trim))))
    (assert-true (= 3 (length (states dfa-reduce))))
))


(define-test ndfa/test-reduce-1
  (let* ((dfa (make-ndfa '((:label i :initial-p t
                            :transitions ((:next-label 1 :transition-label fixnum)
                                          (:next-label 2 :transition-label (and number (not fixnum)))))
                           (:label f :final-p t)
                           (:label 1
                            :transitions ((:next-label f :transition-label number)))
                           (:label 2
                            :transitions ((:next-label f :transition-label number))))))
         (reduced-dfa (minimize-state-machine dfa
                                              :equal-labels (lambda (a b)
                                                              (and (subtypep a b)
                                                                   (subtypep b a)))
                                              :combine (lambda (a b)
                                                         (type-to-dnf-bottom-up `(or ,a ,b))))))
    (assert-true (= 3 (length (states reduced-dfa))))))

(define-test ndfa/test-reduce-2
  (let* ((T1 'fixnum)
         (T3 'number)
         (T4 'string)
         (T6 '(and integer (not fixnum)))
         (T7 '(and number (not integer)))
         (T8 '(and integer
               (not fixnum)
               (or fixnum (not integer))))
         (dfa (make-ndfa `((:label 0 :initial-p t
                            :transitions ((:next-label 2 :transition-label ,T4)
                                          (:next-label 1 :transition-label ,T1)))
                           (:label 1
                            :transitions ((:next-label 6 :transition-label ,T7)
                                          (:next-label 5 :transition-label ,T1)
                                          (:next-label 4 :transition-label ,T6)
                                          (:next-label 3 :transition-label ,T8)))
                           (:label 2
                            :transitions ((:next-label 7 :transition-label ,T3)))
                           (:label 3)
                           (:label 4 :final-p t :exit-form :clause-2)
                           (:label 5 :final-p t :exit-form :clause-1)
                           (:label 6 :final-p t :exit-form :clause-3)
                           (:label 7 :final-p t :exit-form :clause-3)
                           )))
         (reduced-dfa (minimize-state-machine dfa
                                              :equal-labels (lambda (a b)
                                                              (and (subtypep a b)
                                                                   (subtypep b a)))
                                              :combine (lambda (a b)
                                                         (type-to-dnf-bottom-up `(or ,a ,b))))))
    (assert-true (= 6 (length (states reduced-dfa))))))

(define-test ndfa/test-reduce-3
  (let* ((T1 'fixnum)
         (T2 'integer)
         (T3 'number)
         (T4 'string)
         (T6 '(and integer (not fixnum)))
         (T7 '(and number (not integer)))
         (T8 '(and integer
               (not fixnum)
               (or fixnum (not integer))))
         (T9 'symbol)
         (dfa (make-ndfa `((:label 0 :initial-p t
                            :transitions ((:next-label 2 :transition-label ,T4)
                                          (:next-label 1 :transition-label ,T1)
                                          (:next-label 8 :transition-label ,T9)))
                           (:label 1
                            :transitions ((:next-label 6 :transition-label ,T7)
                                          (:next-label 5 :transition-label ,T1)
                                          (:next-label 4 :transition-label ,T6)
                                          (:next-label 3 :transition-label ,T8)))
                           (:label 2
                            :transitions ((:next-label 7 :transition-label ,T3)))
                           (:label 3)
                           (:label 4 :final-p t :exit-form :clause-2)
                           (:label 5 :final-p t :exit-form :clause-1)
                           (:label 6 :final-p t :exit-form :clause-3)
                           (:label 7 :final-p t :exit-form :clause-3)
                           (:label 8
                            :transitions ((:next-label 7 :transition-label ,T2)
                                          (:next-label 6 :transition-label ,T7)))
                           )))
         (reduced-dfa (minimize-state-machine dfa
                                              :equal-labels (lambda (a b)
                                                              (and (subtypep a b)
                                                                   (subtypep b a)))
                                              :combine (lambda (a b)
                                                         (type-to-dnf-bottom-up `(or ,a ,b))))))
    (assert-true (= 6 (length (states reduced-dfa))))))

(define-test ndfa/test-reduce-4
  (let* ((T1 'fixnum)
         (T3 'number)
         (T4 'string)
         (T6 '(and integer (not fixnum)))
         (T7 '(and number (not integer)))
         (T8 '(and integer
               (not fixnum)
               (or fixnum (not integer))))
         (T9 'symbol)
         (dfa (make-ndfa `((:label 0 :initial-p t
                            :transitions ((:next-label 2 :transition-label ,T4)
                                          
                                          (:next-label 8 :transition-label ,T9)))
                          
                           (:label 2
                            :transitions ((:next-label 7 :transition-label ,T3)))
                           (:label 3)
                           (:label 4 :final-p t :exit-form :clause-2)
                           (:label 5 :final-p t :exit-form :clause-1)
                           (:label 6 :final-p t :exit-form :clause-3)
                           (:label 7 :final-p t :exit-form :clause-3)
                           (:label 8
                            :transitions ((:next-label 7 :transition-label ,T3)
                                          (:next-label 6 :transition-label ,T7)))
                           )))
         (reduced-dfa (minimize-state-machine dfa
                                              :equal-labels (lambda (a b)
                                                              (and (subtypep a b)
                                                                   (subtypep b a)))
                                              :combine (lambda (a b)
                                                         (type-to-dnf-bottom-up `(or ,a ,b))))))
    ;;(ndfa::ndfa-to-dot dfa nil :view t :prefix "dfa")
    ;;(ndfa::ndfa-to-dot reduced-dfa nil :view t :prefix "reduced-dfa")
    (assert-true (= 3 (length (states reduced-dfa))))

    ))
