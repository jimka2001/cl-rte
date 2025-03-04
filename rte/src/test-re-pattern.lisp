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

;; TODO add some tests with satisfies types. (satisfies oddp) (satisfies evenp) etc.

(define-test type/match-empty
  (assert-true (typep #() '(rte (:cat))))
  (assert-true (typep () '(rte (:cat))))
  (assert-false (typep #(t) '(rte (:cat))))
  (assert-false (typep #(t) '(rte (:cat)))))

(define-test type/typep-1
  (assert-true (typep '(:x 3 :y 4)
                      '(RTE
                        (:CAT
                         (:OR
                          (:CAT (:OR (:CAT (EQL :X) T)
                                 :EMPTY-WORD)
                           (:OR (:CAT (EQL :Y) T)
                            :EMPTY-WORD))
                          (:CAT (:OR (:CAT (EQL :Y) T)
                                 :EMPTY-WORD)
                           (:OR (:CAT (EQL :X) T)
                            :EMPTY-WORD))))))))

(define-test type/typep-2
  (assert-true (typep '(:x 3 :y 4)
                      '(RTE
                        (:OR
                         (:CAT (:OR (:CAT (EQL :X) T)
                                :EMPTY-WORD)
                          (:OR (:CAT (EQL :Y) T)
                           :EMPTY-WORD))
                         (:CAT (:OR (:CAT (EQL :Y) T)
                                :EMPTY-WORD)
                          (:OR (:CAT (EQL :X) T)
                           :EMPTY-WORD)))))))

(define-test type/typep-2b
  (assert-true (typep '(:x 3 :y 4)
                      '(RTE
                        (:OR
                         (:CAT (:OR (:CAT (EQL :X) T)
                                :EMPTY-WORD)
                          (:OR (:CAT (EQL :Y) T)
                           :EMPTY-WORD))
                         (:CAT (:OR (:CAT (EQL :Y) T)
                                :EMPTY-WORD)
                          (:OR (:CAT (EQL :X) T)
                           :EMPTY-WORD)))))))



(define-test type/typep-13
  (assert-true (typep '(:x 3 :y 4)
                      '(RTE
                        (:OR
                         (:CAT (:OR (:CAT (EQL :X) T)
                                :EMPTY-WORD)
                          (:OR (:CAT (EQL :Y) T)
                           :EMPTY-WORD)))))))



#+sbcl
(define-test type/match-sequence
  (assert-true (match-sequence '(1 2 3) '(:cat number number number)))

  (let ((2d (make-array '(5 4)
                        :initial-contents '((b0 b1 b2 b3)
                                            (b0 1  2  b3)
                                            (b0 2  3  b3)
                                            (b0 4  5  b3)
                                            (b0 b1 b2 b3)))))
    (assert-true (match-sequence (make-instance '2d-array:row-vector :2d-array 2d :row 0)
                                     '(:1-* symbol)))
    (assert-true (match-sequence (make-instance '2d-array:column-vector :2d-array 2d :column 1)
                                     '(:cat symbol (:0-* number) symbol)))))

  

#+sbcl
(define-test type/2d-array
  
  (let ((2d (make-array '(5 4)
                        :initial-contents '((b0 b1 b2 b3)
                                            (b0 1  2  b3)
                                            (b0 2  3  b3)
                                            (b0 4  5  b3)
                                            (b0 b1 b2 b3)))))
    (assert-true (typep (make-instance '2d-array:row-vector :2d-array 2d :row 0)
                        '(rte (:1-* symbol))))
    (assert-true (typep (make-instance '2d-array:row-vector :2d-array 2d :row 1)
                       '(rte (:1 symbol (:1-* fixnum) symbol))))
    (assert-true (typep (make-instance '2d-array:column-vector :2d-array 2d :column 0)
                        '(rte (:1-* (eql b0)))))

    (assert-true (typep (make-instance '2d-array:column-vector :2d-array 2d :column 1)
                        '(rte (:1 (eql b1) (:1-* fixnum) (eql b1)))))
    
    (assert-true (typep (make-instance '2d-array:vector-of-rows :2d-array 2d)
                        '(rte (:1 (rte (:1-* symbol))
                                   (:1-* (rte (:1 symbol (:1-* fixnum) symbol)))
                                   (rte (:1-* symbol))))))

    (assert-true (typep (make-instance '2d-array:vector-of-columns :2d-array 2d)
                        '(rte (:1 (rte (:1-* symbol))
                                   (:1-* (rte (:1 symbol (:1-* fixnum) symbol)))
                                   (rte (:1-* symbol))))))

    (assert-false (typep (make-instance '2d-array:vector-of-columns :2d-array 2d)
                        '(rte (:1 (rte (:1-* symbol))
                                   (:1-* (rte (:1 symbol (:1-* fixnum) symbol)))
                                   (rte (:1-* symbol))
                                   t))))
    ))


(defrte rte-test-1 (:0-* number number))
(defrte rte-test-2 (:1 (RTE (:0-* NUMBER NUMBER))))
(defrte rte-test-3 (:1-* (rte (:0-* number number))))

(defun type/declaration2 ()
  (typep nil '(rte
               (:0-* number number)))
  (typep nil '(rte
               (:1 (rte
                    (:0-* number number)))))
  
  
  (typep nil '(rte
               (:1-* (rte
                      (:0-* number number)))))
  (funcall (lambda (x)
             (declare (type (rte
                             (:1-* (rte
                                    (:0-* number number))))
                            x))
             x)
           '((1 1) (2 3) (5 6.0)))
  )

(define-test type/declaration2b
  (assert-true (type/declaration2)))

(defrte rte-test-4 (:1-* (RTE (:CAT NUMBER NUMBER))))

(defrte 3-number (:cat number number number))
(define-test type/rte-ref
  (assert-true (typep '(1 2 3) '(rte (:or (:rte 3-number)
                                      (:cat string (:rte 3-number) string)))))
  (assert-true (typep '("x" 1 2 3 "y") '(rte (:or (:rte 3-number)
                                              (:cat string (:rte 3-number) string))))))


(define-test type/declaration
  (assert-true (equal '(1 2 3)
                      (funcall (lambda (x)
                                 (declare (type (rte (:0-* t))
                                                x))
                                 x) '(1 2 3))))
  (assert-error error (funcall (lambda (x)
                                  (declare (type (rte (:0-* symbol))
                                                 x))
                                  x) '(1 2 3)))
  
  (assert-error error
                (funcall (lambda (x)
                           (declare (type (rte
                                           (:1-* (rte (:0-* symbol))))
                                          x))
                           x)
                         '((1 1) (2 nil) (5 6.0))))
  (assert-true (funcall (lambda (x)
                          (declare (type (rte (:1-* (rte
                                                                       (:cat number number))))
                                         x))
                          x)
                        '((1 1) (2 3) (5 6.0))))
  (assert-error error
                (funcall (lambda (x)
                           (declare (type (rte
                                           (:1-* (rte
                                                  (:cat number number)))) x))
                           x)
                         '((1 1) (2 nil) (5 6.0)))

                )
  )

(define-test type/remove-redundant-types-2
    ;; canot remove any redundant if one of the types is not a valid lisp type
    (assert-true (equal (remove-redundant-types '(t (:cat t t)) :and)
                        '(t (:cat t t))))
    (assert-true (equal (remove-redundant-types '((:cat t t) t) :and)
                        '((:cat t t) t)))
    (assert-true (equal (remove-redundant-types '(t (:cat t t)) :or)
                        '(t (:cat t t))))
    (assert-true (equal (remove-redundant-types '((:cat t t) t) :or)
                        '((:cat t t) t))))

(define-test type/remove-redundant-types
  (assert-true (equal '((integer 0 1))
                      (remove-redundant-types '(integer (integer 0 1) (integer 0 3) number)
                                                   :and)))
  (assert-true (equal '(number)
                      (remove-redundant-types '(integer (integer 0 1) (integer 0 3) number)
                                                   :or)))
  (assert-true (equal '(integer float)
                      (remove-redundant-types '(integer (integer 0) (integer 0 5) float (float 0.0) (float 0.0 100.0))
                                                   :or))))

(define-test type/alphabetize
  (assert-true (equal '(float number string) (alphabetize '(number float string))))
  (assert-true (equal (alphabetize '(number (number 10 12) (number 2 5) (number 6) float))
                      '(float number (number 2 5) (number 6) (number 10 12)))))

(defclass test-X () ())
(defclass test-A (test-X) ())
(defclass test-B (test-X) ())
(defclass test-C (test-X) ())
(defclass test-D (test-X) ())
(defclass test-E (test-X) ())
(defclass test-F (test-X) ())
(defclass test-G (test-X) ())

(define-test type/canonicalize-pattern

    (assert-true (equal '(:cat symbol number) (canonicalize-pattern '(:cat (type symbol) (type number)))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:cat :empty-word :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:cat :empty-word :empty-set))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:cat :empty-set :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:cat :empty-set float))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:cat))))
    (assert-true (equal 'float      (canonicalize-pattern '(:cat :empty-word float))))
    (assert-true (equal '(:cat float string) (canonicalize-pattern '(:cat float :empty-word string :empty-word))))

    (assert-true (equal :empty-word (canonicalize-pattern '(:and :empty-word :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-word :empty-set))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-set :empty-word))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-set float))))
    (assert-true (equal '(:* t)   (canonicalize-pattern '(:and))))
    (assert-true (equal 'float  (canonicalize-pattern '(:and (:* t) float))))
    ;; (assert-true (equal :empty-set  (canonicalize-pattern '(:and :empty-word float))))
    (assert-true (equal 'float      (canonicalize-pattern '(:and float float))))

    (assert-true (equal :empty-word (canonicalize-pattern '(:or :empty-word :empty-word))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:or :empty-word :empty-set))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:or :empty-set :empty-word))))
    (assert-true (equal 'float      (canonicalize-pattern '(:or :empty-set float))))
    (assert-true (equal :empty-set  (canonicalize-pattern '(:or))))
    (assert-true (equal 'float      (canonicalize-pattern '(:or float float))))

    (assert-true (equal '(:or float string)
                        (canonicalize-pattern '(:or string float))))
    
    (assert-true (equal '(:cat (:* float) (:or float string))
                        (canonicalize-pattern '(:cat (:0-* float) (:or string float)))))

    (assert-true (equal '(:OR (:AND test-A test-D test-E test-F)
                              (:AND test-A test-D test-E test-G)
                              (:AND test-B test-D test-E test-F)
                              (:AND test-B test-D test-E test-G)
                              (:AND test-C test-D test-E test-F)
                              (:AND test-C test-D test-E test-G))
                        (canonicalize-pattern '(:and (:or test-A test-B test-C) test-D test-E
                                                (:or test-F test-G)))))

    ;; redundant types

    (assert-true (equal 'number (canonicalize-pattern '(:or float number))))
    (assert-true (equal 'float  (canonicalize-pattern '(:and float number))))
    (assert-true (equal :empty-set (canonicalize-pattern '(:and float string))))


    ;; test to fix error
    (assert-true (equal :empty-word (canonicalize-pattern '(:AND (:0-* T) (:CAT (:CAT) (:CAT))))))
    (assert-true (equal ':empty-set  (canonicalize-pattern '(:AND t (:CAT (:CAT) (:CAT))))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:CAT (:CAT) (:CAT)))))
    (assert-true (equal :empty-word (canonicalize-pattern '(:AND (:0-* T) :empty-word))))
    )

(define-test type/traverse-pattern
  (assert-true (equal :empty-word (traverse-pattern :empty-word :client #'identity)))
  (assert-true (equal :empty-set (traverse-pattern :empty-set :client #'identity)))
  (assert-true (equal 'float (traverse-pattern 'float :client #'identity)))

  (assert-true (equal '(:or float number) (traverse-pattern '(:or float number) :client #'identity)))
  (assert-true (equal '(:and float number) (traverse-pattern '(:and float number) :client #'identity)))
  (assert-true (equal '(:cat float number) (traverse-pattern '(:cat float number) :client #'identity)))

  (assert-true (equal '(:* float number) (traverse-pattern '(:* float number) :client #'identity)))

  (assert-true (equal '(:* float number) (traverse-pattern '(:* float number) :f-type #'identity))))

(define-test type/first-types
  (assert-false (set-exclusive-or '(float)
                                  (first-types '(:cat float
                                                      integer
                                                      (:0-* symbol (:0-* number) symbol)
                                                      integer
                                                      (number 0 4)
                                                      (rte (:0-1 symbol))))
                                  :test #'equal))
  (assert-false (set-exclusive-or '((integer 0 1) (integer 2 9))
                                  (first-types '(:cat (:0-* (integer 0 1))
                                                      (integer 2 9)))
                                  :test #'equal))
  )

(define-test type/equivalent-types-p
  (assert-true (equivalent-types-p '(member 1 2 3) '(or (eql 1) (eql 2) (eql 3)))))

(define-test type/nullable
  (assert-false (nullable :empty-set))
  (assert-true  (nullable :empty-word))
  (assert-false (nullable 'float))
  (assert-false (nullable '(integer 1 3)))
  (assert-true  (nullable '(:cat (:0-1 number) (:0-1 string) (:0-* float) :empty-word)))
  (assert-true  (nullable '(:0-* number)))
  (assert-false (nullable '(:1-* number)))
  (assert-true  (nullable '(:0-1 number)))
  (assert-true  (nullable '(:and (:0-1 number)
                                 (:0-* number))))
  (assert-false (nullable '(:and (:0-1 number)
                                 (:1-* number))))
  (assert-true  (nullable '(:or (:0-1 number)
                                 (:1-* number))))
  (assert-true  (nullable '(:or (:0-1 string)
                                 (:0-* number))))
  (assert-false (nullable '(:or string
                                 (:1-* number)))))

(define-test type/derivative

  ;; trivial cases
  (assert-true (derivative 'float 'float))
  (assert-true (equal :empty-word (derivative 'float 'float)))
  (assert-true (equal :empty-set  (derivative :empty-word 'float)))
  (assert-true (equal :empty-set  (derivative 'float 'string)))
  (assert-true (equal :empty-set  (derivative :empty-set 'float)))

  ;; or/and

  (assert-true (equal :empty-word (derivative '(:or float float) 'float)))
  (assert-true (equal :empty-word (derivative '(:and float float) 'float)))
  (assert-true (equal :empty-word (derivative '(:or  float string) 'float)))
  (assert-true (equal :empty-word (derivative '(:or  float string) 'string)))


  (assert-true (equal :empty-word
                      (derivative '(:cat float) 'float)))
  (assert-true (equal 'string
                      (derivative '(:cat float string) 'float)))
  (assert-true (equal '(:* string)
                      (derivative '(:cat float (:* string)) 'float)))
  (assert-true (equivalent-patterns '(:* (:or float string))
                                         (derivative '(:cat float (:* (:or string float))) 'float)))
    
  (assert-true (equivalent-patterns '(:* float)
                                         (derivative '(:* float) 'float)))

  (assert-true (equivalent-patterns '(:* float)
                                         (derivative '(:+ float) 'float)))

  (assert-true (equivalent-patterns '(:or (:cat (:* float) (:or float string))  :empty-word)
                                         (derivative '(:cat (:* float) (:or string float)) 'float)))
  (assert-true (equivalent-patterns '(:or (:cat (:* float) (:* (:or float string)))
                                           (:* (:or float string)))
                                         (derivative '(:cat (:* float) (:* (:or string float))) 'float)))
  )

(define-test type/derivative-2
  (assert-true (equivalent-patterns (derivative '(:OR
                                                  (:CAT (:OR (:CAT (EQL :X) T)
                                                         :EMPTY-WORD)
                                                   (:OR (:CAT (EQL :Y) T)
                                                    :EMPTY-WORD))
                                                  (:CAT (:OR (:CAT (EQL :Y) T)
                                                         :EMPTY-WORD)
                                                   (:OR (:CAT (EQL :X) T)
                                                    :EMPTY-WORD)))
                                                '(eql :y))
                                    `(:or ,(derivative '(:CAT (:OR (:CAT (EQL :X) T)
                                                               :EMPTY-WORD)
                                                         (:OR (:CAT (EQL :Y) T)
                                                          :EMPTY-WORD))
                                                       '(eql :y))
                                          ,(derivative '(:CAT (:OR (:CAT (EQL :Y) T)
                                                               :EMPTY-WORD)
                                                         (:OR (:CAT (EQL :X) T)
                                                          :EMPTY-WORD))
                                                       '(eql :y))))))


(define-test type/derivative-3
  ;; D(R.S) = D(R).S + v(R).D(S)
  ;; if R is nullable
  ;;   =  D(R).S + D(S)
  ;; else
  ;;   =  D(R).S
  (let ((R '(:OR (:CAT (EQL :X) T)
             :EMPTY-WORD))
        (S '(:OR (:CAT (EQL :Y) T)
             :EMPTY-WORD)))
    (assert-true (nullable R))
    ;; RS/y
    (assert-true (equal (canonicalize-pattern (derivative `(:cat ,R ,S)
                                                                     '(eql :y)))
                        (canonicalize-pattern `(:or (:cat ,(derivative R '(eql :y)) ,S)
                                                         ,(derivative S '(eql :y))))))
    (assert-true (nullable R))
    (assert-true (nullable S))
    (assert-true (nullable `(:cat ,R ,S)))
    (assert-true (nullable `(:cat ,S ,R)))
    ;; RS/x
    (assert-true (equal (canonicalize-pattern (derivative `(:cat ,R ,S) '(eql :x)))
                        (canonicalize-pattern '(:cat t (:or :empty-word
                                                             (:cat (eql :y) t))))))
    ;; RS/y
    (assert-true (equal (canonicalize-pattern (derivative `(:cat ,R ,S)
                                                                    '(eql :y)))
                        t))
    ;; SR/x
    (assert-true (equal (canonicalize-pattern (derivative `(:cat ,S ,R) '(eql :x)))
                        t))
    
    ;; SR/y
    (assert-true (equal (canonicalize-pattern (derivative `(:cat ,S ,R) '(eql :y)))
                        (canonicalize-pattern '(:cat t (:or :empty-word
                                                             (:cat (eql :x) t))))))

    (assert-false (equal t
                         (canonicalize-pattern (derivative `(:or (:cat ,R ,S)
                                                                           (:cat ,S ,R))
                                                                     '(eql :x)))))

    (assert-false (equal t
                         (canonicalize-pattern (derivative `(:or (:cat ,R ,S)
                                                                           (:cat ,S ,R))
                                                                     '(eql :y)))))
    
    ))
                      


(define-test type/rte-type
  (assert-true (typep '(1 x) '(rte (:cat number symbol))))
  (assert-true (typep '(2 x) '(rte (:cat (:or symbol number)
                                        symbol))))
  (assert-true (typep '()      '(rte (:0-or-more number))))
  (assert-true (typep '(1 2 3) '(rte (:0-or-more number))))

  (assert-true (typep #(1 2 3) '(rte (:1 number number number))))
  (assert-true (typep #(1 2 3 5) '(rte (:0-* integer))))

  )

(define-test type/test1
  (dolist (pattern '((rte (:or (:cat (:1-* (eql 0)) (:0-* (eql 1)))
                                  (:cat (:0-* (eql 0)) (:1-* (eql 1)))))
                     (rte (:and (:1-* (:or (eql 0) (eql 1)))
                                  (:cat (:0-* (eql 0)) (:0-* (eql 1)))))
                     (rte (:or (:cat (:1-* (eql 0)) (:0-* (eql 1)))
                                  (:1-* (eql 1))))))
    (dolist (yes '(#*0
                   #*1
                   #*01
                   #*001
                   #*0001
                   #*0111
                   #*0011))
      (assert-true (typep yes pattern)))
    (dolist (no '(#()
                  #*10
                  #*010
                  #*0110
                  #(0 1 2)))
      (assert-false (typep no pattern)))))

(define-test type/test2
  (let ((pattern '(rte (:and (:cat integer number)
                                    (:cat number integer)))))
    (dolist (yes '((1 1)
                   (1 2)))
      (assert-true (typep yes pattern)))
    (dolist (no '((1.0 2.0)
                  (3 4.0)
                  (5.0 6)
                  (1 2 3)))
      (assert-false (typep no pattern)))))

                                        
(define-test type/rte-2
  (assert-true (typep '(1 1)
                      '(rte (:+ number))))
  (assert-false (typep '(2 2 x 2 2)
                       '(rte (:* number))))
  (assert-false (typep '((1 1)
                         (2 2 x 2 2))
                       '(rte (:* (rte (:* number))))))
  (assert-false (typep '((1 1)
                         (2 2 x 2 2))
                       '(rte (:* (rte (:+ number)))))))
(define-test type/rte
  (assert-true (typep '(1 x) '(rte (:1 number symbol))))
  
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:1-or-more (eql 1)) (:0-or-1 (eql 0))))))
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:0-or-more (eql 1)) (:0-or-1 (eql 0))))))
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:0-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-true (typep '(0)       '(rte (:1 (:0-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-true (typep '()        '(rte (:1 (:0-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-false (typep '()       '(rte (:1 (:0-or-more (eql 1)) (:1-or-more (eql 0))))))
  (assert-true (typep '(1 1 1)   '(rte (:1-or-more (eql 1)))))
  (assert-true (typep '(1 1 1)   '(rte (:0-or-more (eql 1)))))
  (assert-true (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 1)) (:0-or-more (eql 1))))))
  (assert-false (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 2)) (:0-or-more (eql 3))))))
  (assert-true (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 2)) (:0-or-more (eql 1))))))
  (assert-true (typep '(1 1 1)   '(rte (:or (:1-or-more (eql 1)) (:0-or-more (eql 2))))))
  (assert-true (typep '(1 1 1)   '(rte (:and (:1-or-more (eql 1)) (:0-or-more (eql 1))))))
  (assert-true (typep '(1 1 1)   '(rte (:and (:0-or-more (eql 1)) (:1-or-more (eql 1))))))
  (assert-true (typep '(1 1 1 0) '(rte (:1 (:and (:1-or-more (eql 1)) (:0-or-more (eql 1)))
                                        (eql 0)))))
  (assert-true (typep '(1 1 1)   '(rte (:1 (:1-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-false (typep '(1 1 1)   '(rte (:and (:1-or-more (eql 1)) (:0-or-more (eql 0))))))
  (assert-false (typep '(1 1 1)  '(rte (:and (:0-or-more (eql 1)) (:1-or-more (eql 0))))))
  (assert-true (typep '(1 1 1)   '(rte (:and (:0-or-more (eql 1)) (:1-or-more (:or (eql 1) (eql 0)))))))
  (assert-true (typep '(x 100 y 200 z 300 "hello" "world")
                      '(rte (:1 (:0-or-more symbol number) (:1-or-more string)))))

  ;; recursive use of rte
  (assert-false (typep '((1 1)
                         (2 2 x 2 2))
                       '(rte (:0-or-more (rte (:1-or-more number))))))
  (assert-true (typep '((1 1)
                        (2 2 x 2 2))
                      '(rte (:0-or-more (rte (:1-or-more (:or symbol number)))))))
  (assert-true (typep '((1 1) (2 2 2 2)) '(rte (:0-or-more (rte (:1-or-more number))))))
  

  ;; zero-or-more
  (assert-true (typep '() '(rte (:0-* number))))
  (assert-true (typep '(nil) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil nil) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1) '(rte (:1 (:0-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1 2 3 4) '(rte (:1 (:0-* null) (:0-* number)))))
  
  ;; one-or-more
  (assert-false (typep '() '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil nil) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-true (typep '(nil nil 1 2 3 4) '(rte (:1 (:1-* null) (:0-* number)))))
  (assert-false (typep '(nil nil "1" 2 3 4) '(rte (:1 (:1-* null) (:0-* number)))))

  ;; zero-or-one
  (assert-true (typep nil '(rte (:1 (:0-1 null) (:0-* t)))))
  (assert-true (typep nil '(rte (:1 (:0-1 null) (:0-* t)))))
  (assert-true (typep '(1) '(rte (:1 (:0-1 null) (:0-* number)))))
  (assert-true (typep '(nil 1) '(rte (:1 (:0-1 null) (:0-* number)))))
  (assert-false (typep '(nil nil 1) '(rte (:1 (:0-1 null) (:0-* number)))))

  (assert-true (typep '(1 "hello")                 '(rte (:1 number (:1-* string)))))
  (assert-true (typep '(1 2 "hello" "world")       '(rte (:1 number number (:1-* string)))))
  (assert-true (typep '(1 "hello" "there" "world") '(rte (:1 (:1-* number) (:1-* string)))))
  (assert-true (typep '(1 hello "there" "world")   '(rte (:1 number symbol (:1-* string)))))
  (assert-true (typep '(1 hello)                   '(rte (:1 number symbol (:0-* string)))))
  (assert-false (typep '(1 hello)                  '(rte (:1 number (:0-* string)))))

  ;; pattern-prefix
  (assert-true (typep '(x 100 y 200 z 300)       '(rte (:1-* symbol number))))
  (assert-true (typep '(x 100 y 200 z 300 400)   '(rte (:1 (:1-* symbol number) (:0-1 t)))))
  (assert-true (typep '(x 100 y 200 z 300 400)   '(rte (:1 (:1-* symbol number) (:0-* t)))))
  (assert-false (typep  '(x 100 y 200 z 300 400) '(rte (:1 (:1-* symbol number) null))))

  (assert-true (typep '(x 100 y 200 z 300)
                      '(rte (:1 (:or (:0-* string) (:0-* symbol integer))))))
  (assert-true (typep nil '(rte (:0-* string))))
  (assert-true (typep '("hello" nil 1 2 3  
                        "there" nil 
                        "world" nil 12.0 "foo"
                        "world" nil 12.0 "bar"
                        )
                      '(rte (:0-* (:cat string null (:or (:1-* integer) (:0-* number string)))))))

  (assert-false (typep '("hello" nil 1 2 3  
                        "there" nil  12.0
                        "world" nil 13.0 "foo"
                        "world" nil 14.0 "bar"
                        )
                      '(rte (:0-* (:cat string null (:or (:1-* integer) (:0-* number string)))))))
  
  (let ((pattern     '(rte (:1 float integer (:0-* symbol (:0-* number) symbol) integer))))

    (assert-true (typep '(            1.0   3      x 1 2 3 y 12)                pattern))
    (assert-true (typep '(            1.0   3 4)                                pattern))
    (assert-true (typep '(            1.0   3   x y           12) pattern))
    (assert-false (typep '(           1.0   3   x y z         12) pattern))
    (assert-true (typep '(            1.0   3   x 12 y x 12 13 y x 11.0 12.0 13 y 14) pattern))
    )


)


(define-test type/find-keyword
  (assert-true (eq :x (find-keyword 'x nil)))
  (assert-true (eq :x (find-keyword '(x) nil)))
  (assert-true (eq :x (find-keyword '((x y)) nil)))
  (assert-true (eq :x (find-keyword '((:x y)) nil))))

(define-test test/alphabetize
  (assert-true (equal (ALPHABETIZE
                       '((:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD))
                         (:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))))
                      '((:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))
                        (:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD)))))

  (assert-true (equal (ALPHABETIZE
                       '((:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))
                         (:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD))))
                      '((:CAT (:OR (:CAT (EQL :X) T) :EMPTY-WORD) (:OR (:CAT (EQL :Y) T) :EMPTY-WORD))
                        (:CAT (:OR (:CAT (EQL :Y) T) :EMPTY-WORD) (:OR (:CAT (EQL :X) T) :EMPTY-WORD))))))

(define-test test/destructuring-lambda-list-to-rte
  (assert-true (equivalent-patterns '(:cat t t t)
                                         (destructuring-lambda-list-to-rte '(a b c))))
  (assert-true (equivalent-patterns '(:cat t t t)
                                         (destructuring-lambda-list-to-rte '(&whole w a b c))))
  (assert-true (equivalent-patterns '(:cat t t t)
                                    (destructuring-lambda-list-to-rte '(a b c &aux x y)))
               )
  
  (assert-true (equivalent-patterns '(:CAT (:CAT T T T)
                                      (:AND (:* (MEMBER :X) T)
                                       (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))))
                                    (destructuring-lambda-list-to-rte '(a b c &key x))))
  
  (assert-true (equivalent-patterns '(:CAT (:CAT T T T)
                                      (:AND (:* (MEMBER :Y :X) T)
                                       (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))
                                       (:CAT (:* (NOT (EQL :Y)) T) (:? (EQL :Y) T (:* T)))))
                                    (destructuring-lambda-list-to-rte '(a b c &key x y))))
  
  ;; assert an error because &optional cannot follow &key
  (assert-error error (destructuring-lambda-list-to-rte '(a b c &key x y &optional r)))
  
  (assert-true (equivalent-patterns '(:CAT (:CAT T T T)
                                      (:? T
                                       (:AND (:* (MEMBER :Y :X) T)
                                        (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))
                                        (:CAT (:* (NOT (EQL :Y)) T) (:? (EQL :Y) T (:* T))))))
                                    (destructuring-lambda-list-to-rte '(a b c &optional r &key x y))))

  (assert-true (equivalent-patterns '(:CAT (:CAT (:AND LIST (RTE (:CAT T T))) T)
                                      (:? T
                                       (:AND (:* (MEMBER :Y :X) T)
                                        (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))
                                        (:CAT (:* (NOT (EQL :Y)) T) (:? (EQL :Y) T (:* T))))))
                                    (destructuring-lambda-list-to-rte '((a b) c &optional r &key x y))))

  ;; test &rest
  (assert-true (equivalent-patterns '(:CAT (:CAT (:AND LIST (RTE (:CAT T T))) T)
                                            (:? T
                                                (:AND (:AND LIST (RTE (:CAT T T T T)))
                                                      (:AND (:* (MEMBER :Y :X) T)
                                                            (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))
                                                            (:CAT (:* (NOT (EQL :Y)) T) (:? (EQL :Y) T (:* T)))))))
                                      (destructuring-lambda-list-to-rte '((a b) c &optional q &rest (d e f g) &key x y))))

  ;; &aux
  (assert-true (equivalent-patterns '(:CAT (:CAT (:AND LIST (RTE (:CAT T T))) T)
                                      (:? T
                                       (:AND (:AND LIST (RTE (:CAT T T T T)))
                                        (:AND (:* (MEMBER :Y :X) T)
                                         (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))
                                         (:CAT (:* (NOT (EQL :Y)) T) (:? (EQL :Y) T (:* T)))))))
                                    (destructuring-lambda-list-to-rte '((a b) c &optional q &rest (d e f g) &key x y &aux u v))
                                    )
               )
  ;; add &allow-other-keys
  )




(define-test test/equivalent-patterns
  (assert-true (equivalent-patterns '(:or string number)
                                    '(:or string number float)))
  (assert-false (equivalent-patterns '(:or string number)
                                     '(:or string symbol)))

  (assert-true (equivalent-patterns '(:AND (:* T)
                                      (:CAT (:CAT (:AND LIST (RTE (:CAT T T))) T) (:CAT (:|0-1| T))
                                       (:AND (:AND LIST (RTE (:CAT T T T T)))
                                        (:AND (:* (MEMBER :Y :X) T)
                                         (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))
                                         (:CAT (:* (NOT (EQL :Y)) T) (:? (EQL :Y) T (:* T)))))))

                                    '(:CAT (:CAT (:AND LIST (RTE (:CAT T T))) T) (:CAT (:|0-1| T))
                                      (:AND (:AND LIST (RTE (:CAT T T T T)))
                                       (:AND (:* (MEMBER :Y :X) T)
                                        (:CAT (:* (NOT (EQL :X)) T) (:? (EQL :X) T (:* T)))
                                        (:CAT (:* (NOT (EQL :Y)) T) (:? (EQL :Y) T (:* T))))))))

  (assert-true (equivalent-patterns '(:* t)
                                    '(:or number
                                      (:not number))))

  (assert-true (equivalent-patterns ':empty-set
                                    '(:and number
                                      (:not number))))

  (dolist (pattern '(number
                     (:cat number number)
                     (:cat (:* number) string)
                     (:cat (:or number string) (:+ fixnum))
                     (:not (:+ number))
                     (:cat number (:not (:+ number)))
                     (:cat (:and (:+ float) (:* number)) string)))
    (assert-true (equivalent-patterns :empty-set
                                      `(:and ,pattern (:not ,pattern))))
    (assert-true (equivalent-patterns :empty-set
                                      `(:and (:not ,pattern) ,pattern)))
    (assert-true (equivalent-patterns `(:and ,pattern (:not ,pattern))
                                      :empty-set))
    (assert-true (equivalent-patterns `(:and (:not ,pattern) ,pattern)
                                      :empty-set))
    
    (assert-true (equivalent-patterns '(:* t)
                                      `(:or ,pattern (:not ,pattern))))
    (assert-true (equivalent-patterns '(:* t)
                                      `(:or (:not ,pattern) ,pattern)))
    (assert-true (equivalent-patterns `(:or ,pattern (:not ,pattern))
                                      '(:* t)))
    (assert-true (equivalent-patterns `(:or (:not ,pattern) ,pattern)
                                      '(:* t)))
    
    )

  
  )

(define-test test/synchronized-product
  (assert-true (synchronized-product (rte-to-dfa :empty-set)
                                     (rte-to-dfa :empty-set))))
