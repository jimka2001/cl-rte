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


(garbage-collect)

(define-test test/destructuring-lambda-list-to-rte-2
  (equivalent-patterns 
   (destructuring-lambda-list-to-rte '(&whole llist
                                       a (b c)
                                       &rest keys
                                       &key x y z
                                       &allow-other-keys)
                                     :type-specifiers (gather-type-declarations '((declare (type fixnum a b c)
                                                                                   (type symbol x)
                                                                                   (type string y)
                                                                                   (type list z)))))

   '(:1 (:1 fixnum (:and list (rte (:1 fixnum fixnum))))
     (:and
      (:* keyword t)
      (:or
       (:1 (:? (eql :x) symbol (:* (not (member :y :z)) t))
        (:? (eql :y) string (:* (not (eql    :z))    t))
        (:? (eql :z) list   (:* t t)))
       (:1 (:? (eql :y) string (:* (not (member :x :z)) t))
        (:? (eql :x) symbol (:* (not (eql    :z))    t))
        (:? (eql :z) list   (:* t t)))
       (:1 (:? (eql :x) symbol (:* (not (member :y :z)) t))
        (:? (eql :z) list   (:* (not (eql    :y))    t))
        (:? (eql :y) string (:* t t)))
       (:1 (:? (eql :z) list   (:* (not (member :x :y)) t))
        (:? (eql :x) symbol (:* (not (eql    :y))    t))
        (:? (eql :y) string (:* t t)))
       (:1 (:? (eql :y) string (:* (not (member :x :z)) t))
        (:? (eql :z) list   (:* (not (eql    :x))    t))
        (:? (eql :x) symbol (:* t t)))
       (:1 (:? (eql :z) list   (:* (not (member :x :y)) t))
        (:? (eql :y) string (:* (not (eql    :x))    t))
        (:? (eql :x) symbol (:* t t))))))))

(define-test test/destructuring-lambda-list-to-rte-3
  (let ((rte-float (destructuring-lambda-list-to-rte
               '(arg1)
               :type-specifiers (gather-type-declarations '((declare (type float arg1))))))
        (rte-number (destructuring-lambda-list-to-rte
               '(arg2)
               :type-specifiers (gather-type-declarations '((declare (type number arg2)))))))
    (assert-true (equivalent-patterns `(:and ,rte-float (:not ,rte-number)) :empty-set))
    (assert-true (equivalent-patterns `(:and (:not ,rte-number) ,rte-float) :empty-set))
    (assert-false (equivalent-patterns `(:and ,rte-number (:not ,rte-float)) :empty-set))
    (assert-false (equivalent-patterns `(:and (:not ,rte-float) ,rte-number) :empty-set))))


(define-test test/destructuring-case-alt-5
  (destructuring-bind (u v &key x ((:y (y1 y2)) '(nil nil))) '(1 2 :x 3 :y (4 5))
    (assert-true (= u 1))
    (assert-true (= v 2))
    (assert-true (= x 3))
    (assert-true (= y1 4))
    (assert-true (= y2 5)))
  (let ((a '(1 2 :x 3 :y (4 5)))
        (n 0))
    (destructuring-case-alt a
      ((u v &key x ((:y (y1 y2)) '(nil nil))) ()
       (incf n)
       (assert-true (= u 1))
       (assert-true (= v 2))
       (assert-true (= x 3))
       (assert-true (= y1 4))
       (assert-true (= y2 5))))
    (assert-true (= n 1))))

(define-test test/destructuring-case-alt-6
  (LET ((N 0)
        (A '(1 2 :x 3 :y (4 5))))
    (TYPECASE A
      ((NOT LIST) NIL)
      ((RTE
        (:CAT T
              T
              (:OR
               (:CAT (:OR (:CAT (EQL :X) T)
                          :EMPTY-WORD)
                     (:OR (:CAT (EQL :Y)
                                (:AND LIST
                                     (RTE (:CAT T T))))
                          :EMPTY-WORD))
               (:CAT (:OR (:CAT (EQL :Y)
                                (:AND LIST
                                     (RTE (:CAT T T))))
                          :EMPTY-WORD)
                     (:OR (:CAT (EQL :X) T)
                          :EMPTY-WORD)))))
       (DESTRUCTURING-BIND (U V &KEY X ((:Y (Y1 Y2)) '(NIL NIL)))
           A
         (INCF N)
         (ASSERT-TRUE (= U 1))
         (ASSERT-TRUE (= V 2))
         (ASSERT-TRUE (= X 3))
         (ASSERT-TRUE (= Y1 4))
         (ASSERT-TRUE (= Y2 5)))))
    (assert-true (= n 1))))

(define-test test/destructuring-case-alt-7
  (LET ((N 0)
        (A '( :x 3 :y (4 5))))
    (TYPECASE A
      ((NOT LIST) NIL)
      ((RTE
        (:CAT 
         (:OR
          (:CAT (:OR (:CAT (EQL :X) T)
                 :EMPTY-WORD)
                (:OR (:CAT (EQL :Y)
                           (:AND LIST
                                (RTE (:CAT T T))))
                 :EMPTY-WORD))
          (:CAT (:OR (:CAT (EQL :Y)
                           (:AND LIST
                                (RTE (:CAT T T))))
                 :EMPTY-WORD)
                (:OR (:CAT (EQL :X) T)
                 :EMPTY-WORD)))))
       (DESTRUCTURING-BIND ( &KEY X ((:Y (Y1 Y2)) '(NIL NIL)))
           A
         (INCF N)
         (ASSERT-TRUE (= X 3))
         (ASSERT-TRUE (= Y1 4))
         (ASSERT-TRUE (= Y2 5)))))
    (assert-true (= n 1))))


(define-test test/destructuring-case-alt-8b
  (destructuring-lambda-list-to-rte '(&whole llist a (b c) &rest keys &key x y z &allow-other-keys)
                                         :type-specifiers '((a fixnum)
                                                            (b fixnum)
                                                            (c fixnum)
                                                            (x symbol)
                                                            (y string)
                                                            (z list))))

(defmacro assert-equal (a b)
  `(assert-true (equal ,a ,b)))

(define-test test/destructuring-case-alt-8
  ;; with &allow-other-keys
  (assert-equal (canonicalize-pattern (destructuring-lambda-list-to-rte '(&key x y z &allow-other-keys)))
               (canonicalize-pattern
                '(:and (:* t)
                  (:cat (:cat) (:cat)
                   (:and (:* t)
                    (:and (:* keyword t) (:cat (:* (not (eql :x)) t) (:? (eql :x) t (:* t)))
                     (:cat (:* (not (eql :y)) t) (:? (eql :y) t (:* t)))
                     (:cat (:* (not (eql :z)) t) (:? (eql :z) t (:* t)))))))))

  (assert-equal (canonicalize-pattern (destructuring-lambda-list-to-rte '(&key x y z)))
                (canonicalize-pattern
                 '(:and (:* (member :x :y :z) t)
                   (:cat (:* (not (eql :x)) t) (:or :empty-word (:cat (eql :x) t (:* t))))
                   (:cat (:* (not (eql :y)) t) (:or :empty-word (:cat (eql :y) t (:* t))))
                   (:cat (:* (not (eql :z)) t) (:or :empty-word (:cat (eql :z) t (:* t)))))))

    (map-permutations (lambda (perm)
                        (assert-true (equal :here
                                            (destructuring-case-alt (mapcan (lambda (key)
                                                                          (list key 12))
                                                                        perm)
                                              ((&key (x 1) (y 1) (z 1)) ((fixnum x y z))
                                               (assert-true (equal 12 x))
                                               (assert-true (equal 12 y))
                                               (assert-true (equal 12 z))
                                               :here)))))
                      '(:x :y :z))

  (map-permutations (lambda (perm)
                      (assert-true (equal :here
                                          (destructuring-case-alt (mapcan (lambda (key)
                                                                        (list key 12))
                                                                      perm)
                                            ((&key (x 1) (y 1) (z 1) &allow-other-keys) ((fixnum x y z))
                                             (assert-true (equal 12 x))
                                             (assert-true (equal 12 y))
                                             (assert-true (equal 12 z))
                                             :here)))))
                    '(:w :x :y :z)))

(garbage-collect)

(define-test test/destructuring-case-alt-9
  (let ((n 0))
    (destructuring-case-alt '(:x (1 2))
      ((&key ((:x (a b)) '(nil nil))) ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (incf n)))
    (assert-true (equal n 1))))

(garbage-collect)

(define-test test/destructuring-case-alt-10
  (let ((n 0))
    (destructuring-case-alt '(:x (1 2))
      ((&key ((:x (a b c)) '(nil nil nil))) ()
       (declare (ignore a b c))
       nil)
      ((&key ((:x (a b)) '(nil nil))) ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (incf n)))
    (assert-true (equal n 1))))


(garbage-collect)

(define-test test/destructuring-case-alt-11
  (let ((n 0))
    (destructuring-case-alt '(:x (1 2) :y (10 20 30))
      ((&key ((:x (a b c)) '(nil nil nil))
             ((:y (d)) '(nil)))
         ()
       (declare (ignore a b c d))
       nil)
      ((&key ((:x (a b)) '(nil nil))
             ((:y (u v w)) '(nil nil nil)))
         ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (incf n)))
    (assert-true (equal n 1))))

(garbage-collect)

(define-test test/destructuring-case-alt-12
  (let ((n 0))
    (destructuring-case-alt '(:x (1 2) :y (10 20 30))
      ((&key ((:x (a b c)) '(nil nil nil))
             ((:y (d)) '(nil))) ()
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
             ((:x (a b)) '(nil nil))) ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (incf n)))
    (assert-true (equal n 1))))

(garbage-collect)

(define-test test/destructuring-case-alt-13
  (let ((n 0))
    (destructuring-case-alt '(:x (1 2) :y (10 20 30) :z 100)
      ((&key ((:x (a b c)) '(nil nil nil))
             ((:y (d)) '(nil))) ()
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
             ((:x (a b)) '(nil nil))) ()
       (declare (ignore u v w a b))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
             ((:x (a b)) '(nil nil))
             z) ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (assert-true (equal z 100))
       (incf n)))
    
    (assert-true (equal n 1))))

(define-test test/destructuring-case-alt-13b
  (let ((n 0))
    (destructuring-case-alt '(:x (1 2) :y (10 20 30) :y -1 :z 100 :x -2)
      ((&key ((:x (a b c)) '(nil nil nil))
             ((:y (d)) '(nil))) ()
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
             ((:x (a b)) '(nil nil))) ()
       (declare (ignore a b u v w))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
             ((:x (a b)) '(nil nil))
             z) ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (assert-true (equal z 100))
       (incf n)))
    
    (assert-true (equal n 1))))

(garbage-collect)

;; loading this test pegs the CPU, we simply can't load it at startup
;;
(define-test test/destructuring-case-alt-14
  (let ((n 0))
    (destructuring-case-alt '(:x (1 2) :y (10 20 30) :z 100)
      ((&key ((:x (a b c)) '(nil nil nil))
          ((:y (d)) '(nil))
          &allow-other-keys) ()
       (declare (ignore a b c d))
       nil)
      ((&key ((:y (u v w)) '(nil nil nil))
          ((:x (a b)) '(nil nil))
          &allow-other-keys) ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (incf n))
      ((&key ((:y (u v w)) '(nil nil nil))
          ((:x (a b)) '(nil nil))
          z) ()
       (declare (ignore u v w a b z))
       nil))
    
    (assert-true (equal n 1))))

(garbage-collect)

;; commenting out this test because it is toooo sloooow
;;
(define-test test/destructuring-case-alt-14b
  (let ((n :no-case))
    (declare (type keyword n))
    (destructuring-case-alt '(:x (1 2) :y (10 20 30) :z 100 :a 12)
      ((&key ((:x (a b c)) '(nil nil nil))
          ((:y (d)) '(nil))
          &allow-other-keys) ()
       (declare (ignore a b c d))
       (setf n :first-case))
      ((&key ((:y (u v w)) '(nil nil nil))
          ((:x (a b)) '(nil nil))
          &allow-other-keys) ()
       (assert-true (equal a 1))
       (assert-true (equal b 2))
       (assert-true (equal u 10))
       (assert-true (equal v 20))
       (assert-true (equal w 30))
       (setf n :second-case))
      ((&key ((:y (u v w)) '(nil nil nil))
          ((:x (a b)) '(nil nil))
          z
             &allow-other-keys) ()
       (declare (ignore u v w a b z))
       (setf n :third-case)))
    
    (assert-true (eql n :second-case))))

(garbage-collect)

(define-test test/destructuring-case-alt-15
  (let ((n 0))
    (destructuring-case-alt '((1 2) 3)
      ((a b c) ((float a b) (integer c))
       (list a b c))
      ((a b c) ((fixnum a b) (integer c))
       (list a b c))
      (((a b) c) ( ( float a b) ( integer c))
       (list a b c))
      (((a b) c)       ( ( integer a b) ( integer c))
       (incf n)
       (assert-true (= a 1))
       (assert-true (= b 2))
       (assert-true (= c 3))))
    (assert-true (= 1 n))))

(garbage-collect)

(define-test test/destructuring-case-alt-15b
  (let ((n 0))
    (destructuring-case-alt '((1 2) 3)
      ((a b c) ((float a b) (integer c))
       (list a b c))
      ((a b c) ((fixnum a b) (integer c))
       (list a b c))
      (((a b) c) ( ( float a b) ( integer c))
       (list a b c)))
    (assert-true (= 0 n))))

(define-test test/destructuring-case-alt-16
  (assert-true (equal '(1 x 0)
                      (destructuring-case-alt '(x)
                        ((name &key (count 0)) ((fixnum count))
                         (list 1 name count))
                        ((name &key count) ()
                         (list 2 name count)))))

  (assert-true (equal '(2 x y)
                      (destructuring-case-alt '(x :count y)
                        ((name &key (count 0)) ((fixnum count))
                         (list 1 name count))
                        ((name &key (count 'z))  ((symbol count))
                         (list 2 name count)))))

  (assert-true (equal '(1 x 0)
                      (destructuring-case-alt '(x)
                        ((name &key (count 0)) ((fixnum count))
                         (list 1 name count))
                        ((name &key (count 42)) ((fixnum count))
                         (list 2 name count))))))

                    

(define-test test/gather-type-declarations
  (assert-false (set-exclusive-or '((a list))
                                  (gather-type-declarations '("docstring"
                                                              (declare (type list a))
                                                              (cons a)))
                                  :test #'equal))
  (assert-false (set-exclusive-or '((a list))
                                  (gather-type-declarations '((declare (type list a))
                                                              "docstring"
                                                              (cons a)))
                                  :test #'equal))
  (assert-false (set-exclusive-or '((a list)
                                    (b string))
                                  (gather-type-declarations '("docstring"
                                                              (declare (type list a) (type string b))
                                                              (or a b)))
                                  :test #'equal))
  (assert-false (set-exclusive-or '((a list)
                                    (b string))
                                  (gather-type-declarations '("docstring"
                                                              (declare (type list a))
                                                              (declare (type string b))
                                                              (or a b)))
                                  :test #'equal))
  (assert-false (set-exclusive-or '((a list)
                                    (b string))
                                  (gather-type-declarations '((declare (type list a))
                                                              (declare (type string b))
                                                              "docstring"
                                                              (or a b)))
                                  :test #'equal))

  
  (assert-false (set-exclusive-or '((a fixnum)
                                    (b fixnum)
                                    (c string)
                                    (d bignum)
                                    (e float))
                                  (gather-type-declarations '((declare (type fixnum a b))
                                                              (declare (type string c))
                                                              (declare (bignum d)
                                                               (float e))
                                                              (declare (ignore x y z))
                                                              ()))
                                  :test #'equal))

  (assert-true (member (assoc 'a (gather-type-declarations '((declare (type X a))
                                                             (declare (type X b))
                                                             (declare (type Y a)))))
                       '((a (and X Y)) (a (and Y X)))
                       :test #'equal))

  )

(defrte 3-number (:CAT NUMBER NUMBER NUMBER))
(defrte multiple-of-3-number (:CAT (:* NUMBER NUMBER NUMBER)))
(defrte multiple-of-3-fixnum (:CAT (:* FIXNUM FIXNUM FIXNUM)))
(defrte 3-fixnum (:CAT FIXNUM FIXNUM FIXNUM))

(define-test test/destructuring-methods-2
  (assert-true (typep '(1 2 3)
                      '(RTE (:CAT NUMBER NUMBER NUMBER))))
  (assert-true (= 6 (COND
                      ((TYPEP '(1 2 3)
                              '(RTE (:CAT NUMBER NUMBER NUMBER)))
                       (* 2 (COND ((TYPEP '(1 2 3)
                                          '(RTE (:CAT (:* FIXNUM FIXNUM FIXNUM))))
                                   3)
                                  (t 0))))
                      ((TYPEP  '(1 2 3)
                               '(RTE (:CAT (:* FIXNUM FIXNUM FIXNUM)))) 3))))
  (assert-true (= 6
                    (TYPECASE '(1 2 3)
                      ((RTE (:CAT NUMBER NUMBER NUMBER))
                       (* 2 (TYPECASE '(1 2 3)
                              ((RTE (:CAT (:* FIXNUM FIXNUM FIXNUM)))
                               3)
                              (t 0))))
                      ((RTE (:CAT (:* FIXNUM FIXNUM FIXNUM)))
                       3)))))
                                                                 
(define-test test/destructuring-methods
  (assert-true (= 6 (destructuring-methods '(1 2 3) (:call-next-method cnm)
                      ((a b c)
                       (declare (type number a b c)
                                (ignore a b c))
                       (* 2 (or (cnm) 0)))
                      ((a b c)
                       (declare (type fixnum a b c)
                                (ignore a b c))
                       3))))
  (assert-error error (destructuring-methods '(1 2 3) (:call-next-method cnm)
                         ((a b)
                          (declare (ignore a b))
                          1)
                         ((a b c)
                          (declare (ignore a b c))
                          (cnm)))))


(define-test test/destructuring-case-alt-allow-other-keys
  (let ((data '(1 (2 3) :a 12
                :x name :x "not-symbol"
                :b 13 :z nil :z 14 :y "hello" :a 15 :x 16 :y 17 :z 18)))
    (destructuring-bind (&whole llist a (b c) 
                         &rest keys
                         &key (x t) (y "") z &allow-other-keys) data
      (declare (type fixnum a b c)
               (type symbol x)
               (type string y)
               (type list z))
      (assert-true (= a 1))
      (assert-true (= b 2))
      (assert-true (= c 3))
      (list a b c x y z llist keys))

    (assert-false
     (null
      (destructuring-case-alt data
        ((&whole llist a (b c) 
                 &rest keys
                 &key (x t) (y "") z &allow-other-keys)
         ( ( fixnum a b c)
           ( symbol x)
           ( string y)
           ( list z))
         (assert-true (= a 1))
         (assert-true (= b 2))
         (assert-true (= c 3))
         (assert-true (eq x 'name))
         (assert-true (string= "hello" y))
         (assert-true (eq nil z))
         (assert-false (null keys))
         llist))))))

(define-test test/destructuring-lambda-list-to-rte-optional-rest
  ;; this test excercises many combinations of &optional &rest &key &allow-other-keys
  
  ;; &rest
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&rest others))
                                    '(:* t)))
  ;; &key
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&key))
                                    :empty-word))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&key &allow-other-keys))
                                    '(:* keyword t)))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&key a)
                                                                      :type-specifiers '((a integer)))
                                    '(:? (eql :a) integer (:* (eql :a) t))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&key a b)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:and (:* (member :a :b) t) ; don't allow other keys
                                      (:cat (:* (not (eql :a)) t) (:? (eql :a) integer (:* t)))
                                      (:cat (:* (not (eql :b)) t) (:? (eql :b) string  (:* t))))))
  
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&key a b &allow-other-keys)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:and (:* keyword t) ; do allow other keys
                                      (:cat (:* (not (eql :a)) t) (:? (eql :a) integer (:* t)))
                                      (:cat (:* (not (eql :b)) t) (:? (eql :b) string  (:* t))))))
  
  ;; &rest &key
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&rest r &key a b )
                                                                      :type-specifiers '((r cons) (a integer) (b string)))
                                    '(:and (:+ t)
                                      (:* (member :a :b) t) ; do not allow other keys
                                      (:cat (:* (not (eql :a)) t) (:? (eql :a) integer (:* t)))
                                      (:cat (:* (not (eql :b)) t) (:? (eql :b) string  (:* t))))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&rest r &key a b &allow-other-keys)
                                                                      :type-specifiers '((r cons) (a integer) (b string)))
                                    '(:and (:+ t)
                                      (:* keyword t) ; do allow other keys
                                      (:cat (:* (not (eql :a)) t) (:? (eql :a) integer (:* t)))
                                      (:cat (:* (not (eql :b)) t) (:? (eql :b) string  (:* t))))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&rest r &key )
                                                                      :type-specifiers '((r list)))
                                    ':empty-word ; do not allow other keys
                                    ))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&rest r &key &allow-other-keys)
                                                                      :type-specifiers '((r cons))) ;; not yet working
                                    '(:and (:+ t)
                                      (:* keyword t) ; do allow other keys
                                      )))
  
  ;; &optional
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional))
                                    :empty-word))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a))
                                    '(:? t)))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a)
                                                                      :type-specifiers '((a integer)))
                                    '(:? integer)))
  (assert-false (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a b)
                                                                       :type-specifiers '((a integer) (b string)))
                                     '(:or integer
                                       (:cat integer string))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a b)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:? integer (:? string))))
  
  ;; &optional &key
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional &key))
                                    :empty-word))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &key))
                                    '(:? t)))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &key)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:? integer)))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a b &key)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:? integer (:? string))))
  
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional &key a)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:and (:* (eql :a) t)
                                      (:cat (:* (not (eql :a)) t) (:? (eql :a) integer (:* t))))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional &key a &allow-other-keys)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:and (:* keyword t)
                                      (:cat (:* (not (eql :a)) t) (:? (eql :a) integer (:* t))))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &key b)
                                                                      :type-specifiers '((a integer) (b string)))
                                    '(:? integer (:and (:* (eql :b) t)
                                                  (:? (eql :b) string (:* t))))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &key b c &allow-other-keys)
                                                                      :type-specifiers '((a integer) (b string) (c list)))
                                    '(:? integer (:and (:* keyword t)
                                                  (:cat (:* (not (eql :b)) t) (:? (eql :b) string (:* t)))
                                                  (:cat (:* (not (eql :c)) t) (:? (eql :c) list (:* t)))))))
  
  ;; &optional &rest
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional &rest others))
                                    '(:* t)))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &rest others)
                                                                      :type-specifiers '((a integer)))
                                    '(:or :empty-word
                                      (:cat integer (:* t)))))

  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a b &rest others)
                                                                      :type-specifiers '((a integer)
                                                                                         (b string)))
                                    '(:or :empty-word
                                      integer
                                      (:cat integer string (:* t)))))

  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a b c &rest others)
                                                                      :type-specifiers '((a integer)
                                                                                         (b string)
                                                                                         (c list)))
                                    '(:or :empty-word
                                      integer
                                      (:cat integer string)
                                      (:cat integer string list (:* t)))))

  ;; &optional &rest &key
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional &rest r &key))
                                    :empty-word))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional &rest r &key &allow-other-keys))
                                    '(:* keyword t)))
  
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &rest r &key b c)
                                                                      :type-specifiers '((a integer)
                                                                                         (b string)
                                                                                         (c list)))
                                    '(:? integer
                                      (:and (:* (member :b :c) t)
                                       (:cat (:* (not (eql :b)) t) (:? (eql :b) string (:* t)))
                                       (:cat (:* (not (eql :c)) t) (:? (eql :c) list (:* t)))))))

  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &rest r &key b c)
                                                                      :type-specifiers '((a integer)
                                                                                         (b string)
                                                                                         (c list)
                                                                                         (r cons)))
                                    '(:? integer
                                      (:and (:+ t)
                                       (:* (member :b :c) t)
                                       (:cat (:* (not (eql :b)) t) (:? (eql :b) string (:* t)))
                                       (:cat (:* (not (eql :c)) t) (:? (eql :c) list (:* t)))))))
  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &rest r &key b c &allow-other-keys)
                                                                      :type-specifiers '((a integer)
                                                                                         (b string)
                                                                                         (c list)))
                                    '(:? integer
                                      (:and (:* keyword t)
                                       (:cat (:* (not (eql :b)) t) (:? (eql :b) string (:* t)))
                                       (:cat (:* (not (eql :c)) t) (:? (eql :c) list (:* t)))))))

  (assert-true (equivalent-patterns (destructuring-lambda-list-to-rte '(&optional a &rest r &key b c &allow-other-keys)
                                                                      :type-specifiers '((a integer)
                                                                                         (b string)
                                                                                         (c list)
                                                                                         (r cons)))
                                    '(:? integer
                                      (:and (:+ t)
                                       (:* keyword t)
                                       (:cat (:* (not (eql :b)) t) (:? (eql :b) string (:* t)))
                                       (:cat (:* (not (eql :c)) t) (:? (eql :c) list (:* t)))))))

  )

(define-test test/destructuring-case-alt-allow-other-keys-2
  (let ((data '(1 (2 3)
                :x name :y 3.14 :z 14)))
    (assert-error error
                  (destructuring-bind (&whole llist a (b c) 
                                       &rest keys
                                       &key (x t) (y "") z &allow-other-keys) data
                    (declare (type fixnum a b c)
                             (type symbol x)
                             (type string y)
                             (type list z))
                    (list llist a b c keys x y z)))

    (assert-true
     (equal
      'default
      (destructuring-case-alt data
        ((&whole llist a (b c) 
                 &rest keys
                 &key (x t) (y "") z &allow-other-keys)
         (( fixnum a b c)
          ( symbol x)
          ( string y)
          ( list z))
         (list llist a b c keys x y z))
        ((&rest args) ()
         (declare (ignore args))
         'default))))
))
       
(defun test-graph-2keys ()
  (let ((pattern (destructuring-lambda-list-to-rte
                  '(&key (x t) (y "") &allow-other-keys)
                  :type-specifiers
                  (gather-type-declarations
                   '((declare (type symbol x)
                      (type string y)))))))
    (format t "~S~%" pattern)
    (ndfa-to-dot
     (rte-to-dfa pattern)
     #p"/tmp/dfa2.png" :state-legend nil)))

(defun test-graph-3keys ()
  (let ((pattern (destructuring-lambda-list-to-rte
                  '(&whole llist a (b c)
                    &rest keys
                    &key (x t) (y "") (z 12) ;; &allow-other-keys
                    )
                  :type-specifiers
                  (gather-type-declarations
                   '((declare (type fixnum a b c)
                      (type symbol x)
                      (type string y)
                      (type fixnum z)))))))
    (format t "~S~%" pattern)
    (ndfa-to-dot 
     (rte-to-dfa pattern)
     #p"/tmp/dfa3.png"
     :transition-abrevs '((t t1)
                          (list t2)
                          (fixnum t3)
                          (symbol t4)
                          (keyword t5)
                          (string t6)
                          ((and list (rte (:cat fixnum fixnum))) t7)
                          ((eql :x) t8)
                          ((eql :y) t9)
                          ((eql :z) t10)
                          ((eql :x :y) t11)
                          ((eql :x :z) t12)
                          ((eql :y :z) t13)
                          ((eql :x :y :z) t14)
                          ((and keyword (not (eql :x))) t15)
                          ((and keyword (not (eql :y))) t16)
                          ((and keyword (not (eql :z))) t17)
                          ((and keyword (not (member :x :y))) t18)
                          ((and keyword (not (member :x :z))) t19)
                          ((and keyword (not (member :y :z))) t20)
                          ((and keyword (not (member :x :y :z))) t21))
     :transition-legend t
     :state-legend nil)))

(defun test-graph-3keys-b ()
  ;; (&key (x t) (y "") (z 12) &allow-other-keys)
  ;; (declare (type symbol x)
  ;;          (type string y)
  ;;          (type fixnum z))
  (let ((pattern '(:and (:* keyword t)
                   ;; the first :x is followed by a symbol
                   (:cat (:* (not (eql :x)) t) (:? (eql :x) symbol (:* t)))

                   ;; the first :y is followed by a string
                   (:cat (:* (not (eql :y)) t) (:? (eql :y) string (:* t)))

                   ;; the first :z is followed by a fixnum
                   (:cat (:* (not (eql :z)) t) (:? (eql :z) fixnum (:* t))))))
    (format t "~S~%" pattern)
    (ndfa-to-dot 
     (rte-to-dfa pattern)
     #p"/tmp/dfa3.png"
     :transition-abrevs '((t t1)
                          (list t2)
                          (fixnum t3)
                          (symbol t4)
                          (keyword t5)
                          (string t6)
                          ((and list (rte (:cat fixnum fixnum))) t7)
                          ((eql :x) t8)
                          ((eql :y) t9)
                          ((eql :z) t10)
                          ((eql :x :y) t11)
                          ((eql :x :z) t12)
                          ((eql :y :z) t13)
                          ((eql :x :y :z) t14)
                          ((and keyword (not (eql :x))) t15)
                          ((and keyword (not (eql :y))) t16)
                          ((and keyword (not (eql :z))) t17)
                          ((and keyword (not (member :x :y))) t18)
                          ((and keyword (not (member :x :z))) t19)
                          ((and keyword (not (member :y :z))) t20)
                          ((and keyword (not (member :x :y :z))) t21))
     :transition-legend t
     :state-legend nil)))

(defun test-graph-3keys-c ()
  (let ((pattern  '(:and
                    (:and (:* keyword t)
                     (:cat (:* (not (eql :x)) t) (:? (eql :x) symbol (:* t)))
                     (:cat (:* (not (eql :y)) t) (:? (eql :y) string (:* t)))
                     (:cat (:* (not (eql :z)) t) (:? (eql :z) fixnum (:* t))))
                    (:not (:and (:* keyword t)
                           (:cat (:* (not (eql :x)) t) (:? (eql :x) symbol (:* t)))
                           (:cat (:* (not (eql :y)) t) (:? (eql :y) string (:* t)))
                           (:cat (:* (not (eql :z)) t) (:? (eql :z) fixnum (:* t))))))))
    (format t "~S~%" pattern)
    (ndfa::ndfa-to-dot 
     (rte-to-dfa pattern)
     #p"/tmp/dfa4.png"
     :transition-abrevs '((t t1)
                          (list t2)
                          (fixnum t3)
                          (symbol t4)
                          (keyword t5)
                          (string t6)
                          ((and list (rte (:cat fixnum fixnum))) t7)
                          ((eql :x) t8)
                          ((eql :y) t9)
                          ((eql :z) t10)
                          ((eql :x :y) t11)
                          ((eql :x :z) t12)
                          ((eql :y :z) t13)
                          ((eql :x :y :z) t14)
                          ((and keyword (not (eql :x))) t15)
                          ((and keyword (not (eql :y))) t16)
                          ((and keyword (not (eql :z))) t17)
                          ((and keyword (not (member :x :y))) t18)
                          ((and keyword (not (member :x :z))) t19)
                          ((and keyword (not (member :y :z))) t20)
                          ((and keyword (not (member :x :y :z))) t21))
     :transition-legend t
     :state-legend nil)))

(defun test-graph-3keys-d (pattern &key (file #p"/tmp/dfa4.png") (trim t))
  (format t "~S~%" pattern)
  (ndfa-to-dot 
   (if trim
       (trim-state-machine (rte-to-dfa pattern))
       (rte-to-dfa pattern))
   file
   :transition-legend t
   :state-legend t))

;; ================


(define-test test/destructuring-case-15
  (let ((n 0))
    (destructuring-case '((1 2) 3)
      ((a b c)
       (declare (type float a b)
                (type integer c))
       (list a b c))
      ((a b c)
       (declare (type fixnum a b)
                (type integer c))
       (list a b c))
      (((a b) c)
       (declare (type float a b) (type integer c))
       (list a b c))
      (((a b) c)
       (declare (type integer a b) (type integer c))
       (incf n)
       (assert-true (= a 1))
       (assert-true (= b 2))
       (assert-true (= c 3))))
    (assert-true (= 1 n))))

(garbage-collect)

(define-test test/destructuring-case-15-931
  (let ((n 0))
    (DESTRUCTURING-CASE-ALT '((1 2) 3)
      ((A B C) ((INTEGER C) (FLOAT B) (FLOAT A))
       (DECLARE (TYPE FLOAT A B)
                (TYPE INTEGER C))
       (LIST A B C))
      ((A B C) ((INTEGER C) (FIXNUM B) (FIXNUM A))
       (DECLARE (TYPE FIXNUM A B)
                (TYPE INTEGER C))
       (LIST A B C))
      (((A B) C) ((INTEGER C) (FLOAT B) (FLOAT A))
       (DECLARE (TYPE FLOAT A B)
                (TYPE INTEGER C))
       (LIST A B C))
      (((A B) C) ((INTEGER C) (INTEGER B) (INTEGER A))
       (DECLARE (TYPE INTEGER A B)
                (TYPE INTEGER C))
       (INCF N) (ASSERT-TRUE (= A 1)) (ASSERT-TRUE (= B 2)) (ASSERT-TRUE (= C 3))))
    
    (assert-true (= 1 n))))

(garbage-collect)

(defrte float-float-integer (:cat float float integer))

(define-test test/destructuring-case-15b
  (let ((n 0))
    (destructuring-case '((1 2) 3)
      ((a b c)
       (declare (type float a b) (type integer c))
       (list a b c))
      ((a b c)
       (declare (type fixnum a b) (type integer c))
       (list a b c))
      (((a b) c)
       (declare (type float a b) (type integer c))
       (list a b c)))
    (assert-true (= 0 n))))

(define-test test/destructuring-case-16
  (assert-true (equal '(1 x 0)
                      (destructuring-case '(x)
                        ((name &key (count 0))
                         (declare (type fixnum count))
                         (list 1 name count))
                        ((name &key count)
                         (list 2 name count)))))

  (assert-true (equal '(2 x y)
                      (destructuring-case '(x :count y)
                        ((name &key (count 0))
                         (declare (type fixnum count))
                         (list 1 name count))
                        
                        ((name &key (count 'z))  (declare (type symbol count))
                         (list 2 name count)))))

  (assert-true (equal '(2 x 42)
                      (destructuring-case '(x)
                        ((name &key (count 0))
                         (declare (type string name)
                                  (type fixnum count))
                         (list 1 name count))
                        ((name &key (count 42))
                         (declare (type fixnum count))
                         (list 2 name count))))))


(define-test test/destructuring-case-17
  (assert-true (equal 1
                      (destructuring-case '(:x 1)
                        ((&key x)
                         x))))
  (assert-true (equal 1
                      (destructuring-case '(:x 1 :x 2)
                        ((&key x)
                         x))))
  (assert-true (equal nil
                      (destructuring-case '(:x 1 :x 2 :y 3)
                        ((&key x)
                         x))))
  (assert-true (equal 3
                      (destructuring-case '(:x 1 :x 2 :y 3)
                        ((&key y &allow-other-keys)
                         y))))
  )
  
  
(defvar *glob-var*)
(define-test test/destructuring-case-18
  (flet ((test-match (&rest candidate-expression)
           (destructuring-case candidate-expression
             ((a b c)
              (declare (type fixnum a b c) (ignore a b c))
              :clause-1)
             ((a b &optional (c (incf *glob-var*)))
              (declare (type fixnum a b)
                       (type number c)
                       (ignore a b c))
              :clause-2)
             ((a b c)
              (declare (type fixnum a)
                       (type number b c)
                       (ignore a b c))
              :clause-3)
             ((a b &key (c (incf *glob-var*)))
              (declare (type fixnum a b)
                       (type number c)
                       (ignore a b c))
              :clause-4)
             ((&rest args)
              (declare (ignore args))
              :clause-final))))
    (setf *glob-var* 0)
    (assert-true (equal (test-match 1 2 3)
                        :clause-1))
    (assert-true (equal (test-match 1 2 3.3)
                        :clause-2))
    (assert-true (equal (test-match 1 2)
                        :clause-2))
    (assert-true (equal (test-match 1 2.2)
                        :clause-final))
    (assert-true (equal (test-match 1 2.2 3)
                        :clause-3))
    (assert-true (equal (test-match 1 2.2 3.3)
                        :clause-3))
    (assert-true (equal (test-match 1 2 :c 3)
                        :clause-4))))


(define-test test/destructuring-case-19
  (flet ((test-match (&rest candidate-expression)
           (destructuring-case candidate-expression
             ((a b c)
              (declare (type fixnum a b c) (ignore a b c))
              :clause-1)
             ((a b &optional (c 0))
              (declare (type fixnum a b)
                       (type integer c)
                       (ignore a b c))
              :clause-2)
             ((a b &optional (c 0.0))
              (declare (type fixnum a b)
                       (type number c)
                       (ignore a b c))
              :clause-3))))))


(define-test test/destructuring-case-20
  (flet ((test-match (&rest candidate-expression)
           (destructuring-case candidate-expression
             ((b c)
              (declare (type fixnum b c) (ignore b c))
              :clause-1)
             ((b &optional (c 0))
              (declare (type fixnum b)
                       (type integer c)
                       (ignore b c))
              :clause-2)
             ((b &optional (c 0.0))
              (declare (type fixnum b)
                       (type number c)
                       (ignore b c))
              :clause-3))))))




(define-test test/destructuring-case-21
  (flet ((test-match (&rest candidate-expression)
           (destructuring-case candidate-expression
             ((X Y)
              (declare (type fixnum X Y) (ignore X Y))
              :clause-1)
             ((X Y)
              (declare (type fixnum X)
                       (type integer Y)
                       (ignore X Y))
              :clause-2)
             ((X Y)
              (declare (type (or string fixnum) X)
                       (type number Y)
                       (ignore X Y))
              :clause-3))))))

   
