;; Copyright (c) 2019 EPITA Research and Development Laboratory
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

(in-package   :cl-robdd)

(defun bdd-std-numerical-cmp (num1 num2)
  (cond ((= num1 num2)
         '=)
        ((< num1 num2)
         '<)
        ((> num1 num2)
         '>)
        (t
         (error "cannot compare ~A and ~A" num1 num2))))

(defun numerical-cnf-to-bdd (clauses)
  "CLAUSES is a list of sublists, each called a clause.
 A clause is a list of integers, i, either positive or negative.  (abs i)
 represents a literal form. If i<0, the literal is interpreted as negated.
 The clause consists of zero or more such integers, which are interpeted 
 as the disjunction of the clauses.  E.g., (1 -2 3) represents (x1 + !x2 + x3).
 CLAUSES represents a conjunction such as (and clause1 clause2 ...)."
  (labels ((clause-to-bdd (obj)
             (etypecase obj
               (bdd obj)
               (list
                ;; convert list of integers into a bdd
                (bdd (cons 'or
                           (mapcar (lambda (num)
                                     (if (plusp num)
                                         num
                                         (list 'not (abs num))))
                                   obj)))))))
    (the bdd (tree-reduce #'bdd-and clauses
                          :key #'clause-to-bdd
                          :stop-when *bdd-false*
                          :initial-value *bdd-true*))))
                 
(defun comb (n m &aux (acc 1))
  (declare (type unsigned-byte n m))
  ;; how many ways to chose m items from a population of n
  ;; n * (n-1) * ... (n-m+1)
  ;;  (comb 10 9) = 10
  ;;  (comb 10 8) = 10 * 9
  ;;  (comb 10 7) = 10 * 9 * 8
  (assert (<= m n))
  (loop :for i :from (1+ m) :to n
        :do (setf acc (* acc i)))
  acc)
               
(defun random-cnf-clauses (num-vars num-clauses terms-per-clause)
  "Return a list of distinct clauses, where each clause is a list of
 integers between (- num-vars) and num-vars, excluding 0, with the
 restriction that if n is in the clause, then -n is not in the clause.
 each such clause represents a clause in a CNF form where (abs n)
 represents a literal and -(abs n) represents the negated literal."
  (assert (< 0 num-vars))
  (assert (<= terms-per-clause num-vars))
  (assert (<= num-clauses (* (comb num-vars terms-per-clause) (expt 3 num-vars))))
  (labels ((random-var ()
             ;; if num-vars is 13, we need to chose between 1 and 13, not between 0 and 12
             (1+ (random num-vars)))
           (randomly-negate (num)
             (if (= 0 (random 2))
                 num
                 (- num)))
           (random-clause (&aux (remaining terms-per-clause) clause)
             (loop :while (plusp remaining)
                   :for var = (random-var)
                   :unless (member var clause :key #'abs)
                     :do (progn (decf remaining)
                                (push (randomly-negate var) clause)))
             (sort clause #'< :key #'abs)))
    (let ((remaining num-clauses)
          clauses)
      (loop :while (plusp remaining)
            :for clause = (random-clause)
            :unless (member clause clauses :test #'equal)
              :do (progn (decf remaining)
                         (push clause clauses)))
      clauses)))

(defun random-cnf-sat-p (num-vars num-clauses terms-per-clause)
  (the bdd (numerical-cnf-to-bdd (random-cdf-clauses num-vars num-clauses terms-per-clause))))

(defun cnf-statistics (&key num-vars num-clauses terms-per-clause num-samples)
  (bdd-with-new-hash (&aux (num-sat 0) (*bdd-cmp-function* #'bdd-std-numerical-cmp))
    (dotimes (_ num-samples)
      (unless (eql *bdd-false* (random-cnf-sat-p num-vars num-clauses terms-per-clause))
        (incf num-sat)))
    (values (float (/ num-sat num-samples))
            num-sat num-samples)))

(defun quine-mccluskey-reduce (num-vars clauses)
  "Given a list of CLAUSES which represent a CNF form,  apply phase-1 of the
 Quine McCluskey method to reduce terms such as (a+b)(a+b')->a
 In addition, (a+b)(a+b+c)->(a+b) is also done."
  (labels ((sort-clause (clause)
             (sort clause #'< :key #'abs))
           (count-positive (clause)
             (count-if (lambda (var)
                         (plusp var)) clause))
           (group-clauses (&aux (vec (make-array (1+ num-vars) :initial-element nil)))
             (loop :for pair :in (sort (group-by clauses :key #'count-positive) #'> :key #'car)
                   :do (destructuring-bind (length clauses) pair
                         (setf (aref vec length) (mapcar #'sort-clause clauses))))
             vec)
           (compatible? (clause1 clause2)
             ;; returns boolean indicating whether all the corresponding items are equal in absolute value
             (cond
               ((not clause1)
                ;; did we reach the end of both lists at the same time?  then compatible yes.
                (not clause2))
               ((not clause2)
                nil ;;(not clause1)
                )
               (t
                (and (= (abs (car clause1))
                        (abs (car clause2)))
                     (compatible? (cdr clause1) (cdr clause2))))))
           (reduce-one-var (clause1 clause2)
             ;; given two compatible (according to compatible?) clauses, return the list of
             ;;   equal elements, ie removing elements which agree in value but differ in absolute-value.
             ;;   only one such element should be removed.
             (mapcan (lambda (v1 v2)
                       (if (= v1 v2)
                           (list v1)
                           nil)) clause1 clause2))
           (reduce-pass (top-index vec)
             (let (add remove)
               (loop :for i :from top-index :downto 1
                     :do (dolist (clause-1 (aref vec i))
                           (dolist (clause-2 (aref vec (1- i)))
                             (when (compatible? clause-1 clause-2)
                               (pushnew clause-1 remove :test #'equal)
                               (pushnew clause-2 remove :test #'equal)
                               (pushnew (reduce-one-var clause-1 clause-2) add)))))
               (cond
                 ((or remove add)
                  (dolist (clause remove)
                    (let ((i (count-positive clause)))
                      (setf (aref vec i) (remove clause (aref vec i) :test #'equal))))
                  (dolist (clause add)
                    (pushnew clause (aref vec (count-positive clause))  :test #'equal))
                  (reduce-pass (1- top-index) vec))
                 (t
                  (loop :for i :from 0 :to num-vars
                        :nconc (aref vec i))))))
           (remove-supers (clauses acc)
             (cond
               ((null clauses)
                (reverse acc))
               (t
                (remove-supers (cdr clauses)
                               (if  (exists c2 (cdr clauses)
                                      (subsetp c2 (car clauses)))
                                    acc
                                    (cons (car clauses) acc)))))))

    (remove-supers (reverse (reduce-pass num-vars (group-clauses)))
                   nil)))
