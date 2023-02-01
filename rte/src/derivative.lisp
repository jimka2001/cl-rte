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

(in-package   :rte)

(defun canonicalize-pattern (re)
  "Given a regular-type-expression, return a canonical form."
  (fixed-point #'canonicalize-pattern-once re :test #'equal))

(defun canonicalize-pattern-once (re)
  "Given a regular-type-expression, return a more canonical form.  This attempts to create an (:or ..) of (:and ...)'s, and
removing or resolving redundant or trivial type designators. CANONICALIZE-PATTERN calls this function multiple times until
a fixed point is found."
  (flet ((like-multipy (operator patterns &key idempotent)
           (setf patterns (remove :empty-word patterns))
           (let ((new (remove :empty-word (mapcar #'canonicalize-pattern patterns))))
             (cond
               ((member :empty-set patterns)
                :empty-set)
               ((cdr new) ; at least 2 args
                (cons operator new))
               (new ; exactly 1 arg
                (if idempotent
                    (car new)
                    (cons operator new)))
               (t ; empty art list
                :empty-word)))))
  (traverse-pattern re
                    :f-type #'(lambda (pattern)
                                (cond ((atom pattern)
                                       pattern)
                                      ((eql 'member (car pattern)) ; alphabetize the arguments of (member ...)
                                       (cons 'member (alphabetize (cdr pattern))))
                                      ((eql 'rte (car pattern))
                                       (cons 'rte (mapcar #'canonicalize-pattern (cdr pattern))))
                                      (t
                                       pattern)))
               :f-empty-set #'identity
               :f-empty-word #'identity
               :f-0-* #'(lambda (patterns)
                          (like-multipy :* patterns :idempotent nil))
               :f-cat #'(lambda (patterns)
                          ;; (:cat A B (:cat C D) E F) --> (:cat A B C D E F)
                          (setf patterns
                                (mapcan (lambda (term)
                                          (cond ((and (listp term)
                                                      (eql :cat (car term)))
                                                 (copy-list (cdr term)))
                                                (t
                                                 (list term))))
                                        patterns))
                          (like-multipy :cat patterns :idempotent t))
               :f-not #'(lambda (patterns &aux (pattern (car patterns)))
                          (typecase pattern
                            ((cons (eql :not))
                             (canonicalize-pattern (cadr pattern)))
                            ;;   (:not (:and A B)) --> (:or (:not A) (:not B))
                            ((cons (eql :and))
                             (cons :or (mapcar #'(lambda (p)
                                                   (canonicalize-pattern (list :not p))) (cdr pattern))))
                            ;;   (:not (:or A B)) --> (:and (:not A) (:not B))
                            ((cons (eql :or))
                             (cons :and (mapcar #'(lambda (p)
                                                    (canonicalize-pattern (list :not p))) (cdr pattern))))
                            ((cons (member :0-* :*) (cons (eql t) null)) ;; (:not (:0-* t)) --> :empty-set
                             :empty-set)
                            ((eql :empty-word) ;; (:not :empty-word) --> (:+ t)
                              '(:+ t))
                            ((eql :empty-set) ;; (:not :empty-set) --> (:* t)
                             '(:* t))
                            ((cons keyword)
                             (cons :not (mapcar #'canonicalize-pattern patterns)))
                            (t
                             `(:or :empty-word
                                   (not ,@patterns)
                                   (:cat t t (:0-* t))))))
               :f-or #'(lambda (patterns)
                         (let ((sub-or (setof s patterns
                                              (and (listp s)
                                                   (eql :or (car s))))))
                           ;; (:or (:or A B ) C D) --> (:or A B C D)
                           (dolist (s sub-or)
                             (dolist (p (cdr s))
                               (push p patterns)))
                           (setf patterns (set-difference patterns sub-or :test #'equal)))

                         ;; (:or ... :empty-set ...) --> (:or ...)
                         (setf patterns (remove :empty-set patterns :test #'eq))

                         ;; (:or (member 1 2 3) (member 10 20 30))
                         ;;  --> (:or (member 1 2 3 10 20 30))   ;; in some order, unspecified
                         (when (< 1 (count-if #'(lambda (obj)
                                                  (and (listp obj)
                                                       (member (car obj) '(eql member)))) patterns))
                           (multiple-value-bind (matches other) (partition-by-predicate #'(lambda (obj)
                                                                                            (and (listp obj)
                                                                                                 (member (car obj) '(eql member))))
                                                                                        patterns)
                             (setf patterns (cons (cons 'member (mapcan (lambda (match)
                                                                          (copy-list (cdr match))) matches))
                                                  other))))
                         (setf patterns (uniquify patterns)
                               patterns (remove :empty-set patterns)
                               patterns (mapcar #'canonicalize-pattern patterns)
                               patterns (remove :empty-set patterns)
                               patterns (uniquify patterns)
                               patterns (remove-redundant-types patterns :or))
                         ;; (:or A B (:0-* t))
                         ;;  --> (:or (:0-* t))
                         (when (intersection patterns
                                             '((:0-* t)
                                               (:0-or-more t)
                                               (:* t))
                                             :test #'equal)
                           (setf patterns (list '(:* t))))
                         (cond
                           ((cdr patterns)
                            ;; TODO, should not alphabetize patterns because it will not work in the case
                            ;; the types have side effect or if they are order dependents such as
                            ;; (:or (not list) (rte ...))
                            ;; this will break some tests, which will need to be fixed. and it will be harder
                            ;; to make assertions about complicated types.
                            (cons :or (alphabetize patterns)))
                           (patterns
                            (car patterns))
                           (t
                            :empty-set)))
                    :f-and  #'(lambda (patterns)
                                (let ((sub-and (setof s patterns
                                                      (and (listp s)
                                                           (eql :and (car s))))))
                                  ;; (:and (:and A B ) C D) --> (:and A B C C)
                                  (dolist (s sub-and)
                                    (dolist (p (cdr s))
                                      (push p patterns)))
                                  (setf patterns (set-difference patterns sub-and :test #'equal)))

                                ;; (:and A B (:0-* t))
                                ;;  --> (:and A B)
                                ;; TODO, is this correct?  what about (:and (:* t)) -/-> (:and)
                                (dolist (p '((:0-* t)
                                             (:0-or-more t)
                                             (:* t)))
                                  (setf patterns (remove p patterns :test #'equal)))
                                
                                ;; (:and (member 1 2 3) (member 2 3 4) ...)
                                ;;  --> (:and (member 2 3) ...)   ;; in some order, unspecified
                                (when (< 1 (count-if #'(lambda (obj)
                                                         (and (listp obj)
                                                              (member (car obj) '(eql member)))) patterns))
                                  (multiple-value-bind (matches other) (partition-by-predicate #'(lambda (obj)
                                                                                                   (and (listp obj)
                                                                                                        (member (car obj) '(eql member))))
                                                                                               patterns)
                                    (declare (notinline intersection))
                                    (let ((common (cdr (car matches))))
                                      (dolist (match (cdr matches))
                                        (setf common (intersection common (cdr match))))
                                      (setf patterns (cons (cons 'member common)
                                                           other)))))
                                ;; (:and (:or A B) C D) --> (:or (:and A C D) (:and B C D))
                                (let ((sub-or (find-if (lambda (s)
                                                         (and (listp s)
                                                              (eql :or (car s))))
                                                       patterns)))
                                  (setf patterns (remove sub-or patterns :test #'equal))
                                  
                                  (cond
                                    (sub-or
                                     (canonicalize-pattern (cons :or (loop :for p :in (cdr sub-or)
                                                                           :collect `(:and ,p ,@patterns)))))
                                    ((member :empty-set patterns)
                                     :empty-set)

                                    ((and (member :empty-word patterns)
                                          (exists p patterns
                                            (and (symbolp p)
                                                 (valid-type-p p)
                                                 (not (subtypep p nil)))))
                                     :empty-set)

                                    ((and (exists p patterns
                                            (and (symbolp p)
                                                 (valid-type-p p)
                                                 (not (subtypep p nil))))
                                          (exists p patterns
                                            (and (typep p '(cons (eql :cat)))
                                                 (> (count-if-not #'nullable (cdr p)) 1))))
                                     :empty-set)
                                    
                                    ;; NOTE that we cannot convert (:and A :empty-word) into :empty-set nor :empty-word
                                    ;;   becasue if A != :empty-word  then it reduces to :empty-set
                                    ;;   but if     A == :empty-word  then it reduces to :empty-word
                                    (t
                                     (setf patterns (uniquify patterns)
                                           patterns (mapcar #'canonicalize-pattern patterns)
                                           patterns (remove '(:0-* t) patterns :test #'equal)
                                           patterns (uniquify patterns)
                                           patterns (remove-redundant-types patterns :and))
                                     (cond
                                       ((member :empty-set patterns)
                                        :empty-set)
                                       ((cdr patterns)
                                        ;; TODO, should not alphabetize patterns because it will not work in the case
                                        ;; the types have side effect or if they are order dependents such as
                                        ;; (:or (not list) (rte ...))
                                        ;; this will break some tests, which will need to be fixed. and it will be harder
                                        ;; to make assertions about complicated types.
                                        (cons :and (alphabetize patterns)))
                                       (patterns
                                        (car patterns))
                                       (t
                                        '(:0-* t))))))))))

(defun derivative (pattern wrt-type &key type-hints)
  "Calculate the rational derivative of the given pattern.
type-hints is a list of triples:
  each triple is of the form (type factors disjoints)
  where factors is a list of known supertypes of the given type,
  and disjoints is a list of known disjoint types of the given type"
  (flet ((walk (patterns)
           (mapcar (lambda (p)
                     (derivative (canonicalize-pattern p) wrt-type :type-hints type-hints))
                   patterns)))
    (canonicalize-pattern
     (traverse-pattern pattern
                  :f-empty-word (constantly :empty-set)
                  :f-empty-set  (constantly :empty-set)
                  :f-type  #'(lambda (single-type-pattern)
                               (let* ((type-hint (assoc wrt-type type-hints))
                                      (factors (nth 1 type-hint))
                                      (disjoints (nth 2 type-hint)))
                               (cond
                                 ((equal wrt-type single-type-pattern)
                                  ;; the check for equivalence is not strictly necessary because if T1
                                  ;; and T2 are equivalent types then they are NOT mutually exclusive,
                                  ;; thus the 3rd clause of this cond would be taken.  Nevertheless,
                                  ;; equivalence check is probably common, and fast.
                                  :empty-word)
                                 ((member single-type-pattern factors :test #'equal)
                                  (format t "found ~A in factors ~A~%"
                                          single-type-pattern factors)
                                  :empty-word)
                                 ((member single-type-pattern disjoints :test #'equal)
                                  (format t "found ~A in disjoints ~A~%"
                                          single-type-pattern disjoints)
                                  :empty-set)
                                 ((smarter-subtypep wrt-type single-type-pattern)
                                  :empty-word)
                                 ((disjoint-types-p wrt-type single-type-pattern)
                                  ;; are the types mutually exclusive, e.g., string vs number
                                  ;; (warn "~A and ~A are mutually exclusive~%" wrt-type single-type-pattern)
                                  :empty-set)
                                 ((null (nth-value 1 (smarter-subtypep wrt-type single-type-pattern)))
                                  (warn-ambiguous-subtype :sub wrt-type :super single-type-pattern
                                                          :consequence "assuming :empty-word")
                                  :empty-word)
                                 ((null (nth-value 1 (smarter-subtypep single-type-pattern wrt-type)))
                                  (warn-ambiguous-subtype :sub single-type-pattern :super wrt-type
                                                          :consequence "assuming :empty-word")
                                  :empty-word)
                                 ((smarter-subtypep single-type-pattern wrt-type)
                                  (warn "cannot calculate the derivative of ~S~%    w.r.t. ~S because ~S is a subtype of ~S--assuming :empty-word"
                                        single-type-pattern wrt-type single-type-pattern wrt-type)
                                  :empty-word)
                                 (t
                                  (warn "cannot calculate the derivative of ~S~%    w.r.t. ~S--assuming :empty-word"
                                        single-type-pattern wrt-type)
                                  :empty-word))))
                  :f-or    #'(lambda (patterns)
                               (cons :or (walk patterns)))
                  :f-and   #'(lambda (patterns)
                               (cons :and (walk patterns)))
                  :f-not   #'(lambda (patterns)
                               (cons :not (walk patterns)))
                  :f-cat #'(lambda (patterns)
                             (flet ((term1 ()
                                      `(:cat
                                        ,(derivative (car patterns) wrt-type :type-hints type-hints)
                                        ,@(cdr patterns)))
                                    (term2 ()
                                      (derivative `(:cat ,@(cdr patterns)) wrt-type :type-hints type-hints)))
                               (cond
                                 ((null (cdr patterns))
                                  ;; if :cat has single argument, (derivative (:cat X) Y) --> (derivate X Y)
                                  (derivative (car patterns) wrt-type :type-hints type-hints))
                                 ((nullable (car patterns))
                                  `(:or ,(term1) ,(term2)))
                                 (t
                                  (term1)))))
                  :f-0-* #'(lambda (patterns)
                             (let ((deriv (derivative `(:cat ,@patterns) wrt-type :type-hints type-hints)))
                               `(:cat ,deriv (:* ,@patterns)))))))))


