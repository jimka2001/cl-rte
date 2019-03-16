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


(in-package   :rte)

(defun rte-expand-type (type-name)
  (unless (valid-type-p type-name)
    (warn "Invalid type specifier ~S" type-name))
  (if (typep type-name '(cons (eql rte)))
      type-name
      (let ((expanded-type (type-expand type-name)))
        (if (equal expanded-type type-name)
            type-name ;; return the orginal, rather than something equal but not eq to it
            expanded-type))))

(defun with-expanded-type (type-name f-original f-expanded)
  (declare (type (function (t) t) f-original f-expanded))
  (let ((expanded-type (rte-expand-type type-name)))
    ;; (unless (equal type-name expanded-type)
    ;;   (format t "~&expanded ~A --> ~A~%" type-name expanded-type))
    (if (eq expanded-type type-name)
        (funcall f-original type-name)
        (funcall f-expanded expanded-type))))

(defvar *rte-hash* (make-hash-table :test #'eq)
  "Hash table storing named rte's for reuse by (:rte ...) within an rte pattern")

(defun traverse-pattern (pattern &rest functions
                         &key
                           (client #'(lambda (pattern)
                                       (apply #'traverse-pattern pattern functions)))
                           (f-0-* #'(lambda (patterns)
                                      (cons :* (mapcar client patterns))))
                           (f-1-* #'(lambda (patterns)
                                      (apply #'traverse-pattern `(:cat ,@patterns (:* ,@patterns)) functions)))
                           (f-0-1 #'(lambda (patterns)
                                      (apply #'traverse-pattern `(:or :empty-word (:cat ,@patterns)) functions)))
                           (f-and #'(lambda (patterns)
                                      (cons :and (mapcar client patterns))))
                           (f-or  #'(lambda (patterns)
                                      (cons :or (mapcar client patterns))))
                           (f-not #'(lambda (patterns)
                                      (cons :not (mapcar client patterns))))
                           (f-cat #'(lambda (patterns)
                                      (cons :cat (mapcar client patterns))))
                           (f-permutations #'(lambda (patterns)
                                               (let (permutations)
                                                 (map-permutations #'(lambda (permutation)
                                                                       (push (cons :cat permutation) permutations))
                                                                   patterns)
                                                 (cons :or permutations))))
                           (f-empty-word client)
                           (f-empty-set client)
                           (f-type client))
  "Walk a regular type expression, stopping at nodes which designate lisp types.
The given functions F-0-*, F-1-*, etc are called on each node to continue the traversal.
The default behavior is to walk to list and copy it, however the behavior and return value actuall
depend on the choice of F-... function given."
  (cond ((atom pattern)
         (case pattern
           ((:empty-word)
            (funcall f-empty-word pattern))
           ((:empty-set)
            (funcall f-empty-set pattern))
           (t
            (with-expanded-type pattern
              f-type
              (lambda (expanded-pattern)
                (apply #'traverse-pattern expanded-pattern functions))))))
        (t                              ; list
         (case (car pattern)
           ((:rte rte-name)
            (assert (and (cadr pattern)
                         (null (cddr pattern))) () "invalid rte designator ~A" pattern)
            (assert (gethash (cadr pattern) *rte-hash*) ()
                    "Unknown rte definition ~A" (cadr pattern))
            (apply #'traverse-pattern (gethash (cadr pattern) *rte-hash*) functions))
           ((:or)
            (funcall f-or (cdr pattern)))
           ((:and)
            (funcall f-and (cdr pattern)))
           ((:not)
            (funcall f-not (cdr pattern)))
           ((:0-* :0-or-more :*)
            (funcall f-0-* (cdr pattern)))
           ((:1-* :1-or-more :+)
            (funcall f-1-* (cdr pattern)))
           ((:0-1 :0-or-1 :?)
            (funcall f-0-1 (cdr pattern)))
           ((:cat :1)
            (funcall f-cat (cdr pattern)))
           ((:permute)
            (funcall f-permutations (cdr pattern)))
           ((type)
            (assert (null (cddr pattern)) nil "Invalid type: ~S" pattern)
            (funcall f-type (cadr pattern)))
           (t
            (with-expanded-type pattern
              f-type
              (lambda (expanded-pattern)
                (apply #'traverse-pattern expanded-pattern functions))))))))

(defun alphabetize (patterns)
  "non-descructively sort a list of patterns into a canonical order."
  (declare (type list patterns))
  (sort (copy-list patterns) #'cmp-objects))

(defun remove-redundant-types (patterns operator)
  "Given the operand list of either :or or :and, return a new operand list understand some basic logical reductions based on super-typeness."
  (declare (type (member :and :or) operator))
  (if (and (eql :and operator)
           (exists p1 patterns
             (and (valid-type-p p1)
                  (exists p2 (cdr patterns)
                    (and (valid-type-p p2)
                         ;; (subtype (and T1 T2) nil) means T1 and T2 are mutually disjoint such as string number
                         (subtypep (list 'and p1 p2) nil))))))
      (list :empty-set)
      (remove-if (lambda (p1)
                   (and (valid-type-p p1) ;; can only remove redundant types if they are valid lisp types
                        (exists p2 patterns
                          (and (not (equal p1 p2))
                               (valid-type-p p2)
                               (case operator
                                 ((:or)
                                  (subtypep p1 p2))
                                 (t
                                  (subtypep p2 p1)))))))
                 patterns)))


(defun partition-by-predicate (predicate data)
  (let (true-elements false-elements)
    (dolist (element data)
      (if (funcall predicate element)
          (push element true-elements)
          (push element false-elements)))
    (values true-elements false-elements)))


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

(defun canonicalize-pattern (re)
  "Given a regular-type-expression, return a canonical form."
  (fixed-point #'canonicalize-pattern-once re :test #'equal))

(defun nullable (re)
  (traverse-pattern re
               :f-empty-set (constantly nil)
               :f-empty-word (constantly t)
               :f-type (constantly nil)
               :f-0-* (constantly t)
               :f-cat #'(lambda (patterns)
                          (every #'nullable patterns))
               :f-not #'(lambda (patterns)
                          (assert (null (cdr patterns)) () "Invalid :not rte, ~S" (cons :not patterns))
                          (not (nullable (car patterns))))
               :f-or #'(lambda (patterns)
                         (some #'nullable patterns))
               :f-and #'(lambda (patterns)
                          (every #'nullable patterns))))

(defun uniquify (objects)
  (cond
    ((null (cdr objects))
     objects)
    ((member (car objects) (cdr objects) :test #'equal)
     (uniquify (cdr objects)))
    (t
     (cons (car objects)
           (uniquify (cdr objects))))))

(defun derivative (pattern wrt-type)
  (flet ((walk (patterns)
           (mapcar (lambda (p)
                     (derivative (canonicalize-pattern p) wrt-type))
                   patterns)))
    (canonicalize-pattern
     (traverse-pattern pattern
                  :f-empty-word (constantly :empty-set)
                  :f-empty-set  (constantly :empty-set)
                  :f-type  #'(lambda (single-type-pattern)
                               (cond
                                 ((equal wrt-type single-type-pattern)
                                  ;; the check for equivalence is not strictly necessary because if T1 and T2 are equivalent types
                                  ;; then they are NOT mutually exclusive, thus the 3rd clause of this cond would be taken.
                                  ;; Nevertheless, equivalence check is probably common, and fast.
                                  :empty-word)
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
                                  :empty-word)))
                  :f-or    #'(lambda (patterns)
                               (cons :or (walk patterns)))
                  :f-and   #'(lambda (patterns)
                               (cons :and (walk patterns)))
                  :f-not   #'(lambda (patterns)
                               (cons :not (walk patterns)))
                  :f-cat #'(lambda (patterns)
                             (flet ((term1 ()
                                      `(:cat
                                        ,(derivative (car patterns) wrt-type)
                                        ,@(cdr patterns)))
                                    (term2 ()
                                      (derivative `(:cat ,@(cdr patterns)) wrt-type)))
                               (cond
                                 ((null (cdr patterns))
                                  ;; if :cat has single argument, (derivative (:cat X) Y) --> (derivate X Y)
                                  (derivative (car patterns) wrt-type))
                                 ((nullable (car patterns))
                                  `(:or ,(term1) ,(term2)))
                                 (t
                                  (term1)))))
                  :f-0-* #'(lambda (patterns)
                             (let ((deriv (derivative `(:cat ,@patterns) wrt-type)))
                               `(:cat ,deriv (:* ,@patterns))))))))

(defun first-types (pattern)
  (traverse-pattern pattern
               :f-empty-word (constantly nil)
               :f-empty-set  (constantly nil)
               :f-type #'list
               :f-or   #'(lambda (patterns)
                           (mapcan #'first-types patterns))
               :f-and #'(lambda (patterns)
                          (mapcan #'first-types patterns))
               :f-not #'(lambda (patterns)
                          (mapcan #'first-types patterns))
               :f-cat #'(lambda (patterns)
                          (cond ((null (cdr patterns))
                                 (first-types (car patterns)))
                                ((nullable (car patterns))
                                 (append (first-types (car patterns))
                                         (first-types (cons ':cat (cdr patterns)))))
                                (t
                                 (first-types (car patterns)))))
               :f-0-* #'(lambda (patterns)
                          (first-types (cons ':cat patterns)))))

(defclass rte-state-machine (ndfa:state-machine)
  ((ndfa::test :initform #'typep)
   (deterministicp :initform t)
   (transition-label-combine :initform (lambda (a b)
                                         (type-to-dnf-bottom-up (bdd-reduce-lisp-type  `(or ,a ,b)))))
   (transition-label-omit :initform (lambda (label)
                                      ;; we omit creating nil  transitions
                                      ;;    (ie. transitions whose label is nil)
                                      ;;    on these state machines.
                                      (eq nil (lisp-types:type-to-dnf-bottom-up label))))
   (transition-label-equal :initform (lambda (a b &aux
                                                    (a-reduc (lisp-types:type-to-dnf-bottom-up a))
                                                    (b-reduc (lisp-types:type-to-dnf-bottom-up b)))
                                       (and (subtypep a-reduc b-reduc)
                                            (subtypep b-reduc a-reduc))))))

(defmethod print-object ((rte rte-state-machine) stream)
  (print-unreadable-object (rte stream :type t :identity nil)
    (dolist (state (get-initial-states rte))
      (format stream "~A" state))))

(defmethod populate-synchronized-product ((sm-product rte-state-machine)
                                          (sm1 rte-state-machine)
                                          (sm2 rte-state-machine)
                                          &key (boolean-function (lambda (a b) (and a b)))
                                            
                                            (minimize t)
                                            (complement-transition-label (lambda (state)
                                                                           (lisp-types:type-to-dnf-bottom-up
                                                                            `(and t (not (or ,@(and state (mapcar #'transition-label (transitions state)))))))))
                                            (merge-transition-labels (lambda (label-1 label-2)
                                                                       (lisp-types:type-to-dnf-bottom-up `(and ,label-1
                                                                                                               ,label-2))))
                                            
                                                               
                                            (final-state-callback (lambda (product-state st1 st2)
                                                                    (setf (state-exit-form product-state)
                                                                          (cond
                                                                            ((and st1
                                                                                  st2
                                                                                  (state-exit-form st1)
                                                                                  (state-exit-form st2))
                                                                             ;; if the two states both have an exit form, take the one with
                                                                             ;; lowest clause-index (highest priority), this is the one which appears
                                                                             ;; first in the unexpanded typecase.
                                                                             (if (< (clause-index st1) (clause-index st2))
                                                                                 (state-exit-form st1)
                                                                                 (state-exit-form st2)))
                                                                            (t
                                                                             (or (and st1 (state-exit-form st1))
                                                                                 (and st2 (state-exit-form st2)))))))))
  (call-next-method sm-product sm1 sm2 :boolean-function boolean-function
                                       :minimize minimize
                                       :merge-transition-labels merge-transition-labels
                                       :complement-transition-label complement-transition-label
                                       :final-state-callback final-state-callback))

(defgeneric dump-code (object &key var))

(defmethod dump-code ((pattern list) &key (var 'seq))
  (dump-code (rte-to-dfa pattern :reduce t) :var var))

(defmethod dump-code ((ndfa rte-state-machine) &key (var 'seq))
  (let* ((states (append (ndfa:get-initial-states ndfa)
                         (set-difference (ndfa:states ndfa)
                                         (ndfa:get-initial-states ndfa) :test #'eq)))
         (exit-form-p (find-if (lambda (state)
                                 ;; something evaluatable?
                                 (typecase (state-exit-form state)
                                   ((member t nil) nil)
                                   (keyword nil)
                                   (cons t)
                                   (symbol t)
                                   (t nil))) (get-final-states ndfa)))
         (state-assoc (let ((n 0))
                        (mapcar (lambda (state)
                                  (list state (if exit-form-p
                                                  (gensym "L")
                                                  (incf n))))
                                states)))
         (list-end `(null ,var))
         (list-next `(pop ,var))
         (i (if exit-form-p (gensym "I") 'i))
         (check (if exit-form-p (gensym "CHECK") 'check))
         (len (if exit-form-p (gensym "LEN") 'len))
         (simple-vector-end `(>= ,i ,len))
         (simple-vector-next `(prog1 (svref ,var ,i)
                                (incf ,i)))

         (vector-end `(>= ,i ,len))
         (vector-next `(prog1 (aref ,var ,i)
                         (incf ,i)))

         #+sbcl (sequence-end `(or (sequence:emptyp ,var)
                            (>= ,i ,len)))
         #+sbcl (sequence-next `(prog1 (sequence:elt ,var ,i)
                           (incf ,i))))
         
    (labels ((state-name (state)
               (declare (type ndfa::state state))
               (or (cadr (assoc state state-assoc :test #'eq))
                   (error "no state name registered for state ~A, available states are ~A" state state-assoc)))
             (dump-typecase-transition (transition)
               (declare (type ndfa::transition transition))
               (assert (typep (ndfa:next-state transition) 'ndfa::state))
               (assert (typep (state-name (ndfa:next-state transition)) '(not null)))
               `(,(transition-label transition)
                 (go ,(state-name (ndfa:next-state transition)))))
             (dump-case-transition (transition)
               (declare (type ndfa::transition transition))
               (assert (typep (ndfa:next-state transition) 'ndfa::state))
               (assert (typep (state-name (ndfa:next-state transition)) '(not null)))
               `(,(cdr (transition-label transition))
                 (go ,(state-name (ndfa:next-state transition)))))
             (dump-end (state end)
               (cond ((null (state-final-p state))
                      `(when ,end
                         (return-from ,check nil)))
                     ((state-sticky-p state)
                      `(return-from ,check ,(state-exit-form state)))
                     (t
                      `(when ,end
                         (return-from ,check ,(state-exit-form state))))))
             (dump-case (state next)
               (cond
                 ((every #'(lambda (trans)
                             (and (listp (transition-label trans))
                                  (member (car (transition-label trans)) '(eql member))))
                         (transitions state))
                  `(case ,next
                     ,@(mapcar #'dump-case-transition (transitions state))
                     (t (return-from ,check nil))))
                 (t
                  (let* ((leading-clauses (mapcar #'dump-typecase-transition (transitions state)))
                         ;; We use subtypep here to remove the final T clause
                         ;; cases like (string...) ((not string) ...)  or
                         ;; if a (T ...) clause already exists.
                         (exhaustive? (subtypep t (cons 'or (mapcar #'car leading-clauses))))
                         (final-clause-option (if exhaustive?
                                                  nil
                                                  `((t (return-from ,check nil))))))
                    ;; final-clause-option is a ,@-comaptible list of the final
                    ;; clause or NIL in the situation that the leading clauses are
                    ;; exhaustive.  This is because we
                    ;; want to eliminate a final T clause in the clause the leading 
                    ;; clauses are exhaustive.
                    `(typecase ,next ;; TODO -- change to bdd-typecase, but this makes startup VERY slow
                                   ,@leading-clauses
                                   ,@final-clause-option)))))
             (dump-state (state end next)
               (copy-list `(,(state-name state)
                            ,(dump-end state end)
                            ,(dump-case state next))))
             (dump-tagbody (end final-next)
               (cond
                 ((get-initial-states ndfa)
                  (assert (= 1 (length (get-initial-states ndfa))))
                  (assert (typep (car (get-initial-states ndfa)) 'ndfa::state))
                  `(tagbody 
                      (go ,(state-name (car (get-initial-states ndfa))))
                      ,@(mapcan #'(lambda (state) (dump-state state end final-next)) states)))
                 (t
                   nil))))

      `(lambda (,var)
         ;; Don't declare seq a sequence! because if this function gets called with
         ;; a non-sequence, we want to simply return nil, rather than signaling
         ;; an error.
         (declare (optimize (speed 3) (debug 0) (safety 0))
                  ;; (optimize (speed 0) (debug 3) (safety 3))
                  )
         (block ,check
           (typecase ,var
             (list
              ,(dump-tagbody list-end list-next))
             (simple-vector
              (let ((,i 0)
                    (,len (length ,var)))
                (declare (type (and unsigned-byte fixnum) ,i ,len) (ignorable ,len))
                ,(dump-tagbody simple-vector-end simple-vector-next)))
             (vector
              (let ((,i 0)
                    (,len (length ,var)))
                (declare (type (and fixnum unsigned-byte) ,i ,len) (ignorable ,len))
                ,(dump-tagbody vector-end vector-next)))
             #+sbcl
             (sequence           ; case to handle extensible sequences
              (let ((,i 0)
                    (,len (sequence:length ,var))) ; sequence (such as infinite sequence) might not support length
                (declare (type (and fixnum unsigned-byte) ,i ,len) (ignorable ,len))
                ,(dump-tagbody sequence-end sequence-next)))
             (t
              nil)))))))

(defmethod ndfa:perform-some-transitions ((ndfa rte-state-machine) starting-states input-sequence)
  (declare (type list starting-states)
           (type sequence input-sequence))
  (let ((deterministicp (ndfa:deterministicp ndfa))
        (current-states starting-states)
        (sticky-final-states (intersection (ndfa:get-sticky-states ndfa) (ndfa:get-final-states ndfa) :test #'eq)))
    (every (if sticky-final-states
               (lambda (input)
                 (setf current-states
                       (block do-states
                         (mapcan (lambda (state)
                                   (mapcan (lambda (transition)
                                             (when (typep input (transition-label transition))
                                               (let ((next-state (ndfa:next-state transition)))
                                                 (cond
                                                   ((member next-state sticky-final-states :test #'eq)
                                                    (return-from ndfa:perform-some-transitions (list next-state)))
                                                   (deterministicp
                                                    (return-from do-states (list next-state)))
                                                   (t
                                                    (list next-state))))))
                                           (transitions state)))
                                 current-states))))
               (lambda (input)
                 (setf current-states
                       (block do-states
                         (mapcan (lambda (state)
                                   (mapcan (lambda (transition)
                                             (when (typep input (transition-label transition))
                                               (cond
                                                 (deterministicp
                                                  (return-from do-states (list (ndfa:next-state transition))))
                                                 (t
                                                  (list (ndfa:next-state transition))))))
                                           (transitions state)))
                                 current-states)))))
           input-sequence)
    current-states))


(defmethod calc-sticky-states ((sm rte-state-machine))
  ;; if a state only has transitions which are t (or some supertype of t such as (or number (not number))
  ;; then mark it as not escapable.
  (dolist (state (states sm))
    (setf (ndfa:state-sticky-p state)
          ;; a state is sticky, or non-escapable, if evert transition
          ;; has a type=t transition to the state itself.  NOTE that
          ;; this test is somewhat dangerous because it is being run
          ;; using transition-label and state-label, i.e., before next
          ;; (the next state of the transition) has been lazily
          ;; calculated.
          (and (transitions state)
               ;; every transition leads back to this same state
               (every (lambda (transition)
                        (equal (state-label state) (next-label transition)))
                      (transitions state))
               ;; the union of the types is t
               (subtypep t (reduce (transition-label-combine sm) (mapcar #'transition-label (transitions state))
                                   :initial-value nil))))))

(defun rte-to-dfa (pattern &key trim reduce (final-body t) (clause-index 0))
  "Create and return a finite state machine (ndfa) which can be used to determine if a given list
consists of values whose types match PATTERN."
  ;; cannot reduce without trimming
  (setf trim (or trim reduce))
  (let ((sm (make-instance 'rte-state-machine))
        done ; list of patterns for which a state in the state machine has already been created
        pending ; list of paterns (derivatives) pending to examine, some are in the done list, some not.
        (pattern (canonicalize-pattern pattern)))
    (flet ((create-state (re &key initial-p)
             (cond ((member re done :test #'equal)
                    nil)
                   ((eql :empty-set re)
                    nil)
                   (t
                    (push re done)
                    (let (transitions
                          (nullable-p (nullable re)))
                      ;; we must partition the universe by finding the
                      ;; maximal disjoint type decomposition of all the first types
                      ;; plus t.  For example, if the only first type is STRING,
                      ;; then we need (STRING (NOT STRING)), otherwise
                      ;; (:not ...) won't work properly
                      (dolist (type (mdtd-bdd (uniquify (cons t (first-types re)))))
                        (let ((deriv (derivative re type)))
                          (case deriv
                            ((:empty-set)
                             nil)
                            (t
                             (pushnew deriv pending :test #'equal)
                             (push (list :next-label deriv
                                         :transition-label (lisp-types:type-to-dnf-bottom-up type))
                                   transitions)))))
                      ;; TODO, can't this be made to create an instance of an
                      ;;   rte-specific subclass of state.   add-rte-state?
                      ;;   thus removing exit-form from ndfa:state
                      (ndfa:add-state sm
                                      :label re
                                      :initial-p initial-p
                                      :final-p nullable-p
                                      ;; TODO need to add :priority to arbitrate between colliding exit conditions
                                      :exit-form (when nullable-p
                                                   final-body)
                                      :clause-index (if nullable-p
                                                        clause-index
                                                        nil)
                                      :transitions transitions)))))

           (sort-transitions ()
             (declare (notinline sort))
             (dolist (state (ndfa:states sm))
               (setf (ndfa:transitions state)
                     (sort (ndfa:transitions state)
                           #'cmp-objects :key #'ndfa:transition-label))))
           (parallel-transitions (&aux (hash (make-hash-table :test #'equal)))
             ;; if two (or more) transitions from A lead to B, we can
             ;; replace with one transition using lisp type (or label-AB1 label-AB2 ...)
             (dolist (s1 (ndfa:states sm))
               (dolist (tr1 (ndfa:transitions s1))
                 (push tr1 (gethash (list s1 (ndfa:next-label tr1)) hash nil))))
             (maphash (lambda (key transitions &aux (s1 (car key)) (tr1 (car transitions)))
                        (when (cdr transitions)
                          (let ((transition-labels (mapcar #'ndfa:transition-label transitions)))
                            (setf (ndfa:transition-label tr1)
                                  (reduce (transition-label-combine sm)
                                          transition-labels
                                          :initial-value nil))
                            (setf (ndfa:transitions s1)
                                  (set-difference (ndfa:transitions s1)
                                                  (cdr transitions))))))
                      hash)))
      
      (create-state pattern :initial-p t)
      (loop :while pending
            :do (create-state (pop pending)))
      (setf sm
            (cond (reduce
                   (minimize-state-machine sm))
                  (trim
                   (trim-state-machine sm))
                  (t sm)))
      (parallel-transitions)
      (sort-transitions)
      (calc-sticky-states sm))
    sm))
      
(defun remember-state-machine (sm pattern)
  (setf (gethash pattern *state-machines*) sm)
  (register-dependents sm)
  sm)

(defun find-state-machine (pattern)
  (gethash pattern *state-machines*))

(defgeneric match-sequence (input-sequence pattern))

(defmethod match-sequence (input-sequence pattern)
  (declare (ignore input-sequence pattern))
  nil)

(defmethod match-sequence (input-sequence (sm ndfa:state-machine))
  (some #'ndfa:state-final-p (ndfa:perform-transitions sm input-sequence)))

(defmethod match-sequence (input-sequence (pattern list))
  (match-sequence input-sequence (or (find-state-machine pattern)
                                     (remember-state-machine (rte-to-dfa pattern) pattern))))

(defvar *rte-pattern-functions* nil "List of function names created by MAKE-RTE-FUNCTION-NAME")

(defun make-rte-function-name (pattern)
  (when (and (consp pattern)
             (symbolp (car pattern))
             (eql 'rte (car pattern))) 
    (error "cannot make pattern function of pattern string with ~S: ~S" (car pattern) pattern))
  (let* ((name-str (with-output-to-string (str)
                     (format str "-")
                     (let ((*package* (find-package :rte)))
                       (write pattern
                              :stream str
                              :pretty nil
                              :escape t))))
         (name-sym (intern name-str (symbol-package 'rte))))
    (pushnew name-sym *rte-pattern-functions*)
    ;; (format t "~%rte pattern: ~A~%" name-sym) ; debug
    name-sym))

(defun define-rte (pattern)
  ;; TODO
  ;; optimization, a top-level (:cat ...) which contains only
  ;; lisp type specifiers, can be expanded directly into (or (cons ...) (and (not cons) sequence (satisfies ...)))
  ;; This expansion should give the compiler more information about the
  ;; object

  (setf (gethash pattern *rte-types*)
        (let ((dfa (rte-to-dfa pattern))
              (function-name (make-rte-function-name pattern)))
          (register-dependents dfa)
          (remember-state-machine dfa pattern)
          (setf (symbol-function function-name) (eval (dump-code dfa)))
          `(and sequence (satisfies ,function-name)))))

(deftype rte (pattern)
  "Matches a list whose types constitute 'words' in a rational
language described by the given rational expression. The PATTERN must
either be a valid lisp type specifier or a list whose (car PATTERN) is
a keyword of type RTE-KEYWORD, and each element of (cdr PATTERN) is
a valid regular type expression.

:0-* -- matches the types of zero or more successive list elements, E.g,
:1-* -- matches the types of one or more successive list elements
:0-1 -- matches the type optionally (zero or one) list element
:or  -- specifies a logical disjunction of rational expressions
:and -- specifies a logical conjunction of rational expressions
:cat -- in order concatenation of rational expressions
:permute -- specifies any-order concatenation of rational expressions

        (typep '(nil x 1 y 2 z 3 (x) nil (y) nil)
                '(rte (:0-1 null)
                      (:1-* symbol number)
                      (:0-* list null)))

        (typep '(nil x 1 11 y 2 22 z 3 33 (x) nil (y) nil)
                '(rte (:0-1 null)
                      (:or (:1-* symbol number)
                           (:1-* symbol number number))
                      (:0-* list null)))
"
  (or (gethash pattern *rte-types* nil)
      (define-rte pattern)))

(defmacro defrte (rte-name pattern)
  "Declare a given RTE pattern so that that it can be used when loaded from fasl or referenced symbolically be another rte."
  (let* ((dfa (rte-to-dfa pattern))
         (name (make-rte-function-name pattern))
         (code (dump-code dfa)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (getf (symbol-plist ',name) :rte-pattern) ',pattern
             (gethash ',rte-name *rte-hash*) ',pattern)
       (defun ,name ,@(cdr code)))))

(defun rte-reset ()
  "Forget all regular type expressions."
  (maphash (lambda (pattern type)
             (declare (ignore type))
             (let ((name (make-rte-function-name pattern)))
               (setf (symbol-function name)
                     (lambda (&rest args)
                       (warn "function ~A no longer defined, the cache has been reset! redefining function" name)
                       (define-rte pattern)
                       (apply name args)))))
           *rte-types*)
  (maphash (lambda (rte-pattern lisp-type)
             (warn "Removing ~A/~A from *rte-types* hash table" rte-pattern lisp-type))
           *rte-types*)
  (maphash (lambda (parameterized-type-specifier function-name)
             (warn "Removing ~A/~A from *type-functions* hash table" parameterized-type-specifier function-name))
           rte::*type-functions*)
  (setf *rte-types* (make-hash-table :test #'equal))
  (setf *type-functions* (make-hash-table)))

(defun equivalent-patterns (rte1 rte2)
    (and (null (get-final-states (rte-to-dfa `(:and ,rte1 (:not ,rte2)) :reduce t)))
         (null (get-final-states (rte-to-dfa `(:and ,rte2 (:not ,rte1)) :reduce t)))))
