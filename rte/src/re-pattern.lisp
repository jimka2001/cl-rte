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

(defun first-types (pattern)
  (traverse-pattern pattern
               :f-empty-word (constantly nil)
               :f-empty-set  (constantly nil)
               :f-type #'list
               ;; TODO this is a bug need to figure out what should
               ;;   (first-types (:or)), (first-types (:and)),
               ;;   (first-types (:cat)) return
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
                      ;; Some implementations of mdtd return a second value which is an assoc of type hints.
                      ;;   If we have the type-hints, we pass it along to derivative, which will have
                      ;;   an easier job of determining subtypeness and disjointness.
                      (multiple-value-bind (types type-hints) (mdtd-padl (uniquify (cons t (first-types re))))
                        (dolist (type types)
                          (let ((deriv (derivative re type :type-hints type-hints)))
                            (case deriv
                              ((:empty-set)
                               nil)
                              (t
                               (pushnew deriv pending :test #'equal)
                               (push (list :next-label deriv
                                           :transition-label (lisp-types:type-to-dnf-bottom-up type))
                                     transitions))))))
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

(defparameter *dump-code-strategy* (make-instance 'strategy-goto))

(defmethod dump-code ((pattern list) strategy &key (var 'seq))
  (dump-code (rte-to-dfa pattern :reduce t) strategy :var var))

(defmethod dump-code ((ndfa rte-state-machine) strategy &key (var 'seq))
  (declare (ignore var))
  (error
   "need to implement dump-code for strategy of classes ~A ~A~%" (type-of ndfa) (type-of strategy)))

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
          (setf (symbol-function function-name) (eval (dump-code dfa *dump-code-strategy*)))
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
         (code (dump-code dfa *dump-code-strategy*)))
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
