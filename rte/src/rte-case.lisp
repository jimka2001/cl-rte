;; Copyright (c) 2018,18 EPITA Research and Development Laboratory
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

(in-package :rte)

(defun rte-synchronized-product (dfas &key (minimize t) (boolean-function (lambda (a b)
                                                                            (or a b))))
  (declare (type (function (t t) t) boolean-function)
           (type list dfas)
           (optimize (speed 3) (debug 0) (compilation-speed 0)))           
  (tree-reduce #'(lambda (dfa1 dfa2)
                   (declare (type rte-state-machine dfa1 dfa2))
                   (synchronized-product dfa1 dfa2
                                         :minimize minimize
                                         :boolean-function boolean-function))
               dfas :initial-value (rte-to-dfa :empty-set)))

(defun rte-case-clauses-to-dfa (clauses &key (reduce nil) (disjoint-clauses t) (view nil))
  "Helper function for rte-case. Parses the clauses to compute three objects:
1) the list of unreachable-bodys
2) the dfa
3) a pair (transit boolean), the boolean indicates whether there is a transit through the
     dfa which does correspond to one of the given clauses.  If the boolean is TRUE
     then the transit indicates a list of type specifiers of such an object."
  (declare (type list clauses)
           (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (let (previous-patterns
        unreachable-bodys
        (clause-index 0)
        (transition-abrevs (list '(t "T0")))
        dfas)
    (labels ((exit-form (body)
               ;; smart body encapsulation, only wrap in progn if necessary
               (typecase body
                 ((cons t null)
                  (car body))
                 (t
                  `(progn ,@body))))
             (transform-clause (clause)
               (destructuring-bind (pattern &rest body) clause
                 (let ((derived-pattern `(:and ,pattern (:not (:or ,@previous-patterns)))))
                   (push pattern previous-patterns)
                   (if (equivalent-patterns :empty-set derived-pattern)
                       (push body unreachable-bodys)
                       (let ((dfa (rte-to-dfa (if disjoint-clauses
                                                  derived-pattern
                                                  pattern)
                                              :reduce reduce
                                              :final-body (exit-form body)
                                              :clause-index (incf clause-index))))
                         (push dfa dfas)
                         (when view
                           (ndfa-to-dot dfa nil :view t :transition-legend t
                                                :state-legend t
                                                :prefix (format nil "clause-~D" clause-index)
                                                :transition-abrevs transition-abrevs
                                                :transition-label-cb (lambda (label name)
                                                                       (pushnew (list label name)
                                                                                transition-abrevs))
                                                :title (let ((*print-case* :downcase))
                                                         (if disjoint-clauses
                                                             (format nil "disjoint-pattern = ~s" derived-pattern)
                                                             (format nil "pattern = ~s" pattern)))))))))))
      (dolist (clause clauses)
        (transform-clause clause))
      (let* ((dfa-remainder (rte-to-dfa `(:and (:* t) (:not (:or ,@previous-patterns))) :reduce t))
             (transit (cond
                        ((null (get-final-states dfa-remainder))
                         '(nil nil))
                        (t
                         (multiple-value-list
                          (find-transit dfa-remainder)))))
             (product (rte-synchronized-product dfas :minimize reduce)))
        (when view
          (ndfa-to-dot product nil :view t :transition-legend t :state-legend t :prefix "product"
                                   :transition-abrevs transition-abrevs
                                   :title "syncronized product"))
        (list unreachable-bodys product transit)))))

(defun rte-case-expander (object-form clauses &key (complain-remainder nil) (view nil))
  (destructuring-bind (unreachable-bodys dfa (remainder remainderp)) (rte-case-clauses-to-dfa clauses :view view)
    (let ((object (gensym "RTE")))
      (cond
        ((and remainderp
              complain-remainder)
         ;; if there is a sequence not covered by this rte-ecase,
         ;; issue a discriptive warning message and recursively expand
         ;; to rte-ecase with a (:* t) clause to force a runtime
         ;; error if called with an otherwise non-matching form.
         (if remainder
             (warn "rte-ecase not exaustive: for example, ~A" remainder)
             (warn "rte-ecase not exaustive: for example, the empty list"))
         `(let ((,object ,object-form))
            (rte-ecase ,object ,@clauses ((:* t) (error "The sequence ~A fell through the RTE-ECASE" ,object)))))
        (t
         (flet ((unreachable-clause (unreachable-body)
                  `(nil ,@unreachable-body)))
           `(let ((,object ,object-form))
              (typecase ,object
                ((not sequence) nil)
                ,@(mapcar #'unreachable-clause unreachable-bodys)
                (t
                 (funcall ,(dump-code dfa :var object)
                          ,object))))))))))

(defmacro rte-ecase (object-form &body clauses)
  "OBJECT-FORM is the form to be evaluated,
CLAUSES is a list of sublists, each sublist can be destructured as: (RATIONAL-TYPE-EXPRESSION &REST BODY)"
  (rte-case-expander object-form clauses :complain-remainder t :view nil))

(defmacro rte-case (object-form &body clauses)
  "OBJECT-FORM is the form to be evaluated,
CLAUSES is a list of sublists, each sublist can be destructured as: (RATIONAL-TYPE-EXPRESSION &REST BODY)"
  (rte-case-expander object-form clauses :complain-remainder nil :view nil))

