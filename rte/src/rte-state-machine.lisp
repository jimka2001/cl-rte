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
