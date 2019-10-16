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

(defclass strategy-inline (dump-code-strategy)
  ())

(defgeneric goto-next-state (strategy state-name))

(defgeneric format-state-dispatch (strategy initial-state-name dumped-states))

(defgeneric dump-state (strategy state-name dumped-case))

(defgeneric state-assoc (strategy states exit-form-p))

(defmethod dump-code ((ndfa rte-state-machine) (strategy strategy-inline) &key (var 'seq))
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
                                   (t nil)))
                               (get-final-states ndfa)))
         (state-assoc (state-assoc strategy exit-form-p states))
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
                 ,(goto-next-state strategy (state-name (ndfa:next-state transition)))))
             (dump-case-transition (transition)
               (declare (type ndfa::transition transition))
               (assert (typep (ndfa:next-state transition) 'ndfa::state))
               (assert (typep (state-name (ndfa:next-state transition)) '(not null)))
               `(,(cdr (transition-label transition))
                 ,(goto-next-state strategy (state-name (ndfa:next-state transition)))))
             (dump-end (state end handle-case)
               (cond ((null (state-final-p state))
                      `(if ,end
                           (return-from ,check nil)
                           ,handle-case))
                     ((state-sticky-p state)
                      `(return-from ,check ,(state-exit-form state)))
                     (t
                      `(if ,end
                           (return-from ,check ,(state-exit-form state))
                           ,handle-case))))
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

             (dump-tagbody (end final-next)
               (cond
                 ((get-initial-states ndfa)
                  (assert (= 1 (length (get-initial-states ndfa))))
                  (assert (typep (car (get-initial-states ndfa)) 'ndfa::state))
                  (format-state-dispatch strategy
                                         (state-name (car (get-initial-states ndfa)))
                                         (mapcan #'(lambda (state)
                                                     (dump-state strategy
                                                                 (state-name state)
                                                                 (dump-end state end
                                                                           (dump-case state final-next))
                                                                 ))
                                                 states)))
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
              nil))))))
  )
