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

(defun rte-synchronized-product (dfas &key (boolean-function (lambda (a b)
							       (or a b))))
  (declare (type (function (t t) t) boolean-function)
	   (type list dfas)
	   (optimize (speed 3) (debug 0) (compilation-speed 0)))	   
  (tree-reduce #'(lambda (dfa1 dfa2)
		   (declare (type rte-state-machine dfa1 dfa2))
		   (synchronized-product dfa1 dfa2
					      :boolean-function boolean-function))
	       dfas :initial-value (rte-to-dfa :empty-set)))

(defun rte-typecase-helper (clauses)
  "Helper function for rte-typecase."
  (declare (type list clauses)
	   (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (let (previous-patterns
	unreachable-bodys
	(clause-index 0)
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
		       (let ((dfa (rte-to-dfa pattern :reduce nil :final-body (exit-form body) :clause-index (incf clause-index))))
			 ;; (ndfa-to-dot dfa nil :view t :transition-legend nil :state-legend t :prefix "derived-clause"
			 ;; 			    :title (format nil "~s" derived-pattern))
			 (push dfa dfas)))))))
      (dolist (clause clauses)
	(transform-clause clause))
      (let* ((dfa-remainder (rte-to-dfa `(:and (:* t) (:not (:or ,@previous-patterns))) :reduce t))
	     (transit (cond
			((null (get-final-states dfa-remainder))
			 '(nil nil))
			(t
			 (multiple-value-list
                          (find-transit dfa-remainder))))))
	(list unreachable-bodys (rte-synchronized-product dfas) transit)))))

(defmacro rte-etypecase (object-form &body clauses)
  "OBJECT-FORM is the form to be evaluated,
CLAUSES is a list of sublists, each sublist can be destructured as: (RATIONAL-TYPE-EXPRESSION &REST BODY)"
  (destructuring-bind (unreachable-bodys dfa (remainder remainderp)) (rte-typecase-helper clauses)
    (let ((object (gensym "RTE")))
      (cond
	(remainderp
	 ;; TODO -- we don't really want to issue this warning for rte-etypecase, it is not clear
	 ;;  when it should be issued
       
	 ;; if there is a sequence not covered by this typecase, issue a discriptive warning message
         (if remainder
             (warn "rte-etypecase not exaustive: for example, ~A" remainder)
             (warn "rte-etypecase not exaustive: for example, the empty list"))
	 `(let ((,object ,object-form))
	    (rte-etypecase ,object ,@clauses ((:* t) (error "The sequence ~A fell through the RTE-ETYPECASE" ,object)))))
	(t
	 ;; (ndfa-to-dot dfa nil :view t :transition-legend nil :state-legend t :prefix "sync-product" :title "syncronized product")
	 (flet ((unreachable-clause (unreachable-body)
		  `(nil ,@unreachable-body)))
	   `(let ((,object ,object-form))
	      (typecase ,object
		((not sequence) nil)
		,@(mapcar #'unreachable-clause unreachable-bodys)
		(t
		 (funcall ,(dump-code dfa :var object)
			  ,object))))))))))

(defmacro rte-typecase (object-form &body clauses)
  "OBJECT-FORM is the form to be evaluated,
CLAUSES is a list of sublists, each sublist can be destructured as: (RATIONAL-TYPE-EXPRESSION &REST BODY)"
  `(rte-etypecase ,object-form ,@clauses ((:* t) nil)))

