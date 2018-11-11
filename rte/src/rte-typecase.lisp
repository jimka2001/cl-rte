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
	dfas)
    (flet ((transform-clause (clause)
	     (destructuring-bind (pattern &rest body) clause
	       (let ((derived-pattern `(:and ,pattern (:not (:or ,@previous-patterns)))))
		 (push pattern previous-patterns)
		 (if (equivalent-patterns :empty-set derived-pattern)
		     (push body unreachable-bodys)
		     (let ((dfa (rte-to-dfa derived-pattern :reduce t :final-body `(progn ,@body))))
		       ;; (ndfa-to-dot dfa nil :view t :transition-legend nil :state-legend t :prefix "derived-clause"
		       ;; 			    :title (format nil "~s" derived-pattern))
		       (push dfa dfas)))))))
      (dolist (clause clauses)
	(transform-clause clause))
      (list unreachable-bodys dfas (rte-synchronized-product dfas)))))

(defmacro rte-typecase (object-form &body clauses)
  "OBJECT-FORM is the form to be evaluated,
CLAUSES is a list of sublists, each sublist can be destructured as: (RATIONAL-TYPE-EXPRESSION &REST BODY)"
  (destructuring-bind (unreachable-bodys _ dfa) (rte-typecase-helper clauses)
    (declare (ignore _))
    ;; (ndfa-to-dot dfa nil :view t :transition-legend nil :state-legend t :prefix "sync-product" :title "syncronized product")
    (flet ((unreachable-clause (unreachable-body)
	     `(nil ,@unreachable-body)))
      (let ((object (gensym)))
	`(let ((,object ,object-form))
	   (typecase ,object
	     ((not sequence) nil)
	     ,@(mapcar #'unreachable-clause unreachable-bodys)
	     (t
	      (funcall ,(dump-code dfa :var object)
		       ,object))))))))

