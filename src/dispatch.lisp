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

(defpackage :dispatch
  (:export)
  (:use :cl :closer-mop)
  (:shadowing-import-from :closer-mop
			  "STANDARD-GENERIC-FUNCTION"
			  "DEFMETHOD"
			  "DEFGENERIC"))

(in-package :dispatch)

(defun make-assoc-mapping (sequence key &key (test #'eql))
  (let (alist)
    (flet ((file (item &aux (index (funcall key item)) (hit (assoc index alist :test test)))
	     (if hit
		 (push item (cdr hit))
		 (push (list index item) alist))))
      (map nil #'file sequence))
    alist))

(defun standard-method-combination-p (comb)
  (eql comb
       (generic-function-method-combination (fdefinition 'make-assoc-method-qualifiers))))

(defgeneric make-assoc-method-qualifiers (gf))
(defmethod make-assoc-method-qualifiers ((gf symbol))
  (when (fboundp gf)
    (make-assoc-method-qualifiers (fdefinition gf))))
(defmethod make-assoc-method-qualifiers ((gf cl:standard-generic-function))
  (make-assoc-mapping (generic-function-methods gf)
		      #'method-qualifiers
		      :test #'equal))


(defun map-pairs (binary data-list)
  "Call the given binary function once on each pair of objects from the given data-list."
  (mapl (lambda (tail &aux (head (car tail)))
	  (dolist (item (cdr tail))
	    (funcall binary head item)))
	data-list))

(defun map-common-subtypes (unary class1 class2)
  ())

(defmacro setof (var data &body body)
  `(remove-if-not #'(lambda (,var) ,@body) ,data))

(defmacro exists (var data &body body)
  `(member-if #'(lambda (,var) ,@body) ,data))

(defmacro forall (vars data &body body)
  (if (symbolp vars)
      `(forall (,vars) (,data) ,@body)
      `(every (lambda ,vars ,@body) ,@data)))


(defun lconc (buf items)
  (cond
    ((null buf)
     (cons items (last items)))
    ((null (car buf))
     (setf (car buf) items)
     (setf (cdr buf) (last items))
     buf)
    ((null items)
     buf)
    (t
     (setf (cdr (cdr buf)) items)
     (setf (cdr buf) (last items))
     buf)))

(defun tconc (buf &rest items)
  (lconc buf items))

(defun getter (key)
  #'(lambda (obj)
      (getf obj key)))

(define-modify-macro orf (&rest args) or)

(defgeneric specializer-intersections (spec1 spec2)
  (:documentation
   "Compute and return a list of all the specializers (classes for example) which
inherit from both spec1 and spec2, but disregarding specializers which inherit
from another one of the calculated specializers.  E.g., If spec1=<class A> and
spec2=<class B>, then calculate the list of all classes inheriting from
both A and B.  But if C and D both inherit from A and B, but C also inherits from D
then omit C in the return list."))

(defmethod specializer-intersections ((class1 class) (class2 class))
  ;; breadth first search down the class hierarchy towards nil
  (let ((queue (tconc nil
		      (list :class class1 :1 t   :2 nil :supers nil)
		      (list :class class2 :1 nil :2 t   :supers nil))))
    (dolist (node (car queue))
      (dolist (sub (class-direct-subclasses (getf node :class)))
	(let ((found (find sub (car queue) :key (getter :class))))
	  (cond
	    (found
	     (pushnew node (getf found :supers))
	     (orf (getf found :1) (getf node :1))
	     (orf (getf found :2) (getf node :2)))
	    (t
	     (let ((new (list :class sub
			      :1 (getf node :1)
			      :2 (getf node :2)
			      :supers (list node))))
	       (tconc queue new)))))))
    (mapcar (getter :class)
	    ;; find all nodes which are descendants of both class1 and
	    ;; class2, but which have no parent node which are also
	    ;; both descendants thereof.
	    (setof node (car queue)
	      (and (getf node :1)
		   (getf node :2)
		   (not (exists super-node (getf node :supers)
			  (and (getf super-node :1)
			       (getf super-node :2)))))))))

(defmethod specializer-intersections ((eql1 eql-specializer) (class2 class))
  (when (typep (eql-specializer-object eql1) class2)
    (list eql1)))

(defmethod specializer-intersections ((class1 class) (eql2 eql-specializer))
  (when (typep (eql-specializer-object eql2) class1)
    (list eql2)))

(defmethod specializer-intersections ((eql1 eql-specializer) (eql2 eql-specializer))
  (when (or (eql eql1
		 eql2)
	    (eql (eql-specializer-object eql1)
		 (eql-specializer-object eql2)))
    (list eql1)))

(defun unique-least-element (types methods)
  (null (cdr (least-elements types methods))))

(defun least-elements (intersection-types methods)
  (labels ((spec>= (obj-spec meth-spec)
	     (cond
	       ((and (typep obj-spec 'class)
		     (typep meth-spec 'class))
		(member meth-spec (compute-class-precedence-list obj-spec)))
	       ((and (typep obj-spec 'eql-specializer)
		     (typep meth-spec 'eql-specializer))
		(eql (eql-specializer-object obj-spec)
		     (eql-specializer-object meth-spec)))
	       ((and (typep obj-spec 'eql-specializer)
		     (typep meth-spec 'class))
		(typep (eql-specializer-object obj-spec) meth-spec))
	       ((and (typep obj-spec 'class)
		     (typep meth-spec 'eql-specializer))
		nil)))
	   (applicable-methods ()
	     (setof method methods
	       (every #'spec>= intersection-types (method-specializers method))))
	   (method<= (m1 m2)
	     (forall (spec1 spec2) ((method-specializers m1) (method-specializers m2))
	       (spec>= spec2 spec1))))
    (let ((applicable-methods (applicable-methods)))
      (declare (notinline set-difference))
      (set-difference applicable-methods
		      (setof m2 applicable-methods
			(exists m1 methods
			  (and (not (eql m1 m2))
			       (method<= m1 m2))))))))

(defun map-intersections (unary sub-lists)
  "Call the UNARY function once for each constructed list Q where the nth element of Q
is selected from the nth element of sub-lists."
  (labels ((work (sub-lists stack)
	     (dolist (e (car sub-lists))
	       (if (cdr sub-lists)
		   (work (cdr sub-lists) (cons e stack))
		   (funcall unary (cons e stack))))))
    (work (reverse sub-lists) nil)))

(defun find-method-ambiguities (gf)
  (let ((gf (typecase gf
	      ((and symbol (satisfies fboundp))
	       (fdefinition gf))
	      (cl::standard-generic-function
	       gf)
	      (t
	       (error "invalid generic function ~A" gf)))))
    (cond
      ((null (cdr (generic-function-argument-precedence-order gf)))
       ;; no need to check if there is only one (or no) required argument
       nil)
      ((null (standard-method-combination-p (generic-function-method-combination gf)))
       ;; don't try to examine unless this gf uses the standard method combo
       nil)
      (t
       (let (ambiguities)
	 (dolist (qualifiers-methods (make-assoc-method-qualifiers gf))
	   (destructuring-bind (qualifiers &rest methods) qualifiers-methods
	     (map-pairs (lambda (meth1 meth2
				 &aux (type-intersections (mapcar #'specializer-intersections
								  (method-specializers meth1)
								  (method-specializers meth2))))
			  ;; type-intersections is a list of lists where the n-th element
			  ;;  is a list of intersecting types of the nth specializer of meth1
			  ;;  with the nth specializer of meth2
			  (unless (member nil type-intersections)
			    (map-intersections (lambda (type-intersection)
						 (cond
						   ((member nil type-intersection)
						    nil)
						   ((unique-least-element type-intersection methods)
						    nil)
						   (t
						    (push (list :qualifiers qualifiers
								:methods (list meth1 meth2)
								:arg-types type-intersection)
							  ambiguities))))
					       type-intersections)))
			methods)))
	 ambiguities)))))

(defgeneric check-specializers (obj))

(defmethod check-specializers ((package-name symbol))
  (when (find-package package-name)
    (check-specializers (find-package package-name))))
  
(defmethod check-specializers ((package package))
  (let ((buf (list nil)))
    (do-symbols (name package)
      (when (and (fboundp name)
		 (fdefinition name)
		 (typep (fdefinition name) 'cl::standard-generic-function))
	(format t "checking ~A~%" name)
	(lconc buf (find-method-ambiguities (fdefinition name)))))
    (car buf)))
