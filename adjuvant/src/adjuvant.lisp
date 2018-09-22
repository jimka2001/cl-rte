;; Copyright (c) 2018 EPITA Research and Development Laboratory
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



(defpackage :adjuvant
  (:use :cl)
  (:export
   "BOOLEAN-EXPR-TO-LATEX"
   "EXISTS"
   "ENCODE-TIME"
   "*TMP-DIR-ROOT*"
   "MAKE-TEMP-DIR"
   "FORALL"
   "GARBAGE-COLLECT"
   "GETENV"
   "GETTER"
   "GROUP-BY"
   "LCONC"
   "MAP-PAIRS"
   "PROCESS-KILL"
   "RUN-PROGRAM"
   "SETOF"
   "TCONC"
   "TREE-REDUCE"
   "TYPE-EXPAND"
   "UNIONF"
   "USER-READ"
   "WHILE"
))

(in-package :adjuvant)

(defun run-program (program args &key (wait t) output error if-output-exists)
  (declare (type (or null string) output)
	   (type (or null stream) error))
  #+sbcl (sb-ext:run-program program args
			     :search t
			     :wait wait
			     :output output
			     :error error
			     :if-output-exists if-output-exists)
  #+allegro (if (and wait output)
		(with-open-file (s output :direction :output
					  :if-exists if-output-exists)
		  (excl:run-shell-command
		   (apply #'vector (cons program args))
		   :wait wait
		   :error-output error
		   :output s))
		(excl:run-shell-command
		 (apply #'vector (cons program args))
		 :wait wait
		 :error-output error
		 :if-output-exists if-output-exists
		 :output output))
  )

(defun getenv (envvar)
  #+sbcl (sb-posix:getenv envvar)
  #+allegro (sys:getenv envvar))

(defun process-kill (process signal)
  #+sbcl (sb-ext:process-kill process signal)
  #+allegro (progn (excl.osi:kill process signal)
		   (sys:reap-os-subprocess :pid process)))

(defmacro exists (obj data &body body)
  (typecase obj
    (list
     (let ((var (gensym "exists")))
       `(member-if (lambda (,var)
                     (destructuring-bind ,obj ,var
                       ,@body)) ,data)))
    (t
     `(member-if (lambda (,obj) ,@body) ,data))))

(defmacro forall (var data &body body)
  (typecase var
    (list
     (let ((v (gensym "forall")))
       `(every #'(lambda (,v)
		   (destructuring-bind ,var ,v
		     ,@body)) ,data)))
    (t
     `(every #'(lambda (,var) ,@body) ,data))))


(defmacro setof (var data &body body)
  (typecase var
    (list
     (let ((v (gensym "setof")))
       `(remove-if-not (lambda (,v)
			 (destructuring-bind ,var ,v
			   ,@body)) ,data)))
    (t
     `(remove-if-not (lambda (,var) ,@body) ,data))))


(defmacro while (test &body body)
  `(loop :while ,test
	 :do (progn ,@body)))

(defun getter (field)
  (lambda (obj) (getf obj field)))

(defun user-read (&rest args)
  "Calls read with the specified ARGS, but with *PACKAGE* bound to the CL-USER package.  
The effect of this is that symbols like NIL and - get read as COMMON-LISP:NIL and COMMON-LISP:- rather 
than as keywords."
  (let ((*package* (find-package :cl-user)))
    (apply #'read args)))

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

(defun type-expand (type)
  "Expand a type, similar to macro-expand, if the given type specifier is not a user defined type, then an EQUAL type is returned."
  #+sbcl (sb-ext:typexpand type)
  #+allegro (excl::deftype-expand type))

(defun group-by (sequence &key (key #'identity) (test #'eql))
  (declare (type sequence sequence)
           (type (function (t) t) key)
           (type (function (t t) t) test))
  (let (alist)
    (map nil (lambda (item &aux (index (funcall key item)) (hit (assoc index alist :test test)))
	       (if hit
		   (push item (car (cdr hit)))
		   (push (list index (list item)) alist)))
	 sequence)
    alist))

(define-modify-macro unionf (&rest args) union)


(defun tree-reduce (function object-list &key initial-value (key #'identity))
  (declare (type (function (t t) t) function)
	   (type (function (t) t) key)
	   (type list object-list)
	   (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (cond
    ((cdr object-list)
     (labels ((compactify (stack)
		(if (null (cdr stack))
		    stack
		    (destructuring-bind ((n1 obj1) (n2 obj2) &rest tail) stack
		      (declare (type (and fixnum unsigned-byte) n1 n2))
		      (if (= n1 n2)
			  (compactify (cons (list (1+ n1) (funcall function obj1 obj2)) tail))
			  stack))))
	      (finish-stack (acc stack)
		(if stack
		    (finish-stack (funcall function acc (cadr (car stack)))
				  (cdr stack))
		    acc)))
       (destructuring-bind ((_ obj) &rest tail) (reduce (lambda (stack item)
							  (compactify (cons (list 1 (funcall key item)) stack)))
							(cdr object-list)
							:initial-value (list (list 1 (funcall key (car object-list)))))
	 (declare (ignore _))
	 (finish-stack obj tail))))
    (object-list
     (car object-list))
    (t
     initial-value)))
    
(defvar *tmp-dir-root* (format nil "/tmp/~A/" (or (getenv "USER")
						  "unknown-user")))

(defun make-temp-dir (suffix)
  (format nil "~A/~A/" *tmp-dir-root* suffix))

(defun boolean-expr-to-latex (expr &optional (stream t))
  (etypecase expr
    ((eql nil)
     (format stream "\\bot"))
    ((eql t)
     (format stream "\\top"))
    ((not list)
     (format stream "~A" expr))
    ((cons (eql and))
          (format stream "(")
     (boolean-expr-to-latex (cadr expr) stream)
     (dolist (subexpr (cddr expr))
       (format stream " \\wedge ")
       (boolean-expr-to-latex subexpr stream))
     (format stream ")")
     )
    ((cons (eql or))
     (format stream "(")
     (boolean-expr-to-latex (cadr expr) stream)
     (dolist (subexpr (cddr expr))
       (format stream " \\vee ")
       (boolean-expr-to-latex subexpr stream))
     (format stream ")")
     )
    ((cons (eql not))
     (format stream "\\neg ")
     (boolean-expr-to-latex (cadr expr) stream))))

(defun garbage-collect ()
  #+sbcl (sb-ext::gc :full t)
  #+allegro (excl:gc t)
)

(defun encode-time (&optional (time (get-universal-time)) &aux (decoded-time (multiple-value-list (decode-universal-time time))))
  "Create a string similar to the UNIX date command: e.g., \"Thu Aug  3 10:39:18 2017\""
  (destructuring-bind (second minute hour date month year day-of-week ;; (0 = Monday)
                       daylight-savings-times ;; T (daylight savings times) or NIL (standard time)
                       timezone) decoded-time
    (declare (ignore timezone daylight-savings-times))
    (let ((day-of-week (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week))
          (month (aref #("no-month" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)))
      (with-output-to-string (str)
        (format str "~A ~A" day-of-week month)
        (format str " ~2D ~2D:~2,'0D:~2,'0D ~S" date hour minute second year)))))

(defun map-pairs (binary data-list)
  "Call the given binary function once on each pair of objects from the given data-list."
  (mapl (lambda (tail &aux (head (car tail)))
	  (dolist (item (cdr tail))
	    (funcall binary head item)))
	data-list))
