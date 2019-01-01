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
   "*TMP-DIR-ROOT*"
   "*DOT-PATH*"
   "BOOLEAN-EXPR-TO-LATEX"
   "CHOOSE-RANDOMLY"
   "COMPARE-OBJECTS"
   "DEF-CACHE-FUN"
   "DOLIST-TCONC"
   "ENCODE-TIME"
   "EXISTS"
   "FIXED-POINT"
   "FORALL"
   "GARBAGE-COLLECT"
   "GETENV"
   "GETTER"
   "GROUP-BY"
   "GROUP-BY-EQUIVALENCE"
   "LCONC"
   "MAKE-TEMP-DIR"
   "MAKE-TEMP-FILE-NAME"
   "MAP-PAIRS"
   "PROCESS-KILL"
   "REPLACE-ALL"
   "RND-ELEMENT"
   "RUN-PROGRAM"
   "SETOF"
   "SHUFFLE-LIST"
   "TCONC"
   "TREE-REDUCE"
   "TYPE-EXPAND"
   "UNIONF"
   "USER-READ"
   "WHILE"
))

(in-package :adjuvant)

(defvar *dot-path* (if (probe-file "/opt/local/bin/dot")
		  "/opt/local/bin/dot"
		  "dot")
  "Full path to the graphviz dot program")


(defun run-program (program args &key (wait t) output error if-output-exists)
  "Wrapper around the implementation dependent (sbcl/Allegro) shell command function"
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
  "Wrapper around the implementation dependent (sbcl/Allegro) UNIX environment variable reader."
  #+sbcl (sb-posix:getenv envvar)
  #+allegro (sys:getenv envvar))

(defun process-kill (process signal)
  "Kill a process started by RUN-PROGRAM, if it was started with (run-program ... :wait t)"
  #+sbcl (sb-ext:process-kill process signal)
  #+allegro (progn (excl.osi:kill process signal)
		   (sys:reap-os-subprocess :pid process)))

(defmacro exists (obj data &body body)
  "Tests whether there exists an element which satisfies an expression.  E.g., (exists x '(1 2 3) (evenp x))"
  (typecase obj
    (list
     (let ((var (gensym "EXISTS")))
       `(member-if (lambda (,var)
                     (destructuring-bind ,obj ,var
                       ,@body)) ,data)))
    (t
     `(member-if (lambda (,obj) ,@body) ,data))))

(defmacro forall (var data &body body)
  "Tests whether all the elements in a given list satisfies an expression.  E.g., (forall x '(2 4 6 8 10) (evenp x))"
  (typecase var
    (list
     (let ((v (gensym "FORALL")))
       `(every #'(lambda (,v)
		   (destructuring-bind ,var ,v
		     ,@body)) ,data)))
    (t
     `(every #'(lambda (,var) ,@body) ,data))))


(defmacro setof (var data &body body)
  "Construct a new list of all the elements which satisfy an expression.  E.g., (setof x '(2 3 5 6 7) (evenp x))"
  (typecase var
    (list
     (let ((v (gensym "SETOF")))
       `(remove-if-not (lambda (,v)
			 (destructuring-bind ,var ,v
			   ,@body)) ,data)))
    (t
     `(remove-if-not (lambda (,var) ,@body) ,data))))


(defmacro while (test &body body)
  "Loop while condition true. E.g., (while (evenp x) (setf x (g x)))"
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

(defun map-tconc (1-ary tconc-buf)
  "non-public helper function for dolist-tconc"
  (let ((buf (car tconc-buf)))
    (while buf
      (funcall 1-ary (car buf))
      (pop buf))))

(defmacro dolist-tconc ((var tconc-buf &optional result-form) &body body)
  "DOLIST is not a dependable way to iterate over the items in a TCONC structure.
USE DOLIST-TCONC instead."
  `(progn (map-tconc (lambda (,var) ,@body) ,tconc-buf)
	  ,result-form))

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


(defun tree-reduce (fold-function object-list &key initial-value (key #'identity))
  "Same semantics as CL:REDUCE, but does the evaluation tree-wise rather than left-to-right.
I.e., it attempts to (+ (+ (+ x0 x1) (+ x2 x3)) (+ (+ x4 x5) (+ x6 x7))),
Of course this is only possible if the number of objects given is a power of 2.
Otherwise, there will be a somewhat lopsided tree.

FOLD-FUNCTION -- associative binary function, this function is called pairwise
   on the accumulated value and the next value from the OBJECT-LIST, after KEY
   has been applied to the next value of the OBJECT-LIST.
   The first call to the FOLD-FUNCTION is on the first two elements of the OBJECT-LIST
   (after application of KEY to both).
INITIAL-VALUE -- the value to return if the OBJECT-LIST is empty, otherwise it is unused,
   this value is returned as-is, and the KEY function is not applied to it.
KEY -- binary function, applied to each element of the OBJECT-LIST before it is passed
   to the FOLD-FUNCTION."
  (declare (type (function (t t) t) fold-function)
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
			  (compactify (cons (list (1+ n1) (funcall fold-function obj1 obj2)) tail))
			  stack))))
	      (finish-stack (acc stack)
		(if stack
		    (finish-stack (funcall fold-function acc (cadr (car stack)))
				  (cdr stack))
		    acc)))
       (destructuring-bind ((_ obj) &rest tail) (reduce (lambda (stack item)
							  (compactify (cons (list 1 (funcall key item)) stack)))
							(cdr object-list)
							:initial-value (list (list 1 (funcall key (car object-list)))))
	 (declare (ignore _))
	 (finish-stack obj tail))))
    (object-list
     (funcall key (car object-list)))
    (t
     initial-value)))
    
(defvar *tmp-dir-root* (ensure-directories-exist (format nil "/tmp/~A/~A/~D/"
                                                         (or (getenv "HOST")
							     #+sbcl (machine-instance)
                                                             "unknown-host")
                                                         (or (getenv "USER")
                                                             "unknown-user")
                                                         (get-universal-time)))
  "Root directory used by MAKE-TEMP-DIR")

(defvar *tmp-dir-count* 0)

(defun make-temp-dir (suffix)
  (format nil "~A~A/~A/" *tmp-dir-root* (incf *tmp-dir-count*) suffix))

(defun make-temp-file-name (base &key (extension nil) (ensure-file-exists nil) (ensure-dir-exists t))
  (let* ((dir-name (make-temp-dir "tmp-dir"))
	 (file-name (format nil "~A/~A~A" dir-name base (if extension (concatenate 'string "." extension) ""))))
    (when ensure-dir-exists
      (ensure-directories-exist dir-name))
    (when ensure-file-exists
      (ensure-directories-exist dir-name)
      (when (probe-file file-name)
	(return-from make-temp-file-name
	  (make-temp-file-name base :extension extension :ensure-file-exists t)))
      (with-open-file (str file-name
			   :if-exists :error
			   :if-does-not-exist :create)
	())
      file-name)))

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

(defun group-by-equivalence (data-list &key (key #'identity) (test #'eql))
  (let (groups-alist)
    (dolist (item data-list)
      (let ((hit (find-if (lambda (items)
			    (funcall test
				     (funcall key (car items))
				     (funcall key item)))
			  groups-alist)))
	(if hit
	    (push item (cdr hit))
	    (push (list item) groups-alist))))
    groups-alist))
  

(defun rnd-element (data n &aux (r (random n)) (tail (nthcdr r data)))
  "DATA list of objects.
N length of DATA.
returns a list of two elements 1) a randomly selected element of DATA
  and 2) a copy of DATA with the element removed, sharing a tail of DATA."
  (list (car tail) (nconc (ldiff data tail) (cdr tail))))

(defun choose-randomly (data n)
  "return a list of N elements from DATA chosen at random, (in random order).
If N > (length of data) then a permutation of DATA is returned"
  (let ((len (length data))
        random-data)
    (dotimes (_ (min n len))
      (destructuring-bind (r tail) (rnd-element data len)
        (setf data tail)
        (push r random-data)
        (decf len)))
    random-data))

(defun shuffle-list (data)
  "Return a new list with the elements of the given list in randomized order."
  (choose-randomly data (length data)))


(defvar *verbose-caching* nil "Verbose mode for CACHING-CALL function")
(defvar *caching-thresh* 2048 "Threshold used for verbose mode of CACHING-CALL function")
(defvar *secret-default-value* (list nil) "Default value from hash table used by CACHING-CALL function")

(defun caching-call (thunk key hash fun-name access increment)
  ;; caching-call symbol not exported
  "Helper function used by the expansion of DEF-CACHE-FUN.  This function
 manages the caching and cache search of the function defined by DEF-CACHE-FUN."
  (declare (type (function () t) thunk)
           (type (function () unsigned-byte) access increment)
           (type symbol fun-name)
           (type list key)
           (type (or null hash-table) hash))
  (cond
    ((null hash)
     (funcall thunk))
    (t
     (when (and *verbose-caching*
                (= 0 (mod (funcall increment) *caching-thresh*)))
       (format t "~D ~A ~A~%" (funcall access) fun-name hash))
     (apply #'values
            ;; (multiple-value-bind (value foundp) (gethash key hash)
            ;;   (cond
            ;;     (foundp value)
            ;;     (t
            ;;      (setf (gethash key hash) (multiple-value-list (funcall thunk))))))

            ;; trying this optimization to see if it is faster.
            (let ((value (gethash key hash *secret-default-value*)))
              (if (eq value *secret-default-value*)
                  (setf (gethash key hash) (multiple-value-list (funcall thunk)))
                  value))

            ))))

(defmacro def-cache-fun ((fun-name with-name
                          &key (hash (gensym "HASH"))
                            (access-count (gensym "COUNT"))
                            (caching-call (intern (concatenate 'string (symbol-name fun-name) "-CACHING-CALL")
                                                  (symbol-package fun-name))))
                         lam-list doc-string  &body body)
  "Define three functions named by FUN-NAME and WITH-NAME a derived name.  The lambda list of the 
 first function is given by LAM-LIST.  The semantics of the first function will be
 to normally simply return the value of BODY.  However, if the call site to the
 first function is within the dymamic extent of the second function, the
 the return value will be cached, and the arguments are found in the cache
 BODY is not evaluated but simply the cached value of the return value will be
 returned."
  (declare (type string doc-string)
           (type symbol fun-name with-name access-count))
  `(progn
     (defvar ,hash nil)
     (defvar ,access-count 0)
     (defun ,caching-call (thunk key hash fun-name access increment)
       "Helper function used by the expansion of DEF-CACHE-FUN.  This function
 manages the caching and cache search of the function defined by DEF-CACHE-FUN."
       (declare (type (function () t) thunk)
                (type (function () unsigned-byte) access increment)
                (type symbol fun-name)
                (type list key)
                (type (or null hash-table) hash))
       (cond
         ((null hash)
          (funcall thunk))
         (t
          (when (and *verbose-caching*
                     (= 0 (mod (funcall increment) *caching-thresh*)))
            (format t "~D ~A ~A~%" (funcall access) fun-name hash))
          (apply #'values
                 ;; (multiple-value-bind (value foundp) (gethash key hash)
                 ;;   (cond
                 ;;     (foundp value)
                 ;;     (t
                 ;;      (setf (gethash key hash) (multiple-value-list (funcall thunk))))))

                 ;; trying this optimization to see if it is faster.
                 (let ((value (gethash key hash *secret-default-value*)))
                   (if (eq value *secret-default-value*)
                       (setf (gethash key hash) (multiple-value-list (funcall thunk)))
                       value))
                 ))))
     (defun ,with-name (thunk)
       (declare (type (function () t) thunk))
       (if (null ,hash)
           (let ((,hash (make-hash-table :test #'equal)))
             (declare (ignorable ,hash))
             (prog1 (funcall thunk)
               (when *verbose-caching*
                 (format t "finished with ~A=~A~%" ',fun-name ,hash))))
           (funcall thunk)))
       
     (defun ,fun-name (&rest arg)
       ,doc-string
       (destructuring-bind ,lam-list arg
         (,caching-call (lambda () ,@body)
                       arg
                       ,hash
                       ',fun-name
                       (lambda () ,access-count)
                       (lambda () (incf ,access-count)))))))


(defun compare-objects (t1 t2)
  "Deterministic compare function:  returns a symbol in (< > =)."
  (cond
    ((equal t1 t2)
     '=)
    ((null t1)
     '<)
    ((null t2)
     '>)
    ((and (listp t1)
          (not (listp t2)))
     '>)
    ((and (listp t2)
          (not (listp t1)))
     '<)
    ((not (eql (class-of t1) (class-of t2))) 
     (compare-objects (class-name (class-of t1)) (class-name (class-of t2))))
    (t
     ;; thus they are the same type, but they are not equal
     (typecase t1
       (list
        (let (value)
          (while (and t1
                      t2
                      (eq '= (setf value (compare-objects (car t1) (car t2)))))
            (pop t1)
            (pop t2))
          (cond
            ((and t1 t2)
             value)
            (t1    '>)
            (t2    '<)
            (t     '=))))
       (symbol
        (cond
          ((not (eql (symbol-package t1) (symbol-package t2)))
           ;; call compare-objects because symbol-package might return nil
           ;;  don't call string= directly
           (compare-objects (symbol-package t1) (symbol-package t2)))
          ((string< t1 t2) ;; same package
           '<)
          (t
           '>)))
       (package
        (compare-objects (package-name t1) (package-name t2)))
       (string
        ;; know they're not equal, thus not string=
        (cond
          ((string< t1 t2)
           '<)
          (t
           '>)))
       (number
        (cond ((< t1 t2)
               '<)
              (t
               '>)))
       (character
	(cond ((char< t1 t2)
	       '<)
	      (t
	       '>)))
       (t
        (error "cannot compare a ~A with a ~A" (class-of t1) (class-of t2)))))))


(defun fixed-point (function arg &key (test #'equal))
  "Find the fixed point of a FUNCTION, starting with the given ARG."
  (declare (type (function (t) t) function))
  (let ((result (funcall function arg)))
	
    (loop :while (not (funcall test result arg))
	  :do (progn (setf arg result)
		     (setf result (funcall function arg))))
    ;; return arg rather than result, they are EQUAL, but this is a
    ;; chance that arg was never changed, thus result may happen to be
    ;; in short term memory and more easily GCed.
    (if (eq test #'equal)
	arg
	result)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))


  
