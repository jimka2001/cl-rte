;; Copyright (c) 2018,19 EPITA Research and Development Laboratory
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

(in-package :adjuvant)

(defvar *dot-path* (or (find-if #'probe-file '("/opt/local/bin/dot"
                                               "/usr/local/bin/dot"))
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

(defun demand-env-var (env-var-name)
  "Signal an error if the named environment variable is missing, otherwise return its value."
  (or (getenv env-var-name)
      (error "Missing env var ~s" env-var-name)))

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

(defmacro exists-tail (var list &body body)
  "Return the first tail of the given LIST for which BODY evaluates to true.   The given VAR
is bound in turn to each cons cell list, as if by CL:MAPL, until one is found which verifies the BODY."
  (let ((name (gensym)))
    `(block ,name
       (mapl #'(lambda (,var)
		 (when (progn ,@body)
		   (return-from ,name ,var)))
	     ,list)
       nil)))

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

(defmacro remfq (obj place)
  "remove (with CL:REMOVE) element from place destructivly using EQ for equivalence."
  `(setf ,place (remove ,obj ,place :test #'eq)))

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
"Create a car/cadr alist by applying a key function to every element of a sequence
 E.g., to group the lists in an array by length.
  PKG> (group-by #((1) (1 2) (3) (1 2 3) (3 4)) :key #'length)
  ==> ((3 ((1 2 3)))
       (2 ((3 4) (1 2)))
       (1 ((3) (1))))
 E.g., to group strings together in `string-equal` case-independent equal lists.
  PKG> (group-by list-of-strings :key #'identity :test #'string-equal)
 If there are duplicates in the input list, there will be duplicates in some the output
 lists. I.e., the alist is formed as if by push, not pushnew, so there is no way
 to specify an equivalence function for the values."
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

(defun linear-reduce (function sequence &key key from-end (start 0) end initial-value stop-when)
  "The same as CL:REDUCE, except that LINEAR-REDUCE allows an extra keyword argument, :STOP-WHEN
 which it ignores.   This is so LINEAR-REDUCE may be used as a drop-in replacement for TREE-REDUCE
 for the purpose of testing and profiling."
  (declare (ignore stop-when))
  ;; remove :stop-when from argument list
  (reduce function sequence :key key :from-end from-end :start start :end end :initial-value initial-value))

(defvar *stop-when* (list nil))
(defun tree-reduce (fold-function object-list &key initial-value (key #'identity) (stop-when *stop-when*))
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
   to the FOLD-FUNCTION.
 STOP-WHEN -- if this value occurs either in the object-list (after applying the KEY function)
   or as an accumulated value, it is returned immediately, and the iteration is aborted.  
   This term is used for example when multiplying numbers, when 0 occurs, it is returned
   because multiplying the rest of the list would just be wasted time."
  (declare (type (function (t t) t) fold-function)
	   (type (function (t) t) key)
	   (type list object-list)
	   (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (cond
    ((cdr object-list)
     (labels ((local-key (obj &aux (value (funcall key obj)))
                (if (eql value stop-when)
                    (return-from tree-reduce stop-when)
                    value))
              (dwindle-tree (stack)
		(if (null (cdr stack))
		    stack
		    (destructuring-bind ((n1 obj1) (n2 obj2) &rest tail) stack
		      (declare (type (unsigned-byte 16) n1 n2))
                      (cond
                        ((= n1 n2)
                         (let ((value (funcall fold-function obj2 obj1)))
                           (if (eql value stop-when)
                               (return-from tree-reduce stop-when)
                               (dwindle-tree (cons (list (1+ n1) value) tail)))))
                        (t
                         stack)))))
	      (finish-stack (acc stack)
                (when (eql acc stop-when)
                  (return-from tree-reduce stop-when))
		(if stack
		    (finish-stack (funcall fold-function (cadr (car stack)) acc)
				  (cdr stack))
		    acc)))
       (destructuring-bind ((_ obj) &rest tail)
           (reduce (lambda (stack item)
                     (dwindle-tree (cons (list 1 (local-key item)) stack)))
                   (cdr object-list)
                   :initial-value (list (list 1 (local-key (car object-list)))))
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

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement. E.g.,
  (replace-all (format nil \"~A/~A.~A\" dir-name base extension)
                                \"//\" \"/\")"
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


(defun make-temp-file-name (base &key (dir-name (make-temp-dir "tmp-dir")) (extension nil) (ensure-file-exists nil) (ensure-dir-exists t))
  "Make a temporary file name.
If :DIR-NAME is not given, a new filename (string) will be returned,  however,
if :DIR-NAME is given, then a string will be returned which is a function of BASE, DIR-NAME, EXTENSION;
   thus if the same values are used on a subsequent function call, the same string will be returned.
:ENSURE-FILE-EXISTS (default false) will create an empty file of this name.
:ENSURE-DIR-EXISTS (default true) will create the directory if necessary."
  (let ((file-name (replace-all (format nil "~A/~A~A" dir-name base (if extension (concatenate 'string "." extension) ""))
                                "//" "/")))
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
	()))
    file-name))

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
           ;; make this a weak hash table.  When using a strong hash
           ;; table sometimes memory is exhausted on large runs.
           (let ((,hash (make-hash-table :test #'equal
                               #+sbcl :weakness #+sbcl :key-or-value
                               #+allegro :values #+allegro :weak)))
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

(defun map-subsets (visitor data)
  "call the given VISITOR function once for each subset of the list DATA"
  (dotimes (n (expt 2 (length data)))
    (let ((n n)
          (data data)
          subset)
      (loop :while (and data (plusp n))
            :do (progn (when (oddp n)
                         (push (car data) subset))
                       (pop data)
                       (setf n (truncate n 2))))
      (funcall visitor subset))))

(defun map-permutations (visit data)
  "call the given VISITOR function once for each permutation of the given list DATA"
  (declare (type list data)
           (type (function (list) t)))
  (let ((N (length data)))
    (labels ((next (so-far k)
               (cond
                 ((eql k N)
                  (funcall visit (mapcar #'car so-far)))
                 (t
                  (incf k)
                  (loop :for items :on data
                        :do (unless (member items so-far :test #'eq)
                              (next (cons items so-far) k)))))))
      (next nil 0))))

(defun find-duplicates (data &key (test #'eql) (key #'identity))
  "return a uniquified list of elements, each of which appear more than once in the given DATA list"
  (remove-duplicates
   (mapcon (lambda (tail &aux (item (car tail)))
	      (when (member item (cdr tail) :test test :key key)
		(list item)))
	    data)
   :test test
   :key key))

(defun diff-files (file1 file2)
  "Given two file names, acceptable as 2nd argument of CL:WITH-OPEN-FILE, return TRUE
if the files differ and return FALSE if they are the same.
Same ==> FALSE
Different ==> TRUE
Same vs different are judged by the content.  If the files are different lengths
the files are different, otherwise when reading the files in parallel one character at
a time using CL:READ-CHAR, if all each sequence of characters are the same according to
EQL, then the files are judged to be the same."
  (with-open-file (s1 file1 :direction :input :if-does-not-exist :error :element-type '(unsigned-byte 8))
    (with-open-file (s2 file2 :direction :input :if-does-not-exist :error :element-type '(unsigned-byte 8))
      (while t
        (let ((c1 (read-byte s1 nil s1))
              (c2 (read-byte s2 nil s2)))
          (cond
            ((and (eql c1 s1)
                  (eql c2 s2))
             ;; reached eof at same time
             (return-from diff-files nil))
            ((or (eql c1 s1)
                 (eql c2 s2))
             ;; one reached eof but other didnt
             (return-from diff-files t))
            ((not (eql c1 c2))
             ;; read different character
             (return-from diff-files t))))))))

(defmacro prog1-let ((var &optional expr) &body body)
  "This macro declares the given variable, and returns its value after the body has been evaluated. E.g.,
 (prog1-let (A 100)
   ...)
 This expression binds A to 100 and then evaluates the body.  It returns 100
 unless the body modifies the value of A, otherwise that new value of A is
 returned."
  `(let ((,var ,expr))
     ,@body
     ,var))

(defun topological-sort (constraints &key
                                       (if-cycle (lambda (list-so-far remaining-constraints)
                                                   (error "Cannot topologically sort cyclic graph: list-so-far=~A remaining-constraints=~A"
                                                          list-so-far remaining-constraints)))
                                       (tie-breaker (lambda (candidates list-so-far)
                                                      (declare (ignore list-so-far))
                                                      (car candidates)))
                                       (test #'eql))
  "CONSTRAINTS is a car/cdr association list whose keys are objects and whose
 values are lists of objects on which the corresponding key depends.
 CONSTRAINTS is of the form ((early later later ...)
                             (early later later ...)
                             ...)
 This is an implementation of the Kahn algorithm for topological sort, but with a tie
 breaker function.  The tie breaker is allowed to examine the candidates, which each have
 their constraints met, but no remaining constraint.  The tie breaker also has access
 to the head of the list, ie., all the verticies which preceed the candidate vertices
 in question.  The tie-breaker function must select one of the candidate vertices and
 return it."
  (declare (type list constraints)
           (type (function (list list) t) tie-breaker))
  (let* ((sorted_conc (list nil nil))
         (remaining-constraints (mapcan (destructuring-lambda ((before &rest afters))
                                          (mapcar (lambda (after)
                                                    (list before after)) afters))
                                        constraints))
         (vertices (make-hash-table :test test)))
    ;; populate the vertices hash table
    (destructuring-dolist ((a &rest bs) constraints)
      (setf (gethash a vertices) t)
      (dolist (b bs)
        (setf (gethash b vertices) t)))
    (labels ((test (a b)
               (funcall test a b))
             (tie-breaker (candidates list-so-far)
               (funcall tie-breaker candidates list-so-far))
             (unconstrained ()
               (loop :for v :being :the :hash-keys :of vertices
                  :unless (exists edge constraints
                                  (test v (cadr edge)))
                  :collect v))
             (collect (n s)
               (setf s (remove n s :test test))
               (let ((edges-to-remove (setof edge remaining-constraints
                                             (test n (car edge)))))
                 (tconc sorted_conc n)
                 (remhash n vertices)
                 (setf remaining-constraints
                       (set-difference remaining-constraints edges-to-remove
                                       :test (lambda (edge1 edge2)
                                               (every test edge1 edge2))))
                 (destructuring-dolist ((_ m) edges-to-remove)
                                       (declare (ignore _))
                                       (unless (find m remaining-constraints :key #'cadr :test test)
                                         (push m s)))
                 (recure s)))
             (recure (s)
               (cond
                 ((cdr s)
                  ;; ambiguous choice
                  (collect (tie-breaker s (car sorted_conc)) s))
                 (s
                  ;; unique choice
                  (collect (car s) s))
                 ((zerop (hash-table-count vertices))
                  (car sorted_conc))
                 (t
                  (funcall if-cycle (car sorted_conc) remaining-constraints)))))
      (recure (unconstrained)))))

(defun empty-file-p (fname)
  "Predicate to determine whether a file is empty.  A limitation of the CL:OPEN function
is that this only works if the user has read permission on the file.  This is unfortunate
as it is not a UNIX limitation."
  (with-open-file (stream fname :direction :input
                                :element-type 'unsigned-byte
                                :if-does-not-exist nil)
    (and stream
	 (zerop (file-length stream)))))

(defun valid-type-p (type-designator)
  "Predicate to determine whether the given object is a valid type specifier."
  #+sbcl (handler-case (and (SB-EXT:VALID-TYPE-SPECIFIER-P type-designator)
                            (not (eq type-designator 'cl:*)))
           (SB-KERNEL::PARSE-UNKNOWN-TYPE (c) (declare (ignore c)) nil))
  #+(or clisp  allegro) (ignore-errors (subtypep type-designator t))
  #-(or sbcl clisp allegro) (error "VALID-TYEP-P not implemented for ~A" (lisp-implementation-type))
)

(defun change-extension (filename new-extension)
  "change file name extension:
E.g., (change-extension \"/path/to/file.gnu\" \"png\") --> \"/path/to/file.png\""
  (let ((index (search "." filename :from-end t)))
    (when index
      (let ((head (subseq filename 0 index)))
        (concatenate 'string head "." new-extension)))))

(defun insert-suffix (filename suffix)
  "insert the given SUFFIX before the filename extension: 
 E.g., (insert-suffix \"/path/to/file.gnu\" \"-smooth\") --> \"/path/to/file-smooth.gnu\""
  ;; find the final "."
  (let ((index (search "." filename :from-end t)))
    (when index
      (let ((tail (subseq filename index))
            (head (subseq filename 0 index)))
        (concatenate 'string head suffix tail)))))

(defun chop-pathname (filename)
"Return the file name portion of a string, copping off the leading directory name
E.g.,  (chop-pathname \"/full/path/name/to/file.extension\") --> \"file.extension\""
  (let ((slash (search "/" filename :from-end t)))
    (cond
      ((null slash)
       filename)
      (t
       (subseq filename (1+ slash))))))


(defun edges-to-adjacency-hash (edge-list &key (test #'eql))
"Convert a given list of pairs, EDGE-LIST, of verticies, into an adjacency-hash for
  use in function BFS-GRAPH.
 TEST is an equivalence test to compare graph vertices for equivalence."
  (prog1-let (adjacency-hash (make-hash-table :test test))
    (destructuring-dolist ((vertex-from pairs) (group-by edge-list :key #'car :test #'equal))
      (setf (gethash vertex-from adjacency-hash) (mapcar #'cadr pairs)))))

(defun bfs-graph (start-vertex adjacency-hash visitor &key (test #'eql))
  "Visit every vertex of a graph exactly once, in breadth-first order,
 calling VISITOR on each vertex.  VISITOR is called with two arguments, the
 vertex and its parent, with parent being the same as the vertex in the particular case
 of the first vertex examined.
 ADJACENCY-HASH is a hash table (whose equivalence test is TEST), which maps
 each vertex to the list of adjacent vertices.
 TEST is an equivalence test to compare graph vertices for equivalence."
  (declare (type hash-table adjacency-hash)
           (type (function (t t) t) test))
  (let ((conc-buf (tconc nil start-vertex))
        (done (make-hash-table :test test)))
    (funcall visitor start-vertex start-vertex)
    (setf (gethash start-vertex done) t)
    (dolist-tconc (vertex-from conc-buf)
      (dolist (vertex-to (gethash vertex-from adjacency-hash))
        (unless (gethash vertex-to done)
          (funcall visitor vertex-to vertex-from)
          (setf (gethash vertex-to done) t)
          (tconc conc-buf vertex-to))))))

(defun count-1-bits (n &aux (bits 0))
  "Given an non-negative integer, count the number of 1 bits in its binary representation."
  (declare (optimize (speed 3) (debug 0))
           (type (and unsigned-byte fixnum) bits)
           (type unsigned-byte n))
  (while (not (eql 0 n))
    (when (oddp n)
      (incf bits))
    (setf n (ash n -1)))
  bits)

(defun count-bit-diffs (a b)
  "Given two unsigned integers, count the numbers of bits their binary representations differ by."
  (declare (type unsigned-byte a b) ; warning maybe bignums
           (optimize (speed 3) (debug 0)))
  (count-1-bits (boole boole-xor a b)))

(defun sort-unique (data predicate-< predicate-=)
  "Sort a list and remove duplicate elements.  This is
 more efficient than sorting and calling CL:REMOVE-DUPLICATES,
 because duplicates can be removed in linear time when it is known
 that the list is already sorted. E.g.,

 (sort-unique '(1 4 3 5 3 4 1) #'< #'=)
 ==> '(1 3 4 5)"
  (let ((sorted (sort data predicate-<)))
    (labels ((unique-sorted (data acc)
               (cond
                 ((null data)
                  (nreverse acc))
                 ((null (cdr data))
                  (unique-sorted nil (cons (car data) acc)))
                 ((funcall predicate-= (car data) (cadr data))
                  (unique-sorted (cdr data) acc))
                 (t
                  (unique-sorted (cdr data) (cons (car data) acc))))))
      (unique-sorted sorted nil))))
         


(defvar *gnuplot-path* (or (find-if #'probe-file '("/opt/local/bin/gnuplot"
                                                   "/usr/local/bin/gnuplot"
                             ))
		           "gnuplot"))

(defun gnu-plot (gnu-name &key
                            (comment "default comment")
                            (title "default title")
                            (x-log nil)
                            (y-log nil)
                            (x-label nil)
                            (y-label nil)
                            (tics nil) ;; or "nomirror"
                            (create-png-p t)
                            (with "linespoints") ;; or "points"
                            data)
  "Plot curves using the gnuplot UNIX program.
 :DATA specifies a list of curve-specifiers, each curve-specifier is a plist
    with fields :title and :xys.  :title specifies a string and :xys specifies
    a list of xy points.  E.g.,
    (gnu-plot \"/tmp/plot.gnu\" 
              :title \"my plot\"
              :data '((:title \"first\" :xys ((1 10) (2 20) (3 30)))
                      (:title \"second\" :xys ((1.5 15.6) (2.1 20.8) (3.5 34.3) (3.7 37.96)))))
   ==> \"/tmp/plot.png\"

 If :CREATE-PNG-P nil is specified, then just the .gnu file is created, and nil
 is returned."
  (with-open-file (gnu gnu-name
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format t "[writing to ~A~%" gnu-name)
    (format gnu "# ~A~%" comment)
    (format gnu "set term png~%")
    (when x-log
      (format gnu "set logscale x~%"))
    (when y-log
      (format gnu "set logscale y~%"))
    (when x-label
      (format gnu "set xlabel ~S~%" x-label))
    (when y-label
      (format gnu "set ylabel ~S~%" y-label))
    (when tics
      (format gnu "set tics ~S~%" tics))
    (format gnu "set key bmargin~%")
    (when title
      (format gnu "set title ~S~%" title))

    (format gnu "plot ")
    (let ((header (make-string-output-stream))
          (footer (make-string-output-stream)))
      (flet ((plot (&key title xys)
               (format header "~S using 1:2" "-")
               (when with
                 (format header " with ~A" with))
               (when title
                 (format header " title ~S" title)
                 (format footer "# ~S~%" title))
               (dolist (xy xys)
                 (format footer "~A ~A~%" (car xy) (cadr xy)))
               (format footer "end~%")))
        (when data
          (apply #'plot (car data))
          (dolist (d (cdr data))
            (format header ",\\~%    ")
            (apply #'plot d))))
      (format gnu "~A~%" (get-output-stream-string header))
      (format gnu "~A" (get-output-stream-string footer))))
    (format t "   ~A]~%" gnu-name)
  (when create-png-p
    (let* ((png-file (change-extension gnu-name "png"))
           (#+sbcl process
	    #+allegro exit-status
	    (run-program *gnuplot-path* (list gnu-name)
			 :wait t
			 ;; TODO not sure whether allegro understands these options
                         :output png-file
                         :error *error-output*
                         :if-output-exists :supersede)))
      (when (empty-file-p png-file)
        (warn "gnuplot exited with code=~A produced empty output file ~A from input ~A"
              #+sbcl (sb-ext:process-exit-code process)
	      #+allegro exit-status
	      png-file gnu-name))
      png-file)))

(defun matrix-multiply (mat-a mat-b)
  "Given two matrices either as two 2-d arrays, or each as lists of list, (lists of rows)
 calculate and return the product matrix."
  (typecase (cons mat-a mat-b)
    ((cons array array)
     (destructuring-let (((q r) (array-dimensions mat-a))
                         ((r2 s) (array-dimensions mat-b)))
       (assert (eql r r2) () "cannot only multiple arrays with compatible dimensions")
       (let ((prod (make-array (list q s))))
         (loop :for i :from 0 :below q
               :do (loop :for j :from 0 :below s
                         :do (setf (aref prod i j)
                                   (loop :for k :from 0 :below r
                                         :summing (* (aref mat-a i k)
                                                     (aref mat-b k j))))))
         prod)))
    ((cons list)
     (matrix-multiply (make-array (list (length mat-a) (length (car mat-a))) :initial-contents mat-a)
                      mat-b))
    ((cons t (cons list))
     (matrix-multiply mat-a
                      (make-array (list (length mat-b) (length (car mat-b))) :initial-contents mat-b)))))

(defun take-while (predicate items)
  "Return a copy of the head of the given list of ITEMS containing all the element of ITEMS
 for which the given PREDICATE returns true.  The list returns terminates (and does not include
 the first item which satisfies the given PREDICATE.
 This function returns two values, first is the head of the list as described above,
 second is the rest of the list from that point."
  (labels ((recur (items acc)
             (cond
               ((null items)
                (values (nreverse acc) items))
               ((funcall predicate (car items))
                (recur (cdr items) (cons (car items) acc)))
               (t
                (values (nreverse acc) items)))))
    (recur items nil)))

(defun delimit-on (predicate items &aux (not-predicate (lambda (item) (not (funcall predicate item)))))
  "Chop a given list of ITEMS into consecutive sublists such that if you append them back together
 you get the original list.  The chopping occurs on each element for which the given PREDICATE
 returns true.  A list is returned whose first element is a possibly NIL list of the leading
 elements for which the PREDICATE is false, thereafter the elements are lists whose first element
 satisfies the PREDICATE, and whose trailing elements do not satisfy the PREDICATE. e.g.,
 (delimit-on #'evenp '(1 3 5 2 5 7 10 12 3 5 7)
 =>   ((1 3 5) (2 5 7) (10) (12 3 5 7))
 (delimit-on #'evenp '(2 5 7 10 12 3 5 7)
 =>   (nil (2 5 7) (10) (12 3 5 7))"
  (labels ((recur (items groups)
             (cond
               ((null items)
                (nreverse groups))
               (t
                (multiple-value-bind (leading trailing) (take-while not-predicate (cdr items))
                  (recur trailing (cons (cons (car items) leading) groups)))))))
    (when items
      (multiple-value-bind (leading trailing) (take-while not-predicate items)
        (cons leading (recur trailing nil))))))

(defun cmp-objects (a b)
  (cond ((equal a b)
	 t)
	((null a)
	 t)
	((null b)
	 nil)
	((not (eql (class-of a) (class-of b)))
	 (cond 
	   ((and (atom a) (listp b))
	    t)
	   ((and (listp a) (atom b))
	    nil)
	   (t
	    (cmp-objects (class-name (class-of a)) (class-name (class-of b))))))
        (t
         (typecase a
           (symbol
            (if (equal (symbol-package a) (symbol-package b))
                (string<= a b)
                (cmp-objects (symbol-package a)
                             (symbol-package b))))
           (string
            (string<= a b))
           (number
            (<= a b))
           (character
            (char<= a b))
           (package
            (cmp-objects (package-name a)
                         (package-name b)))
           (list
            ;; compare the first two elements which are not equal
            (while (equal (car a) (car b))
              (pop a)
              (pop b))
            (cmp-objects (car a) (car b)))
           (t
            (error "cannot compare ~A ~A with ~A ~A" (class-of a) a (class-of b) b))))))

(defun hash-to-assoc (hash)
  "Build a car/cadr assoc-list from the entries in a given hash table"
  (declare (type hash-table hash))
  (prog1-let (alist)
    (maphash (lambda (&rest args)
               (push args alist)) hash)))

(defun assoc-to-hash (assoc &key (test #'eql) (assoc-get #'cadr))
  "Build a hash table from a car/cadr assoc-list (or car/cdr if :assoc-get #'cdr)"
  (let ((hash (make-hash-table :test test)))
    (dolist (pair assoc)
      (setf (gethash (car pair) hash) (funcall assoc-get pair)))
    hash))
