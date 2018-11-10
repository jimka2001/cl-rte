;; Copyright (c) 2016,18 EPITA Research and Development Laboratory
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


(defun find-keyword (keyvar lambda-list)
  "an &key argument in a lambda list can be declared in several different ways.
FIND-KEYWORD finds and returns the keyword (symbol from keyword package) associated
with the given KEYVAR form"
  ;; {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}
  (typecase keyvar
    (symbol
     (intern (symbol-name keyvar) (find-package "KEYWORD")))
    ((cons symbol t) ; (var [init-form [supplied-p-parameter]])
     (intern (symbol-name (car keyvar)) (find-package "KEYWORD")))
    ((cons (cons symbol t) t); ((keyword-name var) [init-form [supplied-p-parameter]])
     (intern (symbol-name (car (car keyvar))) (find-package "KEYWORD")))
    (t
     (error "invalid &key syntax ~A in lambda-list ~A" keyvar lambda-list))))

(defun find-key-variable (keyvar lambda-list)
  "an &key argument in a lambda list can be declared in several different ways.
FIND-KEY-VARIABLE finds and returns the variable (symbol or destructuring-list) associated
with the given KEYVAR form"
  ;; {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}
  (typecase keyvar
    (symbol
     keyvar)
    ((cons symbol t) ; (var [init-form [supplied-p-parameter]])
     (car keyvar))
    ((cons (cons symbol t) t) ; ((keyword-name var) [init-form [supplied-p-parameter]])
     (cadr (car keyvar)))
    (t
     (error "invalid &key syntax ~A in lambda-list ~A" keyvar lambda-list))))

(defun swap-type-specifier-alist (constraint-alist)
  "constraint-alist is an car/cdr alist mapping each type specifiers to a list of
   variable names. e.g., ((number a b c) ((or string symbol) x y z))
   RETURNS an alist mapping each variable name to its type specifier.
   If any variable appears more than once in the given constraint-alist,
   the corresponding types are intersected in the return alist."
  (let (type-spec-alist)
    (dolist (constraint constraint-alist)
      (destructuring-bind (type-specifier &rest variable-names) constraint
	(dolist (name variable-names)
	  (let* ((hit (assoc name type-spec-alist))
		 (type-spec (cadr hit)))
	  (cond
	    (hit
	     (setf (cadr hit) `(and ,type-specifier ,type-spec)))
	    (t
	     (push (list name type-specifier) type-spec-alist)))))))
    type-spec-alist))


(defun gather-type-declarations (body)
  "BODY is the body of some destructuring-bind form.  This function, gather-type-declaration,
 examines zero, one, or more declarations in the leading position of the body to find
 type declarations.  An assoc list (car/cadr) is returned of the form
 ((var1 typespec1) (var2 typespec2)...)
 If the same variable is declared more than once, the resulting type is the intersection
 of the individual types.
 
 CL-SPEC> Declaration TYPE
          ...
 CL-SPEC> If nested type declarations refer to the same variable, then the value of the
 CL-SPEC> variable must be a member of the intersection of the declared types.
 "
  (let (var-declarations)
    ;; allow one but only one optional "docstring"
    (when (stringp (car body))
      (format t "skipping docstring ~S~%" (car body))
      (pop body))
    (loop :while (typep body '(cons (cons (eql declare))))
	  :do (dolist (decl (cdr (car body)))
		(unless (member (car decl) '(dynamic-extent ignore optimize ftype inline special ignorable notinline type)
				:test #'eq)
		  (push 'type decl))
                (when (typep decl '(cons (eql type)))
		  (destructuring-bind (_ typespec &rest vars) decl
		    (declare (ignore _))
                       ;; need to handle the case that the same
                       ;; variable appears twice with two different
                       ;; declarations.  Need to check with the spec
                       ;; to see what the defined semantics are.
		    (dolist (var vars)
		      (if (assoc var var-declarations)
			  (setf (cadr (assoc var var-declarations))
				(list 'and typespec (cadr (assoc var var-declarations))))
			  (push (list var typespec) var-declarations)))))
		(pop body)))
    var-declarations))

(defun destructuring-lambda-list-to-rte (lambda-list &key type-specifiers)
  "Generate an RTE (regular type expression) given a destructuring-lambda-list.
If TYPE-SPECIFIERS is given it should be an alist (car/cadr) mapping variables
in the lambda list to cl type specifiers.  Such a list can be computed by calling
the function GATHER-TYPE-DECLARATIONS given a list whose leading elements are
zero or more (declare ...) forms some of which contain type declarations.

A destructuring lambda list has the following syntax:

reqvars::= var* 
optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
restvar::= [{&rest | &body} var] 
keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* 
            [&allow-other-keys]] 
auxvars::= [&aux {var | (var [init-form])}*] 
wholevar::= [&whole var] 
lambda-list::= (wholevar reqvars optvars restvar keyvars auxvars)

Not supporting this syntax -> (wholevar reqvars optvars . var) "
  (declare (type list lambda-list type-specifiers))
  (let ((copy-lambda-list lambda-list)
	(ll-keywords '(&whole &optional &rest &body &key &allow-other-keys &aux))
	(whole-pattern '(:* t))
	req-pattern
	rest-pattern
	key-pattern
	optional-types
	tail-pattern)

    (labels ((stack-optional (types base)
	       (if (null types)
		   base
		   (list :? (car types) (stack-optional (cdr types) base))))
	     (get-var-type (var)
	       (cadr (assoc var type-specifiers)))
	     (recursive-pattern-var (var &key (symbol-pattern t))
	       (typecase var
		 (null
		  'null)
		 (symbol
		  (or (get-var-type var)
		      symbol-pattern))
		 (list
		  `(:and list (rte ,(canonicalize-pattern
				     (destructuring-lambda-list-to-rte var
								       :type-specifiers type-specifiers)))))
		 (t
		  (error "invalid object ~A in var position in lambda list ~A" var copy-lambda-list))))
	     (recursive-pattern-var-val (var-val)
	       (typecase var-val
		 (null
		  'null)
		 (list
		  (recursive-pattern-var (car var-val)))
		 (symbol
		  (recursive-pattern-var var-val))
		 (t
		  (error "invalid object ~A in var position of lambea list ~A" var-val copy-lambda-list)))))
      ;; wholevar
      ;; wholevar::= [&whole var]
      (pop ll-keywords) ;; pop &whole off ll-keywords
      (when (eq '&whole (car lambda-list))
	(pop lambda-list)		; pop off &whole
	(setf whole-pattern (recursive-pattern-var (car lambda-list)
						   :symbol-pattern '(:* t)))
	(pop lambda-list)		; pop off the variable
	)
    
      ;; reqvars
      (setf req-pattern (cons :cat
			      (loop :while (and lambda-list
						(not (member (car lambda-list) ll-keywords)))
				    :collect (prog1 (recursive-pattern-var (car lambda-list))
					       (pop lambda-list)))))
    
      ;; optvars
      ;; optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
      (when (eql '&optional (car lambda-list))
	(pop ll-keywords)	    ; pop &optional off of ll-keywords
	(pop lambda-list)           ; pop &optional off the lambda-list
	(setf optional-types (loop :while (and lambda-list
					       (not (member (car lambda-list) ll-keywords)))
				   :collect (prog1 (recursive-pattern-var-val (car lambda-list))
					      (pop lambda-list)))))

      ;; restvar
      ;; restvar::= [{&rest | &body} var] 
      (when (member (car lambda-list) '(&rest &body))
	(pop lambda-list)		; pop off the &rest | &body
	(setf rest-pattern (cond
			     ((listp (car lambda-list))
			      (recursive-pattern-var (car lambda-list) :symbol-pattern '(:* t)))
			     ((member (recursive-pattern-var-val (car lambda-list)) '(list t))
			      '(:* t))
			     ((eq 'cons (recursive-pattern-var-val (car lambda-list)))
			      '(:+ t))
			     (t
			      (error "unable to convert &rest ~A with declared type ~A to RTE"
				     (car lambda-list) (recursive-pattern-var-val (car lambda-list))))))
	(pop lambda-list)		; pop off the var,
	(pop ll-keywords)		; pop &rest
	(pop ll-keywords)		; pop &body
	)

      ;; keyvars
      ;; keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* 
      ;;             [&allow-other-keys]]

      ;; the following (WHEN ...) calculates a regular type expression to represent the &key portion
      ;; of the lambda list, also taking into account the &allow-other-keys if given.
      ;; The result is left in KEY-PATTERN via SETF.
      ;; The format of KEY-PATTERN is one of two things depending on whether &allow-other-keys is given
      ;; or not.  
      ;;
      ;; In each case the expression has (:and (:0-* keyword t) ...) at the top
      ;; level to insist that they &key section contains only key/value pairs.
      ;; There is an (:or (:cat ...) (:cat ...)) sequence of (:cat ...) expressions.
      ;; Each (:cat ...) expression specifies one key/value pair,
      ;; and the (:or ...) indicates that they keys may be in any order.
      ;; However, if a key variable in the lambda list has a structuring element
      ;; denoting that the value will be destructured by the destructuring-case/destructuring-bind
      ;; then the destructuring element corresponds to the FIRST occurance of that keyword
      ;; in the given argument list.  If the same keyword is given multiple times in the argument
      ;; list, no structuring element is applied to occurances other than the first.
      ;; E.g., (&key x ((:y (y0 y1 y2)) '(nil nil nil)))
      ;;  The value associated with the first :y in the given argument list must match the
      ;;  destructuring element (y0 y1 y2), i.e., Fy=(:and list (rte (:cat t t t)))
      ;;  Thus each element of the (:cat ...) is of the form
      ;;  (:0-1 (eql :y) Fy                      ...)
      ;;  (:0-1 (eql :y) (:and list (rte t t t)) ...)
      ;; Here are two examples:
      ;;  the values of Fx, Fy, and Fz depend on the structuring elements of
      ;;    :x :y and :z respectively.

      #|
      (&key x y z) ; without &allow-other-keys
       ==>
          (:and (:0-* keyword t)
                (:0-* (not (member :x :y :z)) t)
                (:cat (:* (not :x) t) (:? (eql :x) Fx (:* t)))
                (:cat (:* (not :y) t) (:? (eql :y) Fy (:* t)))
                (:cat (:* (not :z) t) (:? (eql :z) Fz (:* t))))

      (&key x y z &allow-other-keys)
       ==>
          (:and (:0-* keyword t)
                (:cat (:* (not :x) t) (:? (eql :x) Fx (:* t)))
                (:cat (:* (not :y) t) (:? (eql :y) Fy (:* t)))
                (:cat (:* (not :z) t) (:? (eql :z) Fz (:* t))))
      |#
      
      (when (eql '&key (car lambda-list))
	(pop lambda-list)		; pop off &key
	(let* (key-vars
	       used-keywords
	       (key-formats (loop :while (and lambda-list
					      (not (member (car lambda-list) ll-keywords)))
				  :for ll-var = (pop lambda-list)
				  :for keyword = (find-keyword ll-var copy-lambda-list)
				  :for key-var = (find-key-variable ll-var copy-lambda-list)
				  :for pattern = (recursive-pattern-var key-var)
				  :collect (list keyword pattern)
				  :do (push keyword used-keywords)
				  :do (push key-var key-vars)))
	      (allow-other-keys (when (eql '&allow-other-keys (car lambda-list))
				  (pop lambda-list)
				  t)))

	  (setf key-pattern
		(let ((patt-1 (cond
				(allow-other-keys
				 '(:* keyword t))
				(used-keywords
				 `(:* (member ,@used-keywords) t))
				(t ; used-keywords = (), this means &key was used with no &allow-other-keys and also with no acutal keys
				 :empty-word )))
		      (patt-per-key (loop :for key-format :in key-formats
					  :collect
					  (destructuring-bind (key type) key-format
					    `(:cat (:* (not (eql ,key)) t) (:? (eql ,key) ,type (:* t)))))))
					  
		  `(:and ,patt-1
			 ,@patt-per-key)))))

      (let ((rest-key-pattern (cond
				((and rest-pattern key-pattern)
				 ;; &rest &key
				 `(:and ,rest-pattern ,key-pattern))
				((and rest-pattern (not key-pattern))
				 ;; &rest
				 rest-pattern)
				((and (not rest-pattern) key-pattern)
				 ;; &key
				 key-pattern)
				((and (not rest-pattern) (not key-pattern))
				 ':empty-word))))
	(setf tail-pattern (stack-optional optional-types rest-key-pattern)))

      (pop ll-keywords)			; pop off &key
      (pop ll-keywords)			; pop off &allow-other-keys

      ;; auxvars
      ;; auxvars::= [&aux {var | (var [init-form])}*]     
      (when (eql '&aux (car lambda-list))
	;; We only parse the &aux section so that we can check for errors.  If there's something
	;; trailing the &aux section, we trigger an error below.
	(pop lambda-list)	   ; pop off the &aux from lambda-list
	(loop :while lambda-list
	      :for var = (pop lambda-list) ;; update every time through the loop
	      :do (typecase var
		    (null
		     (error "destructuring not supported in &aux variable ~A of lambda-list ~A" var copy-lambda-list))
		    (symbol
		     t)
		    ((cons symbol t)
		     t)
		    (t
		     (error "destructuring not supported in &aux variable ~A of lambda-list ~A" var copy-lambda-list)))))
      
      (pop ll-keywords)

      (if lambda-list
	  (error "lambda list ~A not parsed correctly: suspicious tail is ~A" copy-lambda-list lambda-list)
	  `(:and ,whole-pattern
		 (:cat ,req-pattern
		       ,tail-pattern))))))

(defun test-illustrate-dfas (pattern previous-patterns derived-pattern)
  (format t "-----------~%pattern: ~S~%" pattern)
  (format t "previous: ~S~%" previous-patterns)
  (format t "derived: ~S~%" derived-pattern)
  (let* ((dfa-derived (rte-to-dfa derived-pattern))
	 (dfa-given (rte-to-dfa pattern))
	 (dfa-given-trim (trim-state-machine dfa-given))
	 (dfa-derived-trim (trim-state-machine dfa-derived))
	 (dfa-given-reduced (progn (format t "reducing dfa of ~S~%" pattern)
				   (minimize-state-machine dfa-given)))
	 (dfa-derived-reduced (progn (format t "reducing dfa of ~S~%" derived-pattern)
				     (minimize-state-machine dfa-derived))))
    (flet ((report-dfa (dfa comment &key view)
	     (format t "~A   dfa size: ~D~%" comment (length (states dfa)))
	     (when view
	       (ndfa-to-dot dfa  (make-temp-file-name comment
						      :extension "png"
						      :ensure-file-exists t)
			    :view t :transition-legend t))))
      ;; TODO need to minimize here, not just trim
      (report-dfa dfa-given "given" :view t)
      (report-dfa dfa-derived "derived")
      (report-dfa dfa-given-trim "given-trim")
      (report-dfa dfa-given-reduced "given-reduced")
      (report-dfa dfa-derived-trim "derived-trim")
      (report-dfa dfa-derived-reduced "derived-reduced" :view t))
    ))

(defun expand-destructuring-case-alt (object-form clauses)
  "OBJECT-FORM is the form to be evaluated,
CLAUSES is a list of sublists, each sublist can be destructured as: (LAMBDA-LIST CONSTRAINTS &REST BODY)
  where CONSTRAINTS is a car/cdr alist mapping each type specifiers to a list of
  variable names. e.g., ((number a b c) ((or string symbol) x y z))"
  (let ((object (gensym))
	previous-anti-patterns)
    (flet ((transform-clause (clause)
	     (destructuring-bind (lambda-list constraints &rest body) clause
	       (declare (type list constraints))
	       ;; constraints is an car/cdr alist mapping each type specifiers to a list of
	       ;;  variable names. e.g., ((number a b c) ((or string symbol) x y z))
	       (let* ((type-specifier-alist (swap-type-specifier-alist constraints))
		      (additional-declarations (mapcar #'(lambda (constraint)
							   `(declare (type ,@constraint))) constraints))
		      (pattern (canonicalize-pattern (destructuring-lambda-list-to-rte
						      lambda-list
						      :type-specifiers type-specifier-alist))))
		 (prog1 `(,pattern
			  (destructuring-bind ,lambda-list ,object
			    ,@additional-declarations
			    ,@body))
		   (push `(:not ,pattern) previous-anti-patterns))))))
      `(let ((,object ,object-form))
	 (rte-typecase ,object
		       ,@(mapcar #'transform-clause clauses))))))

(defmacro destructuring-case-alt (object-form &body clauses)
  "Symantically similar to CASE except that the object is matched against destructuring-lambda-lists and
 optional type constraints.  The first clauses matching the structure and types is evaluated.
 Each clause is of the form (destructuring-lambda-lists constraint-alist &body body)
 Where constraint-alist is an car/cdr alist mapping a type specifier to a list of variable
 names of that type.   The variables will be implicitly declared in the body.
 E.g.,
  (destructuring-case-alt '(1 2 :x 3)
    ((a b c) ((integer a b) (symbol c))
     :first)
    ((a &optional (b 0) &key (x 0) (y 0)) ((integer a b x y))
     :second))
==> :second"
  (expand-destructuring-case-alt object-form clauses))

(defmacro destructuring-case (object-form &body clauses)
  "Similar to CASE except that the object is matched against destructuring-lambda-lists and
 declared type constraints.  The first clauses matching the structure and types is evaluated.
 Each clause is of the form (destructuring-lambda-lists &body BODY).
 Where BODY may have docstring and declarations.  The type constraints will
 be extracted from the (declare ... (type ...)) forms.
 E.g.,
  (destructuring-case '(1 2 :x 3)
    ((a b c)
     (declare (integer a b) (symbol c))
     :first)
    ((a &optional (b 0) &key (x 0) (y 0)) 
     (declare (integer a b x y))
     :second))
 ==> :second"
  (let ((reformed
	  (loop :for clause :in clauses
		:collect (destructuring-bind (lambda-list &body body) clause
			   ;; gather-type-declarations returns a list of the
			   ;;   form ((var1 typespec1) (var2 typespec2) ...)
			   ;; need to convert to ((typespec1 var...)
			   ;;                     (typespec2 var...) ...)
			   (let ((constraints (mapcar #'reverse (gather-type-declarations body))))
			     `(,lambda-list ,constraints ,@body))))))
    `(destructuring-case-alt ,object-form ,@reformed)))

(defun expand-destructuring-methods (object-form clauses call-next-method)
  (declare (type symbol call-next-method))
  (let ((object (gensym))
	(objects (gensym)))
    (flet ((transform-clause (clauses)
	     (destructuring-bind (lambda-list &rest body) (car clauses)
	       (let ((pattern (destructuring-lambda-list-to-rte lambda-list
								:type-specifiers (gather-type-declarations body))))

		 (if (cdr clauses)
		     `((rte ,(canonicalize-pattern pattern))
		       (flet ((,call-next-method (&rest ,objects)
				(destructuring-methods (if ,objects
							   (car ,objects)
							   ,object)
				    (:call-next-method ,call-next-method)
				  ,@(cdr clauses))))
			 (declare (ignorable (function ,call-next-method)))
			 (destructuring-bind ,lambda-list ,object
			   ,@body)))
		     `((rte ,(canonicalize-pattern pattern))
		       (flet ((,call-next-method (&rest ,objects)
				(declare (ignore ,objects))
				(error "cannot call ~A from final clause" ',call-next-method)))
			 (declare (ignorable (function ,call-next-method)))
			 (destructuring-bind ,lambda-list ,object
			   ,@body))))))))
	   `(let ((,object ,object-form))
	      (typecase ,object
		((not list) nil)
		,@(maplist #'transform-clause clauses))))))

(defmacro destructuring-methods (object-form (&key (call-next-method 'call-next-method)) &body clauses)
  "A variant of DESTRUCTURING-CASE, but allows a call-next-method feature.
The first matching clause is executed, if it calls the call-next-method function
then the next matching method is executed.
The call-next-method function defaults to CALL-NEXT-METHOD, but may be renamed
using the :CALL-NEXT-METHOD keyword argument.
E.g.
(destructuring-methods '(1 2 3) (:call-next-method cnm)
  ((a b c)
   (declare (type number a b c))
   (* 2 (or (cnm) 1)))
  ((a b c)
   (declare (type fixnum a b c))
   3))"
  (expand-destructuring-methods object-form clauses call-next-method))

