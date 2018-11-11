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

(in-package :rte-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :rte :package-into :rte-test))

(define-test test/rte-typecase-1
  (let ((data '(1 2 3 4 )))
    (assert-true (eq :yes
		    (rte-typecase data
		      ((:cat number)
		       :no)
		      ((:cat number number)
		       :no)
		      ((:cat number number number)
		       :no)
		      ((:cat number number number number)
		       :yes)
		      ((:cat number number number number number)
		       :no))))))

(define-test test/rte-typecase-2
  (let ((data '(1 2 3 4 )))
    (assert-true (eq :yes
		     (rte-typecase data
		       ((:cat fixnum)
			:no)
		       ((:cat number number)
			:no)
		       ((:cat number number real)
			:no)
		       ((:cat number number number unsigned-byte)
			:yes)
		       ((:cat fixnum number real unsigned-byte number)
			:no))))))

(define-test test/rte-typecase-3
  (let ((data '(:x 3.4)))
    (assert-true (eq :yes
		     (rte-typecase data
		       ((:cat (:* fixnum) number)
			:no)
		       ((:cat (:or number symbol) number)
			:yes))))))

(define-test test/rte-typecase-4
  (assert-true (macroexpand-1 '(rte-typecase data
				(t 42)))))

(define-test test/rte-typecase-5
  (assert-true (macroexpand-1 '(rte-typecase data
				((:cat (:* fixnum) number)
				 :no)
				((:cat (:or number symbol) number)
				 :yes)))))

(define-test test/rte-typecase-6
  (let ((data '(1)))
    (assert-true (eql :here
		      (block block
			(tagbody
			   (go 2)
			 1
			   (return-from block
			     :here)
			 2
			   (rte-typecase data
			     ((:cat fixnum)
			      (go 1))
			     )
			 :no-here))))))

(define-test test/rte-typecase-7
  (let ((data '(1 2 3 )))
    (assert-true (eql :yes
		     (rte-typecase data
		       ((:cat fixnum)
			:no)
		       ((:cat number number string)
			:no)
		       ((:cat string number number )
			:no)
		       ((:cat number number fixnum)
			:yes)
		       ((:cat fixnum number real unsigned-byte number)
			:no))))))

(define-test test/rte-to-dfa
  (dolist (rte '((:CAT (:* FIXNUM) NUMBER)
		 (:AND (:CAT (:* FIXNUM) NUMBER) (:NOT (:OR)))
		 (:CAT (:OR NUMBER SYMBOL) NUMBER)
		 (:AND (:CAT (:OR NUMBER SYMBOL) NUMBER)
		  (:NOT (:OR (:CAT (:* FIXNUM) NUMBER))))))
    (assert-true (rte-to-dfa rte :trim nil :reduce nil))
    (assert-true (rte-to-dfa rte :trim t :reduce nil))
    (assert-true (rte-to-dfa rte :reduce t))))

(define-test test/synchronized-product
  (let ((rte-t (rte-to-dfa t)))
    (assert-true (eq rte-t
		     (rte-synchronized-product (list rte-t)))))
  (let ((rte-number (rte-to-dfa 'number))
	(rte-string (rte-to-dfa 'string)))
    (assert-true (= 2 (length (states rte-number))))
    (assert-true (= 2 (length (states rte-string))))
    (let ((rte-or (synchronized-product rte-number rte-string :boolean-function (lambda (a b) (or a b)))))
      (assert-true (= 2 (length (states rte-or))))
      (dolist (st-i (get-initial-states rte-or))
	(dolist (trans (transitions st-i))
	  (assert-true (member (transition-label trans)
			       '((or string number)
				 (or number string)) :test #'equal)))))
		  
    (assert-true (= 2 (length (states (synchronized-product rte-string rte-number :boolean-function (lambda (a b) (or a b)))))))
    (assert-true (= 0 (length (states (synchronized-product rte-string rte-number :boolean-function (lambda (a b) (and a b)))))))

    (let ((dfa-or (rte-synchronized-product (list rte-number rte-string))))
      (assert-true (= 2 (length (states dfa-or))))
      (dolist (st-i (get-initial-states dfa-or))
	(dolist (trans (transitions st-i))
	  (assert-true (member (transition-label trans)
			       '((or string number)
				 (or number string)) :test #'equal)))))))
    
    
    
(define-test test/rte-typecase-dfas-1
  (let ((expansion-1
	  (macroexpand-1
	   '(destructuring-case '(1 2 :x 3 :y 4)
	     ((a b &key x)
	      (declare (type string a b))
	      (list a b x))
	     ((a b &key x y)
	      (declare (type string a b))
	      (list a b x y))
	     ((a b c d &optional z1 z2)
	      (declare (type fixnum a b d)
	       (type keyword c z1)
	       (type string z2))
	      (list a b c d z1 z2))
	     ((a b &key x y  &allow-other-keys)
	      (declare (type fixnum a b x y))
	      (list a b x y))
	     ((a b &key x y)
	      (declare (type fixnum a b x y))
	      (list a b x y))
	     ((a b c &key y x &allow-other-keys)
	      (declare (type float a b c))
	      (list a b c x y))))))
    
    (assert-true (typep expansion-1
			'(rte (:cat (eql destructuring-case-alt) (:* cons)))))
    (let ((expansion-2 (macroexpand-1 expansion-1)))
      (assert-true (typep expansion-2
			  '(rte (:cat (eql LET)
				 cons
				 (cons (eql rte-typecase))))))
      (destructuring-bind (_1 _2 (_3 obj &rest clauses) &rest _4) expansion-2
	(declare (ignore _1 _2 _3 _4 obj))
	(rte-typecase-helper clauses)))))

(define-test test/rte-typecase-dfas-3
  (flet ((f1 () :x)
	 (f2 () :y))
    (destructuring-case '("1" "2")
      ((&key x)
       (f1))
      ((&key x y)
       (f2)))))

(define-test test/rte-typecase-dfas-4
  (flet ((f1 () :x)
	 (f2 () :y))
    (DESTRUCTURING-CASE-ALT '("1" "2")
      ((&KEY X) NIL (F1))
      ((&KEY X Y) NIL (F2)))))

(define-test test/rte-typecase-dfas-5
  (flet ((f1 () :x)
	 (f2 () :y))
    (let ((g670 '("1" "2")))
      (rte-typecase g670
	 ((:and (:not (:and (:* (member :x) t)
		      (:cat (:* (not (eql :x)) t)
			    (:or :empty-word (:cat (eql :x) t (:* t))))))
		(:and (:* (member :x :y) t)
		      (:cat (:* (not (eql :x)) t)
			    (:or :empty-word (:cat (eql :x) t (:* t))))
		      (:cat (:* (not (eql :y)) t)
			    (:or :empty-word (:cat (eql :y) t (:* t))))))	
	  (destructuring-bind (&key x) g670
	    (f1) (f2)))
	))))

(define-test test/rte-typecase-dfas-2
  (let ((expansion-1
	  (macroexpand-1
	   '(destructuring-case '(1 2 :x 3 :y 4)
	     ((a b &key x)
	      (declare (type string a b))
	      (f1))
	     ((a b &key x y)
	      (declare (type string a b))
	      (f2))))))
    
    (assert-true (typep expansion-1
			'(rte (:cat (eql destructuring-case-alt) (:* cons)))))
    (let ((expansion-2 (macroexpand-1 expansion-1)))
      (assert-true (typep expansion-2
			  '(rte (:cat (eql LET)
				 cons
				 (cons (eql rte-typecase))))))
      (destructuring-bind (_ _ (_ obj &rest clauses) &rest _) expansion-2
	(declare (ignore _))
	;;(format t "obj=~A~%" obj)
	;;(format t "clauses=~A~%" clauses)
	(rte-typecase-helper clauses)))))
    

(define-test test/parse-defmethod
  (flet ((parse (form)
	   (rte-typecase form
	     ;; (defmethod          name qualifiers  lambda-list declarations        documentation  ...body...)
	     ((:cat (eql defmethod) symbol  (:+ keyword) list  (:+ (cons (eql declare))) string     (:* t))
	      1)

	     ;; (defmethod          name   lambda-list documentation declarations            ...body...)
	     ((:cat (eql defmethod) symbol list        string        (:+ (cons (eql declare)))  (:* t))
	      2)

	     ;; (defmethod          name   qualifiers   lambda-list documentation declarations               ...body...)
	     ((:cat (eql defmethod) symbol (:+ keyword) list        string        (:+ (cons (eql declare)))  (:* t))
	      3)

	     ;; (defmethod          name   lambda-list declarations      documentation     ...body...)
	     ((:cat (eql defmethod) symbol list        (:+ (cons (eql declare))) string       (:* t))
	      4)

	     ((:* t)
	      5))))


    (assert-true (eql 1
		      (parse '(defmethod name :before :after (a b) (declare (optimize (safety 3)))  "a docstring" (list a b)))))

    (assert-true (eql 2
		      (parse '(defmethod name (a b) "a docstring"  (declare (optimize (safety 3))) (list a b)))))

    (assert-true (eql 3
		      (parse '(defmethod name :before :after (a b) "a docstring" (declare (optimize (safety 3))) (list a b)))))

    (assert-true (eql 4
		      (parse '(defmethod name (a b) (declare (optimize (safety 3)))  "a docstring" (list a b)))))

    
    (assert-true (eql 5
		      (parse '(defmethod (setf binder) () ()))))))

(define-test test/parse-defmethod-2
  (flet ((parse (form)
	   (rte-typecase form
	     ;; (defmethod          name qualifiers  lambda-list declarations        documentation  ...body...)
	     ((:cat (eql defmethod) symbol  (:+ keyword) list  (:+ (cons (eql declare))) string     (:* t))
	      1)
	     ;; default
	     ((:* t)
	      5))))
    
    (assert-true (eql 5
		      (parse '(defmethod (setf binder) () ()))))))

(define-test test/parse-defmethod-3
  (flet ((parse (form)
	   (rte-typecase form
	     ;; default
	     ((:* t)
	      5))))
    
    (assert-true (eql 5
		      (parse '(defmethod (setf binder) () ()))))))


(define-test test/sticky-rte-typecase
  (assert-true (equal :c
		      (rte-typecase '(1 2)
			((:cat number number number)
			 :a)
			((:cat t t t)
			 :b)
			((:* t)
			 :c)))))

;; (define-test test/parse-defmethod-2a
;;   (flet ((parse (form)
;; 	   (let ((g687 form))
;; 	     (funcall
;; 	      (lambda (g687)
;; 		(declare (optimize (speed 3) (debug 0) (safety 0)))
;; 		(block check696
;; 		  (typecase g687
;; 		    (list
;; 		     (tagbody
;; 			(go l688)
;; 		      l688 (format t "@ ~A~%" 'l688)
;; 			(when (null g687) (return-from check696 (progn 5)))
;; 			(case (pop g687)
;; 			  ((defmethod) (go l693))
;; 			  (t (return-from check696 nil)))
;; 		      l689 (format t "@ ~A~%" 'l689)
;; 			(when (null g687) (return-from check696 (progn 5)))
;; 			(typecase (pop g687)
;; 			  (string (go l694))
;; 			  ((cons (eql declare)) (go l689))
;; 			  (t (return-from check696 nil)))
;; 		      l690 (format t "@ ~A~%" 'l690)
;; 			(when (null g687) (return-from check696 (progn 5)))
;; 			(typecase (pop g687)
;; 			  ((cons (eql declare)) (go l689))
;; 			  (t (return-from check696 nil)))
;; 		      l691 (format t "@ ~A~%" 'l691)
;; 			(when (null g687) (return-from check696 (progn 5)))
;; 			(typecase (pop g687)
;; 			  (keyword (go l691))
;; 			  (list (go l690))
;; 			  (t (return-from check696 nil)))
;; 		      l692 (format t "@ ~A~%" 'l692)
;; 			(when (null g687) (return-from check696 (progn 5)))
;; 			(typecase (pop g687)
;; 			  (keyword (go l691))
;; 			  (t (return-from check696 nil)))
;; 		      l693 (format t "@ ~A~%" 'l693)
;; 			(when (null g687) (return-from check696 (progn 5)))
;; 			(typecase (pop g687)
;; 			  (symbol (go l692))
;; 			  (t (return-from check696 nil)))
;; 		      l694 (format t "@ ~A~%" 'l694)
;; 			(when (null g687) (return-from check696 (progn 1)))
;; 			(typecase (pop g687) (t (go l694)))))
;; 		    (t  (format t "@ ~A~%" 't) nil))))
;; 	      g687))))
    
;;     (assert-true (eql 5
;; 		      (parse '(defmethod (setf binder) () ()))))))

(define-test test/parse-defmethod-2b
  (flet ((parse (form)
	   (rte-typecase form
	     ((:cat (eql defmethod) symbol  (:+ keyword)      )
	      1)
	     ;; default
	     ((:* t)
	      5))))
    
    (assert-true (eql 5
		      (parse '(defmethod (setf binder) () ()))))))

(define-test test/parse-defmethod-2c
  (flet ((parse (form)
	   (rte-typecase form
	     ((:cat t symbol  (:+ keyword)      )
	      1)
	     ;; default
	     ((:* t)
	      5))))
    
    (assert-true (eql 5
		      (parse '(defmethod (setf binder) () ()))))))

(define-test test/parse-defmethod-2d
  (flet ((parse (form)
	   (rte-typecase form
	     ((:cat t symbol  (:+ t)      )
	      1)
	     ;; default
	     ((:* t)
	      5))))
    
    (assert-true (eql 5
		      (parse '(defmethod (setf binder) () ()))))))

(define-test test/parse-defmethod-2e
  (flet ((match (form)
	   (rte-typecase form
	     ((:cat string (:* t))
	      1)
	     ;; default
	     ((:* t)
	      5))))
    
    (assert-true (eql 5
		      (match '(1 2 3))))))
