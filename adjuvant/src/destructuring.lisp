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

(in-package :adjuvant)

(defmacro destructuring-lambda (destructuring-lambda-list &body body)
  "Similar to let, but the variables also understand destructuring like with destructuring-bind:  E.g.,
 (mapcar (destructuring-lambda (a (b) (&key c d &allow-other-keys))
          ...) '((1 (2) (:d 1 :a 2 :b 3 :c 4))
                 (1 (2) (:d 1 :a 2 :b 3 :c 4))
                 (1 (2) (:d 1 :a 2 :b 3 :c 4))
                 ...)
 ...)"
  (let ((arg (gensym)))
    `(lambda (&rest ,arg)
       (destructuring-bind ,destructuring-lambda-list ,arg
         ,@body))))

(defmacro destructuring-let (bindings &body body)
  "Similar to let, but the variables also understand destructuring like with destructuring-bind:  E.g.,
 (destructuring-let ((a 1)
                    ((b) '(2))
                    ((&key c d &allow-other-keys) '(:d 1 :a 2 :b 3 :c 4)))
 ...)"
  (let ((vars (mapcar #'car bindings))
        (values (mapcar #'cadr bindings)))
    `(funcall (destructuring-lambda ,vars
        ,@body)
      ,@values)))

(labels ((expand-functions (functions)
           (mapcar #'expand-function functions))
         (expand-function (function)
           (destructuring-bind (name destructuring-lambda-list &body body &aux (arg (gensym "arg"))) function
             `(,name (&rest ,arg)
                     (destructuring-bind ,destructuring-lambda-list ,arg
                       ,@body)))))

  (defmacro destructuring-labels (functions &body body)
    `(labels ,(expand-functions functions)
       ,@body))

  (defmacro destructuring-flet (functions &body body)
    `(flet ,(expand-functions functions)
      ,@body)))


(defmacro destructuring-dolist ((lambda-list list &optional result) &body body)
  (let ((var (gensym)))
    `(dolist (,var ,list ,result)
       (destructuring-bind ,lambda-list ,var
         ,@body))))
