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

(defmacro trampoline-tagbody (&body body)
  (let* ((goto-labels (setof item body (and item (symbolp item))))
         (label-1 (pop goto-labels))
         (groups (break-on #'symbolp body))
         (leading-exprs (car groups))
         (trailing-groups (cdr groups))
         (special (gensym))
         (arg1 (gensym "arg1"))
         (var1 (gensym "var1"))
         (block-1 (gensym "block")))
    (labels ((construct-call (goto-label)
               (if goto-label
                   `((goto-label ,goto-label))
                   nil))
             (collect-functions (groups acc &aux (group-1 (car groups)) (next-label (pop goto-labels)))
               (cond
                 ((null groups)
                  (nreverse acc))
                 (t
                  (collect-functions (cdr groups)
                                     (cons `(,(car group-1) ()
                                             ,@(cdr group-1)
                                             ,@(construct-call next-label))
                                           acc))))))
      `(while (setf ,var1 (block ,block-1
                            (macrolet ((goto-label (,arg1)
                                         (return-from ,block-1 (list ',special (lambda () (funcall (function ,arg1)))))))
                              (labels ,(collect-functions trailing-groups nil)
                                ,@leading-exprs ,@(construct-call label-1)))))
         (if (and (consp ,var1)
                    (eq ',special (car ,var1)))
             (funcall (cadr ,var1))
             ,var1)))))
    
