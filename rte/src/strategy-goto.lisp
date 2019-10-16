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

(in-package   :rte)

(defclass strategy-goto (strategy-inline)
  ())

(defmethod goto-next-state ((strategy strategy-goto) state-name)
  `(go ,state-name))

(defmethod format-state-dispatch ((strategy strategy-goto) initial-state-name dumped-states)
  `(tagbody 
      ,(goto-next-state strategy initial-state-name)
      ,@dumped-states))

(defmethod dump-state ((strategy strategy-goto) state-name dumped-case)
  (copy-list `(,state-name
               ,dumped-case)))

(defmethod state-assoc ((strategy strategy-goto) exit-form-p states)
  (let ((n 0))
    (mapcar (lambda (state)
              (list state (cond
                            (exit-form-p
                             (gensym "L-EXIT-"))
                            ((state-sticky-p state)
                             (gensym (format nil "STICKY-~D-" (incf n))))
                            (t
                             (incf n)))))
            states)))
