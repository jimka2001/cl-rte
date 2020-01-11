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

(in-package :rte-test)

(define-test test/strategy
  (destructuring-dolist ((&key rte yes no) '((:rte (:cat t symbol (:+ keyword))
                                              :yes ((3 x :x)
                                                    (3 x :x :y :z))
                                              :no ((3 x)
                                                   (3 x 3)
                                                   (3 x :x 3)))
                                             (:rte (:and (:* t) (:not (:cat string (:* t))))
                                              :yes (()
                                                    (3))
                                              :no (("hello")
                                                   ("hello" 3)))))
    (dolist (cl '(strategy-goto
                  strategy-tail-call
                  strategy-trampoline
                  ;;strategy-jump-table
                  ))
      (let ((lambda-match (rte::dump-code (rte-to-dfa rte) (make-instance cl))))
        (let ((f-match (eval lambda-match)))
          (declare (type function f-match))
          (dolist (seq yes)
            (assert (funcall f-match seq) (cl rte seq) "expecting match cl=~A rte=~A seq=~A" cl rte seq)
            (assert (funcall f-match (make-array (length seq) :initial-contents seq)) (cl rte seq) "expecting match cl=~A rte=~A seq=~A" cl rte seq))
          (dolist (seq no)
            (assert (not (funcall f-match seq)) (cl rte seq) "expecting no match")
            (assert (not (funcall f-match (make-array (length seq) :initial-contents seq))) (cl rte seq) "expecting no match")))))))




