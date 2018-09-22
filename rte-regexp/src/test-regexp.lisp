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

(defpackage :rte-regexp-test
  (:use :cl :rte-regexp :rte :scrutiny))

(in-package :rte-regexp-test)

(defun test ()
  (run-package-tests :rte-regexp-test))

(define-test type/regexp-parsing
  ;; implicitly asserts that they expressions parse at all
  (regexp-to-rte "a+b")
  (regexp-to-rte "ab+x")
  (regexp-to-rte "abcde")
  (regexp-to-rte "a+bc")
  (regexp-to-rte "a+bc*")
  (regexp-to-rte "ab+c*")
  (regexp-to-rte "a|b")
  (regexp-to-rte "a|bc")
  (regexp-to-rte "ab|c")
  (regexp-to-rte "a(b|c)")
  (regexp-to-rte "(a+bc)*")
  (regexp-to-rte "(a+bc)*")
  (regexp-to-rte "ab+b(c*)"))


(define-test type/regexp-parsing-2
  ;; a+b
  (assert-true (rte::match-sequence "ab"  (regexp-to-rte "a+b")))
  (assert-true (rte::match-sequence "aab"  (regexp-to-rte "a+b")))
  (assert-true (rte::match-sequence "aaab"  (regexp-to-rte "a+b")))
  (assert-false (rte::match-sequence "aaabb"  (regexp-to-rte "a+b")))
  (assert-false (rte::match-sequence "bb"  (regexp-to-rte "a+b")))
  (assert-false (rte::match-sequence "b"  (regexp-to-rte "a+b")))
  (assert-false (rte::match-sequence "abc"  (regexp-to-rte "a+b")))
  
  ;; ab|c
  (assert-true (rte::match-sequence "ab" (regexp-to-rte "ab|c")))
  (assert-true (rte::match-sequence "c" (regexp-to-rte "ab|c")))

  (assert-false (rte::match-sequence "ac" (regexp-to-rte "ab|c")))
  (assert-false (rte::match-sequence "" (regexp-to-rte "ab|c")))

  ;; & and []
  (assert-true (rte::match-sequence "b" (regexp-to-rte "[abc]&[bcd]")))
  (assert-true (rte::match-sequence "bccb" (regexp-to-rte "([abc]&[bcd])+")))

  ;; [^]
  (assert-true (rte::match-sequence "abcd" (regexp-to-rte "[^xyz]+")))
  (assert-false (rte::match-sequence "abxcd" (regexp-to-rte "[^xyz]+")))

  ;; .
  (assert-true (rte::match-sequence "abxab"  (regexp-to-rte "ab.ab")))
  (assert-false (rte::match-sequence "ab"  (regexp-to-rte "ab.")))
  
  )

(define-test test/match-me
  (let ((match-me (rte-string-matcher "a*b+c")))
    (assert-true match-me)
    (assert-true (typep match-me 'function))
    (assert-true (funcall match-me  "aaaaabbbbbc"))
    (assert-false (funcall match-me  "abcccc"))))
