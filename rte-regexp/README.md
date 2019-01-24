# RTE-REGEXP

## Synopsis
Simple string regular expression matcher based on rte

## API
* `regexp-to-rte` -- convert a string regular expression into a regular type expression E.g.,
```lisp
(regexp-to-rte "a*b+c")
===> (:CAT (:0-* (EQL #\a)) (EQL #\b) (:0-* (EQL #\b)) (EQL #\c))
```

* `rte-string-matcher` --  Given a regular-expression as a string, `str-regexp`, returns a unary function which when called with a target string will return `TRUE` or `FALSE` indicating whether the target string matches the regular expression, `str-regexp`.
```lisp
PKG> (defvar *match-me* (rte-regexp::rte-string-matcher "a*b+c"))
*MATCH-ME*
CL-USER> (funcall *match-me* "aaaaabbbbbc")
T
CL-USER> (funcall *match-me* "abcccc")
NIL
```



    


## License

~~~~
Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
~~~~
