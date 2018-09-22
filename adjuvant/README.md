# ADJUVANT

## Synopsis

Utility functions used in other packages.

## API

### List Manipulation
* `group-by` -- Create an alist by applying a key function to every element of a sequence
E.g., to group the lists in an array by length.
````lisp
PKG> (group-by #((1) (1 2) (3) (1 2 3) (3 4)) :key #'length)
==> ((3 ((1 2 3)))
     (2 ((3 4) (1 2)))
     (1 ((3) (1))))
````
 E.g., to group strings together in `string-equal` case-independent equal lists.

````lisp
PKG> (group-by '("aaa" "aAA" "AAA" "b" "BA" "bA" "AaA") :key #'identity :test #'string-equal)
==> (("BA"  ("bA" "BA"))
     ("b"   ("b"))
     ("aaa" ("AaA" "AAA" "aAA" "aaa")))
````

* `tconc` -- Standard function missing from Common Lisp
* `lconc` -- Standard function missing from Common Lisp
* `map-pairs` -- Call a given function over all the x,y pairs from a given list
````lisp
PKG> (let (pairs)
       (map-pairs (lambda (a b) (push (list a b) pairs))
                  '(a b c d))
       pairs)
===> ((C D) (B D) (B C) (A D) (A C) (A B))
````
* `tree-reduce` -- 
* `unionf` -- 

### Iterators
* `exists` -- Tests whether there exists an element which satisfies an expression.  E.g., `(exists x '(1 2 3) (evenp x))`
* `forall` -- Tests whether all the elements in a given list satisfies an expression.  E.g., `(forall x '(2 4 6 8 10) (evenp x))`
* `setof` -- Construct a new list of all the elements which satisfy an expression.  E.g., `(setof x '(2 3 5 6 7) (evenp x))`
* `while` -- Loop while condition true. E.g., `(while (evenp x) (setf x (g x)))`


### Implementation independent interfaces sbcl/Allegro

* `type-expand` -- expand a type specifier into base types. E.g.,
````lisp
PKG> (deftype and-not (x y)
       `(and ,x (not ,y)))
PKG> (type-expand '(and-not integer fixnum))
==> (AND INTEGER (NOT FIXNUM))
````
* `process-kill` -- Kill a process started by `run-program` if it was started with `(run-program ... :wait t)`
* `run-program` -- Wrapper around the implementation dependent (sbcl/Allegro) shell command function.
* `getenv` -- Wrapper around the implementation dependent (sbcl/Allegro) UNIX environment variable reader.
* `garbage-collect` -- run the garbage collector

### Other
* `boolean-expr-to-latex` -- Generate LaTeX code, and print to `*standard-output*` to post into a tex document to represent a Boolean expression  E.g.,
````lisp
PKG> (boolean-expr-to-latex '(and (or a b (not c)) (not (or c d))))
((A \vee B \vee \neg C) \wedge \neg (C \vee D))
==> NIL
````
* `encode-time` -- Generate a time string in human readable format.  E.g.,
````lisp
PKG> (encode-time)
==> "Sat Sep 22 18:35:18 2018"
PKG> (let ((time (get-universal-time)))
       (sleep 5)
       (encode-time time))
==> "Sat Sep 22 18:35:43 2018"
````
* `*tmp-dir-root*` -- Special global variable used as the base directory for `make-temp-dir`
* `make-temp-dir` -- Return a string indicating the full path to a temporary directory.  E.g.,
````lisp
PKG> (make-temp-dir "mydir")
==> "/tmp/jimka/mydir/"
````
* `getter` -- Given a field name, return a unary function which will retrieve the value of the field from the given object.
````lisp
PKG> (mapcar (getter :x) '((:x 1 :y 2)
                           (:a 3 :x 2 :y 3)
                           (:a 3 :b 4 :x 4 :y 4)))
==> (1 2 4)
````
* `user-read` --  Calls `cl:read` with the specified arguments, but with `*PACKAGE*` bound to the CL-USER package.  
The effect of this is that symbols like `NIL` and `-` get read as `COMMON-LISP:NIL` and `COMMON-LISP:-` rather 
than as keywords.


## License

```
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
```