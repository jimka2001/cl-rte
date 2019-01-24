# RTE - Rational Type Expressions

## Synopsis


Definition of the RTE CL type.  A type (and supporting functions) which implement rational type expressions.
      For information about this project and related publications , see [Efficient dynamic type checking of heterogeneous sequences](https://www.lrde.epita.fr/wiki/Publications/newton.16.rte.report)

## API

* `list-of` -- Type definition for matching a possibly empty list of some specified type.  E.g., 

`(the (list-of (or string integer)) my-list)`

* `rte` -- Type name -- used to designate a regular type expression.  E.g., `(typep obj '(rte (:* (:cat string number))))`
* `rte-reset` -- Forget all regular type expressions.  Useful for debugging.
* `destructuring-case` --   Similar to `CL:CASE` except that the object is matched against destructuring-lambda-lists and
optional type constraints.  The first clauses matching the structure and types is evaluated.
Each clause is of the form `(destructuring-lambda-lists constraint-alist &body body)`,
where `constraint-alist` is an car/cdr alist mapping a type specifier to a list of variable
names of that type.   The variables will be implicitly declared in the body.
E.g.,

```lisp
  (destructuring-case '(1 2 :x 3)
    ((a b c) 
     (declare (type integer a b) (type symbol c))
     :first)
    ((a &optional (b 0) &key (x 0) (y 0)) 
     (declare (type integer a b x y))
     :second))
==> :second
```

* `destructuring-methods` -- A variant of `destructuring-case`, but allows a `call-next-method` feature.
The first matching clause is executed, if it calls the `call-next-method` function
then the next matching method is executed.
The `call-next-method function` defaults to `CALL-NEXT-METHOD`, but may be renamed
using the `:CALL-NEXT-METHOD` keyword argument.
E.g.

```lisp
(destructuring-methods '(1 2 3) (:call-next-method cnm)
  ((a b c)
   (declare (type number a b c))
   (* 2 (or (cnm) 1)))
  ((a b c)
   (declare (type fixnum a b c))
   3))
```

* `canonicalize-pattern` --  Given a regular-type-expression, return a canonical form.
This creates an `(:or ..)` of `(:and ...)` forms, and
removing or resolving redundant or trivial type designators.  E.g.,

```lisp
(canonicalize-pattern '(:and (:or A B C) D E (:or F G)))
==> (:OR (:AND A D E F)
         (:AND A D E G)
         (:AND B D E F)
         (:AND B D E G)
         (:AND C D E F)
         (:AND C D E G))
```

* `defrte` -- Declare a given RTE patter so that that it can be used when loaded from fasl.  E.g.,
```lisp
(defrte (:cat number number number))
(defrte (:+ (:cat keyword number)))
```

## Syntax of regular type expressions

* `:*` -- Match zero or more times.  E.g., `(rte (:* (:or string number)))` matches a
     sequence of zero or more objects each of which is either a `string` or a `number`.
* `:+` -- Match one or more times.  E.g., `(rte (:+ (:or string number)))` matches a
     sequence of one or more objects each of which is either a `string` or a `number`.
* `:?` -- Match zero or one time.  E.g., `(rte (:cat (:? string) number))` matches a either a sequence
of `string` `number` where the string is optional.  `("hello" 42)` or `(42)`.
* `:cat` -- Concatenate zero or more regular type expressions.  E.g., `(rte (:cat (:+ number) (:* string)))` matches a sequence of one or more numbers followed by zero or more strings.
* `:and` -- Match a sequence which simultaneously matches all the given regular typeexpressions.  E.g., `(rte (:and (:cat (:* number) float) (:cat float (:* number))))` matches a sequence of numbers which both ends with a `float` and also begins with a `float`.
* `:or` -- Match a sequence which matches any one the given regular expressions.  E.g., `(rte (:or (:cat (:* number) float) (:cat float (:* number))))` matches a sequence of numbers which either ends with a `float` or begins with a `float`.
* `:not` -- Matches a sequence which does not match the given regular type expression.  E.g., `(rte (:not (:+ number)))` matches a sequence unless it is a sequence of one or more numbers.
* `:empty-word` -- The empty word is a standard concept in
     rational languages.  Useful in combinations with `:or` and
     `:cat`.   E.g. `(:cat string (:or string :empty-word))` matches a sequence or one or two strings.
* `:empty-set` -- Does not match any words.  `:empty-set`
     is the identity element for `:or` and is useful for
     internal representations.  Not really useful for the end user.

## Code Examples

```lisp
(defun F4 (obj)
  (destructuring-case obj
    ((name &key count)
     (declare (type symbol name)
              (type integer count))
     ...)
    ((name data &rest strings)
     (declare (type symbol name)
              (type list data)
              (type (rte (:* string)) strings))
     ...)))
```

```lisp
(defun F (X Y)
  (declare
     (type (rte (:* (cons number)))
           Y))
  ...)
```

## ASDF

The ASDF :file designator is not sufficient for loading a lisp file
which contains rte type specifiers.  You must use :rte-file instead,
and include `:defsystem-depends-on (:rte)`
Here is an example.

```
(asdf:defsystem :rte-test
  ...
  :defsystem-depends-on (:rte)
  :components
  ((:module "src"
    :components
    ((:file "test-rte")
     (:file "test-list-of")
     (:rte-file "test-re-pattern")
     (:rte-file "test-destructuring-case-1")
     (:rte-file "test-destructuring-case-2")
     (:rte-file "test-destructuring-case")
     (:rte-file "test-ordinary-lambda-list")
     (:rte-file "test-rte-case")))))
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
