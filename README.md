# REGULAR-TYPE-EXPRESSION (RTE)

## Synopsis

This project contains several Common Lisp packages.  The packages fall into three categories.
* Usable to the general public.
* Used for analysis in PhD thesis.
* Used for testing other packages in. 

## Motivation

The implementation of rational type expression is the main result of this project.
However, several intermediate results might be useful as well, so they are made
available.


## Packages usable for the general public
### [cl-robdd](cl-robdd/README.md)

Implementation of ROBDD, Reduced Ordered Binary Decision Diagram

### [scrutiny](scrutiny/README.md)

Slime-friendly Unit Testing package, based loosely on lisp-unit (https://github.com/OdonataResearchLLC/lisp-unit). 

### [rte](rte/README.md)

Definition of the RTE CL type.  A type (and supporting functions) which implement rational type expressions.

### [rte-regexp](rte-regexp/README.md)

Simple string regular expression matcher based on rte
    
### [2d-array](2d-array/README.md)

Extensible sequence classes to represent vertical and horizontal "slices" of 2d arrays

### [lisp-types](lisp-types/README.md)

Utilities dealing with CL types

### [ndfa](ndfa/README.md)

Implementation of non-deterministed finite automata

### [adjuvant](adjuvant/README.md)

Certain utilities used in many other packages

### [dispatch](dispatch/README.md)

Implementation of the function `SPECIALIZER-INTERSECTION`
    


## Used for analsis in PhD thesis.
### research
### cl-robdd-analysis
### lisp-types-baker-analysis
### lisp-types-analysis

## Used for testing other packages.
### cl-robdd-test
### cl-robdd-analysis-test
### rte-test
### 2d-array-test
### lisp-types-test
### rte-regexp-test
### ndfa-test
### adjuvant-test
### scrutiny-test



## Installation

This code loads via asdf.
rte.asd loads the RTE system and its dependencies.
However, if you do not wish to use RTE, you may also use ndfa.asd, 2d-array.asd, or lisp-types.asd
as starting points.

## External dependencies

This code references several external asdf systems.  These may be found and installed using quicklisp or gitlab.lrde.epita.fr.

### cl-fad (quicklisp)
### closer-mop (quicklisp)
### subtypep (gitlab.lrde.epita.fr)
### bordeaux-threads (quicklisp)
### yacc (quicklisp)

## API Reference

to-be-done


## Tests

Testing is done using scrutiny.  You may load
the system code without the tests via rte.asd, ndfa.asd, 2d-array.asd, or lisp-types.asd.
But if you wish to run the tests, the starting points are respectively rte-test.asd, ndfa-test.asd, 2d-array-test.asd, and lisp-types-test.asd.
Within each corresponding subdirectory the files containing scrutiny test cases are all prefixed by "test-".
To run the tests, you'll need to use ASDF to load the corresponding asdf system definition, e.g.,

```lisp
CL-USER> (asdf:load-system :rte-test)
CL-USER> (in-package :rte-test)
TEST> (rte-test::test)
```


## Contributors
The majority of the code development has been done by Jim Newton, doctoral candidate at [UPMC](http://www.upmc.fr) [EPITA](http://www.epita.fr) [LRDE](https://www.lrde.epita.fr).



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
