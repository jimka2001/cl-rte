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


(defpackage :adjuvant
  (:use :cl)
  (:export
   "*DOT-PATH*"
   "*GNUPLOT-PATH*"
   "*TMP-DIR-ROOT*"
   "BFS-GRAPH"
   "BOOLEAN-EXPR-TO-LATEX"
   "CHANGE-EXTENSION"
   "CHOOSE-RANDOMLY"
   "CHOP-PATHNAME"
   "COMPARE-OBJECTS"
   "COUNT-1-BITS"
   "COUNT-BIT-DIFFS"
   "DEF-CACHE-FUN"
   "DEMAND-ENV-VAR"
   "DESTRUCTURING-DOLIST"
   "DESTRUCTURING-FLET"
   "DESTRUCTURING-LABELS"
   "DESTRUCTURING-LAMBDA"
   "DESTRUCTURING-LET"
   "DIFF-FILES"
   "DOLIST-TCONC"
   "EDGES-TO-ADJACENCY-HASH"
   "EMPTY-FILE-P"
   "ENCODE-TIME"
   "EXISTS"
   "EXISTS-TAIL"
   "FIND-DUPLICATES"
   "FIXED-POINT"
   "FORALL"
   "GARBAGE-COLLECT"
   "GETENV"
   "GETTER"
   "GNU-PLOT"
   "GROUP-BY"
   "GROUP-BY-EQUIVALENCE"
   "INSERT-SUFFIX"
   "LCONC"
   "LINEAR-REDUCE"
   "LOCATE-SYMBOL"
   "MAKE-TEMP-DIR"
   "MAKE-TEMP-FILE-NAME"
   "MAP-PAIRS"
   "MAP-PERMUTATIONS"
   "MAP-SUBSETS"
   "MATRIX-MULTIPLY"
   "PROCESS-KILL"
   "PROG1-LET"
   "REMFQ"
   "REPLACE-ALL"
   "RND-ELEMENT"
   "RUN-PROGRAM"
   "SETOF"
   "SHUFFLE-LIST"
   "SORT-UNIQUE"
   "TAKE-WHILE"
   "TCONC"
   "TOPOLOGICAL-SORT"
   "TREE-REDUCE"
   "TYPE-EXPAND"
   "UNIONF"
   "USER-READ"
   "VALID-TYPE-P"
   "WHILE"
))
