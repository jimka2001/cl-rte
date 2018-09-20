## Synopsis


Definition of the RTE CL type.  A type (and supporting functions) which implement rational type expressions.
      For information about this project and related publications , see [Efficient dynamic type checking of heterogeneous sequences](https://www.lrde.epita.fr/wiki/Publications/newton.16.rte.report)


## Code Examples

### RTE
```lisp
(defun F4 (obj)
  (destructuring-case obj
    ((name &key count) ((symbol name)
                        (integer count))
     ...)
    ((name data &rest strings) ((name symbol)
                                (data list)
                                (strings (rte (:* string))))
     ...)))
```

```lisp
(defun F (X Y)
  (declare
     (type (rte (:* (cons number)))
           Y))
  ...)
```

      
    