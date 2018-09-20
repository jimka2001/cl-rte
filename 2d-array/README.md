## Synopsis
Extensible sequence classes to represent vertical and horizontal "slices" of 2d arrays
## Code Examples
### 2D-ARRAY
```lisp
(let* ((arr (make-array '(3 2) :initial-contents '((1 2)
                                                   (3 4)
                                                   (5 6))))
         (row-vector (make-instance '2d-array:row-vector
                                    :2d-array arr :row 2))
         (column-vector (make-instance '2d-array:column-vector
                                       :2d-array arr :column 1))
         (vector-of-rows (make-instance '2d-array:vector-of-rows
                                        :2d-array arr))
         (vector-of-columns (make-instance '2d-array:vector-of-columns
                                           :2d-array arr)))
         
    ;; length
    (assert (= 2 (sequence:length vector-of-columns)))
    (assert (= 3 (sequence:length vector-of-rows)))
    (assert (= 2 (sequence:length row-vector)))
    (assert (= 3 (sequence:length column-vector)))


    ;; elt
    (assert (= 5 (sequence:elt row-vector 0)))
    (assert (= 6 (sequence:elt row-vector 1)))

    )
```
