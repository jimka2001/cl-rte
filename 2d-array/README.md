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