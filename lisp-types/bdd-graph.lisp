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

(in-package   :lisp-types)


;;(pushnew :bdd-debug *features*)
(setf *features* (remove :bdd-debug *features*))

(defun bdd-graph-to-dot (graph out)
  (typecase out
    (string
     (let ((pathname (pathname out)))
       (cond ((string= "dot" (pathname-type pathname))
              (with-open-file (stream out :direction :output :if-exists :supersede :if-does-not-exist :create)
                (bdd-graph-to-dot graph stream)))
             ((string= "png" (pathname-type pathname))
              (let ((dot-path (merge-pathnames (make-pathname :type "dot") pathname)))
                (bdd-graph-to-dot graph (namestring dot-path))
                (run-program "dot" (list "-Tpng" (namestring dot-path)
                                         "-o" out)
                             :search t)))))
     out)
    ((eql t)
     (bdd-graph-to-dot graph *standard-output*))
    (null
     (with-output-to-string (str)
       (bdd-graph-to-dot graph str)))
    (stream
     (labels ((dnf (node)
                (bdd-to-dnf (getf node :bdd)))
              (print-head ()
                (print-comments)
                (format out "digraph G {~%")
                (format out "  rankdir=BT ;~%"))
              (print-foot ()
                (format out "}~%"))
              (print-comments ()
                (dolist (node graph)
                  (format out "// ~D " (getf node :id))
                  (write (dnf node) :stream out :pretty nil)
                  (format out "~%")))
              (print-node-defs ()
                (dolist (node graph)
                  (format out "  ~D ; // " (getf node :id))
                  (write (dnf node) :stream out :pretty nil)
                  (terpri out)))
              (print-touching ()
                (format out "  subgraph Rel1 {~%")
                (format out "    edge [dir=none, color=green]~%")
                (dolist (node graph)
                  (dolist (touch (getf node :touches))
                    ;; avoid connecting A->B and B-> A
                    (when (< (getf node :id) (getf touch :id))
                      (format out "    ~D -> ~D~%" (getf node :id) (getf touch :id)))))
                (format out "}~%"))
              (print-sub-super ()
                (format out "  subgraph Rel2 {~%")
                (format out "    edge [color=blue]~%")
                (dolist (node graph)
                  (dolist (super (getf node :super-types))
                    (format out "    ~D -> ~D~%" (getf node :id) (getf super :id))))
                (format out "}~%")))
       (print-head)
       (print-node-defs)
       (print-touching)
       (print-sub-super)
       (print-foot)))))
 





(defun count-connections-per-node (node)
  (+ (length (getf node :touches))
     (length (getf node :sub-types))
     (length (getf node :super-types))))

(defun count-parents-per-node (node)
  (length (getf node :super-types)))

(defun decompose-types-bdd-graph-strong (type-specifiers)
  (decompose-types-bdd-graph type-specifiers :bdd-hash-strength :strong))

(defun decompose-types-bdd-graph-weak (type-specifiers)
  (decompose-types-bdd-graph type-specifiers :bdd-hash-strength :weak))

(defun decompose-types-bdd-graph-weak-dynamic (type-specifiers)
  (decompose-types-bdd-graph type-specifiers :bdd-hash-strength :weak-dynamic))

(defun decompose-types-bdd-graph (type-specifiers &key ((:bdd-hash-strength *bdd-hash-strength*) :weak-dynamic)
                                                    ((:subtypep *subtypep*) *subtypep*))
  (decompose-by-graph-1 type-specifiers :graph-class 'bdd-graph))

(defun decompose-types-bdd-graph-baker (type-specifiers)
  (decompose-types-bdd-graph type-specifiers :subtypep #'baker:baker-subtypep))

