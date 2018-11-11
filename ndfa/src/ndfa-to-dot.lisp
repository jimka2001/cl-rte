;; Copyright (c) 2016,18 EPITA Research and Development Laboratory
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

(in-package :ndfa)

(defgeneric ndfa-to-dot (object stream &rest others &key state-legend transition-legend transition-abrevs view prefix title))

(defmethod ndfa-to-dot ((ndfa state-machine) stream &key (state-legend :dot) (transition-legend nil) transition-abrevs (view nil) prefix title)
  "Generate a dot file (for use by graphviz).  The dot file illustrates the states
and and transitions of the NDFA state machine.  The dot file is written to STREAM
which may be any valid first argument of FORMAT, but is usually t or a stream object.
TRANSITION-ABREVS (a car/cadr alist) mapping type specifiers to symbolic labels.
   If such a type specifier is found in the ndfa, and TRANSITION-LEGEND is true,
   then the name indicated in TRANSITION-ABREVS is used, otherwise a new symbolic name
   is generated.   This feature allows you to create multiple NDFA graphs using the
   same state transition lablels.
   If STATE-LEGEND is nil, then state labels in the graphical output will correspond to
   the (state-legend ...) of the state."
  (declare (ignore view)
	   (type (or null string) title)
	   (type (or (member t nil) stream))
	   (ignore prefix))
  (flet ((stringify (data)
	   (cond ((null data)
		  nil)
		 ((listp data)
		  (with-output-to-string (str)
		    (write data :case :downcase :stream str)))
		 (t
		  (with-output-to-string (str)
		    (write data :case :downcase :stream str)))))
	 (new-transition-name ()
	   (let ((transition-index 1)
		 (proposed-name "T1"))
	     (loop :while (rassoc proposed-name transition-abrevs :test #'equal :key #'car)
		   :do (progn (incf transition-index)
			      (setf proposed-name (format nil "T~d" transition-index))))
	     proposed-name)))
    (format stream "digraph G {~%")
    (when title
      (format stream "  labelloc=~S~%" "t")
      (format stream "  label=~S;~%" title))
    (format stream "  rankdir=LR;~%")
    (format stream "  graph [labeljust=l,nojustify=true];~%")
    (let ((state-map (make-hash-table :test #'equal))
	  (hidden 0))
      (let ((state-num 0))
	(dolist (state (reverse (states ndfa)))
	  (setf (gethash (state-label state) state-map) state-num)
	  (incf state-num)))
      (dolist (state (reverse (states ndfa)))
	(format stream "  /* ~D */~%" (gethash (state-label state) state-map))
	(unless state-legend
	  ;; if state-legend is nil, that means we tell graphvis to diplay the
	  ;;   name the states according to the state-label of the state.
	  (format stream "  ~D [label=\"~A\"]~%" (gethash (state-label state) state-map) (state-label state)))
	(when (state-initial-p state)
	  (format stream "    H~D [label=\"\", style=invis, width=0]~%" hidden)
	  (format stream "    H~D -> ~D;~%" hidden (gethash (state-label state) state-map))
	  (incf hidden))

	;; draw arrows from one state to the next for each transition.
	;; except if two arrows have the same source and destination,
	;; in which case draw one arrow with several comma separated labels.
	(let ((hash (make-hash-table :test #'equal)))
	  (dolist (transition (transitions state))
	    (push (transition-label transition) (gethash (next-label transition) hash nil)))
	  (maphash #'(lambda (next-label transition-labels)
		       (flet ((get-label (transition-label)
				(cond
				  ((not transition-legend)
				   (stringify transition-label))
				  (t
				   (unless (assoc transition-label transition-abrevs :test #'equal)
				     (push (list transition-label (new-transition-name))
					   transition-abrevs))
				   (cadr (assoc transition-label transition-abrevs :test #'equal))))))

			 (format stream "    ~D -> ~D [label=~S]~%"
				 (gethash (state-label state) state-map)
				 (gethash next-label state-map)
				 (with-output-to-string (str)
				   (format str "~A" (get-label (car transition-labels)))
				   (dolist (transition-label (cdr transition-labels))
				     (format str ",~A" (get-label transition-label)))))))
		   hash))
	(cond
	  ((null (state-final-p state))
	   nil)
	  ((state-exit-form state)
	   (format stream "    X~D [label=\"~A\", shape=rarrow]~%"
		   hidden (state-exit-form state))
	   (format stream "    ~D -> X~D ;~%" (gethash (state-label state) state-map) hidden)
	   (incf hidden))
	  (t
	   (format stream "    H~D [label=\"\", style=invis, width=0]~%" hidden)
	   (format stream "    ~D -> H~D ;~%" (gethash (state-label state) state-map) hidden)
	   (incf hidden))))
      (format stream "  labelloc = \"b\";~%")

      (case state-legend
	((:dot)
	 (format stream "  label = \"~A\\l" (or title ""))
	 (maphash #'(lambda (label num)
		      (format stream "~D = " num)
		      (write label :pretty nil :escape t :stream stream :case :downcase)
		      (format stream "\\l"))
		  state-map)
	 (format stream "\""))
	((nil) nil)
	(t
	 (maphash #'(lambda (label num)
		      ;; print state-legend to stdout
		      (format t "~D = " num)
		      (write label :pretty nil :escape t :stream t :case :downcase)
		      (format t "~%"))
		  state-map)))
      
      (when transition-legend
	(format stream "  label = \"\\l")
	(dolist (pair (reverse transition-abrevs))
	  (destructuring-bind (transition-label abbreviation) pair
	    (write abbreviation :pretty nil :escape nil :stream stream)
	    (write " = " :pretty nil :escape nil :stream stream)
	    (write transition-label :pretty nil :escape t :stream stream :case :downcase)
	    (format stream "\\l")
	    ))
	(format stream "\""))
      
      (format stream "}~%"))))

(defmethod ndfa-to-dot :around (ndfa (path string) &rest args)
  (apply #'ndfa-to-dot ndfa (pathname path) args))

(defmethod ndfa-to-dot :around (ndfa (path null) &rest args &key view prefix &allow-other-keys)
  "If NDFA-TO-DOT is called with NIL as second argument, and :VIEW T is give, then
the .dot file will be printed to a temporary file in /tmp (see MAKE-TEMP-FILE)."
  (if view ;; and path==nil
      (apply #'ndfa-to-dot ndfa
	     (make-temp-file-name (or prefix "dfa") :extension "png" :ensure-file-exists t)
	     args)
      (call-next-method)))

(defmethod ndfa-to-dot ((ndfa state-machine) (path pathname) &key (state-legend :dot) (transition-legend nil) transition-abrevs (view nil) prefix title)
  "Calling NDFA-TO-DOT with a PATH whose type is \"dot\" creates the dot file, which is valid input for the
graphviz dot program.   If PATH has type \"png\", a temporary dot file will be created, and
will be converted to a png file which will be displayed using open -n.  This works for MAC only."
  (cond ((string= "dot" (pathname-type path))
	 (with-open-file (stream path :direction :output :if-exists :rename)
	   (ndfa-to-dot ndfa stream :state-legend state-legend :transition-legend transition-legend :transition-abrevs transition-abrevs :view nil :prefix prefix :title title)))
	((string= "png" (pathname-type path))
	 (let ((dotpath (merge-pathnames (make-pathname :type "dot")  path)))
	   (ndfa-to-dot ndfa dotpath :state-legend state-legend :transition-legend transition-legend :transition-abrevs transition-abrevs :view nil :prefix prefix :title title)
	   (run-program *dot-path* (list "-Tpng" (namestring dotpath) "-o" (namestring path)))
	   #+:os-macosx (when view (run-program "open" (list "-n" (namestring path))))))
	(t
	 (error "invalid path ~A" path))))
