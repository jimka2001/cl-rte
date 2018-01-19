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

(in-package :lisp-types.test)

#|

|#



(let ((lisp-types-test (find-package  :lisp-types.test))
      (lisp-types (find-package  :lisp-types)))
  (do-symbols (name :lisp-types)
    (when (and (eq lisp-types (symbol-package name))
               (not (find-symbol (symbol-name name) lisp-types-test)))
      (format t "1 importing name=~A into  :lisp-types.test~%" name)
      (shadowing-import name :lisp-types.test))))

;;(shadow-package-symbols)
;;(do-symbols (name :lisp-types)
;;  (shadowing-import name :lisp-types.test))

(defun locate-symbol (name)
  "Return a list of symbols which is a collection of symbols from all packages which have the given symbol-name"
  (let (symbols)
    (dolist (p (list-all-packages))
      (do-symbols (s p)
        (when (and (symbol-name s)
                   (string= name (symbol-name s)))
          (pushnew s symbols))))
    symbols))

(defun valid-subtypes (super)
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (and (valid-type-p sym)
                 (subtypep sym super))
	(push sym all-types)))
    (remove-duplicates all-types :test (lambda (t1 t2)
                                         (and (subtypep t1 t2)
                                              (subtypep t2 t1))))))



(defvar *cl-types* '(
                     arithmetic-error                  function            simple-condition           
                     array                             generic-function    simple-error               
                     atom                              hash-table          simple-string              
                     base-char                         integer             simple-type-error          
                     base-string                       keyword             simple-vector              
                     bignum                            list                simple-warning             
                     bit                               logical-pathname    single-float               
                     bit-vector                        long-float          standard-char              
                     broadcast-stream                  method              standard-class             
                     built-in-class                    method-combination  standard-generic-function  
                     cell-error                        nil                 standard-method            
                     character                         null                standard-object            
                     class                             number              storage-condition          
                     compiled-function                 package             stream                     
                     complex                           package-error       stream-error               
                     concatenated-stream               parse-error         string                     
                     condition                         pathname            string-stream              
                     cons                              print-not-readable  structure-class            
                     control-error                     program-error       structure-object           
                     division-by-zero                  random-state        style-warning              
                     double-float                      ratio               symbol                     
                     echo-stream                       rational            synonym-stream             
                     end-of-file                       reader-error        t                          
                     error                             readtable           two-way-stream             
                     extended-char                     real                type-error                 
                     file-error                        restart             unbound-slot               
                     file-stream                       sequence            unbound-variable           
                     fixnum                            serious-condition   undefined-function         
                     float                             short-float         unsigned-byte              
                     floating-point-inexact            signed-byte         vector                     
                     floating-point-invalid-operation  simple-array        warning                    
                     floating-point-overflow           simple-base-string                             
                     floating-point-underflow          simple-bit-vector    ))

(defmacro print-conditions (&body body)
  (let ((conditions (gensym "conditions")))
    `(let (,conditions)
       (handler-bind ((t #'(lambda (condition) (push condition ,conditions))))
         (prog1 (progn ,@body)
           (when ,conditions
             (let ((n 0))
               (format t "Conditions signalled while evaluating: ~A~%" ',body)
               (dolist (condition (nreverse ,conditions))
                 (format t "~D: ~S~%" (incf n) condition)))))))))

(defun call-asserting-conditions (thunk condition-types)
  (handler-bind ((t #'(lambda (condition)
                        (assert (member-if (lambda (c-type)
                                             (typep condition c-type))
                                           condition-types)
                                ()
                                "Evaluating expression raised invalid condition: ~A" condition))))
    (funcall thunk)))

(defmacro allowing-conditions (condition-types &body body)
  `(call-asserting-conditions ',condition-types (lambda () ,@body)))


(defun foo (G)
  (allowing-conditions (warning info db-timeout-error)
    (funcall G)))

;; (defmethod print-object ((c SB-KERNEL:PARSE-UNKNOWN-TYPE) stream)
;;   (print-unreadable-object (c stream :type t :identity nil)
;;     (when (slot-boundp c 'SB-KERNEL::specifier)
;;       (format stream " specifier=~A " (SB-KERNEL::parse-unknown-type-specifier c)))))

(defvar *number-combos*

  (let* ((l1 (shuffle-list (valid-subtypes 'number)))
         (l2 (shuffle-list (valid-subtypes 'number)))
         (l3 (loop for t1 in l1
                   for t2 in l2
                   nconc (list t1 `(and ,t1 (not ,t2)) `(or ,t1 ,t2)))))
    (setof e l3
      (not (subtypep e nil)))))



;;(length  *number-combos*)

(defvar *cl-type-combos*
  (loop for types on *cl-types*
        nconc (loop for t2 in (cdr types)
                 with t1 = (car types)
                 nconc (list t1 `(and ,t1 ,t2) `(or ,t1, t2)))))

(defvar *colors* 
 '(
    "000000" "ffff00" "1ce6ff" "ff34ff" "ff4a46" "008941" "006fa6" "a30059"
   ;;"ffdbe5"
   "7a4900" "0000a6" "63ffac" "b79762" "004d43" "8fb0ff" "997d87"
    "5a0007" "809693" "feffe6" "1b4400" "4fc601" "3b5dff" "4a3b53" "ff2f80"
    "61615a" "ba0900" "6b7900" "00c2a0" "ffaa92" "ff90c9" "b903aa" "d16100"
    "ddefff" "000035" "7b4f4b" "a1c299" "300018" "0aa6d8" "013349" "00846f"
    "372101" "ffb500" "c2ffed" "a079bf" "cc0744" "c0b9b2" "c2ff99" "001e09"
    "00489c" "6f0062" "0cbd66" "eec3ff" "456d75" "b77b68" "7a87a1" "788d66"
    "885578" "fad09f" "ff8a9a" "d157a0" "bec459" "456648" "0086ed" "886f4c"

    "34362d" "b4a8bd" "00a6aa" "452c2c" "636375" "a3c8c9" "ff913f" "938a81"
    "575329" "00fecf" "b05b6f" "8cd0ff" "3b9700" "04f757" "c8a1a1" "1e6e00"
    "7900d7" "a77500" "6367a9" "a05837" "6b002c" "772600" "d790ff" "9b9700"
    "549e79" "fff69f" "201625" "72418f" "bc23ff" "99adc0" "3a2465" "922329"
    "5b4534" "fde8dc" "404e55" "0089a3" "cb7e98" "a4e804" "324e72" "6a3a4c"
    "83ab58" "001c1e" "d1f7ce" "004b28" "c8d0f6" "a3a489" "806c66" "222800"
    "bf5650" "e83000" "66796d" "da007c" "ff1a59" "8adbb4" "1e0200" "5b4e51"
    "c895c5" "320033" "ff6832" "66e1d3" "cfcdac" "d0ac94" "7ed379" "012c58"
    ))


(defvar *decomposition-function-descriptors*
  (let ((color 0))
    `((:names (decompose-types) :max-num-types 15 :gnu-color ,(nth (incf color) *colors*) :color "blue" :legend t)
      (:names (decompose-types-rtev2) :max-num-types nil  :gnu-color ,(nth (incf color) *colors*) :color "olive" :legend t)
      (:names (decompose-types-sat)  :gnu-color ,(nth (incf color) *colors*) :color "dark-cyan"  :legend t)
      (:names (decompose-types-graph)  :gnu-color ,(nth (incf color) *colors*) :color "lavender" :legend t)
      (:names (bdd-decompose-types-strong)  :gnu-color ,(nth (incf color) *colors*) :color "orange" :legend t)
      (:names (bdd-decompose-types-weak) :gnu-color ,(nth (incf color) *colors*) :color "gold" :legend t)
      (:names ,*decompose-fun-names*  :gnu-color ,(nth (incf color) *colors*) :color "light-blue" :legend nil)
      (:names (decompose-types-bdd-graph-strong)  :gnu-color ,(nth (incf color) *colors*) :color "red" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph-weak)  :gnu-color ,(nth (incf color) *colors*) :color "rust" :linewidth 1  :legend t)
      (:names (local-minimum) :gnu-color "000000" :color "black" :linewidth 2 :legend t))))

(defvar *decomposition-functions*
  (set-difference (mapcan (lambda (plist)
                            (copy-list (getf plist :names))) *decomposition-function-descriptors*)
                  (cons 'local-minimum *decompose-fun-names*)))

(defun encode-time (time &aux (decoded-time (multiple-value-list (decode-universal-time time))))
  "Create a string similar to the UNIX date command: e.g., \"Thu Aug  3 10:39:18 2017\""
  (destructuring-bind (second minute hour date month year day-of-week ;; (0 = Monday)
                       daylight-savings-times ;; T (daylight savings times) or NIL (standard time)
                       timezone) decoded-time
    (declare (ignore timezone daylight-savings-times))
    (let ((day-of-week (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week))
          (month (aref #("no-month" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)))
      (with-output-to-string (str)
        (format str "~A ~A" day-of-week month)
        (format str " ~2D ~2D:~2,'0D:~2,'0D ~S" date hour minute second year)))))

(defun types/cmp-perfs (&key
                          (re-run t)
                          (verify nil)
                          (num-tries 2)
                          (suite-time-out (* 60 10))
                          (randomize nil)
                          (summary nil)
                          (limit 15)
                          sample
                          (types (choose-randomly (valid-subtypes 'number) limit))
                          (time-out nil)
                          tag
                          (decompose *decomposition-functions*)
                          normalize
                          hilite-min
                          (sorted-name "/dev/null")
                          (ltxdat-name "/dev/null")
                          (ltxdat-no-legend-name "/dev/null")
                          (sexp-name "/dev/null")
                          (gnuplot-name "/dev/null")
                          (gnuplot-normalized-name "/dev/null")
                          (png-name "/dev/null")
                          (png-normalized-name "/dev/null")
                          (dat-name "/dev/null"))
  (declare (type (or list (and symbol (satisfies symbol-function))) decompose))
  (let ((*package* (find-package "KEYWORD"))
        (start-time (get-universal-time))
        (fraction-completion 0)
        (time-out-time (+ (get-universal-time) suite-time-out))
        delayed)
    (labels ((handle (descr len thunk)
               (cond
                 ((and (getf descr :max-num-types)
                       (>= len (getf descr :max-num-types)))
                  nil)
                 (randomize
                  (push thunk delayed))
                 (t
                  (funcall thunk))))
             (log-data ()
               (print-report :re-run re-run
                             :include-decompose decompose
                             :dat-name dat-name
                             :ltxdat-name ltxdat-name
                             :ltxdat-no-legend-name ltxdat-no-legend-name
                             :sexp-name sexp-name
                             :gnuplot-name gnuplot-name
                             :gnuplot-normalized-name gnuplot-normalized-name
                             :png-name png-name
                             :png-normalized-name png-normalized-name
                             :sorted-name sorted-name
                             :summary summary
                             :normalize normalize
                             :time-out time-out
                             :limit limit
                             :hilite-min hilite-min
                             :tag tag))
             (run1 (f len types)
               (lambda (&aux results)
                 (when (> (get-universal-time) time-out-time)
                   (log-data)
                   (return-from types/cmp-perfs 'timed-out))
                 (let ((now (get-universal-time)))
                   (format t "    date:  ~A" (encode-time now))
                   (when (plusp fraction-completion)
                     (format t "  estim finish:  ~A" (encode-time
                                                      (truncate (+ start-time
                                                                   (/ (- now start-time)
                                                                      fraction-completion))))))
                   (terpri t))
                 (format t "function:  ~A~%" f)
                 (format t "   tag:    ~A~%" tag)
                 (format t "   length:  ~D~%" len)
                 (let ((result (types/cmp-perf :num-tries num-tries
                                               :types (choose-randomly types len)
                                               :decompose f
                                               :time-out time-out)))
                   (format t " run-time:  ~A~%" (getf result :run-time))
                   (when verify
                     (push result results)
                     (compare/results results)))))
             (make-gausian (len &aux (half (truncate len 2)))
               (+ 2 (random half)
                  (random half)))
             (run (types)
               (dolist (f (if (listp decompose)
                              decompose
                              (list decompose)))
                 (declare (type symbol f))
                 (let ((descr (find-decomposition-function-descriptor f)))
                   (loop :for len :from 2 :to limit
                         :do (let ((g (make-gausian len)))
                               (handle descr g (run1 f g types))
                               (handle descr len (run1 f len types))))))))
      (when re-run
        (run (choose-randomly types limit))
        (when randomize
          (let* ((c (length delayed))
                 (len c))
            (setf delayed (shuffle-list delayed))
            (while delayed
              (format t "sample: ~A~%" sample)
              (format t "countdown ~D/~D  ~D/~D~%" (decf c) len (- time-out-time (get-universal-time)) suite-time-out)
              (setf fraction-completion (- 1
                                           (min (/ c len)
                                                (/ (- time-out-time (get-universal-time)) suite-time-out))))
              (funcall (pop delayed))))))
      (garbage-collect)
      (log-data)))
  t)




(defun best-time (num-tries thunk)
  "returns a plist with the fields :wall-time :run-time :value"
  (declare (type (and fixnum unsigned-byte) num-tries)
           (type (function () t) thunk))
  (let (result)
    (dotimes (try num-tries)
      (let* ((run-time-t1 (get-internal-run-time))
             (start-real-time (get-internal-real-time))
             (s2 (funcall thunk))
             (run-time-t2 (get-internal-run-time))
             (wall-time (/ (- (get-internal-real-time) start-real-time) internal-time-units-per-second))
             (run-time (/ (- run-time-t2 run-time-t1) internal-time-units-per-second)))
        (setf result
              (cond
                ((not result) ; if first time through dotime/try loop
                 (list :wall-time (the real wall-time)
                       :run-time run-time
                       :value s2))
                ((< run-time (the real (getf result :run-time)))
                 (format t "[try ~D] found faster ~A < ~A~%" try run-time (getf result :run-time))
                 (list :wall-time wall-time
                       :run-time run-time
                       :value s2))
                (t
                 result)))))
    result))

(defun call-with-timeout (time-out thunk num-tries)
  "TIME-OUT, integer, the wall-time allowed to call the function THUNK.
 THUNK is a 0-ary function returning some type X.
 Call the function THUNK, in one thread, and start a 2nd observer thread.  The 2nd thread
 is responsible for monitoring the wall time and killing the 1st thread if the TIME-OUT has
 passed.  The function will be called num-tries times, and the minimum time is the
 value reported in the returned plist.
 If TIME-OUT is nil, then the function is not run in a separate thread, but rather 
 is run directly.
 Returns a plist, one of the following:
 (:wall-time rational :run-time rational :time-out integer) or
 (:wall-time rational :run-time rational :value X)"
  (declare (type (or null (and fixnum unsigned-byte)) time-out)
           (type (function () t) thunk)
           (type (and fixnum unsigned-byte) num-tries))
  (if (not time-out)
      (best-time num-tries thunk)
      (%call-with-timeout time-out thunk num-tries)))

#+allegro
(defun %call-with-timeout (time-out thunk num-tries)
  (declare (type (and fixnum unsigned-byte) time-out num-tries)
           (type (function () t) thunk))
  (let ((start-run-time (get-internal-run-time))
        (start-real-time (get-internal-real-time)))
    (sys:with-timeout (time-out (let ((run-time (get-internal-run-time))
                                      (real-time (get-internal-real-time)))
                                  (list :wall-time (/ (- real-time start-real-time) internal-time-units-per-second)
                                        :run-time  (/ (- run-time start-run-time) internal-time-units-per-second)
                                        :time-out time-out)))
      (best-time num-tries thunk))))


#+sbcl 
(defun %call-with-timeout (time-out thunk num-tries)
  (declare (type (and fixnum unsigned-byte) time-out num-tries)
           (type (function () t) thunk))
  (let (th-worker th-observer th-worker-join-failed th-observer-join-failed th-worker-destroyed-observer time-it-error result1 result2
                  (start-run-time (get-internal-run-time))
                  (start-real-time (get-internal-real-time)))
    (flet ((time-it ()
             ;; evaluate THUNK several times (according to NUM-TRIES)
             (handler-bind ((error (lambda (e)
                                     ;; this handler explicitly declines to handle the error
                                     ;; thus the variable TIME-IT-ERROR will be set as a side
                                     ;; effect, and th-observer will be destroyed, and the system will
                                     ;; continue to search for another handler, probably the
                                     ;; debugger.
                                     (setf time-it-error e)
                                     (when th-observer
                                       (warn "killing thread ~A because of error ~A" th-observer e)
                                       (ignore-errors (bordeaux-threads:destroy-thread th-observer))))))
               (setf result1 (best-time num-tries thunk))
               (when th-observer
                 ;; if we reach this line, that means the THUNK evaluated NUM-TRIES no of times before
                 ;;   TH-OBSERVER finshed.  So we need to kill TH-OBSERVER
                 (setf th-worker-destroyed-observer
                       (bordeaux-threads:destroy-thread th-observer))))))
      (setf th-observer
            (bordeaux-threads:make-thread
             (lambda (&aux elapsed (real-time (get-internal-real-time)) (run-time (get-internal-run-time)))
               (block waiting
                 (dotimes (i time-out)
                   (setf run-time (get-internal-run-time))
                   (setf real-time (get-internal-real-time))
                   (when (plusp (setf elapsed (/ (- real-time start-real-time) internal-time-units-per-second)))
                     (when (> elapsed time-out)
                       (return-from waiting)))
                   (sleep 2)))
               (setf result2 (list :wall-time (/ (- real-time start-real-time) internal-time-units-per-second)
                                   :run-time  (/ (- run-time start-run-time) internal-time-units-per-second)
                                   :time-out time-out))
               (format t "killing thread ~A~%" th-worker)
               (bordeaux-threads:destroy-thread th-worker))
             :name "th-observer stop-watch"))
      (setf th-worker (bordeaux-threads:make-thread #'time-it :name "th-handle thunk"))
      (handler-case (print-conditions (bordeaux-threads:join-thread th-worker))
        #+sbcl(SB-THREAD:JOIN-THREAD-ERROR (e)
                (setf th-worker-join-failed e)
                nil))
      (handler-case (bordeaux-threads:join-thread th-observer)
        #+sbcl(SB-THREAD:JOIN-THREAD-ERROR (e)
                (setf th-observer-join-failed e)
                nil)))
    (assert (typep (or result1 result2) 'cons)
            (th-worker th-observer th-worker-destroyed-observer time-it-error th-worker-join-failed th-observer-join-failed result1 result2 time-out))
    (the cons (or result1 result2))))

(defvar *perf-results* nil)
(defun types/cmp-perf (&key types (decompose 'bdd-decompose-types-weak) (time-out 15) (num-tries 2) &aux (f (symbol-function decompose)))
  (declare (type list types)
           (type symbol decompose)
           (type function f))
  (setf types (remove nil types))
  (cond
    ((null types)
     nil)
    ((exists plist *perf-results*
       (and (eq decompose (getf plist :decompose))
            (equal types (getf plist :types))))
     (format t "skipping duplicate ~A ~A~%" decompose types)
     nil)
    (t
     (garbage-collect)
     (let ((result (call-with-timeout time-out
                                      (lambda ()
                                        (funcall f types))
                                      num-tries))
           (num-unknown 0)
           (num-known 0))
       (declare (type (and unsigned-byte fixnum) num-known num-unknown))
       (dolist (t1 types)
         (dolist (t2 types)
           (dolist (t3 (list t1 `(not ,t1)))
             (dolist (t4 (list t2 `(not ,t2)))
               (if (nth-value 1 (subtypep t3 t4))
                   (incf num-unknown)
                   (incf num-known))))))
       (assert (or (typep (getf result :run-time) 'number )
                   (getf result :time-out)) (result))
       (destructuring-bind (&key time-out (run-time 0) (wall-time 0) value) result
         (declare (type (or null fixnum) time-out)
                  (type list value)
                  (type number run-time wall-time))
         (push
          (cond
            (time-out
             (format t "timed-out: ~D~%" time-out)
             (format t "given: ~D~%" (length types))
             (let ((*package* (find-package "KEYWORD")))
               (format t "given: ~S~%" types))
             ;; :known = number of elements of types X types for which A<B is known
             ;; :unknown = number of elements of types X types for which A<B is unknown
             (list :given (length types)
                   :types types
                   :decompose decompose
                   :known num-known
                   :unknown num-unknown
                   :wall-time (/ wall-time 1.0)
                   :run-time (/ run-time 1.0)
                   :time-out time-out))
            (t
             (list :given (length types)
                   :types types
                   :decompose decompose
                   :known num-known
                   :unknown num-unknown
                   :time (/ run-time 1.0)
                   :wall-time (/ wall-time 1.0)
                   :run-time (/ run-time 1.0)
                   :calculated (length value)
                   ;; :value value
                   )))
          *perf-results*))
       (car *perf-results*)))))


(defun get-all-types ()
  (set-difference (valid-subtypes t) '(t nil class built-in-class )))

(defun find-decomposition-function-descriptor (name)
  (typecase name
    (symbol
     (find-if (lambda (plist)
                (member name (getf plist :names))) 
              *decomposition-function-descriptors*))
    (string
     (find-if (lambda (plist)
                (exists f (getf plist :names)
                  ;; case independent search
                  (string-equal name (symbol-name f))))
              *decomposition-function-descriptors*))))

(defun find-decomposition-discrepancy (&optional (type-specs '(test-array-rank test-array-total-size bignum bit
                                                               complex fixnum float test-float-digits
                                                               test-float-radix integer number ratio rational real
                                                               test-char-code ;; char-int
                                                               double-float ;; long-float
                                                               unsigned-byte)))
  (labels ((recure ( type-specs)
             (when (cdr type-specs)
               (recure (cdr type-specs)))
             (format t "~%~%~%n = ~D~%~%~%~%" (length type-specs))
             (let* ((bdd-types (bdd-decompose-types type-specs))
                    (def-types (decompose-types type-specs))
                    (common (intersection bdd-types def-types :test #'equivalent-types-p))
                    (bdd-left-over (set-difference bdd-types common :test #'equivalent-types-p))
                    (def-left-over (set-difference def-types common :test #'equivalent-types-p)))
               (unless (= (length def-types)
                          (length bdd-types))
                 (format t "n=~D bdd=~D  def=~D~%" (length type-specs) (length bdd-types) (length def-types))
                 (format t " given  :~A~%" type-specs)
                 (format t " common :~A~%" common)
                 (format t "    bdd :~A~%" bdd-left-over)
                 (format t "    def :~A~%" def-left-over)
                 (dolist (com common)
                   (dolist (types (list bdd-left-over def-left-over))
                     (dolist (spec types)
                       (when (subtypep spec com)
                         (format t " ~A <: ~A~%" spec com))
                       (when (subtypep com spec)
                         (format t " ~A <: ~A~%" com spec)))))
                 (format t "checking calculated bdd types~%")
                 (check-decomposition type-specs bdd-types)
                 (format t "checking calculated def types~%")
                 (check-decomposition type-specs def-types)
                 (return-from find-decomposition-discrepancy nil)
                 ))))
    (recure type-specs)))

(defun compare/results (all-results &aux (good-results (setof res all-results
                                                         (and res
                                                              (null (getf res :time-out)))))
                                      (good-decomp (mapcar (lambda (plist) (getf plist :decompose)) good-results)))
  ;; results is a list of plists
  ;; each plist has one of two forms
  ;;  keys: (:types :given :decompose :time-out)
  ;;        or
  ;;        (:types :given :calculated :decompose :value :time)
  ;;   :value designates a list of calculated types according to the algorithm :decompose
  ;;   This function compare/results assures that the list of types is the same for each algorithm
  ;;   Ignoring ones which timed out, i.e., :time-out exists in the plist.
  ;;  If a difference is found, an attempt is made to find a smaller input list which also results
  ;;  in different types being calculated
  (labels ((equiv-type-sets (set1 set2)
             (and (= (length set1) (length set2))
                  (bdd-with-new-hash ()
                    (or (null (set-exclusive-or set1 set2 :test #'%equal))
                        (let ((bdd-set1 (bdd `(or ,@set1)))
                              (bdd-set2 (bdd `(or ,@set2))))
                          (and (eq *bdd-false* (bdd-and-not bdd-set1 bdd-set2))
                               (eq *bdd-false* (bdd-and-not bdd-set2 bdd-set1))))))))
           (%equal (t1 t2)
             (or (bdd-type-equal (bdd t1) (bdd t2))
                 (equivalent-types-p t1 t2)))
           (compare (res1 res2)
             (cond ((equiv-type-sets (getf res1 :value) (getf res2 :value)))
                   (t
                    (find-small-difference res1 res2))))
           (touching-pairs (types)
             (loop for tail on types
                   nconc (loop for type2 in (cdr types)
                               with type1 = (car types)
                               if (not (subtypep `(and ,type1 ,type2) nil))
                                 collect (list type1 type2))))
           (find-small-difference (res1 res2)
             (let ((*package* (find-package "KEYWORD")))
               (format t "found difference given=~D ~A=~D ~A=~D~%"
                       (length (getf res1 :types))
                       (getf res1 :decompose)
                       (length (getf res1 :value))
                       (getf res2 :decompose)
                       (length (getf res2 :value)))
               (let* ((smaller (find-smaller (getf res1 :types) (getf res1 :decompose) (getf res2 :decompose)))
                      (v1 (funcall (getf res1 :decompose) smaller))
                      (v2 (funcall (getf res2 :decompose) smaller))
                      (o1 (touching-pairs v1))
                      (o2 (touching-pairs v2))
                      (v1-v2 (bdd-and-not (bdd `(or ,@v1)) (bdd `(or ,@v2))))
                      (v2-v1 (bdd-and-not (bdd `(or ,@v2)) (bdd `(or ,@v2)))))

                 (dolist (pair o1)
                   (warn "~A touching pair: ~A~%" (getf res1 :decompose) pair))
                 (dolist (pair o2)
                   (warn "~A touching pairs: ~A~%" (getf res2 :decompose) pair))

                 (let ((msg (format nil "given=~A calculated~%   ~A=[~D]~A~%   vs ~A=[~D]~A~%  a\\b=~A~%  b\\a=~A~%   common=~A~%  a-b=~A~%  b-a=~A"
                                    smaller
                                    (getf res1 :decompose) (length v1) v1 
                                    (getf res2 :decompose) (length v2) v2
                                    (set-difference v1 v2 :test #'%equal)
                                    (set-difference v2 v1 :test #'%equal)
                                    (intersection v1 v2 :test #'%equal)
                                    v1-v2
                                    v2-v1)))
                   (warn msg)
                   (error msg)))))
           (find-smaller (given f1 f2 &aux v1 v2)
             (format t "searching for smaller error than ~S~%" given)
             (let ((ts (exists t1 given
                         (not (equiv-type-sets (setf v1 (funcall f1 (remove t1 given)))
                                               (setf v2 (funcall f2 (remove t1 given))))))))
               (cond
                 (ts
                  (format t "   found smaller difference given=~D~%" (1- (length given)))
                  (find-smaller (remove (car ts) given) f1 f2))
                 (t
                  given))))
           (check-1 (given-types calculated-types decompose-function)
             (when given-types
               (loop :for types :on calculated-types
                     :do (loop :for t2 :in (cdr types)
                               :with t1 = (car types)
                               :with bdd1 = (bdd (car types))
                               :do (let ((*package* (find-package "KEYWORD"))
                                         (bdd2 (bdd t2)))
                                     (unless (bdd-disjoint-types-p bdd1 bdd2)
                                       (dolist (type given-types)
                                         (let ((fewer (remove type given-types :test #'eq)))
                                           (check-1 fewer
                                                    (funcall decompose-function fewer)
                                                    decompose-function)))
                                       (error "Calculated touching types: ~S touches ~S~%Given types ~S~%Calculated: ~S~% Decomp ~S"
                                              t1 t2 given-types calculated-types good-decomp)))))
               (let* ((bdd-given (bdd `(or ,@given-types)))
                      (bdd-calc  (bdd `(or ,@calculated-types))))
                 (let ((*package* (find-package "KEYWORD")))
                   (unless (bdd-type-equal bdd-given bdd-calc)
                     (warn "found problem with ~S" given-types)
                     (dolist (type given-types)
                       (let ((fewer (remove type given-types)))
                         (warn "  checking with ~S" fewer)
                         (check-1 fewer (funcall decompose-function fewer) decompose-function)))
                     (error "Calculated types not equivalent to given types~% given: ~S~% calculated: ~S~% decompose: ~S~% calculated - given: ~S~% given - calculated: ~S"
                            given-types
                            calculated-types
                            decompose-function
                            (bdd-to-dnf (bdd-and-not bdd-calc bdd-given))
                            (bdd-to-dnf (bdd-and-not bdd-given bdd-calc)))))))))
    (when good-results
      (let ((res1 (car good-results)))
        (bdd-with-new-hash ()
          (check-1 (getf res1 :types) (getf res1 :value) (getf res1 :decompose))))
      
      (dolist (res (cdr good-results))
        (compare (car good-results) res)))))

(defun count-pairs (data predicate)
  (let ((c 0))
    (loop :for tail :on data
          :do (loop :for d2 :in tail
                    :with d1 = (car tail)
                    :do (when (funcall predicate d1 d2)
                          (incf c))))
    c))

(defun group-by (data &key key (test #'eql))
  (declare (type list data)
           (type (function (t) t) key)
           (type (function (t t) t) test))
  (let ((hash (make-hash-table :test test)))
    (dolist (item data)
      (push item (gethash (funcall key item) hash nil)))
    (loop for key being the hash-keys of hash
          collect (list key (gethash key hash)))))

(defun build-string (delimiter data)
  (cond
    ((null data)
     "")
    (t
     (with-output-to-string (str)
       (format str "~A" (car data))
       (dolist (next (cdr data))
         (format str "~A~A" delimiter next))))))

(defun create-gnuplot (sorted-file gnuplot-file png-filename normalize hilite-min include-decompose key)
  (declare (type (member :smooth :xys) key))
  (let ((content (with-open-file (stream sorted-file :direction :input)
                   (format t "reading    ~A~%" sorted-file)
                   (read stream nil nil))))
    (with-open-file (stream gnuplot-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format t "[writing to ~A~%" gnuplot-file)
      (destructuring-bind (&key summary sorted &allow-other-keys &aux min-curve min-curve-line-style) content
        (assert (typep sorted 'cons))
        ;; sort DATA so that the order agrees with *decomposition-function-descriptors*
        (setf sorted (mapcan (lambda (desc &aux (names (getf desc :names)))
                               (setof plist sorted
                                 (exists name names
                                   (string= (getf plist :decompose) (symbol-name name)))))
                             (setof plist *decomposition-function-descriptors*
                               (exists name include-decompose
                                 (member name (getf plist :names))))))
        ;; if we are trying to normalize, we need at least two points in the graph
        ;;    so in this case remove DATA which has less that 2 points to plot
        (when normalize
          (setf sorted (setof plist sorted
                         (cdr (getf plist key)))))
        (when hilite-min
          (setf min-curve (reduce (lambda (curve1 curve2)
                                    (if (< (getf curve1 :integral)
                                           (getf curve2 :integral))
                                        curve1
                                        curve2)) (cdr sorted) :initial-value (car sorted)))
          (assert (typep min-curve 'cons))
          (when (cdr (getf (find-decomposition-function-descriptor (getf min-curve :decompose)) :names))
            ;; this is one of the 45 curves of the parameterized functions
            (setf min-curve `(:decompose "LOCAL-MINIMUM" ,@min-curve))))
        (format stream "# summary ~A~%" summary)
        (format stream "set term png~%")
        (format stream "set key below~%") ;; enable legend
        (format stream "set title ~S~%" summary)
        (unless normalize
          (format stream "set logscale xy~%"))
        (labels ((xys (curve)
                   (declare #+sbcl (notinline sort))
                   (remove-duplicates (sort (copy-list (getf curve key))
                                            (lambda (a b)
                                              (if (= (car a) (car b))
                                                  (< (cadr a) (cadr b))
                                                  (< (car a) (car b)))))
                                      :test #'equal)))
          (let* ((line-style 0)
                 (mapping (mapcar (lambda (descr)
                                    (incf line-style)
                                    (format stream "set style line ~D linewidth ~D linecolor rgb \"#~A\"~%"
                                            line-style
                                            (or (getf descr :linewidth) 1)
                                            (getf descr :gnu-color))
                                    ;; collect
                                    `(:line-style ,line-style ,@descr))
                                  *decomposition-function-descriptors*))
                 (normalize-to-xys (when normalize
                                     (let ((plist (or (find (symbol-name normalize) sorted
                                                            :key (getter :decompose) :test #'string=)
                                                      (error "cannot find :decompose ~A~%   with key=~A~%  with include-decompose=~A~%  in sorted=~A"
                                                             (symbol-name normalize)
                                                             key
                                                             include-decompose
                                                             sorted))))
                                       (or (xys plist)
                                           (error "failed to compute points from sorted=~A" plist))))))
            (assert (or (not normalize) normalize-to-xys)
                    (normalize normalize-to-xys sorted))
            (incf line-style)
            (when hilite-min
              (setf min-curve-line-style line-style)
              (format stream "set style line ~D linewidth 2 linecolor rgb ~S~%" min-curve-line-style "black"))
            (labels ((interpolate-y (x0 x1 y1 x2 y2)
                       (+ y1 (* (- y2 y1)
                                (/ (float (- x0 x1)) (- x2 x1)))))
                     (interpolate (x)
                       (declare (type integer x))
                       (mapl (lambda (tail &aux (xy1 (car tail)) (xy2 (cadr tail)) (xy-tail (cddr tail)))
                               ;; normalize-to-xys is a list of x-y pairs already ordered by x value, with no duplicate x's
                               ;; for the given x, interpolate the value of y
                               ;; 1) if x < the min x from normalize-to-xys
                               ;; 2) if x is found exactly in normalize-to-xys
                               ;; 3) if x is strictly between two x's from normalize-to-xys
                               ;; 4) if x > the max x from normalize-to-xys
                               (assert xy2 (x normalize tail normalize-to-xys)
                                       "interpolation failed to find x=~A" x)
                               (cond
                                 ((or (< x (car xy1)) ; case 1
                                      (and xy2        ; case 3
                                           (> x (car xy1))
                                           (< x (car xy2)))
                                      (and xy2 ; case 4
                                           (null xy-tail)
                                           (> x (car xy2))))
                                  (return-from interpolate (interpolate-y x (car xy1) (cadr xy1) (car xy2) (cadr xy2))))
                                 ((= x (car xy1)) ; case 2
                                  (return-from interpolate (cadr xy1)))
                                 ((and xy2
                                       (= x (car xy2))) ; case 2
                                  (return-from interpolate (cadr xy2)))))
                             normalize-to-xys))
                     (plot-curve (curve)
                       (destructuring-bind (&key decompose
                                            &allow-other-keys
                                            &aux (color (or (getf (find-decomposition-function-descriptor decompose) :gnu-color)
                                                            (getf (find-decomposition-function-descriptor decompose) :color))))
                           curve
                         (format stream "# ~A ~A~%" color decompose)
                         (dolist (pair (xys curve))
                           (destructuring-bind (x y) pair
                             (cond
                               (normalize
                                (format stream "~A ~A~%" x (- y (the number (interpolate x)))))
                               ((zerop y))
                               (t
                                (format stream "~A ~A~%" x y)))))
                         (format stream "end~%"))))
              (format stream "plot ~A"
                      (build-string (format nil ",\\~%")
                                    (mapcar (lambda (data-plist &aux (decompose (getf data-plist :decompose)))
                                              (let ((mapping-plist (find-if (lambda (mapping-plist)
                                                                              (exists name (getf mapping-plist :names)
                                                                                (string= decompose
                                                                                         (symbol-name name))))
                                                                            mapping)))
                                                (with-output-to-string (str)
                                                  (format str "   \"-\" using 1:2 with lines ls ~D"
                                                          (getf mapping-plist :line-style))
                                                  (format str " title ~S" (string-downcase decompose)))))
                                            sorted)))
              (when hilite-min
                (format stream ",\\~%\   \"-\" using 1:2 with lines ls ~D" min-curve-line-style)
                (format stream " title ~S~%" "unknown"))
              (mapc #'plot-curve sorted)
              (when hilite-min
                (plot-curve min-curve))))))
      (format t "~A ]~%" gnuplot-file))

    (run-program "gnuplot" (list gnuplot-file)
                 :search t 
                 :output png-filename
                 :error *error-output*
                 :if-output-exists :supersede)))

(defun integral (xys)
  "given a list of xy pairs (car cadr) calculate the area under the curve formed by the trapizoids.
the list of xys need not be already ordered."
  (let ((acc 0.0)
        (sorted (sort (copy-list xys) (lambda (p1 p2 &aux (x1 (car p1)) (x2 (car p2)) (y1 (cadr p1)) (y2 (cadr p2)))
                                        (cond ((= x1 x2)
                                               (< y1 y2))
                                              (t
                                               (< x1 x2)))))))
    (mapl (lambda (xys-tail)
            (destructuring-bind ((x1 y1) &optional ((x2 y2) '(nil nil)) &rest tail) xys-tail
              (declare (ignore x1 x2))
              (when tail
                ;; increment by trapizoid area
                (incf acc (* 1.0 ;;(- x2 x1)
                             (/ (+ y2 y1) 2))))))
          sorted)
    (when (minusp acc)
      (warn "negative integral ~A: ~A~%" acc sorted))
    (/ acc (length xys))))

(defun statistics (data)
  (destructuring-bind (&key summary
                         time-out-count
                         run-count
                         time-out-run-time
                         run-time
                         wall-time
                         limit
                         unknown
                         known
                         sorted &allow-other-keys) data
    (let ((integrals (mapcar (getter :integral) sorted)))
      (destructuring-bind (&key count sum) (reduce (lambda (accum this)
                                                     (destructuring-bind (&key count sum) accum
                                                       (list :count (1+ count) :sum (+ this sum)))) integrals
                                                       :initial-value '(:count 0 :sum 0))
        (labels ((sqr (x) (* x x))
                 (stdev (count mean data)
                   (sqrt (/ (reduce (lambda (sum this)
                                      (+ sum (sqr (- this mean))))
                                    data :initial-value 0.0)
                            count))))
          (let* ((mean (unless (zerop count)
                         (/ sum count)))
                 (sigma (when mean
                          (stdev count mean integrals)))
                 previous)
            (list :summary summary
                  :time-out-count time-out-count
                  :run-count run-count
                  :time-out-run-time time-out-run-time
                  :run-time run-time
                  :wall-time wall-time
                  :limit limit
                  ;; :known = number of elements of types X types for which A<B is known
                  ;; :unknown = number of elements of types X types for which A<B is unknown
                  :unknown unknown
                  :known known
                  :sigma sigma
                  :mean mean
                  :sorted (mapcar (lambda (plist)
                                    (destructuring-bind (&key integral samples decompose arguments xys smooth &allow-other-keys
                                                         &aux (score (unless (member sigma '(0 0.0 nil))
                                                                       (/ (- integral mean) sigma)))) plist
                                    (list :integral integral
                                          :score score
                                          :delta (when score
                                                   (prog1 (when previous
                                                            (- previous score))
                                                     (setf previous score)))
                                          :samples samples
                                          :decompose decompose
                                          :arguments arguments
                                          :smooth smooth
                                          :xys xys)))
                                sorted))))))))

(defun append-tail (l1 l2 value)
  (append l1 (mapcar (constantly value) (nthcdr (length l1) l2))))

;; (append-tail '(a b c d) '(0 0 0 0 1 2 3 4 5 6 7) -1)
;;(append-tail '(0 0 0 0 1 2 3 4 5 6 7) '(a b c d) -1)



;; (reduce (lambda (string num) (format nil "~D~S" num string)) '(1 2 3) :initial-value "")


(defun smoothen-2 (xys)
  "takes a list of (x y) pairs and returns a new list of (x y) pairs with some of the noise filtered out.
The smoothening method is simply a weighted averages of the 2 closest neighbors below and the 2 closest neighbors above."
  (let* ((weights '(1 2 5 2 1))
         (weight-sum (apply #'+ weights))
         (num-weights (length weights))
         (mid-index (truncate num-weights 2))
         (data (list nil nil)))
    (dotimes (i mid-index)
      (tconc data (nth i xys)))
    (while (nthcdr num-weights xys)
      (tconc data (list (car (nth mid-index xys))
                        (/ (float (reduce #'+ (mapcar (lambda (weight xy)
                                                        (* weight (cadr xy))) weights xys))) weight-sum)))
      (pop xys))
    (pop xys)
    (pop xys)
    (dotimes (i mid-index)
      (tconc data (nth i xys)))
    (car data)))


(defun smoothen (xys)
  "Takes a list of (x y) pairs and returns a new list of (x y) pairs with some of the noise filtered out.
The smoothening method is an average of all the points in an x-radius of the given point, 
i.e., of all the points whose xcoord is between x/2 and x*2."
  (flet ((between (x y z)
           (and (<= x y)
                (<= y z)))
         (mean (xs)
           (/ (reduce #'+ xs :initial-value 0.0)
              (float (length xs)))))

    (let* ((radius 2))
      (loop :for xy :in xys
            :for x0 = (car xy)
            :for close = (loop :for sample :in xys
                               :when (between (/ x0 radius)
                                              (car sample)
                                              (* x0 radius))
                                 :collect (cadr sample))
            :collect (list x0 (mean close))))))

(defun sort-results (in out &rest options)
  (declare #+sbcl (notinline sort))
  (cond
    ((eq in nil) nil)
    ((typep in '(or pathname string))
     (format t "[reading from ~A~%" in)
     (with-open-file (stream in :direction :input :if-does-not-exist nil)
       (prog1 (apply #'sort-results stream out options)
         (format t "]~%"))))
    ((typep out '(or pathname string))
     (ensure-directories-exist out)
     (with-open-file (stream out :direction :output :if-exists :supersede :if-does-not-exist :create)
       (format t "[writing to ~A~%" out)
       (prog1 (apply #'sort-results in stream options)
         (format t "]~%"))))
    ((eq out :return)
     ;; (reduce (lambda (num string) (format nil "~D~S" num string)) '(1 2 3) :initial-value "")
     (destructuring-bind (&key summary limit data time-out-count run-count time-out-run-time run-time wall-time unknown known)
         (read in nil nil)
       (let (observed-max-time observed-min-time observed-min-prod observed-max-prod)
         (dolist (plist data)
           (mapc (lambda (given calculated time)
                   (setf observed-max-prod (if observed-max-prod
                                               (max observed-max-prod (* given calculated))
                                               (* given calculated)))
                   (setf observed-min-prod (if observed-min-prod
                                               (min observed-min-prod (* given calculated))
                                               (* given calculated)))
                   (setf observed-max-time (if observed-max-time
                                               (max observed-max-time time)
                                               time))
                   (setf observed-min-time (if observed-min-time
                                               (min observed-min-time time)
                                               time)))
                 (getf plist :given) (getf plist :calculated) (getf plist :run-time)))
         (statistics
          (list :summary summary
                :time-out-count time-out-count
                :run-count run-count
                :time-out-run-time time-out-run-time
                :run-time run-time
                :wall-time wall-time
                :limit limit
                ;; :known = number of elements of types X types for which A<B is known
                ;; :unknown = number of elements of types X types for which A<B is unknown
                :unknown unknown
                :known known
                :sorted (sort 
                         (loop for plist in data
                               collect (destructuring-bind (&key decompose given calculated run-time &allow-other-keys) plist
                                         (flet ((cmp (a b)
                                                  (if (= (car a) (car b))
                                                      (< (cadr a) (cadr b))
                                                      (< (car a) (car b)))))
                                           (let* ((xys (mapcar (lambda (given calculated run-time)
                                                                 (declare (type fixnum given calculated)
                                                                          (type number run-time))
                                                                 (list (* given calculated) run-time)) given calculated run-time))
                                                  (xys-extended xys))
                                             (unless (find observed-min-prod xys-extended :key #'car)
                                               (push (list observed-min-prod observed-min-time) xys-extended))
                                             (unless (find observed-max-prod xys-extended :key #'car)
                                               (push (list observed-max-prod observed-max-time) xys-extended))
                                             ;; if two adjacent points have the same x, remove the second, because
                                             ;;  its y value is larger, thanks to the sort #'cmp
                                             (setf xys (remove-duplicates (sort (copy-list xys) #'cmp)
                                                                          :key #'car :from-end t))
                                             (setf xys-extended (remove-duplicates (sort (copy-list xys-extended) #'cmp)
                                                                                   :key #'car :from-end t))
                                             (list :integral (integral xys-extended)
                                                   :xys xys
                                                   :smooth (smoothen xys)
                                                   :samples (length xys)
                                                   :decompose decompose
                                                   :arguments (get (intern decompose (find-package :lisp-types))
                                                                   'decompose-properties))))))
                         #'< :key (lambda (obj) (getf obj :integral))))))))
    (t
     (let ((*package* (find-package "CL")))
       (format out "~S~%" (apply #'sort-results in :return options))))))

(defun print-sexp (stream summary limit)
  (let ((groups (group-by (cdr *perf-results*) :key (getter :decompose))))
    (format stream "(")
    (when summary
      (format stream " :SUMMARY ~S~%" summary))
    (format stream "  :LIMIT ~D~%" limit)
    (format stream "  :TIME-OUT-COUNT ~D~%" (count-if (getter :time-out) *perf-results*))
    (format stream "  :RUN-COUNT ~D~%" (count-if (getter :calculated) *perf-results*))
    (format stream "  :TIME-OUT-RUN-TIME ~A~%" (reduce (lambda (total next)
                                                         (if (getf next :time-out)
                                                             (+ total (getf next :run-time))
                                                             total)) *perf-results*
                                                             :initial-value 0))
    (format stream "  :RUN-TIME ~A~%" (reduce (lambda (total next)
                                            (if (getf next :calculated)
                                                (+ total (getf next :run-time))
                                                total))
                                              *perf-results*
                                              :initial-value 0))
    (format stream "  :WALL-TIME ~A~%" (reduce (lambda (total next)
                                            (if (getf next :calculated)
                                                (+ total (getf next :wall-time))
                                                total))
                                               *perf-results*
                                               :initial-value 0))
    (format stream " :DATA  (")
    (flet ((print-group (group)
             (destructuring-bind (decompose data) group
               (format stream "( :DECOMPOSE ~S" (symbol-name decompose))
               (when (get decompose 'lisp-types::decompose-properties)
                 (let ((*package* (find-package "KEYWORD")))
                   (format stream "~%:OPTIONS ~S" (get decompose 'lisp-types::decompose-properties))))
               (let ((time-outs (setof item data
                                  (getf item :time-out)))
                     (no-time-out (setof item data
                                    (null (getf item :time-out)))))
                 (when time-outs
                   (format stream "~%:TIME-OUT (:TIME ~D :COUNT ~D~%"
                           (getf (car time-outs) :time-out) 
                           (length time-outs))
                   (format stream ":GIVEN ~A)"
                           (mapcar (getter :given) time-outs)))
               (dolist (tag `(:given :calculated :run-time :wall-time :known :unknown))
                   (format stream "~%~S (" tag)
                   (when no-time-out
                     (format stream "~S" (getf (car no-time-out) tag)))
                   (dolist (item (cdr no-time-out))
                     (format stream " ~A" (getf item tag)))
                   (format stream ")")))
               (format stream ")~%"))))
      (when (car groups)
        (print-group (car groups)))
      (dolist (group (cdr groups))
        (format stream " ")
        (print-group group)))
    (format stream "))~%"))
  nil)

(defun print-ltxdat (ltxdat-name sorted-name include-decompose legendp tag)
  (let ((content (with-open-file (stream sorted-name :direction :input :if-does-not-exist :error)
                   (read stream))))
    (destructuring-bind (&key sorted &allow-other-keys) content
      (ensure-directories-exist ltxdat-name)
      (with-open-file (stream ltxdat-name :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format t "writing to ~A~%" ltxdat-name)
        (format stream "\\begin{tikzpicture}~%")
        (format stream "\\begin{axis}[~% ")
        (when tag
          (format stream "title=~A,~% " tag))
        (format stream "xlabel=Size,~% ylabel=Time,~% xmode=log,~% ymode=log,~% legend style={at={(0.5,-0.2)},anchor=north},~% xmajorgrids,~% xminorgrids,~% ymajorgrids,~% legend style={font=\\tiny},~% xticklabel style={font=\\tiny},~% yticklabel style={font=\\tiny},~% label style={font=\\tiny}~%]~%")
        (let ((*print-case* :downcase)
              (min-curve (reduce (lambda (curve1 curve2)
                                  (if (< (getf curve1 :integral)
                                         (getf curve2 :integral))
                                      curve1
                                      curve2)) (cdr sorted) :initial-value (car sorted)))
              legend)
          (setf min-curve (if (cdr (getf (find-decomposition-function-descriptor (getf min-curve :decompose)) :names))
                              ;; this is one of the 45 curves of the parameterized functions
                              `(:decompose "LOCAL-MINIMUM" ,@min-curve)
                              nil))
          (flet ((plot (xys decompose descr)
                   (let* ((decimal (parse-integer (getf descr :gnu-color) :radix 16))
                          (red   (/ (logand decimal #xff0000) #x10000))
                          (green (/ (logand decimal #x00ff00) #x100))
                          (blue  (logand decimal #x0000ff)))
                     (format stream "\\definecolor{color~A}{RGB}{~A,~A,~A}~%" (getf descr :gnu-color) red green blue)
                     (format stream "\\addplot[color=color~A] coordinates {~%" (getf descr :gnu-color)))
                   (dolist (xy xys)
                     (format stream "(~D, ~S)~%" (car xy) (cadr xy)))
                   (format stream "};~%")
                   (push (if (getf descr :legend)
                             (format nil "~A" decompose)
                             "") legend)))
            (dolist (descr *decomposition-function-descriptors*)
              (dolist (curve sorted)
                (destructuring-bind (&key decompose xys &allow-other-keys
                                     &aux (name (find-if (lambda (name)
                                                           (string= (symbol-name name) decompose))
                                                         include-decompose))
                                       (descr2 (find-decomposition-function-descriptor name)))
                    curve
                  ;; decompose is a string such as "DECOMPOSE-TYPES-BDD-GRAPH"
                  ;; include-decompose contains symbols such as DECOMPOSE-TYPES-BDD-GRAPH
                  (when (and (eq descr descr2) name)
                    (plot xys decompose descr)))))
            (when min-curve
              (let ((descr (find-decomposition-function-descriptor (getf min-curve :decompose))))
                (plot (getf min-curve :xys)
                      (getf min-curve :decompose)
                      descr))))
          (format stream "~A\\legend{~A};~%" (if legendp "" "%% ") (build-string ", " (reverse legend)))
          (format stream "~A\\legend{};~%" (if legendp "%% " "")))
        (format stream "\\end{axis}~%")
        (format stream "\\end{tikzpicture}~%")))))
            
(defun print-dat (dat-name include-decompose)
  (with-open-file (stream dat-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "given calculated sum product touching disjoint disjoint*given touching*given new time decompose~%")    
    (let ((domain (remove-duplicates *perf-results*
                                     :test #'equal
                                     :key #'(lambda (plist)
                                              (list (getf plist :decompose)
                                                    (getf plist :types))))))
      (dolist (plist domain)
        (destructuring-bind (&key given calculated decompose value types time time-out &allow-other-keys) plist
          (when time-out
            (setf time time-out))
          (unless calculated
            (let ((hit (find-if (lambda (plist)
                                  (equal given (getf plist :given)))
                                domain)))
              (setf calculated (getf hit :calculated))))
           
          (cond
            ((or (not given) (not calculated)))
            ((< time 0.0011))
            ((= 0 time))
            ((member decompose include-decompose)
             (let ((*print-case* :downcase)
                   (num-disjoint (count-pairs types #'disjoint-types-p))
                   (num-touching (count-pairs types #'(lambda (x y)
                                                        (not (disjoint-types-p x y))))))
               (format stream "~D ~D ~D ~D ~D ~D ~D ~D ~D ~S ~A~%"
                       given calculated
                       (+ given calculated)    ;; sum
                       (* given calculated)    ;; product
                       num-touching            ;; touching
                       num-disjoint            ;; disjoint
                       (* num-disjoint given)  ;; disjoint * given
                       (* num-touching given)  ;; touching * given
                       (length (set-difference value types :test #'equivalent-types-p)) ;; new
                       time decompose)))))))))


(defun insert-suffix (filename suffix)
  "insert the given SUFFIX before the filename extension: 
 E.g., (insert-suffix \"/path/to/file.gnu\" \"-smooth\") --> \"/path/to/file-smooth.gnu\""
  ;; find the final "."
  (let ((index (search "." filename :from-end t)))
    (when index
      (let ((tail (subseq filename index))
            (head (subseq filename 0 index)))
        (concatenate 'string head suffix tail)))))


(defun print-report (&key (re-run t) limit (summary nil) normalize (dat-name "/dev/null") (ltxdat-name "/dev/null") (ltxdat-no-legend-name "/dev/null") (sorted-name "/dev/null") (sexp-name "/dev/null") (png-name "/dev/null") (png-normalized-name "/dev/null") (gnuplot-name "/dev/null") (gnuplot-normalized-name "/dev/null") (include-decompose *decomposition-functions*) (tag "NO TITLE") (hilite-min nil) &allow-other-keys)
  (format t "report ~A~%" summary)
  (when re-run
    (ensure-directories-exist sexp-name)
    (with-open-file (stream sexp-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format t "writing to ~A~%" sexp-name)
      (print-sexp stream summary limit)))
  (sort-results sexp-name sorted-name)
  (unless (string= sorted-name "/dev/null")
    (create-gnuplot sorted-name gnuplot-name png-name
                    nil hilite-min include-decompose :xys)
    (create-gnuplot sorted-name (insert-suffix gnuplot-name "-smooth") (insert-suffix png-name "-smooth")
                    nil hilite-min include-decompose :smooth)
    
    (when normalize
      (create-gnuplot sorted-name gnuplot-normalized-name png-normalized-name
                      normalize hilite-min include-decompose :xys)
      (create-gnuplot sorted-name (insert-suffix gnuplot-normalized-name "-smooth") (insert-suffix png-normalized-name "-smooth")
                      normalize hilite-min include-decompose :smooth))
    (print-ltxdat ltxdat-name           sorted-name include-decompose t nil)
    (print-ltxdat ltxdat-no-legend-name sorted-name include-decompose nil tag))
  (print-dat dat-name include-decompose))

(defvar *destination-dir* "/Users/jnewton/newton.16.edtchs/src")
(defun test-report (&key sample (prefix "") (re-run t) (suite-time-out (* 60 60 4))  (time-out (* 3 60)) normalize (destination-dir *destination-dir*)
                      types file-name (limit 15) tag hilite-min (num-tries 2)
                    &allow-other-keys)
  "TIME-OUT is the number of seconds to allow for one call to a single decompose function.
SUITE-TIME-OUT is the number of time per call to TYPES/CMP-PERFS."
  (when re-run
    (setf *perf-results* nil))
  (let ((type-specifiers (shuffle-list types)))
    (apply #'types/cmp-perfs :re-run re-run
                             :types type-specifiers
                             :suite-time-out suite-time-out
                             :randomize t
                             :limit (truncate limit)
                             :tag tag
                             :summary file-name
                             :time-out time-out
                             :decompose  *decomposition-functions*
                             :normalize normalize
                             :sample sample
                             :num-tries num-tries
                             :hilite-min hilite-min
                             (when file-name
                               (list
                                :ltxdat-name (format nil "~A/~A~A.ltxdat" destination-dir prefix file-name)
                                :ltxdat-no-legend-name (format nil "~A/no-legend/~A~A.ltxdat" destination-dir prefix file-name)
                                :dat-name (format nil "~A/~A~A.dat" destination-dir prefix file-name)
                                :png-name (format nil "~A/~A~A.png" destination-dir prefix file-name)
                                :png-normalized-name (format nil "~A/~A~A-normalized.png" destination-dir prefix file-name)
                                :gnuplot-name (format nil "~A/~A~A.gnu" destination-dir prefix file-name)
                                :gnuplot-normalized-name (format nil "~A/~A~A-normalized.gnu" destination-dir prefix file-name)
                                :sexp-name (format nil "~A/~A~A.sexp" destination-dir prefix file-name)
                                :sorted-name (format nil "~A/~A~A.sorted" destination-dir prefix file-name))))))

(defun random-subset-of-range (min max)
  (loop for i from min to max
        when (zerop (random 2))
          collect i))

(defun ranges (type min max)
  (when (> min max)
    (rotatef min max))
  (list `(,type ,min ,max)
        `(,type (,min) ,max)
        `(,type ,min (,max))
        `(,type (,min) (,max))))

(defvar *real-number-range-types* 
  (remove-duplicates
   (loop for i from 1 to 25
         nconc (let* ((a (random 100))
                      (b (random 100))
                      (c (random 100))
                      (d (random 100))
                      (min (/ a (1+ b)))
                      (max (/ c (1+ d))))
                 (ranges 'real min max))
         nconc (ranges 'float (random 100.0) (random 100.0))
         nconc (ranges 'integer (random 100) (random 100))
         )
   :test #'equal))

(defvar *integer-range-types*
  (remove-duplicates
   (loop for i from 1 to 25
         nconc (let* ((min (random (expt 10 (random i))))
                      (max (random (expt 10 (random i)))))
                 (ranges 'integer min max)))
   :test #'equal))

(defvar *member-types* (remove-duplicates
                        (loop for i from 1 to 25
                              collect (let ((s (random-subset-of-range 0 10)))
                                        (cond ((cdr s)
                                               (cons 'member s))
                                              (s
                                               (cons 'eql s))
                                              (t
                                               'null))))
                        :test #'equal))



(defun analysis (file-names)
  (declare #+sbcl (notinline sort))
  (let* ((measures '(:recursive :inner-loop :sort-strategy :do-break-sub :do-break-loop))
        (table (make-hash-table :test #'eq))
        (data (sort (mapcan (lambda (file-name)
                              (with-open-file (stream file-name)
                                (destructuring-bind (&key summary sorted &allow-other-keys) (read stream)
                                  (mapcar (lambda (sorted-plist)
                                            (destructuring-bind (&key score integral arguments &allow-other-keys) sorted-plist
                                              (dolist (measure measures)
                                                (pushnew (getf arguments measure) (gethash measure table) :test #'equal))
                                              (destructuring-bind (&key sort-strategy inner-loop recursive do-break-sub do-break-loop &allow-other-keys) arguments
                                                (list :score score
                                                      :recursive recursive
                                                      :inner-loop inner-loop
                                                      :sort-strategy sort-strategy
                                                      :do-break-sub do-break-sub
                                                      :do-break-loop do-break-loop
                                                      :integral integral
                                                      :summary summary))))
                                          sorted))))
                            file-names)
                    #'< :key (getter :score))))
    (flet ((mean (numbers &aux (c 0))
             (/ (reduce (lambda (acc next)
                          (incf c)
                          (+ acc next)) numbers :initial-value 0.0) c)))
      (list :attributes
            (sort (mapcan (lambda (measure)
                      (mapcar (lambda (value)
                                (list measure value (mean (mapcar (getter :score) (setof plist data
                                                                                    (equal value (getf plist measure)))))))
                              (gethash measure table)))
                    measures)
                  #'< :key #'caddr)
            :data data))))

(defun do-analysis ()
  (analysis '("/Users/jnewton/newton.16.edtchs/src/bdd-graph-cl-combos.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-cl-types.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-member.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-pcl-types.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-condition.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-number-or-condition.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-number.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-t.sorted")))



(defvar *bucket-reporters* nil)


(defun make-bucket-reporter (&key tag scale types file-name)
  (lambda (multiplier sample options &key (destination-dir *destination-dir*))
    (apply #'test-report :limit (* multiplier scale)
                         :tag tag
                         :types types
                         :file-name file-name
                         :sample sample
                         :destination-dir destination-dir
                         options)))

(defun add-bucket-reporter (&key tag scale types file-name)
  (let ((pair (assoc tag *bucket-reporters* :test #'equal)))
    (setf *bucket-reporters* (remove pair *bucket-reporters*))
    (push (list tag (make-bucket-reporter :tag tag :scale scale :types types :file-name file-name))
          *bucket-reporters*)))

(add-bucket-reporter :scale 20
                     :tag "Real number ranges"
                     :types *real-number-range-types*
                     :file-name "number-ranges")

(add-bucket-reporter :scale 20
                     :tag "Integer ranges"
                     :types *integer-range-types*
                     :file-name "integer-ranges")

(add-bucket-reporter :scale 18
                     :tag "Subtypes of CONDITION"
                     :types (valid-subtypes 'condition)
                     :file-name "subtypes-of-condition")

(add-bucket-reporter :scale 30
                     :tag "MEMBER types"
                     :types *member-types*
                     :file-name "member")

(add-bucket-reporter :scale 21
                     :tag "OBJECT SYSTEM types"
                     :types (loop :for name being the external-symbols in #+sbcl "SB-PCL" #+allegro "ACLMOP"
                                  :when (find-class name nil)
                                    :collect name)
                     :file-name "pcl-types")

(add-bucket-reporter :scale 30
                     :tag "CL types"
                     :types *cl-types*
                     :file-name "cl-types")

(add-bucket-reporter :scale 18
                     :tag "Subtypes of NUMBER or CONDITION"
                     :types (setof e (remove-duplicates (loop for t1 in (shuffle-list (union (valid-subtypes 'number)
                                                                                             (valid-subtypes 'condition)))
                                                              for t2 in (shuffle-list (union (valid-subtypes 'number)
                                                                                             (valid-subtypes 'condition)))
                                                              nconc (list t1 `(or ,t1 ,t2)
                                                                          `(and ,t1 (not ,t2))))
                                                        :test #'equal)
                              (null (subtypep e nil)))
                     :file-name "subtypes-of-number-or-condition")

(add-bucket-reporter :scale 25
                     :tag "Subtypes of NUMBER"
                     :types *number-combos*
                     :file-name "subtypes-of-number")

(add-bucket-reporter :scale 33
                     :tag "CL combinations"
                     :types (choose-randomly  *cl-type-combos* 13850)
                     :file-name "cl-combos")
    
(add-bucket-reporter :scale 22
                     :tag "Subtypes of T"
                     :types (valid-subtypes t)
                     :file-name "subtypes-of-t")

(defun display-theta (n theta)
  (flet ((e2 (theta)
           (- (expt 2 (expt 2 theta))
              (expt 2 (expt 2 (1- theta)))))
         (e1 (theta)
           (expt 2 (- n theta 1)))
         (pr (n fn)
           (format t " ~A=" fn)
           (if (< (log n) 10)
               (format t "~D" n)
               (format t "2^~A" (log n 2)))))
    (loop for i from (1- theta) to (1+ theta)
          do (format t "n=~D log_2(~D)=~D theta=~D" n n (floor (log n 2)) i)
          do (pr (e2 i) "e2")
          do (format t " ~A"
                     (cond
                       ((< (e2 i) (e1 i))
                        "<")
                       ((= (e2 i) (e1 i))
                        "=")
                       (t ">")))
          do (pr (e1 i) "e1")
          do (terpri))))


(defun delta (i n)
  (labels ((e2 (i)
             (- (expt 2 (expt 2 (1+ (- n i))))
                (expt 2 (expt 2 (- n i)))))
           (e1 (i)
             (expt 2 (1- i)))
           (delta-bar (i)
             (- (e2 i) (e1 i))))
    (format t "     n=~A e1=~A   e2=~A~%" n (e1 (- n 1)) (e2 (- n 1)))
    (delta-bar (- n i))))

(defun delta-print-table (n)
  (let ((d -1)
        (i 0))

    (loop :while  (< d 0)
          :do (setf d (delta i n))
          :do (format t "i=~A  delta= ~A~%" i d)
          :do (incf i)
          :finally (format t "n = ~A theta=~A~%" n (1- i)))
    ))


(defun theta-bounds (n)
  (list
   (if (< (log n 2) (1- n))
       (ceiling (log (- n (log n 2) 1) 2))
       0)
   (floor (log n 2))))




(defun 2^ (n) (expt 2 n))
(defun 2^^ (n) (2^ (2^ n)))

(defun power-diff (n)
  (- (2^^ n)
     (2^^ (1- n))))

(defun cmp-power-diff (n)
  (let ((theta (floor (log n 2))))
    (- (power-diff theta)
       (expt 2 (- n theta 1)))))

(defun row-r (n i)
  (declare (ignore n))
  (2^ (1- i)))

(assert (= 2 (row-r 3 2)) ())
(assert (= 4 (row-r 3 3)) ())
(assert (= 1 (row-r 2 1)) ())



(defun row-RR (n i)
  (- (2^^ (+ n (- i) 1))
     (2^^ (- n i))))

(assert (= 12 (row-RR 3 2)) ())
(assert (= 2 (row-RR 3 3)) ())
(assert (= 12 (row-RR 2 1)) ())
(assert (= 2 (row-RR 2 2)) ())

(defun log2 (n)
  (log n 2))

(defun theta (n)
  (let ((max (floor (log2 n))))
    ;;(format t "log=~A~%" max)
    (case max
      ((0)
       (values 0 0))
      (t
       (loop :for theta :downfrom max :downto 0
             :for r = (row-r n (- n theta))
             :for RR = (row-RR n (- n theta))
             :for iterations = 1 :then (1+ iterations)
             ;; :do (format t "n=~A theta=~A n-theta=~A r<=R?  ~A ~A ~A?~%"
             ;;             n theta (- n theta) r
             ;;             (cond ((< r RR) "<")
             ;;                   ((= r RR) "=")
             ;;                   (t ">"))
             ;;             RR)
             :do (when (>= r RR)
                   (return-from theta (values (1+ theta) iterations))))))))

(defun find-theta (limit)
  (let ((hash-iterations (make-hash-table)))
    (loop for n from 2 to limit
          do (multiple-value-bind (theta iterations) (theta n)
               (declare (ignore theta))
               (incf (gethash iterations hash-iterations 0))))
    (maphash (lambda (key value)
               (format t "iterations=~D occurances=~D~%" key value))
             hash-iterations)))

(defun estim (n nterms)
  (labels ((rec (l i accum)
             (if (zerop i)
                 accum
                 (rec (log (- n l) 2) (1- i) (cons l accum)))))
    (let ((seq (nreverse (rec (log n 2) nterms nil))))
      (maplist (lambda (tail)
                 (if (cdr tail)
                     (destructuring-bind (a b &rest ignored) tail
                       (declare (ignore ignored))
                       (list a (cond ((< a b) 1)
                                     ((= a b) 0)
                                     (t -1)) (- a b)))
                     (car tail)))
               seq))))

(defun robdd-size (n)
  (let* ((theta (theta n))
         (robdd-size (+ (1- (2^ (- n theta)))
                        (2^^ theta)))
         (bdd-size (1- (2^ (1+ n)))))
    (list :n n :theta theta :robdd robdd-size :bdd bdd-size :compression (float (/ (+ 1d0 robdd-size) bdd-size) 1.0))))

(defun log-based-compression (min max)
  (loop :for n :from min :to max
        :collect (list n (let ((theta (float (log n 2))))
                           (/ (+ (expt 2d0 (expt 2d0 theta)) (expt 2d0 (- n theta)) -1)
                              (1- (expt 2d0 (1+ (float n) ))))))))
  

(defun robdd-compressions (min max)
  (loop :for n :from min :to max
        :collect (destructuring-bind (&key compression &allow-other-keys) (robdd-size n)
                   (list n compression))))



(defun theta-latex-table (n-min n-max) ;; normally 1 21
  (format t "$~A$ & $~A$ & $~A$ & $~A$ & $~A$ & $~A$ & $~A$\\\\~%"
          "\\numvars"
          "\\lfloor \\log_2\\numvars \\rfloor"
          "\\theta"
          "2^{\\numvars -\\theta}-1"
          "2^{2^\\theta}"
          "|ROBDD_{\\numvars}|"
          "\\frac{|ROBDD_{\\numvars}|}{|UOBDD_{\\numvars}|}"
          )
  (loop :for n :from n-min :to n-max
        :do (let* ((theta (theta n))
                   (theta-max (floor (log2 n)))
                   (|ROBDD_n| (+ (1- (2^ (- n theta)))
                                 (2^ (2^ theta))))
                   (|UOBDD_n| (1- (2^ (1+ n))))
                   (compression (/ (float |ROBDD_n|)
                                   |UOBDD_n|)))
              (format t "~A & ~A & ~A & ~A & ~A & ~A & ~6,3f\\%\\\\~%"
                      n
                      theta-max
                      theta
                      (1- (2^ (- n theta))) 
                     (2^ (2^ theta))
                      |ROBDD_n|
                      (* 100 compression)))))
                           
(defun compression (n)
  (let ((theta (theta n)))
    (format t "theta = ~A~%" theta)
    (format t "(2^^~A + 2^(~A - ~A) - 1) / (2^~A - 1)~%" theta n theta (1+ n))
    (format t "= (2^~A + 2^(~A - ~A) - 1) / (2^~A - 1)~%" (2^ theta) n theta (1+ n))
    (format t " = (~A + ~A - 1) / (~A - 1)~%" (2^^ theta) (2^ (- n theta)) (2^ (1+ n)))
    (format t " = ~A / ~A~%" (+ (2^^ theta) (2^ (- n theta)) -1) (1- (2^ (1+ n))))
    (format t " = ~A~%" (/ (+ (2^^ (float theta)) (2^ (- n (float theta))) -1) (1- (2^ (1+ (float n))))))))

(defun graph-theta (n-min n-max)
  (loop :for n :from n-min :to n-max
        :for theta = (theta n)
        :do (format t "(~A, ~A)~%" n theta))
  (format t "~%")
  (loop :for n :from n-min :to n-max
        :do (format t "(~A, ~A)~%" n (log2 n)))
  (format t "~%")

  (loop :for n :from n-min :to n-max
        :do (format t "(~A, ~A)~%" n (- (log2 (- n 2 (log2 n))) 2))))

(defun call-with-caffeinate (thunk)
  (let ((caff (sb-ext:run-program "caffeinate"
                                  '("-i" "sleep" "31449600") ;; sleep for 1 year or until killed
                                  :search t :wait nil)))
    (prog1 (funcall thunk)
      (sb-ext:process-kill caff 1))))

(defmacro caffeinate (&body body)
  "Evaluate the given code body, but using the caffeinate MACOS program to prevent the system from
sleeping before the code finishes evaluating."
  `(call-with-caffeinate (lambda () ,@body)))


(defmacro with-dup-stream ((stream file) &body body)
  "Rebind the named STREAM to a broadcast-stream which duplicates it to the named FILE, evaluating the BODY within the dynamic extend of the rebinding."
  (let ((log-file (gensym)))
    `(with-open-file (,log-file ,file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
       (let ((,stream (make-broadcast-stream ,stream ,log-file)))
         ,@body))))
                         





(defun big-test-report (&rest options &key (num-tries 2) (multiplier 1) (prefix "") (re-run t)
                                        (suite-time-out (* 60 60 4)) (time-out 100) normalize hilite-min
                                        (decomposition-functions *decomposition-functions*)
                                        (destination-dir *destination-dir*)
                                        )
  (declare (ignore prefix re-run suite-time-out time-out normalize num-tries hilite-min))
  (let ((*decomposition-functions*  decomposition-functions))
    (loop for (tag bucket-reporter) in *bucket-reporters*
          for sample = (/ 1 (length *bucket-reporters*)) then (+ sample (/ 1 (length *bucket-reporters*)))
          do (funcall bucket-reporter multiplier sample options :destination-dir destination-dir)))    )


(defun parameterization-report (&key (re-run t) (multiplier 1) (destination-dir *destination-dir*))
  (big-test-report :re-run re-run
                   :prefix "bdd-graph-"
                   :normalize 'decompose-types-bdd-graph-weak
                   :hilite-min t
                   :destination-dir destination-dir
                   :multiplier multiplier
                   :decomposition-functions (cons 'decompose-types-bdd-graph-weak
                                                  *decompose-fun-names*)))


(defun best-2-report (&key (re-run t) (multiplier 1.8) (destination-dir *destination-dir*))
  (big-test-report :re-run re-run
                   :prefix "best-2-" ;; should change to best-4-
                   :multiplier multiplier
                   :normalize nil
                   :time-out 20
                   :num-tries 4
                   :hilite-min nil
                   :destination-dir destination-dir
                   :decomposition-functions '(decompose-types-bdd-graph-strong
                                              decompose-types-bdd-graph-weak
                                              bdd-decompose-types-strong
                                              bdd-decompose-types-weak
                                              decompose-types-rtev2
                                              decompose-types-graph)))


(defun bdd-report (&key (re-run t) (multiplier 1.8)  (destination-dir *destination-dir*))
  (big-test-report :re-run re-run
                   :prefix "bdd-ws-" ;; should change to best-4-
                   :multiplier multiplier
                   :normalize nil
                   :time-out 20
                   :num-tries 4
                   :hilite-min nil
                   :destination-dir destination-dir
                   :decomposition-functions '(decompose-types-bdd-graph-strong
                                              decompose-types-bdd-graph-weak
                                              bdd-decompose-types-strong
                                              bdd-decompose-types-weak)))

