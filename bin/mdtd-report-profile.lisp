#!/lrde/home/jnewton/opt/sbcl/bin/sbcl --script
#|
#PBS -m e
|#
(require :asdf)
(require :sb-posix)
(let ((home (directory-namestring (user-homedir-pathname)))
      (uid (sb-posix:getuid))
      (pid  (sb-posix:getpid)))
  (setf asdf::*user-cache* (ensure-directories-exist (format nil "/tmp~A~D/~D/" home uid pid))))

;; (declaim (optimize (safety 3) (debug 3) (space 0) (speed 0))) 
(declaim (optimize (safety 1) (debug 0) (space 0) (speed 3) (compilation-speed 0)))

#-quicklisp
(let ((quicklisp-init
	"/lrde/home/jnewton/quicklisp/setup.lisp"))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "file not found ~S" quicklisp-init)))
(asdf:load-system :lisp-types-analysis)
(in-package :lisp-types-analysis)

(defvar *bucket-index* (parse-integer (sb-posix:getenv "BUCKET-INDEX")))
(defvar *bucket*    (or (nth *bucket-index* *bucket-reporters* )
                        (error "no *bucket* found with index ~D" *bucket-index*)))

(defvar *decompose-function-index* (parse-integer (sb-posix:getenv "DECOMPOSE-INDEX")))
(defvar *decompose* (or (nth *decompose-function-index* *decomposition-functions*)
                        (error "no *decompose* function found with index ~D" *decompose-function-index*)))
(assert (= 13 (length  *decomposition-functions*)) ()
	"make-profile-reports.sh assumes there are exactly 12 decomposition functions, ~D were found"
	(length  *decomposition-functions*))
(defvar *broadcast* (format nil "cluster.~A/broadcast.mdtd-report-profile-~A-~D-~D"
			    (sb-posix:getenv "CLUSTER_JOB_NUM")
			    (or (sb-posix:getenv "PBS_JOBID") "0")
			    (sb-posix:getpid) *decompose-function-index*
			    *bucket-index*))

(with-dup-stream (*standard-output* *broadcast*)
  (format t "Writing to broadcast file ~A~%" *broadcast*)
  (format t "-------------------------------------------------~%")
  (format t "-------------------------------------------------~%")
  (format t "*bucket-index* = ~A~%" *bucket-index*)
  (format t "*decompose-function-index* = ~A~%" *decompose-function-index*)
  (format t "starting mdtd-report-profile ~A ~A ~%" *decompose* *bucket*)
  (format t "-------------------------------------------------~%")
  (finish-output)
  (time (mdtd-report-profile :decomposition-functions (list *decompose*)
			     :bucket-reporters (list *bucket*)
			     :num-tries 2
			     :prefix (format nil "mdtd-profile-single-~A-" *decompose*)
			     :create-png-p nil
			     :multiplier 6.0
			     :destination-dir "/lrde/home/jnewton/analysis/."))
  (format t "finished mdtd-report-profile ~A ~A ~%" *decompose* *bucket*))



(sb-ext:run-program "rm" (list "-r" asdf::*user-cache*)
		    :search t)
(qstat-f)
