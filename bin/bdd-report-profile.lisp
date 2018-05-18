#!/lrde/home/jnewton/opt/sbcl/bin/sbcl --script
#|
#PBS -m ea
|#
(require :asdf)
(require :sb-posix)
(let ((home (directory-namestring (user-homedir-pathname)))
      (uid (sb-posix:getuid))
      (pid  (sb-posix:getpid)))
  (setf asdf::*user-cache* (ensure-directories-exist (format nil "/tmp~A~D/~D/" home uid pid))))

#-quicklisp
(let ((quicklisp-init
	"/lrde/home/jnewton/quicklisp/setup.lisp"))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "file not found ~S" quicklisp-init)))
(asdf:load-system :lisp-types-test)
(in-package :lisp-types.test)

(defvar *bucket-index* (parse-integer (sb-posix:getenv "BUCKET-INDEX")))
(defvar *bucket*    (nth *bucket-index* *bucket-reporters* ))

(defvar *decompose-function-index* (parse-integer (sb-posix:getenv "DECOMPOSE-INDEX")))
(defvar *decompose* (nth *decompose-function-index* *decomposition-functions* ))

(defvar *broadcast* (format nil "cluster.~A/broadcast.bdd-report-profile-~A-~D-~D"
			    (sb-posix:getenv "CLUSTER_JOB_NUM")
			    (or (sb-posix:getenv "PBS_JOBID") "0")
			    (sb-posix:getpid) *decompose-function-index*
			    *bucket-index*))

(with-dup-stream (*standard-output* *broadcast*)
  (format t "Writing to broadcast file ~A~%" *broadcast*)
  (format t "-------------------------------------------------~%")
  (format t "-------------------------------------------------~%")
  (format t "starting bdd-report-profile ~A ~A ~%" *decompose* *bucket*)
  (format t "-------------------------------------------------~%")
  (finish-output)
  (time (bdd-report-profile :decomposition-functions (list *decompose*)
			    :bucket-reporters (list *bucket*)
			    :num-tries 2
			    :prefix (format nil "bdd-profile-~D-~D-" *decompose-function-index* *bucket-index*)
			    :create-png-p nil
			    :multiplier 6.0
			    :destination-dir "/lrde/home/jnewton/analysis/."))
  (format t "finished bdd-report-profile ~A ~A ~%" *decompose* *bucket*))



(sb-ext:run-program "rm" (list "-r" asdf::*user-cache*)
		    :search t)
(qstat-f)
