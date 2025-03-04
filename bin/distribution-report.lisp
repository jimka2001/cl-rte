#!/lrde/cluster/jnewton/opt/sbcl/bin/sbcl --script
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
(asdf:load-system :cl-robdd-analysis)
(in-package :cl-robdd-analysis)

(defvar *num-vars* (parse-integer (demand-env-var "NUM-VARS")))
(defvar *num-samples* (parse-integer (demand-env-var "NUM-SAMPLES")))
(defvar *bdd-sizes-file* (format nil "cluster.~A/bdd-sizes.~A-~D-~D"
                                 (demand-env-var "CLUSTER_JOB_NUM")
                                 (or (sb-posix:getenv "PBS_JOBID") "0")
                                 (sb-posix:getpid)
                                 *num-vars*))
(defvar *broadcast* (format nil "cluster.~A/broadcast.distribution-report.~A-~D-~D"
			    (demand-env-var "CLUSTER_JOB_NUM")
			    (or (sb-posix:getenv "PBS_JOBID") "0")
			    (sb-posix:getpid)
			    *num-vars*))


(setf *random-state* (make-random-state t))

(with-dup-stream (*standard-output* *broadcast*)
  (format t "Writing to broadcast file ~A~%" *broadcast*)
  (format t "-------------------------------------------------~%")
  (format t "-------------------------------------------------~%")
  (format t "starting distribution-report ~A~%" *num-vars*)
  (format t "-------------------------------------------------~%")
  (finish-output)
  (time (measure-and-write-bdd-distribution
	 "/lrde/home/jnewton/analysis/."
	 *num-vars*
	 *num-samples*
         *bdd-sizes-file*
         :interval (* 60 5)))
  (format t "finished distribution-report ~A~%" *num-vars*))

(run-program "rm" (list "-r" asdf::*user-cache*))
(qstat-f)
