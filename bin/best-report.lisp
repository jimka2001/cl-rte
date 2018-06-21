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
(asdf:load-system :lisp-types-analysis)
(in-package :lisp-types-analysis)

(defvar *bucket-index* (parse-integer (sb-posix:getenv "BUCKET-INDEX")))
(defvar *bucket*    (nth *bucket-index* *bucket-reporters* ))

(defvar *broadcast* (format nil "cluster.~A/broadcast.best-report.~A-~D-~D"
			    (sb-posix:getenv "CLUSTER_JOB_NUM")
			    (or (sb-posix:getenv "PBS_JOBID") "0")
			    (sb-posix:getpid)
			    *bucket-index*))

(with-dup-stream (*standard-output* *broadcast*)
  (format t "Writing to broadcast file ~A~%" *broadcast*)
  (format t "-------------------------------------------------~%")
  (format t "-------------------------------------------------~%")
  (format t "*bucket-index* = ~A~%" *bucket-index*)
  (format t "starting best-2-report ~A ~%" *bucket*)
  (format t "-------------------------------------------------~%")
  (finish-output)
  (time (best-2-report :create-png-p nil
		       :bucket-reporters (list *bucket*)
		       :destination-dir "/lrde/home/jnewton/analysis/."))
  (format t "finshed best-2-report ~A ~%" *bucket*))


(sb-ext:run-program "rm" (list "-r" asdf::*user-cache*)
		    :search t)
(qstat-f)
