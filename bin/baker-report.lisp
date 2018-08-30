#!/usr/bin/env sbcl --script
#|
#PBS -m a
|#

;; sb-ext:*posix-argv* = ("/usr/local/bin/sbcl" "bucket-index" )

(require :asdf)
(require :sb-posix)
(defvar *home* (directory-namestring (user-homedir-pathname))) ;; e.g. "/Users/jimka/"
(let ((uid (sb-posix:getuid))
      (pid  (sb-posix:getpid)))
  (setf asdf::*user-cache* (ensure-directories-exist (format nil "/tmp~A~D/~D/" *home* uid pid))))

;; (declaim (optimize (safety 3) (debug 3) (space 0) (speed 0))) 
(declaim (optimize (safety 1) (debug 0) (space 0) (speed 3) (compilation-speed 0)))

#-quicklisp
(let ((quicklisp-init
	(format nil "~a~A" *home* "quicklisp/setup.lisp")))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "file not found ~S" quicklisp-init)))
(asdf:load-system :lisp-types-analysis)
(in-package :lisp-types-analysis)

(defvar *bucket-index* (parse-integer (or (pop (cdr sb-ext:*posix-argv*))
					  (sb-posix:getenv "BUCKET-INDEX"))))
(defvar *bucket*    (nth *bucket-index* *bucket-reporters* ))

(defvar *broadcast-dir* (format nil "~Acluster.~A"
				(directory-namestring (user-homedir-pathname))
				(or (sb-posix:getenv "CLUSTER_JOB_NUM")
				    "noncluster")))	

(defvar *broadcast* (format nil "~A/broadcast.baker-report.~A-~D-~D"
			    *broadcast-dir*
			    (or (sb-posix:getenv "PBS_JOBID") "0")
			    (sb-posix:getpid)
			    *bucket-index*))

(with-dup-stream (*standard-output* (ensure-directories-exist *broadcast*))
  (format t "Writing to broadcast file ~A~%" *broadcast*)
  (format t "-------------------------------------------------~%")
  (format t "-------------------------------------------------~%")
  (format t "*bucket-index* = ~A~%" *bucket-index*)
  (format t "starting baker-report ~A ~%" *bucket*)
  (format t "-------------------------------------------------~%")
  (finish-output)
  (time (baker-report :create-png-p nil
                      :multiplier 8
                      :bucket-reporters (list *bucket*)
                      :destination-dir (format nil "~Aanalysis/." (directory-namestring (user-homedir-pathname)))))
  (format t "finshed baker-report ~A ~%" *bucket*))


(sb-ext:run-program "rm" (list "-r" asdf::*user-cache*)
		    :search t)
(qstat-f)
