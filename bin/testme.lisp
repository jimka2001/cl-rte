#!/usr/local/bin/sbcl --script

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
(asdf:load-system :research)
(in-package :scrutiny)
(run-tests)

(if *failed-tests*
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))


