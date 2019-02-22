#!/usr/local/bin/sbcl --script

(require :asdf)
(require :sb-posix)
(let ((home (directory-namestring (user-homedir-pathname)))
      (uid (sb-posix:getuid))
      (pid  (sb-posix:getpid)))
  (setf asdf::*user-cache* (ensure-directories-exist (format nil "/tmp~A~D/~D/" home uid pid))))

;; (declaim (optimize (safety 3) (debug 3) (space 0) (speed 0))) 
(declaim (optimize (safety 1) (debug 0) (space 0) (speed 3) (compilation-speed 0)))
(format t "============= apt-get graphviz~%")
(sb-ext:run-program "apt-get" (list "graphviz")
                    :search t :output t)
(format t "============= apt-get gnuplot~%")
(sb-ext:run-program "apt-get" (list "gnuplot")
                    :search t :output t)
(format t "============= apt-get update~%")
(sb-ext:run-program "apt-get" (list "update")
                    :search t :output t)
(format t "============= installing git~%")
(sb-ext:run-program "apt-get" (list "install" "-y" "git")
                    :search t  :output t)
(format t "============= cloning subtypep~%")

;;  git clone http://<username>:<deploy_token>@gitlab.example.com/tanuki/awesome_project.git
;;  http://<username>:<deploy_token>@gitlab.example.com/tanuki/awesome_project.git
;; user name gitlab+deploy-token-2
;; token = gLsySaNTMHGmqZS6xtVe
(sb-ext:run-program "git" (list "clone" "http://gitlab+deploy-token-2:gLsySaNTMHGmqZS6xtVe@gitlab.lrde.epita.fr/climb/subtypep.git")
                    ;; clone subtypep repo
                    :search t :output t)
(format t "cwd=~A~%" (sb-posix:getcwd))
(format t "user-homedir=~A~%" (user-homedir-pathname))
(format t "PATH=~A~%" (sb-posix:getenv "PATH"))
(format t "*features*=~A~%" *features*)
(let ((quicklisp-init
	"/root/quicklisp/setup.lisp"))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "file not found ~S" quicklisp-init)))
(ql:quickload :closer-mop)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-fad)
(ql:quickload :iterate)
(ql:quickload :yacc)

(asdf:initialize-source-registry `(:source-registry
                                   (:tree (,(sb-posix:getcwd)))
                                   :inherit-configuration
                                   ))
(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)
(asdf:load-system :research)
(in-package :scrutiny)
(run-tests)

(if *failed-tests*
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))


