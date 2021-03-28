;;;; -*- Mode: Lisp -*-

;;;; debug.lisp


(in-package "CL3270")

(defparameter *do-debug* t)

(defun dbgmsg (fmt &rest args)
  (when *do-debug*
    (write-string "CL3270: " *trace-output*)
    (apply #'format *trace-output* fmt args)))


;;;; end of file -- debug.lisp
