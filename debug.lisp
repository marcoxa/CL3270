;;;; -*- Mode: Lisp -*-

;;;; debug.lisp
;;;;
;;;; Minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(in-package "CL3270")

(defparameter *do-debug* t)

(defun dbgmsg (fmt &rest args)
  (when *do-debug*
    (write-string ";;; CL3270: " *trace-output*)
    (apply #'format *trace-output* fmt args)))


;;;; end of file -- debug.lisp
