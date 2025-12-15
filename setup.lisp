;;;; -*- Mode: Lisp -*-

;;;; setup.lisp --
;;;;
;;;; Setting up the CL3270 system.
;;;; This is where some of the key global variables are created, in
;;;; particular, those relevant for locating certain bits and pieces of
;;;; the system.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(in-package "CL3270")


;;; setup-source-pathname
;;;
;;; Thanks to David Cooper for this nifty trick.  It is needed to
;;; circumvent COMPILE-FILE output changes, which is what ASDF and MK
;;; essentially do.  The macro call freezes the right pathname in the
;;; code. 

(defmacro setup-source-pathname ()
  "Substitute the truename of the 'setup.lisp' in the code."
  `,(or *compile-file-truename* *load-truename*))


;;; *cl3270-source-location*
;;;
;;; Note the use of SETUP-SOURCE-LOCATION.

(defparameter *cl3270-source-location*
  (make-pathname :name nil :type nil :defaults (setup-source-pathname))
  "The location (a directory) where the CL3270 source is found.")

(declaim (type pathname *cl3270-source-location*))

;;;; end of file -- setup.lisp
