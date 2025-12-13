;;;; -*- Mode: Lisp -*-

;;;; setup.lisp --
;;;;
;;;; Setting up the CL3270 system.
;;;; This is where some of the key global variables are created, in
;;;; particular, those relevan for locating certain bits and pieces of
;;;; the system.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(in-package "CL3270")


(defparameter *cl3270-source-location*
  nil
  "The location (a directory) where the CL3270 source is found.

If NIL, it means that the package has not been properly installed.")


(declaim (type (or null pathname) *cl3270-source-location*))


;;; Ensuring *cl3270-source-location* has a proper value.
;;; Repetition and kludginess necessary to handle system definition (mk or asdf)
;;; or no system definition, in which case we must handle load time vs
;;; compile time.
;;;
;;; Using CLAD may ease some of this stuff.


(eval-when (:load-toplevel :execute)
  (let ((cl3270-loc (or
                     #+asdf
                     (and (asdf:find-system "cl3270" nil)
                          (asdf:system-source-directory "cl3270"))

                     #+mk-defsystem
                     (and (mk:find-system "cl3270" :load-or-nil)
                          (mk:system-definition-pathname "cl3270"))

                     #-(or asdf mk-defsystem)
                     *load-pathname*
                     ))
        )
    (unless *cl3270-source-location*
      (setq *cl3270-source-location*
            (make-pathname :name nil :type nil
                           :defaults cl3270-loc)))))


(eval-when (:compile-toplevel :execute)
  (let ((cl3270-loc (or
                     #+asdf
                     (and (asdf:find-system "cl3270" nil)
                          (asdf:system-source-directory "cl3270"))

                     #+mk-defsystem
                     (and (mk:find-system "cl3270" :load-or-nil)
                          (mk:system-definition-pathname "cl3270"))

                     #-(or asdf mk-defsystem)
                     *compile-file-pathname*
                     ))
        )
    (unless *cl3270-source-location*
      (setq *cl3270-source-location*
            (make-pathname :name nil :type nil
                           :defaults cl3270-loc)))))


;;;; end of file -- setup.lisp
