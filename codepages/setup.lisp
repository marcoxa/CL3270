;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; setup.lisp
;;;;
;;;; Ensuring that the codepages are generated.
;;;; This is essentially a "script" file.
;;;;
;;;; See the COPYING file in the main folder for licensing and
;;;; copyright information.


(in-package "CL3270")

(defparameter *generate-code-pages* t
  "Internal flag to selectively start CL code page generation.

When not NULL generation can take place.")


;;; Pretty kludgy and blunt.  I should explicitely check that the
;;; desired code pages are in place, instead of just checking that
;;; there are files in the folder.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (handler-case
      (when *generate-code-pages*
        (loop initially (format t ";;; CL3270: setup codepages.~%")

              for cp-files = (directory *cl3270-code-page-local-dir*)
              if (null cp-files)
                do (generate-code-pages)
              else
                do (dolist (cp-file cp-files)
                     (compile-file cp-file)
                     (load (compile-file-pathname cp-file)))
                   (loop-finish)
                
              finally (format t ";;; CL3270: codepages setup complete.~2%")
                      ))
    (error (e)
      (format *error-output* ";;; CL3270: codepages setup error ~S.~%" e)
      (error e))))

;;;; setup.lisp ends here.
