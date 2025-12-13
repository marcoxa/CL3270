;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; setup.lisp
;;;;
;;;; Ensuring that the codepages are generated, compiled and loaded.
;;;; This is essentially a "script" file.
;;;;
;;;; See the COPYING file in the main folder for licensing and
;;;; copyright information.


(in-package "CL3270")

(defparameter *generate-code-pages* t
  "Internal flag to selectively start CL code page generation.

When not NULL generation can take place.")


(defun compile-load-code-page-file (cpf)
  "Compile (if needed) and load a CL code page file CPF.

Notes:

This function is needed as the files are not managed by the system
defintion facilities like ASDF."

  (declare (type pathname cpf))
  
  (if (not (probe-file cpf))
      (warn "CL3270: nonexistent code page file '~A.~A'."
            (pathname-name cpf)
            (pathname-type cpf))

      (let* ((cpcfp (compile-file-pathname cpf))
             (cpfwd (file-write-date cpf))
             (cpcfpwd (file-write-date cpcfp))
             )
        (declare (type pathname cpcfp)
                 (type (or null integer) cpfwd cpcfpwd))

        (cond ((and (probe-file cpcfp)
                    cpfwd
                    cpcfpwd
                    (< cpfwd cpcfpwd))
               (load cpcfp))

          ((null (probe-file cpcfp))
           (compile-file cpf)
           (load cpcfp))

          ((or (null cpfwd) (null cpcfpwd))
           (warn "CL3270: cannot determine write dates for '~A'."
                 (pathname-name cpf)))
          ))))


(defun ensure-codepages ()
  "Ensure that the Common Lisp codepages are generated and loaded."

  (handler-case
      (when *generate-code-pages*
        (format t "~%;;; CL3270: setting up codepages...~%")

        (cond ((some (complement #'probe-file) *cl3270-code-page-pathnames*)
               (format t "~&;;; CL3270: generating codepages...~2%")
               (generate-code-pages)
               (format t "~&;;; CL3270: codepages generated.~2%"))

              (t
               (format t "~&;;; CL3270: codepages already generated.~2%")))

        (format t "~&;;; CL3270: compiling and/or loading codepages...~2%")
        (dolist (cp-file *cl3270-code-page-pathnames*)
          (format t "~&;;; CL3270: compiling and/or loading '~A'.~%"
                  (pathname-name cp-file))
          (compile-load-code-page-file cp-file))

        (format t "~%;;; CL3270: codepages setup complete.~2%"))

    (error (e)
      (format *error-output* ";;; CL3270: codepages setup error ~S.~%" e)
      (error e))))


;;; Pretty kludgy and blunt.

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Yep... I am doing it twice: while compiling and loading.
  ;; I need to check that the "loaded" codepage is newer than the
  ;; (compiled) file.
  ;;
  ;; Will do this later... if ever.

  (ensure-codepages)
  )

;;;; setup.lisp ends here.
