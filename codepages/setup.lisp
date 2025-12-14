;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; setup.lisp
;;;;
;;;; Ensuring that the codepages are generated, compiled and loaded.
;;;; This is essentially a "script" file.
;;;;
;;;; See the COPYING file in the main folder for licensing and
;;;; copyright information.


(in-package "CL3270")


;;; Pretty kludgy and blunt.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (setq *generate-code-pages* t)

  ;; Yep... I am doing it twice: while compiling and loading.
  ;; I need to check that the "loaded" codepage is newer than the
  ;; (compiled) file.
  ;;
  ;; Will do this later... if ever.

  (ensure-codepages)
  )

;;;; setup.lisp ends here.
