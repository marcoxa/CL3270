;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; ebcdic.lisp
;;;;
;;;; EBCDIC <-> ASCII mincing.
;;;;
;;;; See the file COPYING for copyright and licensing information.
;;;; To do: deal with code pages.
;;;;
;;;; Instances of CODEPAGE provide EBCDIC<->UTF-8 translation. By default,
;;;; CL3270 is configured to use CP 1047. You may alternatively set a different
;;;; codepage using the SET-CODEPAGE function during your application
;;;; initialization.
;;;;
;;;; Notes:
;;;;
;;;; Matthew R. Wilson's original note.
;;;;
;;;; After careful consideration, I have decided that the default code page we
;;;; will support for EBCDIC is IBM CP 1047. Other code pages may be globally
;;;; selected with the SetCodepage() function.
;;;;
;;;; In suite3270 (e.g. c3270/x3270), the default code page is what it calls
;;;; "brackets". This is CP37 with the [, ], Ý, and ¨ characters swapped around.
;;;; This ends up placing all four of those characters in the correct place for
;;;; 1047 (and thus they will all work correctly with go3270 by default).
;;;; HOWEVER, the ^ and ¬ characters are swapped relative to CP1047. (Or, more
;;;; succinctly, you could say the suite3270 "brackets" codepage is CP1047 with
;;;; the ^ and ¬ characters swapped back to where they are in CP37). If you plan
;;;; on using the ^ and ¬ characters, run c/x3270 in proper 1047 mode,
;;;; `c3270-codepage 1047` or make it your default by setting the
;;;; `c3270.codePage` resource to `1047` in your `.c3270pro` file, for example.
;;;;
;;;; In Vista TN3270, "United States" is the default code page. This is CP1047
;;;; and will map 100% correctly.
;;;;
;;;; In IBM PCOMM, CP37 is the default. For correct mapping of [, ], Ý, ¨, ^,
;;;; and ¬, you must switch the session parameters from "037 United States" to
;;;; "1047 United States".

(in-package "CL3270")


;;; *default-codepage*

(defparameter *default-codepage*
  (if (boundp '*codepage-1147*)
      *codepage-1147*
      *codepage-bracket*)
  "The current default codepage.")


;;; set-codepage

(defun set-codepage (cp-designator)
  (setq *default-codepage*
        (etypecase cp-designator
          (codepage cp-designator)
          (codepage-id (let ((cp (get-codepage cp-designator)))
                         (if cp
                             cp
                             (error "CL3270: cannot find codepage ~D."
                                    cp-designator))))
          )))


;;;; end of file -- ebcdic.lisp
