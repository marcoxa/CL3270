;;;; -*- Mode: Lisp -*-

;;;; cl3270-pkg.lisp
;;;;
;;;; Minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(defpackage "IT.UNIMIB.DISCO.MA.MAINFRAME.CL3270" (:use "CL")
  (:nicknames "CL3270")

  ;; EBDCIC
  (:export
   "EBCDIC-037"
   "ASCII-037"
   
   "EBCDIC"
   "ASCII"
   "TO-ASCII"
   "TO-EBCDIC"
   )
  )

;;;; end of file -- cl3270-pkg.lisp
