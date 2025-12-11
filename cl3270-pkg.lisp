;;;; -*- Mode: Lisp -*-

;;;; cl3270-pkg.lisp
;;;;
;;;; Minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(defpackage "IT.UNIMIB.DISCO.MA.MAINFRAME.CL3270" (:use "CL")
  (:nicknames "CL3270")

  (:documentation "The IT.UNIMIB.DISCO.MA.MAINFRAME.CL3270 Package.

The package containing the \"3270\" Common Lisp data stream emulation
code.")

  ;; EBDCIC.
  (:export
   "EBCDIC-037"
   "ASCII-037"
   
   "EBCDIC"
   "ASCII"
   "TO-ASCII"
   "TO-EBCDIC"
   )

  ;; Codepages.
  (:export
   "CODEPAGE"
   "CODEPAGE-P"
   "CODEPAGE-NAME"
   "CODEPAGE-ID"
   )

  )

;;;; end of file -- cl3270-pkg.lisp
