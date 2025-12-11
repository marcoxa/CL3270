;;;; -*- Mode: Lisp -*-

;;;; device.lisp
;;;;
;;;; Device (i.e., a 3270) representation for minimal 3720 data stream
;;;; emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.
;;;;
;;;; Notes:
;;;;
;;;; Just roughly following Matthew R. Wilson's API.

(in-package "CL3270")

(defstruct (device-info (:conc-name nil))

  "The Device Info Struct.

Minimal information about the device, i.e., the 3270 terminal."

  (rows 0 :type (mod 1024))		; Let's exaggerate!
  (cols 0 :type (mod 1024))
  (term-type "IBM 3270" :type string)
  (codepage nil)			; FTTB.
  )


(defun alt-dimensions (d)
  (declare (type device-info d))
  (values (rows d) (cols d)))

;;;; end of file -- device.lisp
