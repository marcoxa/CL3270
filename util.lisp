;;;; -*- Mode: Lisp: Encoding UTF-8 -*-

;;;; util.lisp
;;;;
;;;; A few useful definitions.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(in-package "CL3270")


;; (declaim (type (array (unsigned-byte 8)) *codes*))
(declaim (type vector *codes*))
(defvar *codes*
  (vector
   #x40 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 #xc8
   #xc9 #x4a #x4b #x4c #x4d #x4e #x4f #x50 #xd1 #xd2 #xd3 #xd4
   #xd5 #xd6 #xd7 #xd8 #xd9 #x5a #x5b #x5c #x5d #x5e #x5f #x60
   #x61 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #x6a #x6b #x6c
   #x6d #x6e #x6f #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8
   #xf9 #x7a #x7b #x7c #x7d #x7e #x7f
   )
  "The 3270 control character I/O codes.

Pre-computed as provided at
http://www.tommysprinkle.com/mvs/P3270/iocodes.htm
"
  )

(declaim (type vector *decodes*))
(defvar *decodes*
  (vector
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 0 -1 -1 -1
   -1 -1 -1 -1 -1 -1 10 11 12 13 14 15 16 -1 -1 -1 -1 -1
   -1 -1 -1 -1 26 27 28 29 30 31 32 33 -1 -1 -1 -1 -1 -1
   -1 -1 42 43 44 45 46 47 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   58 59 60 61 62 63 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
   -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 1 2
   3 4 5 6 7 8 9 -1 -1 -1 -1 -1 -1 -1 17 18 19 20 21 22
   23 24 25 -1 -1 -1 -1 -1 -1 -1 -1 34 35 36 37 38 39 40
   41 -1 -1 -1 -1 -1 -1 48 49 50 51 52 53 54 55 56 57 -1
   -1 -1 -1 -1
   )

  "The inverse of the 3270 control character I/O codes table.

The value -1 is used to indicate invalid positions."
  )


;;; Strings and octects.
;;; Just not to depend on BABEL or FLEXI-STREAMS

(defun octets-to-string (octets)
  "Create a string from OCTETS, which is a vector of bytes."

  (declare (type (vector unsigned-byte *) octets))

  ;; Assume bytes code for (ASCII) characters with no encoding.
  ;; Note that declaring (vector (unsigned-byte 8) *) is too strict
  ;; FTTB, at least on LW.

  (with-output-to-string (result)
    (loop for o across octets
          do (write-char (code-char o) result))))


;;; Dictionaries
;;;
;;; Useful wrapper around MAKE-HASH-TABLE.

(defun make-dict (&rest keys &key (initial-map ()) &allow-other-keys)
  (remf keys :initial-map)
  (loop with ht of-type hash-table = (apply #'make-hash-table keys)
        for (k v) in initial-map
        do (setf (gethash k ht) v)
        finally (return ht)))


(defun dict-keys (dict)
  (declare (type hash-table dict))
  (loop for k being the hash-key of dict collect k))


(defun dict-values (dict)
  (declare (type hash-table dict))
  (loop for v being the hash-value of dict collect v))


(defun dict-kv-pairs (dict)
  (declare (type hash-table dict))
  (loop for k being the hash-key of dict using (hash-value v) collect (cons k v)))


;;; Dates

(declaim (ftype (function (&optional boolean boolean boolean) string)
                today-date
                now-time))

(defun today-date (&optional h m s)
  "Return a string representing today's date.

The format of the string is `YYYY-MM-DD` if the optional arguments H,
M, and S are all NIL.  Otherwise hours, minuts and seconds are added
to the result, accordingly."

  (declare (type boolean h m s))

  (multiple-value-bind (ss mn hh dd mm yy dw dsp tz)
      (decode-universal-time (get-universal-time))
    (declare (type (mod 60) ss mm)
             (type (mod 24) hh)
             (type (integer 0) yy)
             (type (mod 7) dw)
             (type boolean dsp)
             (type (rational -24 24) tz))
    (declare (ignorable ss mm hh dw dsp tz))

    (with-output-to-string (today nil :element-type 'character)
      (format today "~D-~2,'0D-~2,'0D" yy mm dd)
      (when h
        (format today ":~2,'0D" hh)
        (when m
          (format today ":~2,'0D" mn)
          (when s
            (format today ":~2,'0D" ss)))))))


(defun now-time (&optional h m s)
  (today-date h m s))


;;;; end of file -- util.lisp
