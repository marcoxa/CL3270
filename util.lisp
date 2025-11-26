;;;; -*- Mode: Lisp -*-

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


;;;; end of file -- util.lisp
