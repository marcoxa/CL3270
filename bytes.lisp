;;;; -*- Mode: Lisp: Coding: utf-8 -*-

;;;; bytes.lisp --
;;;;
;;;; Functions needed to handle bytes (UNISIGNED-BYTE 8) and byte
;;;; buffers.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(in-package "CL3270")

(deftype octet ()
  "The Octet Type.

Useful as a shorthand."
  '(unsigned-byte 8))


(deftype rune ()
  "The Rune Type.

A shorthand for two-bytes 'code points'."
  '(unsigned-byte 16))


(deftype dict () 
  "The Dict Type.

Just a synonym of hash-table."
  'hash-table)


(deftype buffer ()
  "The Buffer Type.

Denotes the vectors that contain `octet's.

Notes:

Often these vectors have a fill pointer and are adjustable."
  '(array octet (*)))


(declaim (inline make-buffer))
(defun make-buffer (&key (length 0)
                         (capacity 5280) ; 132 * 40.
                         )
  (declare (type (integer 0 (#.array-total-size-limit)) length)
           (type (integer 1 (#.array-total-size-limit)) capacity))
  (assert (<= length capacity))
  (make-array capacity
              :element-type 'octet
              :initial-element 0
              :fill-pointer length
              ))


(declaim (ftype (function (buffer octet) buffer) write-buffer)
         (inline write-buffer))
(defun write-buffer (buffer b)
  "Write a (unsigned) byte, an octet, B at the end of BUFFER.

The 'end' of BUFFER is actually its FILL-POINTER; that is, BUFFER must
be a vector with a FILL-POINTER.

The (modified) BUFFER is returned."

  (declare (type buffer buffer)
           (type unsigned-byte b))
  (vector-push b buffer)
  buffer)


(declaim (ftype (function (buffer (vector octet)) buffer) write-buffer*)
         (inline write-buffer*))
(defun write-buffer* (buffer bs)
  "Appends a vector of (unsigned) bytes BS at the end of BUFFER.

The 'end' of BUFFER is actually its FILL-POINTER; that is, BUFFER must
be a vector with a FILL-POINTER.

The (modified) BUFFER is returned."

  (declare (type vector bs)
           (type buffer buffer))
  (loop for b of-type octet across bs
        do (vector-push b buffer))
  buffer)


(declaim (ftype (function (buffer &rest octet) buffer) write-octets-buffer)
         (inline write-octets-buffer))
(defun write-octets-buffer (buffer &rest octets)
  "Appends OCTETS at the end of BUFFER.

OCTETS is a list of OCTET.
The 'end' of BUFFER is actually its FILL-POINTER; that is, BUFFER must
be a vector with a FILL-POINTER.

The (modified) BUFFER is returned."

  (declare (type list octets)
           (type buffer buffer))

  (loop for b of-type octet in octets
        do (vector-push b buffer))
  buffer)
  

(declaim (ftype (function (&rest octet) buffer) bufferize))
(defun bufferize (&rest octets)
  (let ((buf (make-buffer :capacity (length octets))))
    (dolist (o octets buf)
      (vector-push o buf))))

;;;; end of file -- bytes.lisp
