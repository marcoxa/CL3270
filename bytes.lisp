;;;; -*- Mode: Lisp -*-

;;;; bytes.lisp --
;;;;
;;;; Functions needed to handle bytes (UNISIGNED-BYTE 8) and byte
;;;; buffers.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(in-package "CL3270")

(deftype octet ()
  '(unsigned-byte 8))


(deftype buffer ()
  '(array (unsigned-byte 8) (*)))


(declaim (inline make-buffer))
(defun make-buffer (&key (length 0)
                         (capacity 2048) ; A bit more than 80 * 24.
                         )
  (declare (type (integer 0 (#.array-total-size-limit)) length)
           (type (integer 1 (#.array-total-size-limit)) capacity))
  (make-array capacity
              :element-type '(unsigned-byte 8)
              :initial-element 0
              :fill-pointer (min length (max 1 (1- capacity))) ; Being paranoid.
              ))


(declaim (inline write-buffer))
(defun write-buffer (buffer b)
  "Write a (unsigned) byte B at the end of BUFFER.

The 'end' of BUFFER is actually its FILL-POINTER; that is, BUFFER must
be a vector with a FILL-POINTER.

The (modified) BUFFER is returned."
  (declare (type buffer buffer)
           (type unsigned-byte b))
  (vector-push b buffer)
  buffer)


(declaim (inline write-buffer*))
(defun write-buffer* (buffer bs)
  "Appends a vector of (unsigned) bytes BS at the end of BUFFER.

The 'end' of BUFFER is actually its FILL-POINTER; that is, BUFFER must
be a vector with a FILL-POINTER.

The (modified) BUFFER is returned."
  (declare (type vector bs)
           (type buffer buffer))
  (loop for b of-type unsigned-byte across bs
        do (vector-push b buffer))
  buffer)


;;;; end of file -- bytes.lisp

                   
  
  

             
  
  



