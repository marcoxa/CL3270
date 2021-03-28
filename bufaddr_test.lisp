;;;; -*- Mode: Lisp -*-

;;;; bufaddr-test.lisp

(in-package "CL3270")

(defun test-encode ()
  (let ((encoded nil))

    (setq encoded (getpos 0 0))
    (when (or (/= (aref encoded 0) #x40) (/= (aref encoded 1) #x40))
      (error "Position (0, 0) not correctly encoded."))

    (setq encoded (getpos 11 39))
    (when (or (/= (aref encoded 0) #x4E) (/= (aref encoded 1) #xD7))
      (error "Position (11, 39) not correctly encoded."))
    t))
    

(defun test-decode ()
  (let ((decoded nil))
    
    (setq decoded (decode-buf-addr (vector #x40 #x40)))
    (unless (zerop decoded)
      (error "Buffer address [#x40 #x40] incorrectly decoded."))

    (setq decoded (decode-buf-addr (vector #x4E #xD7)))
    (unless (= 919 decoded)
      (error "Buffer address [#x4E #xD7] incorrectly decoded."))
    t))

;;;; end of file -- bufaddr-test.lisp
