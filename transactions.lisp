;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; transactions.lisp
;;;;
;;;; Transaction handling.
;;;;
;;;; See the file COPYING for copyright and licensing information.
;;;;
;;;; Notes:
;;;;
;;;; Most comments are taken directly from Matthew R. Wilson's.
;;;; 
;;;; After seeing 'example4' this functionality myst be changed to
;;;; accommodate a 'session' object.

(in-package "CL3270")

(defstruct abstract-session) ; Essentially a place-holder.

(deftype tx ()
  '(function (abstract-session usocket:usocket device-info t)
             (values abstract-session t #| tx |# t error)))


(defun run-transactions (conn dev initial data
                              &optional (s (make-abstract-session)))

  (declare (type usocket:usocket conn)
           (type (or null device-info) dev)
           (type (or null tx) initial)
           (type abstract-session s)
           )

  (let ((next initial)
        (err nil)
        )
    (declare (type (or null tx) next)
             (type (or null error) err))

    (unless dev
      (setq dev (make-device-info :rows 24
                                  :cols 80
                                  :term-type "DEFAULT")))

    (loop (dbgmsg "RT: next ~S~%" next)

          (setf (values s next data err)
                (funcall next s conn dev data))

          (when err
            (return-from run-transactions err))

          (unless next
            (return-from run-transactions nil))
          )))

;;;; end of file -- transactions.lisp
