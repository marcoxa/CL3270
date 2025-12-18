;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; transactions.lisp
;;;;
;;;; Transaction handling.
;;;;
;;;; See the file COPYING for copyright and licensing information.
;;;;
;;;; Notes:
;;;;
;;;; Most comments are taken directly from Matthew Wilson's.


(in-package "CL3270")


(deftype tx ()
  `(function (usocket:usocket device-info t)
             (values t #| tx |# t error)))


(defun run-transactions (conn dev initial data)

  (declare (type usocket:usocket conn)
           (type (or null device-info) dev)
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

    (loop (setf (values next data err)
                (funcall next conn dev data))

          (when err
            (return-from run-transactions err))

          (unless next
            (return-from run-transactions nil))
          )))

;;;; end of file -- transactions.lisp
