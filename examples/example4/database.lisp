;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; database.lisp
;;;; cl3270 example 4 from Matthew Wilson's GO code.
;;;;
;;;; See the file COPYING in the top folder for copyright and
;;;; licensing information.
;;;;
;;;; This example demonstrates using the RUN-TRANSACTIONS approach to
;;;; structuring a CL3270 application.
;;;;
;;;; This file contains a mock in-memory database for the purposes of
;;;; our example application.
;;;;
;;;; Notes:
;;;;
;;;; This example works only in Lispworks for the time being.
;;;; Eventually there will be a portable Multiprocessing, LOCK and MAILBOX
;;;; implementation to fill in for GO channels (yes, I know about
;;;; Bordeaux Threads and ChanL).


(in-package "CL3270.E4")


(defstruct user
  (username "")
  (password "")
  (name "")
  (signup-date 0))


(defstruct dbstate
  (lock (mp:make-lock :name "DB Lock"))
  (users (make-dict :test #'equalp)))


(defun db-connect () (make-dbstate))


(define-condition user-exists-error () ()
  (:report "username already exists."))


(define-condition not-found-error () ()
  (:report "record not found."))


(defun get-user (dbs username)
  (mp:with-lock ((dbstate-lock dbs))
    (let ((user (gethash username (dbstate-users dbs))))
      (unless user
        (return-from get-user
          (values (make-user) (make-condition 'not-found-error))))

      (values user nil))))


(defun create-user (dbs user)
  (mp:with-lock ((dbstate-lock dbs))
    (let ((user (gethash (user-name user) (dbstate-users dbs))))
      (when user
        (return-from create-user
          (values (make-user) (make-condition 'user-exists-error)))))

    (setf (user-signup-date user) (get-universal-time))

    ;; Obviously in a real application you wouldn't store plaintext
    ;; password, but this is a simple example for the purposes of
    ;; demonstrating go3270, not demonstrating how to build a secure
    ;; application.

    (setf (gethash (user-username user) (dbstate-users dbs)) user)

    (values user nil)))


(defun update-user (dbs user)
  (mp:with-lock ((dbstate-lock dbs))
    (let ((olduser (gethash (user-username user) (dbstate-users dbs))))
      (unless olduser
        (return-from update-user
          (values (make-user) (make-condition 'not-found-error))))

      ;; Make sure original signup date is maintained.

      (setf (user-signup-date user) (user-signup-date olduser))

      ;; Carry over existing password if we aren't setting a new
      ;; password.

      (when (string= "" (user-password user))
        (setf (user-password user) (user-password olduser)))

      (setf (gethash (user-name user) (dbstate-users dbs)) user)

      (values user nil))))


;;;; end of file -- database.lisp
