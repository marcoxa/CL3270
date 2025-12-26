;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; help.lisp
;;;; cl3270 example 4 from Matthew Wilson's GO code.
;;;;
;;;; See the file COPYING in the top folder for copyright and
;;;; licensing information.
;;;;
;;;; This example demonstrates using the RUN-TRANSACTIONS approach to
;;;; structuring a CL3270 application.
;;;;
;;;; This file contains the help transaction generator.
;;;;
;;;; Notes:
;;;;
;;;; This example works only in Lispworks for the time being.
;;;; Eventually there will be a portable Multiprocessing, LOCK and MAILBOX
;;;; implementation to fill in for GO channels (yes, I know about
;;;; Bordeaux Threads and ChanL).


(in-package "CL3270.E4")

(defparameter *help-screen*
  (make-screen
   "E4 Help Screen"
   (make-field :row 0 :col 45 :Intense t :content "Online Help")

   (make-field :row 2 :col 0
               :content "This help screen is an example of a transaction that")

   (make-field :row 3 :col 0
               :content "can be used as the next transaction from many other transactions")

   (make-field :row 4 :col 0 :content "and returns to the transaction that called it.")

   (make-field :row 6 :col 0 :content "Press")
   (make-field :row 6 :col 6 :content "PF3" :color +white+ :intense t)
   (make-field :row 6 :col 10
               :content "to return to the transaction from which you came.")

   ;; Error message
   (make-field :row 21 :col 0 :name "errormsg" :color +red+ :intense t)

   ;; Key legend
   (make-field :row 23 :col 1 :content "F3=Exit")
   ))


;;; help
;;;
;;; The help transaction is an example of returning a closure over
;;; some parameters, in this case the session and the desired next
;;; transaction. This allows us to use the same transaction from
;;; multiple other transactions and return to the requested
;;; transaction when the user leaves the help transaction.
;;;
;;; Any data passed to help will be passed back to the transaction it
;;; returns to.
;;;
;;; The help transaction has no need for any global or session state,
;;; so this generator function for it is a stand-alone function and
;;; not a method on the session.
;;;
;;; Notes:
;;;
;;; Yep, CPS!

(defun help (s return-transaction)
  (declare (ignore s))

  #'(lambda (session conn devinfo data)
      (let ((err
             (nth-value
              1
              (handle-screen *help-screen*
                             nil
                             nil
                             (list +aid-pf3+)
                             nil
                             "errormsg" ; name of field to put error
                                        ; messages in.
                             23 79
                             conn
                             (codepage devinfo)))))
        (when err
          (return-from help (values session nil nil err)))
        
        ;; Any accepted key returns to the requested
        ;; RETURN-TRANSACTION.
        
        (values session return-transaction data nil))))


;;;; end of file -- help.lisp
