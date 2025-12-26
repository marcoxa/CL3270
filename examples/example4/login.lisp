;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; login.lisp
;;;; cl3270 example 4 from Matthew Wilson's GO code.
;;;;
;;;; See the file COPYING in the top folder for copyright and
;;;; licensing information.
;;;;
;;;; This example demonstrates using the RUN-TRANSACTIONS approach to
;;;; structuring a CL3270 application.
;;;;
;;;; This file contains the login and user registration transactions.
;;;;
;;;; Notes:
;;;;
;;;; This example works only in Lispworks for the time being.
;;;; Eventually there will be a portable Multiprocessing, LOCK and MAILBOX
;;;; implementation to fill in for GO channels (yes, I know about
;;;; Bordeaux Threads and ChanL).


(in-package "CL3270.E4")

(defconstant +login-user-name+ "username")
(defconstant +login-password+ "password")
(defconstant +login-password-conf+ "password confirmation")
(defconstant +login-name+ "name")
(defconstant +login-error+ "errormsg")


(defparameter *login-screen*
  (make-screen
   "E4 Login Screen"
   (make-field :row 0 :col 37 :intense t :content "Logon")

   (make-field :row 2 :col 0
               :content "Welcome to the go3270 example application. Please log on.")

   ;; Username
   (make-field :row 4 :col 0 :content "Username . . ." :color +green+)
   (make-field :row 4 :col 15 :name +login-user-name+
               :write t :highlighting +underscore+ :color +turquoise+)
   (make-field :row 4 :col 24 :Autoskip t) ; field "stop" character

   ;; Password
   (make-field :row 5 :col 0 :content "Password . . ." :color +green+)
   (make-field :row 5 :col 15 :name +login-password+ :write t :hidden t)
   (make-field :row 5 :col 79) ; field "stop" character

   ;; Registration instructions
   (make-field :row 7 :col 0 :content "If you don't yet have an account press")
   (make-field :row 7 :col 40 :content "PF5" :color +white+ :intense t)
   (make-field :row 7 :col 44 :content "to register a new account.")

   ;; Error message
   (make-field :row 21 :col 0 :name +login-error+ :color +red+ :intense t)

   ;; Key legend
   (make-field :row 23 :col 1  :content "F1=Help")
   (make-field :row 23 :col 14 :content "F3=Exit")
   (make-field :row 23 :col 27 :content "F5=Register")
   )
  "The Example 4 Login Screen")


(defparameter *login-screen-rules*
  (make-rules
   (field-rules +login-user-name+ :validator #'non-blank-validator)
   (field-rules +login-password+ :validator #'non-blank-validator :reset t)
   ))


;;; login
;;;
;;; Notes:
;;;
;;; Do I need CLOS for this?  No, I don't

(defun login (s conn devinfo data)

  (declare (type session s)
           (type usocket:stream-usocket conn)
           (type (or null device-info) devinfo))

  (let ((field-vals (make-dict :test #'equalp))
        (username "")
        (password "")
        (user "")
        (resp nil)
        (err nil)
        )

    (unless (and data (stringp data))
      (setf (gethash +login-error+ field-vals) ""))

    (multiple-value-setq (resp err)
        (handle-screen *login-screen*
                       *login-screen-rules*
                       field-vals
                       (list +aid-enter+)
                       (list +aid-pf1+
                             +aid-pf3+
                             +aid-pf5+
                             +aid-clear+)
                       +login-error+
                       4 16
                       conn
                       (codepage devinfo)))

    (when err
      (return-from login (values nil nil err)))

    (let ((resp-aid (response-aid resp)))
      (cond ((is-key resp-aid +aid-clear+)
             (return-from login (values s #'login nil nil)))

            ((is-key resp-aid +aid-pf1+)
             (return-from login (values s (help s #'login) nil nil)))

            ((is-key resp-aid +aid-pf3+)
             (return-from login (values s nil nil nil)))

            ((is-key resp-aid +aid-pf5+)
             (return-from login (values s #'new-user nil nil)))
            ))

    ;; If we didn't get one of the other allowed keys, try to log
    ;; the user in.
    
    (setf username (gethash +login-user-name+ (response-vals resp))
          password (gethash +login-password+ (response-vals resp)))
    
    (multiple-value-setq (user err) (get-user (session-db s) username))

    (when (or err (string/= (user-password (session-db s)) password))

      ;; If we couldn't get the username from the DB, or if the
      ;; password doesn't match, re-run the login transaction with an
      ;; error message displayed.

      (return-from login
        (values s
                #'login
                "Username or password not valid"
                nil)))

    ;; Login was successful. Update the session state to the logged-in user.
    
    (setf (session-user s) user)

    (values s #'main-menu nil nil)
    ))
  

(defparameter *new-user-screen*
  (make-screen
   "E4 New User Screen"
   (make-field :row 0 :col 30 :intense t
               :content "New User Registration")

   (make-field :row 2 :col 0
               :content "Please provide your user registration details.")

   ;; Username
   (make-field :row 4 :col 0 :content "Username . . ." :color +green+)
   (make-field :row 4 :col 15 :name +login-user-name+ :Write t
               :highlighting +underscore+ :color +turquoise+)
   (make-field :row 4 :col 24 :autoskip t) ; field "stop" character

   ;; Password
   (make-field :row 5 :col 0 :content "Password . . ." :color +green+)
   (make-field :row 5 :col 15 :name +login-password+ :write t :hidden t)
   (make-field :row 5 :col 79 :autoskip t) ; field "stop" character

   ;; Password
   (make-field :row 6 :col 0 :content "Confirm Pass ." :color +green+)
   (make-field :row 6 :col 15 :name +login-password-conf+ :write t :hidden t)
   (make-field :row 6 :col 79 :autoskip t) ; field "stop" character

   ;; Name
   (make-field :row 7 :col 0 :content "Name . . . . ." :color +green+)
   (make-field :row 7 :col 15 :name +login-name+ :Write t
               :highlighting +underscore+ :color +turquoise+)
   (make-field :row 7 :col 46 :autoskip t) ; field "stop" character

   ;; Error message
   (make-field :row 21 :col 0 :name +login-error+ :color +red+ :intense t)

   ;; Key legend
   (make-field :row 23 :col 1 :content "F1=Help")
   (make-field :row 23 :col 14 :content "F3=Exit")
   ))


(defparameter *new-user-screen-rules*
  (make-rules
   (field-rules +login-user-name+     :validator #'non-blank-validator)
   (field-rules +login-password+      :validator #'non-blank-validator)
   (field-rules +login-password-conf+ :validator #'non-blank-validator)))


(defstruct new-user-data
  (username "")
  (name "")
  (errmsg ""))


;;; new-user
;;;
;;; Notes:
;;;
;;; Do I need CLOS for this? No.

(defun new-user (s conn devinfo data)

  (declare (type session s)
           (type usocket:stream-usocket conn)
           (type device-info devinfo))

  (let ((field-vals (make-dict :test #'equalp))
        (username "")
        (password "")
        (password-conf "")
        (user "")
        (name "")
        (resp nil)
        (err nil)
        )

    (when (and data (new-user-data-p data))
      (setf (gethash +login-user-name+ field-vals)
            (new-user-data-username data)

            (gethash +login-name+ field-vals)
            (new-user-data-name data)

            (gethash +login-error+ field-vals)
            (new-user-data-errmsg data)))

    (multiple-value-setq (resp err)
        (handle-screen *new-user-screen*
                       *new-user-screen-rules*
                       field-vals
                       (list +aid-enter+)
                       (list +aid-pf1+
                             +aid-pf3+
                             +aid-clear+)
                       +login-error+
                       4 16
                       conn
                       (codepage devinfo)))

    (when err
      (return-from new-user (values nil nil err)))

    (let ((resp-aid (response-aid resp)))
      (cond ((is-key resp-aid +aid-clear+)
             (return-from new-user (values s #'new-user nil nil)))

            ((is-key resp-aid +aid-pf1+)
             (return-from new-user (values s (help s #'new-user) nil nil)))

            ((is-key resp-aid +aid-pf3+)
             (return-from new-user (values s #'login nil nil)))
            ))

    ;; If we didn't get one of the other allowed keys, try to log
    ;; the user in.
    
    (setf username (gethash +login-user-name+ (response-vals resp))
          password (gethash +login-password+ (response-vals resp))
          password-conf (gethash +login-password-conf+ (response-vals resp))
          name     (gethash +login-name+ (response-vals resp))
          )
    
    (when (string/= password password-conf)
      ;; Re-run the newuser transaction with an error message
      (return-from new-user
        (values s
                #'new-user
                (make-new-user-data
                 :username username
                 :name name
                 :errmsg "Passwords do not match.")
                nil)))
        
      
    (multiple-value-setq (user err)
        (create-user (session-db s)
                     (make-user :username username
                                :password password
                                :name name)))

    (dbgmsg "E4: NEW-USER: CREATE-USER -> ~S ~S~%" user err)

    (when (eq err 'error-user-exists)

      ;; Re-run the newuser transaction with an error message

      (return-from new-user
        (values s
                #'new-user
                (make-new-user-data
                 :username username
                 :name name
                 :errmsg "Username already exists; please choose a new one.")
                 nil)))

    (when err
      (return-from new-user
        (values s
                #'new-user
                (make-new-user-data
                 :username username
                 :name name
                 :errmsg "Unknown error creating new user")
                nil)))


    ;; Success! We'll stick the new user in the session state like login does
    ;; and go to the main menu.

    (setf (session-user s) user)

    (values s #'main-menu nil nil)
    ))


;;;; end of file -- login.lisp
