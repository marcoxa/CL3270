;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; mainmenu.lisp
;;;; cl3270 example 4 from Matthew Wilson's GO code.
;;;;
;;;; See the file COPYING in the top folder for copyright and
;;;; licensing information.
;;;;
;;;; This example demonstrates using the RUN-TRANSACTIONS approach to
;;;; structuring a CL3270 application.
;;;;
;;;; This file contains the main menu and the example placeholder features
;;;; of the application.
;;;;
;;;; Notes:
;;;;
;;;; This example works only in Lispworks for the time being.
;;;; Eventually there will be a portable Multiprocessing, LOCK and MAILBOX
;;;; implementation to fill in for GO channels (yes, I know about
;;;; Bordeaux Threads and ChanL).


(in-package "CL3270.E4")


(defconstant +main-menu-option+ "option")
(defconstant +main-menu-username+ "username")
(defconstant +main-menu-time+ "time")
(defconstant +main-menu-users+ "users")
(defconstant +main-menu-errormsg+ "errormsg")


(defparameter *main-menu-screen*
  (make-screen
   "E4 Main Menu Screen"
   (make-field :row 0 :col 31 :intense t :content "Main Menu")

   ;; Option
   (make-field :row 1 :col 0 :content "Option ===>" :color +green+)
   (make-field :row 1 :col 12 :name +main-menu-option+ :write t
               :highlighting +underscore+ :color +turquoise+)
   (make-field :row 1 :col 79 :Autoskip t) ; field "stop" character

   ;; Info
   (make-field :row 3 :col 57 :content "User ID . :" :color +green+)
   (make-field :row 3 :col 69 :name +main-menu-Username+ :color +turquoise+)
   (make-field :row 4 :col 57 :content "Time. . . :" :color +green+)
   (make-field :row 4 :col 69 :name +main-menu-Time+ :color +turquoise+)
   (make-field :row 5 :col 57 :content "Users . . :" :color +green+)
   (make-field :row 5 :col 69 :name +main-menu-Users+ :color +turquoise+)

   ;; Options
   (make-field :row 3 :col 0 :content "1" :color +white+)
   (make-field :row 3 :col 3 :content "Feature 1"
               :color +turquoise+ :intense t)
   (make-field :row 3 :col 17 :content "A very cool feature" :color +green+)

   (make-field :row 4 :col 0 :content "2" :color +white+)
   (make-field :row 4 :col 3 :content "Feature 2"
               :color +turquoise+ :intense t)
   (make-field :row 4 :col 17 :content "Another neat feature" :color +green+)

   (make-field :row 5 :col 0 :content "3" :color +white+)
   (make-field :row 5 :col 3 :content "Feature 3"
               :color +turquoise+ :intense t)
   (make-field :row 5 :col 17 :content "This one's a boring feature"
               :color +green+)

   (make-field :row 7 :col 5 :content "Enter" :color +green+)
   (make-field :row 7 :col 11 :content "X" :color +turquoise+ :intense t)
   (make-field :row 7 :col 13 :content "to log off and exit." :color +green+)

   ;; Error message
   (make-field :row 21 :col 0 :name +main-menu-errormsg+ :color +red+ :intense t)

   ;; Key legend
   (make-field :row 23 :col 1 :content "F1=Help")
   (make-field :row 23 :col 14 :content "F3=Exit")
   (make-field :row 23 :col 27 :content "F5=Refresh")
   ))


(defstruct main-menu-data
  (option "")
  (errormsg ""))


;;; main-menu
;;;
;;; The main-menu transaction accepts a mainmenuData struct as the
;;; data if the option field or error message should be populated.

(defun main-menu (session c devinfo data)
  (let ((field-values (make-dict :test #'equalp))
        (resp nil)
        (err nil))
    (when (and data (main-menu-data-p data))
      (setf (gethash +main-menu-option+ field-values)
            (main-menu-data-option data)

            (gethash +main-menu-option+ field-values)
            (main-menu-data-errormsg data)))

    (setf (gethash +main-menu-username+ field-values)
          (string-upcase (user-username (session-user session)))

          (gethash +main-menu-time+ field-values)
          (now-time)

          (gethash +main-menu-users+ field-values)
          (format nil "~D" (global-usercount (session-global session))))

    (multiple-value-setq (resp err)
        (handle-screen *main-menu-screen*
                       nil
                       field-values
                       (list +aid-enter+)
                       (list +aid-pf1+
                             +aid-pf3+
                             +aid-pf5+)
                       +main-menu-errormsg+
                       1 13
                       c
                       (codepage devinfo)
                       ))

    (when err
      (return-from main-menu (values session nil nil err)))

    (let ((r-aid (response-aid resp)))
      (cond ((is-key r-aid +aid-pf1+)
             (return-from main-menu
               (values session (help session #'main-menu) nil nil)))

            ((is-key r-aid +aid-pf3+)
             ;; Exit
             (return-from main-menu
               (values session nil nil nil)))

            ((is-key r-aid +aid-pf5+)
             (return-from main-menu
               (values session #'main-menu nil nil)))
            ))

    (let ((mm-option (gethash +main-menu-option+ (response-vals resp))))
      (cond ((string-equal "1" mm-option)
             (return-from main-menu
               (values session
                       #'example-feature
                       "This is feature 1, a very cool feature."
                       nil)))

            ((string-equal "2" mm-option)
             (return-from main-menu
               (values session
                       #'example-feature
                       "This is feature 2, another neat feature."
                       nil)))

            ((string-equal "3" mm-option)
             (return-from main-menu
               (values session
                       #'example-feature
                       "This is feature 3, which is a boring one."
                       nil)))

            ((or (string-equal "x" mm-option) (string-equal "X" mm-option))
             ;; Exit
             (return-from main-menu
               (values session nil nil nil)))

            ((string-equal "" mm-option)
             ;; As if PF5, refresh
             (return-from main-menu
               (values session #'main-menu nil nil)))

            (t
             ;; Run the transaction again with an error message.
             (return-from main-menu
               (values session
                       #'main-menu
                       (make-main-menu-data
                        :option mm-option
                        :errormsg "Unknown option")
                       nil)))
            ))))


(defconstant +feature-message+ "message")

(defparameter *example-screen*
  (make-screen
   "E4 Example Screen"
   (make-field :row 0 :col 30 :intense t :Content "Application Feature")

   ;; Info -- replicate the data shown on the main menu
   (make-field :row 3 :col 57 :Content "User ID . :" :color +green+)
   (make-field :row 3 :col 69 :Name +main-menu-username+ :color +turquoise+)

   (make-field :row 4 :col 57 :Content "Time. . . :" :color +green+)
   (make-field :row 4 :col 69 :Name +main-menu-time+ :color +turquoise+)

   (make-field :row 5 :col 57 :Content "Users . . :" :color +green+)
   (make-field :row 5 :col 69 :Name +main-menu-users+ :color +turquoise+)

   ;; Feature-specific message
   (make-field :row 12 :col 0 :Name +feature-message+)

   (make-field :row 14 :col 0 :Content "Press")
   (make-field :row 14 :col 6 :Content "PF3" :intense t :color +white+)
   (make-field :row 14 :col 10 :Content "to return to the main menu.")

   ;; Error message
   (make-field :row 21 :col 0 :Name +main-menu-errormsg+ :color +red+ :intense t)

   ;; Key legend
   (make-field :row 23 :col 1 :Content "F1=Help")
   (make-field :row 23 :col 14 :Content "F3=Exit")
   (make-field :row 23 :col 27 :Content "F5=Refresh")
   ))


;;; example-feature
;;;
;;; example-feature is a transaction that will act as a placeholder
;;; for real application functionality. It accepts a string in the
;;; data which will be displayed on the panel.

(defun example-feature (session conn devinfo data)
  (let ((field-values (make-dict :test #'equal))
        (resp nil)
        (err nil)
        )

    (when (and data (stringp data))
      (setf (gethash +feature-message+ field-values) data))

    (setf (gethash +main-menu-username+ field-values)
          (string-upcase (user-username (session-user session)))

          (gethash +main-menu-time+ field-values)
          (now-time)
          
          (gethash +main-menu-users+ field-values)
          (format nil "~D"
                  (global-usercount (session-global session))))

    (multiple-value-setq (resp err)
        (handle-screen *example-screen*
                       nil
                       field-values
                       nil
                       (list +aid-pf1+
                             +aid-pf3+
                             +aid-pf5+)
                       +main-menu-errormsg+
                       23 79
                       conn
                       (codepage devinfo)))

    (when err
      (return-from example-feature (values session nil nil err)))

    (let ((r-aid (response-aid resp)))
      (cond ((is-key r-aid +aid-pf1+)
             ;; Display help, then return to this transaction
             (return-from example-feature
               (values session (help session #'example-feature) data nil)))

            ((is-key r-aid +aid-pf3+)
             ;; Exit
             (return-from example-feature
               (values session #'main-menu nil nil)))

            ((is-key r-aid +aid-pf5+)
             ;; Refresh
             (values session #'example-feature data nil))
            ))

    ;; ...there shouldn't be any actions not handled above. Just in
    ;; case, we'll just re-run this transaction as if refresh was hit.

    (values session #'example-feature data nil)
    ))

;;;; end of file -- mainmenu.lisp
