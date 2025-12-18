;;;; -*- Mode: Lisp -*-

;;;; example1.lisp
;;;; cl3270 example 1

(in-package "CL3270")

(defparameter screen1
  (make-screen
   (make-field :row 0 :col 27 :intense t
               :content "3270 example application")
   (make-field :row 2 :col 0
               :content "Welcome to the cl3270 example application. please enter your name.")
   (make-field :row 4 :col 0
               :content "First Name  . . .")
   (make-field :row 4 :col 19 :name "fname" :write t :highlighting +underscore+)
   (make-field :row 4 :col 40) ; Field "stop" character
   (make-field :row 5 :col 0
               :content "Last Name . . . .")
   (make-field :row 5 :col 19 :name "lname" :write t :highlighting +underscore+)
   (make-field :row 5 :col 40) ; Field "stop" character
   (make-field :row 6 :col 0
               :content "Password  . . . .")
   (make-field :row 6 :col 19 :name "password" :write t :hidden t)
   (make-field :row 6 :col 40) ; Field "stop" character
   (make-field :row 8 :col 0
               :content "Press")
   (make-field :row 8 :col 6 :intense t :content "enter")
   (make-field :row 8 :col 12
               :content "to submit your name.")
   (make-field :row 10 :col 0 :intense t
               :color +red+ :name "errormsg") ; A blank field for error messages.
   (make-field :row 22 :Col 0
               :content "PF3 Exit")
   ))


(defparameter screen2
  (make-screen
   (make-field :row 0 :col 27 :intense t
               :content "3270 Example Application")
   (make-field :row 2 :col 0
               :content "Thank you for submitting your name. Here's what I know:")
   (make-field :row 4 :col 0 :content "Your first name is")
   (make-field :row 4 :col 19 :Name "fname") ; We're giving this field
                                             ; a name to replace its
                                             ; value at runtime.
   (make-field :row 5 :col 0 :content "And your last name is")
   (make-field :row 5 :col 22 :name "lname") ; We're giving this field
                                             ; a name to replace its
                                             ; value at runtime.
   (make-field :row 6 :col 0 :name "passwordOutput")
   (make-field :row 8 :col 0 :content "Press")
   (make-field :row 8 :col 6 :intense t :Content "enter")
   (make-field :row 8 :col 12 :content "to enter your name again or")
   (make-field :row 8 :col 41 :intense t :Content "PF3")
   (make-field :row 8 :col 45 :content "to quit and disconnect.")
   (make-field :row 11 :Col 0 :color +turquoise+ :highlighting +reverse-video+
               :content "Here is a field with extended attributes.")
   (make-field :row 11 :col 42) ; Remember to "stop" fields with a
                                ; regular field to clear the reverse
                                ; video for example.
   (make-field :row 22 :col 0 :content "PF3 Exit")
   ))


(defparameter goodbye-screen
  (make-screen
   (make-field :row 0 :col 0 :intense t
               :content "3270 Example Application")
   (make-field :row 2 :col 0
               :content "Thank you for using this application. Goodbye.")))



#| Possible alternative
(defun cl3270-example1 ()
  (usocket:socket-server "127.0.0.1" 3270
                         #'cl3270-handle
                         ()
                         :element-type '(unsigned-byte 8)
                         :multi-threading t
                         ))
|#


(defun cl3270-example1 (&key (handler 'cl3270-handle) (host "127.0.0.1") (debug t))
  (prog1
      (usocket:with-socket-listener (conn host 3270
                                          :element-type '(unsigned-byte 8)
                                          )
        (format t "CL3270: server listening on host ~S, port 3270...~%"
                host)
        (usocket:with-connected-socket (c (usocket:socket-accept conn))
          (let ((*do-debug* debug))
            (funcall handler c))))
    (format t "CL3270: server closed~%")))



#|
(defun cl3270-handle (c &aux (field-values (make-hash-table :test #'equal)))
  ;; C is a USOCKET:SOCKET
  (unwind-protect
      (progn
        (negotiate-telnet c)

        (dbgmsg ">>> Telnet negotiated.~2%")

        (loop named mainloop do
              (tagbody
               start-screen1-loop
               (loop do
                     (setf (gethash "password" field-values) "")
                     (multiple-value-bind (resp err)
                         (show-screen screen1 field-values 4 20 c)

                       (when err
                         (format *error-output* "!!! SHOW-SCREEN 1 error ~S~%" err)
                         (return-from cl3270-handle err))
                                                
                       (when (= (response-aid resp) +aid-pf3+)
                         (return-from mainloop t))
                         
                       (when (/= (response-aid resp) +aid-enter+)
                         (go start-screen1-loop))

                       ;; User must have pressed "Enter", so let's
                       ;; check the input.

                       (setf field-values (response-values resp))
                       (let ((fname
                              (string-trim " "
                                           (gethash "fname" field-values)))
                             (lname
                              (string-trim " "
                                           (gethash "lname" field-values)))
                             )
                         (when (and (string= "" fname)
                                    (string= "" lname))
                           (setf (gethash "errormsg" field-values)
                                 "First and Last Name fields are required.")
                           (go start-screen1-loop))

                         (when (string= "" fname)
                           (setf (gethash "errormsg" field-values)
                                 "First Name field is required.")
                           (go start-screen1-loop))

                         (when (string= "" lname)
                           (setf (gethash "errormsg" field-values)
                                 "Last Name field is required.")
                           (go start-screen1-loop))


                         ;; At this point, we know the user provided
                         ;; both fields and had hit enter, so we are
                         ;; going to reset the error message for the
                         ;; next time through the loop, and break out
                         ;; of this loop so we move on to screen 2.

                         (setf (gethash "errormsg" field-values) "")
                         (loop-finish)
                         )))

               ;; Now we're ready to display screen2
               (let ((password-length
                      (length (string-trim " "
                                           (gethash "password" field-values))))
                     )

                 (setf (gethash "passwordOutput" field-values)
                       (format nil
                               "Your password was ~D character~:P long."
                               password-length))

                 (multiple-value-bind (resp err)
                     (show-screen screen2 field-values 0 0 c)
                   (when err
                     (format *error-output*
                             "CL3270: error: SHOW-SCREEN 2 error ~S~%." err)
                     (return-from mainloop err))

                   (when (= (response-aid resp) +aid-pf3+)
                     (return-from mainloop t))
                   ))
               ))
        )
    (usocket:socket-close c)
    (dbgmsg "CL3270: connection closed.~%"))
  )
|#


(defun cl3270-handle (c &aux (field-values (make-hash-table :test #'equal)))
  ;; C is a USOCKET:SOCKET

  (declare (type usocket:stream-usocket c))

  (handler-case
      (multiple-value-bind (devinfo err)
          (negotiate-telnet c)

        (when err
          (format *error-output* "CL3270: error: ~S~%" err)
          (return-from cl3270-handle nil))

        (dbgmsg "CL3270: telnet negotiated.~2%")

        (loop named mainloop do
              (tagbody
               start-screen1-loop
               (loop do
                     (setf (gethash "password" field-values) "")
                     (multiple-value-bind (resp err)
                         (show-screen-opts screen1
                                           field-values
                                           c
                                           (make-screen-opts
                                            :codepage (codepage devinfo)
                                            :cursor-row 4
                                            :cursor-col 20))

                       (when err
                         (format *error-output*
                                 "CL3270: error SHOW-SCREEN 1 error ~S~%" err)
                         ;; (return-from cl3270-handle err)
                         (error err))
                                                
                       (when (= (response-aid resp) +aid-pf3+)
                         (show-screen-opts goodbye-screen
                                           nil
                                           c
                                           (make-screen-opts
                                            :codepage (codepage devinfo)
                                            :no-response t))
                         (sleep 2)
                         (return-from mainloop t))
                         
                       (when (/= (response-aid resp) +aid-enter+)
                         (go start-screen1-loop))

                       ;; User must have pressed "Enter", so let's
                       ;; check the input.

                       (setf field-values (response-vals resp))
                       (let ((fname
                              (string-trim " "
                                           (gethash "fname" field-values)))
                             (lname
                              (string-trim " "
                                           (gethash "lname" field-values)))
                             )
                         (when (and (string= "" fname)
                                    (string= "" lname))
                           (setf (gethash "errormsg" field-values)
                                 "First and Last Name fields are required.")
                           (go start-screen1-loop))

                         (when (string= "" fname)
                           (setf (gethash "errormsg" field-values)
                                 "First Name field is required.")
                           (go start-screen1-loop))

                         (when (string= "" lname)
                           (setf (gethash "errormsg" field-values)
                                 "Last Name field is required.")
                           (go start-screen1-loop))


                         ;; At this point, we know the user provided
                         ;; both fields and had hit enter, so we are
                         ;; going to reset the error message for the
                         ;; next time through the loop, and break out
                         ;; of this loop so we move on to screen 2.

                         (setf (gethash "errormsg" field-values) "")
                         (loop-finish)
                         )))

               ;; Now we're ready to display screen2
               (let ((password-length
                      (length (string-trim " "
                                           (gethash "password" field-values))))
                     )

                 (setf (gethash "passwordOutput" field-values)
                       (format nil
                               "Your password was ~D character~:P long."
                               password-length))

                 (multiple-value-bind (resp err)
                     (show-screen screen2 field-values 0 0 c)
                   (when err
                     (format *error-output*
                             "CL3270: error: SHOW-SCREEN 2 error ~S~%." err)
                     (return-from mainloop err))

                   (when (= (response-aid resp) +aid-pf3+)
                     (return-from mainloop t))
                   ))
               ))
        )

    (error (e)
      (usocket:socket-close c)
      (dbgmsg "CL3270: connection closed.~%")
      (error e))

    (:no-error (result)
      (usocket:socket-close c)
      (dbgmsg "CL3270: closing connection and returning ~S.~%" result)
      result)
    ))


;;;; end of file -- example1.lisp
