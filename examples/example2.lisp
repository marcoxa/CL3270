;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; example2.lisp
;;;; cl3270 example 2

(in-package "CL3270")

(defparameter example2-screen1
  (make-screen
   "Example 2 Screen 1"
   (make-field :row 0 :col 27 :intense t
               :content "3270 example 2 application")
   (make-field :row 2 :col 0
               :content "Welcome to the cl3270 example 2 application. please enter your name.")

   (make-field :row 4 :col 0 :content "First Name  . . .")
   (make-field :row 4 :col 19 :name "fname" :write t :highlighting +underscore+)
   (make-field :row 4 :col 40 :autoskip t) ; Field "stop" character

   (make-field :row 5 :col 0 :content "Last Name . . . .")
   (make-field :row 5 :col 19 :name "lname" :write t :highlighting +underscore+)
   (make-field :row 5 :col 40 :autoskip t) ; Field "stop" character

   (make-field :row 6 :col 0 :content "Password  . . . .")
   (make-field :row 6 :col 19 :name "password" :write t :hidden t)
   (make-field :row 6 :col 40) ; Field "stop" character

   (make-field :row 7 :col 0 :content "Change me   . . .")
   (make-field :row 7 :col 19 :name "changeme" :content "change me"
               :write t :highlighting +underscore+)
   (make-field :row 7 :col 40) ; Field "stop" character

   (make-field :row 8 :col 0 :content "Press")
   (make-field :row 8 :col 6 :intense t :content "enter")
   (make-field :row 8 :col 12 :content "to submit your name.")

   (make-field :row 10 :col 0 :intense t
               :color +red+ :name "errormsg") ; A blank field for error messages.

   (make-field :row 22 :Col 0 :content "PF3 Exit")
   ))


(defparameter example2-screen1-rules
  (make-rules
   (field-rules "fname" :validator #'non-blank-validator)
   (field-rules "lname" :validator #'non-blank-validator)
   (field-rules "password" :validator #'non-blank-validator :reset t)
   
   (field-rules "changeme"
                :validator #'non-blank-validator
                :must-change t
                :error-text "You can't leave \"change me\" as the value in the Change me field")))

(defparameter example2-screen2
  (make-screen
   "Example 2 Screen 2"
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
   (make-field :row 8 :col 6 :intense t :content "enter")
   (make-field :row 8 :col 12 :content "to enter your name again or")
   (make-field :row 8 :col 41 :intense t :content "PF3")
   (make-field :row 8 :col 45 :content "to quit and disconnect.")

   (make-field :row 11 :Col 0 :color +turquoise+ :highlighting +reverse-video+
               :content "Here is a field with extended attributes.")
   (make-field :row 11 :col 42) ; Remember to "stop" fields with a
                                ; regular field to clear the reverse
                                ; video for example.

   (make-field :row 20 :col 0 :intense t :color +red+ :name "errormsg")
   (make-field :row 22 :col 0 :content "PF3 Exit")
   ))


#| Possible alternative
(defun cl3270-example1 ()
  (usocket:socket-server "127.0.0.1" 3270
                         #'cl3270-handle
                         ()
                         :element-type '(unsigned-byte 8)
                         :multi-threading t
                         ))
|#


(defun cl3270-example2 (&key (handler 'cl3270-handle-2) (host "127.0.0.1") (debug nil))
  (prog1
      (usocket:with-socket-listener (conn host 3270
                                          :element-type '(unsigned-byte 8)
                                          )
        (format t "CL3270: example2: server listening on host ~S, port 3270...~%"
                host)
        (usocket:with-connected-socket (c (usocket:socket-accept conn))
          (let ((*do-debug* debug))
            (funcall handler c))))
    (format t "CL3270: example2: server closed~%")))



(defun cl3270-handle-2 (c &aux (field-values (make-hash-table :test #'equalp)))
  ;; C is a USOCKET:SOCKET

  (declare (type usocket:stream-usocket c)
           (type dict field-values))

  (unwind-protect
      (multiple-value-bind (devinfo err)
          (negotiate-telnet c)

        (when err
          (format *error-output*
                  ";;; CL3270: C3270 HANDLE: error: TR returned ~S~%" err)
          (return-from cl3270-handle-2 nil))

        (dbgmsg "telnet negotiated.~2%")

        (loop named mainloop do

                (multiple-value-bind (resp err)
                    (handle-screen example2-screen1
                                   example2-screen1-rules
                                   field-values
                                   (list +aid-enter+)
                                   (list +aid-pf3+)
                                   "errormsg"
                                   4 20
                                   c
                                   (codepage devinfo))

                  (when err
                    (format *error-output*
                            ";;; C3270: C3270 HANDLE: error handling SCREEN1: ~S.~%" 
                            err)
                    (return-from cl3270-handle-2 nil))

                  (when (= (response-aid resp) +aid-pf3+)
                    ;; Exit.
                    (loop-finish))

                  (setf field-values (response-vals resp))
                  )

                ;; Now we are ready to display screen2

                (let ((password-length
                       (length (string-trim " "
                                            (gethash "password" field-values))))
                      )
                
                  (setf (gethash "passwordOutput" field-values)
                        (format nil
                                "Your password was ~D character~:P long."
                                password-length))

                  (multiple-value-bind (resp err)
                      (handle-screen example2-screen2
                                     nil
                                     field-values
                                     (list +aid-enter+)
                                     (list +aid-pf3+)
                                     "errormsg"
                                     0 0
                                     c
                                     (codepage devinfo))

                    (when err
                      (format *error-output*
                              ";;; C3270: C3270 HANDLE: error handling SCREEN1: ~S.~%" 
                              err)
                      (return-from cl3270-handle-2 nil))

                    (when (= (response-aid resp) +aid-pf3+)
                      (loop-finish))

                    ;; If anything got pressed just continue the loop.
                    ;; :continue
                    ))
                ))
    (dbgmsg "closing connection.~%")
    (usocket:socket-close c))
  )


;;;; end of file -- example2.lisp
