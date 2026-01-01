;; -*- Mode: Lisp; cl3270-screen-mode: t -*-

(in-package "CL3270")

(defscreen congress-screen1
  "
                    39C3 CL3270 guestbook application

                   3333333   999999    CCCCCC  33333333
                        3   9      9  CCCCCCCC 33333333
                       3    9      9  CCC  CCC     333
                      3     9      9  CCC  CCC    333
                     3      9      9  CCC  CCC   333
                    3       9     99  CCC  CCC  333
                   333333    99999 9  CCC  CCC 3333333
                         3         9  CCC      33333333
                  3      3  9      9  CCC  CCC      333
                  3      3  9      9  CCC  CCC 333  333
                  3      3  9      9  CCC  CCC 333  333
                  3      3  9      9  CCCCCCCC 33333333
                   333333    999999    CCCCCC   333333


Your name or handle:                     
Your message.......:                                                            

Press enter to submit your guest book entry.
                                                                                
PF3 Exit
"
  :fields ((:from (0 20) :len 33
                  :intense t)
           (:from (17 21) :len 20
                  :name "name" :write t :highlighting +underscore+)
           (:from (18 21) :len 58
                  :name "message" :write t :highlighting +underscore+)
           (:from (20 6) :len 5
                  :intense t)
           (:from (21 0) :len 79
                  :color +red+ :name "errormsg")))

(defscreen screen2 "
                           3270 Example Application

Thank you for submitting your name. Here's what I know:

Your first name is                      and you entered this message:
                                                                               


Press enter to enter your name again or  PF3 to quit and disconnect.


Here is a field with extended attributes.










PF3 Exit
"
  :fields ((:from (0 27) :len 24 :intense t)
           (:from (4 19) :len 20 :highlighting +underscore+ :name "name")
           (:from (5 0) :len 79 :highlighting +underscore+ :name "message")
           (:from (8 6) :len 5 :intense t)
           (:from (11 0) :len 41 :color +turquoise+ :highlighting +reverse-video+)))

(defscreen goodbye-screen "
                           3270 Example Application

Thank you for using this application! Goodbye.




















"
  :fields ((:from (0 27) :len 24 :intense t)))

(defun cl3270-congress (&key (handler 'cl3270-handle) (host "127.0.0.1") (debug nil))
  (prog1
      (usocket:with-socket-listener (conn host 3270
                                          :element-type '(unsigned-byte 8)
                                          :reuse-address t)
        (format t ";;; CL3270: congress: server listening on host ~S, port 3270...~%"
                host)
        (usocket:with-connected-socket (c (usocket:socket-accept conn))
          (let ((*do-debug* debug))
            (funcall handler c))))
    (format t ";;; CL3270: congress: server closed.~%")))

(defun cl3270-handle (c
                      &aux
                      (field-values (make-hash-table :test #'equalp))
                      response)
  ;; C is a USOCKET:SOCKET

  (declare (type usocket:stream-usocket c))

  (handler-case
      (multiple-value-bind (devinfo err)
          (negotiate-telnet c)

        (when err
          (format *error-output* "CL3270 HANDLE: error: ~S~%" err)
          (return-from cl3270-handle nil))

        (dbgmsg "CL3270 HANDLE: telnet negotiated; devinfo ~S.~2%" devinfo)

        (setf (gethash "codepage" field-values)
              (if (codepage devinfo)
                  (princ-to-string (codepage-id (codepage devinfo)))
                  "(unknown)"))

        (loop named mainloop
              do
                (dbgmsg "C3270 HANDLE: mainloop.~%")
                (tagbody
                 start-screen1-loop
                 (dbgmsg "C3270 HANDLE: start-screen-1.~%")
                 (loop ; loop until the user passes input validation, or quits.

                    do ; Need LOOP-FINISH

                      ;; Show the first screen, and wait to get a client
                      ;; response. Place the cursor at the beginning of
                      ;; the first input field. We're passing in the
                      ;; fieldValues map to carry values over from the
                      ;; previous submission. We could pass nil,
                      ;; instead, if always want the fields to start out
                      ;; blank.

                      (multiple-value-bind (resp err)
                          (show-screen-opts congress-screen1
                                            field-values
                                            c
                                            (make-screen-opts
                                             :codepage (codepage devinfo)
                                             :cursor-row 17
                                             :cursor-col 21))

                        (when err
                          (format *error-output*
                                  "CL3270: error SHOW-SCREEN 1 error ~S~%" err)
                          ;; (return-from cl3270-handle err)
                          (error err))

                        (setf response resp)

                        ;; If the user pressed PF3, exit.

                        (dbgmsg "C3270 HANDLE: got aid ~S~%"
                                (aid-to-string (response-aid response)))

                        (when (= (response-aid response) +aid-pf3+)
                          (dbgmsg "C3270 HANDLE: got PF3, ready to show goodbye screen~%")

                          (show-screen-opts goodbye-screen
                                            nil
                                            c
                                            (make-screen-opts
                                             :codepage (codepage devinfo)
                                             :no-response t))

                          (dbgmsg "C3270 HANDLE: returning from mainloop in 2 secs.~%")
                          (force-output *trace-output*)
                          (sleep 2)
                          (return-from mainloop t))


                        (when (/= (response-aid response) +aid-enter+)
                          (go start-screen1-loop))

                        ;; User must have pressed "Enter", so let's
                        ;; check the input.

                        (setf field-values (response-vals response))
                        (let ((name (string-trim " " (gethash "name" field-values)))
                              (message (string-trim " " (gethash "message" field-values))))
                          (when (or (string= "" name)
                                    (string= "" message))
                            (setf (gethash "errormsg" field-values)
                                  "Name and message fields are required.")
                            (go start-screen1-loop))

                          ;; At this point, we know the user provided
                          ;; both fields and had hit enter, so we are
                          ;; going to reset the error message for the
                          ;; next time through the loop, and break out
                          ;; of this loop so we move on to screen 2.

                          (setf (gethash "errormsg" field-values) "")
                          (loop-finish))))

                 (dbgmsg "C3270 HANDLE: ready to display screen2~%")

                 ;; Now we're ready to display screen2
                 (multiple-value-bind (resp err)
                     (show-screen-opts screen2
                                       field-values
                                       c
                                       (make-screen-opts
                                        :codepage (codepage devinfo)))
                   (when err
                     (format *error-output*
                             "CL3270: error: SHOW-SCREEN 2 error ~S~%." err)
                     (return-from mainloop err))

                   (setf response resp)

                   ;; If the user pressed PF3, exit
                   (when (= (response-aid response) +aid-pf3+)
                     (show-screen-opts goodbye-screen
                                       nil
                                       c
                                       (make-screen-opts
                                        :codepage (codepage devinfo)
                                        :no-response t))
                     (dbgmsg "C3270 HANDLE: returning from mainloop in 2 secs.~%")
                     (sleep 2)
                     (return-from mainloop t))))))

    (error (e)
      ;; (usocket:socket-close c)
      (dbgmsg "CL3270: connection closed.~%")
      (error e))

    (:no-error (result)
      ;; (usocket:socket-close c)
      (dbgmsg "CL3270: closing connection and returning ~S.~%" result)
      result)
    ))

