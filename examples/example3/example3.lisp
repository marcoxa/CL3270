;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; example3.lisp
;;;; cl3270 example 3 from Matthew Wilson's GO code.
;;;;
;;;; See the file COPYING in the top folder for copyright and
;;;; licensing information.
;;;;
;;;; Notes:
;;;;
;;;; This example works only in Lispworks for the time being.
;;;; Eventually there will be a portable Multiprocessing and MAILBOX
;;;; implementation to fill in for GO channels (yes, I know about
;;;; Bordeaux Threads and ChanL).


(in-package "CL3270")

(defparameter example3-screen1
  (make-screen
   "Example 3 Screen 1"
   (make-field :row 0 :col 35 :intense t :content "3270 Clock")
   (make-field :row 1 :col 0 :color +white+
               :content "------------------------------------------------------------------------")

   (make-field :row 5 :col 5 :color +turquoise+
               :content "The current UTC time is:")
   (make-field :name "ticker"
               :row 5 :col 30 :color +yellow+ :intense t
               :content "XX:XX:XX")

   (make-field :row 22 :col 0 :content "PF3 Exit")
   ))


(defparameter *refresh-field*
  (elt (screen-fields example3-screen1) 3))


(defparameter *refresh-screen-field*
  (make-screen "Ticker Field Screen" *refresh-field*))


#| Possible alternative
(defun cl3270-example3 ()
  (usocket:socket-server "127.0.0.1" 3270
                         #'cl3270-handle
                         ()
                         :element-type '(unsigned-byte 8)
                         :multi-threading t
                         ))
|#


(defun cl3270-example3 (&key (handler 'cl3270-handle-3) (host "127.0.0.1") (debug t))
  (prog1
      (usocket:with-socket-listener (conn host 3270
                                          :element-type 'octet
                                          :reuse-address t
                                          )
        (format t ";;; CL3270: example 3: server listening on host ~S, port 3270...~%"
                host)
        (usocket:with-connected-socket (c (usocket:socket-accept conn))
          (let ((*do-debug* debug))
            (funcall handler c))))
    (format t ";;; CL3270: example 3: server closed~%")))



(defun cl3270-handle-3 (c)
  ;; C is a USOCKET:SOCKET

  (declare (type usocket:stream-usocket c))

  (unwind-protect
      (multiple-value-bind (devinfo err)
          (negotiate-telnet c)

        (when err
          (format *error-output*
                  ";;; CL3270: C3270 HANDLE 3: error: TR returned ~S~%" err)
          (return-from cl3270-handle-3 nil))

        (dbgmsg "telnet negotiated.~2%")

        (let* ((done-mb (mp:make-mailbox :name "done"))

               (ticker-mb (mp:make-mailbox :name "ticker"))

               (update-proc
                (mp:process-run-function
                 "update"
                 (list :mailbox done-mb)
                 #'(lambda ()
                     (loop do ; Just to have LOOP-FINISH
                          (setf (field-content *refresh-field*)
                                (now-time))
                          (multiple-value-bind (resp err)
                              (show-screen-opts example3-screen1
                                                nil
                                                c
                                                (make-screen-opts
                                                 :cursor-row 23
                                                 :cursor-col 0
                                                 :codepage (codepage devinfo)))
                            (when err
                              ;; User dropped connection, maybe? We'll
                              ;; end things.
                              (mp:mailbox-send done-mb t)
                              (loop-finish))

                            (when (= (response-aid resp) +aid-pf3+)
                              (mp:mailbox-send done-mb t)
                              (loop-finish)))))))

               (ticker-proc ; I could use a MP:TIMER.
                (mp:process-run-function
                 "ticker"
                 (list :mailbox ticker-mb)
                 #'(lambda ()
                     (loop (cl:sleep 1)
                           (mp:mailbox-send-limited ticker-mb 1 3600)
                           ;; (mp:mailbox-send ticker-mb 1)
                           ))))
               )

          (declare (ignorable update-proc))

          (unwind-protect
              (loop ; Polling as if 'select'.
                 with err = nil
                 when (mp:mailbox-not-empty-p done-mb) do

                     ;; Consume the signal (not strictly necessary).

                     (mp:mailbox-read done-mb)

                     (let ((*do-debug* t)) (dbgmsg "done by PF3 or other.~%"))
                     (loop-finish)
                 end
                 when (mp:mailbox-not-empty-p ticker-mb) do

                     ;; Consume the tick.

                     (mp:mailbox-read ticker-mb)

                     ;; Send the updated time, without clearing the
                     ;; screen.
                     
                     (let ((*do-debug* t)) (dbgmsg "tick~%"))

                     (setf (field-content *refresh-field*) (now-time)
                           err (nth-value
                                1
                                (show-screen-opts *refresh-screen-field*
                                                  nil
                                                  c
                                                  (make-screen-opts
                                                   :no-clear t
                                                   :no-response t
                                                   :codepage (codepage devinfo))
                                                  )))
                 end
                 when err do
                     (return-from cl3270-handle-3 nil))
            (let ((*do-debug* t))
              (dbgmsg "terminating ticker.~%"))
            (mp:process-terminate ticker-proc))
          ))
    (dbgmsg "closing connection.~%")
    (usocket:socket-close c))
  )


;;;; end of file -- example3.lisp
