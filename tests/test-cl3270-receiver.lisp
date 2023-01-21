;;;; -*- Mode: Lisp -*-

;;;; test-cl3270-receiver.lisp
;;;; Simulates a 3270 "sink" which "responds" to the "examples".

(in-package "CL3270")

(defun start-telnet-3270-thread (&optional
                          (telnet-3270-function 'telnet-3270-function-1))
  (with-open-stream (socket-handle
                     (comm:open-tcp-stream "127.0.0.1" 3270
                                           :element-type '(unsigned-byte 8)))
    (let ((telnet-3270-process
           (mp:process-run-function (format nil "Telnet CL3270 ~D on port ~D"
                                            socket-handle
                                            3270)
                                    ()
                                    telnet-3270-function
                                    socket-handle))
          )
      
      (mp:process-join telnet-3270-process)
      )))


(defun start-telnet-3270-client (&optional
                                 (telnet-3270-function 'telnet-3270-function-1))
  (with-open-stream (socket-handle
                     (comm:open-tcp-stream "127.0.0.1" 3270
                                           :element-type '(unsigned-byte 8)))
    (funcall telnet-3270-function socket-handle)))


(defun start-telnet-3270-server (&optional
                                 (telnet-3270-server-fn 'telnet-3270-server))
  (comm:start-up-server :function telnet-3270-server-fn
                        :service 3270))


(defun telnet-3270-server (conn)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket conn
                               :direction :io
                               :element-type '(unsigned-byte 8)))
        )
    (mp:process-run-function (format nil "Telnet 3270 ~D on port 3270"
                                     conn)
                             ()
                             'telnet-3270-function-1
                             stream)))


;;; Real function(s).

(defun telnet-3270-function-1 (c)
  (unwind-protect
      (loop with next-b = 0
            for b = (read-byte c nil nil)
            while b
            do (format *trace-output* "<<< READ-BYTE ~S~%" (telnet-code-name b))
            if (= b #xFF)
              do (setf next-b (read-byte c))
              and do (format *trace-output* "<<< READ-BYTE ~S after #xFF~%" (telnet-code-name next-b))
              and if (= next-b #xEF)
                do (loop-finish)
              else
                do (format *trace-output* "<<< Need to do something else~%")
              end
            else
            do (format *trace-output* "<<< READ-BYTE E ~2,'0x A ~2,'0x C ~C~%"
                         b (ascii b) (code-char (ascii b)))
            end
            )
    (format *trace-output* ">>> Done; sending PF3.~%")
    (write-byte +aid-pf3+ c)
    (finish-output c)
    (sleep 5)))


(defun telnet-3270-function-loop (c &aux (lc (list c))) ; Avoid consing in LOOP.
  (format *trace-output* "<<< Starting telnet 3270 loop on ~S~2%" lc)
  (unwind-protect
      (loop for ready = (progn
                          (format *trace-output* "<<< Waiting on ~S~2%" lc)
                          (sys:wait-for-input-streams-returning-first
                           lc
                           :wait-reason "Waiting for 3720 server."
                           :timeout 5
                           ))
            for b = (and ready (read-byte ready nil nil)) ; (eq WAIT C)
            do (format *trace-output* "<<< WAIT ~S B ~S~%" ready b)
            unless (and b ready) do (loop-finish)
            ;; do (setf lc (list ready))
            ;; when b
            do (format *trace-output* "<<< READ-BYTE ~S~@
                                       <<< READ-BYTE E ~2,'0x A ~2,'0x C ~C~%"
                       (telnet-code-name b)
                       b (ascii b) (code-char (ascii b)))
            )
    (format *trace-output* ">>> Done; sending 4 4 PF3.~%")
    (write-byte 4 c)
    (write-byte 4 c)
    (write-byte +aid-pf3+ c)
    (finish-output c)
    ; (yes-or-no-p "The end?")
    ))



(defun simple-function-loop (c)
  (format *trace-output* "<<< Starting simple loop on ~S~2%" c)
  (unwind-protect
      (loop for ready = (progn
                          (format *trace-output* "<<< Waiting on ~S~2%" c)
                          (sys:wait-for-input-streams-returning-first
                           (list c)
                           :wait-reason "Waiting for server."
                           :timeout 1
                           ))
            for b = (and ready (read-byte ready nil nil)) ; (eq WAIT C)
            do (format *trace-output* "<<< WAIT ~S B ~S~%" ready b)
            unless (and b ready) do (loop-finish)
            when b
            do (format *trace-output* "<<< READ-BYTE ~2,'0x~%" b)
            ; b (ascii b) (code-char (ascii b))
            )
    (format *trace-output* ">>> Done; sending #\\a.~%")
    (write-byte 97 c) ; an #\a.
    (finish-output c)
    (yes-or-no-p "The end?")))


;;;; end of file -- test-cl3270-receiver.lisp
