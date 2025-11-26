;;;; -*- Mode: Lisp -*-

;;;; telnet.lisp
;;;;
;;;; Minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.

;;;; Notes:
;;;;
;;;; Version with USOCKET networking.

(in-package "CL3270")

;;; Constants.

;;; Should use my own DEFENUM.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (progn
    (deftype telnet-code ()
      '(mod 256))

    (defvar *telnet-code-names* ; Maybe make it into a... A-List!!!
      (make-array 256
                  :element-type 'symbol
                  :initial-element nil))

    (defmacro def-telnet-code (name code &optional (doc "Not documented."))
      `(progn
         (declaim (type telnet-code ,name))
         (setf (aref *telnet-code-names* ,code) ',name)
         (defconstant ,name ,code ,doc))
      ))
  )

(defun telnet-code-name (code)
  (or (aref *telnet-code-names* code)
      code))


;;; Start simple with Matthew R. Wilson's setup.

(def-telnet-code +binary+ 0 "Binary.")
(def-telnet-code +send+ 1 "Send.")

(def-telnet-code +terminal-type+ 24 "Terminal type.")

(def-telnet-code +eor-option+ 25 "EOR option.")

(def-telnet-code +eor+ 239 "EOR.")

;;; Telnet "commands", always "prefixed" by the "Interpret As Command"
;;; (IAC) sequence.
;;; Cfr., RFC 854

(def-telnet-code +se+ 240
                 "End of subnegotiation parameters.")

(def-telnet-code +nop+ 241
                 "No operation.")

(def-telnet-code +dm+ 242
                 "Data Mark.

The data stream portion of a Synch.
This should always be accompanied
by a TCP Urgent notification.")

(def-telnet-code +break+ 243
                 "Break.

NVT character BRK.")

(def-telnet-code +ip+ 244
                 "Interrupt Process.

The function IP.")

(def-telnet-code +ao+ 245
                 "Abort Output.

The function AO.")

(def-telnet-code +ayt+ 246
                 "Are You There.

The function AYT.")

(def-telnet-code +ec+ 247
                 "Erase Character.

The function EC.")

(def-telnet-code +el+ 248
                 "Erase Line.

The function EL.")

(def-telnet-code +ga+ 249
                 "Go Ahead.

The function GA.")

(def-telnet-code +sb+ 250
                 "Subnegotiation next.

Indicates that what follows is
subnegotiation of the indicated 
option.")

(def-telnet-code +will+ 251
                 "WILL (option code).

Indicates the desire to begin
performing, or confirmation that
you are now performing, the
indicated option.")

(def-telnet-code +wont+ 252
                 "WON'T (option code).

Indicates the refusal to perform,
or continue performing, the
indicated option.")

(def-telnet-code +do+ 253
                 "DO (option code).

Indicates the request that the
other party perform, or
confirmation that you are expecting
the other party to perform, the
indicated option.")

(def-telnet-code +dont+ 254
                 "DON'T (option code).

Indicates the demand that the
other party stop performing,
or confirmation that you are no
longer expecting the other party
to perform, the indicated option.")

(def-telnet-code +IAC+ 255
                 "Data Byte 255.")


(defun negotiate-telnet (c &aux (ss (usocket:socket-stream c)))
  "Negotiate TN3270 or telnet connection options.

NEGOTIATE-TELNET will naively (e.g., not checking client responses)
negotiate the options necessary for TN3270 or similar on a new telnet
connection, C."

  ;; (declare (type usocket:stream-server-usocket c))
  (write-sequence (vector +iac+ +do+ +terminal-type+) ss)
  (write-sequence (vector +iac+ +sb+ +terminal-type+ +send+ +iac+ +se+) ss)
  (write-sequence (vector +iac+ +do+ +eor-option+) ss)
  (write-sequence (vector +iac+ +do+ +binary+) ss)
  (write-sequence (vector +iac+ +will+ +eor-option+ +iac+ +will+ +binary+) ss)
  (flush-connection c 5)
  nil
  )


(defun unnegotiate-telnet (c timeout &aux (ss (usocket:socket-stream c)))
  "Un-negotiate a TN3270 or telnet connection options.

UNNEGOTIATE-TELNET will naively (e.g. not checking client responses)
attempt to restore the telnet options state to what it was before
NEGOTIATE-TELNET was called."

  ;; (declare (type usocket:stream-server-usocket c))
  (write-sequence (vector +iac+ +wont+ +eor-option+ +iac+ +wont+ +binary+) ss)
  (write-sequence (vector +iac+ +dont+ +binary+) ss)
  (write-sequence (vector +iac+ +dont+ +eor-option+) ss)
  (write-sequence (vector +iac+ +dont+ +terminal-type+) ss)
  (flush-connection c timeout)
  nil
  )


(defun flush-connection (c timeout
                           &aux
                           ;; (ss (usocket:socket-stream c))
                           (buffer (make-buffer :capacity 1024))
                           )
  "Flush the connection C (with TIMEOUT).

FLUSH-CONNECTION discards all bytes that it can read from conn,
allowing up to the duration TIMEOUT for the first byte to be read."
  
  (loop
   (multiple-value-bind (ready time-remaining)
       (usocket:wait-for-input c :timeout timeout :ready-only t)
     (dbgmsg ">>> Conn ready ~S ~S ~S ~S~%" c ready timeout time-remaining)

     (let ((ready-conn (first ready)))
       (cond ((and ready time-remaining) ; No timeout, no error.
              (usocket:with-mapped-conditions (ready-conn)
                (unwind-protect
                     (let ((n-read
			    (read-sequence
			     buffer
			     (usocket:socket-stream ready-conn)))) ; There is only C.
                       (dbgmsg ">>> ~D bytes read while flushing connection ~S ~S.~%"
                              n-read
                              ready-conn
                              timeout
                              ))
                  (when (= timeout time-remaining)
                    (setf timeout (/ time-remaining 2.0)))
                  (dbgmsg ">>> Timeout now ~S~%" timeout)
                  ))
              )
           
             (time-remaining ; And error occurred.
              (dbgmsg ">>> Error while flushing.~%")
              (return-from flush-connection t)
              )

             (ready ; Timeout, but somehow we may read.
              (usocket:with-mapped-conditions (ready-conn)
                (unwind-protect
                     (let ((n-read
			    (read-sequence
			     buffer
			     (usocket:socket-stream ready-conn)))) ; There is only C.
                      (dbgmsg
                              ">>> ~D bytes read while flushing connection.~%"
                              n-read))
                  (return-from flush-connection nil)
                  ))
              )

             (t ; Error and timeout
              (return-from flush-connection t))
             )))
   ))


;;; telnet-read

(defun telnet-read (c pass-eor &aux (ss (usocket:socket-stream c)))
  "Return the next byte of data from the connection C.

While reading, TELNET-READ filters out all telnet commands. If PASS-EOR
is true, then TELNET-READ will return upon encountering the telnet 'End
of Record' command, setting isEor to true. When isEor is true, the
value of b is meaningless and must be ignored (valid will be false).
When valid is true, the value in byte b is a real value read from the
connection; when value is false, do not use the value in b. (For
example, a valid byte AND error can be returned in the same call.)

Notes:

This doc string needs fixing."

  (let ((state :normal))
    (declare (type (member :normal :command :subneg) state))
    
    (dbgmsg ">>> Telnet read~%")

    (handler-case
        (loop for b of-type unsigned-byte = (read-byte ss)

              do (dbgmsg ">>> TR: read (~A) ~2,'0x ~S~%" state b b)

              ;; I always got a new byte (I hope).
              do (case state
                   (:normal
                    (cond ((= b +iac+)
                           (setf state :command)
                           (dbgmsg ">>> Entering telnet command state ~2,'0X +IAC+~%" b))
                          (t
                           (return-from telnet-read
                             (values b t nil nil)))
                          ))
                   (:command
                    (cond ((= b #xFF)
                           (dbgmsg ">>> Leaving telnet command state; escaped #xFF ~A~%"
                                   (telnet-code-name b))
                           (return-from telnet-read
                             (values b t nil nil)))

                          ((= b +sb+)
                           (setf state :subneg)
                           (dbgmsg ">>> Entering telnet command ~
                                        subnegotiation state +SB+~%"))
                          
                          ((and pass-eor (= b +eor+))
                           (dbgmsg ">>> Leaving telnet command ~
                                        state; returning EOR~%")
                           (return-from telnet-read
                             (values 0 nil t nil)))

                          (t
                           (setf state :normal)
                           (dbgmsg ">>> Leaving telnet command ~
                                        state; command was ~2,'0x ~A~%"
                                   b  (telnet-code-name b)
                                   ))
                          ))

                   (:subneg
                    (cond ((= b +se+)
                           (setf state :normal)
                           (dbgmsg ">>> Leaving telnet command ~
                                        subnegotiation state~%"))
                          (t
                           ;; Remain in subnegotiation consuming bytes
                           ;; until we get +se+.
                           (dbgmsg ">>> Consumed telnet subnegotiation ~
                                    byte: ~2,'0X ~A~%"
                                   b (telnet-code-name b)
                                   ))))
                   )
                 ) ; LOOP
      (end-of-file (eofc)
        (return-from telnet-read
          (values 0 nil nil eofc)))
      (error (e)
        (return-from telnet-read
          (values 0 nil nil e)))
      )))


;;;; end of file -- telnet.lisp
