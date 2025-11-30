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

;;; Constants
;;; ---------
;;;
;;; That is, "telnet codes".
;;;
;;; Notes:
;;;
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

;;; Extra codes used here.

(defconstant +binary-option+ +binary+)

(defconstant +terminal-type-is+ 0)

(defconstant +terminal-type-send+ +send+)


;;; Error Conditions
;;; ----------------

(define-condition no-3270-error (error)
  ()
  (:report "CL3270: could not negotiate telnet options for tn3270."))


(define-condition telnet-error (error)
  ()
  (:report "CL3270: telnet or 3270 protocol error."))


(define-condition unknown-terminal-error (error)
  ()
  (:report "CL3270: unknown terminal type."))


(define-condition option-rejected-error (error)
  ()
  (:report "CL3270: option-rejected."))


;;; Functions
;;; ---------

#+old-simple-minded-version
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


;;; negotiate-telnet
;;;
;;; New version tracking Matthew Wilson's GO one.

(defun negotiate-telnet (c &aux (ss (usocket:socket-stream c)))
  "Negotiate TN3270 or telnet connection options.

NEGOTIATE-TELNET will check client responses and negotiate the options
necessary for TN3270 or similar on a new telnet connection, C."

  ;; Sometimes the client will trigger us to send our "will" assertions
  ;; sooner than we otherwise would. Keep track here so we know not to send
  ;; them again.

  (let ((sent-will-bin nil)
        (sent-will-eor nil)
        (devtype nil)
        )
    (declare (type boolean sent-will-bin sent-will-eor))

    ;; Enable terminal type option.

    (write-sequence (vector +iac+ +do+ +terminal-type+) ss)

    (handler-case
        (setf (values sent-will-bin sent-will-eor)
              (check-option-response c +terminal-type+ +do+))

      (option-rejected-error ()
        (format *error-output* "CL3270: Error: option rejected.~%")
        (error 'no-3270-error))

      (telnet-error ()
        (format *error-output* "CL3270: Error: telnet error.~%")
        (error 'no-3270-error)))

    ;; Switch to the first available terminal type.

    (write-sequence
     (vector +iac+ +sb+ +terminal-type+ +terminal-type-send+ +iac+ +se+)
     ss)

    (handler-case
        (setq devtype (get-terminal-type c))

      (telnet-error ()
        (format *error-output* "CL3270: Error: telnet error.~%")
        (error 'no-3270-error)))

    ;; Request end of record mode

    (write-sequence (vector +iac+ +do+ +eor-option+) ss)

    (handler-case
        (setf (values sent-will-bin sent-will-eor)
              (check-option-response c +eor-option+ +do+))

      (option-rejected-error ()
        (format *error-output* "CL3270: Error: option rejected.~%")
        (error 'no-3270-error))

      (telnet-error ()
        (format *error-output* "CL3270: Error: telnet error.~%")
        (error 'no-3270-error)))

    ;; Request binary mode

    (write-sequence (vector +iac+ +do+ +binary+) ss)

    (handler-case
        (setf (values sent-will-bin sent-will-eor)
              (check-option-response c +binary+ +do+))

      (option-rejected-error ()
        (format *error-output* "CL3270: Error: option rejected.~%")
        (error 'no-3270-error))

      (telnet-error ()
        (format *error-output* "CL3270: Error: telnet error.~%")
        (error 'no-3270-error)))
    
    ;; It's possible there are already some client requests in
    ;; the queue that we haven't processed yet. We'll need to
    ;; consume any outstanding requests here and respond if
    ;; necessary.

    ;; loop here...

    (loop named drain-queue-requests
          with buf = (make-array 3
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)
          do
            (multiple-value-bind (rs tr)
                (usocket:wait-for-input ss
                                        :read-only t
                                        :timeout (/ 1 100.0))

              ;; I do not really use TR; the LOOP could
              ;; probably be prettified.

              (if rs
                  ;; rs is ready
                  (let ((n (read-sequence buf rs :end 3)))
                    (if (= n 3)
                        (cond ((and (= (aref buf 0) +iac+)
                                    (= (aref buf 1) +do+)
                                    (= (aref buf 2) +eor-option+))
                               (write-sequence (vector +iac+
                                                       +will+
                                                       +eor-option+)
                                               ss)
                               (setq sent-will-eor t))

                              ((and (= (aref buf 0) +iac+)
                                    (= (aref buf 1) +do+)
                                    (= (aref buf 2) +binary+))
                               (write-sequence (vector +iac+
                                                       +will+
                                                       +binary+)
                                               ss)
                               (setq sent-will-bin t)))
                        (format *error-output*
                                "CL3270: SHORT READ SHORT READ SHORT READ~%")
                        ))

                  ;; RS is NIL.
                  ;; Either we timed out, or the waiting was
                  ;; interrupted. C.f., usocket:wait-for-input
                  ;; documentation.

                  (loop-finish))))
                    
      ;; Enter end of record mode.

      (unless sent-will-eor
        (write-sequence (vector +iac+ +will+ +eor-option+) ss)

        (handler-case
            (setf (values sent-will-eor sent-will-bin)
                  (check-option-response c +eor-option+ +will+))
          (option-rejected-error ()
            (format *error-output* "CL3270: Error: option rejected.~%")
            (error 'no-3270-error))

          (telnet-error ()
            (format *error-output* "CL3270: Error: telnet error.~%")
            (error 'no-3270-error))))

      ;; Enter binary mode.

      (unless sent-will-bin
        (write-sequence (vector +iac+ +will+ +binary-option+) ss)
        
        (handler-case
            (setf (values sent-will-eor sent-will-bin)
                  (check-option-response c +binary-option+ +will+))

          (option-rejected-error ()
            (format *error-output* "CL3270: Error: option rejected.~%")
            (error 'no-3270-error))

          (telnet-error ()
            (format *error-output* "CL3270: Error: telnet error.~%")
            (error 'no-3270-error))))

      (make-device-info c devtype)
  
      #| Old
      ;; (declare (type usocket:stream-server-usocket c))
      (write-sequence (vector +iac+ +do+ +terminal-type+) ss)
      (write-sequence (vector +iac+ +sb+ +terminal-type+ +send+ +iac+ +se+) ss)
      (write-sequence (vector +iac+ +do+ +eor-option+) ss)
      (write-sequence (vector +iac+ +do+ +binary+) ss)
      (write-sequence (vector +iac+ +will+ +eor-option+ +iac+ +will+ +binary+) ss)
      (flush-connection c 5)
      nil
      |#

      ))


;;; unnegotiate-telnet

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
           
             (time-remaining ; An error occurred.
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


;; check-option-response
;;
;; Check for the client's "will/wont" (if mode is do) or "do/dont" (if
;; mode is will) response. mode is the option command the server just
;; sent, and option is the option code to check for.
;;
;; If we end up getting a client request instead, we'll response and
;; set sentEor or sentBin before trying to read the response again.
;;
;; Notes:
;;
;; Since GO has harebrained error handling (read: let's go back to
;; early, early C) the code for 'checkOptionResponse' in Matthew
;; Wilsons' code must follow that scheme: 'checkOptionResponse' either
;; returns 'nil' or returns an error thingy, while, possibly,
;; modifying SENT-EOR or SENT-BIN (used as "out" parameters).
;;
;; Now. 'checkOptionResponse' is called only in 'negotiate-telnet' and
;; its effect is always: if it "raises" 'ErrTelnetError' or
;; 'errOptionRejected' then 'negotiateTelnet' will "catch" them and
;; raise a 'ErrNo3270', otherwise it either "re-raises" or succeeds
;; (returning 'nil'), possibly modifying the 'sent*' parameters as a
;; side effect.
;;
;; The logic of my implementation is to cram this behavior in
;; CHECK-OPTION-RESPONSE.  If a stray error gets raised, it will not
;; be caught here.  As it should be.

(defun check-option-response (c option mode sent-eor sent-bin)
  "Check for the client's response.

Arguments and Values:

C : The connection, a USOCKET:USOCKET"

  (let ((buf (make-array 3
                         :element-type 'octect
                         :initial-element 0))
        (expected-yes 0)
        (expected-no 0)
        (ss (usocket:socket-stream c))
        )
    (declare ;; (type (vector octect 3) buf)
             (type (vector (unsigned-byte 8) 3) buf)
             (dynamic-extent buf))

    (case mode
      (+do+ (setf expected-yes +will+ expected-no +wont+))

      (+will+ (setf expected-yes +do+ expected-no +dont+))

      (otherwise (error 'telnet-error)))

    (handler-case
        (let ((n (read-sequence buf (usocket:socket-stream c))))
          (when (or (< n 3) (/= (aref buf 0) +iac+))
            (error 'telnet-error)))

      (telnet-error (te) ; Re-signal.
        (error te))

      (error (e)
        (format *error-output*
                "CL3270: got error ~S while checking response.~%")
        (error e)))

    ;; If the client is requesting to negotiate a mode with us before
    ;; the response to our request, we'll satisfy it if it's one of
    ;; the expected modes and then try to read the client's response
    ;; again.
    ;;
    ;; We only want to do this if we're not already expecting a "do"
    ;; response for the particular option.

    (unless (and (= expected-yes +do+) (= (aref buf 2) option))
      (cond ((and (= (aref buf 0) +iac+)
                  (= (aref buf 1) +do+)
                  (= (aref buf 2) +eor-option+))
             (write-sequence (vector +iac+ +will+ +eor-option+) ss)
             (setf sent-eor t)
             (return-from check-option-response
               (check-option-response c option mode sent-eor sent-bin)))

            ((and (= (aref buf 0) +iac+)
                  (= (aref buf 1) +do+)
                  (= (aref buf 2) +binary+))
             (write-sequence (vector +iac+ +will+ +binary+) ss)
             (setf sent-bin t)
             (return-from check-option-response
               (check-option-response c option mode sent-eor sent-bin)))
            ))
                         
    (when (= (aref buf 1) expected-no)
      (when (/= (aref buf 2) option)
        (error 'telnet-error))
      (error 'option-rejected-error))

    (when (/= (aref buf 1) expected-yes)
      (error 'telnet-error))

    ;; We have "will" now. But for the right option?

    (when (/= (aref buf 0) option)
      (error 'telnet-error))

    ;; All good, client accepted the option we requested.

    (values sent-eor sent-bin)))


;;; get-terminal-type

(defun get-terminal-type (c)
  "Read the response to a \"send terminal type\" option subfield command."

  (let* ((buf (make-array 100
                          :element-type '(unsigned-byte 8)
                          :initial-element 0))
         (term-type "")
         (n (read-sequence buf (usocket:socket-stream c)))
         )

    ;; At a minimum, with a one-character terminal type name, we
    ;; expect 7 bytes.

    (when (< n 7)
      (error 'telnet-error))

    ;; We'll check the expected control bytes all in one go...
    (when (or (/= (aref buf 0) +iac+)
              (/= (aref buf 1) +sb+)
              (/= (aref buf 2) +terminal-type+)
              ;; (/= (aref buf 3) +terminal-type-is+)
              (/= (aref buf 3) 0) ; +terminal-type-is+
              (/= (aref buf (- n 2)) +iac+)
              (/= (aref buf (- n 1)) +se+))
      (error 'telnet-error))

    ;; Everything looks good. The terminal type is an ASCII string
    ;; between all the control/command bytes.
    
    (octets-to-string (subseq buf 4 (- n 2)))))
              

;;; model-device-info

(defun model-device-info (c term-type &aux (ss (usocket:stream-usocket c)))
  "Return the terminal model (as a DEVICE-INFO)."

  (declare (type usocket:usocket c)
           (type string term-type))

  (flet ((check-term-type (term-type)
           ;; No regexp!
           (and (string-equal "IBM-" term-type :end2 4)
                (every #'digit-char-p (subseq term-type 4 8))
                (char= (aref term-type 8) #\-)
                (member (aref term-type 9) '(#\2 #\3 #\4 #\5))
                t)))

    ;; tn3270e restricts to a small list of valid models, but since
    ;; we're not doing tn3270e, we are seeing a variety of model
    ;; numbers. We'll generically handle anything claiming to be a -2,
    ;; -3, -4, or -5 type.
    ;;
    ;; We'll default to known terminal sizes in case we don't get the
    ;; structured field query response later.

    (let ((rows 24)
          (cols 80)
          (cpid 0)
          (is-x3270 nil)
          (aid (make-array 1
                           :element-type '(unsigned-byte 8)
                           :intial-element 0))
          (n 0)
          (cpfunc (constantly nil)) ; NIL is a handled as a codepage.
          (ok t) ; General useful boolean
          )
      (declare (type (integer 24 132) rows cols) ; Ok, quirky.
               (type boolean is-x3270 ok))

      (cond ((check-term-type term-type)
             (case (aref term-type 9)
               (#\2 (setq rows 24 cols 80)) ; Pleonastic
               (#\3 (setq rows 32 cols 80))
               (#\4 (setq rows 43 cols 80))
               (#\5 (setq rows 27 cols 132))))
            ((string-not-equal term-type "IBM-DYNAMIC")

             (setq rows 24
                   cols 80
                   term-type (concatenate 'string
                                          "unknown ("
                                          term-type
                                          ")")))
            ) ; cond
      
      ;; Now we'll discover the terminal size and character set.

      ;; First, we perform an ERASE / WRITE ALTERNATE to clear the
      ;; screen and put it in alternate screen mode. (EWA, reset WCC,
      ;; telnet EOR)

      (write-sequence (vector #x7e #xc3 #xff #xef) ss)

      ;; Now we need to send the Write Structured Field command (0xf3)
      ;; with the "Read Partition - Query" structured field. Note that
      ;; we're telnet-escaping the 0xff in the data, but the subfield
      ;; length is the *unescaped* length, including the 2 length
      ;; bytes but excluding the telnet EOR (5).

      (write-sequence (vector #xf3 0 5 #x01 0xff 0xff 0x02 0xff 0xef) ss)
      
      (multiple-value-bind (rs tr)
          (usocket:wait-for-input ss :timeout 3)
        (if rs
            (setq n (read-sequence aid ss)) ; If it errors, it errors.

            
            ;; Else, got a timeout or an interruption.
            ;; In this case, we'll assume it's because the client
            ;; didn't reply to our query command. In that case, we'll
            ;; return whatever we're already assuming.

            (return-from model-device-info
              (make-device-info 24 80 term-type))))

      (when (or (/= n 1) (/= +aid-query-response+ (aref aid 0)))
        (error 'telnet-error))

      ;; There are an arbitrary number of query reply structured fields. We are
      ;; only interested in the "Usable Area" SFID=0x81 QCODE=0x81 field and
      ;; "Character Sets" QCODE=0x85 field so we'll just consume any others.
      ;; Consume all data until the EOR is received.
              
      (loop with l of-type fixnum = 0
            for buf = (telnet-read-n c 2)
            unless buf do (loop-finish) end
            do (setq l (+ (ash 8 (aref buf 0)) (aref buf 1)))
            do (setq buf (telnet-read c (- l 2)))
            unless buf do (error 'telnet-error) end

            ;; Note that because length isn't at the beginning,
            ;; offsets in buf are 2 less than in the 3270 data stream
            ;; documentation.

            do (cond ((and (= (aref buf 0) #x81) (= (aref buf 1) #x81))
                      ;; Usable area.
                      (setf (values rows cols) (get-usable-area buf)))

                     ((and (= (aref buf 0) #x81) (= (aref buf 1) #x85))
                      ;; Character sets (codepage)
                      (setf cpid (get-code-page buf)))
                     
                     ((and (= (aref buf 0) #x81) (= (aref buf 1) #xA1))
                      ;; RPQ Names. We use this to determine if the client
                      ;; is x3270 family.
                      (setf is-x3270 (get-rpq-names buf)))

                     (t
                      ;; Not a field we are interested in.
                      'continue)))

      (setf (values cpfunc ok) (codepage-to-function cpid))

      (cond (ok
             (setq codepage (funcall cpfunc))

             ;; But if x3270 family, assume that this is really the default
             ;; "bracket" codepage, which reports as 37, not true CP37.

             (when (and (= cpid 37) is-x3270)
               (setq codepage (codepage-bracket))))

            (t ; else
             (setq codepage nil)))
      (make-device-info rows cols term-type codepage)
      )))


;; get-usable-area
;;
;; Processes the "Query Reply (Usable Area)" response to return the
;; rows and columns count of the terminal. The byte slice passed in to
;; buf must begin with {0x81, 0x81}.

(defun get-usable-area (buf &aux (rows 0) (cols 0))
  "Get the 'usable area'.

Notes:

A valid Usable Area reply will always include at least 18 (20 with
length) bytes."

  (declare (type (mod 1024) rows cols)) ; Let's exaggerate.

  (when (or (< (length buf) 18)
            (/= (aref buf 0) #x81)
            (/= (aref buf 1) #x81))
    (error 'telnet-error))

  ;; big-endian two byte values
  (setq cols (+ (ash 8 (aref buf 4)) (aref buf 5))
        rows (+ (ash 8 (aref buf 6)) (aref buf 7)))

  (when (or (zerop rows) (zerop cols))
    ;; Got a Usable Area response but the values are 0?
    (error 'unknown-terminal-error))

  ;; We support 12- and 14-bit addressing. Using 16-bit addressing
  ;; would require a mode change and the current API design doesn't
  ;; support tracking the state necessary for that.
  ;;
  ;; We'll limit the reported screen size to what fits in 14-bit
  ;; addressing by removing rows if necessary.

  (loop with bit14 of-type = (ash 1 14)
        while (>= (* rows cols) bit14) do (decf rows))

  (values rows cols))


;;; get-codepage-id
;;;
;;; Processes the "Query Reply (Character Sets)" response to
;;; return the integer code page number if present. If unable, returns 0. The
;;; byte slice passed in to buf must begin with {0x81, 0x85}.

(defun get-codepage-id (buf)
  "Get the codepage id."

  ;; Initial validity check.

  (when (or (< (length buf) 11)
            (/= (aref buf 0) #x81)
            (/= (aref buf 1) #x85))
    (return-from get-codepage-id 0))

  ;; If the GF bit is not set, no point in continuing.

  (when (/= (logand (aref buf 2) (ash 1 1)) (ash 1 1)) ; Quirky!
    (return-from get-codepage-id 0))

  ;; Ok loop through the descriptors...

  (let ((dl (aref buf 10)) ; Descriptor length.
        (pos 11) ; First descriptor.
        (buflen (length buf))
        )

    (declare (type fixnum dl pos buflen))

    (loop when (< buflen (+ pos dl))
            ;; No more descriptors.
            do (return-from get-codepage-id 0)

          if (zerop (aref buf pos))
            do (incf pos dl)
          else
            ;; This is the first descriptor we've seen with ID 0,
            ;; we'll use it.  No matter how long the descriptor is,
            ;; the code page will 2-byte big endian integer in the
            ;; last two bytes.
            do (return-from get-codepage-id
                 ;; The 'cpid'.
                 (+ (ash (aref buf (+ pos dl -2)) 8)
                    (aref (+ pos dl -1))))
            )))
            

;;; req-rpq-names
;;;
;;; Checks the "Query Reply (RPQ NAMES)" response to see if the client
;;; is in the x3270 family. The byte slice passed in to buf must begin
;;; with {0x81, 0xA1}.

(defun get-rpq-names (buf)

  (declare (type (vector unsigned-byte *) buf))

  (cond ((< (length buf) 16) nil)

        ;; "x3270" in EBCDIC
        ((and (= (aref buf 11) #xa7)
              (= (aref buf 12) #xf3)
              (= (aref buf 13) #xf2)
              (= (aref buf 14) #xaf7)
              (= (aref buf 15) #xa0))
         t)
  
        (t nil)))


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


;; telnetReadN
;;
;; Reads n unescaped, valid, non-EOR characters. The returned byte
;; slice will always be length n (see special case below, though), unless
;; error is non-nil, in which case the byte slice will be nil. Invalid or
;; early EOR will return ErrTelnetError.
;;
;; AS A SPECIAL CASE, if the first byte read is EOR, then the returned byte
;; slice AND error will be nil.

(defun telnet-read-n (c n)
  "Read n unescaped, valid, non-EOR characters."

  (loop with buf of-type (vector (unsigned-byte 8) *)
          = (make-array n
                        :element-type '(unsigned-byte 8)
                        :initial-element 0)
        for i from 0 below n
        do (multiple-value-bind (b valid is-eor err)
               (telnet-read c t)
             (cond (err (error err))
                   ((and (zerop i) is-eor)
                    ;; If we're still on the first byte and it's EOR,
                    ;; return a non-error nil value.
                    (return-from telnet-read-n nil))

                   ((or (not valid) is-eor)
                    (error 'telnet-error))

                   (t (setf (aref buf i) b))))

        finally (return-from telnet-read-n buf)))

;;;; end of file -- telnet.lisp
