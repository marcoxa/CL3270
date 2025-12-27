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

(declaim (ftype (function ((vector octet) usocket:stream-usocket) null)
                send-sequence)
         (inline send-sequence))
(defun send-sequence (seq ss)
  (declare (type (vector octet) seq)
           (type usocket:stream-usocket ss))

  (write-sequence seq ss)
  (force-output ss) ; Maybe it should be FINISH-OUTPUT.
  )


(declaim (ftype (function ((vector octet) usocket:stream-usocket) null)
                recv-sequence)
         (inline recv-sequence))
(defun recv-sequence (seq ss &rest keys &key (start 0) end &allow-other-keys)
  (declare (type (vector octet) seq)
           (type usocket:stream-usocket ss)
           (ignore start end))
  
  (apply #'read-sequence seq ss keys)
  ;; (apply #'usocket:socket-receive ss seq nil keys)
  )


(defun read-delimited-sequence (seq ss start-seq end-seq)
  "Read an octet sequence from strem SS.

The octets read are destructively stored in sequence SEQ and the bound
by (sub)sequences START-SEQ and END-SEQ.

The function returns the sequence SEQ and a number which is the length
of the sequence read (including the length of START-SEQ and END-SEQ).
If the function fails to read a bounded sequence of octets, the
function returns NIL and 0.

Exceptional Situations:

Other, IO related, errors may be raised."

  (declare (type (vector octet) seq)
           (type (or null (vector octet)) start-seq end-seq))

  (assert (array-has-fill-pointer-p seq))

  (let ((ss-len (length start-seq))
        (es-len (length end-seq))
        )
    (declare (type (mod 1024) ss-len es-len) ; Should be enough.
             (ignorable ss-len))
    (labels
        ((fail ()
           (return-from read-delimited-sequence
             (values nil 0)))

         (success ()
           (values seq (length seq))) ; Should keep a count.

         (start ()
           (loop for o across start-seq
                 for b of-type (or null octet)
                   = (read-byte ss nil nil)
                 when (null b)
                   ;; Fail.
                   do (fail)
                 end
                 if (= o b)
                   do (vector-push-extend b seq)
                 else
                   ;; Fail
                   do (fail)
                 end)
           (read-next))
           
         (read-next ()
           (let ((b (read-byte ss nil nil)))
             (declare (type (or null octet) b))

             (cond ((and (null b) (zerop es-len))
                    ;; We are (hopefully) done...
                    (success))

                   ((null b)
                    (fail))

                   ((= b (aref end-seq 0))
                    (vector-push-extend b seq)
                    (maybe-read-end 1))

                   (t
                    (vector-push-extend b seq)
                    (read-next)))
             ))

         (maybe-read-end (end-seq-i)
           (declare (type (mod 1024) end-seq-i)) ; Should be enough.

           (when (= end-seq-i es-len)
             ;; Finished.
             (success))

           (let ((b (read-byte ss nil nil)))
             (declare (type (or null octet) b))
               
             (cond ((= b (aref end-seq end-seq-i))
                    (vector-push-extend b seq)
                    (maybe-read-end (1+ end-seq-i)))

                   (t
                    (vector-push-extend b seq)
                    (read-next)))))
         ) ; labels
      (start))))


(defun recv-sequence-no-hang (seq c &key (start 0) end (timeout 1.0))
  (declare (type (vector octet) seq)
           (type usocket:stream-usocket c)
           (ignore start end))

  (assert (array-has-fill-pointer-p seq))

  (loop with b of-type (or null octet) = 0
        with ss = (usocket:socket-stream c)
        for br from 0
        while (usocket:wait-for-input c :timeout timeout :ready-only t)
        do (setf b (read-byte ss nil nil))
           (vector-push b seq)
        finally (return (values br seq))))



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
;;; New version tracking Matthew R. Wilson's GO one.
;;;
;;; Notes:
;;;
;;; The new version uses a better factored CHECK-OPTION-RESPONSE.  See
;;; the note before that function.

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

    (send-sequence (vector +iac+ +do+ +terminal-type+) ss)

    (setf (values sent-will-bin sent-will-eor)
          (check-option-response c +terminal-type+ +do+ sent-will-eor sent-will-bin))

    ;; Switch to the first available terminal type.

    (send-sequence
     (vector +iac+ +sb+ +terminal-type+ +terminal-type-send+ +iac+ +se+)
     ss)

    (handler-case
        (setq devtype (get-terminal-type c))

      (telnet-error ()
        (format *error-output* "CL3270: Error: telnet error.~%")
        (error 'no-3270-error)))

    ;; Request end of record mode

    (send-sequence (vector +iac+ +do+ +eor-option+) ss)

    (setf (values sent-will-bin sent-will-eor)
          (check-option-response c +eor-option+ +do+ sent-will-eor sent-will-bin))

    ;; Request binary mode

    (send-sequence (vector +iac+ +do+ +binary+) ss)

    (setf (values sent-will-bin sent-will-eor)
          (check-option-response c +binary+ +do+ sent-will-eor sent-will-bin))

    ;; It's possible there are already some client requests in
    ;; the queue that we haven't processed yet. We'll need to
    ;; consume any outstanding requests here and respond if
    ;; necessary.

    ;; loop here...

    (loop
       named drain-queue-requests
       with buf = (make-array 3
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)
       for rs = (usocket:wait-for-input c
                                        :ready-only t
                                        :timeout (/ 1 100.0))
       if rs
         ;; RS is ready (it is really SS), we have pending stuff.

         do (let ((n (read-sequence buf rs :end 3)))
              (if (= n 3)
                  (cond ((and (= (aref buf 0) +iac+)
                              (= (aref buf 1) +do+)
                              (= (aref buf 2) +eor-option+))
                         (send-sequence (vector +iac+
                                                 +will+
                                                 +eor-option+)
                                         ss)
                         (setq sent-will-eor t))

                        ((and (= (aref buf 0) +iac+)
                              (= (aref buf 1) +do+)
                              (= (aref buf 2) +binary+))
                         (send-sequence (vector +iac+
                                                 +will+
                                                 +binary+)
                                         ss)
                         (setq sent-will-bin t)))
                  (format *error-output*
                          "CL3270: SHORT READ SHORT READ SHORT READ~%")
                  ))
       else do

           ;; RS is NIL.
           ;; Either we timed out, or the waiting was
           ;; interrupted. C.f., usocket:wait-for-input
           ;; documentation.

           (loop-finish))
                    
    ;; Enter end of record mode.

    (unless sent-will-eor
      (send-sequence (vector +iac+ +will+ +eor-option+) ss)

      (setf (values sent-will-eor sent-will-bin)
            (check-option-response c +eor-option+ +will+ sent-will-eor sent-will-bin)))

    ;; Enter binary mode.

    (unless sent-will-bin
      (send-sequence (vector +iac+ +will+ +binary-option+) ss)
        
      (setf (values sent-will-eor sent-will-bin)
            (check-option-response c +binary-option+ +will+ sent-will-eor sent-will-bin)))

    (model-device-info c devtype)
  
    #| Old
    ;; (declare (type usocket:stream-server-usocket c))
    (send-sequence (vector +iac+ +do+ +terminal-type+) ss)
    (send-sequence (vector +iac+ +sb+ +terminal-type+ +send+ +iac+ +se+) ss)
    (send-sequence (vector +iac+ +do+ +eor-option+) ss)
    (send-sequence (vector +iac+ +do+ +binary+) ss)
    (send-sequence (vector +iac+ +will+ +eor-option+ +iac+ +will+ +binary+) ss)
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
     (dbgmsg "conn ready ~S ~S ~S ~S~%" c ready timeout time-remaining)

     (let ((ready-conn (first ready)))
       (cond ((and ready time-remaining) ; No timeout, no error.
              (usocket:with-mapped-conditions (ready-conn)
                (unwind-protect
                     (let ((n-read
                            #|
			    (read-sequence
			     buffer
			     (usocket:socket-stream ready-conn))
                            |#
                            (recv-sequence-no-hang
			     buffer
			     (usocket:socket-stream ready-conn))
                            )) ; There is only C.
                       (dbgmsg "~D bytes read while flushing connection ~S ~S.~%"
                              n-read
                              ready-conn
                              timeout
                              ))
                  (when (= timeout time-remaining)
                    (setf timeout (/ time-remaining 2.0)))
                  (dbgmsg "timeout now ~S~%" timeout)
                  ))
              )
           
             (time-remaining ; An error occurred.
              (dbgmsg "error while flushing.~%")
              (return-from flush-connection t)
              )

             (ready ; Timeout, but somehow we may read.
              (usocket:with-mapped-conditions (ready-conn)
                (unwind-protect
                     (let ((n-read
                            #|
			    (read-sequence
			     buffer
			     (usocket:socket-stream ready-conn))
                            |#
                            (recv-sequence-no-hang
			     buffer
			     (usocket:socket-stream ready-conn)))) ; There is only C.
                      (dbgmsg "~D bytes read while flushing connection.~%"
                              n-read))
                  (return-from flush-connection nil)
                  ))
              )

             (t ; Error and timeout
              (return-from flush-connection t))
             )))
   ))


;;; check-option-response
;;;
;;; Check for the client's "will/wont" (if mode is do) or "do/dont" (if
;;; mode is will) response. mode is the option command the server just
;;; sent, and option is the option code to check for.
;;;
;;; If we end up getting a client request instead, we'll response and
;;; set sentEor or sentBin before trying to read the response again.
;;;
;;; Notes:
;;;
;;; Since GO has harebrained error handling (read: let's go back to
;;; early, early C) the code for 'checkOptionResponse' in Matthew R.
;;; Wilsons' code must follow that scheme: 'checkOptionResponse' either
;;; returns 'nil' or returns an error thingy, while, possibly,
;;; modifying SENT-EOR or SENT-BIN (used as "out" parameters).
;;;
;;; Now. 'checkOptionResponse' is called only in 'negotiate-telnet' and
;;; its effect is always: if it "raises" 'ErrTelnetError' or
;;; 'errOptionRejected' then 'negotiateTelnet' will "catch" them and
;;; raise a 'ErrNo3270', otherwise it either "re-raises" or succeeds
;;; (returning 'nil'), possibly modifying the 'sent*' parameters as a
;;; side effect.
;;;
;;; The logic of my implementation is to cram this behavior in
;;; CHECK-OPTION-RESPONSE.  If a stray error gets raised, it will not
;;; be caught here.  As it should be.

(defun check-option-response (c option mode sent-eor sent-bin)
  "Check for the client's response.

Arguments and Values:

C : the connection, a USOCKET:USOCKET
OPTION : a OCTET
MODEL  : a OCTET
SENT-EOR : a BOOLEAN
SENT-BIN : a BOOLEAN
"

  (declare (type usocket:stream-usocket c)
           (type octet option mode)
           (type boolean sent-eor sent-bin))

  (handler-case
      (let ((buf (make-array 3
                             :element-type 'octet
                             :initial-element 0))
            (expected-yes 0)
            (expected-no 0)
            (ss (usocket:socket-stream c))
            )
        (declare ;; (type (vector octet 3) buf)
                 (type (vector (unsigned-byte 8) 3) buf)
                 (dynamic-extent buf)
                 (type octet expected-yes expected-no))

        (dbgmsg "CHECK-OPTION-RESPONSE: mode ~D, +will ~D, +do+ ~D~%"
                mode +will+ +do+)

        (cond ((= mode +do+) (setf expected-yes +will+ expected-no +wont+))
          
              ((= mode +will+) (setf expected-yes +do+ expected-no +dont+))
          
              (t (error 'telnet-error)))

        (dbgmsg "CHECK-OPTION-RESPONSE: reading...~%")
        (let ((n (read-sequence buf ss)))
          (dbgmsg "CHECK-OPTION-RESPONSE: read ~D bytes from conn...~%" n)
          (dbgmsg "CHECK-OPTION-RESPONSE: (aref buf 0) = ~2,'0X (~:*~D)~%" (aref buf 0))
          (when (or (< n 3) (/= (aref buf 0) +iac+))
            (format *error-output* "CL3270: error...~%")
            (error 'telnet-error)))

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

                 (send-sequence (vector +iac+ +will+ +eor-option+) ss)

                 (setf sent-eor t)

                 (return-from check-option-response
                   (check-option-response c option mode sent-eor sent-bin)))

                ((and (= (aref buf 0) +iac+)
                      (= (aref buf 1) +do+)
                      (= (aref buf 2) +binary+))

                 (send-sequence (vector +iac+ +will+ +binary+) ss)

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

        (when (/= (aref buf 2) option)
          (error 'telnet-error))

        ;; All good, client accepted the option we requested.
        ;; Just return SENT-EOR and SENT-BIN

        (values sent-eor sent-bin))

    ;; Hanling errors.

    (option-rejected-error ()
      (format *error-output*
              "CL3270: Error: option rejected.~%")
      (error 'no-3270-error))

    (telnet-error ()
      (format *error-output*
              "CL3270: Error: telnet error.~%")
      (error 'no-3270-error))

    (error (e)
      (format *error-output*
              "CL3270: got error ~S while checking response.~%")
      (error e))))


;;; get-terminal-type

(defun get-terminal-type (c)
  "Read the response to a \"send terminal type\" option subfield command."

  (declare (type usocket:stream-usocket c))

  (let* ((buf (make-buffer :capacity 100))
         ;; (term-type "")
         (n (recv-sequence-no-hang buf c))
         )

    (declare (type (vector (unsigned-byte 8) 100) buf)
             (dynamic-extent buf) ; We SUBSEQ it, therefore...
             (type fixnum n))

    ;; At a minimum, with a one-character terminal type name, we
    ;; expect 7 bytes.

    (dbgmsg "GET-TERMINAL-TYPE: got ~D bytes from connection...~%" n)

    (when (< n 7)
      (error 'telnet-error))

    ;; We'll check the expected control bytes all in one go...
    (when (or (/= (aref buf 0) +iac+)
              (/= (aref buf 1) +sb+)
              (/= (aref buf 2) +terminal-type+)
              (/= (aref buf 3) +terminal-type-is+)
              (/= (aref buf (- n 2)) +iac+)
              (/= (aref buf (- n 1)) +se+))
      (error 'telnet-error))

    ;; Everything looks good. The terminal type is an ASCII string
    ;; between all the control/command bytes.
    
    (octets-to-string (subseq buf 4 (- n 2)))))
              

;;; model-device-info

(defun model-device-info (c term-type &aux (ss (usocket:socket-stream c)))
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
          (cpid 3270) ; I know this is the "bracket" codpage.
          (is-x3270 nil)
          (aid (make-array 1
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
          (n 0)
          ;; (cpfunc (constantly nil)) ; NIL is a handled as a codepage.
          (codepage nil)
          (ok t) ; General useful boolean
          )
      (declare (type (integer 12 132) rows cols) ; Ok, quirky.
               (type codepage-id cpid)
               (type (mod 2048) n) ; Just for the heck of it; I know, I know.
               (type boolean is-x3270 ok)
               (type (vector octet 1) aid)
               ;; (type function cpfunc)
               (type (or null codepage) codepage)
               )

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

      (send-sequence (vector #x7e #xc3 #xff #xef) ss)

      ;; Now we need to send the Write Structured Field command (0xf3)
      ;; with the "Read Partition - Query" structured field. Note that
      ;; we're telnet-escaping the 0xff in the data, but the subfield
      ;; length is the *unescaped* length, including the 2 length
      ;; bytes but excluding the telnet EOR (5).

      (send-sequence (vector #xf3 0 5 #x01 #xff #xff #x02 #xff #xef) ss)
      
      (let ((rs (usocket:wait-for-input c :timeout 3)))
        (if rs
            (setq n (read-sequence aid ss)) ; If it errors, it errors.

            
            ;; Else, got a timeout or an interruption.
            ;; In this case, we'll assume it's because the client
            ;; didn't reply to our query command. In that case, we'll
            ;; return whatever we're already assuming.

            (return-from model-device-info
              (make-device-info :rows 24 :cols 80 :term-type term-type))))

      (when (or (/= n 1) (/= +aid-query-response+ (aref aid 0)))
        (error 'telnet-error))

      ;; There are an arbitrary number of query reply structured fields. We are
      ;; only interested in the "Usable Area" SFID=#x81 QCODE=#x81 field and
      ;; "Character Sets" QCODE=#x85 field so we'll just consume any others.
      ;; Consume all data until the EOR is received.
              
      (loop with l of-type fixnum = 0
            for buf = (telnet-read-n c 2)
            unless buf do (loop-finish) end
            do
              (dbgmsg "TRN: buf 1 ~S~%" buf)
              (setq l (+ (ash (aref buf 0) 8) (aref buf 1))
                    buf (telnet-read-n c (- l 2)))
            unless buf do (error 'telnet-error) end

            ;; Note that because length isn't at the beginning,
            ;; offsets in buf are 2 less than in the 3270 data stream
            ;; documentation.

            do
              (dbgmsg "TRN: buf 2 ~S~%" buf)
              (cond ((and (= (aref buf 0) #x81) (= (aref buf 1) #x81))
                      ;; Usable area.
                      (setf (values rows cols) (get-usable-area buf)))

                     ((and (= (aref buf 0) #x81) (= (aref buf 1) #x85))
                      ;; Character sets (codepage)
                      (setf cpid (get-codepage-id buf)))
                     
                     ((and (= (aref buf 0) #x81) (= (aref buf 1) #xA1))
                      ;; RPQ Names. We use this to determine if the client
                      ;; is x3270 family.
                      (setf is-x3270 (get-rpq-names buf)))

                     (t
                      ;; Not a field we are interested in.
                      'continue)))

      (setf (values codepage ok)
            (get-codepage cpid nil)
            ;; (codepage-to-function cpid)
            )

      (cond (ok
             ;; (setq codepage (funcall cpfunc)) ; No need for this.

             ;; But if x3270 family, assume that this is really the default
             ;; "bracket" codepage, which reports as 37, not true CP37.

             (when (and (= cpid 37) is-x3270)
               (setq codepage *codepage-bracket*)))

            (t ; else
             (setq codepage nil)))

      (make-device-info :rows rows
                        :cols cols
                        :term-type term-type
                        :codepage codepage)
      )))


;;; get-usable-area
;;;
;;; Processes the "Query Reply (Usable Area)" response to return the
;;; rows and columns count of the terminal. The byte slice passed in to
;;; buf must begin with {#x81, #x81}.

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

  (loop with bit14 of-type fixnum = (ash 1 14)
        while (>= (* rows cols) bit14) do (decf rows))

  (values rows cols))


;;; get-codepage-id
;;;
;;; Processes the "Query Reply (Character Sets)" response to
;;; return the integer code page number if present. If unable, returns 0. The
;;; byte slice passed in to buf must begin with {#x81, #x85}.

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
                    (aref buf (+ pos dl -1))))
            )))
            

;;; req-rpq-names
;;;
;;; Checks the "Query Reply (RPQ NAMES)" response to see if the client
;;; is in the x3270 family. The byte slice passed in to buf must begin
;;; with {#x81, #xA1}.

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
    
    (dbgmsg "TR: telnet read~%")

    (handler-case
        (loop for b of-type unsigned-byte = (read-byte ss)

              do (dbgmsg "TR: read (~A) ~2,'0x ~S~%" state b b)

              ;; I always got a new byte (I hope).
              do (case state
                   (:normal
                    (cond ((= b +iac+)
                           (setf state :command)
                           (dbgmsg "entering telnet command state ~2,'0X +IAC+~%" b))
                          (t
                           (return-from telnet-read
                             (values b t nil nil)))
                          ))
                   (:command
                    (cond ((= b #xFF)
                           (dbgmsg "leaving telnet command state; escaped #xFF ~A~%"
                                   (telnet-code-name b))
                           (return-from telnet-read
                             (values b t nil nil)))

                          ((= b +sb+)
                           (setf state :subneg)
                           (dbgmsg "entering telnet command subnegotiation state +SB+~%"))
                          
                          ((and pass-eor (= b +eor+))
                           (dbgmsg "leaving telnet command state; returning EOR~%")
                           (return-from telnet-read
                             (values 0 nil t nil)))

                          (t
                           (setf state :normal)
                           (dbgmsg "leaving telnet command state; command was ~2,'0x ~A~%"
                                   b  (telnet-code-name b)
                                   ))
                          ))

                   (:subneg
                    (cond ((= b +se+)
                           (setf state :normal)
                           (dbgmsg "leaving telnet command subnegotiation state~%"))
                          (t
                           ;; Remain in subnegotiation consuming bytes
                           ;; until we get +se+.
                           (dbgmsg "Consumed telnet subnegotiation byte: ~2,'0X ~A~%"
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


;;; telnet-read-n
;;;
;;; Reads n unescaped, valid, non-EOR characters. The returned byte
;;; slice will always be length n (see special case below, though), unless
;;; error is non-nil, in which case the byte slice will be nil. Invalid or
;;; early EOR will return ErrTelnetError.
;;;
;;; AS A SPECIAL CASE, if the first byte read is EOR, then the returned byte
;;; slice AND error will be nil.

(defun telnet-read-n (c n)
  "Read n unescaped, valid, non-EOR characters."

  (loop with buf of-type (vector octet *)
          = (make-array n
                        :element-type 'octet
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
