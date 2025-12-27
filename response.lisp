;;;; -*- Mode: Lisp -*-

;;;; response.lisp
;;;;
;;;; Handling responses from a 3270.
;;;;
;;;; See the file COPYING for copyright and licensing information.


(in-package "CL3270")

#|
;; The GO library uses 'int'.  By using the more precise type,
;; sometime I get an error from the 3270 (wx3270) sending a position
;; that encodes a bad position.  This also happens with the GO
;; library.  Not being precise gives some leeway.  I guess this
;; happens because the actual implementation of the 3720 protocol is a
;; bit (a lot) shaky.

(deftype row-index ()
  '(integer 0 24) ; 24 hardcoded for simple 3720
  )


(deftype col-index ()
  '(integer 0 80) ; 80 hardcoded for simple 3720
  )
|#


(deftype row-index ()
  "The type of row indices."
  '(mod 1024) ; Should be more than sufficient.
  )


(deftype col-index ()
  "The type of column indices."
  '(mod 1024) ; Should be more than sufficient.
  )


;;; response

(defstruct response
  "The Response Structure."

  (aid  +aid-none+ :type aid)
  (row  0 :type row-index)
  (col  0 :type col-index)
  (vals (make-dict :test #'equalp) :type dict) ; STRING -> STRING.
  )


(defmethod print-object ((r response) stream)
  (print-unreadable-object (r stream :identity t)
    (format stream "Response 3720: ~A ~D ~D ~S"
            (response-aid r)
            (response-row r)
            (response-col r)
            (response-vals r))))


;;; read-aid

(defun read-aid (c)
  "Read an octet from a (usocket) stream C."

  (declare (type usocket:usocket c))

  ;; C is a "connection", i.e., a stream tied to "telnet".
  
  ;; (dbgmsg "READ-AID: read aid~%")

  (loop (multiple-value-bind (b valid _ err)
            (telnet-read c nil)
          (declare (ignore _))
          
          (dbgmsg "READ-AID: got #x~2,'0X (~S) valid ~S err ~S~%"
                  b
                  (aid-to-string b)
                  valid
                  err)

          (when (and (not valid) err)
            (return-from read-aid (values +aid-none+ err)))

          (when (or (is-aid-none b)
                    (is-enter-key b)
                    (is-attention-key b)
                    (is-pf-key b))
            ;; debug
            (dbgmsg "READ-AID: got AID byte: ~2,'0X (~S)~%"
                    b
                    (aid-to-string b))
            (return-from read-aid (values b nil)))

          ;; debug
          (dbgmsg "got non-AID byte: ~2,'0X~%" b)
          )))


;;; field-map

(deftype field-map ()
  "The Field Map Type.

A synonym for HASH-TABLE."
  'hash-table)


;;; read-reaponse

(defun read-response (c field-map dev cp)
  
  ;; C is a "connection", i.e., a stream tied to "telnet"; a USOCKET:USOCKET.
  ;; FIELD-MAP is a ... hash-table.

  (declare (type usocket:stream-usocket c)
           (type field-map field-map)
           (type (or null device-info) dev)
           (type codepage cp))

  (let ((r (make-response))
        (cols 80)
        )
    (declare (type response r)
             (type col-index cols))

    ;; Read the AID key.

    (multiple-value-bind (aid err)
        (read-aid c)
      (when err
        (format *error-output* "CL3270: error: READ-AID error ~S~%" err)
        (return-from read-response (values r err)))

      (dbgmsg "READ-RESPONSE: #x~2,'0X ~S~%" aid (aid-to-string aid))

      (setf (response-aid r) aid)

      ;; If the user pressed clear, or a PA key we should return now
      ;; TODO: actually, we should consume the 0xffef, but that will
      ;; currently get taken care of in our next AID search.

      (when (or (is-clear-key aid)
                (is-key aid +aid-pa1+)
                (is-key aid +aid-pa2+)
                (is-key aid +aid-pa3+)
                ;; (is-attention-key aid)
                )
        (return-from read-response (values r nil)))
      )

    (when dev
      (setf cols (nth-value 1 (alt-dimensions dev))))

    ;; Read the row and col (i.e., the position).
    (multiple-value-bind (row col _ err)
        (read-position c cols)
      (declare (ignore _))

      (when err
        (format *error-output* "CL3270: error: READ-POSITION error ~S~%" err)
        (return-from read-response (values r err)))

      (setf (response-col r) col
            (response-row r) row)
      )

    ;; Read the field values.

    (multiple-value-bind (field-values err)
        (read-fields c field-map cols cp)
      (when err
        (return-from read-response (values r err)))
      
      (setf (response-vals r) field-values))
    
    (dbgmsg "READ-RESPONSE: return ~S~%" r)
    (values r nil)
    ))


(defun read-position (c cols
                      &aux
                      (raw (make-array 2 :element-type 'octet))
                      )

  (declare (type usocket:stream-usocket c)
           (type col-index cols)
           (type (vector octet 2) raw))

  ;; (dbgmsg "READ-POSITION: cols ~D~%" cols)
  (dotimes (i 2)
    (multiple-value-bind (b _ __ err)
        (telnet-read c nil)
      (declare (ignore _ __))

      (when err
        (return-from read-position (values 0 0 0 err)))
      #|
      (dbgmsg "READ-POSITION: ~D ~2,'0X ~S~%"
              i
              b
              b)
      |#
      (setf (aref raw i) b)))

  (let* ((addr (decode-buf-addr raw))
         (col (mod addr cols))
         (row (/ (- addr col) cols))
         )

    (dbgmsg "READ-POSITION: got position bytes ~2,'0X ~:*~D ~2,'0X ~:*~D, decoded to ~2,'0X, row ~d col ~d~%"
            (aref raw 0)
            (aref raw 1)
            addr
            row
            col)
    (values row col addr nil)
    ))


;;; read-fields

(defun read-fields (c fm cols cp)

  (declare (type usocket:stream-usocket c)
           (type field-map fm)
           (type col-index cols)
           (type (or null codepage) cp))

  (unless cp
    (setq cp *default-codepage*))

  (let ((infield nil)
        (fieldval (make-buffer))
        (fieldpos 0)
        (vals (make-dict :test #'equalp))
        )
    (declare (type boolean infield)
             (type buffer fieldval)
             (type fixnum fieldpos)
             (type hash-table vals))

    ;; Consume bytes until we get #xFFEF

    ;; (terpri *trace-output*)
    (dbgmsg "READ-FIELDS: read the fields until #xFFEF~%")

    (loop (multiple-value-bind (b _ eor err)
              (telnet-read c t)
            (declare (ignore _))

            ;; (dbgmsg "READ-FIELDS: got ~S (#x~:*~2,'0X) eor ~S err ~S from TR~%" b eor err)
            (when err
              (dbgmsg "READ-FIELDS: got error ~S~%" err)
              (return-from read-fields (values nil err)))

            (cond (eor ; Check for end of data stream (#xFFEF)
                   ;; Finish current field.
                   ;; (dbgmsg "READ-FIELDS: handling EOR infield ~S~%" infield)
                   (when infield
                     (dbgmsg "READ-FIELDS: EOR infield ~S ~S~%" cp fieldval)
                     (let ((val (decode-ebcdic cp fieldval)))
                       (dbgmsg "READ-FIELDS: field ~D: ~S~%" fieldpos val)
                       (handle-field fieldpos val fm vals)))
                   (dbgmsg "READ-FIELDS: EOR returning ~S~%" vals)
                   (return-from read-fields (values vals nil)))

                  ((= b #x11) ; No? Check for start-of-field.
                   ;; Finish previous field if necessary.
                   (when infield
                     ;; (dbgmsg "READ-FIELDS: #x11 infield ~S ~S~%" cp fieldval)
                     (let ((val (decode-ebcdic cp fieldval)))
                       ;; (dbgmsg "READ-FIELDS: field ~D: ~S~%" fieldpos val)
                       (handle-field fieldpos val fm vals)))

                   ;; Start a new field.
                   (setf infield t
                         fieldval (make-buffer)
                         fieldpos 0)
                   
                   (multiple-value-bind (_ __ fps err)
                       (read-position c cols)
                     (declare (ignore _ __))
                     
                     (when err
                       (return-from read-fields (values nil err)))
                     
                     (setf fieldpos fps))

                   ;; Next iteration.
                   )

                  ((not infield) ; Consume all other bytes as field contents
                           ; if we're in a field.
                   ;; (dbgmsg "READ-FIELDS: got unexpected byte while processing fields: ~2,'0x~%" b)
                   ;; Next iteration.
                   )
                  (t
                   (write-buffer fieldval b))
                  ))) ; loop
    ))


;;; handle-field

(defun handle-field (addr val fm vals)
  (declare (type fixnum addr)
           (type string val)
           (type field-map fm)
           (type dict vals)
           )

  (multiple-value-bind (name ok)
      (gethash addr fm)
    ;; Ok.  Looks like it is a HASH-TABLE

    (unless ok
      (return-from handle-field nil))

    (setf (gethash name vals) val)
    t))


;;; decode-buf-addr

(defun decode-buf-addr (raw)
  "Decode a RAW 2-byte encoded buffer address.

Returns the integer value of the address."

  (declare (type (vector octet 2) raw))

  (let ((d-raw-0 (aref raw 0))
        (d-raw-1 (aref raw 1))
        )
    (declare (type octet d-raw-0 d-raw-1))

    (if (zerop (logand d-raw-0 #xC0))

        ;; 16 bits addressing.

        (+ (ash d-raw-0 8) d-raw-1)
        
        ;; 12 bits addressing.

        (+ (ash (logand d-raw-0 #x3F) 6) (logand d-raw-1 #x3F))
        )))

;;;; end of file -- response.lisp
