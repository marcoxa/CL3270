;;;; -*- Mode: Lisp -*-

;;;; screen.lisp

(in-package "CL3270")

;;; color -- 3270 extended field attribute color values.
;;;
;;; Notes:
;;; Should use my own DEFENUM.

(deftype color ()
  '(unsigned-byte 8))

(defconstant +default-color+ 0)

(defconstant +Blue+         #xf1)
(defconstant +Red+          #xf2)
(defconstant +Pink+         #xf3)
(defconstant +Green+        #xf4)
(defconstant +Turquoise+    #xf5)
(defconstant +Yellow+       #xf6)
(defconstant +White+        #xf7)


;;; highlight -- 3270 extended field attribute highlighting method.

(deftype highlight ()
  '(unsigned-byte 8))


;;; The valid 3270 highlights.

(defconstant +Default-Highlight+ 0)
(defconstant +Blink+            #xf1)
(defconstant +ReverseVideo+     #xf2)
(defconstant +Underscore+       #xf4)


;;; field

(defstruct field
  (row 0 :type (integer 0 23))
  (col 0 :type (integer 0 79))
  (content "" :type string)
  (write nil :type boolean)
  (intense nil :type boolean)
  (hidden nil :type boolean)
  (color +default-color+ :type color)
  (highlighting +default-highlight+ :type highlight)
  (name nil :type (or null string))
  (keepspaces nil :type boolean)
  )

;;; screen

(defstruct (screen (:constructor make-screen (&rest fields)))
  (fields () :type list))


;;; fieldmap

(deftype fieldmap ()
  ;; STRING -> FIELD
  'hash-table)

(defun make-fieldmap ()
  (make-hash-table :test #'equal))


;;; show-screen

(defun show-screen (screen vals curs-row curs-col conn)
  "Write the 3270 datastream for the screen to a connection.

Fields that aren't valid (e.g. outside of the 24x80 screen) are silently
ignored. If a named field has an entry in the values map, the content of
the field from the values map is used INSTEAD OF the Field struct's Content
field. The values map may be nil if no overrides are needed. After writing
the fields, the cursor is set to crow, ccol, which are 0-based positions:
row 0-23 and col 0-79. Errors from conn.Write() are returned if
encountered.
"

  ;; CONN is a USOCKET:USOCKET.

  (dbgmsg ">>> Show screen on ~S (~S)~2%"
          conn
          (usocket:socket-stream conn))

  (let ((b (make-buffer))
        (fm (make-fieldmap))
        )
    (write-buffer b #xF5) ; Erase/Write to terminal.
    (write-buffer b #xC3) ; WCC = Reset, Unlock Keyboard, Reset MDT.

    ;; Build the commands for each field on the screen

    (loop for f in (screen-fields screen)
          for frow = (field-row f)
          for fcol = (field-col f)
          for fcontent = (field-content f)
          for fname = (field-name f)

          do (when (and (<= 0 frow 23) (<= 0 fcol 79))

               (write-buffer* b (sba frow fcol))
               (write-buffer* b (build-field f))

               (when (and fname (not (string= "" fname)))
                 (multiple-value-bind (val foundp)
                     (gethash fname vals)
                   (when foundp
                     (setf fcontent val))))

               (when (and fcontent (not (string= "" fcontent)))
                 (write-buffer* b (to-ebcdic (char-codes fcontent))))

               (when (field-write f)
                 (let ((bufaddr (+ (* frow 80) fcol)))
                   (setf (gethash (1+ bufaddr) fm) fname)))
          
               ))

    ;; Set the cursor position. Correct out-of-bouds values to 0.
    (unless (<= 0 curs-row 23)
      (setf curs-row 0))
    (unless (<= 0 curs-col 79)
      (setf curs-col 0))

    (write-buffer* b (ic curs-row curs-col))
    
    (write-buffer* b (vector #xFF #xEF)) ; Telnet IAC EOR

    ;; Now write the datastream to the writer, returning any potential
    ;; error.

    (dbgmsg ">>> Sending datastream: ~A~%" b)
    (dbgmsg ">>> Sending datastream: ~S~%"
            (reduce (lambda (x z) (concatenate 'string x z))
                    (map 'vector (lambda (x) (format nil "~2,'0X" x)) b)))
    (dbgmsg ">>> Sending datastream: ~S~%"
            (map 'string 'code-char (to-ascii b)))

    (handler-case
        (let ((ss (usocket:socket-stream conn)))
          (write-sequence b ss)
          (force-output ss)
          )
      (error (e)
        (declare (ignorable e))
        (format *error-output* ">>> Error writing datastream ~S~%" e)
        (return-from show-screen (values (make-response) e))))

    (dbgmsg ">>> Datastream sent~%")

    (multiple-value-bind (response err)
        (read-response conn fm)
      (when err
        (return-from show-screen (values response err)))

      ;; Strip leading+trailing spaces from field values.
    
      (dolist (f (screen-fields screen))
        (unless (field-keepspaces f)
          (multiple-value-bind (fv foundp)
              (gethash (field-name f)
                       (response-values response))
            (when foundp
              (setf (gethash (field-name f)
                             (response-values response))
                    (string-trim '(#\Space) fv))))))

      (values response nil)
      ))
  )


(defun sba (row col)
  "The \"set buffer address\" 3270 command."
  (let ((result (make-buffer :capacity 3)))
    (write-buffer result #x11) ; SBA
    (write-buffer* result (getpos row col))
    result))


(defun build-field (f &aux
                      (buf (make-buffer))
                      (paramcount 1))
  (when (and (= (field-color f) +default-color+)
             (= (field-highlighting f) +default-highlight+))
    ;; This is a traditional field, issue a normal sf command.
    (write-buffer buf #x1D) ; SF - "start field"
    (write-buffer buf
                  (sf-attribute (field-write f)
                                (field-intense f)
                                (field-hidden f)))
    (return-from build-field buf))

  ;; Otherwise, this needs an extended attribute field.
  (write-buffer buf #x29)

  (when (/= (field-color f) +default-color+)
    (incf paramcount))
  (when (/= (field-highlighting f) +default-highlight+)
    (incf paramcount))

  (write-buffer buf paramcount)

  ;; Write the basic field attribute.
  (write-buffer buf #xC0)
  (write-buffer buf
                (sf-attribute (field-write f)
                              (field-intense f)
                              (field-hidden f)))

  ;; Write the highlighting attribute.
  (when (/= (field-highlighting f) +default-highlight+)
    (write-buffer buf #x41)
    (write-buffer buf (field-highlighting f)))

  ;; Write the color attribute.
  (when (/= (field-color f) +default-color+)
    (write-buffer buf #x42)
    (write-buffer buf (field-color f)))

  buf
  )


(defun sf-attribute (write-a intense-a hidden-a &aux (attribute 0))
  "Build the attribute byte for the \"start field\" 3270 command."
  (if (not write-a)
      (setq attribute (logior attribute (ash 1 5)))
      ;; ... else, set the MDT bit; we always want writable field values
      ;; returned, even if unchanged.
      (setq attribute (logior attribute 1)))

  (when intense-a
    (setq attribute (logior attribute (ash 1 3))))

  (when hidden-a
    (setq attribute (logior attribute (ash 1 3) (ash 1 2))))

  ;; Fill in top 2 bits with appropriate values.

  (setq attribute (aref *codes* attribute))
  attribute)


(defun ic (row col
               &aux
               (result (make-buffer :capacity 4)))
  "This is the \"insert cursor\" 3270 command.

This function will include the appropriate SBA command."
  (write-buffer* result (sba row col))
  (write-buffer result #x13) ; IC
  result)


(defun getpos (row col
                   &aux
                   (result (make-buffer :capacity 2))
                   (address (+ (* row 80) col))
                   (hi (ash (logand address #xFC0) -6))
                   (lo (logand address #x3F)))
  "Translate ROW and COL to buffer address control characters."

  (write-buffer result (aref *codes* hi))
  (write-buffer result (aref *codes* lo))
  result)


;;;; end of file -- screen.lisp
