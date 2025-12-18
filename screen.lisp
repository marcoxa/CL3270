;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; screen.lisp
;;;;
;;;; Screen handling (mapping) for minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.
;;;;
;;;; Notes:
;;;;
;;;; Most comments are taken directly from Matthew Wilson's.


(in-package "CL3270")

;;; color -- 3270 extended field attribute color values.
;;;
;;; Notes:
;;;
;;; Should use my own DEFENUM.

(deftype color ()
  'octet)

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

(defconstant +default-highlight+ 0)
(defconstant +blink+             #xf1)
(defconstant +reverse-video+     #xf2)
(defconstant +underscore+        #xf4)


;;; field

(defstruct field
  (row 0 :type row-index)
  (col 0 :type col-index)
  (content ""   :type string)
  (write nil    :type boolean)
  (autoskip nil :type boolean)
  (intense nil  :type boolean)
  (hidden nil   :type boolean)
  (numeric-only nil :type boolean)
  (color +default-color+ :type color)
  (highlighting +default-highlight+ :type highlight)
  (name nil :type (or null string))
  (keepspaces nil :type boolean)
  )

;;; screen

(defstruct (screen (:constructor make-screen (&rest fields)))
  (fields () :type list))


;;; screen-opts
;;;


(defstruct screen-opts
  "The Screen-opts Struct.

SCREEN-OPTS are the options that callers may set when sending a screen
to the 3270 client."
  
  (altscreen nil :type (or null device-info))

  (codepage *default-codepage* :type codepage)

  (no-response nil :type boolean)

  (no-clear nil :type boolean)

  (cursor-row 0 :type row-index)

  (cursor-col 0 :type col-index)

  (post-send-callback nil :type (or null function))

  (callback-data nil)
  )


;;; fieldmap

(deftype fieldmap ()
  ;; STRING -> FIELD
  'hash-table)

(defun make-fieldmap ()
  (make-hash-table :test #'equal))



;;; show-screen-opts
;;;
;;; ShowScreenOpts writes the 3270 datastream for the screen, with the
;;; provided ScreenOpts, to a connection.
;;;
;;; Fields that aren't valid (e.g. outside of the screen size) are
;;; silently ignored. If a named field has an entry in the values map,
;;; the content of the field from the values map is used INSTEAD OF
;;; the Field struct's Content field. The values map may be nil if no
;;; overrides are needed.
;;;
;;; If opts.NoClear is false, the client screen will be cleared before
;;; writing the new screen, and the cursor will be repositioned to the
;;; values in opts.CursorRow and opts.CursorCol. If opts.NoClear is
;;; true, the screen will NOT be cleared, the cursor will NOT be
;;; repositioned, and the new screen will be overlayed over the
;;; current state of the client screen.
;;;
;;; If opts.NoResponse is false, ShowScreenOpts will block before
;;; returning, waiting for data from the client and returning the
;;; Response. If opts.NoResponse is true, ShowScreenOpts will
;;; immediately return after sending the datastream and the Response
;;; will be empty.
;;;
;;; If using from multiple threads -- one to block and wait for a
;;; response, and another to send screens with NoResponse and/or
;;; NoClear, be aware that if you change the input fields on screen
;;; after the initial blocking call is made, the response fields will
;;; not line up correctly and end up being invalid. That is to say,
;;; while waiting for a response, don't perform other actions from
;;; another thread that could layout the user input fields
;;; differently.

(defun show-screen-opts (screen vals conn opts &aux (resp (make-response)))
  (declare (type screen screen)
           (type dict vals)
           (type usocket:stream-usocket conn)
           (type screen-opts opts))

  (multiple-value-bind (fm err)
      (show-screen-internal screen
                            vals
                            (screen-opts-cursor-row opts)
                            (screen-opts-cursor-col opts)
                            conn
                            (not (screen-opts-no-clear opts))
                            (screen-opts-altscreen opts)
                            (screen-opts-codepage opts))
    (when err
      (return-from show-screen-opts (values resp err)))

    (when (screen-opts-post-send-callback opts)
      (when (funcall (screen-opts-post-send-callback opts)
                     (screen-opts-callback-data opts))
        (return-from show-screen-opts (values resp err))))

    (unless (screen-opts-no-response opts)
      (multiple-value-bind (resp err)
          (read-response conn
                         fm
                         (screen-opts-altscreen opts)
                         (screen-opts-codepage opts))
        (when err
          (return-from show-screen-opts (values resp err)))

        ;; Strip spaces from field values unless the caller requested
        ;; that we maintain whitespace.

        (dolist (fld (screen-fields screen))
          (unless (field-keepspaces fld)
            (let ((found
                   (nth-value 1
                              (gethash (field-name fld)
                                       (response-vals resp)))))
              (when found
                (setf (gethash (field-name fld) (response-vals resp))
                      (string-trim " "
                                   (gethash (field-name fld)
                                            (response-vals resp))))))))
        ))
    (values resp nil)))


;;; show-screen
;;; Deprecated: use ShowScreenOpts with default/empty ScreenOpts.
;;;
;;; Notes:
;;;
;;; This deprecated function is NOT codepage-aware. The global code
;;; page set by SET-CODEPAGE will always be used.

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
  (show-screen-opts screen
                    vals
                    conn
                    (make-screen-opts :cursor-row curs-row
                                      :cursor-col curs-col)))


(defun show-screen-no-response (screen vals crow ccol conn)
  (nth-value 1
             (show-screen-opts screen vals conn
                               (make-screen-opts :no-response t
                                                 :cursor-row crow
                                                 :cursor-col ccol))))


(defun show-screen-internal (screen vals crow ccol conn clear dev cp)
  (declare (type screen screen)
           (type dict vals)
           (type row-index crow)
           (type col-index ccol)
           (type usocket:usocket conn)
           (type boolean clear)
           (type (or null device-info) dev)
           (type (or null codepage) cp)
           )

  (unless cp
    (setq cp *default-codepage*))

  (let ((rows 24)
        (cols 80)
        (b (make-buffer))
        (fm (make-fieldmap))
        )

    (when dev
      (setf (values rows cols) (alt-dimensions dev)))

    (if clear
        (if (not (and (= rows 24) (= cols 80)))
            (write-buffer b #x7E)
            (write-buffer b #xF5))
        (write-buffer b #xF1))


    (if clear
        (write-buffer b #xC3)
        (write-buffer b #xC2))

    (dolist (fld (screen-fields screen))
      (let ((frow (field-row fld))
            (fcol (field-col fld))
            )

        (unless (or (minusp frow) (>= frow rows)
                    (minusp fcol) (>= fcol cols))
          
          (write-buffer* b (sba frow fcol cols))
          (write-buffer* b (build-field fld)) ; Double check this!

          (let ((content (field-content fld)))
            (when (string/= (field-name fld) "")
              (multiple-value-bind (v found)
                  (gethash (field-name fld) vals)
                (when found
                  (setf content v))))

            (when (string/= content "")
              (write-buffer* b (encode-characters cp content))
              )

            (when (field-write fld)
              (let ((bufaddr (+ (* frow cols) fcol)))
                (setf (gethash (1+ bufaddr) fm) (field-name fld))))
            )
          ))) ; dolist

    (when clear
      (when (or (minusp crow) (>= crow rows)) (setq crow 0))
      (when (or (minusp ccol) (>= ccol cols)) (setq ccol 0))
      (write-buffer* b (ic crow ccol cols)))


    (write-buffer b +iac+)
    (write-buffer b +eor+)

    (let ((*print-base* 2))
      (dbgmsg "sending datastream ~S~%" b))

    (handler-case
        (write-sequence b (usocket:socket-stream conn))
      (error (e)
        (return-from show-screen-internal (values nil e))))


    (values fm nil)
    ) ; let
  ) ; show-screen-internal


(defun sba (row col cols)
  "The \"set buffer address\" 3270 command."

  (declare (type octet row cols cols))

  (let ((result (make-buffer :capacity 3)))
    (write-buffer result #x11) ; SBA
    (write-buffer* result (getpos row col cols))
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
                                (field-hidden f)
                                (field-autoskip f)
                                (field-numeric-only f)
                                ))
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
                              (field-hidden f)
                              (field-autoskip f)
                              (field-numeric-only f)
                              ))

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


(defun sf-attribute (write-a intense-a hidden-a skip-a numeric-a
                             &aux (attribute 0))
  "Build the attribute byte for the \"start field\" 3270 command."
  (if (not write-a)
      (progn
        (setq attribute (logior attribute (ash 1 5)))
        (when skip-a
          (setq attribute (logior attribute (ash 1 4)))))

      ;; ... else, set the MDT bit; we always want writable field values
      ;; returned, even if unchanged.
      (progn
        (setq attribute (logior attribute 1))
        (when numeric-a
          (setq attribute (logior attribute (ash 1 4))))))

  (when intense-a
    (setq attribute (logior attribute (ash 1 3))))

  (when hidden-a
    (setq attribute (logior attribute (ash 1 3) (ash 1 2))))

  ;; Fill in top 2 bits with appropriate values.

  (setq attribute (aref *codes* attribute))
  attribute)


(defun ic (row col cols
               &aux
               (result (make-buffer :capacity 4)))
  "This is the \"insert cursor\" 3270 command.

This function will include the appropriate SBA command."
  (write-buffer* result (sba row col cols))
  (write-buffer result #x13) ; IC
  result)


(defun getpos (row col cols
                   &aux
                   (result (make-buffer :capacity 2))
                   (address (+ (* row cols) col))
                   (hi (ash (logand address #xFC0) -6))
                   (lo (logand address #x3F))
                   )
  "Translate ROW and COL to buffer address control characters."

  (when (< address (ash 1 12))
    ;; Use 12-bit addressing if the buffer address fits in 12 bits.
    
    (setf hi (ash (logand address #xFC0) -6)
          lo (logand address #x3F))


    (write-buffer result (aref *codes* hi))
    (write-buffer result (aref *codes* lo))
    (return-from getpos result))

  ;; Otherwise, use 14-bit addressing. The library limits terminal
  ;; size to fit within 14-bit addressing, because 16-bit addressing
  ;; would require us to track state that the current API design
  ;; doesn't lend itself to.  Someday, perhaps in a v2 library
  ;; version, we'll support absurdly large terminal sizes. But for
  ;; now, 14 bits is as big as we can go.

  (setf hi (ash (logand address #x3F00) -8)
        lo (logand address #xFF))

  (when (= lo #xFF)
    (write-buffer result hi)
    (write-buffer result #xFF)
    (write-buffer result lo)
    (return-from getpos result))

  (write-buffer result hi)
  (write-buffer result lo)
  result)


;;;; end of file -- screen.lisp
