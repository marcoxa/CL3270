;;;; -*- Mode: Lisp -*-

;;;; looper.lisp
;;;;
;;;; Handling the back and forth from a 3270.
;;;;
;;;; See the file COPYING for copyright and licensing information.


(in-package "CL3270")

;;; Rules is a map of field names (strings) to FieldRules structs. Each
;;; field for which you wish validation to occur must appear in the
;;; map. Fields not in the map will not have any input validation
;;; performed.

(deftype rules ()
  'hash-table ; map[string]FieldRules
  )


;;; Validator is a type that represents a function which can perform
;;; field input validation. The function is passed a string, input, and
;;; returns true if the input is valid or false if the not.

(deftype validator ()
  '(function (string) boolean))


;;; NonBlank is a Validator that returns true if, after spaces are
;;; trimmed from the beginning and end of the string, the value is not
;;; empty.

(defun non-blank-validator (input)
  (declare (type string input))
  (not (string= "" (string-trim '(#\Space) input))))


;;;;; var isIntegerRegexp = regexp.MustCompile(`^-?[0-9]+$`)

;;; is-integer-validator is a Validator that returns true if, after
;;; spaces are trimmed from the beginning and end if the string, the
;;; value is an integer (including negative numbers and 0).

(defun is-integer-validator (input)
  (declare (type string input))
  (ignore-errors
    (integerp
     (parse-integer (string-trim '(#\Space) input)
                    :junk-allowed nil))))


(defstruct (field-rules
            (:constructor field-rules (field
                                       &key
                                       must-change
                                       error-text
                                       validator
                                       reset)))
  "The FIELD-RULES structure.

FIELD-RULES objects provide validation rules for a particular field."

  (field "" :type string)

  ;; must-change, when true, indicates that the value of the field MUST be
  ;; altered by the user -- if applied to a field with no starting value,
  ;; this makes the field a required field. If true on a field with a
  ;; starting value (either in the field's Content attribute, or with an
  ;; override in the initial values map), then the user must change
  ;; the value from the default.
  (must-change nil :type boolean)

  ;; error-text is the text displayed with the MustChange validation fails.
  ;; If ErrorText is the empty string, but MustValidation fails, an error
  ;; string will be constructed from the field name:
  ;; "Please enter a valid value for <fieldName>."
  (error-text "" :type string)

  ;; validator is a function to validate the value the user input into
  ;; the field. It may be nil if no validation is required. The
  ;; Validator function is called *after* the MustChange logic, so if
  ;; you wish to fully handle validation, ensure MustChange is set to
  ;; false.
  (validator (constantly t) :type (or symbol function validator)) ; Not quite right.

  ;; reset indicates that if the screen fails validation, this field
  ;; should always be reset to its original/default value, regardless
  ;; of what the user entered.
  (reset nil :type boolean)
  )


;;; make-rules

(defun make-rules (&rest frs &aux (rs (make-hash-table :test #'equal)))
  (loop for fr in frs
        do (setf (gethash (field-rules-field fr) rs) fr)
        )
  rs)


;;; handle-screen

(defun handle-screen (screen
                      rules
                      vals
                      pf-keys
                      exit-keys
                      error-field
                      curs-row
                      curs-col 
                      conn)
  "A higher-level interface to the SHOW-SCREEN function.

HANDLE-SCREEN will loop until all validation rules are satisfied, and only
return when an expected AID (i.e. PF) key is pressed.

HANDLE-SCREEN will return when the user: 1) presses a key in pfkeys AND all
fields pass validation, OR 2) the user presses a key in exitkeys. In all
other cases, HANDLE-SCREEN will re-present the screen to the user again,
possibly with an error message set in the errorField field.

Arguments and Values:

SCREEN -- the screen to display (see SHOW-SCREEN).
RULES -- the rules to enforce: each key in the Rules map corresponds
         to a FIELD-NAME in the SCREEN array.
VALS  -- field values you wish to override (see SHOW-SCREEN).
PF-KEYS, EXIT-KEYS -- the AID keys that you wish to accept (that is,
   perform validation and return if successful) and treat as exit keys
   (unconditionally return).
ERROR-FIELD -- the name of a field in the screen array that you wish error
   messages to be written in when HANDLE-SCREEN loops waiting for a valid
   user submission.
CURS-ROW, CURS-COL -- the initial cursor position.
CONN -- the network connection to the 3270 client.
"

  ;; Save the original field values for any named fields to support
  ;; the MustChange rule. Also build a map of named fields.
  (let ((orig-values (make-hash-table :test #'equal))
        (fields (make-hash-table :test #'equal))
        (my-vals (make-hash-table :test #'equal))
        )

    (dbgmsg ">>> HANDLE-SCREEN: saving values and fields.~%")

    (dolist (f (screen-fields screen))
      (when (not (string= "" (field-name f)))
        (setf (gethash (field-name f) orig-values)
              (field-content f)

              (gethash (field-name f) fields)
              f)))

    (loop for f being the hash-key of vals using (hash-value v)
          do (setf (gethash f my-vals) v))

    (dbgmsg ">>> HANDLE-SCREEN: saved values and fields.~%")


    ;; The tagbodies and the GOs are probably fixable in a better way,
    ;; but as such the mapping is almost 1-1 with Matthew R. Wilson's GO
    ;; code.

    (tagbody
     :mainloop
     (loop
      (tagbody
       :continue

       ;; Reset fields with FIELD-RULES-RESET set.
       (when rules
         (loop for field being the hash-key of rules using (hash-value rule)
               when (and (field-rules-reset rule)
                         (nth-value 1 (gethash field fields)))
               do (multiple-value-bind (v foundp)
                      (gethash field my-vals)
                    (if foundp
                        (setf (gethash field my-vals) v)
                        (remhash field my-vals)))))
       

       (multiple-value-bind (resp err)
           (show-screen screen my-vals curs-row curs-col conn)

         (when err
           (format *error-output* "!!! SHOW-SCREEN 1 error ~S~%" err)
           (return-from handle-screen (values resp err)))

         ;; If we got an exit key, return without performing
         ;; validation.

         (when (aid-in-set (response-aid resp) exit-keys)
           (return-from handle-screen (values resp nil)))

         ;; If we got an unexpected key, set error message and restart
         ;; loop.

         (unless (aid-in-set (response-aid resp) pf-keys)
           (unless (or (is-clear-key (response-aid resp))
                       (is-attention-key (response-aid resp)))
             (setf my-vals (merge-field-values my-vals (response-values resp))))
           (setf (gethash error-field my-vals)
                 (format nil "~S: unknown key"
                         (aid-to-string (response-aid resp))))
           (go :continue))

         ;; At this point, we have an expected key. If one of the
         ;; "clear" keys is expected, we can't do much, so we'll just
         ;; return.

         (when (or (is-clear-key (response-aid resp))
                   (is-attention-key (response-aid resp)))
           (return-from handle-screen (values resp nil)))

         (setq my-vals (merge-field-values my-vals (response-values resp)))
         (remhash error-field my-vals)

         ;; Now we can validate each field, if we must
        
         (when rules
           (loop for field being the hash-key of rules using (hash-value fr)

                 unless (nth-value 1 (gethash field my-vals))
                 do
                 (go :continue)
                 end

                 when (and (field-rules-must-change fr)
                           (string= (gethash field my-vals)
                                    (gethash field orig-values)))
                 do (setf (gethash error-field my-vals)
                          (field-rules-error-text fr))
                 (go :mainloop)
                 end
              
                 when (and (field-rules-validator fr)
                           (not (funcall (field-rules-validator fr)
                                         (gethash field my-vals))))
                 do (setf (gethash error-field my-vals)
                          (format nil "Value for ~S is not valid"
                                  field))
                 (go :mainloop)
                 end
                 ))
         (return-from handle-screen (values resp nil))
         ))
      ))
    ))


(defun aid-in-set (aid aids)
  (find aid aids :test #'=))


(defun merge-field-values (original current)

  "Merge the ORIGINAL and CURRENT maps.

The function returns a new map, containing all keys from the current
map and keys from the original map that do not exist in the current map.
This is sometimes necessary because the caller of HANDLE-SCREEN may
provide override values for non-writable fields, and we don't get those
values back when we round-trip with the 3270 client.
"
  (declare (type hash-table original current))
  (let ((result (make-hash-table :test (hash-table-test original))))
    (loop for key being the hash-key of current
          do (setf (gethash key result) (gethash key current)))

    (loop for key being the hash-key of original
          unless (nth-value 1 (gethash key result))
          do (setf (gethash key result)
                   (gethash key original)))
          
    result
    ))


;;;; end of file -- looper.lisp
