;;;; -*- Mode: Lisp -*-

;;;; codepage.lisp
;;;;
;;;; Codepage representations for minimal 3720 data stream
;;;; emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.
;;;;
;;;; Notes:
;;;;
;;;; Again, just roughly following Matthew R. Wilson's API.
;;;;
;;;; Probably, all of this can be replaced by BABEL, but not yet.

(in-package "CL3270")

(deftype codepage-id ()
  "A Codepage ID.

Notes:

According to Wikipedia (https://https://en.wikipedia.org/wiki/Code_page)
the IBM convention was to use a 16 bits number to identify a codepage.
Hence the (MOD #x10000) limitation.

Of course, vendors messed up, as the Wikipedia page explains."

  '(mod #x10000))


(defstruct codepage
  "The Codepage Structure."

  ;; (id 0 :type codepage-id :read-only t) ; LW complains.
  (id 0 :type codepage-id :read-only t)
  (name "" :type string :read-only t)

  ;; EBCDIC byte to Unicode code point for bytes #x00 to #xff.
  ;; The type CHARACTER appears to work in most Common Lisp.

  (e2u nil :type (or null (vector character 256)) :read-only t)

  ;; Unicode code point to EBCDIC byte for code points #x00 to #xff.

  (u2e nil :type (or null (vector octet 256)) :read-only t)

  ;; Map of Unicode code points to EBCDIC bytes for code points above #xff.

  (high-u2e (make-hash-table :test #'eq) :type hash-table :read-only t)

  ;; The EBCDIC substitute character to use if there is no EBCDIC character
  ;; for the requested Unicode code point (typically #x3f).

  (esub #x3f :type octet :read-only t)

  ;; The "graphic escape" EBCDIC byte (is it ever anything other than 0x0E?).

  (ge #x0e :type octet :read-only t)

  ;; Graphic escape codepage EBCDIC byte to Unicode code point for bytes
  ;; #x00 to #xff.  Use character `#\Replacement-Character` (`#\Ufffd`)
  ;; for unmapped bytes.
  
  (ge2u nil :type (or null (vector character 256)) :read-only t)

  ;; Map of Unicode code points to graphic escape EBCDIC bytes.

  (u2ge (make-hash-table :test #'eq) :type hash-table :read-only t)

  )


;;; Functions and methods.

(defmethod print-object ((cp codepage) stream)
  (print-unreadable-object (cp stream)
    (format stream "CODEPAGE ~D ~S"
            (codepage-id cp)
            (codepage-name cp))))


;;; decode-ebcdic

(defun decode-ebcdic (cp bytes)
  "Decode an EBCDIC byte array into a 'character' string.

The decoding handles graphic escape to codepage CP310 as needed."

  (declare (type codepage cp)
           (type (vector (unsigned-byte 8)) bytes))

  (let ((runes (make-array (length bytes)
                           :fill-pointer 0
                           :element-type 'character
                           :initial-element #\ufffd ; Replacement Character.
                           ))
        (escape nil)
        (ge2u (codepage-ge2u cp))
        (e2u  (codepage-e2u cp))
        (ge   (codepage-ge cp))
        (repl-char-code (char-code #\ufffd)) ; Replacement Character Code.
        (sub-char (code-char #x1a)) ; Substitution Character.
        )
    (declare (type (vector character) runes)
             (type boolean escape)
             (type (or null (vector character)) ge2u e2u)
             (type (unsigned-byte 16) repl-char-code)
             (type (unsigned-byte 8) ge) ; octet
             (type character sub-char)
             )

    (loop for b of-type octet across bytes
          if escape
            do (let ((r (aref ge2u b)))
                 (declare (type character r))

                 (setq escape nil)
                 (if (/= (char-code r) repl-char-code)

                     (vector-push runes r)
                     (vector-push runes sub-char) ; Unicode "substitute".
                     ))
          else
            ;; Enter graphic escape mode if necessary.
            do (if (/= b (char-code ge))
                   (vector-push runes (the character (aref e2u b)))
                   (setf escape t)))

    ;; Finally return the string (UNICODE).

    (with-output-to-string (s nil :element-type 'character)
      (loop for r of-type character across runes do (write-char r s)))))


;;; encode-characters

(defun encode-characters (cp s)
  "Encode CHARACTER string S into an EBCDIC byte array.

The encoding will handle graphic escape to CP310 as needed."

  (declare (type codepage cp)
           (type string s))
  
  (let ((u2e    (codepage-u2e cp))
        (high2e (codepage-u2ge cp))
        (u2ge   (codepage-u2ge cp))
        (ge     (codepage-ge cp))
        (esub   (codepage-esub cp))
        )
    (declare (type (vector octet) u2e)
             (type hash-table high2e u2ge)
             (type octet ge esub)) 

    (loop with out = (make-buffer :capacity (length s))
          with ec of-type octet = esub
          with ec-p of-type boolean = nil
          with u2e-len of-type fixnum = (length u2e) ; of-type (eql 256) ?

          for c of-type character across s
          for cc = (char-code c)

          initially (progn ec-p ge) ; Will it work?
          do (cond ((< cc u2e-len) (write-buffer out (aref cc u2e)))

                   ((nth 1 (setf (values ec ec-p) (gethash cc high2e)))
                    (write-buffer out ec))

                   ((nth 1 (setf (values ec ec-p) (gethash cc u2ge)))
                    (write-buffer out ge)
                    (write-buffer out ec))
                   
                   (t (write-buffer out esub)))

          finally (return-from encode-characters out))))


;;;; end of file -- codepage.lisp
