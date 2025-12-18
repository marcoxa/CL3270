;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; codes.lisp
;;;;
;;;; This files contains definitions of "codes" used by the some of
;;;; the parts of the system (cf., "response" and/or "telnet").
;;;;
;;;; See the file COPYING for copyright and licensing information.
;;;;
;;;; Notes:
;;;;
;;;; The code in this file was refactored here from files 'telnet.lisp
;;;; and 'response.lisp'.
;;;;
;;;; Should use my own DEFENUM for the following.


(in-package "CL3270")


;;; Telnet Constants.
;;; -----------------
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

(def-telnet-code +iac+ 255
                 "Data Byte 255.")


;;; Extra codes used here; essentially some synonyms.

(defconstant +binary-option+ +binary+
  "A synonym of +BINARY+.")

(defconstant +terminal-type-is+ 0
  "A query used by the telnet code, checking for the terminal type.")

(defconstant +terminal-type-send+ +send+
  "A synonym of +SEND+.")


;;; "AID" codes.
;;; ------------

(deftype aid ()
  "The 'aid' codes of the 3270 data stream."
  'octet ; Hopefully right.
  )


(defvar *aid-symbols* (make-hash-table))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro def-aid-code (const-name code &optional (name nil))
    `(progn
       (declaim (type aid ,const-name))
       (when ',name
         (setf (gethash ,code *aid-symbols*) ',name))
       (defconstant ,const-name ,code))))


(def-aid-code +aid-none+ #x60 none)
(def-aid-code +aid-enter+ #x7D enter)

(def-aid-code +aid-pf1+ #xF1 pf1)
(def-aid-code +aid-pf2+ #xF2 pf2)
(def-aid-code +aid-pf3+ #xF3 pf3)
(def-aid-code +aid-pf4+ #xF4 pf4)
(def-aid-code +aid-pf5+ #xF5 pf5)

(def-aid-code +aid-pf6+ #xF6 pf6)
(def-aid-code +aid-pf7+ #xF7 pf7)
(def-aid-code +aid-pf8+ #xF8 pf8)
(def-aid-code +aid-pf9+ #xF9 pf9)
(def-aid-code +aid-pf10+ #x7A pf10)

(def-aid-code +aid-pf11+ #x7B pf11)
(def-aid-code +aid-pf12+ #x7C pf12)
(def-aid-code +aid-pf13+ #xC1 pf13)
(def-aid-code +aid-pf14+ #xC2 pf14)
(def-aid-code +aid-pf15+ #xC3 pf15)

(def-aid-code +aid-pf16+ #xC4 pf16)
(def-aid-code +aid-pf17+ #xC5 pf17)
(def-aid-code +aid-pf18+ #xC6 pf18)
(def-aid-code +aid-pf19+ #xC7 pf19)
(def-aid-code +aid-pf20+ #xC8 pf20)

(def-aid-code +aid-pf21+ #xC9 pf21)
(def-aid-code +aid-pf22+ #x4A pf22)
(def-aid-code +aid-pf23+ #x4B pf23)
(def-aid-code +aid-pf24+ #x4C pf24)

(def-aid-code +aid-pa1+ #x6C pa1)
(def-aid-code +aid-pa2+ #x6E pa2)
(def-aid-code +aid-pa3+ #x6B pa3)

(def-aid-code +aid-clear+ #x6D clear)

(def-aid-code +aid-query-response+ #x88)


(declaim (inline aid-to-string)
         (ftype (function (aid) string) aid-to-string))
(defun aid-to-string (aid-key)
  (string (gethash aid-key *aid-symbols* "[unknown]")))
      

(declaim (inline is-aid-none))
(defun is-aid-none (b)
  (= b +aid-none+))


(declaim (inline is-attention-key))
(defun is-attention-key (b)
  (<= #x6B ; +aid-pa3+
      b
      #x6E ; +aid-pa2+
      ))

(declaim (inline is-clear-key))
(defun is-clear-key (b)
  (= +aid-clear+ b))


(declaim (inline is-enter-key))
(defun is-enter-key (b)
  (= +aid-enter+ b))


(declaim (inline is-pf-key))
(defun is-pf-key (b)
  (or (<= +aid-pf1+  b +aid-pf9+)
      (<= +aid-pf10+ b +aid-pf12+)
      (<= +aid-pf13+ b +aid-pf21+)
      (<= +aid-pf22+ b +aid-pf24+))
  )


(defmacro is-key (k1 k2)
  `(= ,k1 ,k2))


(declaim
 (ftype (function (aid) boolean)
        is-aid-none is-attention-key is-clear-key is-enter-key is-pf-key))

;;;; codes.lisp end here.
