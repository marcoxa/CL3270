;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; cl3270-pkg.lisp
;;;;
;;;; Minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(defpackage "IT.UNIMIB.DISCO.MA.MAINFRAME.CL3270" (:use "CL")
  (:nicknames "CL3270")

  (:documentation "The IT.UNIMIB.DISCO.MA.MAINFRAME.CL3270 Package.

The package containing the \"3270\" Common Lisp data stream emulation
code.")

  ;; Bytes and buffers.
  (:export
   "OCTET"
   "BUFFER"
   "MAKE-BUFFER"
   "WRITE-BUFFER"
   "WRITE-BUFFER*"
   "WRITE-OCTETS-BUFFER"

   )

  ;; EBDCIC and keys
  (:export
   "EBCDIC-037"
   "ASCII-037"
  
   "EBCDIC"
   "ASCII"
   "TO-ASCII"
   "TO-EBCDIC"

   "*AID-SYMBOLS*"
   "IS-AID-NONE"
   "IS-ATTENTION-KEY"
   "IS-CLEAR-KEY"
   "IS-ENTER-KEY"
   "IS-KEY"
   "IS-PF-KEY"

   "AID-IN-SET"
   "AID-TO-STRING"

   "+AID-CLEAR+"
   "+AID-ENTER+"
   "+AID-NONE+"
   "+AID-PA1+"
   "+AID-PA2+"
   "+AID-PA3+"
   "+AID-PF1+"
   "+AID-PF10+"
   "+AID-PF11+"
   "+AID-PF12+"
   "+AID-PF13+"
   "+AID-PF14+"
   "+AID-PF15+"
   "+AID-PF16+"
   "+AID-PF17+"
   "+AID-PF18+"
   "+AID-PF19+"
   "+AID-PF2+"
   "+AID-PF20+"
   "+AID-PF21+"
   "+AID-PF22+"
   "+AID-PF23+"
   "+AID-PF24+"
   "+AID-PF3+"
   "+AID-PF4+"
   "+AID-PF5+"
   "+AID-PF6+"
   "+AID-PF7+"
   "+AID-PF8+"
   "+AID-PF9+"
   "+AID-QUERY-RESPONSE+"
   )

  ;; Codepages.
  (:export
   "CODEPAGE"
   "MAKE-CODEPAGE"
   "CODEPAGE-P"
   "CODEPAGE-E2U"
   "CODEPAGE-ESUB"
   "CODEPAGE-GE"
   "CODEPAGE-GE2U"
   "CODEPAGE-HIGH-U2E"
   "CODEPAGE-ID"
   "CODEPAGE-NAME"
   "CODEPAGE-U2E"
   "CODEPAGE-U2GE"

   "CLEAN-CODE-PAGE-LOCAL-DIR"
   "CLEAN-CODEPAGES"

   "GET-CODEPAGE"
   "GET-CODEPAGE-ID"

   "LIST-CODEPAGES"

   "*CODEPAGE-1026*"
   "*CODEPAGE-1047*"
   "*CODEPAGE-1140*"
   "*CODEPAGE-1141*"
   "*CODEPAGE-1142*"
   "*CODEPAGE-1143*"
   "*CODEPAGE-1144*"
   "*CODEPAGE-1145*"
   "*CODEPAGE-1146*"
   "*CODEPAGE-1147*"
   "*CODEPAGE-1148*"
   "*CODEPAGE-1149*"
   "*CODEPAGE-1160*"
   "*CODEPAGE-273*"
   "*CODEPAGE-275*"
   "*CODEPAGE-277*"
   "*CODEPAGE-278*"
   "*CODEPAGE-280*"
   "*CODEPAGE-284*"
   "*CODEPAGE-285*"
   "*CODEPAGE-297*"
   "*CODEPAGE-37*"
   "*CODEPAGE-424*"
   "*CODEPAGE-500*"
   "*CODEPAGE-803*"
   "*CODEPAGE-870*"
   "*CODEPAGE-871*"
   "*CODEPAGE-875*"
   "*CODEPAGE-880*"
   "*CODEPAGE-BRACKET*"
   "*CODEPAGES*"
   )

  ;; Device info
  (:export
   "DEVICE-INFO"
   "DEVICE-INFO-P"
   "MAKE-DEVICE-INFO"
   )

  ;; Telnet
  (:export
   "NEGOTIATE-TELNET"
   "UNNEGOTIATE-TELNET"

   "RESPONSE"
   "MAKE-RESPONSE"
   "RESPONSE-P"

   "RESPONSE-AID"
   "RESPONSE-ROW"
   "RESPONSE-COL"
   "RESPONSE-VALS"
   )

  ;; Application building.
  (:export
   "COLOR"
   "DEFAULT-COLOR+"
   "+BLUE+"
   "+RED+"
   "+PINK+"
   "+GREEN+"
   "+TURQUOISE+"
   "+YELLOW+"
   "+WHITE+"

   "HIGHLIGHT"
   "+DEFAULT-HIGHLIGHT+"
   "+BLINK+"
   "+REVERSE-VIDEO+"
   "+UNDERSCORE+"

   "MAKE-SCREEN"
   "SCREEN-P"
   "SCREEN-FIELDS"
   "SCREEN-NAME"

   "MAKE-SCREEN-OPTS"
   "SCREEN-OPTS-ALTSCREEN"
   "SCREEN-OPTS-CALLBACK-DATA"
   "SCREEN-OPTS-CODEPAGE"
   "SCREEN-OPTS-CURSOR-COL"
   "SCREEN-OPTS-CURSOR-ROW"
   "SCREEN-OPTS-NO-CLEAR"
   "SCREEN-OPTS-NO-RESPONSE"
   "SCREEN-OPTS-P"
   "SCREEN-OPTS-POST-SEND-CALLBACK"

   "MAKE-FIELD"
   "FIELD-AUTOSKIP"
   "FIELD-COL"
   "FIELD-COLOR"
   "FIELD-CONTENT"
   "FIELD-HIDDEN"
   "FIELD-HIGHLIGHTING"
   "FIELD-INTENSE"
   "FIELD-KEEPSPACES"
   "FIELD-NAME"
   "FIELD-NUMERIC-ONLY"
   "FIELD-P"
   "FIELD-ROW"

   "MAKE-RULES"
   "FIELD-RULES"
   "FIELD-RULES-ERROR-TEXT"
   "FIELD-RULES-FIELD"
   "FIELD-RULES-MUST-CHANGE"
   "FIELD-RULES-P"
   "FIELD-RULES-RESET"
   "FIELD-RULES-VALIDATOR"

   "HANDLE-FIELD"
   "HANDLE-SCREEN"
   "HANDLE-SCREEN-ALT"

   "HANDLE-SCREEN"
   "SHOW-SCREEN"
   "SHOW-SCREEN-NO-RESPONSE"
   "SHOW-SCREEN-OPTS"

   "VALIDATOR"
   "NON-BLANK-VALIDATOR"
   "IS-INTEGER-VALIDATOR"
   )

  ;; Transactions
  (:export
   "ABSTRACT-SESSION"
   "ABSTRACT-SESSION-P"

   "TX"

   "RUN-TRANSACTIONS"
   )

  ;; Utilities
  (:export

   "*DO-DEBUG*"
   "DBGMSG"

   "MAKE-DICT"

   "NOW-TIME"
   "TODAY-DATE"
   )
  )

;;;; end of file -- cl3270-pkg.lisp
