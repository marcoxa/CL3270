;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; cp-803.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-13:21:38:52

(in-package "CL3270")

;;; *e2u-codepage-803*
;;; *u2e-codepage-803*
;;;
;;; IBM CP 803 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-803*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD   #x24   #x2E   #x3C   #x28   #x2B   #x7C 
    #| 5_ |#    #x5D0 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD   #x21   #xA2   #x2A   #x29   #x3B   #xAC 
    #| 6_ |#     #x2D   #x2F #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#   #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD   #x3A   #x23   #x40   #x27   #x3D   #x22 
    #| 8_ |#   #xFFFD  #x5D1  #x5D2  #x5D3  #x5D4  #x5D5  #x5D6  #x5D7  #x5D8  #x5D9 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| 9_ |#   #xFFFD  #x5DA  #x5DB  #x5DC  #x5DD  #x5DE  #x5DF  #x5E0  #x5E1  #x5E2 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| A_ |#   #xFFFD #xFFFD  #x5E3  #x5E4  #x5E5  #x5E6  #x5E7  #x5E8  #x5E9  #x5EA #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| B_ |#   #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| C_ |#   #xFFFD   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| D_ |#   #xFFFD   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| E_ |#   #xFFFD #xFFFD   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 803 code page.")


(defparameter *u2e-codepage-803*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x37   #x2D   #x2E   #x2F   #x16   #x05   #x25   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x3C   #x3D   #x32   #x26   #x18   #x19   #x3F   #x27   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x40   #x5A   #x7F   #x7B   #x4A   #x6C   #x3F   #x7D   #x4D   #x5D   #x5C   #x4E   #x6B   #x60   #x4B   #x61 
    #| 3_ |#     #xF0   #xF1   #xF2   #xF3   #xF4   #xF5   #xF6   #xF7   #xF8   #xF9   #x7A   #x5E   #x4C   #x7E   #x6E   #x6F 
    #| 4_ |#     #x7C   #xC1   #xC2   #xC3   #xC4   #xC5   #xC6   #xC7   #xC8   #xC9   #xD1   #xD2   #xD3   #xD4   #xD5   #xD6 
    #| 5_ |#     #xD7   #xD8   #xD9   #xE2   #xE3   #xE4   #xE5   #xE6   #xE7   #xE8   #xE9   #x3F   #x3F   #x3F   #x3F   #x6D 
    #| 6_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| 7_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x4F   #x3F   #x3F   #x07 
    #| 8_ |#     #x20   #x21   #x22   #x23   #x24   #x15   #x06   #x17   #x28   #x29   #x2A   #x2B   #x2C   #x09   #x0A   #x1B 
    #| 9_ |#     #x30   #x31   #x1A   #x33   #x34   #x35   #x36   #x08   #x38   #x39   #x3A   #x3B   #x04   #x14   #x3E   #xFF 
    #| A_ |#     #x3F   #x3F   #x5B   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x5F   #x3F   #x3F   #x3F 
    #| B_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| C_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| D_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| E_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| F_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    ))
    "Implements the Unicode->EBCDIC IBM CP 803 code page.")


(defparameter *high-u2e-codepage-803*
  (make-dict
   :initial-map
   '(
    (#x05D0 #x0050) (#x05D1 #x0081) (#x05D2 #x0082) (#x05D3 #x0083) 
    (#x05D4 #x0084) (#x05D5 #x0085) (#x05D6 #x0086) (#x05D7 #x0087) 
    (#x05D8 #x0088) (#x05D9 #x0089) (#x05DA #x0091) (#x05DB #x0092) 
    (#x05DC #x0093) (#x05DD #x0094) (#x05DE #x0095) (#x05DF #x0096) 
    (#x05E0 #x0097) (#x05E1 #x0098) (#x05E2 #x0099) (#x05E3 #x00A2) 
    (#x05E4 #x00A3) (#x05E5 #x00A4) (#x05E6 #x00A5) (#x05E7 #x00A6) 
    (#x05E8 #x00A7) (#x05E9 #x00A8) (#x05EA #x00A9) 
    ))
    "Unicode->EBCDIC IBM CP 803 map for codepoints > #xff.")


(defparameter *codepage-803*
  (make-codepage
   :id 803
   :name "Codepage 803"
   :e2u *e2u-codepage-803*
   :u2e *u2e-codepage-803*
   :high-u2e *high-u2e-codepage-803*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 803.")


;;;; cp-803.lisp ends here.
