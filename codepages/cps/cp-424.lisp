;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; cp-424.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-13:21:38:52

(in-package "CL3270")

;;; *e2u-codepage-424*
;;; *u2e-codepage-424*
;;;
;;; IBM CP 424 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-424*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20  #x5D0  #x5D1  #x5D2  #x5D3  #x5D4  #x5D5  #x5D6  #x5D7  #x5D8   #xA2   #x2E   #x3C   #x28   #x2B   #x7C 
    #| 5_ |#     #x26  #x5D9  #x5DA  #x5DB  #x5DC  #x5DD  #x5DE  #x5DF  #x5E0  #x5E1   #x21   #x24   #x2A   #x29   #x3B   #xAC 
    #| 6_ |#     #x2D   #x2F  #x5E2  #x5E3  #x5E4  #x5E5  #x5E6  #x5E7  #x5E8  #x5E9   #xA6   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#   #xFFFD  #x5EA #xFFFD #xFFFD   #xA0 #xFFFD #xFFFD #xFFFD #x2017   #x60   #x3A   #x23   #x40   #x27   #x3D   #x22 
    #| 8_ |#   #xFFFD   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69   #xAB   #xBB #xFFFD #xFFFD #xFFFD   #xB1 
    #| 9_ |#     #xB0   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72 #xFFFD #xFFFD #xFFFD   #xB8 #xFFFD   #xA4 
    #| A_ |#     #xB5   #x7E   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD   #xAE 
    #| B_ |#     #x5E   #xA3   #xA5 #x2022   #xA9   #xA7   #xB6   #xBC   #xBD   #xBE   #x5B   #x5D #x203E   #xA8   #xB4   #xD7 
    #| C_ |#     #x7B   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49   #xAD #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| D_ |#     #x7D   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52   #xB9 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| E_ |#     #x5C   #xF7   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A   #xB2 #xFFFD #xFFFD #xFFFD #xFFFD #xFFFD 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39   #xB3 #xFFFD #xFFFD #xFFFD #xFFFD   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 424 code page.")


(defparameter *u2e-codepage-424*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x37   #x2D   #x2E   #x2F   #x16   #x05   #x25   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x3C   #x3D   #x32   #x26   #x18   #x19   #x3F   #x27   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x40   #x5A   #x7F   #x7B   #x5B   #x6C   #x50   #x7D   #x4D   #x5D   #x5C   #x4E   #x6B   #x60   #x4B   #x61 
    #| 3_ |#     #xF0   #xF1   #xF2   #xF3   #xF4   #xF5   #xF6   #xF7   #xF8   #xF9   #x7A   #x5E   #x4C   #x7E   #x6E   #x6F 
    #| 4_ |#     #x7C   #xC1   #xC2   #xC3   #xC4   #xC5   #xC6   #xC7   #xC8   #xC9   #xD1   #xD2   #xD3   #xD4   #xD5   #xD6 
    #| 5_ |#     #xD7   #xD8   #xD9   #xE2   #xE3   #xE4   #xE5   #xE6   #xE7   #xE8   #xE9   #xBA   #xE0   #xBB   #xB0   #x6D 
    #| 6_ |#     #x79   #x81   #x82   #x83   #x84   #x85   #x86   #x87   #x88   #x89   #x91   #x92   #x93   #x94   #x95   #x96 
    #| 7_ |#     #x97   #x98   #x99   #xA2   #xA3   #xA4   #xA5   #xA6   #xA7   #xA8   #xA9   #xC0   #x4F   #xD0   #xA1   #x07 
    #| 8_ |#     #x20   #x21   #x22   #x23   #x24   #x15   #x06   #x17   #x28   #x29   #x2A   #x2B   #x2C   #x09   #x0A   #x1B 
    #| 9_ |#     #x30   #x31   #x1A   #x33   #x34   #x35   #x36   #x08   #x38   #x39   #x3A   #x3B   #x04   #x14   #x3E   #xFF 
    #| A_ |#     #x74   #x3F   #x4A   #xB1   #x9F   #xB2   #x6A   #xB5   #xBD   #xB4   #x3F   #x8A   #x5F   #xCA   #xAF   #x3F 
    #| B_ |#     #x90   #x8F   #xEA   #xFA   #xBE   #xA0   #xB6   #x3F   #x9D   #xDA   #x3F   #x8B   #xB7   #xB8   #xB9   #x3F 
    #| C_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| D_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #xBF   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| E_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| F_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #xE1   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    ))
    "Implements the Unicode->EBCDIC IBM CP 424 code page.")


(defparameter *high-u2e-codepage-424*
  (make-dict
   :initial-map
   '(
    (#x05D0 #x0041) (#x05D1 #x0042) (#x05D2 #x0043) (#x05D3 #x0044) 
    (#x05D4 #x0045) (#x05D5 #x0046) (#x05D6 #x0047) (#x05D7 #x0048) 
    (#x05D8 #x0049) (#x05D9 #x0051) (#x05DA #x0052) (#x05DB #x0053) 
    (#x05DC #x0054) (#x05DD #x0055) (#x05DE #x0056) (#x05DF #x0057) 
    (#x05E0 #x0058) (#x05E1 #x0059) (#x05E2 #x0062) (#x05E3 #x0063) 
    (#x05E4 #x0064) (#x05E5 #x0065) (#x05E6 #x0066) (#x05E7 #x0067) 
    (#x05E8 #x0068) (#x05E9 #x0069) (#x05EA #x0071) (#x2017 #x0078) 
    (#x2022 #x00B3) (#x203E #x00BC) 
    ))
    "Unicode->EBCDIC IBM CP 424 map for codepoints > #xff.")


(defparameter *codepage-424*
  (make-codepage
   :id 424
   :name "Codepage 424"
   :e2u *e2u-codepage-424*
   :u2e *u2e-codepage-424*
   :high-u2e *high-u2e-codepage-424*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 424.")


;;;; cp-424.lisp ends here.
