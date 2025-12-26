;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; cp-1142.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-13:21:38:52

(in-package "CL3270")

;;; *e2u-codepage-1142*
;;; *u2e-codepage-1142*
;;;
;;; IBM CP 1142 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-1142*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20   #xA0   #xE2   #xE4   #xE0   #xE1   #xE3   #x7D   #xE7   #xF1   #x23   #x2E   #x3C   #x28   #x2B   #x21 
    #| 5_ |#     #x26   #xE9   #xEA   #xEB   #xE8   #xED   #xEE   #xEF   #xEC   #xDF #x20AC   #xC5   #x2A   #x29   #x3B   #x5E 
    #| 6_ |#     #x2D   #x2F   #xC2   #xC4   #xC0   #xC1   #xC3   #x24   #xC7   #xD1   #xF8   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#     #xA6   #xC9   #xCA   #xCB   #xC8   #xCD   #xCE   #xCF   #xCC   #x60   #x3A   #xC6   #xD8   #x27   #x3D   #x22 
    #| 8_ |#     #x40   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69   #xAB   #xBB   #xF0   #xFD   #xFE   #xB1 
    #| 9_ |#     #xB0   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72   #xAA   #xBA   #x7B   #xB8   #x5B   #x5D 
    #| A_ |#     #xB5   #xFC   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A   #xA1   #xBF   #xD0   #xDD   #xDE   #xAE 
    #| B_ |#     #xA2   #xA3   #xA5   #xB7   #xA9   #xA7   #xB6   #xBC   #xBD   #xBE   #xAC   #x7C   #xAF   #xA8   #xB4   #xD7 
    #| C_ |#     #xE6   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49   #xAD   #xF4   #xF6   #xF2   #xF3   #xF5 
    #| D_ |#     #xE5   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52   #xB9   #xFB   #x7E   #xF9   #xFA   #xFF 
    #| E_ |#     #x5C   #xF7   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A   #xB2   #xD4   #xD6   #xD2   #xD3   #xD5 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39   #xB3   #xDB   #xDC   #xD9   #xDA   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 1142 code page.")


(defparameter *u2e-codepage-1142*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x37   #x2D   #x2E   #x2F   #x16   #x05   #x25   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x3C   #x3D   #x32   #x26   #x18   #x19   #x3F   #x27   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x40   #x4F   #x7F   #x4A   #x67   #x6C   #x50   #x7D   #x4D   #x5D   #x5C   #x4E   #x6B   #x60   #x4B   #x61 
    #| 3_ |#     #xF0   #xF1   #xF2   #xF3   #xF4   #xF5   #xF6   #xF7   #xF8   #xF9   #x7A   #x5E   #x4C   #x7E   #x6E   #x6F 
    #| 4_ |#     #x80   #xC1   #xC2   #xC3   #xC4   #xC5   #xC6   #xC7   #xC8   #xC9   #xD1   #xD2   #xD3   #xD4   #xD5   #xD6 
    #| 5_ |#     #xD7   #xD8   #xD9   #xE2   #xE3   #xE4   #xE5   #xE6   #xE7   #xE8   #xE9   #x9E   #xE0   #x9F   #x5F   #x6D 
    #| 6_ |#     #x79   #x81   #x82   #x83   #x84   #x85   #x86   #x87   #x88   #x89   #x91   #x92   #x93   #x94   #x95   #x96 
    #| 7_ |#     #x97   #x98   #x99   #xA2   #xA3   #xA4   #xA5   #xA6   #xA7   #xA8   #xA9   #x9C   #xBB   #x47   #xDC   #x07 
    #| 8_ |#     #x20   #x21   #x22   #x23   #x24   #x15   #x06   #x17   #x28   #x29   #x2A   #x2B   #x2C   #x09   #x0A   #x1B 
    #| 9_ |#     #x30   #x31   #x1A   #x33   #x34   #x35   #x36   #x08   #x38   #x39   #x3A   #x3B   #x04   #x14   #x3E   #xFF 
    #| A_ |#     #x41   #xAA   #xB0   #xB1   #x3F   #xB2   #x70   #xB5   #xBD   #xB4   #x9A   #x8A   #xBA   #xCA   #xAF   #xBC 
    #| B_ |#     #x90   #x8F   #xEA   #xFA   #xBE   #xA0   #xB6   #xB3   #x9D   #xDA   #x9B   #x8B   #xB7   #xB8   #xB9   #xAB 
    #| C_ |#     #x64   #x65   #x62   #x66   #x63   #x5B   #x7B   #x68   #x74   #x71   #x72   #x73   #x78   #x75   #x76   #x77 
    #| D_ |#     #xAC   #x69   #xED   #xEE   #xEB   #xEF   #xEC   #xBF   #x7C   #xFD   #xFE   #xFB   #xFC   #xAD   #xAE   #x59 
    #| E_ |#     #x44   #x45   #x42   #x46   #x43   #xD0   #xC0   #x48   #x54   #x51   #x52   #x53   #x58   #x55   #x56   #x57 
    #| F_ |#     #x8C   #x49   #xCD   #xCE   #xCB   #xCF   #xCC   #xE1   #x6A   #xDD   #xDE   #xDB   #xA1   #x8D   #x8E   #xDF 
    ))
    "Implements the Unicode->EBCDIC IBM CP 1142 code page.")


(defparameter *high-u2e-codepage-1142*
  (make-dict
   :initial-map
   '(
    (#x20AC #x005A) 
    ))
    "Unicode->EBCDIC IBM CP 1142 map for codepoints > #xff.")


(defparameter *codepage-1142*
  (make-codepage
   :id 1142
   :name "Codepage 1142"
   :e2u *e2u-codepage-1142*
   :u2e *u2e-codepage-1142*
   :high-u2e *high-u2e-codepage-1142*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 1142.")


;;;; cp-1142.lisp ends here.
