;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; cp-273.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-28:10:30:21

(in-package "CL3270")

;;; *e2u-codepage-273*
;;; *u2e-codepage-273*
;;;
;;; IBM CP 273 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-273*
  (make-array 256 :element-type 'rune
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20   #xA0   #xE2   #x7B   #xE0   #xE1   #xE3   #xE5   #xE7   #xF1   #xC4   #x2E   #x3C   #x28   #x2B   #x21 
    #| 5_ |#     #x26   #xE9   #xEA   #xEB   #xE8   #xED   #xEE   #xEF   #xEC   #x7E   #xDC   #x24   #x2A   #x29   #x3B   #x5E 
    #| 6_ |#     #x2D   #x2F   #xC2   #x5B   #xC0   #xC1   #xC3   #xC5   #xC7   #xD1   #xF6   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#     #xF8   #xC9   #xCA   #xCB   #xC8   #xCD   #xCE   #xCF   #xCC   #x60   #x3A   #x23   #xA7   #x27   #x3D   #x22 
    #| 8_ |#     #xD8   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69   #xAB   #xBB   #xF0   #xFD   #xFE   #xB1 
    #| 9_ |#     #xB0   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72   #xAA   #xBA   #xE6   #xB8   #xC6   #xA4 
    #| A_ |#     #xB5   #xDF   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A   #xA1   #xBF   #xD0   #xDD   #xDE   #xAE 
    #| B_ |#     #xA2   #xA3   #xA5   #xB7   #xA9   #x40   #xB6   #xBC   #xBD   #xBE   #xAC   #x7C   #xAF   #xA8   #xB4   #xD7 
    #| C_ |#     #xE4   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49   #xAD   #xF4   #xA6   #xF2   #xF3   #xF5 
    #| D_ |#     #xFC   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52   #xB9   #xFB   #x7D   #xF9   #xFA   #xFF 
    #| E_ |#     #xD6   #xF7   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A   #xB2   #xD4   #x5C   #xD2   #xD3   #xD5 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39   #xB3   #xDB   #x5D   #xD9   #xDA   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 273 code page.")


(defparameter *u2e-codepage-273*
  (make-array 256 :element-type 'octet
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x37   #x2D   #x2E   #x2F   #x16   #x05   #x25   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x3C   #x3D   #x32   #x26   #x18   #x19   #x3F   #x27   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x40   #x4F   #x7F   #x7B   #x5B   #x6C   #x50   #x7D   #x4D   #x5D   #x5C   #x4E   #x6B   #x60   #x4B   #x61 
    #| 3_ |#     #xF0   #xF1   #xF2   #xF3   #xF4   #xF5   #xF6   #xF7   #xF8   #xF9   #x7A   #x5E   #x4C   #x7E   #x6E   #x6F 
    #| 4_ |#     #xB5   #xC1   #xC2   #xC3   #xC4   #xC5   #xC6   #xC7   #xC8   #xC9   #xD1   #xD2   #xD3   #xD4   #xD5   #xD6 
    #| 5_ |#     #xD7   #xD8   #xD9   #xE2   #xE3   #xE4   #xE5   #xE6   #xE7   #xE8   #xE9   #x63   #xEC   #xFC   #x5F   #x6D 
    #| 6_ |#     #x79   #x81   #x82   #x83   #x84   #x85   #x86   #x87   #x88   #x89   #x91   #x92   #x93   #x94   #x95   #x96 
    #| 7_ |#     #x97   #x98   #x99   #xA2   #xA3   #xA4   #xA5   #xA6   #xA7   #xA8   #xA9   #x43   #xBB   #xDC   #x59   #x07 
    #| 8_ |#     #x20   #x21   #x22   #x23   #x24   #x15   #x06   #x17   #x28   #x29   #x2A   #x2B   #x2C   #x09   #x0A   #x1B 
    #| 9_ |#     #x30   #x31   #x1A   #x33   #x34   #x35   #x36   #x08   #x38   #x39   #x3A   #x3B   #x04   #x14   #x3E   #xFF 
    #| A_ |#     #x41   #xAA   #xB0   #xB1   #x9F   #xB2   #xCC   #x7C   #xBD   #xB4   #x9A   #x8A   #xBA   #xCA   #xAF   #xBC 
    #| B_ |#     #x90   #x8F   #xEA   #xFA   #xBE   #xA0   #xB6   #xB3   #x9D   #xDA   #x9B   #x8B   #xB7   #xB8   #xB9   #xAB 
    #| C_ |#     #x64   #x65   #x62   #x66   #x4A   #x67   #x9E   #x68   #x74   #x71   #x72   #x73   #x78   #x75   #x76   #x77 
    #| D_ |#     #xAC   #x69   #xED   #xEE   #xEB   #xEF   #xE0   #xBF   #x80   #xFD   #xFE   #xFB   #x5A   #xAD   #xAE   #xA1 
    #| E_ |#     #x44   #x45   #x42   #x46   #xC0   #x47   #x9C   #x48   #x54   #x51   #x52   #x53   #x58   #x55   #x56   #x57 
    #| F_ |#     #x8C   #x49   #xCD   #xCE   #xCB   #xCF   #x6A   #xE1   #x70   #xDD   #xDE   #xDB   #xD0   #x8D   #x8E   #xDF 
    ))
    "Implements the Unicode->EBCDIC IBM CP 273 code page.")


(defparameter *high-u2e-codepage-273*
  (make-dict
   :initial-map
   '(
    
    ))
    "Unicode->EBCDIC IBM CP 273 map for codepoints > #xff.")


(defparameter *codepage-273*
  (make-codepage
   :id 273
   :name "Codepage 273"
   :e2u *e2u-codepage-273*
   :u2e *u2e-codepage-273*
   :high-u2e *high-u2e-codepage-273*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 273.")


;;;; cp-273.lisp ends here.
