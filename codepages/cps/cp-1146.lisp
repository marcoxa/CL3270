;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; cp-1146.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-28:10:30:21

(in-package "CL3270")

;;; *e2u-codepage-1146*
;;; *u2e-codepage-1146*
;;;
;;; IBM CP 1146 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-1146*
  (make-array 256 :element-type 'rune
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20   #xA0   #xE2   #xE4   #xE0   #xE1   #xE3   #xE5   #xE7   #xF1   #x24   #x2E   #x3C   #x28   #x2B   #x7C 
    #| 5_ |#     #x26   #xE9   #xEA   #xEB   #xE8   #xED   #xEE   #xEF   #xEC   #xDF   #x21   #xA3   #x2A   #x29   #x3B   #xAC 
    #| 6_ |#     #x2D   #x2F   #xC2   #xC4   #xC0   #xC1   #xC3   #xC5   #xC7   #xD1   #xA6   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#     #xF8   #xC9   #xCA   #xCB   #xC8   #xCD   #xCE   #xCF   #xCC   #x60   #x3A   #x23   #x40   #x27   #x3D   #x22 
    #| 8_ |#     #xD8   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69   #xAB   #xBB   #xF0   #xFD   #xFE   #xB1 
    #| 9_ |#     #xB0   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72   #xAA   #xBA   #xE6   #xB8   #xC6 #x20AC 
    #| A_ |#     #xB5   #xAF   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A   #xA1   #xBF   #xD0   #xDD   #xDE   #xAE 
    #| B_ |#     #xA2   #x5B   #xA5   #xB7   #xA9   #xA7   #xB6   #xBC   #xBD   #xBE   #x5E   #x5D   #x7E   #xA8   #xB4   #xD7 
    #| C_ |#     #x7B   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49   #xAD   #xF4   #xF6   #xF2   #xF3   #xF5 
    #| D_ |#     #x7D   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52   #xB9   #xFB   #xFC   #xF9   #xFA   #xFF 
    #| E_ |#     #x5C   #xF7   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A   #xB2   #xD4   #xD6   #xD2   #xD3   #xD5 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39   #xB3   #xDB   #xDC   #xD9   #xDA   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 1146 code page.")


(defparameter *u2e-codepage-1146*
  (make-array 256 :element-type 'octet
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x37   #x2D   #x2E   #x2F   #x16   #x05   #x25   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x3C   #x3D   #x32   #x26   #x18   #x19   #x3F   #x27   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x40   #x5A   #x7F   #x7B   #x4A   #x6C   #x50   #x7D   #x4D   #x5D   #x5C   #x4E   #x6B   #x60   #x4B   #x61 
    #| 3_ |#     #xF0   #xF1   #xF2   #xF3   #xF4   #xF5   #xF6   #xF7   #xF8   #xF9   #x7A   #x5E   #x4C   #x7E   #x6E   #x6F 
    #| 4_ |#     #x7C   #xC1   #xC2   #xC3   #xC4   #xC5   #xC6   #xC7   #xC8   #xC9   #xD1   #xD2   #xD3   #xD4   #xD5   #xD6 
    #| 5_ |#     #xD7   #xD8   #xD9   #xE2   #xE3   #xE4   #xE5   #xE6   #xE7   #xE8   #xE9   #xB1   #xE0   #xBB   #xBA   #x6D 
    #| 6_ |#     #x79   #x81   #x82   #x83   #x84   #x85   #x86   #x87   #x88   #x89   #x91   #x92   #x93   #x94   #x95   #x96 
    #| 7_ |#     #x97   #x98   #x99   #xA2   #xA3   #xA4   #xA5   #xA6   #xA7   #xA8   #xA9   #xC0   #x4F   #xD0   #xBC   #x07 
    #| 8_ |#     #x20   #x21   #x22   #x23   #x24   #x15   #x06   #x17   #x28   #x29   #x2A   #x2B   #x2C   #x09   #x0A   #x1B 
    #| 9_ |#     #x30   #x31   #x1A   #x33   #x34   #x35   #x36   #x08   #x38   #x39   #x3A   #x3B   #x04   #x14   #x3E   #xFF 
    #| A_ |#     #x41   #xAA   #xB0   #x5B   #x3F   #xB2   #x6A   #xB5   #xBD   #xB4   #x9A   #x8A   #x5F   #xCA   #xAF   #xA1 
    #| B_ |#     #x90   #x8F   #xEA   #xFA   #xBE   #xA0   #xB6   #xB3   #x9D   #xDA   #x9B   #x8B   #xB7   #xB8   #xB9   #xAB 
    #| C_ |#     #x64   #x65   #x62   #x66   #x63   #x67   #x9E   #x68   #x74   #x71   #x72   #x73   #x78   #x75   #x76   #x77 
    #| D_ |#     #xAC   #x69   #xED   #xEE   #xEB   #xEF   #xEC   #xBF   #x80   #xFD   #xFE   #xFB   #xFC   #xAD   #xAE   #x59 
    #| E_ |#     #x44   #x45   #x42   #x46   #x43   #x47   #x9C   #x48   #x54   #x51   #x52   #x53   #x58   #x55   #x56   #x57 
    #| F_ |#     #x8C   #x49   #xCD   #xCE   #xCB   #xCF   #xCC   #xE1   #x70   #xDD   #xDE   #xDB   #xDC   #x8D   #x8E   #xDF 
    ))
    "Implements the Unicode->EBCDIC IBM CP 1146 code page.")


(defparameter *high-u2e-codepage-1146*
  (make-dict
   :initial-map
   '(
    (#x20AC #x009F) 
    ))
    "Unicode->EBCDIC IBM CP 1146 map for codepoints > #xff.")


(defparameter *codepage-1146*
  (make-codepage
   :id 1146
   :name "Codepage 1146"
   :e2u *e2u-codepage-1146*
   :u2e *u2e-codepage-1146*
   :high-u2e *high-u2e-codepage-1146*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 1146.")


;;;; cp-1146.lisp ends here.
