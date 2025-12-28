;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; cp-875.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-28:10:30:21

(in-package "CL3270")

;;; *e2u-codepage-875*
;;; *u2e-codepage-875*
;;;
;;; IBM CP 875 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-875*
  (make-array 256 :element-type 'rune
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20  #x391  #x392  #x393  #x394  #x395  #x396  #x397  #x398  #x399   #x5B   #x2E   #x3C   #x28   #x2B   #x21 
    #| 5_ |#     #x26  #x39A  #x39B  #x39C  #x39D  #x39E  #x39F  #x3A0  #x3A1  #x3A3   #x5D   #x24   #x2A   #x29   #x3B   #x5E 
    #| 6_ |#     #x2D   #x2F  #x3A4  #x3A5  #x3A6  #x3A7  #x3A8  #x3A9  #x3AA  #x3AB   #x7C   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#     #xA8  #x386  #x388  #x389   #xA0  #x38A  #x38C  #x38E  #x38F   #x60   #x3A   #x23   #x40   #x27   #x3D   #x22 
    #| 8_ |#    #x385   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69  #x3B1  #x3B2  #x3B3  #x3B4  #x3B5  #x3B6 
    #| 9_ |#     #xB0   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72  #x3B7  #x3B8  #x3B9  #x3BA  #x3BB  #x3BC 
    #| A_ |#     #xB4   #x7E   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A  #x3BD  #x3BE  #x3BF  #x3C0  #x3C1  #x3C3 
    #| B_ |#     #xA3  #x3AC  #x3AD  #x3AE  #x3CA  #x3AF  #x3CC  #x3CD  #x3CB  #x3CE  #x3C2  #x3C4  #x3C5  #x3C6  #x3C7  #x3C8 
    #| C_ |#     #x7B   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49   #xAD  #x3C9  #x390  #x3B0 #x2018 #x2015 
    #| D_ |#     #x7D   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52   #xB1   #xBD #xFFFD  #x387 #x2019   #xA6 
    #| E_ |#     #x5C #xFFFD   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A   #xB2   #xA7 #xFFFD #xFFFD   #xAB   #xAC 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39   #xB3   #xA9 #xFFFD #xFFFD   #xBB   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 875 code page.")


(defparameter *u2e-codepage-875*
  (make-array 256 :element-type 'octet
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x37   #x2D   #x2E   #x2F   #x16   #x05   #x25   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x3C   #x3D   #x32   #x26   #x18   #x19   #x3F   #x27   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x40   #x4F   #x7F   #x7B   #x5B   #x6C   #x50   #x7D   #x4D   #x5D   #x5C   #x4E   #x6B   #x60   #x4B   #x61 
    #| 3_ |#     #xF0   #xF1   #xF2   #xF3   #xF4   #xF5   #xF6   #xF7   #xF8   #xF9   #x7A   #x5E   #x4C   #x7E   #x6E   #x6F 
    #| 4_ |#     #x7C   #xC1   #xC2   #xC3   #xC4   #xC5   #xC6   #xC7   #xC8   #xC9   #xD1   #xD2   #xD3   #xD4   #xD5   #xD6 
    #| 5_ |#     #xD7   #xD8   #xD9   #xE2   #xE3   #xE4   #xE5   #xE6   #xE7   #xE8   #xE9   #x4A   #xE0   #x5A   #x5F   #x6D 
    #| 6_ |#     #x79   #x81   #x82   #x83   #x84   #x85   #x86   #x87   #x88   #x89   #x91   #x92   #x93   #x94   #x95   #x96 
    #| 7_ |#     #x97   #x98   #x99   #xA2   #xA3   #xA4   #xA5   #xA6   #xA7   #xA8   #xA9   #xC0   #x6A   #xD0   #xA1   #x07 
    #| 8_ |#     #x20   #x21   #x22   #x23   #x24   #x15   #x06   #x17   #x28   #x29   #x2A   #x2B   #x2C   #x09   #x0A   #x1B 
    #| 9_ |#     #x30   #x31   #x1A   #x33   #x34   #x35   #x36   #x08   #x38   #x39   #x3A   #x3B   #x04   #x14   #x3E   #xFF 
    #| A_ |#     #x74   #x3F   #x3F   #xB0   #x3F   #x3F   #xDF   #xEB   #x70   #xFB   #x3F   #xEE   #xEF   #xCA   #x3F   #x3F 
    #| B_ |#     #x90   #xDA   #xEA   #xFA   #xA0   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #xFE   #x3F   #xDB   #x3F   #x3F 
    #| C_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| D_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| E_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| F_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    ))
    "Implements the Unicode->EBCDIC IBM CP 875 code page.")


(defparameter *high-u2e-codepage-875*
  (make-dict
   :initial-map
   '(
    (#x0385 #x0080) (#x0386 #x0071) (#x0387 #x00DD) (#x0388 #x0072) 
    (#x0389 #x0073) (#x038A #x0075) (#x038C #x0076) (#x038E #x0077) 
    (#x038F #x0078) (#x0390 #x00CC) (#x0391 #x0041) (#x0392 #x0042) 
    (#x0393 #x0043) (#x0394 #x0044) (#x0395 #x0045) (#x0396 #x0046) 
    (#x0397 #x0047) (#x0398 #x0048) (#x0399 #x0049) (#x039A #x0051) 
    (#x039B #x0052) (#x039C #x0053) (#x039D #x0054) (#x039E #x0055) 
    (#x039F #x0056) (#x03A0 #x0057) (#x03A1 #x0058) (#x03A3 #x0059) 
    (#x03A4 #x0062) (#x03A5 #x0063) (#x03A6 #x0064) (#x03A7 #x0065) 
    (#x03A8 #x0066) (#x03A9 #x0067) (#x03AA #x0068) (#x03AB #x0069) 
    (#x03AC #x00B1) (#x03AD #x00B2) (#x03AE #x00B3) (#x03AF #x00B5) 
    (#x03B0 #x00CD) (#x03B1 #x008A) (#x03B2 #x008B) (#x03B3 #x008C) 
    (#x03B4 #x008D) (#x03B5 #x008E) (#x03B6 #x008F) (#x03B7 #x009A) 
    (#x03B8 #x009B) (#x03B9 #x009C) (#x03BA #x009D) (#x03BB #x009E) 
    (#x03BC #x009F) (#x03BD #x00AA) (#x03BE #x00AB) (#x03BF #x00AC) 
    (#x03C0 #x00AD) (#x03C1 #x00AE) (#x03C2 #x00BA) (#x03C3 #x00AF) 
    (#x03C4 #x00BB) (#x03C5 #x00BC) (#x03C6 #x00BD) (#x03C7 #x00BE) 
    (#x03C8 #x00BF) (#x03C9 #x00CB) (#x03CA #x00B4) (#x03CB #x00B8) 
    (#x03CC #x00B6) (#x03CD #x00B7) (#x03CE #x00B9) (#x2015 #x00CF) 
    (#x2018 #x00CE) (#x2019 #x00DE) 
    ))
    "Unicode->EBCDIC IBM CP 875 map for codepoints > #xff.")


(defparameter *codepage-875*
  (make-codepage
   :id 875
   :name "Codepage 875"
   :e2u *e2u-codepage-875*
   :u2e *u2e-codepage-875*
   :high-u2e *high-u2e-codepage-875*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 875.")


;;;; cp-875.lisp ends here.
