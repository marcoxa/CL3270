;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; cp-1160.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-13:21:38:52

(in-package "CL3270")

;;; *e2u-codepage-1160*
;;; *u2e-codepage-1160*
;;;
;;; IBM CP 1160 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-1160*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20   #xA0  #xE01  #xE02  #xE03  #xE04  #xE05  #xE06  #xE07   #x5B   #xA2   #x2E   #x3C   #x28   #x2B   #x7C 
    #| 5_ |#     #x26 #xFFFD  #xE08  #xE09  #xE0A  #xE0B  #xE0C  #xE0D  #xE0E   #x5D   #x21   #x24   #x2A   #x29   #x3B   #xAC 
    #| 6_ |#     #x2D   #x2F  #xE0F  #xE10  #xE11  #xE12  #xE13  #xE14  #xE15   #x5E   #xA6   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#    #xE3F  #xE4E  #xE16  #xE17  #xE18  #xE19  #xE1A  #xE1B  #xE1C   #x60   #x3A   #x23   #x40   #x27   #x3D   #x22 
    #| 8_ |#    #xE4F   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69  #xE1D  #xE1E  #xE1F  #xE20  #xE21  #xE22 
    #| 9_ |#    #xE5A   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72  #xE23  #xE24  #xE25  #xE26  #xE27  #xE28 
    #| A_ |#    #xE5B   #x7E   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A  #xE29  #xE2A  #xE2B  #xE2C  #xE2D  #xE2E 
    #| B_ |#    #xE50  #xE51  #xE52  #xE53  #xE54  #xE55  #xE56  #xE57  #xE58  #xE59  #xE2F  #xE30  #xE31  #xE32  #xE33  #xE34 
    #| C_ |#     #x7B   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49 #xFFFD  #xE35  #xE36  #xE37  #xE38  #xE39 
    #| D_ |#     #x7D   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52  #xE3A  #xE40  #xE41  #xE42  #xE43  #xE44 
    #| E_ |#     #x5C #xFFFD   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A  #xE45  #xE46  #xE47  #xE48  #xE49  #xE4A 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39 #xFFFD  #xE4C  #xE4D  #xE4B #x20AC   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 1160 code page.")


(defparameter *u2e-codepage-1160*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x37   #x2D   #x2E   #x2F   #x16   #x05   #x25   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x3C   #x3D   #x32   #x26   #x18   #x19   #x3F   #x27   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x40   #x5A   #x7F   #x7B   #x5B   #x6C   #x50   #x7D   #x4D   #x5D   #x5C   #x4E   #x6B   #x60   #x4B   #x61 
    #| 3_ |#     #xF0   #xF1   #xF2   #xF3   #xF4   #xF5   #xF6   #xF7   #xF8   #xF9   #x7A   #x5E   #x4C   #x7E   #x6E   #x6F 
    #| 4_ |#     #x7C   #xC1   #xC2   #xC3   #xC4   #xC5   #xC6   #xC7   #xC8   #xC9   #xD1   #xD2   #xD3   #xD4   #xD5   #xD6 
    #| 5_ |#     #xD7   #xD8   #xD9   #xE2   #xE3   #xE4   #xE5   #xE6   #xE7   #xE8   #xE9   #x49   #xE0   #x59   #x69   #x6D 
    #| 6_ |#     #x79   #x81   #x82   #x83   #x84   #x85   #x86   #x87   #x88   #x89   #x91   #x92   #x93   #x94   #x95   #x96 
    #| 7_ |#     #x97   #x98   #x99   #xA2   #xA3   #xA4   #xA5   #xA6   #xA7   #xA8   #xA9   #xC0   #x4F   #xD0   #xA1   #x07 
    #| 8_ |#     #x20   #x21   #x22   #x23   #x24   #x15   #x06   #x17   #x28   #x29   #x2A   #x2B   #x2C   #x09   #x0A   #x1B 
    #| 9_ |#     #x30   #x31   #x1A   #x33   #x34   #x35   #x36   #x08   #x38   #x39   #x3A   #x3B   #x04   #x14   #x3E   #xFF 
    #| A_ |#     #x41   #x3F   #x4A   #x3F   #x3F   #x3F   #x6A   #x3F   #x3F   #x3F   #x3F   #x3F   #x5F   #x3F   #x3F   #x3F 
    #| B_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| C_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| D_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| E_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| F_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    ))
    "Implements the Unicode->EBCDIC IBM CP 1160 code page.")


(defparameter *high-u2e-codepage-1160*
  (make-dict
   :initial-map
   '(
    (#x0E39 #x00CF) (#x0E3A #x00DA) (#x0E3F #x0070) (#x0E40 #x00DB) 
    (#x0E41 #x00DC) (#x0E42 #x00DD) (#x0E43 #x00DE) (#x0E44 #x00DF) 
    (#x0E45 #x00EA) (#x0E46 #x00EB) (#x0E47 #x00EC) (#x0E48 #x00ED) 
    (#x0E49 #x00EE) (#x0E4A #x00EF) (#x0E4B #x00FD) (#x0E4C #x00FB) 
    (#x0E4D #x00FC) (#x0E4E #x0071) (#x0E4F #x0080) (#x0E50 #x00B0) 
    (#x0E51 #x00B1) (#x0E52 #x00B2) (#x0E53 #x00B3) (#x0E54 #x00B4) 
    (#x0E55 #x00B5) (#x0E56 #x00B6) (#x0E57 #x00B7) (#x0E58 #x00B8) 
    (#x0E59 #x00B9) (#x0E5A #x0090) (#x0E5B #x00A0) (#x20AC #x00FE) 
    (#x0E01 #x0042) (#x0E02 #x0043) (#x0E03 #x0044) (#x0E04 #x0045) 
    (#x0E05 #x0046) (#x0E06 #x0047) (#x0E07 #x0048) (#x0E08 #x0052) 
    (#x0E09 #x0053) (#x0E0A #x0054) (#x0E0B #x0055) (#x0E0C #x0056) 
    (#x0E0D #x0057) (#x0E0E #x0058) (#x0E0F #x0062) (#x0E10 #x0063) 
    (#x0E11 #x0064) (#x0E12 #x0065) (#x0E13 #x0066) (#x0E14 #x0067) 
    (#x0E15 #x0068) (#x0E16 #x0072) (#x0E17 #x0073) (#x0E18 #x0074) 
    (#x0E19 #x0075) (#x0E1A #x0076) (#x0E1B #x0077) (#x0E1C #x0078) 
    (#x0E1D #x008A) (#x0E1E #x008B) (#x0E1F #x008C) (#x0E20 #x008D) 
    (#x0E21 #x008E) (#x0E22 #x008F) (#x0E23 #x009A) (#x0E24 #x009B) 
    (#x0E25 #x009C) (#x0E26 #x009D) (#x0E27 #x009E) (#x0E28 #x009F) 
    (#x0E29 #x00AA) (#x0E2A #x00AB) (#x0E2B #x00AC) (#x0E2C #x00AD) 
    (#x0E2D #x00AE) (#x0E2E #x00AF) (#x0E2F #x00BA) (#x0E30 #x00BB) 
    (#x0E31 #x00BC) (#x0E32 #x00BD) (#x0E33 #x00BE) (#x0E34 #x00BF) 
    (#x0E35 #x00CB) (#x0E36 #x00CC) (#x0E37 #x00CD) (#x0E38 #x00CE) 
    ))
    "Unicode->EBCDIC IBM CP 1160 map for codepoints > #xff.")


(defparameter *codepage-1160*
  (make-codepage
   :id 1160
   :name "Codepage 1160"
   :e2u *e2u-codepage-1160*
   :u2e *u2e-codepage-1160*
   :high-u2e *high-u2e-codepage-1160*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 1160.")


;;;; cp-1160.lisp ends here.
