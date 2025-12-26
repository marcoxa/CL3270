;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; cp-870.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-13:21:38:52

(in-package "CL3270")

;;; *e2u-codepage-870*
;;; *u2e-codepage-870*
;;;
;;; IBM CP 870 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-870*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20   #xA0   #xE2   #xE4  #x163   #xE1  #x103  #x10D   #xE7  #x107   #x5B   #x2E   #x3C   #x28   #x2B   #x21 
    #| 5_ |#     #x26   #xE9  #x119   #xEB  #x16F   #xED   #xEE  #x13E  #x13A   #xDF   #x5D   #x24   #x2A   #x29   #x3B   #x5E 
    #| 6_ |#     #x2D   #x2F   #xC2   #xC4  #x2DD   #xC1  #x102  #x10C   #xC7  #x106   #x7C   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#    #x2C7   #xC9  #x118   #xCB  #x16E   #xCD   #xCE  #x13D  #x139   #x60   #x3A   #x23   #x40   #x27   #x3D   #x22 
    #| 8_ |#    #x2D8   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69  #x15B  #x148  #x111   #xFD  #x159  #x15F 
    #| 9_ |#     #xB0   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72  #x142  #x144  #x161   #xB8  #x2DB   #xA4 
    #| A_ |#    #x105   #x7E   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A  #x15A  #x147  #x110   #xDD  #x158  #x15E 
    #| B_ |#    #x2D9  #x104  #x17C  #x162  #x17B   #xA7  #x17E  #x17A  #x17D  #x179  #x141  #x143  #x160   #xA8   #xB4   #xD7 
    #| C_ |#     #x7B   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49   #xAD   #xF4   #xF6  #x155   #xF3  #x151 
    #| D_ |#     #x7D   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52  #x11A  #x171   #xFC  #x165   #xFA  #x11B 
    #| E_ |#     #x5C   #xF7   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A  #x10F   #xD4   #xD6  #x154   #xD3  #x150 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39  #x10E  #x170   #xDC  #x164   #xDA   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 870 code page.")


(defparameter *u2e-codepage-870*
  (make-array 256 :element-type '(mod #x10000)
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
    #| A_ |#     #x41   #x3F   #x3F   #x3F   #x9F   #x3F   #x3F   #xB5   #xBD   #x3F   #x3F   #x3F   #x3F   #xCA   #x3F   #x3F 
    #| B_ |#     #x90   #x3F   #x3F   #x3F   #xBE   #x3F   #x3F   #x3F   #x9D   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| C_ |#     #x3F   #x65   #x62   #x3F   #x63   #x3F   #x3F   #x68   #x3F   #x71   #x3F   #x73   #x3F   #x75   #x76   #x3F 
    #| D_ |#     #x3F   #x3F   #x3F   #xEE   #xEB   #x3F   #xEC   #xBF   #x3F   #x3F   #xFE   #x3F   #xFC   #xAD   #x3F   #x59 
    #| E_ |#     #x3F   #x45   #x42   #x3F   #x43   #x3F   #x3F   #x48   #x3F   #x51   #x3F   #x53   #x3F   #x55   #x56   #x3F 
    #| F_ |#     #x3F   #x3F   #x3F   #xCE   #xCB   #x3F   #xCC   #xE1   #x3F   #x3F   #xDE   #x3F   #xDC   #x8D   #x3F   #x3F 
    ))
    "Implements the Unicode->EBCDIC IBM CP 870 code page.")


(defparameter *high-u2e-codepage-870*
  (make-dict
   :initial-map
   '(
    (#x0150 #x00EF) (#x0151 #x00CF) (#x0154 #x00ED) (#x0155 #x00CD) 
    (#x0158 #x00AE) (#x0159 #x008E) (#x015A #x00AA) (#x015B #x008A) 
    (#x015E #x00AF) (#x015F #x008F) (#x0160 #x00BC) (#x0161 #x009C) 
    (#x0162 #x00B3) (#x0163 #x0044) (#x0164 #x00FD) (#x0165 #x00DD) 
    (#x016E #x0074) (#x016F #x0054) (#x0170 #x00FB) (#x0171 #x00DB) 
    (#x0179 #x00B9) (#x017A #x00B7) (#x017B #x00B4) (#x02C7 #x0070) 
    (#x017C #x00B2) (#x017D #x00B8) (#x017E #x00B6) (#x02D8 #x0080) 
    (#x02D9 #x00B0) (#x02DB #x009E) (#x02DD #x0064) (#x0102 #x0066) 
    (#x0103 #x0046) (#x0104 #x00B1) (#x0105 #x00A0) (#x0106 #x0069) 
    (#x0107 #x0049) (#x010C #x0067) (#x010D #x0047) (#x010E #x00FA) 
    (#x010F #x00EA) (#x0110 #x00AC) (#x0111 #x008C) (#x0118 #x0072) 
    (#x0119 #x0052) (#x011A #x00DA) (#x011B #x00DF) (#x0139 #x0078) 
    (#x013A #x0058) (#x013D #x0077) (#x013E #x0057) (#x0141 #x00BA) 
    (#x0142 #x009A) (#x0143 #x00BB) (#x0144 #x009B) (#x0147 #x00AB) 
    (#x0148 #x008B) 
    ))
    "Unicode->EBCDIC IBM CP 870 map for codepoints > #xff.")


(defparameter *codepage-870*
  (make-codepage
   :id 870
   :name "Codepage 870"
   :e2u *e2u-codepage-870*
   :u2e *u2e-codepage-870*
   :high-u2e *high-u2e-codepage-870*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 870.")


;;;; cp-870.lisp ends here.
