;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; cp-880.lisp

;;;; This file is part of CL3270
;;;;
;;;; See the file COPYING in the top folder for licensing and
;;;; copyright information.
;;;;
;;;; File generated on 2025-12-13:21:38:52

(in-package "CL3270")

;;; *e2u-codepage-880*
;;; *u2e-codepage-880*
;;;
;;; IBM CP 880 <-> Unicode mappings from `https://github.com/unicode-org/icu-data`.



(defparameter *e2u-codepage-880*
  (make-array 256 :element-type '(mod #x10000)
    :initial-contents '(
    #|             _0     _1     _2     _3     _4     _5     _6     _7     _8     _9     _A     _B     _C     _D     _E     _F |#
    #| 0_ |#     #x00   #x01   #x02   #x03   #x9C   #x09   #x86   #x7F   #x97   #x8D   #x8E   #x0B   #x0C   #x0D   #x0E   #x0F 
    #| 1_ |#     #x10   #x11   #x12   #x13   #x9D   #x85   #x08   #x87   #x18   #x19   #x92   #x8F   #x1C   #x1D   #x1E   #x1F 
    #| 2_ |#     #x80   #x81   #x82   #x83   #x84   #x0A   #x17   #x1B   #x88   #x89   #x8A   #x8B   #x8C   #x05   #x06   #x07 
    #| 3_ |#     #x90   #x91   #x16   #x93   #x94   #x95   #x96   #x04   #x98   #x99   #x9A   #x9B   #x14   #x15   #x9E   #x1A 
    #| 4_ |#     #x20   #xA0  #x452  #x453  #x451  #x454  #x455  #x456  #x457  #x458   #x5B   #x2E   #x3C   #x28   #x2B   #x21 
    #| 5_ |#     #x26  #x459  #x45A  #x45B  #x45C  #x45E  #x45F  #x42A #x2116  #x402   #x5D   #x24   #x2A   #x29   #x3B   #x5E 
    #| 6_ |#     #x2D   #x2F  #x403  #x401  #x404  #x405  #x406  #x407  #x408  #x409   #x7C   #x2C   #x25   #x5F   #x3E   #x3F 
    #| 7_ |#    #x40A  #x40B  #x40C   #xAD  #x40E  #x40F  #x44E  #x430  #x431   #x60   #x3A   #x23   #x40   #x27   #x3D   #x22 
    #| 8_ |#    #x446   #x61   #x62   #x63   #x64   #x65   #x66   #x67   #x68   #x69  #x434  #x435  #x444  #x433  #x445  #x438 
    #| 9_ |#    #x439   #x6A   #x6B   #x6C   #x6D   #x6E   #x6F   #x70   #x71   #x72  #x43A  #x43B  #x43C  #x43D  #x43E  #x43F 
    #| A_ |#    #x44F   #x7E   #x73   #x74   #x75   #x76   #x77   #x78   #x79   #x7A  #x440  #x441  #x442  #x443  #x436  #x432 
    #| B_ |#    #x44C  #x44B  #x437  #x448  #x44D  #x449  #x447  #x44A  #x42E  #x410  #x411  #x426  #x414  #x415  #x424  #x413 
    #| C_ |#     #x7B   #x41   #x42   #x43   #x44   #x45   #x46   #x47   #x48   #x49  #x425  #x418  #x419  #x41A  #x41B  #x41C 
    #| D_ |#     #x7D   #x4A   #x4B   #x4C   #x4D   #x4E   #x4F   #x50   #x51   #x52  #x41D  #x41E  #x41F  #x42F  #x420  #x421 
    #| E_ |#     #x5C   #xA4   #x53   #x54   #x55   #x56   #x57   #x58   #x59   #x5A  #x422  #x423  #x416  #x412  #x42C  #x42B 
    #| F_ |#     #x30   #x31   #x32   #x33   #x34   #x35   #x36   #x37   #x38   #x39  #x417  #x428  #x42D  #x429  #x427   #x9F 
    ))
    "Implements the EBCDIC->Unicode IBM CP 880 code page.")


(defparameter *u2e-codepage-880*
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
    #| A_ |#     #x41   #x3F   #x3F   #x3F   #xE1   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x73   #x3F   #x3F 
    #| B_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| C_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| D_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| E_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    #| F_ |#     #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F   #x3F 
    ))
    "Implements the Unicode->EBCDIC IBM CP 880 code page.")


(defparameter *high-u2e-codepage-880*
  (make-dict
   :initial-map
   '(
    (#x0401 #x0063) (#x0402 #x0059) (#x0403 #x0062) (#x0404 #x0064) 
    (#x0405 #x0065) (#x0406 #x0066) (#x0407 #x0067) (#x0408 #x0068) 
    (#x0409 #x0069) (#x040A #x0070) (#x040B #x0071) (#x040C #x0072) 
    (#x040E #x0074) (#x040F #x0075) (#x0410 #x00B9) (#x0411 #x00BA) 
    (#x0412 #x00ED) (#x0413 #x00BF) (#x0414 #x00BC) (#x0415 #x00BD) 
    (#x0416 #x00EC) (#x0417 #x00FA) (#x0418 #x00CB) (#x0419 #x00CC) 
    (#x041A #x00CD) (#x041B #x00CE) (#x041C #x00CF) (#x041D #x00DA) 
    (#x041E #x00DB) (#x041F #x00DC) (#x0420 #x00DE) (#x0421 #x00DF) 
    (#x0422 #x00EA) (#x0423 #x00EB) (#x0424 #x00BE) (#x0425 #x00CA) 
    (#x0426 #x00BB) (#x0427 #x00FE) (#x0428 #x00FB) (#x0429 #x00FD) 
    (#x042A #x0057) (#x042B #x00EF) (#x042C #x00EE) (#x042D #x00FC) 
    (#x042E #x00B8) (#x042F #x00DD) (#x0430 #x0077) (#x0431 #x0078) 
    (#x0432 #x00AF) (#x0433 #x008D) (#x0434 #x008A) (#x0435 #x008B) 
    (#x0436 #x00AE) (#x0437 #x00B2) (#x0438 #x008F) (#x0439 #x0090) 
    (#x043A #x009A) (#x043B #x009B) (#x043C #x009C) (#x043D #x009D) 
    (#x043E #x009E) (#x043F #x009F) (#x0440 #x00AA) (#x0441 #x00AB) 
    (#x0442 #x00AC) (#x0443 #x00AD) (#x0444 #x008C) (#x0445 #x008E) 
    (#x0446 #x0080) (#x0447 #x00B6) (#x0448 #x00B3) (#x0449 #x00B5) 
    (#x044A #x00B7) (#x044B #x00B1) (#x044C #x00B0) (#x044D #x00B4) 
    (#x044E #x0076) (#x044F #x00A0) (#x0451 #x0044) (#x0452 #x0042) 
    (#x0453 #x0043) (#x0454 #x0045) (#x0455 #x0046) (#x0456 #x0047) 
    (#x0457 #x0048) (#x0458 #x0049) (#x0459 #x0051) (#x045A #x0052) 
    (#x045B #x0053) (#x045C #x0054) (#x045E #x0055) (#x045F #x0056) 
    (#x2116 #x0058) 
    ))
    "Unicode->EBCDIC IBM CP 880 map for codepoints > #xff.")


(defparameter *codepage-880*
  (make-codepage
   :id 880
   :name "Codepage 880"
   :e2u *e2u-codepage-880*
   :u2e *u2e-codepage-880*
   :high-u2e *high-u2e-codepage-880*
   :esub #x3f
   :ge #x08
   :ge2u *cp310-to-unicode*
   :u2ge *unicode-to-cp310*
   )
  "Codepage 880.")


;;;; cp-880.lisp ends here.
