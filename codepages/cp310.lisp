;;;; -*- Mode: Lisp; Coding: UTF-8 -*-

;;;; cp310.lisp
;;;;
;;;; Special provisions for the "graphic escape" Codepage CP 310.
;;;; 
;;;; See the COPYING file in the main folder for licensing and
;;;; copyright information.

(in-package "CL3270")


;;; Certain characters are supported in the "graphic escape" CP310. These are
;;; arbitrary Unicode code points, so we will look them up via a map. For
;;; simplicity of our mapping implementation, we will not support the italic
;;; underlined A-Z characters that require combining characters.
;;;
;;; We will share this map among all of the codepages that we provide
;;; implementations for.
;;;
;;; https://public.dhe.ibm.com/software/globalization/gcoc/attachments/CP00310.pdf


(defparameter *unicode-to-cp310*
  (make-dict :initial-map
             '((#\◊ 0x70) (#\⋄ 0x70) (#\◆ 0x70) (#\∧ 0x71) (#\⋀ 0x71) (#\¨ 0x72)
               (#\⌻ 0x73) (#\⍸ 0x74) (#\⍷ 0x75) (#\⊢ 0x76) (#\⊣ 0x77) (#\∨ 0x78)
               (#\∼ 0x80) (#\║ 0x81) (#\═ 0x82) (#\⎸ 0x83) (#\⎹ 0x84) (#\│ 0x85)
               (#\⎥ 0x85) (#\↑ 0x8A) (#\↓ 0x8B) (#\≤ 0x8C) (#\⌈ 0x8D) (#\⌊ 0x8E)
               (#\→ 0x8F) (#\⎕ 0x90) (#\▌ 0x91) (#\▐ 0x92) (#\▀ 0x93) (#\▄ 0x94)
               (#\█ 0x95) (#\⊃ 0x9A) (#\⊂ 0x9B) (#\⌑ 0x9C) (#\¤ 0x9C) (#\○ 0x9D)
               (#\± 0x9E) (#\← 0x9F) (#\¯ 0xA0) (#\‾ 0xA0) (#\° 0xA1) (#\─ 0xA2)
               (#\∙ 0xA3) (#\• 0xA3) (#\ₙ 0xA4) (#\∩ 0xAA) (#\⋂ 0xAA) (#\∪ 0xAB)
               (#\⋃ 0xAB) (#\⊥ 0xAC) (#\≥ 0xAE) (#\∘ 0xAF) (#\⍺ 0xB0) (#\α 0xB0)
               (#\∊ 0xB1) (#\∈ 0xB1) (#\ε 0xB1) (#\⍳ 0xB2) (#\ι 0xB2) (#\⍴ 0xB3)
               (#\ρ 0xB3) (#\⍵ 0xB4) (#\ω 0xB4) (#\× 0xB6) (#\∖ 0xB7) (#\÷ 0xB8)
               (#\∇ 0xBA) (#\∆ 0xBB) (#\⊤ 0xBC) (#\≠ 0xBE) (#\∣ 0xBF) (#\⁽ 0xC1)
               (#\⁺ 0xC2) (#\■ 0xC3) (#\∎ 0xC3) (#\└ 0xC4) (#\┌ 0xC5) (#\├ 0xC6)
               (#\┴ 0xC7) (#\⍲ 0xCA) (#\⍱ 0xCB) (#\⌷ 0xCC) (#\⌽ 0xCD) (#\⍂ 0xCE)
               (#\⍉ 0xCF) (#\⁾ 0xD1) (#\⁻ 0xD2) (#\┼ 0xD3) (#\┘ 0xD4) (#\┐ 0xD5)
               (#\┤ 0xD6) (#\┬ 0xD7) (#\¶ 0xD8) (#\⌶ 0xDA) (#\ǃ 0xDB) (#\⍒ 0xDC)
               (#\⍋ 0xDD) (#\⍞ 0xDE) (#\⍝ 0xDF) (#\≡ 0xE0) (#\₁ 0xE1) (#\₂ 0xE2)
               (#\₃ 0xE3) (#\⍤ 0xE4) (#\⍥ 0xE5) (#\⍪ 0xE6) (#\€ 0xE7) (#\⌿ 0xEA)
               (#\⍀ 0xEB) (#\∵ 0xEC) (#\⊖ 0xED) (#\⌹ 0xEE) (#\⍕ 0xEF) (#\⁰ 0xF0)
               (#\¹ 0xF1) (#\² 0xF2) (#\³ 0xF3) (#\⁴ 0xF4) (#\⁵ 0xF5) (#\⁶ 0xF6)
               (#\⁷ 0xF7) (#\⁸ 0xF8) (#\⁹ 0xF9) (#\⍫ 0xFB) (#\⍙ 0xFC) (#\⍟ 0xFD)
               (#\⍎ 0xFE)
               )))


(defparameter *cp310-to-unicode*
  (make-array
   256
   :element-type 'character
   :initial-contents
   '(
     #|            x0      x1      x2      x3      x4      x5      x6      x7      x8      x9      xA      xB      xC      xD      xE      xF |#
     #| 0x |# #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 1x |# #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 2x |# #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 3x |# #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 4x |# #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 5x |# #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 6x |# #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 7x |# #\◊     #\∧     #\¨     #\⌻     #\⍸    #\⍷     #\⊢     #\⊣    #\∨     #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd #\ufffd
     #| 8x |# #\∼     #\║     #\═     #\⎸      #\⎹     #\⎥     #\ufffd #\ufffd #\ufffd  #\ufffd #\↑     #\↓     #\≤     #\⌈     #\⌊    #\→
     #| 9x |# #\⎕     #\▌     #\▐     #\▀     #\▄    #\█      #\ufffd #\ufffd #\ufffd #\ufffd #\⊃     #\⊂     #\⌑    #\○      #\±    #\←
     #| Ax |# #\‾     #\°     #\─     #\•     #\ₙ    #\ufffd  #\ufffd #\ufffd #\ufffd #\ufffd #\∩     #\⋃     #\⊥     #\ufffd #\≥    #\∘
     #| Bx |# #\⍺     #\∈     #\⍳     #\⍴    #\ω    #\ufffd  #\×     #\∖     #\÷     #\ufffd #\∇     #\∆     #\⊤     #\ufffd #\≠    #\∣
     #| Cx |# #\ufffd #\⁽     #\⁺     #\■    #\└    #\┌      #\├     #\┴     #\ufffd #\ufffd  #\⍲      #\⍱      #\⌷     #\⌽     #\⍂    #\⍉
     #| Dx |# #\ufffd #\⁾     #\⁻     #\┼     #\┘    #\┐     #\┤     #\┬     #\¶      #\ufffd #\⌶      #\ǃ     #\⍒     #\⍋     #\⍞    #\⍝
     #| Ex |# #\≡     #\₁     #\₂     #\₃     #\⍤    #\⍥     #\⍪      #\€     #\ufffd  #\ufffd #\⌿      #\⍀      #\∵     #\⊖     #\⌹    #\⍕
     #| Fx |# #\⁰     #\¹     #\²     #\³ #\⁴     #\⁵    #\⁶     #\⁷     #\⁸     #\⁹      #\ufffd #\⍫     #\⍙     #\⍟     #\⍎      #\ufffd
     )
   ))


;;;; Epilogue.

(declaim (type dict *unicode-to-cp310*)
         (type (vector character 256) *cp310-to-unicode*))

;;;; cp310.lisp ends here.
