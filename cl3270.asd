;;;; -*- Mode: Lisp -*-

;;;; cl3270.asd
;;;;
;;;; Minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING for copyright and licensing information.

(asdf:defsystem "cl3270"
  :author "Marco Antoniotti"
  :license "MIT"
  :description "The CL3270 system.

The CL3270 implements a 3270 data stream handling/emulation.

THe inspiration (and source material) is Matthew Wilson's GO library
which can be found at
[go3720](https://pkg.go.dev/github.com/racingmars/go3270)."

  :components ((:file "cl3270-pkg")
               (:file "setup"    :depends-on ("cl3270-pkg"))
               (:file "debug"    :depends-on ("cl3270-pkg" "setup"))

               (:file "bytes"    :depends-on ("cl3270-pkg" "setup" "debug"))
               (:file "util"     :depends-on ("cl3270-pkg" "setup" "debug"))

               (:file "codepage" :depends-on ("bytes" "util"))

               ;; (:file "ebcdic-ascii" :depends-on ("cl3270-pkg" "bytes"))
               (:file "ebcdic"   :depends-on ("bytes" "codepage" "codepages"))

               (:file "device"   :depends-on ("cl3270-pkg" "setup" "debug"))

               (:file "codes"    :depends-on ("cl3270-pkg" "setup" "debug"))

               (:file "telnet"
                :depends-on ("ebcdic" "codes" "device" "bytes" "util" "debug")
                )

               (:file "response" :depends-on ("ebcdic" "codes" "telnet"))
               (:file "screen"   :depends-on ("telnet" "response"))
               (:file "looper"   :depends-on ("screen"))
               (:file "transactions" :depends-on ("device"))
               (:module "codepages"
                :components ((:file "cp310")
                             (:file "cpbracket" :depends-on ("cp310"))
                             (:file "generate"  :depends-on ("cp310"))
                             (:file "setup"     :depends-on ("generate" "cps"))
                             (:module "cps"     :depends-on ("cp310")) ; Yep.  Quite fake.
                             )
                :depends-on ("codepage"))
               )
  :depends-on ("usocket" "usocket-server")
  )

;;;; end of file -- cl3270.asd
