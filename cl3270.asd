;;;; -*- Mode: Lisp -*-

;;;; cl3270.asd

(asdf:defsystem "cl3270"
  :author "Marco Antoniotti"
  :components ((:file "cl3270-pkg")
               (:file "bytes"  :depends-on ("cl3270-pkg"))
               (:file "ebcdic-ascii" :depends-on ("cl3270-pkg" "bytes"))
               (:file "util"  :depends-on ("cl3270-pkg"))
               (:file "debug" :depends-on ("cl3270-pkg"))
               (:file "telnet" :depends-on ("ebcdic-ascii" "bytes" "util" "debug"))
               (:file "response" :depends-on ("ebcdic-ascii" "telnet"))
               (:file "screen" :depends-on ("telnet" "response"))
               (:file "looper" :depends-on ("screen"))
               )
  :depends-on ("usocket" "usocket-server")
  )

;;;; end of file -- cl3270.asd
