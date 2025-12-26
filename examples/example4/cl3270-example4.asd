;;;; -*- Mode: Lisp -*-

;;;; cl3270-example4.asd
;;;;
;;;; Minimal 3720 data stream emulation.
;;;;
;;;; See the file COPYING in the top directory for copyright and
;;;; licensing information.

(asdf:defsystem "cl3270-example4"
  :author "Marco Antoniotti"
  :licence "MIT"
  :description "The CL3270 system.

The CL3270 Example 4 implements a 3270 'application' of a mock database.

Again, the inspiration (and source material) is Matthew Wilson's GO library
which can be found at
[go3720](https://pkg.go.dev/github.com/racingmars/go3270)."

  :components ((:file "cl3270-example4-pkg")
               (:file "example4" :depends-on ("cl3270-example4-pkg"))
               (:file "database" :depends-on ("cl3270-example4-pkg"))
               (:file "login" :depends-on ("cl3270-example4-pkg"))
               (:file "mainmenu" :depends-on ("cl3270-example4-pkg"))
               (:file "help" :depends-on ("cl3270-example4-pkg"))
               )
  :depends-on ("cl3270")
  )

;;;; end of file -- cl3270-example4.asd
