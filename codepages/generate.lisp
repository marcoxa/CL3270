;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; generate.lisp
;;;;
;;;; Generate codepage codes from Unicode icu-data UCM format files.
;;;;
;;;; This file is part of CL3270.
;;;; Based on Matthew R. Wilson's GO code, which is licensed under the
;;;; MIT license.
;;;;
;;;; See the COPYING file in the main folder for licensing and
;;;; copyright information.

(in-package "CL3270")


;;; ucm-process-file
;;;
;;; Notes:
;;;
;;; 2025-12-04: MA: This is essentially the `read` function in Matthew R.
;;; Wilson's `generate` GO code.

(defun ucm-process-file (ucm-filename &aux (ucm-file (pathname ucm-filename)))
  "Process a UCM file and returns a map of of Unicode code points to EBCDIC."

  (declare (type (or string pathname) ucm-filename)
           (type pathname ucm-file))

  ;; UCM files are not that big.  I just read in a list of "lines" and
  ;; then parse them.  Yep.  No standard stream "line reader" in Common Lisp.

  (let ((ucm-raw-lines (ucm-read-lines ucm-file))
        (in-char-map nil)
        (u2e (make-hash-table :test #'equal))
        )

    (declare (type list ucm-raw-lines)
             (type boolean in-char-map)
             (type hash-table u2e))

    (dolist (line ucm-raw-lines)
      (cond ((or (string= "" line) (char= #\# (char line 0)))
             ;; Do nothing.
             )

            ((and (not in-char-map) (string-not-equal line "CHARMAP"))
             ;; Do nothing.
             )

            ((string-equal line "CHARMAP")
             (setq in-char-map t))

            ((string-equal "|1" line :start2 (- (length line) 2))
             ;; Do nothing.  Really skip non-roundtrip characters.
             )

            ((string-equal "END CHARMAP" line)
             (return))

            (t (multiple-value-bind (code-point ebcdic)
                   (parse-ucm-line line)
                 (multiple-value-bind (e foundp)
                     (gethash code-point u2e)
                   (when foundp
                     (warn "CL3270: duplicate code-point #\\U~4,'0X (#x~2,'0X) in '~A.~A'~%"
                           code-point
                           e
                           (pathname-name ucm-file)
                           (pathname-type ucm-file))))
                 (setf (gethash code-point u2e) ebcdic)))
            ))

    (values u2e #| ucm-raw-lines |#)
    ))


(defun parse-ucm-line (line)
  "Parse a LINE according to the UCM format.

Return, as two values, the two codes appearing on LINE."

  ;; Matthew R. Wilson's code uses GO regexps.  I want to avoid
  ;; dependencies, plus, after all, the UCM lines look like
  ;;
  ;; <UXXX> \xXX(\xXX\)? |D
  ;;
  ;; with 'X' a hexadecimal digit and D = 0, 1, 2, 3.

  (declare (type string line))

  (let ((u-code (subseq line 2 6))
        (\\-pos-1 (position #\\ line :start 6))
        (\\-pos-2 (position #\\ line :start 9))
        )

    (declare (type string u-code)
             (type (or null (mod 80)) \\-pos-1 \\-pos-2)) ; 80 is the
                                                          ; assumed
                                                          ; line
                                                          ; length.

    (unless \\-pos-1
      (error "CL3270: no hex found; UCM line = ~S." line))

    (when \\-pos-2
      (error "CL3270: cannot handle 16 bits codes; UCM line = ~S." line))
    
    (let ((u (parse-integer u-code :radix 16))
          (x (parse-integer line
                            :radix 16
                            :start (+ \\-pos-1 2) ; Lose the initial '\x'.
                            :junk-allowed t))
          )
      (values u x)
      )))


;;; *code-pages-dir*

(defparameter *code-pages-dir*
  (merge-pathnames (make-pathname :directory '(:relative "codepages")
                                  :name nil
                                  :type nil)
                   *cl3270-source-location*)
  "The location of the 'codepages' sub module.")


;;; *code-pages-ucm-dir*

(defparameter *code-pages-ucm-dir*
  (merge-pathnames
   (make-pathname :directory '(:relative "icu-data" "charset" "data" "ucm")
                  :name nil
                  :type nil)
   *code-pages-dir*)
  "The (relative) directory where the ICU UCM files reside.")


;;; *ibm-icu-code-page-filenames*
;;;
;;; This file contains a manually curated list of IBM code page UCM
;;; files, taken from *CODE-PAGES-UCM-DIR*.

(defparameter *ibm-icu-code-page-filenames*
  (make-pathname :name "icu-data-ibm-ucm-files" :type "txt"
                 :defaults *code-pages-dir*
                 ))


;;; *ibm-icu-code-page-local-dir*
;;;
;;; This directory contains a manually curated set of IBM code page
;;; UCM files, taken from *CODE-PAGES-UCM-DIR*.

(defparameter *ibm-icu-code-page-local-dir*
  (merge-pathnames (make-pathname :directory '(:relative "icu-data-ibm")
                                  :name nil
                                  :type nil)
                   *code-pages-dir*))


(defparameter *cl3270-code-page-local-dir*
  (merge-pathnames (make-pathname :directory '(:relative "cps")
                                  :name nil
                                  :type nil)
                   *code-pages-dir*)
  "The directory where the generated Common Lisp code pages reside.")


;;; clean-code-page-local-dir

(defun clean-code-page-local-dir (&optional
                                  (cps-dir *cl3270-code-page-local-dir*))
  "Delete (generated) codepage files from CPS-DIR.

CPS-DIR defaults to *CL3270-CODE-PAGE-LOCAL-DIR*."

  (dolist (cpf (directory cps-dir))
    (format t ";;; CL3270: deleting CL codepage '~A.~A'.~%"
            (pathname-name cpf)
            (pathname-type cpf))
    (delete-file cpf)))


;;; *ibm-x3270-icu-code-page-codes*

(defparameter *ibm-x3270-icu-code-page-codes*
  (list 37 273 275 277 278 280 284 285 297 424 500 803 870 871 875 880
        1026 1047 1140 1141 1142 1143 1144 1145 1146 1147 1148
        1149 1160) ; Matthew R. Wilson's Discord post list.
  
  "Code page ids of files 'ibm-\\([0-9]+\\)_*.ucm' in *CODE-PAGES-UCM-DIR*.

These are the code pages recognized by x3270.  Eventually a few more
will be added according to what Matthew R. Wilson does with his GO
library.")



(defparameter *cl3270-code-page-pathnames*
  (mapcar #'(lambda (cpn)
              (make-pathname :name (format nil "cp-~D" cpn)
                             :type "lisp"
                             :defaults *cl3270-code-page-local-dir*))
          *ibm-x3270-icu-code-page-codes*)
  "List of CL code page pathnames.")
              


(defun select-codepage-file (cp-id
                             &optional
                             (cp-ucm-dir *ibm-icu-code-page-local-dir*))
  (find-if #'(lambda (f)
               (search (format nil "ibm-~D" cp-id) (pathname-name f)))
           (directory cp-ucm-dir)))


(defun select-codepage-files (&optional
                              (cp-ids *ibm-x3270-icu-code-page-codes*)
                              (cp-ucm-dir *ibm-icu-code-page-local-dir*)
                              )
  (flet ((select-cp-file (cp-id file-name)
           (search (format nil "ibm-~D" cp-id) (pathname-name file-name))))

    (loop with ibm-data-ucm-files = (directory cp-ucm-dir)
          for cp-id in cp-ids
          for ibm-data-ucm-file =
            (find-if #'(lambda (f) (select-cp-file cp-id f))
                     ibm-data-ucm-files)
          when ibm-data-ucm-file
            collect (list cp-id ibm-data-ucm-file))
    ))


;;; generate-code-pages

(defun generate-code-pages (&optional
                            (cp-ids *ibm-x3270-icu-code-page-codes*)
                            (cp-ucm-dir *ibm-icu-code-page-local-dir*)
                            )
  "Generates the code pages for the ids in CP-IDS.

The argument CP-UCM-DIR contains the UCM files.

CP-IDS defaults to *IBM-x3270-ICU-CODE-PAGES-CODES*, while CP-UCM-DIR
defaults to *IBM-ICU-CODE-PAGE-LOCAL-DIR*."

  (declare (type list cp-ids)
           (type (or string pathname) cp-ucm-dir))

  (loop for (cp-id cp-ucm-file) in (select-codepage-files cp-ids cp-ucm-dir)
        do (generate-code-page cp-id cp-ucm-file)
        ))


;;; generate-code-page
;;;
;;; Notes:
;;;
;;; I should probably use the pretty printer in this function.

(defun generate-code-page (cp-id
                           cp-ucm-file
                           &optional ; Useful for debugging.
                           (cp-pathname
                            (merge-pathnames
                             (make-pathname :directory '(:relative :up "cps")
                                            :name (format nil "cp-~D" cp-id)
                                            :type "lisp")
                             (pathname cp-ucm-file)))
                           )
  "Generate the CL3720 code page definition file.

The function creates a file (CP-PATHNAME) containing the definition of
code page CP-ID -- an instance of struct CODEPAGE -- as a result of
parsing the 'UCM' file CP-UCM-FILE.  CP-PATHNAME is built relative to
the location of CP-UCM-FILE.


Arguments and Values:

CP-ID : the code page id (of type CODEPAGE-ID).
CP-UCM-FILE : the file where the 'ucm' defition is found (STRING or PATHNAME).
CP-PATHNAME : the output file (a PATHNAME).


Exceptional Situations:

If CP-ID and the name of CP-UCM-FILE do not agree (i.e., CP-ID does
not appear in CP-UCM-FILE name) an assertion fails.


Notes:

CP-PATHNAME may be supplied, bypassing its automated construction;
this is useful for debugging purposes.
"

  (declare (type (or string pathname) cp-ucm-file)
           (type pathname cp-pathname))

  (assert (search (format nil "ibm-~D" cp-id)
                  (pathname-name (pathname cp-ucm-file)))
      ()
    "CL3270: codepage id ~S and file name ~S are incompatible."
    cp-id
    (pathname-name (pathname cp-ucm-file)))

  (format t ";;; CL3270: Generating ~S~%" cp-pathname)
  (format t ";;; CL3270: from       ~S~2%" cp-ucm-file)
  (finish-output)

  (handler-case
      (let* ((u2e (ucm-process-file cp-ucm-file))
             (e2u (let ((e2u-map (make-dict)))
                    (maphash #'(lambda (k v) (setf (gethash v e2u-map) k)) u2e)
                    e2u-map))
             )
        (declare (type hash-table u2e e2u))

        (with-open-file (cps cp-pathname
                             :direction :output
                             :if-exists :supersede
                             :element-type 'character
                             :external-format :utf-8
                             )
          (format cps ";;;; -*- Mode: Lisp; Coding: utf-8 -*-~2%")
          (format cps ";;;; ~A.~A~2%"
                  (pathname-name cp-pathname)
                  (pathname-type cp-pathname))

          (format cps ";;;; This file is part of CL3270~%;;;;~%")
          (format cps ";;;; See the file COPYING in the ")
          (format cps "top folder for licensing and~%")
          (format cps ";;;; copyright information.~%")
          (format cps ";;;;~%;;;; File generated on ~A~2%" (today-date t t t))

          (format cps "(in-package \"CL3270\")~2%")

          (format cps ";;; *e2u-codepage-~D*~%" cp-id)
          (format cps ";;; *u2e-codepage-~D*~%;;;~%" cp-id)
          (format cps ";;; IBM CP ~D <-> Unicode mappings from " cp-id)
          (format cps "`https://github.com/unicode-org/icu-data`.~2%")

          (block e2ucp
            (terpri cps) (terpri cps)
            (format cps "(defparameter *e2u-codepage-~D*~%" cp-id)
            (format cps "  (make-array 256 :element-type '(mod #x10000)~%")
            (format cps "    :initial-contents '(~%")

            (let ((line 0)
                  (pos -1)
                  )

              (declare (type fixnum line pos))

              (loop initially (format cps "~4T#|         ")
                    for i from 0 upto #xf
                    do (format cps "~6<_~X~> " i)
                    finally (format cps "|#~%"))

              (loop initially (format cps "~4T#| 0_ |#   ")
                    for i from 0 upto #xff
                    do (incf pos)
                       (when (>= pos 16)
                         (incf line)
                         (setq pos 0)
                         (format cps "~&~4t#| ~X_ |#   " line))
                       (multiple-value-bind (v foundp)
                           (gethash i e2u)
                         (if foundp
                             (format cps "~6<#x~2,'0X~> " v)
                             (format cps "~6<#x~X~> " (char-code #\ufffd)))))
              )

            (format cps "~&~4T))~@
                     ~4T\"Implements the EBCDIC->Unicode IBM CP ~D code page.~
                     \")~%"
                    cp-id)
            ) ; e2ucp


          (block u2ecp
            (format cps "~2%(defparameter *u2e-codepage-~D*~%" cp-id)
            (format cps "  (make-array 256 :element-type '(mod #x10000)~%")
            (format cps "    :initial-contents '(~%")

            (let ((line 0)
                  (pos -1)
                  )
              (declare (type fixnum line pos))

              (loop initially (format cps "~4T#|         ")
                    for i from 0 upto #xf
                    do (format cps "~6<_~X~> " i)
                    finally (format cps "|#~%"))

              (loop initially (format cps "~4T#| 0_ |#   ")
                    for i from 0 upto #xff
                    do (incf pos)
                       (when (>= pos 16)
                         (incf line)
                         (setq pos 0)
                         (format cps "~&~4t#| ~X_ |#   " line))
                       (multiple-value-bind (v foundp)
                           (gethash i u2e)
                         (if foundp
                             (format cps "~6<#x~2,'0X~> " v)
                             (format cps "~6<#x~X~> " #x3f))))
              )

            (format cps "~&~4T))~@
                     ~4T\"Implements the Unicode->EBCDIC IBM CP ~D code page.~
                     \")~%"
                    cp-id)
            ) ; u2ecp


          (block high-u2e
            (format cps "~2%(defparameter *high-u2e-codepage-~D*~%" cp-id)
            (format cps "  (make-dict~%")
            (format cps "   :initial-map~%")
            (format cps "   '(~%~4t")

            (loop with pos = -1
                  for k being the hash-key of u2e using (hash-value v)
                  when (> k #xff)
                    do (incf pos)
                       (when (>= pos 4)
                         (setq pos 0)
                         ;; (terpri cps)
                         (format cps "~&~4t"))
                       (format cps "(#x~4,'0X #x~4,'0X) " k v)
                       )

            (format cps "~&~4T))~@
                     ~4T\"Unicode->EBCDIC IBM CP ~D map for codepoints > #xff.~
                     \")~%"
                    cp-id)
            ) ; high-u2e


          ;; Creating the codepage struct.

          (block cp-creation
            (terpri cps) (terpri cps)
            (format cps "(defparameter *codepage-~D*~%" cp-id)
            (format cps "  (make-codepage~%")
            (format cps "   :id ~S~%" cp-id)
            (format cps "   :name \"Codepage ~S\"~%" cp-id)
            (format cps "   :e2u *e2u-codepage-~S*~%" cp-id)
            (format cps "   :u2e *u2e-codepage-~S*~%" cp-id)
            (format cps "   :high-u2e *high-u2e-codepage-~D*~%" cp-id)
            (format cps "   :esub #x3f~%")
            (format cps "   :ge #x08~%")
            (format cps "   :ge2u *cp310-to-unicode*~%")
            (format cps "   :u2ge *unicode-to-cp310*~%")
            (format cps "   )~%")
            (format cps "  \"Codepage ~S.\")~2%" cp-id)
            ) ; cp-creation
      

          (format cps "~%;;;; ~A.~A ends here.~%"
                  (pathname-name cp-pathname)
                  (pathname-type cp-pathname))
          )
        t)

    (error (e)
      (format *error-output* "CL3270: error: ~S.~%" e)
      nil)))


(defparameter *generate-code-pages* t
  "Internal flag to selectively start CL code page generation.

When not NULL generation can take place.")


(defun compile-load-code-page-file (cpf)
  "Compile (if needed) and load a CL code page file CPF.

Notes:

This function is needed as the files are not managed by the system
defintion facilities like ASDF."

  (declare (type pathname cpf))
  
  (if (not (probe-file cpf))
      (warn "CL3270: nonexistent code page file '~A.~A'."
            (pathname-name cpf)
            (pathname-type cpf))

      (let* ((cpcfp (compile-file-pathname cpf))
             (cpfwd (file-write-date cpf))
             (cpcfpwd (file-write-date cpcfp))
             )
        (declare (type pathname cpcfp)
                 (type (or null integer) cpfwd cpcfpwd))

        (format t "~&;;; CL3270: source file '~A.~A' ~D~%"
                (pathname-name cpf)
                (pathname-type cpf)
                cpfwd)

        (format t "~&;;; CL3270: output file '~A.~A' ~D~%"
                (pathname-name cpcfp)
                (pathname-type cpcfp)
                cpcfpwd)

        (cond ((and (probe-file cpcfp)
                    cpfwd
                    cpcfpwd
                    (<= cpfwd cpcfpwd))
               
               (format t "~&;;; CL3270: loading '~A.~A'.~%"
                       (pathname-name cpcfp)
                       (pathname-type cpcfp))
               (load cpcfp :verbose t))

          ((or (null (probe-file cpcfp))
               (> cpfwd cpcfpwd))
           (format t "~&;;; CL3270: compiling '~A.~A'.~%"
                   (pathname-name cpf)
                   (pathname-type cpf))
           (compile-file cpf :verbose t)
           (format t "~&;;; CL3270: loading '~A.~A'.~%"
                   (pathname-name cpf)
                   (pathname-type cpf))
           (load cpcfp :verbose t))

          ((or (null cpfwd) (null cpcfpwd))
           (warn "CL3270: cannot determine write dates for '~A'."
                 (pathname-name cpf)))
          ))))


(defun ensure-codepages ()
  "Ensure that the Common Lisp codepages are generated and loaded."

  (handler-case
      (when *generate-code-pages*
        (format t "~%;;; CL3270: setting up codepages...~%")

        (cond ((some (complement #'probe-file) *cl3270-code-page-pathnames*)
               (format t "~&;;; CL3270: generating codepages...~2%")
               (generate-code-pages)
               (format t "~&;;; CL3270: codepages generated.~2%"))

              (t
               (format t "~&;;; CL3270: codepages already generated.~2%")))

        (format t "~&;;; CL3270: compiling and/or loading codepages...~2%")
        (dolist (cp-file *cl3270-code-page-pathnames*)
          
          (compile-load-code-page-file cp-file))

        (format t "~%;;; CL3270: codepages setup complete.~2%"))

    (error (e)
      (format *error-output* ";;; CL3270: codepages setup error ~S.~%" e)
      (error e))))


;;; Utilities.

(defun ucm-read-lines (FILE)
  "Read a list of lines from FILE.

FILE can be a STRING, a PATHNAME or a (open, input) FILE-STREAM."

  (declare (type (or string pathname stream) file))

  (etypecase file
    (string (ucm-read-lines (pathname file)))

    (pathname (with-open-file (s file
                                 :direction :input
                                 :external-format :utf-8
                                 :element-type 'character
                                 )
                (ucm-read-lines s)))

    (stream (assert (typep file 'file-stream))
            (assert (open-stream-p file))
            (assert (input-stream-p file))
   
            (loop for line #| of-type (or null string) |# = (read-line file nil)
                  while line
                  collect line)
            )))

;;;; generate.lisp ends here.
