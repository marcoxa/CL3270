;; -*- Lisp -*-

(in-package :cl3270)

(defconstant +screen-rows+ 24)
(defconstant +screen-columns+ 80)

(defun make-declared-field (field content)
  (destructuring-bind (&key from &allow-other-keys) field
    (destructuring-bind (row col) from
      `(make-field :row ,row :col ,col :content ,content
                   ,@(alexandria:remove-from-plist field :from :len)))))

(defun make-row-to-field-map (fields static-row-definitions)
  (let ((row-to-fields (make-array +screen-rows+ :initial-element nil)))
    (dolist (field fields
                   row-to-fields)
      (destructuring-bind (&key from len &allow-other-keys) field
        (destructuring-bind (row column) from
          (push (make-declared-field field (subseq (aref static-row-definitions row)
                                                   column (+ len column)))
                (aref row-to-fields 0))
          ;; make terminator for field
          (if (< (+ column len 1) +screen-columns+)
            (push `(make-field :row ,row :col ,(+ column len 1)) (aref row-to-fields 0))
            (when (< (+ row 1) +screen-rows+)
              (push `(make-field :row ,(1+ row) :col 0) (aref row-to-fields 0)))))))))

(defun get-static-fields (row string)
  (let (fields)
    (ppcre:do-matches (start end "\\S+(?: \\S+)*" string (nreverse fields))
      (push `(make-field :row ,row :col ,start :content ,(subseq string start end)) fields))))

(defun parse-screen (screen fields)
  (let* ((static-row-definitions (coerce (rest (split-sequence:split-sequence #\Newline screen)) 'vector))
         (row-to-field-map (make-row-to-field-map fields static-row-definitions)))
    (assert (= (length static-row-definitions) +screen-rows+))
    (loop for static-row-definition across static-row-definitions
          for row from 0
          for fields = (concatenate 'list
                                    (get-static-fields row static-row-definition)
                                    (aref row-to-field-map row))
          when fields
            append fields)))

(defmacro defscreen (name screen &key fields)
  `(defparameter ,name (make-screen ,(string name) ,@(parse-screen screen fields))))
