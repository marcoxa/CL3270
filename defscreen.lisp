;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; defscreen.lisp
;;;;
;;;; A DEFSCREEN macro to simplify screen layout.
;;;; Thanks to Hans Hübner for providing this (nad much more).
;;;;
;;;; See the file COPYING for copyright and licensing information.

(in-package "CL3270")

(defconstant +screen-rows+ 24)
(defconstant +screen-columns+ 80)

;;;; Stuff copied from alexandria to avoid dependency

(defun remove-from-plist (plist &rest keys)
  "Returns a property-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed.
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified. Keys are compared using EQ."
  (declare (optimize (speed 3)))
  ;; FIXME: possible optimization: (remove-from-plist '(:x 0 :a 1 :b 2) :a)
  ;; could return the tail without consing up a new list.
  (loop for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
        unless (member key keys :test #'eq)
          collect key and collect (first rest)))

(defmacro when-let (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

;; -- end of alexandria stuff

(defun sort-fields (fields)
  (sort (copy-list fields)
        (lambda (a b)
          (if (= (getf (rest a) :row) (getf (rest b) :row))
              (< (getf (rest a) :col) (getf (rest b) :col))
              (< (getf (rest a) :row) (getf (rest b) :row))))))

(defun make-cell-map ()
  (loop with cell-map = (make-array +screen-rows+)
        for row below +screen-rows+
        do (setf (aref cell-map row) (make-array +screen-columns+ :initial-element nil))
        finally (return cell-map)))

(defun ensure-valid-screen-layout (screen-image)
  (let ((definitions (coerce (rest (split-sequence:split-sequence #\Newline screen-image)) 'vector)))
    (unless (= (length definitions) +screen-rows+)
      (error "Layout definition must start with an empty row and have ~A row following" +screen-rows+))
    (when-let (overlong-row (position-if (lambda (row) (> (length row) +screen-columns+)) definitions))
      (error "Row ~A of the layout definition is longer than the the maximum of ~A characters"
             overlong-row +screen-columns+))
    definitions))

(defun screen-image-to-cell-map (screen-image)
  (loop with static-row-definitions = (ensure-valid-screen-layout screen-image)
        with cell-map = (make-cell-map)
        for row-map across cell-map
        for static-row-definition across static-row-definitions
        do (loop for char across (format nil "~80A" static-row-definition)
                 for col from 0
                 do (setf (aref row-map col) (cons char nil)))
        finally (return cell-map)))

(defun attribute-change-position (row-map start-col attributes)
  (or (position-if-not (lambda (cell)
                         (equal (cdr cell) attributes))
                       row-map
                       :start start-col)
      +screen-columns+))

(defun static-text-end-position (row-map start-col)
  (loop for col from start-col below +screen-columns+
        if (and (eql (car (aref row-map col)) #\Space)
                (or (= col (1- +screen-columns+))
                    (eql (car (aref row-map (1+ col))) #\Space)))
          return col))

(defun find-field (cell-map row col)
  (let* ((row-map (aref cell-map row))
         (attributes (cdr (aref row-map col)))
         (end (if attributes
                  (attribute-change-position row-map (1+ col) attributes)
                  (static-text-end-position row-map (1+ col))))
         (content (map 'string #'car (subseq row-map col end))))
    (values `(make-field :row ,(if (> col 0) row (1- row))
                         :col ,(1- (if (> col 0) col +screen-columns+))
                         ,@(unless (every (lambda (c) (eql c #\Space)) content) `(:content ,content))
                         ,@attributes)
            row
            end)))

(defun make-fields (cell-map &aux fields)
  (dotimes (row +screen-rows+
                (nreverse fields))
    (dotimes (col +screen-columns+)
      (unless (equal (cons #\Space nil) (aref (aref cell-map row) col))
        (let (field)
          (setf (values field row col) (find-field cell-map row col))
          (push field fields)
          (when (and (< col +screen-columns+)
                     (remove-from-plist (rest field) :row :col :content))
            (push `(make-field :row ,row :col ,col) fields)))))))

(defun set-attributes-in-cell-map (cell-map field-definitions)
  (dolist (field-definition field-definitions
                            cell-map)
    (destructuring-bind (&rest keys &key from len &allow-other-keys) field-definition
      (destructuring-bind (row col) from
        (let ((attributes (remove-from-plist keys :from :len)))
          (dotimes (i len)
            (setf (cdr (aref (aref cell-map row) (+ col i))) attributes)))))))

(defun parse-screen (screen-image field-definitions)
  (make-fields (set-attributes-in-cell-map (screen-image-to-cell-map screen-image)
                                           field-definitions)))

(defmacro defscreen (name screen &key fields)
  `(defparameter ,name (make-screen ,(string name) ,@(parse-screen screen fields))))


;;;; end of file -- defscreen.lisp
