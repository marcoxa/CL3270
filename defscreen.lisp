;; -*- Lisp -*-

(in-package :cl3270)

(defconstant +screen-rows+ 24)
(defconstant +screen-columns+ 80)

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
    (alexandria:when-let (overlong-row (position-if (lambda (row) (> (length row) +screen-columns+)) definitions))
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
    (values `(make-field :row ,row
                         :col ,col
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
                     (alexandria:remove-from-plist (rest field) :row :col :content))
            (push `(make-field :row ,row :col ,col) fields)))))))

(defun set-attributes-in-cell-map (cell-map field-definitions)
  (dolist (field-definition field-definitions
                            cell-map)
    (destructuring-bind (&rest keys &key from len &allow-other-keys) field-definition
      (destructuring-bind (row col) from
        (let ((attributes (alexandria:remove-from-plist keys :from :len)))
          (dotimes (i len)
            (setf (cdr (aref (aref cell-map row) (+ col i))) attributes)))))))

(defun parse-screen (screen-image field-definitions)
  (make-fields (set-attributes-in-cell-map (screen-image-to-cell-map screen-image)
                                           field-definitions)))

(defmacro defscreen (name screen &key fields)
  `(defparameter ,name (make-screen ,(string name) ,@(parse-screen screen fields))))
