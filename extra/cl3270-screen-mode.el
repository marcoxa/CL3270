;;; cl3270-screen-mode.el --- WYSIWYG helpers for 3270 screen strings  -*- lexical-binding: t; -*-

(require 'cl-lib)

;;;; ------------------------------------------------------------------
;;;; 1) Compute zero-based (:row :col :len) for the active region (single line only)
;;;; ------------------------------------------------------------------

(defun cl3270--string-bounds-around-point ()
  "Return (QSTART . QEND) for the surrounding double-quoted string.
QSTART is the opening quote position, QEND is the closing quote position."
  (let ((ppss (syntax-ppss)))
    (unless (nth 3 ppss)
      (user-error "Point is not inside a string"))
    (let* ((qstart (nth 8 ppss))
           (qend (save-excursion
                  (goto-char qstart)
                  (forward-sexp 1)
                  (1- (point)))))
      (cons qstart qend))))

(defun cl3270--string-content-start (qstart qend)
  "Return position of first non-empty line inside string (between QSTART/QEND).
Ignores leading whitespace-only lines that end in a newline."
  (save-excursion
    (goto-char (1+ qstart)) ; first char inside the quotes
    (let ((limit qend)
          (result nil))
      (while (and (< (point) limit) (not result))
        (let ((bol (point)))
          (end-of-line)
          (let ((eol (min (point) limit)))
            (if (and
                 ;; whitespace-only from bol..eol
                 (save-excursion
                   (goto-char bol)
                   (skip-chars-forward " \t" eol)
                   (>= (point) eol))
                 ;; newline exists and is inside the string
                 (< eol limit)
                 (eq (char-after eol) ?\n))
                (goto-char (1+ eol)) ; skip empty line including newline
              (setq result bol)))))  ; found content - return start of this line
      (or result (point)))))

(defun cl3270--pos-to-rowcol-0 (pos content-start qend)
  "Convert POS to zero-based (ROW . COL) relative to CONTENT-START."
  (setq pos (max pos content-start))
  (setq pos (min pos qend))
  (save-excursion
    (goto-char content-start)
    (let ((row 0)
          (col 0)
          (limit pos))
      (while (< (point) limit)
        (let ((ch (char-after)))
          (if (eq ch ?\n)
              (progn
                (setq row (1+ row))
                (setq col 0)
                (forward-char 1))
            (setq col (1+ col))
            (forward-char 1))))
      (cons row col))))

(defun cl3270-static-run-from-region ()
  "Compute a zero-based (:row :col :len) run for the active region inside a string.

Constraints:
- Region must be fully inside the surrounding double-quoted string.
- Multi-line selections are rejected (region must not contain newline).
- Leading empty lines at the beginning of the string are ignored for row numbering."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((rbeg (region-beginning))
         (rend (region-end)))
    (when (= rbeg rend)
      (user-error "Empty region"))
    (when (save-excursion
            (goto-char rbeg)
            (search-forward "\n" rend t))
      (user-error "Multi-line selections are not supported"))
    (let* ((sb (cl3270--string-bounds-around-point))
           (qstart (car sb))
           (qend (cdr sb)))
      (unless (and (> rbeg qstart) (< rbeg qend)
                   (> rend qstart) (< rend qend))
        (user-error "Region must be entirely inside the surrounding string"))
      (let* ((content-start (cl3270--string-content-start qstart qend))
             (start-rc (cl3270--pos-to-rowcol-0 rbeg content-start qend))
             (len (- rend rbeg))
             (row (car start-rc))
             (col (cdr start-rc)))
        (message "Run: row=%d col=%d len=%d (zero-based)" row col len)
        (list :row row :col col :len len
              :qstart qstart :qend qend :content-start content-start)))))

;;;; ------------------------------------------------------------------
;;;; 2) Insert the generated run into :fields in the surrounding defscreen
;;;; ------------------------------------------------------------------

(defgroup cl3270 nil
  "Helpers for editing CL3270 defscreen forms."
  :group 'lisp)

(defcustom cl3270-fields-insert-template "(:from (%d %d) :len %d)"
  "Template inserted into :fields. Args are row col len (all integers)."
  :type 'string
  :group 'cl3270)

(defun cl3270--defun-bounds ()
  "Return (BEG . END) bounds of the surrounding top-level form."
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (cons (point) end))))

(defun cl3270--ensure-fields-list (defun-beg defun-end)
  "Ensure a :fields (...) property exists in the current defun.
Return (INSERT-POS . EMPTY-P) where INSERT-POS is inside the list
and EMPTY-P is t if the list is empty."
  (save-excursion
    (goto-char defun-beg)
    (let ((kw ":fields"))
      (if (re-search-forward (concat "\\_<" (regexp-quote kw) "\\_>") defun-end t)
          ;; Move to the opening paren of the value list
          (progn
            (skip-chars-forward " \t\n")
            (unless (eq (char-after) ?\()
              (user-error "%s exists but is not followed by a list" kw))
            (forward-char 1)
            (let* ((list-start (point))
                   (list-end (1- (scan-sexps (1- list-start) 1)))
                   (empty-p (save-excursion
                              (skip-chars-forward " \t\n" list-end)
                              (>= (point) list-end))))
              (goto-char list-end)
              (cons (point) empty-p)))
        ;; Insert a new :fields () before the final closing paren of defun.
        (goto-char defun-end)
        (skip-chars-backward " \t\n")
        (backward-char 1)
        (unless (eq (char-after) ?\))
          (user-error "Could not locate end of defscreen form"))
        (insert "\n  " kw " ()")
        (backward-char 1) ; inside the ()
        (cons (point) t)))))

(defun cl3270--entry-< (a b)
  "Return t if entry A should come before entry B (by row, then col)."
  (let ((a-from (plist-get a :from))
        (b-from (plist-get b :from)))
    (or (< (nth 0 a-from) (nth 0 b-from))
        (and (= (nth 0 a-from) (nth 0 b-from))
             (< (nth 1 a-from) (nth 1 b-from))))))

(defun cl3270-insert-static-run (row col len)
  "Insert a run into :fields of the surrounding defscreen form.
Entries are kept sorted by screen position (row, then column)."
  (let* ((b (cl3270--defun-bounds))
         (beg (car b))
         (end (cdr b))
         (attrs-pos (cl3270--find-fields-value-pos beg end))
         (entries (when attrs-pos (cl3270--parse-fields beg end)))
         (new-entry (list :from (list row col) :len len))
         (entry-str (format cl3270-fields-insert-template row col len)))
    (if (null attrs-pos)
        ;; No :fields yet, create it with the new entry
        (let ((result (cl3270--ensure-fields-list beg end)))
          (save-excursion
            (goto-char (car result))
            (insert entry-str)))
      ;; Find insertion position among existing entries
      (save-excursion
        (goto-char attrs-pos)
        (forward-char 1) ; skip opening (
        (let ((insert-pos nil)
              (first-entry-pos nil)
              (prev-entry-end nil))
          ;; Scan through entries to find where new one belongs
          (dolist (e entries)
            (skip-chars-forward " \t\n")
            (let ((entry-start (point))
                  (entry-end (save-excursion (forward-sexp 1) (point))))
              (unless first-entry-pos
                (setq first-entry-pos entry-start))
              (when (and (null insert-pos)
                         (cl3270--entry-< new-entry e))
                (setq insert-pos (or prev-entry-end first-entry-pos)))
              (setq prev-entry-end entry-end)
              (goto-char entry-end)))
          ;; Insert at found position, or at end if new entry comes last
          (if insert-pos
              (progn
                (goto-char insert-pos)
                (if (= insert-pos first-entry-pos)
                    ;; Insert before first entry
                    (progn
                      (insert entry-str "\n")
                      (lisp-indent-line)
                      (forward-line -1)
                      (lisp-indent-line))
                  ;; Insert after a previous entry
                  (insert "\n" entry-str)
                  (lisp-indent-line)))
            ;; Append at end
            (if (null entries)
                (insert entry-str)
              (insert "\n" entry-str)
              (lisp-indent-line))))))
    entry-str))

;;;; ------------------------------------------------------------------
;;;; 3) Preview overlays for :fields runs (faces in the string)
;;;; ------------------------------------------------------------------

(defcustom cl3270-highlight-colors
  '("LightCoral" "PaleGreen1" "LightSkyBlue1" "plum1" "khaki1"
    "PaleTurquoise1" "NavajoWhite1" "thistle1" "DarkSeaGreen1"
    "LightSteelBlue1" "PeachPuff1" "aquamarine1" "MistyRose1"
    "LemonChiffon1" "lavender" "wheat1" "honeydew1" "LightBlue1"
    "RosyBrown1" "burlywood1" "SlateGray1" "SeaGreen1" "pink1"
    "LightYellow1" "azure1" "cornsilk1" "bisque1" "snow1"
    "ivory1" "AntiqueWhite1")
  "List of background colors to cycle through for highlighting entries.
Each entry and its corresponding spec get the same color."
  :type '(repeat string)
  :group 'cl3270)

(defun cl3270--highlight-faces ()
  "Return list of face specs derived from `cl3270-highlight-colors'."
  (mapcar (lambda (color)
            (list :foreground "black" :background color))
          cl3270-highlight-colors))

(defvar-local cl3270--overlays nil)

(defun cl3270--clear-overlays ()
  (when cl3270--overlays
    (mapc #'delete-overlay cl3270--overlays)
    (setq cl3270--overlays nil)))

(defun cl3270--rowcol-to-pos (row col qstart qend)
  "Convert zero-based ROW/COL to buffer position inside the string.
Returns nil if ROW is out of range. If COL is past end of line
\(due to missing trailing spaces), clamps to end of line."
  (let ((content-start (cl3270--string-content-start qstart qend)))
    (save-excursion
      (goto-char content-start)
      (catch 'done
        ;; advance ROW lines
        (dotimes (_ row)
          (unless (search-forward "\n" qend t)
            (throw 'done nil)))
        ;; advance COL chars, stopping at newline/end (clamp if line is short)
        (dotimes (_ col)
          (when (or (>= (point) qend) (eq (char-after) ?\n))
            (throw 'done (point)))
          (forward-char 1))
        (point)))))

(defun cl3270--read-sexp-at (pos)
  "Read one Lisp object from buffer at POS without moving point globally."
  (save-excursion
    (goto-char pos)
    (read (current-buffer))))

(defun cl3270--find-fields-value-pos (defun-beg defun-end)
  "Return buffer position of the list value of :fields, or nil."
  (save-excursion
    (goto-char defun-beg)
    (let ((kw ":fields"))
      (when (re-search-forward (concat "\\_<" (regexp-quote kw) "\\_>") defun-end t)
        (skip-chars-forward " \t\n")
        (when (eq (char-after) ?\()
          (point))))))

(defun cl3270--parse-fields (defun-beg defun-end)
  "Parse :fields list into Lisp objects. Returns a list of entries."
  (let ((pos (cl3270--find-fields-value-pos defun-beg defun-end)))
    (when pos
      (let ((obj (cl3270--read-sexp-at pos)))
        (unless (listp obj) (user-error ":fields is not a list"))
        obj))))

(defun cl3270--entry->overlay-spec (entry)
  "Normalize an ENTRY into (row col len) or nil if invalid."
  (when (and (listp entry) (keywordp (car entry)))
    (let* ((from (plist-get entry :from))
           (len  (plist-get entry :len)))
      (when (and (consp from) (= (length from) 2) (integerp (nth 0 from)) (integerp (nth 1 from))
                 (integerp len) (> len 0))
        (list (nth 0 from) (nth 1 from) len)))))

(defun cl3270--preview-fields-1 ()
  "Internal: create overlays for the current defscreen without clearing first."
  (let* ((db (cl3270--defun-bounds))
         (beg (car db))
         (end (cdr db))
         (attrs-pos (cl3270--find-fields-value-pos beg end))
         (entries (when attrs-pos (cl3270--parse-fields beg end)))
         (faces (cl3270--highlight-faces))
         (face-idx 0)
         (num-faces (length faces)))
    (when entries
      (save-excursion
        ;; Find the first string in the defscreen form; assume that's the layout.
        (goto-char beg)
        (unless (re-search-forward "\"" end t)
          (user-error "No string found in this defscreen form"))
        ;; point is now inside the string (right after opening quote)
        (let* ((sb (cl3270--string-bounds-around-point))
               (qstart (car sb))
               (qend (cdr sb)))
          ;; Now iterate through entries in :fields, tracking source positions
          (goto-char attrs-pos)
          (forward-char 1) ; skip opening (
          (dolist (entry entries)
            (let ((spec (cl3270--entry->overlay-spec entry))
                  (face-spec (nth (mod face-idx num-faces) faces)))
              (setq face-idx (1+ face-idx))
              ;; Find source position of this entry
              (skip-chars-forward " \t\n")
              (let ((entry-start (point))
                    (entry-end (save-excursion (forward-sexp 1) (point))))
                ;; Overlay on the source spec
                (let ((ov (make-overlay entry-start entry-end)))
                  (overlay-put ov 'face face-spec)
                  (overlay-put ov 'cl3270 t)
                  (push ov cl3270--overlays))
                (goto-char entry-end)
                ;; Overlay on screen content
                (when spec
                  (pcase-let ((`(,row ,col ,len) spec))
                    (let ((s (cl3270--rowcol-to-pos row col qstart qend)))
                      (when s
                        (let* ((line-end (save-excursion
                                           (goto-char s)
                                           (min qend (line-end-position))))
                               (ov-end (min line-end (+ s len)))
                               (ov (make-overlay s ov-end)))
                          (overlay-put ov 'face face-spec)
                          (overlay-put ov 'cl3270 t)
                          (push ov cl3270--overlays))))))))))))))

(defun cl3270-preview-fields ()
  "Create face overlays for :fields runs in the surrounding defscreen.
Each entry gets a unique color, and the same color is applied to
both the screen content and the :fields spec."
  (interactive)
  (cl3270--clear-overlays)
  (cl3270--preview-fields-1)
  (when cl3270--overlays
    (message "CL3270 preview: %d overlays" (length cl3270--overlays))))

(defun cl3270-clear-preview ()
  "Remove CL3270 overlays from the current buffer."
  (interactive)
  (cl3270--clear-overlays)
  (message "CL3270 preview cleared"))

(defun cl3270-preview-all-fields ()
  "Scan the entire buffer for defscreen forms and apply highlights to all of them."
  (interactive)
  (cl3270--clear-overlays)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^(defscreen\\>" nil t)
        (ignore-errors
          (cl3270--preview-fields-1)
          (setq count (1+ count))))
      (when (> count 0)
        (message "CL3270 preview: highlighted %d defscreen form(s)" count)))))

;;;; ------------------------------------------------------------------
;;;; 4) One command that does all: compute run, insert into :fields, refresh preview
;;;; ------------------------------------------------------------------

(defun cl3270-add-field-from-region ()
  "Compute a static run from region, insert into :fields, refresh overlays.
Rejects multi-line selections."
  (interactive)
  (let* ((run (cl3270-static-run-from-region))
         (row (plist-get run :row))
         (col (plist-get run :col))
         (len (plist-get run :len))
         (entry (cl3270-insert-static-run row col len)))
    (cl3270-preview-fields)
    (message "Inserted %s" entry)))

;;;; ------------------------------------------------------------------
;;;; 5) Minor mode (bind F9, auto-refresh preview on edits)
;;;; ------------------------------------------------------------------

(defcustom cl3270-auto-preview t
  "If non-nil, refresh overlay preview after buffer edits (when mode is on)."
  :type 'boolean
  :group 'cl3270)

(defun cl3270--after-change (_beg _end _len)
  (when (and cl3270-screen-mode cl3270-auto-preview)
    ;; Keep it simple and robust: full refresh.
    (ignore-errors (cl3270-preview-fields))))

(defvar cl3270-screen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f9>") #'cl3270-add-field-from-region)
    map)
  "Keymap for `cl3270-screen-mode'.")

(define-minor-mode cl3270-screen-mode
  "Minor mode for editing CL3270 defscreen layouts with static attribute runs.

Keys:
- F9        Add :fields run from region (single line only), then refresh preview
- C-c C-p   Refresh preview overlays
- C-c C-c   Clear preview overlays"
  :lighter " CL3270"
  :keymap cl3270-screen-mode-map
  (if cl3270-screen-mode
      (progn
        (add-hook 'after-change-functions #'cl3270--after-change nil t)
        (cl3270-preview-all-fields))
    (remove-hook 'after-change-functions #'cl3270--after-change t)
    (cl3270--clear-overlays)))

;;;; ------------------------------------------------------------------
;;;; 6) Enable automatically in Lisp mode (and ensure F9 is available there)
;;;; ------------------------------------------------------------------

(add-hook 'lisp-mode-hook #'cl3270-screen-mode)

;; If you prefer NOT to enable the mode automatically, comment the hook above and
;; use this instead to just bind F9 in lisp-mode:
;; (with-eval-after-load 'lisp-mode
;;   (define-key lisp-mode-map (kbd "<f9>") #'cl3270-add-field-from-region))

(provide 'cl3270-screen-mode)
