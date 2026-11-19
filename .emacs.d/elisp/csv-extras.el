;;; csv-extras.el --- spreadsheet-style navigation for csv-mode -*- lexical-binding: t -*-

;;; Commentary:

;; Excel-style field and data-block motion.  See the csv-mode comment in
;; init.el.  Loaded from the csv-mode block's :config in init.el, which
;; binds the keys.

;;; Code:

(require 'csv-mode)

(defun csv--disable-visual-line-mode ()
  "Turn off `visual-line-mode', which is inherited from `text-mode-hook'."
  (visual-line-mode -1))

(defun csv--goto-field (n)
  "Move point to the start of field N on the current line.
Return nil if the line has fewer than N fields, in which case
point is left at end of line."
  (beginning-of-line)
  (let ((i 1) (found t))
    (while (and (< i n) found)
      (csv-end-of-field)
      (if (eolp)
          (setq found nil)
        (forward-char 1)
        (setq i (1+ i))))
    found))

(defun csv--field-blank-p ()
  "Return non-nil if the field starting at point is empty.
Whitespace and an empty quoted value count as empty."
  (save-excursion
    (let ((start (point)))
      (csv-end-of-field)
      (string-match-p "\\`[ \t]*\\(\"\"\\)?[ \t]*\\'"
                      (buffer-substring-no-properties start (point))))))

(defun csv--column-at-point ()
  "Return the 1-based index of the field at point, 0 outside a record.
Unlike `csv--field-index', count a trailing empty field, so that
point at the end of \"a,b,\" reports column 3, not 2."
  (let ((index (csv--field-index)))
    (if (and (eolp) (memq (preceding-char) csv-separator-chars))
        (1+ index)
      index)))

(defun csv--field-position (column)
  "Return the position of COLUMN's field on this line.
Return nil unless the line is a record with at least COLUMN fields."
  (save-excursion
    (beginning-of-line)              ; csv-not-looking-at-record assumes it
    (and (not (csv-not-looking-at-record))
         (csv--goto-field column)
         (point))))

(defun csv--field-filled-p (column)
  "Return the position of COLUMN's field on this line if it is non-empty.
Comment lines, blank lines, records with too few fields and empty
fields all count as blank, and return nil."
  (let ((pos (csv--field-position column)))
    (and pos
         (save-excursion
           (goto-char pos)
           (not (csv--field-blank-p)))
         pos)))

(defvar-local csv--edge-column nil
  "Goal column for consecutive data-edge motion commands.
Landing on a record with too few fields would otherwise make the
next command jump along a different column.")

(defun csv--data-edge-position (column step)
  "Return the position to jump to from point, moving by STEP in COLUMN.
STEP is 1 to move down or -1 to move up.  Mimics Command-Down-Arrow
in a spreadsheet: if the current field and the adjacent one are both
filled, stop at the last filled field before a blank; otherwise skip
any blank run and stop at the first filled field.  With no filled
field left in that direction, return the last record, the CSV
equivalent of shooting to the edge of the worksheet."
  (save-excursion
    ;; STATE is nil until the adjacent field decides which kind of jump
    ;; this is: `run' walks to the end of a filled block, `gap' crosses
    ;; blanks to the next filled field.  Starting on a blank field is
    ;; always a `gap' jump.
    (let ((state (unless (csv--field-filled-p column) 'gap))
          (prev nil)
          (edge nil)
          (target nil))
      (while (and (not target)
                  (zerop (forward-line step))
                  (bolp))
        (let ((pos (csv--field-position column))
              (filled (csv--field-filled-p column)))
          (when pos (setq edge pos))
          (cond ((null state)
                 (setq state (if filled 'run 'gap)
                       prev filled))
                ((eq state 'run)
                 (if filled (setq prev filled) (setq target prev)))
                (filled (setq target filled)))))
      (or target prev edge))))

(defun csv--move-to-data-edge (n)
  "Jump to the edge of the data block in the current column.
Repeat the jump N times, moving up if N is negative."
  (let ((column (or (and (memq last-command '(csv-forward-data-edge
                                              csv-backward-data-edge))
                         csv--edge-column)
                    (csv--column-at-point)))
        (step (if (> n 0) 1 -1)))
    (when (zerop column)
      (user-error "Point is not in a CSV record"))
    (setq csv--edge-column column)
    (dotimes (_ (abs n))
      (let ((target (csv--data-edge-position column step)))
        (when target (goto-char target))))))

(defun csv-forward-data-edge (&optional n)
  "Move down to the edge of the data block in this column.
Works like Command-Down-Arrow in macOS Excel.  With prefix argument
N, repeat the jump N times."
  (interactive "^p")
  (csv--move-to-data-edge (or n 1)))

(defun csv-backward-data-edge (&optional n)
  "Move up to the edge of the data block in this column.
Works like Command-Up-Arrow in macOS Excel.  With prefix argument N,
repeat the jump N times."
  (interactive "^p")
  (csv--move-to-data-edge (- (or n 1))))

(defun csv--field-count ()
  "Return the number of fields on the current line, 0 if not a record.
A trailing empty field counts, so \"a,b,\" has three."
  (save-excursion
    (beginning-of-line)
    (if (csv-not-looking-at-record)
        0
      (end-of-line)
      (csv--column-at-point))))

(defun csv--move-by-column (n)
  "Move point to the beginning of the field N columns to the right.
Negative N moves left.  Stay in the current row, stopping at its
first or last field."
  (let ((column (csv--column-at-point))
        (count (csv--field-count)))
    (when (zerop count)
      (user-error "Point is not in a CSV record"))
    (csv--goto-field (max 1 (min count (+ column n))))))

(defun csv-forward-column (&optional n)
  "Move point to the beginning of the next field in this row.
With prefix argument N, move N fields.  Unlike \\[csv-tab-command],
never create a field, and never leave the current row."
  (interactive "^p")
  (csv--move-by-column (or n 1)))

(defun csv-backward-column (&optional n)
  "Move point to the beginning of the previous field in this row.
With prefix argument N, move N fields.  Unlike \\[csv-backtab-command],
never leave the current row."
  (interactive "^p")
  (csv--move-by-column (- (or n 1))))

(provide 'csv-extras)

;;; csv-extras.el ends here
