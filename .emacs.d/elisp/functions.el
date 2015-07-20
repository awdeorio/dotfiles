;;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hungry-delete ()
  "Delete character and consecutive whitespace before point"  
  (interactive "*")
  (let ((here (point)))
    (skip-chars-backward " \t")
    (if (/= (point) here)
        (delete-region (point) here)
      (delete-backward-char 1))))

(defun hungry-delete-ws ()
  "Delete consecutive whitespace before point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-backward " \t\n")
    (if (/= (point) here)
        (delete-region (point) here))))

(defun hungry-delete-forward ()
  "Delete character and consecutive whitespace after point"  
  (interactive "*")
  (let ((here (point)))
    (skip-chars-forward " \t\n")
    (if (/= here (point))
        (delete-region here (point))
      (delete-backward-char 1))))

(defun hungry-delete-forward-ws ()
  "Delete consecutive whitespace after point"
  (interactive "*")
  (let ((here (point)))
    (skip-chars-forward " \t\n")
    (if (/= here (point))
        (delete-region here (point)))))


;;; alignment helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun align-comments (start end)
  "Align comments"
  (let ((left 0) (end (copy-marker end)))
    ;; compute left
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line 1))
      (save-excursion
        (while (< (point) end)
          (search-forward ";") (setq left (max left (current-column)))
          (forward-line)))
      ;; move lines
      (save-excursion
        (while (< (point) end)
          (search-forward "//") (backward-char 2) (hungry-delete) (to-column left) (insert " ")
          (forward-line))))))


(defun align-equals (start end)
  "Align equal statements"
  (interactive "r")
  (let ((left 0) (end (copy-marker end)))
    ;; compute left
    (save-excursion
      (goto-char start)
      (or (bolp) (forward-line 1))
      (save-excursion
        (while (< (point) end)
          (search-forward "=")
          (setq left (max left (current-column)))
          (forward-line)))
      ;; move lines
      (save-excursion
        (while (< (point) end)
          (search-forward "=") (backward-char) (hungry-delete) (to-column left)
          (forward-line))))))


;;; word counting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun word-count-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")

; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

; 2. Run the while loop.
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))

; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

; Count the words in the entire document
(defun word-count-buffer ()
  "Count all the words in the buffer"
  (interactive)
  (count-words-region (point-min) (point-max) )
)


;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reload .emacs file
(defun reload () (interactive)
  "Reload .emacs"
  (if (file-exists-p "~/.emacs")
      (load-file "~/.emacs")))


(defun path-append ( my-path )
(setq load-path (cons (expand-file-name my-path) load-path)))

(defun path-prepend ( my-path )
(setq load-path (append load-path (list (expand-file-name my-path)))))

