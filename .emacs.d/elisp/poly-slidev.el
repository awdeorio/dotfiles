;;; poly-slidev.el --- Slidev decks with YAML front matter in polymode -*- lexical-binding: t -*-

;;; Commentary:

;; `poly-slidev-mode' edits Slidev decks (https://sli.dev) with per-slide
;; YAML front matter in yaml-mode, plus slide navigation.  See the
;; polymode comment in init.el.  Loaded from the poly-markdown block's
;; :config in init.el, which binds the keys, adds the advice, and adds
;; the mode line entry.

;;; Code:

(require 'polymode)
(require 'poly-markdown)
(require 'markdown-mode)
(require 'seq)

(defun poly-slidev--frontmatter-line-p ()
  "Return non-nil if the line at point could belong inside a YAML
front matter block: blank, a `# comment', a `key: value' pair, or a
`- list item' (any of these optionally indented)."
  (looking-at "^[ \t]*\\(?:$\\|#\\|[[:alpha:]_][^\n:]*:\\|-\\)"))

(defun poly-slidev--frontmatter-closes-p ()
  "From point (the line right after a `---' front matter opener),
scan forward through lines that look like YAML and return non-nil if
a closing `---' is reached before any line that doesn't.  Blank lines
and comments are allowed inside the block (see `poly-slidev--frontmatter-line-p'),
so a legitimate front matter block with blank-line spacing isn't
mistaken for a bare slide separator."
  (save-excursion
    (catch 'done
      (while t
        (cond
         ((eobp) (throw 'done nil))
         ((looking-at "^---[ \t]*$") (throw 'done t))
         ((poly-slidev--frontmatter-line-p) (forward-line 1))
         (t (throw 'done nil)))))))

(defun poly-slidev--frontmatter-open-p (border-end)
  "Return non-nil if a `---' border ending at BORDER-END opens a real
Slidev front matter block: the next line looks like a YAML key, and a
closing `---' is reached before any unrelated line."
  (save-excursion
    (goto-char border-end)
    (forward-line 1)
    (and (looking-at "^[[:alpha:]_][^[:space:]:]*:")
         (poly-slidev--frontmatter-closes-p))))

(defun poly-slidev-yaml-frontmatter-head-matcher (ahead)
  "Match a Slidev front matter opening `---' line.
Search forward when AHEAD is positive, backward when negative, per
polymode's head-matcher calling convention."
  (let ((search-fn (if (< ahead 0) #'re-search-backward #'re-search-forward))
        found)
    (while (and (not found) (funcall search-fn "^---[ \t]*$" nil t))
      (let ((border (cons (match-beginning 0) (match-end 0))))
        (when (poly-slidev--frontmatter-open-p (cdr border))
          (setq found border))))
    found))

(defun poly-slidev--clear-markdown-properties-in-frontmatter (start end)
  "Undo markdown-mode's own syntax-propertize inside a Slidev front
matter block within START..END: its setext-heading and hr matchers
don't know about our boundaries and can misfire on a block's `---'
lines and last content line.  Advised onto `markdown-syntax-propertize',
shared by every markdown/gfm buffer, so guard by file name rather than
the poly-slidev-mode variable, which isn't set yet during the initial
whole-buffer pass."
  (when (and buffer-file-name
             (string-match-p "/slides\\.md\\'" buffer-file-name))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^---[ \t]*$" end t)
        (let ((open-beg (match-beginning 0)) (open-end (match-end 0)))
          (when (poly-slidev--frontmatter-open-p open-end)
            (save-excursion
              (goto-char open-end)
              (when (re-search-forward "^---[ \t]*$" nil t)
                (remove-text-properties open-beg (match-end 0)
                                         markdown--syntax-properties)))))))))

(defun poly-slidev-yaml-chunk-mode ()
  "Enable yaml-mode for a Slidev front matter chunk, skipping
`yaml-mode-hook' -- eglot-ensure there (see the yaml-mode block above)
would LSP-analyze the whole underlying buffer, not just the fragment."
  (delay-mode-hooks (yaml-mode)))

(define-innermode poly-slidev-yaml-frontmatter-innermode poly-markdown-root-innermode
  :mode 'poly-slidev-yaml-chunk-mode
  :head-matcher #'poly-slidev-yaml-frontmatter-head-matcher
  :tail-matcher "^---[ \t]*$"
  ;; polymode bolds head/tail lines (:head-adjust-face) and tints the
  ;; body's background (:adjust-face, inherited default 2) by default;
  ;; front matter should look like plain text, not call attention to itself.
  :head-adjust-face nil
  :adjust-face nil)

;; Fenced code blocks still use native fontification (see
;; markdown-fontify-code-blocks-natively above), not a polymode innermode.
(define-polymode poly-slidev-mode poly-gfm-mode
  :innermodes '(poly-slidev-yaml-frontmatter-innermode))

;; Slide navigation reuses the innermode's span classification: a `---'
;; boundary starts a slide unless it's the `tail' closing front matter.
;; The first boundary, if at point-min, is the deck's own front matter
;; and belongs to slide 1 rather than starting a new one.
(defun slidev--skip-blank-lines-forward (pos)
  "Return the first position at or after POS that isn't a blank line."
  (save-excursion
    (goto-char pos)
    (while (and (not (eobp)) (looking-at "^[ \t]*$"))
      (forward-line 1))
    (point)))

(defun slidev--boundary-content-start (boundary-pos)
  "Return where a slide's content begins, given the position of its
opening `---' boundary BOUNDARY-POS, skipping any front matter block
and blank lines."
  (save-excursion
    (goto-char boundary-pos)
    (forward-line 1)
    (when (eq (nth 0 (pm-innermost-span boundary-pos)) 'head)
      (re-search-forward "^---[ \t]*$" nil t)
      (forward-line 1))
    (slidev--skip-blank-lines-forward (point))))

(defun slidev--slide-boundaries ()
  "Return a sorted list of positions where each slide begins: its
opening `---' boundary, or point-min for an implicit first slide."
  (save-excursion
    (goto-char (point-min))
    (let (boundaries in-fence)
      ;; Like Slidev's parser, ignore `---' inside fenced code blocks,
      ;; e.g. a Mermaid diagram's own front matter.
      (while (re-search-forward "^\\(?:[ \t]*\\(```\\|~~~\\)\\|---[ \t]*$\\)" nil t)
        (let ((bol (match-beginning 0)))
          (cond
           ((match-beginning 1) (setq in-fence (not in-fence)))
           ((or in-fence (eq (nth 0 (pm-innermost-span bol)) 'tail)))
           (t (push bol boundaries))))
        (forward-line 1))
      (setq boundaries (nreverse boundaries))
      (if (and boundaries (= (car boundaries) (point-min)))
          boundaries
        (cons (point-min) boundaries)))))

(defun slidev--slide-start-positions ()
  "Return a sorted list of content-start positions, one per slide."
  (mapcar (lambda (boundary)
            (if (save-excursion (goto-char boundary) (looking-at-p "---[ \t]*$"))
                (slidev--boundary-content-start boundary)
              (slidev--skip-blank-lines-forward boundary)))
          (slidev--slide-boundaries)))

;; Mode line "Slide N/M".  Redisplay evaluates it often, so cache the
;; boundary scan until the buffer text changes.  Boundaries rather than
;; content starts, so point inside a slide's front matter counts as that
;; slide.
(defvar-local slidev--boundaries-cache nil
  "Cons of (CHARS-MODIFIED-TICK . BOUNDARIES) for the mode line.")

(defun slidev--mode-line-string ()
  "Return \" Slide N/M\" for point in a `poly-slidev-mode' buffer."
  (when (bound-and-true-p poly-slidev-mode)
    (let ((tick (buffer-chars-modified-tick)))
      (unless (eql (car slidev--boundaries-cache) tick)
        (setq slidev--boundaries-cache
              (cons tick (ignore-errors (slidev--slide-boundaries))))))
    (let ((boundaries (cdr slidev--boundaries-cache)))
      (when boundaries
        (format " Slide %d/%d"
                (max 1 (seq-count (lambda (b) (<= b (point))) boundaries))
                (length boundaries))))))


(defun slidev--report-slide-number (starts)
  (message "Slide %d/%d" (1+ (or (seq-position starts (point)) 0)) (length starts)))

(defun poly-slidev-forward-slide (&optional n)
  "Move to the start of the Nth next slide's content."
  (interactive "p")
  (let* ((starts (slidev--slide-start-positions))
         (later (seq-filter (lambda (s) (> s (point))) starts)))
    (if (>= (length later) (or n 1))
        (goto-char (nth (1- (or n 1)) later))
      (user-error "No next slide"))
    (slidev--report-slide-number starts)))

(defun poly-slidev-backward-slide (&optional n)
  "Move to the start of the Nth previous slide's content."
  (interactive "p")
  (let* ((starts (slidev--slide-start-positions))
         (earlier (reverse (seq-filter (lambda (s) (< s (point))) starts))))
    (if (>= (length earlier) (or n 1))
        (goto-char (nth (1- (or n 1)) earlier))
      (user-error "No previous slide"))
    (slidev--report-slide-number starts)))

(defun poly-slidev-goto-slide (n)
  "Move to the start of slide N's content, like `goto-line'."
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Goto slide: "))))
  (let* ((starts (slidev--slide-start-positions))
         (total (length starts)))
    (unless (and (>= n 1) (<= n total))
      (user-error "Slide %d out of range (1-%d)" n total))
    (goto-char (nth (1- n) starts))
    (slidev--report-slide-number starts)))

(provide 'poly-slidev)

;;; poly-slidev.el ends here
