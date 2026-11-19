;;; org-extras.el --- org mode helper commands -*- lexical-binding: t -*-

;;; Commentary:

;; Commands and helpers for org mode.  Loaded from the org block's
;; :config in init.el, which binds the keys and adds the hooks.  Loading
;; this file only defines functions and variables.

;;; Code:

(require 'org)
(require 'org-list)
(require 'rich-text)
(require 'hippie-exp)
(declare-function clean-url "init" (url))

;; Dynamic, so the let in `org-copy-region-as-html' affects the export
(defvar org-html-htmlize-output-type)
(defvar org-export-filter-headline-functions)
(defvar org-export-filter-section-functions)

;; Record when an entry was created, the same way Orgzly does
(defun org-add-created-property ()
  "Set the CREATED property of the current entry to today's date.
If the entry already has a CREATED property, do nothing.  Leave the
property drawer unfolded."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (unless (org-entry-get nil "CREATED")
      (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a]"))
      ;; Org folds a new property drawer, so unfold it
      (goto-char (car (org-get-property-block)))
      (forward-line -1)
      (org-fold-hide-drawer-toggle 'off t))))

;; Insert a WAIT heading with a CREATED property.  Speed key W
(defun org-insert-wait-heading ()
  "Insert a WAIT sibling after the current subtree, with CREATED."
  (interactive)
  (org-insert-heading-respect-content)
  (insert "WAIT ")
  (org-add-created-property))

;; Archive all tasks in this file with a done keyword
(defun org-archive-done-tasks-file ()
  (interactive)
  (org-map-entries
   (lambda ()
     (when (org-entry-is-done-p)
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))))
   nil 'file))

;; Move subtree to bottom/top of its parent.  C-M-S-<down>, C-M-S-<up>
(defun org-move-subtree-to-bottom ()
  "Move the current subtree below its last sibling."
  (interactive)
  (condition-case nil
      (while t (org-move-subtree-down))
    (user-error nil)))
(defun org-move-subtree-to-top ()
  "Move the current subtree above its first sibling."
  (interactive)
  (condition-case nil
      (while t (org-move-subtree-up))
    (user-error nil)))

;; Go to first/last visible heading in the buffer.  Speed keys P, N
(defun org-goto-first-heading ()
  "Move point to the first heading in the buffer."
  (interactive)
  (goto-char (point-min))
  (unless (org-at-heading-p)
    (org-next-visible-heading 1)))
(defun org-goto-last-heading ()
  "Move point to the last visible heading in the buffer."
  (interactive)
  (goto-char (point-max))
  (org-previous-visible-heading 1))

;; Speed keys on plain list items.  Headings and items form one
;; continuous outline: n/p walk both, u from a top-level item reaches
;; its heading, and d from a heading with no child heading reaches its
;; first item.  Item keys are active from column 0 through the bullet.
(defun org-speed--goto-speed-position ()
  "Move to the speed key position of the current heading or item."
  (if (org-at-heading-p) (forward-line 0) (back-to-indentation)))

(defun org-speed--outline-line-p ()
  "Return non-nil if this line is a visible heading or list item."
  (and (not (org-invisible-p (line-beginning-position)))
       (or (org-at-heading-p) (org-at-item-p))))

(defun org-speed-next-visible ()
  "Move to the next visible heading or list item."
  (interactive)
  (let ((start (point))
        (re (concat org-outline-regexp-bol "\\|" (org-item-re)))
        found)
    (goto-char (line-end-position))
    (while (and (not found) (re-search-forward re nil t))
      (forward-line 0)
      (if (org-speed--outline-line-p)
          (setq found t)
        (goto-char (line-end-position))))
    (if found
        (org-speed--goto-speed-position)
      (goto-char start)
      (user-error "No next heading or item"))))

(defun org-speed-previous-visible ()
  "Move to the previous visible heading or list item."
  (interactive)
  (let ((start (point))
        (re (concat org-outline-regexp-bol "\\|" (org-item-re)))
        found)
    (forward-line 0)
    (while (and (not found) (re-search-backward re nil t))
      (forward-line 0)
      (setq found (org-speed--outline-line-p)))
    (if found
        (org-speed--goto-speed-position)
      (goto-char start)
      (user-error "No previous heading or item"))))

(defun org-speed--first-item-in-entry ()
  "Return the start of the first list item in this entry's text, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (org-entry-end-position))
          (re (org-item-re))
          found)
      (goto-char (line-end-position))
      (while (and (not found) (re-search-forward re end t))
        (forward-line 0)
        (if (org-at-item-p)
            (setq found (point))
          (goto-char (line-end-position))))
      found)))

(defun org-speed-down ()
  "Move to the first child heading, else to the entry's first item.
On an item, move to its first child item."
  (interactive)
  (if (org-at-item-p)
      (let* ((struct (org-list-struct))
             (parents (org-list-parents-alist struct))
             (item (line-beginning-position))
             (child (car (org-list-get-children item struct parents))))
        (unless child (user-error "No child item"))
        (org-list-set-item-visibility item struct 'children)
        (goto-char child)
        (back-to-indentation))
    (org-back-to-heading t)
    (org-fold-show-children)
    (unless (org-goto-first-child)
      (let ((item (org-speed--first-item-in-entry)))
        (unless item (user-error "No child heading or item"))
        (org-fold-show-entry)
        (goto-char item)
        (back-to-indentation)))))

(defun org-speed-up-item ()
  "Move to the parent item, or to the heading from a top-level item."
  (interactive)
  (let* ((struct (org-list-struct))
         (parents (org-list-parents-alist struct))
         (parent (org-list-get-parent (line-beginning-position)
                                      struct parents)))
    (cond (parent (goto-char parent) (back-to-indentation))
          ((org-before-first-heading-p)
           (user-error "No parent item or heading"))
          (t (org-back-to-heading t)))))

(defun org-speed-forward-item ()
  "Move to the next sibling item."
  (interactive)
  (org-next-item)
  (back-to-indentation))

(defun org-speed-backward-item ()
  "Move to the previous sibling item."
  (interactive)
  (org-previous-item)
  (back-to-indentation))

(defun org-move-item-to-bottom ()
  "Move the current item below its last sibling."
  (interactive)
  (let ((org-list-use-circular-motion nil))
    (condition-case nil
        (while t (org-move-item-down))
      (user-error nil))))

(defun org-move-item-to-top ()
  "Move the current item above its first sibling."
  (interactive)
  (let ((org-list-use-circular-motion nil))
    (condition-case nil
        (while t (org-move-item-up))
      (user-error nil))))

(defun org-speed-insert-item ()
  "Insert a new item after the current item and its children.
Insert before the current item, then move the new item down past it,
so org keeps the list's bullets, numbering, and blank lines."
  (interactive)
  (let ((box (org-list-get-checkbox (line-beginning-position)
                                    (org-list-struct)))
        (org-list-use-circular-motion nil))
    (back-to-indentation)
    (org-insert-item box)
    (org-move-item-down)
    (goto-char (line-end-position))))

(defun org-speed-toggle-checkbox ()
  "Toggle the item's checkbox, adding one if there is none."
  (interactive)
  (if (org-list-get-checkbox (line-beginning-position) (org-list-struct))
      (org-toggle-checkbox)
    (org-toggle-checkbox '(4))))

(defvar org-item-speed-commands
  '(("n" . org-speed-next-visible)
    ("p" . org-speed-previous-visible)
    ("f" . org-speed-forward-item)
    ("b" . org-speed-backward-item)
    ("u" . org-speed-up-item)
    ("d" . org-speed-down)
    ("c" . org-cycle)
    ("U" . org-metaup)
    ("D" . org-metadown)
    ("r" . org-metaright)
    ("l" . org-metaleft)
    ("R" . org-shiftmetaright)
    ("L" . org-shiftmetaleft)
    ("A" . org-move-item-to-top)
    ("Z" . org-move-item-to-bottom)
    ("N" . org-goto-last-heading)
    ("P" . org-goto-first-heading)
    ("i" . org-speed-insert-item)
    ("t" . org-speed-toggle-checkbox)
    ("?" . org-item-speed-command-help))
  "Speed commands on plain list items, like `org-speed-commands'.")

(defun org-item-speed-command-help ()
  "Show the item speed keys."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Item speed commands\n===================\n")
    (dolist (c org-item-speed-commands)
      (princ (format "%s   %s\n" (car c) (cdr c))))))

(defun org-item-speed-command-activate (keys)
  "Return the item speed command for KEYS, or nil.
Active from column 0 through the bullet of a list item.  A heading
speed key with no item version gives an error instead of inserting.
Space and digits always insert."
  (when (and (save-excursion (skip-chars-backward " \t") (bolp))
             (save-excursion (forward-line 0) (looking-at-p (org-item-re)))
             (org-at-item-p))
    (or (cdr (assoc keys org-item-speed-commands))
        (and (not (string-match-p "\\`[ 0-9]\\'" keys))
             (cdr (assoc keys org-speed-commands))
             `(user-error "No item speed key: %s" ,keys)))))

;; Copy region as rich text for pasting into Gmail and Google Docs.  C-c w
(defun org-html-rich-text-unwrap (html _backend _info)
  "Strip the outline <div> that wraps a headline or section in HTML."
  (if (string-match (concat "\\`[ \t\n]*<div \\(?:id=\"outline-container-"
                            "\\|class=\"outline-text-\\)[^>]*>")
                    html)
      (replace-regexp-in-string "</div>[ \t\n]*\\'" ""
                                (substring html (match-end 0)))
    html))
(defun org-copy-region-as-html (beg end)
  "Export the region from BEG to END to HTML and copy it as rich text.
See `copy-html-as-rich-text'."
  (interactive "r")
  (unless (use-region-p) (user-error "No active region"))
  (require 'ox-html)
  ;; Plain code blocks; fontifying needs htmlize and doesn't paste anyway.
  ;; Drop the outline <div>s around sections, which make Google Docs
  ;; paste everything bold.
  (let ((org-html-htmlize-output-type nil)
        (org-export-filter-headline-functions '(org-html-rich-text-unwrap))
        (org-export-filter-section-functions '(org-html-rich-text-unwrap)))
    (copy-html-as-rich-text
     (org-export-string-as (buffer-substring-no-properties beg end)
                           'html t '(:with-toc nil :section-numbers nil))))
  (deactivate-mark))

;; Paste a Gmail URL from the clipboard as an Org link.
;; Silence byte-compile warning; ol is loaded by org at runtime
(declare-function org-link-make-string "ol" (link &optional description))
(defun org-insert-gmail-link ()
  "Insert the Gmail URL on the clipboard/kill-ring as an Org link.
The URL is cleaned with `clean-url'.  The anchor text is the active
region, which the link replaces, or \"email\".  Like `yank', set the
mark at the start of the inserted link.  Signal an error if the
clipboard does not hold a Gmail message URL."
  (interactive)
  (let* ((raw (string-trim (current-kill 0 t)))
         (url (and (string-match-p "\\`https?://" raw) (clean-url raw)))
         (desc "email"))
    (unless (and url (string-match-p
                      "\\`https://mail\\.google\\.com/mail/u/[0-9]+/#all/"
                      url))
      (user-error "Clipboard does not contain a Gmail message URL"))
    (when (use-region-p)
      (let ((beg (region-beginning)) (end (region-end)))
        (setq desc (buffer-substring-no-properties beg end))
        (delete-region beg end)
        (goto-char beg)))
    (push-mark)
    (insert (org-link-make-string url desc))))

;; Add file local mode variable enabling org mode
(defun org-insert-file-local-mode-variable ()
  "Insert a -*- mode: org; -*- line, so the file opens in org mode."
  (interactive)
  (insert "-*- mode: org; -*-\n"))

;; M-/ tag completion in the tag-entry minibuffer (C-c C-c)
;; ---------------------------------------------------------------
;; M-/ already runs `hippie-expand' everywhere (see the
;; dabbrev-expand alias in the hippie-expand section above); the
;; functions below just prepend a tag-aware try-function while the
;; "Tags: " minibuffer is active, so M-/ there completes existing
;; tags before falling back to word expansion from the org buffer.
;; Only tags already used in the current buffer are offered, in
;; alphabetical (case-insensitive) order.
;; https://orgmode.org/manual/Setting-Tags.html

(defun org-tag-dabbrev-candidates ()
  "Return tags already used in the current buffer, alphabetically
(case-insensitive)."
  (sort (delete-dups
         (delq nil (mapcar (lambda (x) (and (stringp (car-safe x)) (car x)))
                            org-last-tags-completion-table)))
        (lambda (a b) (string-lessp (downcase a) (downcase b)))))

;; Words in BUFFER starting with PREFIX (case-insensitive), for
;; word completion.  `try-expand-dabbrev' can't be used for this:
;; it has no concept of a minibuffer's "origin" buffer, so from
;; inside the Tags: minibuffer it only searches the minibuffer's
;; own (nearly empty) text, never the org buffer.
(defun org-buffer-word-candidates (prefix buffer)
  "Return words in BUFFER starting with PREFIX, alphabetically
(both case-insensitive)."
  (let (words)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t)
              (regexp (concat "\\_<" (regexp-quote prefix) "\\(?:\\sw\\|\\s_\\)*")))
          (while (re-search-forward regexp nil t)
            (push (match-string-no-properties 0) words)))))
    (sort (delete-dups (nreverse words))
          (lambda (a b) (string-lessp (downcase a) (downcase b))))))

;; Shared hippie-expand cycling logic.  CANDIDATES-FN is called
;; with no args (when OLD is nil) to compute the full candidate
;; list for the current prefix; both try-functions below just
;; differ in where that list comes from.
(defun org-tag-entry-try-expand (old candidates-fn)
  (unless old
    (he-init-string (save-excursion
                       (skip-chars-backward "^:" (minibuffer-prompt-end))
                       (point))
                     (point))
    ;; When the field being completed is empty, `he-string-beg' and
    ;; `he-string-end' start at the same position.  Markers don't
    ;; advance past text inserted exactly at them by default, so
    ;; `he-substitute-string' would immediately delete-region right
    ;; back over whatever it just inserted.  Force the end marker to
    ;; advance so the inserted candidate survives.
    (set-marker-insertion-type he-string-end t)
    (setq he-expand-list (funcall candidates-fn)))
  (while (and he-expand-list (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string)) nil)
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun try-expand-org-tags (old)
  "Hippie-expand try function that completes against known org tags."
  ;; `org-tag-dabbrev-candidates' already returns tags in sorted
  ;; order; `all-completions' preserves list order, so don't
  ;; re-sort here.  Org tags are normally case-sensitive, but
  ;; ignore case here so, e.g., "h" also matches "Home".
  (org-tag-entry-try-expand
   old (lambda ()
         (let ((completion-ignore-case t))
           (all-completions he-search-string (org-tag-dabbrev-candidates))))))

(defun try-expand-org-buffer-words (old)
  "Hippie-expand try function that completes words from the
Tags: minibuffer's origin buffer (see `org-buffer-word-candidates')."
  (org-tag-entry-try-expand
   old (lambda ()
         (org-buffer-word-candidates he-search-string
                                      (window-buffer (minibuffer-selected-window))))))

;; Restrict M-/ to tags, then current-buffer words, only while
;; actually in the tag-entry minibuffer; every other minibuffer
;; prompt is unaffected.  The global `hippie-expand-try-functions-list'
;; also tries other buffers, file names, abbrevs, etc., none of
;; which make sense for a tags field.
(defun org-tags-minibuffer-hippie-expand-setup ()
  "Restrict `hippie-expand-try-functions-list' to tags, then
current-buffer words, while entering tags in the minibuffer opened
by `org-set-tags-command'."
  (when (string-prefix-p "Tags: " (or (minibuffer-prompt) ""))
    (setq-local hippie-expand-try-functions-list
                '(try-expand-org-tags try-expand-org-buffer-words))))

(provide 'org-extras)

;;; org-extras.el ends here
