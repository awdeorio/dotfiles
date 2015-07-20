;; todotxt.el -- A major mode for editing todo.txt files

;; Filename: todotxt.el

;; Description: A major mode for editing todo.txt files
;; Author: Rick Dillon <rpdillon@etherplex.org>
;; Copyright (C) 2011, Rick Dillon, all rights reserved.
;; Created: 14 March 2011
;; Version: 0.2
;; URL: https://github.com/rpdillon/todotxt.el
;; Keywords: todo.txt, todotxt, todotxt.el
;; Compatibility: GNU Emacs 22 ~ 24
;;

;; This file is NOT part of GNU Emacs

;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Commentary:
;; This file provides a Emacs interface to the todo.txt file format
;; use by Gina Trapani's Todo.txt command-line tool
;; (http://todotxt.com/) and Android application
;; (https://github.com/ginatrapani/todo.txt-touch).
;;
;; Setup:
;;  - Put todotxt.el somewhere on your Emacs path
;;  - Load todotxt using (require 'todotxt) in you .emacs (or other initialization) file
;;  - Customize the variable 'todotxt-file' with the location of your todo.txt file.
;;  - View the file with M-x todotxt
;;  - Bind 'todotxt' to some accelerator like C-x t: (global-set-key (kbd "C-x t") 'todotxt)
;;
;; Usage:
;;  - Navigate up and down with 'p' and 'n' (or 'k' and 'j')
;;  - Toggle completion of an item with 'c'
;;  - Add a new item with 'a'
;;  - Tag the current item with 't' (use tab completion as necessary)
;;  - Edit the current item with 'e'
;;  - Show only incomplete items with 'i'
;;  - Filter for any keyword or tag with '/'
;;
;; See 'readme.org' for more information.
;;
;; The vast majority of the behavior of this program is governed by
;; the todo.txt formatting rules.  The can be found on the GitHub page
;; for todo.txt-cli:
;;
;; https://github.com/ginatrapani/todo.txt-cli/wiki/The-Todo.txt-Format

;; Variables that are available for customization
(defcustom todotxt-file (expand-file-name "~/todo.txt")
  "The location of your todo.txt file."
  :type 'string
  :require 'todotxt
  :group 'todotxt)

(defcustom todotxt-use-creation-dates  'nil
  "If non-nil, include creation dates for newly added items.
Defaults to 't."
  :type 'boolean
  :require 'todotxt
  :group 'todotxt)

(defcustom todotxt-save-after-change  't
  "If non-nil, the file is saved after any operation is
performed.  Defaults to 't."
  :type 'boolean
  :require 'todotxt
  :group 'todotxt)

;; awdeorio: bug fix: "C++" recognized as a project
;; awdeorio: bug fix: a single "+" recognized as a project
(setq tags-regexp "\\(?:^\\|\\s-\\)\\([+|@][^[:space:]]+\\)") ; Used to find keywords for completion
(setq projects-regexp "\\(?:^\\|\\s-\\)\\(+[^[:space:]]+\\)")
(setq contexts-regexp "\\(?:^\\|\\s-\\)\\(@[^[:space:]]+\\)")
(setq complete-regexp "^x .*?$")
(setq priority-regexp "^(\\([A-Z]\\)) .*?$")
(setq priority-a-regexp "^\\((A)\\) .*?$")
(setq priority-b-regexp "^\\((B)\\) .*?$")
(setq priority-c-regexp "^\\((C)\\) .*?$")

(setq todotxt-active-filters '())

;; Font Lock and Faces
(defface todotxt-complete-face '(
  (t (:strike-through t)))
  "Todotxt face used for completed task."
  :group 'todotxt-highlighting-faces)

(defvar todotxt-complete-face 'todotxt-complete-face
  "Todotxt mode face used for completed task.")

(defface todotxt-priority-a-face '(
  (((class color) (background dark)) (:foreground "red"))
  (((class color) (background light)) (:foreground "red"))
  (t (:bold t)))
  "Todotxt mode face used for tasks with a priority of A."
  :group 'todotxt-highlighting-faces)

(defvar todotxt-priority-a-face 'todotxt-priority-a-face
  "Todotxt mode face used for tasks with a priority of A.")

(defface todotxt-priority-b-face '(
  (((class color) (background dark)) (:foreground "orange"))
  (((class color) (background light)) (:foreground "dark orange"))
  (t (:bold t)))
  "Todotxt mode face used for tasks with a priority of B."
  :group 'todotxt-highlighting-faces)

(defvar todotxt-priority-b-face 'todotxt-priority-b-face
  "Todotxt mode face used for tasks with a priority of B.")

(defface todotxt-priority-c-face '(
  (((class color) (background dark)) (:foreground "yellow"))
  (((class color) (background light)) (:foreground "gold"))
  (t (:bold t)))
  "Todotxt mode face used for tasks with a priority of C."
  :group 'todotxt-highlighting-faces)

(defvar todotxt-priority-c-face 'todotxt-priority-c-face
  "Todotxt mode face used for tasks with a priority of C.")

(setq todotxt-highlight-regexps
      `((,projects-regexp   0 font-lock-variable-name-face t)
        (,contexts-regexp   0 font-lock-keyword-face t)
        (,complete-regexp   0 todotxt-complete-face t)
        (,priority-a-regexp 1 todotxt-priority-a-face t)
        (,priority-b-regexp 1 todotxt-priority-b-face t)
        (,priority-c-regexp 1 todotxt-priority-c-face t)))

;; Setup a major mode for todotxt
(define-derived-mode todotxt-mode text-mode "todotxt" 
  "Major mode for working with todo.txt files. \\{todotxt-mode-map}"
  (setq font-lock-defaults '(todotxt-highlight-regexps))
  (setq goal-column 0)
  (setq flyspell-mode 0) ;; awdeorio HACK HACK HACK
;;  (setq buffer-read-only t) ;; awdeorio
)

;; Setup key map
;; FIXME awdeorio commented out
;; (define-key todotxt-mode-map (kbd "l") 'todotxt-unhide-all)      ; (L)ist
;; (define-key todotxt-mode-map (kbd "i") 'todotxt-show-incomplete) ; list (I)ncomplete
;; (define-key todotxt-mode-map (kbd "c") 'todotxt-complete-toggle) ; (C)omplete item
;; (define-key todotxt-mode-map (kbd "N") 'todotxt-nuke-item)       ; (N)uke item
;; (define-key todotxt-mode-map (kbd "a") 'todotxt-add-item)        ; (A)dd item
;; (define-key todotxt-mode-map (kbd "q") 'todotxt-bury)            ; (Q)uit
;; (define-key todotxt-mode-map (kbd "r") 'todotxt-add-priority)    ; Add p(r)iority
;; (define-key todotxt-mode-map (kbd "A") 'todotxt-archive)         ; (A)rchive completed items
;; (define-key todotxt-mode-map (kbd "e") 'todotxt-edit-item)       ; (E)dit item
;; (define-key todotxt-mode-map (kbd "t") 'todotxt-tag-item)        ; (T)ag item
;; (define-key todotxt-mode-map (kbd "/") 'todotxt-filter-for)      ; 
;; (define-key todotxt-mode-map (kbd "\\") 'todotxt-filter-out)     ; 
;; (define-key todotxt-mode-map (kbd "s") 'save-buffer)             ; (S)ave
;; (define-key todotxt-mode-map (kbd "n") 'next-line)               ; (N)ext
;; (define-key todotxt-mode-map (kbd "p") 'previous-line)           ; (P)revious
;; (define-key todotxt-mode-map (kbd "j") 'next-line)               ; Vi Binding
;; (define-key todotxt-mode-map (kbd "k") 'previous-line)           ; Vi Binding


(define-key todotxt-mode-map "\C-c\C-g" 'todotxt-unhide-all)      ; (L)ist
;; (define-key todotxt-mode-map "\C-c\C-i" 'todotxt-show-incomplete) ; list (I)ncomplete
(define-key todotxt-mode-map "\C-c\C-x" 'todotxt-complete-toggle) ; (C)omplete item
;; (define-key todotxt-mode-map (kbd "a") 'todotxt-add-item)        ; (A)dd item
;; (define-key todotxt-mode-map (kbd "r") 'todotxt-add-priority)    ; Add p(r)iority
;; (define-key todotxt-mode-map "\C-c\C-a") 'todotxt-archive)         ; (A)rchive completed items
;; (define-key todotxt-mode-map (kbd "e") 'todotxt-edit-item)       ; (E)dit item
;; (define-key todotxt-mode-map (kbd "t") 'todotxt-tag-item)        ; (T)ag item
(define-key todotxt-mode-map "\C-c\C-s" 'todotxt-filter-for)      ; 
;; (define-key todotxt-mode-map (kbd "\\") 'todotxt-filter-out)     ; 
(define-key todotxt-mode-map "\C-c\C-p" 'todotxt-prioritize-item) ; priority++
(define-key todotxt-mode-map "\C-c\C-n" 'todotxt-deprioritize-item) ; priority--

;; Utility functions
(defun todotxt-current-line-re-match (re)
  "Test whether or not the current line contains text that
matches the provided regular expression"
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward re nil 't)
          (equal line-number (line-number-at-pos))
        nil))))

(defun todotxt-current-line-match (s)
  "Test whether or not the current line contains text that
matches the provided string"
  (let ((line-number (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (search-forward s nil 't)
          (equal line-number (line-number-at-pos))
        nil))))

(defun todotxt-complete-p ()
  "Returns whether or not the current line is 'complete'. Used as
part of a redefined filter for showing incomplete items only"
  (todotxt-current-line-re-match complete-regexp))

(defun todotxt-get-priority ()
  "If the current item has a priority, return it as a string.
Otherwise, return nil."
  (let* ((line (todotxt-get-current-line-as-string))
         (idx (string-match priority-regexp line)))
    (if idx
        (match-string-no-properties 1 line)
      nil)))

(defun todotxt-hide-line ()
  "Hides the current line, returns 't"
  (beginning-of-line)
  (let ((beg (point)))
    (forward-line)
    (let ((overlay (make-overlay beg (point))))
      (overlay-put overlay 'invisible 't))
    't))

(defun todotxt-line-empty-p ()
  "Returns whether or not the current line is empty."
  (save-excursion
    (beginning-of-line)
    (let ((b (point)))
      (end-of-line)
      (equal (point) b))))

(defun todotxt-jump-to-item (item)
  "Given the full text of an item, moves the point to the
beginning of the line containing that item."
  (todotxt-find-first-visible-char)
  (search-forward item)
  (beginning-of-line)
  (if (not (equal (overlays-at (point)) nil))
      (todotxt-find-first-visible-char)))

(defun todotxt-find-first-visible-char ()
  "Move the point to the next character in the buffer that does
not have an overlay applied to it.  This function exists to
address an odd bug in which the point can exist at (point-min)
even though it is invisible.  This usually needs to be called
after items are filtered in some way, but perhaps in other case
as well."
  (goto-char (point-min))
  (while (not (equal (overlays-at (point)) nil))
    (forward-char)))

(defun todotxt-filter (predicate)
  "Hides lines for which the provided predicate returns 't.  This
is our main filtering function that all others call to do their
work."
  (goto-char (point-min))
  (while (progn
           (if (and (not (todotxt-line-empty-p)) (funcall predicate))
               (todotxt-hide-line)
             (equal (forward-line) 0))))
  (todotxt-find-first-visible-char)
  (if (not (member predicate todotxt-active-filters))
      (setq todotxt-active-filters (cons predicate todotxt-active-filters))))

(defun todotxt-apply-active-filters ()
  (defun inner-loop (filters)
    (if (not (equal filters nil))
        (progn
          (todotxt-filter (car filters))
          (inner-loop (cdr filters)))))
  (inner-loop todotxt-active-filters)
  (todotxt-find-first-visible-char))

(defun todotxt-get-tag-completion-list-from-string (string)
  "Search the buffer for tags (strings beginning with either '@'
or '+') and return a list of them."
  (save-excursion
    (let ((completion-list '())
          (start-index 0))
      (while (string-match tags-regexp string start-index)
        (let ((tag (match-string-no-properties 0 string)))
          (if (not (member tag completion-list))
              (progn
                (setq completion-list (cons tag completion-list))))
          (setq start-index (match-end 0))))
      completion-list)))

(defun todotxt-get-current-line-as-string ()
  "Return the text of the line in which the point currently
resides."
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (buffer-substring beg (point)))))

(defun todotxt-prioritize-items ()
  "Performs a specialized sort of the lines in the buffer,
placing prioritized items in priority order at the top leaving
the other items' order unaltered."
  (remove-overlays)
  (let ((nextrecfun 'forward-line)
        (endrecfun 'end-of-line)
        (startkeyfun (lambda ()
                       (let ((priority (todotxt-get-priority)))
                         (if priority
                             priority
                           "a")))))
    (let ((origin (point)))
      (goto-char (point-min))
      (setq inhibit-read-only 't)
      (sort-subr nil nextrecfun endrecfun startkeyfun)
      (todotxt-apply-active-filters)
      (goto-char origin))
    (setq inhibit-read-only nil)))

(defun todotxt-get-formatted-date ()
  "Returns a string with the date formatted in standard todo.txt
format."
  (format-time-string "%Y-%m-%d"))

;;; externally visible functions
(defun todotxt ()
  "Open the todo.txt buffer.  If one already exists, bring it to
the front and focus it.  Otherwise, create one and load the data
from 'todotxt-file'."
  (interactive)
  (let* ((buf (find-file-noselect todotxt-file))
         (win (get-buffer-window buf)))
    (if (equal win nil)
        (progn
          (let* ((height (nth 3 (window-edges)))
                 (nheight (- height (/ height 3)))
                 (win (split-window (selected-window) nheight)))
            (select-window win)
            (switch-to-buffer buf)
            (todotxt-mode)
            (todotxt-prioritize-items)))
      (select-window win))
    (todotxt-find-first-visible-char)))

(defun todotxt-show-incomplete ()
  "Filter out complete items from the todo list."
  (interactive)
  (todotxt-filter 'todotxt-complete-p))

(defun todotxt-nuke-item ()
  "Deletes the current item without passing Go or collecting
$200"
  (interactive)
  (setq inhibit-read-only 't)
  (beginning-of-line)
  (kill-whole-line)
  (setq inhibit-read-only nil))

(defun todotxt-add-item (item)
  "Prompt for an item to add to the todo list and append it to
the file, saving afterwards."
  (interactive "sItem to add: ")
  (setq inhibit-read-only 't)
  (goto-char (point-max))
  (insert (concat
           (if todotxt-use-creation-dates
               (concat (todotxt-get-formatted-date) " "))
           item "\n"))
  (todotxt-prioritize-items)
  (if todotxt-save-after-change (save-buffer))
  (setq inhibit-read-only nil)
  (todotxt-jump-to-item item))

(defun todotxt-add-priority ()
  "Prompts for a priority from A-Z to be added to the current
item.  If the item already has a priority, it will be replaced.
If the supplied priority is lower case, it will be made upper
case.  If the input is the empty string, no priority will be
added, and if the item already has a priority, it will be
removed."
  (interactive)
  (let ((priority (read-from-minibuffer "Priority: ")))
    (if (or (and (string-match "[A-Z]" priority) (equal (length priority) 1))
            (equal priority ""))
        (save-excursion
          (setq inhibit-read-only 't)
          (if (todotxt-get-priority)
              (progn
                (beginning-of-line)
                (delete-char 4)))
          (if (not (equal priority ""))
              (progn
                (beginning-of-line)
                (insert (concat "(" (upcase priority) ") "))
                (setq inhibit-read-only nil)))
          (todotxt-prioritize-items)
          (if todotxt-save-after-change (save-buffer)))
      (error "%s is not a valid priority.  Try a letter between A and Z." priority))))

(defun todotxt-edit-item ()
  (interactive)
  (save-excursion
    (let ((new-text (read-from-minibuffer "Edit: " (todotxt-get-current-line-as-string))))
      (beginning-of-line)
      (setq inhibit-read-only 't)
      (kill-line)
      (insert new-text)
      (todotxt-prioritize-items)
      (if todotxt-save-after-change (save-buffer))
      (setq inhibit-read-only nil))))

(defun todotxt-tag-item ()
  (interactive)
  (let* ((new-tag (completing-read "Tags: " (todotxt-get-tag-completion-list-from-string (buffer-string))))e
         (new-text (concat (todotxt-get-current-line-as-string) " " new-tag)))
    (beginning-of-line)
    (setq inhibit-read-only 't)
    (kill-line)
    (insert new-text)
    (if todotxt-save-after-change (save-buffer))
    (setq inhibit-read-only nil)))

(defun todotxt-archive ()
  (interactive)
  (save-excursion
    (remove-overlays)
    (goto-char (point-min))
    (setq inhibit-read-only 't)    
    (while (progn
             (if (and (not (todotxt-line-empty-p)) (todotxt-complete-p))
                 (progn
                   (beginning-of-line)
                   (let ((beg (point)))
                     (forward-line)
                     (append-to-file beg (point) (concat (file-name-directory buffer-file-name) "/done.txt")))
                   (previous-line)
                   (kill-line 1)
                   't)
               (equal (forward-line) 0))))
    (if todotxt-save-after-change (save-buffer))
    (todotxt-apply-active-filters)
    (setq inhibit-read-only nil)))

(defun todotxt-bury ()
  (interactive)
  (bury-buffer)
  (delete-window))

(defun todotxt-unhide-all ()
  (interactive)
  (remove-overlays)
  (setq todotxt-active-filters '()))

(defun todotxt-filter-for (arg)
  "Filters the todo list for a specific tag or keyword.  Projects
and contexts should have their preceding '+' and '@' symbols,
respectively, if tab-completion is to be used."
  (interactive "p")
  (let* ((keyword (completing-read "Tag or keyword: " (todotxt-get-tag-completion-list-from-string (buffer-string)))))
    (if (equal arg 4)
        (todotxt-unhide-all))
    (goto-char (point-min))
                                        ; The contortions are to work around the lack of closures
    (todotxt-filter (eval `(lambda () (not (todotxt-current-line-match ,keyword)))))))

                                        ; Should probably be combined with filter-for
                                        ; TODO: evaluate utility of filter-for unhide-all functionality as a
                                        ; possible place for this to go
(defun todotxt-filter-out (arg)
  (interactive "p")
  (let* ((keyword (completing-read "Tag or keyword: " (todotxt-get-tag-completion-list-from-string (buffer-string)))))
    (save-excursion
      (if (equal arg 4)
          (todotxt-unhide-all))
      (goto-char (point-min))
                                        ; The contortions are to work around the lack of closures
      (todotxt-filter (eval `(lambda () (todotxt-current-line-match ,keyword)))))))

(defun todotxt-complete-toggle ()
  "Toggles the complete state for the item under the point In
accordance with the spec, this also adds a completion date to
completed items, and removes it if the item is being change to a
'not completed' state."
  (interactive)
  (setq inhibit-read-only 't)
  (if (todotxt-complete-p)
      (progn
        (beginning-of-line)
        (delete-char 2)
        (save-excursion
          (if (re-search-forward
               "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
               (+ (point) 11))
              (progn
                (beginning-of-line)
                (delete-char 11)))))
    (progn
      (beginning-of-line)
                                        ; This isn't in the spec, but the CLI version removes priorities
                                        ; upon completion.  It's problematic, because there's no good
                                        ; way to put them back if you toggle completion back to "not
                                        ; done".
      (if (todotxt-get-priority)
          (delete-char 4))
      (insert (concat "x " (todotxt-get-formatted-date) " "))
      (beginning-of-line)))
  ;;(todotxt-prioritize-items) ;; awdeorio: do not re-sort
  (setq inhibit-read-only nil)
  (if todotxt-save-after-change (save-buffer)))


;; awdeorio
(defun decrement-letter-at-point ()
  (interactive)
  (or (looking-at "[B-Z]")
      (error "Must be a letter [B-Z] at point"))
  (replace-match (char-to-string (1- (string-to-char (match-string 0))))))

;; awdeorio
(defun increment-letter-at-point ()
  (interactive)
  (or (looking-at "[A-Y]")
      (error "Must be a letter [A-Y] at point"))
  (replace-match (char-to-string (1+ (string-to-char (match-string 0))))))

;; awdeorio
(defun todotxt-prioritize-item ()
  "Increments the priority of this item."
  (interactive)
  (setq inhibit-read-only 't)
  (save-excursion
    (setq inhibit-read-only 't)
    (if (todotxt-get-priority)
        (progn
          (beginning-of-line)
          (forward-char)
          (decrement-letter-at-point)))
    (if (not (equal priority ""))
        (progn
          (beginning-of-line)
          (insert (concat "(A)"))
          (setq inhibit-read-only nil)))
    (if todotxt-save-after-change (save-buffer)))
  )

;; awdeorio
(defun todotxt-deprioritize-item ()
  "Increments the priority of this item."
  (interactive)
  (setq inhibit-read-only 't)
  (save-excursion
    (setq inhibit-read-only 't)
    (if (todotxt-get-priority)
        (progn
          (beginning-of-line)
          (forward-char)
          (increment-letter-at-point)))
    (if (not (equal priority ""))
        (progn
          (beginning-of-line)
          (insert (concat "(A)"))
          (setq inhibit-read-only nil)))
    (if todotxt-save-after-change (save-buffer)))
  )

(provide 'todotxt)
