;;; init.el --- awdeorio's emacs customizations -*- lexical-binding: t -*-
;;;
;;; A few rare packages are provided in ~/.emacs.d/elisp/ .  More packages are
;;; automatically downloaded by use-package and placed in ~/.emacs.d/elpa/ .
;;;
;;; Start-up performance is important.  We use use-package to configure most
;;; packages at load time, not startup.  Summary of when sections evaluate.
;;; https://www.gnu.org/software/emacs/manual/html_mono/use-package.html
;;;   :defer t  Load when needed (on hook)
;;;   :hook     When to load package
;;;   :bind     Keybinding.  Includes hook and defered loading.
;;;   :preface  Runs immediately on startup
;;;   :init     Runs immediately on startup
;;;   :config   Runs later when package loads

;; User-provided packages go in ~/.emacs.d/elisp
;; https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Relocate automatically modified files.  These are lines added to
;; the file when you use the customise system. They're generated when
;; you use customize-*. By default, the customisation options are
;; stored in the init.el (or .emacs) file. You don't usually edit
;; these by hand.
;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file) (write-region "" nil custom-file))

;; Byte-compile init.el to init.elc on save, which cuts its load time to
;; about a third by expanding use-package macros once instead of every
;; startup.  Compile in a separate batch Emacs: use-package loads each
;; package while compiling, which would pollute this session.
;; early-init.el sets `load-prefer-newer', so a stale init.elc is skipped.
(defun init-recompile ()
  "Byte-compile init.el in a background Emacs."
  (interactive)
  (let ((file (expand-file-name "init.el" user-emacs-directory))
        (buffer (get-buffer-create "*init-recompile*")))
    (with-current-buffer buffer (erase-buffer))
    (make-process
     :name "init-recompile"
     :buffer buffer
     :command (list (expand-file-name invocation-name invocation-directory)
                    "--batch" "--eval" "(package-initialize)"
                    "-f" "batch-byte-compile" file)
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (message (if (eq 0 (process-exit-status proc))
                                "Compiled init.el"
                              "Compiling init.el failed, see *init-recompile*")))))))
(defun init-recompile-on-save ()
  "Run `init-recompile' if the current buffer visits init.el."
  (when (and buffer-file-name
             (file-equal-p buffer-file-name
                           (expand-file-name "init.el" user-emacs-directory)))
    (init-recompile)))
(add-hook 'after-save-hook #'init-recompile-on-save)

;; Modified keyboard shortcuts
(global-set-key "\C-ci"                         'indent-to)
(global-set-key "\C-c\C-s"                      'search-forward-regexp)
(global-set-key "\C-c\C-r"                      'search-backward-regexp)
(global-set-key "\M-o"                          'other-window)
(global-set-key "\C-x\C-b"                      'electric-buffer-list)
(global-set-key "\C-x\C-t"                      'insert-todays-date)
(global-set-key "\C-x\C-u"                      'browse-url-at-point)
(global-set-key "\C-cu"                         'clean-url-at-point)
(global-set-key "\C-cy"                         'insert-clean-url)

;; Org keybindings are set in the org use-package block below

;; macOS modifier keys
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)  ; Command == Meta
  (setq mac-option-modifier 'super)) ; Option == Super

;; Focus Emacs at startup when launched from a terminal on macOS
(when (eq window-system 'ns)
  (add-hook 'window-setup-hook (lambda () (x-focus-frame nil))))

;; Open link in browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program
      (cond ((eq system-type 'darwin) "open")
            ((eq system-type 'gnu/linux) "xdg-open")))
(global-set-key "\C-c\C-o" 'browse-url-at-point)

;; Dialog settings.  No more typing the whole yes or no. Just y or n
;; will do. Disable GUI dialogs and use emacs text interface.
(setq use-short-answers t)
(setq use-dialog-box nil)

;; Remove scrollbars, menu bars, and toolbars
;; These are now handled in early-init.el for faster startup
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Don't show a startup message
(setq inhibit-startup-message t)

;; Don't ring the terminal bell or flash the screen
(setq ring-bell-function 'ignore)

;; Show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Show syntax highlighting
(global-font-lock-mode t)

;; Highlight marked regions
(setq-default transient-mark-mode t)

;; Truncate long lines
(setq-default truncate-lines t)

;; Parentheses
(electric-pair-mode 1)                  ; automatically close parentheses, etc.
(show-paren-mode t)                     ; show matching parentheses

;; Smooth scrolling (one line at a time)
(setq scroll-step 1)

;; Update string in the first 8 lines looking like Time-stamp: <> or " "
(add-hook 'before-save-hook 'time-stamp)

;; Tab settings: 2 spaces.  See also: language-specific customizations below.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Line length for features like fill-paragraph (M-q)
(setq-default fill-column 79)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions

(defalias 'word-count 'count-words)

(defun reload-init-file ()
  "Reload user init file."
  (interactive)
  (load-file user-init-file)
  )

(defun insert-todays-date ()
  "Insert today's date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
)

(defun clean-url (url)
  "Return URL with tracking cruft and session junk stripped.
YouTube URLs are shortened to watch?v=ID, keeping any t= timestamp.
Amazon URLs are shortened to /dp/ASIN/, Home Depot URLs to /p/ID,
Menards URLs to /main/p-NNNN-c-NNNN.htm, and Gmail URLs have their
search/label/mailbox prefix replaced with #all/.  Any other URL has
its query string and fragment removed."
  (cond
   ;; YouTube: watch?v=ID, youtu.be/ID, shorts/ID -> watch?v=ID[&t=T]
   ((and (string-match-p
          (concat "\\`https?://\\(?:\\(?:www\\|m\\)\\.\\)?"
                  "\\(?:youtube\\.com\\|youtu\\.be\\)/")
          url)
         (or (string-match "[?&]v=\\([A-Za-z0-9_-]+\\)" url)
             (string-match "\\`https?://youtu\\.be/\\([A-Za-z0-9_-]+\\)" url)
             (string-match "/shorts/\\([A-Za-z0-9_-]+\\)" url)))
    (let ((id (match-string 1 url))
          (time (and (string-match "[?&#]t=\\([0-9hms]+\\)" url)
                     (match-string 1 url))))
      (concat "https://www.youtube.com/watch?v=" id
              (if time (concat "&t=" time) ""))))
   ;; Amazon: .../dp/ASIN/... or .../gp/product/ASIN/...
   ;;   -> https://www.amazon.com/dp/ASIN/
   ((string-match
     (concat "\\`https?://\\(?:www\\.\\)?amazon\\.com/\\(?:.*/\\)?"
             "\\(?:dp\\|gp/product\\)/\\([A-Z0-9]+\\)")
     url)
    (format "https://www.amazon.com/dp/%s/" (match-string 1 url)))
   ;; Home Depot: .../p/.../ID -> https://www.homedepot.com/p/ID
   ((string-match "\\`https?://\\(?:www\\.\\)?homedepot\\.com/p/.*/\\([0-9]+\\)"
                  url)
    (format "https://www.homedepot.com/p/%s" (match-string 1 url)))
   ;; Menards: .../p-NNNN-c-NNNN.htm -> https://www.menards.com/main/p-NNNN-c-NNNN.htm
   ((string-match
     "\\`https?://\\(?:www\\.\\)?menards\\.com/.*/\\(p-[0-9]+-c-[0-9]+\\)\\.htm"
     url)
    (format "https://www.menards.com/main/%s.htm" (match-string 1 url)))
   ;; Gmail: #search/query/ID or #sent/ID etc -> #all/ID
   ((string-match
     "\\`https?://mail\\.google\\.com/mail/u/\\([0-9]+\\)/#.*/\\([^/?#]+\\)\\'"
     url)
    (format "https://mail.google.com/mail/u/%s/#all/%s"
            (match-string 1 url) (match-string 2 url)))
   ;; General: strip query string and fragment
   (t (replace-regexp-in-string "[?#].*\\'" "" url))))

(defun clean-url-at-point ()
  "Replace the URL at point, or in the active region, with a cleaned version.
See `clean-url' for the cleanup rules applied."
  (interactive)
  (let* ((bounds (if (use-region-p)
                      (cons (region-beginning) (region-end))
                    (bounds-of-thing-at-point 'url)))
         (url (and bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (unless url
      (user-error "No URL found at point"))
    (let ((cleaned (clean-url (string-trim url))))
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert cleaned))))

(defun insert-clean-url ()
  "Insert a cleaned version of the URL on the clipboard/kill-ring at point.
Like `yank', set the mark at the start of the inserted text.  Signal an
error if the clipboard does not hold an http(s) URL.  See `clean-url'
for the cleanup rules applied."
  (interactive)
  (let ((url (string-trim (current-kill 0 t))))
    (unless (string-match-p "\\`https?://" url)
      (user-error "Clipboard does not contain a URL"))
    (push-mark)
    (insert (clean-url url))))

(defun from-pptx ()
  "Reformat entire buffer after paste from PowerPoint."
  (interactive)
  (save-excursion

    ;; Replace ^M (carriage return) with empty string
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "" nil t))

    ;; Replace ^K (vertical tab) with newline
    (goto-char (point-min))
    (while (search-forward "\x0b" nil t)
      (replace-match "\n" nil t))

    ;; Indent entire buffer
    (indent-region (point-min) (point-max))))

;; Exact pixel sizes, no rounding to character cells
(setq frame-resize-pixelwise t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager

;; Native compilation
;; Emacs 28+, optimize for speed, suppress compilation warnings
(when (featurep 'native-compile)
  (setq native-comp-speed 2)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Bug workaround emacs <26.3 connection to package repos
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(if (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Configure built-in package manager.  early-init.el sets
;; `package-quickstart', so packages are already activated by the time this
;; runs and there is no (require 'package) / (package-initialize) here.
;;
;; EXCEPTION: `emacs --batch' skips early-init.el, so nothing is activated
;; and every :ensure / :hook that needs a package fails with "Cannot open
;; load file".  Batch callers must run (package-initialize) before loading
;; this file; see ~/bin/upgrade.
;;
;; These settings only matter once package.el actually loads, which happens
;; on demand the first time something installs a package.
;; Emacs bundles its own (older) compat.el and reports it as already
;; installed, which silently blocks package.el from installing the newer
;; ELPA compat that magit/transient require.  Without this, a fresh
;; machine install ends up with a magit that half-loads and fails with
;; "Symbol's function definition is void: set-local" the first time
;; magit-status runs.
(setq package-install-upgrade-built-in t)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

;; Load and configure use-package (built in since Emacs 29, no bootstrap
;; needed).  Globally defer package loading.  eval-and-compile because
;; use-package expands at compile time; a plain setq is ignored in init.elc.
(eval-when-compile
  (require 'use-package))
(eval-and-compile
  (setq use-package-always-defer t))

;; `:ensure t' normally calls `package-installed-p', which pulls in all of
;; package.el (and url, browse-url, auth-source...) on every startup just to
;; answer "is it there?".  package-quickstart already leaves the answer in
;; `package-activated-list', so check that first and fall back to the real
;; thing only for a package that is genuinely missing.
(defun awdeorio-use-package-ensure (name args state &optional no-refresh)
  "Like `use-package-ensure-elpa', but skip loading package.el when every
package NAME/ARGS asks for is already in `package-activated-list'."
  (let ((all-active t))
    (dolist (ensure args)
      (let ((pkg (cond ((eq ensure t) (use-package-as-symbol name))
                       ((consp ensure) (car ensure))
                       (t ensure))))
        (when (and pkg
                   (not (memq pkg (bound-and-true-p package-activated-list))))
          (setq all-active nil))))
    (or all-active
        (use-package-ensure-elpa name args state no-refresh))))
(setq use-package-ensure-function #'awdeorio-use-package-ensure)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

;; More intuitive undo/redo.  M-_ undo, C-M-_ redo
;; https://github.com/emacsmirror/undo-fu
(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-M-_" . undo-fu-only-redo))
  :ensure t)

;; Integrated debugging mode for LLDB.
;; https://github.com/realgud/realgud/wiki/Debuggers-Available
(use-package realgud
  :ensure t
  :defer t
  )
(use-package realgud-lldb
  :ensure t
  :defer t
  )

;; Emacs Lisp mode customizations
(use-package elisp-mode
  :ensure nil
  :custom
  (indent-tabs-mode nil))

;; Verilog mode customizations
;;
;; NOTE to self: Take a look at verilog-ts-mode which includes tree-sitter
;;  support https://github.com/gmlarumbe/verilog-ext
(use-package verilog-mode
  ;; Built in to Emacs; no :ensure needed.
  :mode "\\.v\\'"
  :mode "\\.vh\\'"
  :mode "testfixture.verilog"
  :mode "testfixture.template"
  :config
  (setq verilog-indent-level             2
        verilog-indent-level-module      2
        verilog-indent-level-declaration 2
        verilog-indent-level-behavioral  2
        verilog-indent-level-directive   2
        verilog-case-indent              2
        verilog-cexp-indent              2
        verilog-case-indent              2
        verilog-auto-newline             nil
        verilog-auto-indent-on-newline   nil
        verilog-tab-always-indent        t
        verilog-auto-endcomments         t
        verilog-minimum-comment-distance 40
        verilog-indent-begin-after-if    'declarations
        verilog-auto-lineup              '(none))
  :defer t)

;; Unfill paragraph: join all lines into one long line
(defun unfill-paragraph ()
  "Replace newlines in current paragraph with spaces, producing one line."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

;; Text mode
(setq auto-mode-alist (cons '("README\\'" . text-mode) auto-mode-alist))
(add-hook 'text-mode-hook 'visual-line-mode)  ; wrap long lines
(add-hook 'text-mode-hook (lambda () (electric-pair-mode -1))) ; disable
(add-hook 'text-mode-hook
          (lambda () (local-set-key (kbd "M-q") 'unfill-paragraph)))

;; AUCTeX - enhanced LaTeX editing
;; https://www.gnu.org/software/auctex/
;;
;; Usage:
;;   C-c C-c  compile (LaTeX, BibTeX, View, etc.)
;;   C-c C-v  view PDF
;;   C-c C-e  insert environment (\begin{}...\end{})
;;   C-c C-s  insert section/chapter
;;   C-c =    table of contents (reftex)
;;   C-c [    insert citation (reftex)
;;   C-c (    insert label (reftex)
;;   C-c )    insert reference (reftex)
;; LSP via texlab:
;;   $ brew install texlab
(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex)
         (latex-mode . eglot-ensure)
         (LaTeX-mode . eglot-ensure))
  :config
  (setq TeX-auto-save t)                ; parse on save
  (setq TeX-parse-self t)               ; parse on load
  (setq-default TeX-master nil)         ; prompt for master file
  (setq TeX-PDF-mode t)                 ; default to PDF output
  (setq reftex-plug-into-AUCTeX t)      ; integrate reftex with AUCTeX
  (setq TeX-auto-local ".auto"))        ; hide AUCTeX cache


;; Spell checking backend: prefer hunspell (better dictionary) over ispell.
;;
;; Install hunspell and the SCOWL en_US-large dictionary on macOS:
;; https://wordlist.aspell.net/dicts/
;; Find the latest version at https://github.com/en-wl/wordlist/releases/latest
;;   $ brew install hunspell
;;   $ mkdir -p ~/Library/Spelling
;;   $ curl -sL https://github.com/en-wl/wordlist/releases/download/rel-YYYY.MM.DD/hunspell-en_US-large-YYYY.MM.DD.zip | tar -xvf- -C ~/Downloads/
;;
;; Add words to personal dictionary (~/.hunspell_en_US): M-x ispell-word RET i
(use-package ispell
  :if (executable-find "hunspell")
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US-large"))

;; Flyspell: "red squiggles" spell checking
;;
;; flyspell-prog-mode checks strings and comments only (skips identifiers).  It
;; checks words as you type or move past them.
;;
;; M-x flyspell-buffer  ; check whole buffer
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (latex-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)))

;; Flymake-languagetool: real-time grammar checking via flymake
;; https://github.com/emacs-languagetool/flymake-languagetool
;;
;; Install LanguageTool:
;;   $ brew install languagetool
;;
;; Runs a local LanguageTool HTTP server (auto-started on first check).
;; No data is sent to external servers.
;;
;; Provides inline grammar diagnostics (like flyspell, but for grammar).
;; Usage:
;;   M-x flymake-show-buffer-diagnostics
;;   M-x flymake-goto-next-error
;;   M-x flymake-goto-prev-error
(use-package flymake-languagetool

  ;; Disabled in favor of flywrite
  :disabled

  :ensure t
  :hook ((text-mode . flymake-languagetool-load)
         (text-mode . flymake-mode)
         (latex-mode . flymake-languagetool-load)
         (latex-mode . flymake-mode)
         (LaTeX-mode . flymake-languagetool-load)
         (LaTeX-mode . flymake-mode)
         (markdown-mode . flymake-languagetool-load)
         (markdown-mode . flymake-mode)
         (org-mode . flymake-languagetool-load)
         (org-mode . flymake-mode))
  :config
  (setq flymake-languagetool-language "en-US")
  (setq flymake-languagetool-server-jar
        "/opt/homebrew/opt/languagetool/libexec/languagetool-server.jar")

  ;; Disable WHITESPACE_RULE in org-mode because aligned tags (e.g.,
  ;; org-tags-column) produce repeated whitespace that triggers false positives.
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local flymake-languagetool-disabled-rules
                          '("WHITESPACE_RULE")))))

;; Flymake-popon: show flymake diagnostics in a popup near point
;; https://github.com/flymake/flymake-popon
(use-package flymake-popon
  :ensure t
  :hook (flymake-mode . flymake-popon-mode)
  :config
  ;; Disable minibuffer diagnostics (eldoc) since popon handles display
  (add-hook 'flymake-mode-hook
            (lambda ()
              (remove-hook 'eldoc-documentation-functions
                           #'flymake-eldoc-function t))))

;; Markdown mode (.md)
;;
;; Custom keys:
;;   C-c w  markdown-copy-region-as-html
;; Requires:
;;   $ brew install pandoc
(use-package markdown-mode
  :if (version<= "24.4" emacs-version)
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-gfm-use-electric-backquote nil)
  :hook (markdown-mode . visual-line-mode)
  ;; flyspell handled by flyspell use-package block
  :config
  ;; markdown-mode.el self-registers a broad auto-mode-alist entry for
  ;; markdown-mode on load; since add-to-list prepends, it shadows gfm-mode
  ;; above for every .md file after the first one opened each session.
  ;; Strip it, keeping our own .markdown entry (matched by regexp so this
  ;; survives if the package's own pattern changes upstream).
  (setq auto-mode-alist
        (cl-remove-if (lambda (entry)
                         (and (eq (cdr entry) 'markdown-mode)
                              (not (equal (car entry) "\\.markdown\\'"))))
                       auto-mode-alist))

  ;; Copy region as rich text for pasting into Gmail and Google Docs.  C-c w
  (require 'rich-text)
  (defun markdown-copy-region-as-html (beg end)
    "Convert the region from BEG to END to HTML with pandoc and copy it
as rich text.  See `copy-html-as-rich-text'."
    (interactive "r")
    (unless (use-region-p) (user-error "No active region"))
    (unless (executable-find "pandoc") (user-error "Need pandoc"))
    (let ((md (buffer-substring-no-properties beg end)))
      (copy-html-as-rich-text
       (with-temp-buffer
         (insert md)
         (unless (eq 0 (call-process-region (point-min) (point-max) "pandoc"
                                            t t nil "-f" "gfm" "-t" "html"))
           (error "pandoc failed: %s" (buffer-string)))
         (buffer-string))))
    (deactivate-mark))
  (define-key markdown-mode-map (kbd "C-c w") #'markdown-copy-region-as-html)
  :ensure t)

;; Polymode: edit Slidev YAML front matter blocks as yaml-mode
;;
;; Slidev decks (https://sli.dev), conventionally named slides.md, separate
;; slides with bare `---' lines; a `---' immediately followed by `key:
;; value' opens per-slide front matter, closed by another `---'.  Since a
;; bare separator and a front matter opener look identical, markdown-mode's
;; `markdown-use-pandoc-style-yaml-metadata' can't be used -- it treats
;; every `---' ... `---' pair as YAML, swallowing whole slides.  The
;; head-matcher below only accepts a real front matter block: next line
;; looks like a YAML key, closed by another `---' before an unrelated line.
;;
;; poly-markdown pulls in polymode; its keybinding setup is a separate
;; use-package block below.  :mode defers loading until a slides.md file is
;; opened, same as the markdown-mode block above but pointed at a mode this
;; :config defines rather than one the package ships.
(use-package polymode
  :ensure t
  :defer t
  :config
  ;; polymode claims M-n as a prefix key (`polymode-prefix-key'), shadowing
  ;; the M-n/M-p slide navigation bound below.  Move it to C-c n, as
  ;; polymode's own docs suggest.
  (define-key polymode-minor-mode-map (kbd "M-n") nil)
  (define-key polymode-minor-mode-map (kbd "C-c n") polymode-map)
  ;; Don't hide inner buffers by renaming them with a leading space.  The
  ;; base buffer gets hidden too while point is in a chunk, and Emacs adds a
  ;; random suffix when the hidden name is taken, so a revert could leave
  ;; the window showing " slides.md-318220" instead of slides.md.
  ;; Cost: inner buffers like slides.md[yaml] show up in C-x b.
  (setq pm-hide-implementation-buffers nil))

(use-package poly-markdown
  :ensure t
  :defer t
  :mode ("/slides\\.md\\'" . poly-slidev-mode)
  :config
  ;; poly-markdown.el self-registers ("\\.md\\'" . poly-markdown-mode) on
  ;; load, which would steal every markdown buffer away from gfm-mode.
  ;; Only /slides\.md\' -> poly-slidev-mode is wanted; strip its entry.
  (setq auto-mode-alist (rassq-delete-all 'poly-markdown-mode auto-mode-alist))

  (advice-add 'markdown-syntax-propertize :after
              #'poly-slidev--clear-markdown-properties-in-frontmatter)

  (require 'poly-slidev)
  (add-to-list 'mode-line-position '(:eval (slidev--mode-line-string)) t)

  (define-key poly-slidev-mode-map (kbd "M-n") #'poly-slidev-forward-slide)
  (define-key poly-slidev-mode-map (kbd "M-p") #'poly-slidev-backward-slide)
  (define-key poly-slidev-mode-map (kbd "C-c C-g") #'poly-slidev-goto-slide))

;; Shell programming
;;
;; Linting via shellcheck, which runs automatically through eglot +
;; bash-language-server.  Install both:
;;   $ brew install shellcheck
;;   $ brew install bash-language-server
(use-package sh-script
  :ensure nil  ; built-in
  :mode ("\\.bats\\'" . sh-mode)
  :hook (sh-mode . (lambda ()  ; skip eglot for .always_forget.txt
                     (unless (string-match-p "always_forget"
                                             (or (buffer-file-name) ""))
                       (eglot-ensure))))
  :config
  (setq-default sh-basic-offset tab-width)
  (setq-default sh-indentation tab-width))

;; Perl programming
;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(setq-default cperl-indent-level tab-width)

;; C and C++ programming.  Build with C-c m.  Rebuild with C-c c.  Put
;; this in c-mode-base-map because c-mode-map, c++-mode-map, and so
;; on, inherit from it.
;; (add-hook 'c-initialization-hook
;;           (lambda () (define-key c-mode-base-map (kbd "C-c m") 'compile)))
(add-hook 'c-initialization-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c c") 'recompile)))
(setq-default c-basic-offset tab-width) ; indentation
;; C/C++ eglot (non-tree-sitter fallback)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; C programming (fallback for Emacs without tree-sitter)
(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'auto-mode-alist '("\\.ino$" . c-mode)))  ; c-mode for Arduino

;; C++ programming (fallback for Emacs without tree-sitter)
(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))  ; assume C++ for .h files
  (add-to-list 'auto-mode-alist '("\\.h.starter$" . c++-mode))  ; EECS 280 starter files
  (add-to-list 'auto-mode-alist '("\\.cpp.starter$" . c++-mode)))  ; EECS 280 starter files

;; Go programming
;;
;; Install Go tools
;;   $ export GOPATH=${HOME}/.go
;;   $ export PATH=${PATH}:${GOPATH}/bin
;;   $ go install golang.org/x/tools/gopls@latest      # gopls (LSP server)
;;   $ go get golang.org/x/tools/cmd/...               # godoc
;;   $ go get -v -u golang.org/x/lint/golint           # golint
;;   $ go get -u github.com/nsf/gocode                 # gocode
;;   $ go get -v github.com/rogpeppe/godef             # godef
;;   $ go get -u github.com/derekparker/delve/cmd/dlv  # dlv
;;
;; Ref: "Configure Emacs as a Go Editor From Scratch"
;; https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
(use-package go-mode
  :mode "\\.go\\'"
  :init
  ;; Remove go-mode's Go assembly check, which loads go-mode on every
  ;; file open, even non-Go files
  (setq magic-mode-alist (assq-delete-all 'go--is-go-asm magic-mode-alist))
  :hook (go-mode . eglot-ensure)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)  ; Call gofmt before save
  ;; :bind (("M-." . godef-jump)    ; Jump to definition
  ;;        ("M-*" . pop-tag-mark)  ; Go back
  ;;        )
  :ensure t
  :defer t
)
(use-package go-dlv
  ;; Integrated debugger support
  :after go-mode  ; lazy loading
  :ensure t
  :defer t
)

;; Web Development
;;   C-c f f  toggle folding
(use-package web-mode
  :mode "\\.mjs\\'"
  :mode "\\.jsx?\\'"
  :mode "\\.tsx?\\'"
  :mode "\\.html?\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  :bind (:map web-mode-map
              ("C-c f f" . web-mode-fold-or-unfold))
  :ensure t
  :defer t
  )

;; CMake
;;
;; LSP support via cmake-language-server.  Install:
;;   $ pipx install cmake-language-server
(use-package cmake-mode
  :ensure t
  :hook (cmake-mode . eglot-ensure)
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'")

;; C/C++ (built-in tree-sitter mode, Emacs 29+)
;; Install grammars: M-x treesit-install-language-grammar RET c RET
;;                   M-x treesit-install-language-grammar RET cpp RET
;; LSP via clangd:
;;   $ brew install llvm  (or clangd comes with Xcode Command Line Tools)
(use-package c-ts-mode
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c++-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.ino\\'" . c-ts-mode)
         ("\\.h\\.starter\\'" . c++-ts-mode)
         ("\\.cpp\\.starter\\'" . c++-ts-mode))
  :hook ((c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure))
  :config
  (setq c-ts-mode-indent-offset tab-width))

;; TypeScript (built-in tree-sitter mode, Emacs 29+)
;; Install grammar: M-x treesit-install-language-grammar RET typescript RET
;; LSP:
;;   $ brew install typescript-language-server
(use-package typescript-ts-mode
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :ensure nil
  :hook (typescript-ts-mode . eglot-ensure)
  :mode "\\.ts\\'")

;; Autocomplete for words and filenames (text buffers).
;; M-/ expands words from buffer and completes filenames.
;; In prog-mode, corfu overrides M-/ for code completion (see below).
(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        ))

;; Autocomplete for code (prog-mode buffers).
;; M-/ triggers code completion, overriding hippie-expand (see above).
;; Corfu docs: https://github.com/minad/corfu
;; NOTE: Wrapped in `when` because :if doesn't prevent :ensure in older use-package.
(when (version<= "29.1" emacs-version)
  (use-package corfu
    :hook (prog-mode . corfu-mode)
    :config
    ;; Manual completion with M-/, subsequent M-/ cycles through options
    (setq corfu-auto nil)                 ; disable auto-popup
    (setq corfu-cycle t)                  ; loop back to first candidate
    (define-key prog-mode-map (kbd "M-/") 'completion-at-point) ; manual trigger
    (define-key corfu-map (kbd "M-/") 'corfu-next) ; cycle
    :ensure t))

;; Autocomplete for chunks of code with Generative AI
;; https://github.com/copilot-emacs/copilot.el
;;
;; M-x copilot-install-server
;; M-x copilot-mode
;; M-x copilot-login
;; M-x copilot-diagnose
;;
;; NOTE: Uses `eval` to defer macro expansion because :vc keyword requires
;; Emacs 30+ use-package.  Eager macro expansion would error on older Emacs.
(when (version<= "30.0" emacs-version)
  (eval
   '(use-package copilot
      :commands (copilot-mode copilot-complete)
      :init
      (define-key prog-mode-map (kbd "C-M-/") 'copilot-complete-or-cycle)
      :config
      (setq copilot-idle-delay nil)  ; disable auto-completion, trigger manually
      (setq copilot-cycle-completion t)  ; cycle back to first after last completion
      (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "C-g") 'copilot-clear-overlay)
      (define-key copilot-completion-map (kbd "C-RET") 'copilot-accept-completion-by-line)
      :vc (:url "https://github.com/copilot-emacs/copilot.el"
                :rev :newest
                :branch "main"))))

;; Lazy-load copilot: the package is not loaded until the user presses
;; C-M-/ for the first time.  This improves startup time.
(defun copilot-complete-or-cycle ()
  "Enable copilot-mode if needed, trigger completion, or cycle to next.
On first call, loads copilot package and enables copilot-mode.
Subsequent calls cycle through available completions."
  (interactive)
  (unless (bound-and-true-p copilot-mode)
    (copilot-mode 1))
  (if (copilot--overlay-visible)
      (copilot-next-completion)
    (copilot-complete)))

;; Python virtual environment support
;; M-x pyvenv-activate    activate a venv by directory
;; M-x pyvenv-workon      activate from $WORKON_HOME
;; M-x pyvenv-deactivate  deactivate current venv
(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)
         (python-ts-mode . pyvenv-mode))
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

;; Python eglot (non-tree-sitter fallback)
;; LSP:
;;   $ pipx install python-lsp-server
(add-hook 'python-mode-hook 'eglot-ensure)

;; Python (built-in tree-sitter mode, Emacs 29+)
;; Install grammar: M-x treesit-install-language-grammar RET python RET
;; Falls back to python-mode if tree-sitter or grammar unavailable.
(use-package python
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :ensure nil
  :hook (python-ts-mode . eglot-ensure)
  :mode ("\\.py\\'" . python-ts-mode))

;; Remote file editing with TRAMP.  Configure TRAMP to use the same SSH
;; multiplexing that I configure in ~/.ssh/config.  By default, TRAMP ignores my
;; SSH config's multiplexing configuration, so configure the same settings here.
;; https://www.emacswiki.org/emacs/TrampMode
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlMaster auto "
         "-o ControlPersist yes "
         "-o ControlPath ~/.ssh/socket-%%C "
         "-o ServerAliveInterval 60 "
         "-o ServerAliveCountMax 5 "
         ))
  (setq tramp-use-ssh-controlmaster-options nil)
  :defer t
)

;; YAML mode
;; LSP:
;;   $ brew install yaml-language-server
(use-package yaml-mode
  :mode "\\.yml\\'"
  :hook (yaml-mode . eglot-ensure)
  :ensure t
  :defer t)

;; JSON mode
;; LSP:
;;   $ npm install -g vscode-langservers-extracted
(use-package json-mode
  :mode "\\.json\\'"
  :hook (json-mode . eglot-ensure)
  :config
  (setq js-indent-level 2)
  :ensure t
  :defer t)

;; JSONC (JSON with Comments) - use js-mode which supports // and /* */ comments
(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-mode))
(add-hook 'js-mode-hook (lambda () (setq-local js-indent-level 2)))
(add-hook 'js-mode-hook 'eglot-ensure)

;; .env file support
(use-package dotenv-mode
  :mode "\\.env\\'"
  :mode "\\.env\\..*\\'"
  :ensure t
  :defer t)

;; Code folding (prog-mode only, see web-mode for HTML/web files)
;;   C-c f f  toggle hiding
;;   C-c f a  toggle hide-all/show-all
(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)
         (web-mode . (lambda () (hs-minor-mode -1))))
  :config
  (setq hs-hide-comments-when-hiding-all nil)  ; Do not hide comments
  (setq hs-isearch-open t)  ; Unhide code and comments during search

  (defvar hs-all-hidden nil "Track whether all blocks are hidden.")
  (make-variable-buffer-local 'hs-all-hidden)

  (defun hs-toggle-all ()
    "Toggle between `hs-hide-all' and `hs-show-all'."
    (interactive)
    (if hs-all-hidden
        (progn (hs-show-all) (setq hs-all-hidden nil))
      (progn (hs-hide-all) (setq hs-all-hidden t))))

  :bind (:map hs-minor-mode-map
              ("C-c f a" . hs-toggle-all)
              ("C-c f f" . hs-toggle-hiding)))

;; Outline - folding and heading navigation (markdown, latex)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Outline-Mode.html
;;
;; Custom keys:
;;   TAB    outline-cycle (on a heading only)
;;   S-TAB  outline-cycle-buffer (on a heading only)
;;   n      outline-next-visible-heading (at start of a heading only)
;;   p      outline-previous-visible-heading (at start of a heading only)
;;   f      outline-forward-same-level (at start of a heading only)
;;   b      outline-backward-same-level (at start of a heading only)
;;   u      outline-up-heading (at start of a heading only)
;;   d      outline-down-heading (at start of a heading only)
;;   c      outline-cycle (at start of a heading only)
(use-package outline
  :hook ((markdown-mode . outline-minor-mode)
         (latex-mode . outline-minor-mode)
         (LaTeX-mode . outline-minor-mode))

  :custom
  ;; TAB/S-TAB cycle only on headings; elsewhere they work as usual
  (outline-minor-mode-cycle t)

  ;; Org-style speed keys, active only at the beginning of a heading line
  :bind (:map outline-minor-mode-map
         :filter (and (bolp) (outline-on-heading-p t))
         ("n" . outline-next-visible-heading)
         ("p" . outline-previous-visible-heading)
         ("f" . outline-forward-same-level)
         ("b" . outline-backward-same-level)
         ("u" . outline-up-heading)
         ("d" . outline-down-heading)
         ("c" . outline-cycle))

  :config
  ;; Go down to the first child heading, the opposite of `outline-up-heading'
  (defun outline-down-heading ()
    "Show the children of the current heading and move to the first one."
    (interactive)
    (outline-back-to-heading t)
    (let ((start (point))
          (level (funcall outline-level)))
      (outline-show-children)
      (outline-next-heading)
      (unless (and (not (eobp)) (> (funcall outline-level) level))
        (goto-char start)
        (user-error "No child heading")))))

;; Dockerfile syntax highlighting
;; LSP:
;;   $ npm install -g dockerfile-language-server-nodejs
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :hook (dockerfile-mode . eglot-ensure)
  :ensure t
  :defer t
)

;; ediff graphical diff viewer
;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html
;;
;; Custom keybindings:
;;   cap  A→B prepend (insert A before B)
;;   can  A→B append  (insert A after B)
;;   cbp  B→A prepend (insert B before A)
;;   cbn  B→A append  (insert B after A)
;;   u    reload buffers from disk and recompute diffs
(use-package ediff
  :ensure nil
  :defer t

  ;; :custom runs at startup, setting variables via customize-set-variable.
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)

  ;; :config runs when ediff is first invoked.  Function here use ediff
  ;; internals.  They are defined after ediff loads.
  :config
  (defun ediff-copy-a-to-b-prepend ()
    "Insert difference region from buffer A before the region in buffer B."
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (let* ((n ediff-current-difference)
           (a-begin (ediff-get-diff-posn 'A 'beg n))
           (a-end (ediff-get-diff-posn 'A 'end n))
           (b-begin (ediff-get-diff-posn 'B 'beg n))
           (b-end (ediff-get-diff-posn 'B 'end n))
           (a-content (ediff-with-current-buffer ediff-buffer-A
                        (buffer-substring a-begin a-end)))
           (b-content (ediff-with-current-buffer ediff-buffer-B
                        (buffer-substring b-begin b-end))))
      (ediff-with-current-buffer ediff-buffer-B
        (goto-char b-begin)
        (insert a-content))

      ;; Save original B region for restoration with `rb'
      (ediff-save-diff-region n 'B b-content)
      (ediff-clear-fine-differences n)
      (ediff-refresh-mode-lines)))

  (defun ediff-copy-a-to-b-append ()
    "Insert difference region from buffer A after the region in buffer B."
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (let* ((n ediff-current-difference)
           (a-begin (ediff-get-diff-posn 'A 'beg n))
           (a-end (ediff-get-diff-posn 'A 'end n))
           (b-begin (ediff-get-diff-posn 'B 'beg n))
           (b-end (ediff-get-diff-posn 'B 'end n))
           (a-content (ediff-with-current-buffer ediff-buffer-A
                        (buffer-substring a-begin a-end)))
           (b-content (ediff-with-current-buffer ediff-buffer-B
                        (buffer-substring b-begin b-end))))
      (ediff-with-current-buffer ediff-buffer-B
        (goto-char b-end)
        (insert a-content))
      (ediff-save-diff-region n 'B b-content)
      (ediff-clear-fine-differences n)
      (ediff-refresh-mode-lines)))

  (defun ediff-copy-b-to-a-prepend ()
    "Insert difference region from buffer B before the region in buffer A."
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (let* ((n ediff-current-difference)
           (a-begin (ediff-get-diff-posn 'A 'beg n))
           (a-end (ediff-get-diff-posn 'A 'end n))
           (b-begin (ediff-get-diff-posn 'B 'beg n))
           (b-end (ediff-get-diff-posn 'B 'end n))
           (a-content (ediff-with-current-buffer ediff-buffer-A
                        (buffer-substring a-begin a-end)))
           (b-content (ediff-with-current-buffer ediff-buffer-B
                        (buffer-substring b-begin b-end))))
      (ediff-with-current-buffer ediff-buffer-A
        (goto-char a-begin)
        (insert b-content))
      (ediff-save-diff-region n 'A a-content)
      (ediff-clear-fine-differences n)
      (ediff-refresh-mode-lines)))

  (defun ediff-copy-b-to-a-append ()
    "Insert difference region from buffer B after the region in buffer A."
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (let* ((n ediff-current-difference)
           (a-begin (ediff-get-diff-posn 'A 'beg n))
           (a-end (ediff-get-diff-posn 'A 'end n))
           (b-begin (ediff-get-diff-posn 'B 'beg n))
           (b-end (ediff-get-diff-posn 'B 'end n))
           (a-content (ediff-with-current-buffer ediff-buffer-A
                        (buffer-substring a-begin a-end)))
           (b-content (ediff-with-current-buffer ediff-buffer-B
                        (buffer-substring b-begin b-end))))
      (ediff-with-current-buffer ediff-buffer-A
        (goto-char a-end)
        (insert b-content))
      (ediff-save-diff-region n 'A a-content)
      (ediff-clear-fine-differences n)
      (ediff-refresh-mode-lines)))

  ;; Keybindings extend the existing 'c' prefix (ca, cb).  We use :hook instead
  ;; of :bind because ediff dynamically creates its keymap; :bind would fail
  ;; with "Key sequence starts with non-prefix key" since 'c' isn't defined
  ;; as a prefix until ediff-keymap-setup runs.
  ;; c = copy, a/b = source buffer, p/n = prepend/next(append)
  ;;   cap  A→B prepend (insert A before B)
  ;;   can  A→B append  (insert A after B)
  ;;   cbp  B→A prepend (insert B before A)
  ;;   cbn  B→A append  (insert B after A)
  :hook
  (ediff-keymap-setup . (lambda ()
                          (define-key ediff-mode-map "u"
                            (lambda () (interactive)
                              (ediff-revert-buffers-then-recompute-diffs t)))
                          ;; (define-key ediff-mode-map "cap" 'ediff-copy-a-to-b-prepend)
                          ;; (define-key ediff-mode-map "can" 'ediff-copy-a-to-b-append)
                          ;; (define-key ediff-mode-map "cbp" 'ediff-copy-b-to-a-prepend)
                          ;; (define-key ediff-mode-map "cbn" 'ediff-copy-b-to-a-append)
                          ))
  )

;; Git
(use-package magit
  :ensure t
  :defer t
  )

;; Better org agenda views
;; https://github.com/alphapapa/org-super-agenda/
;; NOTE: Wrapped in `when` because :if doesn't prevent :ensure in older use-package.
(when (version<= "28.1" emacs-version)
  (use-package org-super-agenda
    :ensure t
    :defer t
    :after org)

  (use-package org-ql
    :ensure t
    :defer t
    :after org))

;; Org mode
;;
;; Custom keybindings:
;;   C-c l         store link
;;   C-c a         agenda
;;   C-c c         capture
;;   C-c w         copy region as HTML rich text (org-mode-map)
;;   C-c e         paste Gmail URL as org link (org-mode-map)
;;   C-M-S-<down>  move subtree to bottom of parent
;;   C-M-S-<up>    move subtree to top of parent
;;   Z             move subtree to bottom of parent (speed key)
;;   A             move subtree to top of parent (speed key)
;;   N             last heading in buffer (speed key)
;;   P             first heading in buffer (speed key)
;;   n             next visible heading or list item (speed key)
;;   p             previous visible heading or list item (speed key)
;;   d             first child heading or list item (speed key)
;; Speed keys also work on list items.
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c e"        . org-insert-gmail-link)
         ("C-c w"        . org-copy-region-as-html)
         ("C-M-S-<down>" . org-move-subtree-to-bottom)
         ("C-M-S-<up>"   . org-move-subtree-to-top))
  :init
  (setq org-use-speed-commands t)
  (setq org-log-done 'time)
  (setq org-tags-column 59)
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files
        (list "home.org"
              "work.org"))
  (setq org-agenda-search-view-always-boolean t)
  ;; Don't load org's default link modules, which slow the first org file open
  (setq org-modules nil)
  (add-hook 'org-mode-hook 'auto-revert-mode) ; for Dropbox

  :config
  ;; Helper commands live in elisp/org-extras.el
  (require 'org-extras)

  ;; Enable org element cache for faster parsing
  (setq org-element-use-cache t)

  ;; Table alignment treats currency like $3,770 and ~$119,000 as
  ;; numbers, so those columns right-align without <r> cookies.  Same
  ;; as the default regexp plus an optional leading ~ and $, and commas.
  (setq org-table-number-regexp
        (concat "^\\([<>]?[-+~]?\\$?[-+^.,0-9]*[0-9]"
                "[-+^.,0-9eEdDx()%:]*"
                "\\|[<>]?[-+]?0[xX][[:xdigit:].]+"
                "\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+"
                "\\|nan\\|[-+u]?inf\\)$"))

  ;; TODO keywords as workflow states for GTD
  ;; https://orgmode.org/manual/Workflow-states.html
  ;;
  ;; Multiple keyword sets in one file
  ;; https://orgmode.org/manual/Multiple-sets-in-one-file.html
  ;;
  ;; Faces for TODO keywords
  ;; M-x list-colors-display  ; show color names
  ;; https://orgmode.org/manual/Faces-for-TODO-keywords.html
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE")
          (sequence "HOLD" "TODO" "|" "DONE")
          (sequence "WAIT" "|" "RECV" "CANC" "NANA")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "dark red" :weight bold))
          ("DONE" . (:foreground "dark green" :weight bold))
          ("CANC" . (:foreground "dark green" :weight bold))
          ("NANA" . (:foreground "dark green" :weight bold))
          ("RECV" . (:foreground "dark green" :weight bold))
          ("WAIT" . (:foreground "orange" :weight bold))
          ("HOLD" . (:foreground "orange" :weight bold))))

  ;; Do not fold org files in ediff mode
  (add-hook 'ediff-prepare-buffer-hook #'org-show-all)

  ;; Move subtree to bottom/top of its parent.  Speed keys Z, A
  (add-to-list 'org-speed-commands '("Z" . org-move-subtree-to-bottom) t)
  (add-to-list 'org-speed-commands '("A" . org-move-subtree-to-top) t)

  ;; Go to first/last visible heading in the buffer.  Speed keys P, N
  (add-to-list 'org-speed-commands '("N" . org-goto-last-heading) t)
  (add-to-list 'org-speed-commands '("P" . org-goto-first-heading) t)

  ;; Speed keys on plain list items.  Headings and items form one
  ;; continuous outline.  Item keys are active from column 0 through
  ;; the bullet.
  (add-hook 'org-speed-command-hook #'org-item-speed-command-activate t)

  ;; Heading n/p/d walk into list items too
  (setf (alist-get "n" org-speed-commands nil nil #'equal)
        'org-speed-next-visible)
  (setf (alist-get "p" org-speed-commands nil nil #'equal)
        'org-speed-previous-visible)
  (add-to-list 'org-speed-commands '("d" . org-speed-down) t)

  ;; How far ahead to list SCHEDULED items in agenda
  (setq org-deadline-warning-days 1)

  ;; Capture templates
  ;; https://orgmode.org/manual/Capture-templates.html
  (setq org-capture-templates
        '(
          ("h" "Home"
           entry (file "home.org")
           "* TODO %?\n  %i\n  %a")

          ("w" "Work"
           entry (file "work.org")
           "* TODO %?\n  %i\n  %a")

          ("l" "Letter"
           entry (file+headline "work.org" "Letters")
           "* HOLD Recommendation for NAME %? (CATEGORY)\nDEADLINE: TBD\n\nSTUDENT EMAIL\n\nLINK TO EMAIL THREAD\n\nI would be happy to help.  I would be able to comment on your performance in EECS FIXME, where it looks like you got an FIXME and your rank was FIXME/FIXME = top FIXME percent.\n\nIf you're planning to apply to multiple schools, please sign up for the confidential Interfolio reference letter service.  I can upload one letter and then they will submit copies to each school. https://account.interfolio.com/signup\n\nWhen is the first deadline?\n\n")

          )
        )

  ;; Custom agenda views
  ;; https://github.com/alphapapa/org-ql
  ;; https://github.com/alphapapa/org-ql/blob/master/examples.org
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((org-ql-block

             '(and
               (or
                (priority "A")
                (deadline auto) ;; has deadline set and due today or overdue
                (scheduled :to today)
                )
               (not (done))
               )

             ((org-ql-block-header "Today Dashboard"))
             )))))

  ;; M-/ tag completion in the tag-entry minibuffer (C-c C-c)
  (add-hook 'minibuffer-setup-hook #'org-tags-minibuffer-hippie-expand-setup)
  ) ; closes (use-package org ...)

;; EditorConfig - per-project coding style from .editorconfig files
;; https://editorconfig.org
;;
;; Global mode, enabled after init so files on the command line get it.
(use-package editorconfig
  :hook (after-init . editorconfig-mode))

;; version482 required by EECS 482 autograder
;; Automatically loads when "eecs482" appears anywhere in the full path
;; (use-package version482
;;   :commands version482-mode
;;
;;   ;; Automatically start version485-mode when abs path contains "eecs482"
;;   :hook (find-file . (lambda ()
;;                        (when (and buffer-file-name
;;                                   (string-match-p "eecs482" buffer-file-name))
;;                          (version482-mode 1))))
;;
;;   ;; Install from GitHub
;;   :load-path "~/.emacs.d/site-lisp/version482.el"
;;   )

;; fcat personal finance tool - fcat.settings.jsonc support Provides category
;; completion via `completion-at-point' and `fcat-insert-rule'.
(use-package fcat
  :load-path "~/src/fcat/contrib/emacs"
  :mode ("fcat\\.settings\\.jsonc\\'" . fcat-settings-mode)
  :hook (fcat-settings-mode . (lambda () (setq-local js-indent-level 2)))
  :bind (:map fcat-minor-mode-map
              ("M-/" . fcat-complete-category)))

;; flywrite DEVELOPMENT
(use-package flywrite
  :load-path "~/src/flywrite"
  :commands (flywrite-mode)

  ;; Fast navigation among errors
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))

  :init
  ;; Register safe file-local values before package is loaded
  (put 'flywrite-system-prompt 'safe-local-variable
       (lambda (v) (memq v '(prose academic))))

  ;; Auto-enable flywrite-mode for files in ~/src/flywrite/samples/
  ;; Split window: file on top, log on bottom
  (let ((samples-eval
         '(progn
            (flywrite-mode 1)
            (delete-other-windows)
            (let ((log-buf (get-buffer-create "*flywrite-log*")))
              (split-window-below)
              (other-window 1)
              (switch-to-buffer log-buf)
              (other-window 1)))))
    (add-to-list 'safe-local-eval-forms samples-eval)
    (dir-locals-set-class-variables
     'flywrite-samples
     `((nil . ((eval . ,samples-eval))))))
  (dir-locals-set-directory-class
   (expand-file-name "~/src/flywrite/samples") 'flywrite-samples)

  ;; Anthropic API configuration
  :config
  (setq flywrite-api-url "https://api.anthropic.com/v1/messages")
  (setq flywrite-api-key-file "~/.flywrite-api-key")

  ;; Feedback configuration
  (setq flywrite-system-prompt 'academic)

  (defun reload-flywrite ()
    "Reload flywrite.el and restart flywrite-mode in the current buffer."
    (interactive)
    (let ((was-active (bound-and-true-p flywrite-mode)))
      (when was-active (flywrite-mode -1))
      (when (get-buffer "*flywrite-log*")
        (kill-buffer "*flywrite-log*"))
      (load-file (expand-file-name "~/src/flywrite/flywrite.el"))
      (when was-active (flywrite-mode 1))
      (message "flywrite reloaded")))
  )

(use-package flymake-popon
  :hook (flymake-mode . flymake-popon-mode)
  :ensure t)

;; CSV mode, with spreadsheet-style navigation
;; https://elpa.gnu.org/packages/csv-mode.html
;;
;; Custom keybindings, mimicking macOS Excel (Command is Meta):
;;   <right>    beginning of next field, this row only
;;   <left>     beginning of previous field, this row only
;;   M-<down>   jump to the edge of the data block below, in this column
;;   M-<up>     jump to the edge of the data block above, in this column
;;
;; The M-arrow jumps follow Excel's Command-Arrow rules: from a filled
;; field next to another filled one, stop at the last filled field before
;; a blank; otherwise skip the blank run and stop at the first filled
;; field; with nothing filled left, go to the last record.  Empty,
;; whitespace-only and "" fields count as blank, as do rows too short to
;; reach the column.  Blank and comment lines break a block.
;;
;; Built-in csv-mode keybindings worth remembering:
;;   TAB        next field, creating one at end of line
;;   S-TAB      beginning of previous field
;;   C-M-f      forward one field, wrapping to the next row
;;   C-<down>   forward one record block, ignoring columns
(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :hook ((csv-mode . csv-align-mode)
         ;; csv-mode derives from text-mode, whose hook turns on
         ;; visual-line-mode.  Wrapped lines break column alignment.
         (csv-mode . csv--disable-visual-line-mode))
  :config

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

  ;; Command is Meta, so M-<down> is what Excel's Command-Down-Arrow sends
  (define-key csv-mode-map (kbd "M-<down>") #'csv-forward-data-edge)
  (define-key csv-mode-map (kbd "M-<up>") #'csv-backward-data-edge)
  ;; Plain arrows step one column, as in Excel; C-f and C-b still move
  ;; point by one character
  (define-key csv-mode-map (kbd "<right>") #'csv-forward-column)
  (define-key csv-mode-map (kbd "<left>") #'csv-backward-column)
  )
