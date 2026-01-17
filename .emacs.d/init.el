;;; init.el --- awdeorio's emacs customizations -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; A few rare packages are provided in ~/.emacs.d/elisp/ .  More packages are
;;; automatically downloaded by use-package and placed in ~/.emacs.d/elpa/ .
;;;
;;; Code:

;; Startup time measurement
(defconst emacs-start-time (current-time))

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

;; Modified keyboard shortcuts
(global-set-key "\C-ci"                             'indent-to)
(global-set-key "\C-c\C-s"                          'search-forward-regexp)
(global-set-key "\C-c\C-r"                          'search-backward-regexp)
(global-set-key "\M-o"                              'other-window)
(global-set-key "\C-x\C-b"                          'electric-buffer-list)
(global-set-key "\C-x\C-t"                          'insert-todays-date)
(global-set-key "\C-x\C-u"                          'browse-url-at-point)
;; Org keybindings are set in the org use-package block below

;; macOS modifier keys
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)  ; Command == Meta
  (setq mac-option-modifier 'super)) ; Option == Super

;; Function aliases
(defalias 'word-count 'count-words)

;; Open link in browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program
      (cond ((eq system-type 'darwin) "open")
            ((eq system-type 'gnu/linux) "xdg-open")))
(global-set-key "\C-c\C-o" 'browse-url-at-point)

;; Dialog settings.  No more typing the whole yes or no. Just y or n
;; will do. Disable GUI dialogs and use emacs text interface.
(fset 'yes-or-no-p 'y-or-n-p)
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

;; Convenience function to relad this file
(defun reload-init-file ()
  "Reload user init file."
  (interactive)
  (load-file user-init-file)
  )

;; Native compilation
;; Emacs 28+, optimize for speed, suppress compilation warnings
(when (featurep 'native-compile)
  (setq native-comp-speed 2)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Bug workaround emacs <26.3 connection to package repos
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(if (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Configure built-in package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-defer t)  ; Globally defer package loading

;; More intuitive undo/redo.  M-_ undo, C-M-_ redo
;; https://www.emacswiki.org/emacs/UndoTree
;; Deferred via :bind until first undo/redo command for faster startup
(use-package undo-tree
  :bind (("C-/" . undo-tree-undo)
         ("C-M-_" . undo-tree-redo))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil)
  :ensure t)

;; Dark Mode Theme
(use-package spacemacs-common
    :ensure spacemacs-theme
    :defer t
    ;:config (load-theme 'spacemacs-dark t)
    )

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
(use-package emacs-lisp-mode
  :ensure nil
  :custom
  (lisp-indent-offset 2))

;; Verilog mode customizations
;;
;; NOTE to self: Take a look at verilog-ts-mode which includes tree-sitter
;;  support https://github.com/gmlarumbe/verilog-ext
(use-package verilog-mode
  :ensure t
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

;; Text mode
(setq auto-mode-alist (cons '("README\\'" . text-mode) auto-mode-alist))
(add-hook 'text-mode-hook 'visual-line-mode)  ; wrap long lines
(add-hook 'text-mode-hook (lambda () (electric-pair-mode -1))) ; disable

;; LaTeX mode
(add-hook 'latex-mode-hook 'visual-line-mode) ; wrap long lines

;; Flyspell (spell checking) - deferred until mode activation for faster startup
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (latex-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)))

;; Sort function is useful in todo.txt mode
(defun sort-buffer ()
  "Sort buffer."
  (interactive)
  (mark-whole-buffer)
  (sort-lines nil (point-min) (point-max))
)

;; Todo.txt mode
;; NOTE: The package is "todotxt", but the mode is "todotxt-mode":
;; https://github.com/rpdillon/todotxt.el/blob/master/readme.org
(use-package todotxt
  :mode ("todo.txt" . todotxt-mode)
  :mode ("someday.txt" . todotxt-mode)
  :mode ("bills.*" . todotxt-mode)
  :config
  (add-hook 'todotxt-mode-hook 'goto-address-mode) ; for URLs
  (add-hook 'todotxt-mode-hook 'auto-revert-mode) ; for Dropbox
  (add-hook 'todotxt-mode-hook (lambda () (visual-line-mode -1))) ; disable
  (add-hook 'todotxt-mode-hook (lambda () (flyspell-mode -1))) ; disable
  (add-hook 'todotxt-mode-hook (lambda () (electric-pair-mode -1))) ; disable
  :defer t
)

;; Markdown mode (.md)
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
  :ensure t)

;; Markdown ToC generator
;; https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :if (version<= "24.4" emacs-version)
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc)
  :ensure t)

;; Shell programming
(setq-default sh-basic-offset tab-width)
(setq-default sh-indentation tab-width)

;; Perl programming
;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(setq-default cperl-indent-level tab-width)

;; C and C++ programming.  Build with C-c m.  Rebuild with C-c c.  Put
;; this in c-mode-base-map because c-mode-map, c++-mode-map, and so
;; on, inherit from it.
(add-hook 'c-initialization-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c m") 'compile)))
(add-hook 'c-initialization-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c c") 'recompile)))
(setq-default c-basic-offset tab-width) ; indentation

;; C programming (fallback for Emacs without tree-sitter)
(unless (treesit-available-p)
  (add-to-list 'auto-mode-alist '("\\.ino$" . c-mode)))  ; c-mode for Arduino

;; C++ programming (fallback for Emacs without tree-sitter)
(unless (treesit-available-p)
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))  ; assume C++ for .h files
  (add-to-list 'auto-mode-alist '("\\.h.starter$" . c++-mode))  ; EECS 280 starter files
  (add-to-list 'auto-mode-alist '("\\.cpp.starter$" . c++-mode)))  ; EECS 280 starter files

;; Go programming
;;
;; Install Go tools
;;   $ export GOPATH=${HOME}/.go
;;   $ export PATH=${PATH}:${GOPATH}/bin
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
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)  ; Call gofmt before save
  :bind (("M-." . godef-jump)    ; Jump to definition
         ("M-*" . pop-tag-mark)  ; Go back
         )
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
(use-package web-mode
  :mode "\\.jsx?\\'"
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
  :ensure t
  :defer t
  )

;; CMake
(use-package cmake-mode
  :ensure t
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'")

;; C/C++ (built-in tree-sitter mode, Emacs 29+)
;; Install grammars: M-x treesit-install-language-grammar RET c RET
;;                   M-x treesit-install-language-grammar RET cpp RET
(use-package c-ts-mode
  :if (treesit-available-p)
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c++-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.ino\\'" . c-ts-mode)
         ("\\.h\\.starter\\'" . c++-ts-mode)
         ("\\.cpp\\.starter\\'" . c++-ts-mode))
  :config
  (setq c-ts-mode-indent-offset tab-width))

;; TypeScript (built-in tree-sitter mode, Emacs 29+)
;; Install grammar: M-x treesit-install-language-grammar RET typescript RET
(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")

;; Autocomplete for words and filenames (text buffers).
;; M-/ expands words from buffer and completes filenames.
;; In prog-mode, company-mode overrides M-/ for code completion (see below).
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
;; Company docs: https://company-mode.github.io/
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (company-tng-configure-default)       ; use default configuration

  ;; Manual completion with M-/, subsequent M-/ cycles through options
  (setq company-idle-delay nil)         ; disable auto-popup
  (setq company-selection-wrap-around t) ; loop back to first candidate
  (define-key prog-mode-map (kbd "M-/") 'company-complete) ; manual trigger
  (define-key company-active-map (kbd "M-/") 'company-select-next) ; cycle
  :ensure t)

;; Eglot - LSP client providing IDE features (completions, diagnostics, etc.)
;; Eglot feeds completions to company-mode via the company-capf backend.
;;  $ pipx install python-lsp-server
;;  $ go install golang.org/x/tools/gopls@latest
;;  $ brew install llvm  (or clangd comes with Xcode Command Line Tools)
;;  $ brew install typescript-language-server
;;  $ brew install bash-language-server
;;  $ brew install yaml-language-server
;;  $ npm install -g vscode-langservers-extracted  (json)
;;  $ npm install -g dockerfile-language-server-nodejs
;;  $ brew install texlab
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (dockerfile-mode . eglot-ensure)
         (latex-mode . eglot-ensure)))

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
(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure t
  :defer t
)

;; Code folding
;;
;; In programming major modes, hs-minor-mode provides folding functionality.
;; In text modes, outline-minor-mode provides folding functionality.  We use
;; fold-dwim to integrate the different folding behaviors in different minor
;; modes.
(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))  ; Enable folding in programming modes
  :defer t
  :config

  (use-package fold-dwim
    :hook ((text-mode . outline-minor-mode))  ; Enable folding in text mode
    :config
    (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
    (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
    (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)
    :ensure t
    :defer t
    )
  )

;; Dockerfile syntax highlighting
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :ensure t
  :defer t
)

(defun insert-todays-date ()
  "Insert today's date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
)

;; ediff graphical diff viewer
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Change window width from 80 to 160
(add-hook 'ediff-after-setup-windows-hook (lambda () (if (window-system) (set-frame-width (selected-frame) 160))))

;; Restore windows after Ediff quits
(use-package winner
  :init
  (winner-mode 1)
  :config
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  (add-hook 'ediff-after-quit-hook-internal (lambda () (if (window-system) (set-frame-width (selected-frame) 80)))))

;; Git
;; (use-package magit
;;   :ensure t
;;   :defer t
;;   )

;; Better org agenda views
;; https://github.com/alphapapa/org-super-agenda/
(use-package org-super-agenda
  :ensure t
  :defer t
  :after org
  )

(use-package org-ql
  :ensure t
  :defer t
  :after org)

(use-package org
  :defer t
  :init
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-log-done 'time)
  (setq org-tags-column 59)
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files
        (list "home.org"
              "work.org"))
  (setq org-agenda-search-view-always-boolean t)
  (add-hook 'org-mode-hook 'auto-revert-mode) ; for Dropbox

  :config
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

  ;; Syntax highlighting for special property CREATED
  (font-lock-add-keywords
   'org-mode
   '(("^\\([ \t]*CREATED:\\)[ \t]*\\(.*\\)"
      (1 'org-special-keyword t) ; Highlight `CREATED:` property
      (2 'org-date t))))        ; Highlight the timestamp

  ;; Do not fold org files in ediff mode
  (add-hook 'ediff-prepare-buffer-hook #'org-show-all)

  (defun org-add-created-property ()
    "Add a :CREATED: property with the current timestamp as a special property.
If the :CREATED: property already exists, do nothing."
    (interactive)
    (when (org-at-heading-p) ;; Ensure we are at a heading
      (save-excursion
        (org-back-to-heading t)
        (forward-line 1)
        (let ((created-regexp "^:CREATED:"))
          ;; Check if the :CREATED: property already exists
          (unless (re-search-forward created-regexp (line-end-position) t)
            (insert (format "CREATED: %s\n" (format-time-string "[%Y-%m-%d %a]"))))))))

  ;; Archive all tasks in this file marked DONE
  (defun org-archive-done-tasks-file ()
    (interactive)
    (org-map-entries
     (lambda ()
       (let ((current-keyword (org-get-todo-state)))
         (when (member current-keyword '("DONE" "RECV" "CANC" "NANA" ))
           (org-archive-subtree)
           (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))))
     nil 'file))

  ;; Add file local mode variable enabling org mode
  (defun org-insert-file-local-mode-variable ()
    "Insert today's date in YYYY-MM-DD format."
    (interactive)
    (insert "-*- mode: org; -*-\n")
    )

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
           "* HOLD Recommendation for NAME %? (CATEGORY)\nDEADLINE: TBD\n\nSTUDENT EMAIL\n\nLINK TO EMAIL THREAD\n\nI would be happy to help.  I would be able to comment on your performance in EECS FIXME, where it looks like you got an FIXME and your rank was FIXME/FIXME = top FIXME percentile.\n\nIf you're planning to apply to multiple schools, please sign up for the confidential Interfolio reference letter service.  I can upload one letter and then they will submit copies to each school. https://account.interfolio.com/signup\n\nWhen is the first deadline?\n\n")

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
  )

;; editorconfig
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure t
  :hook (prog-mode . editorconfig-mode))

;; copilot
;; https://github.com/copilot-emacs/copilot.el
;;
;; M-x copilot-install-server
;; M-x copilot-mode
;; M-x copilot-login
;; M-x copilot-diagnose
;;
;; This installation method (:vc) works on Emacs 30+
(use-package copilot
  :if (version<= "30.0" emacs-version)
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-g") 'copilot-clear-overlay)
  (define-key copilot-completion-map (kbd "M-n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion-by-line)
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

;; Display startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time emacs-start-time))
                     gcs-done)))
