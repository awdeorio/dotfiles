;;; init.el --- awdeorio's emacs customizations
;;;
;;; Commentary:
;;; A few rare packages are provided in ~/.emacs.d/elisp/ .  More packages are
;;; automatically downloaded by use-package and placed in ~/.emacs.d/elpa/ .
;;;
;;; Code:


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

;; macOS modifier keys
(setq mac-command-modifier 'meta) ; Command == Meta
(setq mac-option-modifier 'super) ; Option == Super

;; Open link in browser.  Settings for OSX.
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "open")
(global-set-key "\C-c\C-o" 'browse-url-at-point)

;; Dialog settings.  No more typing the whole yes or no. Just y or n
;; will do. Disable GUI dialogs and use emacs text interface.
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Remove scrollbars, menu bars, and toolbars
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
(add-hook 'write-file-hooks 'time-stamp)

;; Tab settings: 2 spaces.  See also: language-specific customizations below.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Line length for features like fill-paragraph (M-q)
(setq-default fill-column 79)

;; Bug workaround emacs <26.3 connection to package repos
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(if (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Built in package manager
(require 'package)
(package-initialize)  ; FIXME this is slow with EMacs <27 https://emacs.stackexchange.com/questions/38368/how-can-i-improve-startup-time-despite-many-packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Install use-package
;; https://github.com/jwiegley/use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Automatically update packages installed by use-package periodically
(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  :ensure t
  :defer t
  )

;; More intuitive undo/redo.  M-_ undo, C-M-_ redo
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key "\C-\M-_" 'undo-tree-redo)
  (setq undo-tree-auto-save-history nil)
  :ensure t
  :defer t
  )

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

;; Verilog mode customizations
(use-package verilog-mode
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
  ;; can't use :ensure because this package isn't available on package manager
  :defer t
  )

;; Text mode
(setq auto-mode-alist (cons '("README\\'" . text-mode) auto-mode-alist))
(add-hook 'text-mode-hook 'visual-line-mode)  ; wrap long lines
(add-hook 'text-mode-hook 'flyspell-mode)     ; spell check
(add-hook 'text-mode-hook (lambda () (electric-pair-mode -1))) ; disable

;; LaTeX mode
(add-hook 'latex-mode-hook 'visual-line-mode) ; wrap long lines
(add-hook 'latex-mode-hook 'flyspell-mode)    ; spell check

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
  (add-hook 'todotxt-mode-hook 'global-auto-revert-mode) ; for Dropbox
  (add-hook 'todotxt-mode-hook (lambda () (visual-line-mode -1))) ; disable
  (add-hook 'todotxt-mode-hook (lambda () (flyspell-mode -1))) ; disable
  (add-hook 'todotxt-mode-hook (lambda () (electric-pair-mode -1))) ; disable
  :defer t
)

;; Markdown mode (.md)
;; Only works on Emacs 24.4 +
(unless (version< emacs-version "24.4")
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  :init
  (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook 'visual-line-mode) ; wrap long lines
  (add-hook 'markdown-mode-hook 'flyspell-mode) ; check spelling
  (setq markdown-fontify-code-blocks-natively t) ; syntax highlight code blocks
  (setq markdown-gfm-use-electric-backquote nil) ; no interactive code block insertion
  :ensure t
  :defer t
  )
)

;; Markdown ToC generator
;; https://github.com/ardumont/markdown-toc
;; Only works on Emacs 24.4 +
(unless (version< emacs-version "24.4")
(use-package markdown-toc
  :commands markdown-toc-generate-toc
  :commands markdown-toc-refresh-toc
  :ensure t
  :defer t
  )
)

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

;; C programming
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))  ; c-mode for Arduino

;; C++ programming
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))  ; assume C++ for .h files
(add-to-list 'auto-mode-alist '("\\.h.starter$" . c++-mode))  ; EECS 280 starter files
(add-to-list 'auto-mode-alist '("\\.cpp.starter$" . c++-mode))  ; EECS 280 starter files

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
(use-package company-go
  ;; Autocomplete
  :after company  ; lazy loading
  :init
  (add-hook 'go-mode-hook (lambda () (add-to-list 'company-backends 'company-go)))
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

;; Intellisense syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :config

  ;; enable in all modes
  (global-flycheck-mode)

  ;; disable jshint since we prefer eslint checking
  ;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  ;; C++11
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

  :ensure t
  :defer t
)

;; Autocomplete for words and filenames.  M-/ auto-completes a word
;; from other words in the buffer and filenames.
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

;; Autocomplete for code
;; Company docs: https://company-mode.github.io/
;; Company TNG: https://github.com/company-mode/company-mode/issues/526
(use-package company
  :config
  (company-tng-configure-default)       ; use default configuration
  (global-company-mode)
  :ensure t
  :defer t
  )

;; Python backend for autocomplete
;; https://github.com/syohex/emacs-company-jedi
;; You may need to:
;; $ pip install virtualenv
;; Only works on Emacs 24.4 +
;; (unless (version< emacs-version "24.4")
;; (use-package company-jedi
;;   :after company                        ; lazy loading
;;   :init
;;   (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
;;   :ensure t
;;   :defer t
;;   )
;; )

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
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(add-hook 'ediff-after-quit-hook-internal (lambda () (if (window-system) (set-frame-width (selected-frame) 80))))

;; Do not fold org files in ediff mode
(with-eval-after-load 'outline
  (add-hook 'ediff-prepare-buffer-hook #'org-show-all))

;; Git
;; (use-package magit
;;   :ensure t
;;   :defer t
;;   )

(use-package org
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done 'time)
  (setq org-tags-column 59)
  (setq org-agenda-files
        (list "~/Dropbox/org/home.org"
              "~/Dropbox/org/work.org"))
  (add-hook 'org-mode-hook 'global-auto-revert-mode) ; for Dropbox

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
          (sequence "WAIT" "|" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "dark red" :weight bold))
          ("DONE" . (:foreground "dark green" :weight bold))
          ("WAIT" . (:foreground "orange" :weight bold))))

  ;; Archive all tasks in this file marked DONE
  (defun org-archive-done-tasks-file ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file))
)
