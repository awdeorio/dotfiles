; .emacs
; awdeorio's emacs customizations

; Custom packages go in ~/.emacs.d/elisp
; https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Emacs automatically saved information in a separate file
;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

; Custom keyboard shortcuts
(global-set-key "\C-ci"                             'indent-to)
(global-set-key "\C-cl"                             'this-line-to-top-of-window)
(global-set-key "\C-c\C-s"                          'search-forward-regexp)
(global-set-key "\C-c\C-r"                          'search-backward-regexp)
(global-set-key "\C-x\C-b"                          'electric-buffer-list)
(global-set-key "\C-c\C-b"                          'browse-url-at-point)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

; macOS modifier keys
(setq mac-command-modifier 'meta) ; Command == Meta
(setq mac-option-modifier 'super) ; Option == Super

;; Remove scrollbars, menu bars, and toolbars
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; Misc appearance
(setq use-file-dialog                         -1) ; no GTK dialoge boxes
(setq inhibit-startup-message                  t) ; no startup message
(setq ring-bell-function                #'ignore) ; no audible bell
(setq line-number-mode                         t) ; show line numbers
(setq column-number-mode                       t) ; show column numbers
(global-font-lock-mode                         t) ; show syntax highlighting
(setq-default transient-mark-mode              t) ; highlight marked regions
(show-paren-mode                               t) ; parentheses matching
(setq-default truncate-lines                   t) ; truncate long lines
(setq frame-title-format "%b")                    ; Window title = buffer name

; Disable pop-up boxes in the GUI
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

; Smooth scrolling (one line at a time)
(setq scroll-step 1)

; Update string in the first 8 lines looking like Time-stamp: <> or " "
(add-hook 'write-file-hooks 'time-stamp)

; Tab settings: 2 spaces
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'sh-basic-offset 'tab-width)
(defvaralias 'sh-indentation 'tab-width)

; Automatically close parentheses, braces, etc.
(electric-pair-mode 1)

; Default browser
(setq browse-url-generic-program (executable-find "open") ; OSX
      browse-url-browser-function 'browse-url-generic)
; (setq browse-url-generic-program (executable-find "google-chrome") ; Linux
;       browse-url-browser-function 'browse-url-generic)

(setq-default truncate-lines t)

; Package Management
;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
;; http://cachestocaches.com/2015/8/getting-started-use-package/
;;
;; Configured with this reference
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; More intuitive Undo/Redo
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key "\C-\M-_" 'redo)
  :ensure t
  )

;; GUD mode for LLDB
(use-package gud-lldb
  :commands gud-lldb
)

; Verilog mode customizations
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
  ; can't use :ensure because this package isn't available on package manager
  )

; Text mode
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(add-hook 'text-mode-hook 'visual-line-mode)  ; wrap long lines
(add-hook 'text-mode-hook 'flyspell-mode)     ; spell check

; LaTeX mode
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)

; Todo.txt mode
; NOTE: The package is "todotxt", but the mode is "todotxt-mode":
; https://github.com/rpdillon/todotxt.el/blob/master/readme.org
(use-package todotxt
  :mode ("todo.txt" . todotxt-mode)
  :mode ("bills.*" . todotxt-mode)
  :config
  (add-hook 'todotxt-mode-hook 'goto-address-mode) ; for URLs
  (add-hook 'todotxt-mode-hook 'global-auto-revert-mode) ; for Dropbox
  (add-hook 'todotxt-mode-hook (lambda () (visual-line-mode -1))) ; disable
  (add-hook 'todotxt-mode-hook (lambda () (flyspell-mode -1))) ; disable
)

; C-mode for Arduino files (.ino)
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))

;; C++-mode for .h files
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Markdown mode (.md)
(use-package markdown-mode
  ; load and enable markdown-mode for *.md files
  :mode "\\.md\\'"
  ; run this code after loading markdown-mode
  :config
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  ; automatically install markdown-mode using package manager on first use
  :ensure t
  )

;; Markdown ToC generator
;; https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :commands markdown-toc-generate-toc
  :commands markdown-toc-refresh-toc
  :ensure t
  )

; Web Development
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
  )

;; fly-check
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
)

;; Tab completion
;; Company docs: https://company-mode.github.io/
;; Company TNG: https://github.com/company-mode/company-mode/issues/526
(use-package company
  :config
  (company-tng-configure-default)       ; use default configuration
  (global-company-mode)                 ; enable tab completion in all modes
  :ensure t
  )

;; Python backend for tab completion
;; https://github.com/syohex/emacs-company-jedi
;; You may need to:
;; $ pip install virtualenv
(use-package company-jedi
  :init
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
  :ensure t
  )
