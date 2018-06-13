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
(global-set-key "\C-x\C-b"                          'electric-buffer-list)
(global-set-key [(control \')]                      'other-window)

;; macOS modifier keys
(setq mac-command-modifier 'meta) ; Command == Meta
(setq mac-option-modifier 'super) ; Option == Super

;; Open link in browser.  Settings for OSX.
(setq browse-url-browser-function (quote browse-url-generic))
(defvar browse-url-generic-program "open")
(global-set-key "\C-c\C-b" 'browse-url-at-point)

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

;; Package Management.  Configure the built-in emacs package manager to use
;; several publicly available repositories.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package' and enable it.  Later, 'use-package- will
;; download and install third-party packages automatically.
;; http://cachestocaches.com/2015/8/getting-started-use-package/
;;
;; EXAMPLE:
;; (use-package foo-mode
;;   :after bar      ; load after bar package
;;   :mode "\\.foo"  ; load and enable foo-mode for *.foo files
;;   :init           ; run this code when init.el is read
;;   :config         ; run this code after loading foo-mode
;;   :ensure t       ; automatically install foo-mode if not present
;;   :defer t        ; defer loading for performance (usually the default)
;; )
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; More intuitive undo/redo.  M-_ undo, C-M-_ redo
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key "\C-\M-_" 'undo-tree-redo)
  :ensure t
  )

;; Integrated debugging mode for LLDB.  However, prefer GDB over LLDB
;; because GDB integration is much better.
;; https://www.reddit.com/r/emacs/comments/6qbwjl/seriously_how_can_i_use_lldb_in_emacs/
(use-package gud-lldb
  :commands gud-lldb
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
  )

;; Text mode
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(add-hook 'text-mode-hook 'visual-line-mode)  ; wrap long lines
(add-hook 'text-mode-hook 'flyspell-mode)     ; spell check

;; LaTeX mode
(add-hook 'latex-mode-hook 'visual-line-mode) ; wrap long lines
(add-hook 'latex-mode-hook 'flyspell-mode)    ; spell check

;; Todo.txt mode
;; NOTE: The package is "todotxt", but the mode is "todotxt-mode":
;; https://github.com/rpdillon/todotxt.el/blob/master/readme.org
(use-package todotxt
  :mode ("todo.txt" . todotxt-mode)
  :mode ("bills.*" . todotxt-mode)
  :config
  (add-hook 'todotxt-mode-hook 'goto-address-mode) ; for URLs
  (add-hook 'todotxt-mode-hook 'global-auto-revert-mode) ; for Dropbox
  (add-hook 'todotxt-mode-hook (lambda () (visual-line-mode -1))) ; disable
  (add-hook 'todotxt-mode-hook (lambda () (flyspell-mode -1))) ; disable
)

;; Markdown mode (.md)
;; Only works on Emacs 24.4 +
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  :init
  (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook 'visual-line-mode) ; wrap long lines
  (add-hook 'markdown-mode-hook 'flyspell-mode) ; check spelling
  (setq markdown-fontify-code-blocks-natively t) ; syntax highlight code blocks
  :ensure t
  )
)

;; Markdown ToC generator
;; https://github.com/ardumont/markdown-toc
;; Only works on Emacs 24.4 +
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
(use-package markdown-toc
  :commands markdown-toc-generate-toc
  :commands markdown-toc-refresh-toc
  :ensure t
  )
)

;; Shell programming
(setq-default sh-basic-offset tab-width)
(setq-default sh-indentation tab-width)

;; Perl programming
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
  :defer 1  ; lazy loading
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
  :defer 1  ; lazy loading
  )

;; Python backend for autocomplete
;; https://github.com/syohex/emacs-company-jedi
;; You may need to:
;; $ pip install virtualenv
;; Only works on Emacs 24.4 +
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
(use-package company-jedi
  :after company                        ; lazy loading
  :init
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
  :ensure t
  )
)

;; Remote file editing with TRAMP.  Configure TRAMP to use the same SSH
;; multiplexing that I configure in ~/.ssh/config.  By default, TRAMP ignore my
;; SSH config's multiplexing configuration, so configure the same settings here.
;; https://www.emacswiki.org/emacs/TrampMode
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=~/.ssh/master-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes"))
  :defer 1  ; lazy loading
)
