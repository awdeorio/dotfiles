; .emacs
; awdeorio's emacs customizations

; Custom packages go in ~/.emacs.d/elisp
; https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

; Required packages
(require 'redo)
(require 'gud-lldb)

; Custom keyboard shortcuts
(global-set-key "\C-ci"                             'indent-to)
(global-set-key "\C-cl"                             'this-line-to-top-of-window)
(global-set-key "\C-\M-_"                           'redo)
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
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

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

; Default mode
(setq default-mode 'fundamental-mode)

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

; Emacs Package Manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

; Verilog mode customizations
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(setq auto-mode-alist (cons '("\\.v\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vh\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("testfixture.verilog" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("testfixture.template" . verilog-mode) auto-mode-alist))
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

; Text mode
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.eml\\'" . text-mode) auto-mode-alist))
(setq default-fill-column 80)  ; width
;(add-hook 'text-mode-hook 'visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . visual-line-mode))
(add-to-list 'auto-mode-alist '("README" . visual-line-mode))

; LaTex mode
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)

; PHP mode
(autoload 'php-mode "php-mode" "Enter PHP mode." t)
(setq auto-mode-alist (cons '("\\.php\\'" . php-mode) auto-mode-alist))

; Graphviz dot mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter Graphviz mode." t)
(setq auto-mode-alist (cons '("\\.dot\\'" . graphviz-dot-mode) auto-mode-alist))

; Spell checking mode
(autoload 'flyspell-mode-on "flyspell" "On-the-fly ispell." t)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'html-mode-hook 'flyspell-mode)

; Outline mode
(add-hook 'outline-minor-mode-hook       ; C-m is the prefix key
          (lambda () (local-set-key "\C-m" outline-mode-prefix-map)))
(add-hook 'outline-mode-hook             ; C-m is the prefix key
          (lambda () (local-set-key "\C-m" outline-mode-prefix-map)))

; Todo.txt mode
; https://github.com/rpdillon/todotxt.el/blob/master/readme.org
(require 'todotxt)
(add-to-list 'auto-mode-alist '("todo.txt" . todotxt-mode))
(add-to-list 'auto-mode-alist '("bills.*" . todotxt-mode))
(add-hook 'todotxt-mode-hook 'goto-address-mode) ; for URLs
(add-hook 'todotxt-mode-hook 'global-auto-revert-mode) ; for Dropbox

; C-mode for Arduino files (.ino)
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))

; Markdown mode (.md)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

; Web Development
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-auto-indentation nil)
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

; Tab completion
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(defun clever-hippie-tab (arg)
  "Ordinary tab or dabbrev"
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   (t (indent-for-tab-command))))

(global-set-key (kbd "TAB") 'clever-hippie-tab)
