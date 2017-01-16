;; .emacs
;; awdeorio's emacs customizations

;; Added by Package.el.  This must come before configurations of
;; installed packages.
(package-initialize)

; custom functions
(load-file "~/.emacs.d/elisp/functions.el")

; customizations go in ~/.elisp, check this dir first
(path-prepend "~/.emacs.d/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customized Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Command key mapped to meta
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

(global-set-key "\C-cf"                             'auto-fill-mode)
(global-set-key "\C-ci"                             'indent-to)
(global-set-key "\C-cl"                             'this-line-to-top-of-window)
(global-set-key "\C-cq"                             'query-replace)
(global-set-key "\C-cr"                             'query-replace-regexp)
(global-set-key "\C-\M-_"                           'redo)
(global-set-key "\C-c\C-s"                          'search-forward-regexp)
(global-set-key "\C-c\C-r"                          'search-backward-regexp)
(global-set-key [f2]                                'compile)
(global-set-key [f3]                                'next-error)
(global-set-key [f4]                                'kill-compilation)
(global-set-key [f5]                                'gud-next)
(global-set-key [f10]                               'gdb)
(global-set-key "\C-x\C-b"                          'electric-buffer-list)
(global-set-key "\C-c\C-c"                          'comment-region)
(global-set-key "\C-c\C-d"                          'hungry-delete-forward-ws)
(global-set-key "\C-c\C-h"                          'hungry-delete-ws)
(global-set-key [(control c) (control backspace)]   'hungry-delete-ws)
(global-set-key "\C-c\C-b"                          'browse-url-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-scroll-bar-mode                          -1) ; no scrollbar
(menu-bar-mode                                -1) ; no menubar
(tool-bar-mode                                -1) ; no buttonbar
(setq use-file-dialog                         -1) ; no GTK dialoge boxes
(setq inhibit-startup-message                  t) ; no startup message
(setq ring-bell-function                #'ignore) ; no audible bell
(setq line-number-mode                         t) ; show line numbers
(setq column-number-mode                       t) ; show column numbers
(global-font-lock-mode                         t) ; show syntax highlighting
(setq-default transient-mark-mode              t) ; highlight marked regions
(show-paren-mode                               t) ; parentheses matching

; window title is name of buffer
(setq frame-title-format "%b")

; Disable pop-up boxes in the GUI
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set the name of the initial buffer when no file is specified.
(setq target-buffer (get-buffer "*scratch*"))

; Set the default mode.
(setq default-mode 'fundamental-mode)

; Scroll at bottom of window one line at a time.
(setq scroll-step 1)

; Update string in the first 8 lines looking like Time-stamp: <> or " "
(add-hook 'write-file-hooks 'time-stamp)

; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

; display tabs as 2 spaces
(setq default-tab-width 2)

; default browser
; OSX
(defun rcy-browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url
	 (if (aref (url-generic-parse-url url) 0)
	     url
	   (concat "http://" url))))
    (start-process (concat "open " url) nil "open" url)))
 
; OSX
(setq browse-url-generic-program (executable-find "open")
      browse-url-browser-function 'browse-url-generic)

; Linux
; (setq browse-url-generic-program (executable-find "google-chrome")
;       browse-url-browser-function 'browse-url-generic)

(custom-set-variables
 '(truncate-lines t)                      ; do not wrap lines
 '(case-fold-search t)                    ; do not require case match in search
 '(current-language-environment "English")
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-yank-at-point t)                 ; yank at cursor, not mouse
 '(uniquify-buffer-name-style nil nil (uniquify))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; verilog mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(setq auto-mode-alist (cons '("\\.v\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vh\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("testfixture.verilog" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("testfixture.template" . verilog-mode) auto-mode-alist))

; user customization for Verilog mode
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.eml\\'" . text-mode) auto-mode-alist))
(setq default-fill-column 80)  ; width
;;(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . visual-line-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . visual-line-mode))
(add-to-list 'auto-mode-alist '("README" . visual-line-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; php mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'php-mode "php-mode" "Enter PHP mode." t)
(setq auto-mode-alist (cons '("\\.php\\'" . php-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphviz dot mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Enter Graphviz mode." t)
(setq auto-mode-alist (cons '("\\.dot\\'" . graphviz-dot-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AucTeX mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load "tex-site.el" nil t t)
;; (load "auctex.el" nil t t)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; ;(setq-default TeX-master nil) ; for multi-file docs (\include or \input)
;; (load "preview-latex.el" nil t t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'redo)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flyspell mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'flyspell-mode-on "flyspell" "On-the-fly ispell." t)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'html-mode-hook 'flyspell-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cperl-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ansi-color mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add color to a shell running in emacs M-x shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sh-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 's-mode "sh-mode" "Load sh-mode")
(add-to-list 'auto-mode-alist '("\\.*bashrc.*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.*cshrc.*\\'" . sh-mode))
(setq sh-basic-offset 2)
(setq sh-indentation  2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outline-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'outline-minor-mode-hook       ; C-m is the prefix key
          (lambda () (local-set-key "\C-m" outline-mode-prefix-map)))
(add-hook 'outline-mode-hook             ; C-m is the prefix key
          (lambda () (local-set-key "\C-m" outline-mode-prefix-map)))
;; (add-to-list 'auto-mode-alist '(".*outline.*" . outline-mode))
;(setq outline-minor-mode 1)              ; always enabled

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smv-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'smv-mode "smv-mode" "SMV mode.")
(add-to-list 'auto-mode-alist '("\\.smv$" . smv-mode))
(add-to-list 'auto-mode-alist '("\\.ord$" . smv-ord-mode))
(add-to-list 'completion-ignored-extensions ".ord")
(add-to-list 'completion-ignored-extensions ".opt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; todo.txt mode
;;https://github.com/rpdillon/todotxt.el/blob/master/readme.org
;; (require 'todotxt)
;; (setq todotxt-file "/home/awdeorio/Dropbox/todo/todo.txt")
;; (global-set-key (kbd "C-x t") 'todotxt)
(require 'todotxt)
(add-to-list 'auto-mode-alist '("todo.txt" . todotxt-mode))
(add-to-list 'auto-mode-alist '("bills.*" . todotxt-mode))
(add-hook 'todotxt-mode-hook 'goto-address-mode) ;; for URLs
(add-hook 'todotxt-mode-hook 'global-auto-revert-mode) ;; for Dropbo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode for Arduino files (.ino)
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))


;;; tab completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;(require 'gud)
(load-file "~/.emacs.d/elisp/gud.el")
