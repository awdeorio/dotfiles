;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Runs before init.el and before the package system and GUI are initialized.
;;; This is the best place for startup optimizations.
;;;
;;; Code:

;; Do not load site-start.el (system-wide config that can be slow)
(setq site-run-file nil)

;; Increase garbage collection threshold during startup (default is 800KB)
;; This dramatically reduces GC pauses during init
(setq gc-cons-threshold most-positive-fixnum)

;; Disable file-name-handler-alist during startup (speeds up file loading)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore defaults after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))  ; 16MB - reasonable for interactive use
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Prevent package.el from loading packages before init.el runs
;; use-package handles this instead
(setq package-enable-at-startup nil)

;; Disable UI elements before they're initialized (faster than disabling after)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(menu-bar-lines . 0) default-frame-alist)  ; uncomment to disable menu bar

;; Don't resize frame when setting font or other UI elements
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
