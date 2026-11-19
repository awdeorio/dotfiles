;;; ediff-extras.el --- ediff commands to copy a difference without replacing -*- lexical-binding: t -*-

;;; Commentary:

;; Copy a difference region from one buffer to the other, inserting it
;; before or after the region instead of replacing it.  Loaded from the
;; ediff block's :config in init.el.

;;; Code:

(require 'ediff)
(require 'ediff-init)
(require 'ediff-util)

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

(provide 'ediff-extras)

;;; ediff-extras.el ends here
