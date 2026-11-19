;;; rich-text.el --- copy HTML to the clipboard as rich text -*- lexical-binding: t -*-

;;; Commentary:

;; Shared by `org-copy-region-as-html' (org-extras.el) and
;; `markdown-copy-region-as-html' (init.el).  Cleans up HTML so it pastes
;; into Gmail and Google Docs without extra space, then copies it as
;; text/html.

;;; Code:

(require 'subr-x)

(defun rich-text-html-cleanup (html)
  "Return HTML adjusted to paste into Gmail and Google Docs.
Paragraphs become <div>s like Gmail's own, with a blank line between
them.  List margins are removed and strikethrough uses an inline style.
Each step below shows an example input and output."
  (let* (;; Unwrap text in loose list items, which pandoc puts in <p>.
         ;;   <li><p>one</p></li>  =>  <li>one</li>
         (html (replace-regexp-in-string "<li>\\s-*<p>" "<li>" html))
         (html (replace-regexp-in-string "</p>\\s-*</li>" "</li>" html))
         ;; Mark a blank line before a paragraph that follows a paragraph
         ;; or list.  Paragraph margins are dropped below, so this keeps
         ;; the gap that a blank line in the source means.
         ;;   </p><p>B  =>  </p><div><br></div><p>B
         ;;   </ul><p>B =>  </ul><div><br></div><p>B
         (html (replace-regexp-in-string
                "\\(</p>\\|</[ou]l>\\)\\s-*<p\\b"
                "\\1\n<div><br></div>\n<p" html))
         ;; Unwrap a paragraph that introduces a list.  The list starts
         ;; on a new line anyway, and the wrapper adds space before it.
         ;; The content may hold inline tags but no </p>.
         ;;   <p>Ideas for <b>FR1</b>:</p><ul>  =>  Ideas for <b>FR1</b>:<ul>
         (html (replace-regexp-in-string
                "<p>\\(\\(?:[^<]\\|<[^/p]\\|</[^p]\\)*\\)</p>\\s-*\\(<[ou]l\\b\\)"
                (lambda (m)
                  ;; `replace-regexp-in-string' reuses the match data
                  ;; after this returns, and `string-trim' clobbers it.
                  (save-match-data
                    (let ((text (match-string 1 m))
                          (list (match-string 2 m)))
                      (concat (string-trim text) list))))
                html t t))
         ;; Turn remaining paragraphs into <div>s, which have no margin.
         ;;   <p>A</p>  =>  <div>A</div>
         (html (replace-regexp-in-string "<p\\b" "<div" html))
         (html (replace-regexp-in-string "</p>" "</div>" html))
         ;; Google Docs ignores <del> but honors an inline style.
         ;;   <del>x</del>
         ;;   =>  <span style="text-decoration:line-through">x</span>
         (html (replace-regexp-in-string
                "<del>" "<span style=\"text-decoration:line-through\">" html))
         (html (replace-regexp-in-string "</del>" "</span>" html))
         ;; Remove default list margins, which add space above and below.
         ;;   <ul class="org-ul">
         ;;   =>  <ul style="margin-top:0;margin-bottom:0" class="org-ul">
         (html (replace-regexp-in-string
                "<\\([ou]l\\)\\b"
                "<\\1 style=\"margin-top:0;margin-bottom:0\"" html))
         ;; Strip whitespace around block tags, which can paste as empty
         ;; lines.  Whitespace between inline tags is left alone.
         ;;   <div>\n<b>A</b>\n</div>\n<ul>  =>  <div><b>A</b></div><ul>
         (html (replace-regexp-in-string
                "[ \t\n]*\\(</?\\(?:div\\|ul\\|ol\\|li\\)\\b[^>]*>\\)[ \t\n]*"
                "\\1" html)))
    ;; Without a charset, pasted non-ASCII text can come out garbled
    (concat "<meta charset=\"utf-8\">" html)))

(defun copy-html-as-rich-text (html)
  "Clean up HTML and copy it to the clipboard as type text/html.
Gmail and Google Docs then paste bold, bullets, links, etc. natively.
Uses osascript on macOS, otherwise wl-copy or xclip."
  (let* ((html (rich-text-html-cleanup html))
         (status
          (with-temp-buffer
            (cond
             ((eq system-type 'darwin)
              ;; Script on stdin avoids argument length limits
              (insert "set the clipboard to «data HTML"
                      (mapconcat (lambda (b) (format "%02X" b))
                                 (encode-coding-string html 'utf-8) "")
                      "»")
              (call-process-region (point-min) (point-max) "osascript"))
             ((and (getenv "WAYLAND_DISPLAY") (executable-find "wl-copy"))
              (insert html)
              (call-process-region (point-min) (point-max) "wl-copy"
                                   nil nil nil "--type" "text/html"))
             ((executable-find "xclip")
              (insert html)
              (call-process-region (point-min) (point-max) "xclip"
                                   nil nil nil "-selection" "clipboard"
                                   "-t" "text/html"))
             (t (user-error "Need osascript, wl-copy, or xclip"))))))
    (unless (eq status 0) (error "Clipboard copy failed: %s" status))
    (message "Copied region as HTML")))

(provide 'rich-text)

;;; rich-text.el ends here
