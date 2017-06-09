;;; jsx-mode.el --- major mode for JSX

;; Copyright (c) 2012 DeNA, Co., Ltd (http://dena.jp/intl/)

;; Author: Takeshi Arabiki (abicky)
;; Version: 0.1.10
;; Package-Version: 20130908.1024
;; URL: https://github.com/jsx/jsx-mode.el

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; =============
;;  Get Started
;; =============

;; Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add to the following lines to your ~/.emacs.d/init.el.

;;    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;    (autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; See also init.el.example.


;; ==============
;;  Key Bindings
;; ==============

;; In `jsx-mode', the following keys are bound by default.

;; C-c C-c     comment-region (Comment or uncomment each line in the region)
;; C-c c       jsx-compile-file (Compile the current buffer)
;; C-c C       jsx-compile-file-async (Compile the current buffer asynchronously)
;; C-c C-r     jsx-run-buffer (Run the current buffer)


;; TODO:
;; * support imenu
;; * fix a bug that any token after implements is colored
;;   e.g. 'J' will be colored in the code like 'class C implements I { J'
;; * support indentations for lambda statment
;; * support auto-complete

;;; Code:

(eval-when-compile
  (require 'thingatpt)
  (require 'flymake)
  (require 'cl)
  (require 'json)
  (require 'auto-complete nil t)
  (require 'popup nil t))


(defconst jsx-version "0.1.10"
  "Version of `jsx-mode'")

(defgroup jsx nil
  "JSX mode."
  :group 'languages)

(defcustom jsx-indent-level 4
  "indent level in `jsx-mode'"
  :type 'integer
  :group 'jsx-mode)

(defcustom jsx-cmd "jsx"
  "jsx command for `jsx-mode'"
  :type 'string
  :group 'jsx-mode)

(defcustom jsx-cmd-options '()
  "jsx command options for `jsx-mode'.

For example, if this value is '(\"--add-search-path\" \"/path/to/lib\"),
then execute command like \"jsx --add-search-path /path/to/lib --run sample.jsx\".
"
  :type '(repeat string)
  :group 'jsx-mode)

(defcustom jsx-node-cmd "node"
  "node command for `jsx-mode'"
  :type 'string
  :group 'jsx-mode)

(defcustom jsx-syntax-check-mode "parse"
  "Jsx compilation mode for the syntax check in `jsx-mode'.
The value should be \"parse\" or \"compile\". (Default: \"parse\")"
  :type '(choice (const "parse")
                 (const "compile"))
  :group 'jsx-mode)

(defcustom jsx-use-flymake nil
  "Whether or not to use `flymake-mode' in `jsx-mode'."
  :type 'boolean
  :group 'jsx-mode)

(defcustom jsx-use-auto-complete nil
  "Whether or not to use `auto-complete-mode' in `jsx-mode'."
  :type 'boolean
  :group 'jsx-mode)

(defvar jsx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'comment-region)
    (define-key map (kbd "C-c c") 'jsx-compile-file)
    (define-key map (kbd "C-c C") 'jsx-compile-file-async)
    (define-key map (kbd "C-c C-r") 'jsx-run-buffer)
    map))

(defvar jsx-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_  "w" st)
    (modify-syntax-entry ?%  "." st)
    ;; C-style comments
    ;; cf. Syntax Tables > Syntax Descriptors > Syntax Flags
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23"   st)
    (modify-syntax-entry ?\n "> b"  st)
    ;; string
    (modify-syntax-entry ?\' "\"" st)
    st))



;; Constant variables
(defconst jsx--constant-variables
  '("__FILE__" "__LINE__"
    "Infinity"
    "NaN"
    "false"
    "null"
    "this" "true"
    "undefined"))

(defconst jsx--keywords
  '(;; literals shared with ECMA 262literals shared with ECMA 262
    "null"      "true"       "false"
    "NaN"       "Infinity"
    ;; keywords shared with ECMA 262
    "break"     "do"         "instanceof" "typeof"
    "case"      "else"       "new"        "var"
    "catch"     "finally"    "return"     "void"
    "continue"  "for"        "switch"     "while"
    "function"  "this"
    "default"   "if"         "throw"
    "delete"    "in"         "try"
    ;; keywords of JSX
    "class"     "extends"    "super"
    "import"    "implements"
    "interface" "static"
    "__FILE__"  "__LINE__"
    "undefined")
  "keywords defined in parser.js of JSX")

(defconst jsx--reserved-words
  '(;; literals of ECMA 262 but not used by JSX
    "debugger"  "with"
    ;; future reserved words of ECMA 262
    "const"     "export"
    ;; future reserved words within strict mode of ECMA 262
    "let"       "private"     "public"     "yield"
    "protected"
    ;; JSX specific reserved words
    "extern"    "native"
    "trait"     "using"
    "as"        "is"
    "operator"  "package")
  "reserved words defined in parser.js of JSX")

(defconst jsx--contextual-keywords
  '("__noconvert__" "__readonly__" "abstract" "final" "mixin" "override"))

(defconst jsx--builtin-functions
  '("assert" "log"))

(defconst jsx--primary-types
  '("boolean" "int" "number" "string"))

(defconst jsx--extra-types
  '("MayBeUndefined" "Nullable" "void" "variant"))

(defconst jsx--reserved-classes
  '("Array"
    "Boolean"
    "Date"
    "Error" "EvalError"
    "Function"
    "Map"
    "Number"
    "Object"
    "RangeError" "ReferenceError" "RegExp"
    "String" "SyntaxError"
    "TypeError"))

(defconst jsx--template-owners
  '("Array" "Map" "MayBeUndefined"))

(defconst jsx--modifiers
  '("static" "abstract" "override" "final" "const" "native" "__readonly__"))

(defconst jsx--class-definitions
  '("class" "extends" "implements" "interface" "mixin"))



;; Regural expressions
(defconst jsx--identifier-start-re
  "[a-zA-Z_]")

(defconst jsx--identifier-re
  (concat jsx--identifier-start-re "[a-zA-Z0-9_]*"))

(defconst jsx--function-definition-re
  (concat
   "^\\s-*\\(?:\\(?:" (mapconcat 'identity jsx--modifiers "\\|") "\\)\\s-+\\)*"
   "function\\s-+\\(" jsx--identifier-re "\\)"))

(defconst jsx--function-definition-in-map-re
  (concat
   "\\(?:^\\|,\\)\\s-*\\(" jsx--identifier-re "\\)\\s-*:\\s-*function\\s-*("))

(defconst jsx--keywords-re
  (regexp-opt
   (append jsx--keywords jsx--reserved-words jsx--contextual-keywords)
   'words))

(defconst jsx--constant-variable-re
  (regexp-opt jsx--constant-variables 'words))

(defconst jsx--primitive-type-re
  (regexp-opt
   (append jsx--primary-types jsx--extra-types jsx--template-owners)
   'words))

(defconst jsx--reserved-class-re
  (concat
   "\\<\\("
   (regexp-opt jsx--reserved-classes)
   "\\)\\s-*[,;>(]"))

(defconst jsx--regex-literal-re
  (concat
   "\\(?:^\\|[(,;:=]\\)\\s-*"
   ;; beginning of a regex literal
   "\\(/\\)"
   ;; first character
   ;; "/*" means beginning of a comment, so exclude "*"
   "\\(?:\\[\\(?:\\\\.\\|[^]]\\)*\\]\\|\\\\.\\|[^/\\*\n]\\)"
   "\\(?:\\[\\(?:\\\\.\\|[^]]\\)*\\]\\|\\\\.\\|\\|[^/\\\n]\\)*"
   ;; end of a regex literal
   "\\(/\\)\\([gim]*\\)"))

(defconst jsx--builtin-function-re
  (concat
   "\\(?:^\\|[;{]\\)\\s-*\\("
   (regexp-opt jsx--builtin-functions 'words)
   "\\)"))

(defconst jsx--class-definition-re
      (concat
       (regexp-opt jsx--class-definitions 'words)
       "\\s-+\\(" jsx--identifier-re "\\)"))

(defconst jsx--create-instance-re
      (concat
       "\\<new\\s-+"
       ;; call 'new foo.Foo' to create a Foo class instance defined in foo.jsx
       ;; if import "foo.jsx" into foo
       "\\(?:" jsx--identifier-re "\\.\\)?"
       "\\(" jsx--identifier-re "\\)"))

;; class name of the return value like function createFoo() : Foo {
(defconst jsx--return-class-re
  (concat
   ")\\s-*:\\s-*"
   "\\(?:" jsx--identifier-re "\\.\\)?"
   "\\(" jsx--identifier-re "\\)\\s-*\\(?:[[,{]\\|$\\)"))

(defconst jsx--template-class-re
  (concat "<\\s-*\\(" jsx--identifier-re "\\)\\s-*>"))

(defconst jsx--variable-definition-with-class-re
  (concat
   "\\<\\var\\s-+\\(" jsx--identifier-re "\\)"
   "\\s-*:\\s-*"
   "\\(?:" jsx--identifier-re "\\.\\)?"
   "\\(" jsx--identifier-re "\\)""\\>"))

;; currently not support definitions like 'var a:int, b:int;'
(defconst jsx--variable-definition-re
  (concat
   "\\<\\var\\s-+\\(" jsx--identifier-re "\\)\\>"))

(defconst jsx--variable-and-type-re
  (concat
   "\\(" jsx--identifier-re "\\)\\s-*:\\s-*\\(" jsx--identifier-re "\\)"))

(defconst jsx--import-from-re
  (concat
   "import\\s-+\\(" jsx--identifier-re "\\)\\s-*\\(from\\|$\\)?"))

(defconst jsx--quoted-re
  (concat
   "'[^'\\]*\\(?:\\\\.[^'\\]*\\)*'"        ; single quote
   "\\|"
   "\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\""))  ; double quote

(defconst jsx--import-into-re
  (concat
   "import\\s-+" jsx--quoted-re "\\s-+\\(into\\)"))


(defun jsx--in-arg-definition-p ()
  (when (list-at-point)
    (save-excursion
      (search-backward "(" nil t)
      (forward-symbol -1)
      (or (equal (word-at-point) "function")
          (progn (forward-symbol -1)
                 (equal (word-at-point) "function"))))))


(defvar jsx-font-lock-keywords
  `(
    (,jsx--constant-variable-re 0 font-lock-constant-face)
    (,jsx--builtin-function-re 1 font-lock-builtin-face)
    (,jsx--regex-literal-re 3 font-lock-string-face)
    (,jsx--variable-definition-re 1 font-lock-variable-name-face)
    (,jsx--primitive-type-re 0 font-lock-type-face)
    (,jsx--reserved-class-re 1 font-lock-type-face)
    (,jsx--keywords-re 0 font-lock-keyword-face)
    (,jsx--class-definition-re 2 font-lock-type-face)
    (,jsx--create-instance-re 1 font-lock-type-face)
    (,jsx--template-class-re  1 font-lock-type-face)
    (,jsx--return-class-re 1 font-lock-type-face)
    (,jsx--import-from-re
     (1 font-lock-type-face)
     (2 font-lock-keyword-face))
    (,jsx--import-into-re 1 font-lock-keyword-face)
    (,jsx--variable-definition-with-class-re
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face))

    ;; color names of interface or mixin like implements A, B, C
    ,(list
      "\\(?:^\\|\\s-\\)implements\\s-+"
      (list (concat "\\(" jsx--identifier-re "\\)\\s-*\\(?:[,{]\\|$\\)")
            '(forward-symbol -1)
            nil
            '(1 font-lock-type-face)))

    ;; color class names like below (color 'B', 'I', and 'J')
    ;;     class A
    ;;     extends B
    ;;     implements I, J {
    ;;
    ;; currently not color names like below  (not color 'J')
    ;;     class A
    ;;     extends B
    ;;    implements I,
    ;;     J {
    ,(list
      (concat
        "^\\s-*\\(" jsx--identifier-re "\\)\\(?:\\s-\\|$\\)")
      (list (concat "\\<" jsx--identifier-re "\\>")
            '(if (save-excursion
                   (jsx--backward-non-comment-word 2)
                   (looking-at (concat
                                (regexp-opt jsx--class-definitions)
                                "\\s-*$")))
                 (backward-word)
               (end-of-line))
            nil
            '(0 font-lock-type-face)))

    ;; color function arguments like function(a: int, b:int)
    ,(list
      (concat
       "\\<function\\>\\(?:\\s-+" jsx--identifier-re "\\)?\\s-*(\\s-*")
      (list jsx--variable-and-type-re
            '(unless (jsx--in-arg-definition-p) (end-of-line))
            nil
            '(1 font-lock-variable-name-face)
            '(2 font-lock-type-face)))
    (,(concat "<\\s-*\\(" jsx--identifier-re "\\)\\s-*>") 1 font-lock-type-face)

    ;; color classes of function arugments like function(:int, :int)
    ,(list
     (concat
      "\\<function\\>\\s-*(\\s-*")
     (list (concat "\\s-*:\\s-*\\(" jsx--identifier-re "\\)")
           '(unless (jsx--in-arg-definition-p) (end-of-line))
           nil
           '(1 font-lock-type-face)))

    ;; color function arguments
    ;;     function(a: int,
    ;;              b:int)
    ;;
    ;; currently not color arguments like below
    ;;     function(a:
    ;;              int,
    ;;              b
    ;;              :int)
    ,(list
      (concat
       "^\\s-*,?\\s-*" jsx--variable-and-type-re)
      (list jsx--variable-and-type-re
            '(if (save-excursion (backward-char)
                                 (jsx--in-arg-definition-p))
                 (forward-symbol -2)
               (end-of-line))
            nil
            '(1 font-lock-variable-name-face)
            '(2 font-lock-type-face)))

    ;; color classes of function arguments like below
    ;;     function(:int,
    ;;              :int)
    ;;
    ;; currently not color classes like below
    ;;     function(:
    ;;              int,
    ;;              :int)
    ,(list
      (concat
       "^\\s-*,?\\s-*:\\s-*\\(" jsx--identifier-re "\\)")
      (list (concat "\\s-*:\\s-*\\(" jsx--identifier-re "\\)")
            '(if (save-excursion (backward-char)
                                 (jsx--in-arg-definition-p))
                 (search-backward ":")
               (end-of-line))
            nil
            '(1 font-lock-type-face)))

    ;; function names should be colored after coloring arguments,
    ;; otherwise arguments will be colored
    ;; e.g. function(b : string, a : function() : void)
    (,jsx--function-definition-re 1 font-lock-function-name-face)
    (,jsx--function-definition-in-map-re 1 font-lock-function-name-face)
    ))

(defvar jsx-font-lock-syntactic-keywords
  `((,jsx--regex-literal-re (1 "|") (2 "|"))))


(defun jsx--in-string-or-comment-p (&optional pos)
  (nth 8 (syntax-ppss pos)))

(defun jsx--in-comment-p (&optional pos)
  (nth 4 (syntax-ppss pos)))

(defun jsx--non-block-statement-p ()
  (save-excursion
    (jsx--go-to-previous-non-comment-char)
    (or (string-match-p "\\(?:do\\|else\\)\\_>" (or (word-at-point) ""))
        (and (= (char-after) ?\))
             (progn
               (forward-char)
               (backward-list)
               (backward-word)
               (looking-at "\\(?:for\\|if\\|while\\)\\_>"))))))

(defun jsx--in-condition-p ()
  (when (list-at-point)
    (save-excursion
      (search-backward "(" nil t)
      (forward-symbol -1)
      (let ((word (word-at-point)))
        (or (equal word "while") (equal word "if"))))))

(defun jsx--go-to-previous-non-comment-char ()
  (search-backward-regexp "[[:graph:]]" nil t)
  (while (jsx--in-comment-p)
    ;; move to the beginning of the comment
    (search-backward-regexp "/\\*\\|//" nil t)
    ;; move to the previous visible character
    (search-backward-regexp "[[:graph:]]" nil t)))

(defun jsx--go-to-next-non-comment-char ()
  (if (looking-at "[[:graph:]]")
      (forward-char))
  (search-forward-regexp "[[:graph:]]" nil t)
  (while (save-excursion (forward-char) (jsx--in-comment-p))
    ;; move to the end of the comment
    (search-forward-regexp "\\*/\\|$" nil t)
    ;; move to the next visible character
    (search-forward-regexp "[[:graph:]]" nil t)))

(defun jsx--backward-non-comment-word (&optional arg)
  (let ((cnt (or arg 1)))
    (while (> cnt 0)
      (backward-word)
      (while (jsx--in-comment-p)
        (backward-word))
      (setq cnt (1- cnt)))))

(defun jsx--backward-up-list (&optional level ppss)
  "Move back outside of parentheses LEVEL times
and return the position if suceeded.
If LEVEL is larger than the current depth, the ourermost leve is used."
  (let* ((pos-list (nth 9 (or ppss (syntax-ppss))))
         (pos (nth (- (length pos-list) (or level 1)) pos-list)))
    (and pos (goto-char pos))))

(defun jsx-indent-line ()
  (interactive)
  (let ((indent-length (jsx--calculate-indentation))
        (offset (- (current-column) (current-indentation))))
    (when indent-length
      (indent-line-to indent-length)
      (if (> offset 0) (forward-char offset)))))

(defun jsx--calculate-indentation ()
  ;; TODO: refactoring
  (save-excursion
    (back-to-indentation)
    (let* ((cw (current-word t))
           (ca (char-after))
           (ppss (syntax-ppss)))
      (cond
       ((eq (syntax-ppss-context ppss) 'string)
        nil)
       ((eq (syntax-ppss-context ppss) 'comment)
        (save-excursion
          (let ((end-of-comment-p (looking-at "\\*/")))
            (forward-line -1)
            (back-to-indentation)
            (if (eq (point) (nth 8 ppss))
                ;; the previous line is the beginning of the comment
                (+ (current-column) (if (eq ca ?*) 1 2))
              ;; the indentation level of the end of the comment
              ;; should be the same level as its beginning
              ;; if the previous line doesn't begin with '*'
              (when (and end-of-comment-p
                         (not (eq (char-after) ?*)))
                (goto-char (nth 8 ppss)))
              (current-column)))))
       ((eq ca ?{)
        (progn
          (jsx--go-to-previous-non-comment-char)
          (if (eq (char-after) ?\[)
              (+ (current-indentation) jsx-indent-level)
            (current-indentation))))
       ((and
         (or (eq ca ?})
             (eq ca ?\))
             (eq ca ?\])
             (equal cw "case")
             (equal cw "default"))
         (jsx--backward-up-list 1 ppss))
        (back-to-indentation)
        (while (or (jsx--in-arg-definition-p) (jsx--in-condition-p))
          (jsx--backward-up-list)
          (back-to-indentation))
        (current-indentation))
       ((jsx--non-block-statement-p)
        (+ (progn
             (jsx--go-to-previous-non-comment-char)
             (current-indentation))
           jsx-indent-level))
       ((or (jsx--in-arg-definition-p) (jsx--in-condition-p))
        (progn
          (jsx--go-to-previous-non-comment-char)
          (if (= (char-after) ?\()
              (+ (current-indentation) jsx-indent-level)
            (jsx--backward-up-list)
            (jsx--go-to-next-non-comment-char)
            (backward-char)
            (current-column))))
       ((jsx--backward-up-list 1 ppss)
        (back-to-indentation)
        (while (or (jsx--in-arg-definition-p) (jsx--in-condition-p))
          (jsx--backward-up-list)
          (back-to-indentation))
        (+ (current-column) jsx-indent-level))
       (t 0)
       ))))


;; compile or run the buffer

(defun jsx--some-buffers-modified-p ()
  (let ((bufs (buffer-list))
        buf modified-p)
    (while (and (not modified-p) bufs)
      (setq buf (car bufs))
      (when (string-match-p "\\.jsx\\'" (buffer-name buf))
        (with-current-buffer buf
          (setq modified-p (buffer-modified-p))))
      (setq bufs (cdr bufs)))
    modified-p))

(defun jsx--generate-cmd (&optional options)
  (setq options (append jsx-cmd-options options))
  (format "%s %s" jsx-cmd (mapconcat 'identity options " ")))

(defun jsx-compile-file (&optional options dst async)
  "Compile the JSX script of the current buffer
and make a JS script in the same directory."
  (interactive)
  (if (jsx--some-buffers-modified-p)
      (save-some-buffers nil t))
  ;; FIXME: file-name-nondirectory needs temporarily
  (let* ((jsx-file (file-name-nondirectory (buffer-file-name)))
         (js-file (or dst (substring jsx-file 0 -1)))
         cmd)
    (setq options (append jsx-cmd-options options))
    (setq cmd (jsx--generate-cmd
               (append options (list "--output" js-file jsx-file))))
    (if async
        (setq cmd (concat cmd " &")))
    (message "Compiling...: %s" cmd)
    (if (eq (shell-command cmd) 0) js-file nil)))

(defun jsx-compile-file-async (&optional options dst)
  "Compile the JSX scirpt of the current buffer asynchronously
and make a JS script in the same directory."
  (interactive)
  (jsx-compile-file options dst t))

(defun jsx-compile-file-and-run ()
  "Compile the JSX script of the current buffer,
make a JS script in the same directory, and run it."
  (interactive)
  (let* ((js-file (jsx-compile-file '("--executable")))
         (cmd (format "%s %s" jsx-node-cmd js-file)))
    (if js-file
        (shell-command cmd))))

(defun jsx-run-buffer (&optional options)
  "Run the JSX script of the current buffer."
  (interactive)
  (if (jsx--some-buffers-modified-p)
      (save-some-buffers nil t))
  (let ((jsx-file (file-name-nondirectory (buffer-file-name)))
        (cmd jsx-cmd))
    (setq options (append jsx-cmd-options options))
    (setq cmd (jsx--generate-cmd
               (append options (list "--run" jsx-file))))
    (setq cmd (format "%s --run %s" cmd jsx-file))
    (shell-command cmd)))


;; flymake

(defvar jsx-err-line-patterns
  '(("\\[\\([^:]+\\):\\([0-9]+\\)\\(?::[0-9]+\\)?\\] \\(.*\\)" 1 2 nil 3)))

(defun jsx-flymake-on ()
  "Turn on `flymake-mode' in `jsx-mode'"
  (interactive)
  (set (make-local-variable 'flymake-err-line-patterns) jsx-err-line-patterns)
  (add-to-list 'flymake-allowed-file-name-masks '("\\.jsx\\'" jsx--flymake-init))
  (flymake-mode t))

(defun jsx-flymake-off ()
  "Turn off `flymake-mode' in `jsx-mode'"
  (interactive)
  (flymake-mode 0))

(defun jsx--flymake-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     ;; if use import "*.jsx", _flymake.jsx is very annoying,
                     ;; so not use 'flymake-create-temp-inplace
                     (lambda (file-name prefix)
                       (concat
                        (flymake-create-temp-inplace file-name prefix) ".tmp"))))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list jsx-cmd (append jsx-cmd-options
                          (list
                           "--mode" jsx-syntax-check-mode
                           "--output" "/dev/null"
                           local-file)))))

(defun jsx--get-errs-for-current-line ()
  "Return the list of errors/warnings for the current line"
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (msgs '()))
    (dolist (err-info line-err-info-list)
      (let* ((text (flymake-ler-text err-info))
             (line (flymake-ler-line err-info)))
        (setq msgs (append msgs (list (format "[%s] %s" line text))))))
    msgs))

(defun jsx-display-err-for-current-line ()
  "Display the errors/warnings for the current line in the echo area
if there are any errors or warnings in `jsx-mode'."
  (interactive)
  (let ((msgs (jsx--get-errs-for-current-line)))
    (message (mapconcat 'identity msgs "\n"))))

(defun jsx-display-popup-err-for-current-line ()
  "Display a popup window with errors/warnings for the current line
if there are any errors or warnings in `jsx-mode'."
  (interactive)
  (let ((msgs (jsx--get-errs-for-current-line)))
    (if (require 'popup nil t)
        (popup-tip (mapconcat 'identity msgs "\n"))
      (message "`popup' is not instelled."))))


;; auto-complete

(defvar jsx--try-to-show-document-p nil)
(defvar jsx--hard-line-feed
  (propertize "\n" 'hard t)
  "Line feed for `fill-region'")

(defvar jsx-ac-source
  '((candidates . jsx--get-candidates)
    (document . jsx--get-document)
    (prefix . jsx--ac-prefix)
    (cache)))

(defun jsx--ac-prefix ()
  "Enable completion even if after invisible characters."
  (or (ac-prefix-default) (point)))

(defadvice auto-complete (before jsx--add-requires-to-ac-source activate)
  "Invoke completion whenever auto-complete is executed."
  (if (string= major-mode "jsx-mode")
      (add-to-list 'jsx-ac-source '(requires . 0))))

(defadvice auto-complete (after jsx--remove-requires-from-ac-source activate)
  (if (string= major-mode "jsx-mode")
      (setq jsx-ac-source (delete '(requires . 0) jsx-ac-source))))

(defun jsx--copy-buffer-to-tmp-file ()
  (let ((tmpfile (make-temp-name (concat (buffer-file-name) "."))))
    (write-region nil nil tmpfile nil 'silent)
    tmpfile))

(defun jsx--sort-candidates (a b)
  (string< (assoc-default 'word a) (assoc-default 'word b)))

(defun jsx--make-method-string (method args return-type)
  (format "%s(%s) : %s"
          method
          (mapconcat
           (lambda (arg)
             (format "%s : %s"
                     (assoc-default 'name arg)
                     (assoc-default 'type arg)))
           args ", ")
          return-type))

(defun jsx--parse-candidates (str)
  ;; JSON example (cf. src/completion.jsx of JSX)
  ;; {
  ;;   "word" : "stringify",
  ;;   "partialWord" : "ringify",
  ;;   "doc" : "serialize a value or object as a JSON string",
  ;;   "type" : "function (value : variant) : string",
  ;;   "returnType" : "string",
  ;;   "args" : [ { "name" : "value", "type" : "variant" } ],
  ;;   "definedClass" : "JSON",
  ;;   "definedFilename" : "lib/built-in/jsx",
  ;;   "definedLineNumber" : 903
  ;; }
  (let* ((json-array-type 'list)
         (candidates-info (json-read-from-string str)))
    (when candidates-info
      (setq candidates-info (sort candidates-info 'jsx--sort-candidates))
      (loop with (candidates docs symbol)
            for info in candidates-info
            ;; (assoc-default 'args info) is nil if the method has no arguments
            for args = (assq 'args info)
            for desc = (or (assoc-default 'doc info) "not documented")
            for prev-word = nil then word
            for word = (assoc-default 'word info)
            for name = word
            when (and prev-word (not (equal word prev-word)))
              collect (propertize prev-word 'docs docs 'symbol symbol)
                into candidates
                and do (setq docs)
                and do (setq symbol)
            do (when args
                 (setq symbol "f")
                 (setq name (jsx--make-method-string
                             word (cdr args) (assoc-default 'returnType info))))
            collect `((name . ,name) (desc . ,desc)) into docs
            finally return
                    (nconc candidates
                           (list (propertize word 'docs docs 'symbol symbol)))))))

(defun jsx--get-candidates ()
  (let* ((tmpfile (jsx--copy-buffer-to-tmp-file))
         (line (line-number-at-pos))
         ;; don't use (current-column) for tab indents
         (col (1+ (- (point) (line-beginning-position))))
         (cmd (jsx--generate-cmd
               (list "--complete" (format "%d:%d" line col)  tmpfile))))
    (with-temp-buffer
      (unwind-protect
          (call-process-shell-command cmd nil t)
        (delete-file tmpfile))
      (jsx--parse-candidates (buffer-string)))))

(defadvice fill-region (before jsx--fill-region activate)
  "Preserve the line feeds in documents
cf. https://github.com/auto-complete/popup-el/issues/43"
  (when jsx--try-to-show-document-p
    (beginning-of-buffer)
    (replace-string "\n" jsx--hard-line-feed)
    (setq use-hard-newlines t)))

(defun jsx--sort-docs (a b)
  (or
   (string< (assoc-default 'desc a) (assoc-default 'desc b))
   (string< (assoc-default 'name a) (assoc-default 'name b))))

(defun jsx--get-document (candidate)
  (setq jsx--try-to-show-document-p t)
  (run-at-time 0 nil (lambda() (setq jsx--try-to-show-document-p)))
  (let ((docs (get-text-property 0 'docs candidate)))
    (when docs
      (setq docs (sort (copy-alist docs) 'jsx--sort-docs))
      (loop with document and names
            for doc in docs
            for prev-desc = nil then desc
            for desc = (assoc-default 'desc doc)
            do (when (and prev-desc (not (equal desc prev-desc)))
                 (if (and document (not (equal document "")))
                     (setq document (concat "\n\n" document)))
                 (setq document (format "%s\n\n%s" names prev-desc))
                 (setq names nil))
            do (if names
                   (setq names (format "%s\n%s" names (assoc-default 'name doc)))
                 (setq names (assoc-default 'name doc)))
            finally return
                    (if document
                        (format "%s\n\n%s\n\n%s" document names desc)
                        (format "%s\n\n%s" names desc))))))


;;;###autoload
(define-derived-mode jsx-mode fundamental-mode "Jsx"
  :syntax-table jsx-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(jsx-font-lock-keywords nil nil))
  (set (make-local-variable 'font-lock-syntactic-keywords)
       jsx-font-lock-syntactic-keywords)
  (set (make-local-variable 'indent-line-function) 'jsx-indent-line)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (when (and jsx-use-auto-complete (require 'auto-complete nil t))
    (require 'json)
    (add-to-list 'ac-modes 'jsx-mode)
    (setq ac-sources '(jsx-ac-source ac-source-filename)))
  (if jsx-use-flymake
      (jsx-flymake-on)))

(provide 'jsx-mode)
;;; jsx-mode.el ends here
